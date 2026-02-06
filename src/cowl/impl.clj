(ns cowl.impl
  {:doc "Base implementations for the COWL protocols"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [tiara.data :as data :refer [ordered-map ordered-set]]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot]
            [cowl.io :as cio]
            [cowl.impl.common :as common :refer [om os mm]]
            [cowl.impl.object-prop]
            [cowl.impl.data-prop]
            [cowl.impl.classes])
  (:import [cowl.protocols DocumentElement AddressableElement Annotatable AnnotationTest TTLStreamable Inlineable
            Property ObjectPropertyProtocol Document ClassExpression]
           [quoll.rdf IRI]
           [cowl.impl.object_prop ObjectProperty SubObjectPropertyOf ObjectPropertyChain InverseObjectProperties
            ObjectInverseOf EquivalentObjectProperties DisjointObjectProperties ObjectPropertyDomain
            ObjectPropertyRange FunctionalObjectProperty InverseFunctionalObjectProperty ReflexiveObjectProperty
            IrreflexiveObjectProperty SymmetricObjectProperty AsymmetricObjectProperty TransitiveObjectProperty]
           [cowl.impl.data_prop DataProperty SubDataPropertyOf EquivalentDataProperties DisjointDataProperties
            DataPropertyDomain DataPropertyRange FunctionalDataProperty]
           [cowl.impl.classes OWLClass ObjectIntersectionOf ObjectUnionOf ObjectComplementOf ObjectOneOf
            ObjectSomeValuesFrom ObjectAllValuesFrom ObjectHasValue ObjectHasSelf
            ObjectMinCardinality ObjectMaxCardinality ObjectExactCardinality
            DataSomeValuesFrom DataAllValuesFrom DataHasValue
            DataMinCardinality DataMaxCardinality DataExactCardinality]))


(defn check
  "Test that a value is one of a known set"
  ([expected value] (check expected value "Unexpected value"))
  ([expected value msg]
   (when-not (contains? expected value)
     (throw (ex-info (str msg ": " value) {:value value})))))

(defn ominus
  "Performs a set minus operation, where the first set is ordered, and s2 may be a seq."
  [s1 s2]
  (persistent!
   (reduce (fn [result item] (if (contains? s1 item) (disj! result item) result))
           (transient s1)
           s2)))

(defn struct-merge
  "Merges records so that sets and maps are added to"
  [struct1 struct2]
  (reduce (fn [result [k v]]
            (cond
              (set? v) (let [sv (get result k)]
                         (if (set? sv)
                           (assoc result k (into sv v))
                           (assoc result k v)))
              (map? v) (let [mv (get result k)]
                         (if (map? mv)
                           (assoc result k (merge mv v))
                           (assoc result k v)))
              :else (assoc result k v)))
          struct1 struct2))

(defn doc-add-entity
  [{:keys [annotation-props] :as doc} index entity]
  (let [eid (prot/id entity)
        annotation-props* (retrieve-annotation-props entity)
        new-aprops (ominus annotation-props annotation-props*)]
    (cond-> (update doc index update eid struct-merge entity)
      (seq new-aprops) (update :annotation-props into new-aprops))))

(defrecord Ontology [_id version class-idx oprop-idx dprop-idx annotations
                     instance-idx prefixes annotation-props annotation-axioms datatypes]
  Annotatable
  (annotate [this ann]
    (let [property (:prop ann)
          new-ann-props (ominus annotation-props (retrieve-annotation-props ann))]
      (cond-> (update this :annotations assoc property ann)
        (seq new-ann-props) (update :annotation-props into new-ann-props))))
  (annotate [this prop text]
    (let [new-prop (not (or (contains? owl-annotation-props prop)
                            (contains? annotation-props prop)))]
      (cond-> (update this :annotations assoc prop (annotation prop text))
        new-prop (update :annotation-props conj prop))))
  (annotate [this id prop text]
    (let [result (reduce (fn [doc index]
                           (if (contains? oprop-idx id)
                             (update doc index update id prot/annotate prop text)
                             doc))
                         this [:oprop-idx :dprop-idx :class-idx])]
      (if (identical? result this)
        (ex-info (str "Unknown entity: " id) {:id id})
        result)))
  (get-annotations [_] annotations)
  Document
  (add-object-property [this prop]
    (doc-add-entity this :oprop-idx prop))
  (add-data-property [this prop]
    (doc-add-entity this :dprop-idx prop))
  (add-class [this cls]
    (doc-add-entity this :class-idx cls))
  (get-object-property [_ id] (get oprop-idx id))
  (get-data-property [_ id] (get dprop-idx id))
  (get-class [_ id] (get class-idx id))
  TTLStreamable
  (ttl-emit [_ stream]
    (cio/write-prefixes stream prefixes)
    (cio/start-doc stream _id version)
    (cio/write-doc-annotations stream annotations)
    (cio/write-declarations stream (keys class-idx) (keys oprop-idx) (keys dprop-idx) annotation-props datatypes (keys instance-idx))
    (cio/write-obj-props stream (vals oprop-idx))
    (cio/end-doc stream)))

(defn normalize
  "Normalize all ids in a document into IRIs according to the document prefixes"
  [{:keys [prefixes] :as document}]
  (let [refn (common/recontextualize-element-fn prefixes)]
    (-> document
        (update :_id refn)
        (update :version refn)
        (update :annotations common/mapmm refn)
        (update :class-idx common/mapom refn)
        (update :oprop-idx common/mapom refn)
        (update :dprop-idx common/mapom refn)
        (update :instance-idx common/mapom refn)
        (update :annotation-props common/mapos refn)
        (update :annotation-axioms common/mapos refn)
        (update :datatypes common/mapos refn))))

(defn as-namespace
  "Converts an IRI into a namespace IRI"
  [base-iri]
  (if base-iri
    (let [lst (last base-iri)]
      (if (#{\/ \#} lst)
        base-iri
        (str base-iri "/")))
    "#"))

(defn standardize-prefixes
  "Standardizes a prefix list to always use the `default-pre` (empty-keyword) as the default.
  If no default prefix exists, then create one from the provided iri."
  [iri prefixes]
  (let [[dk default] (->> [:_default (keyword "") nil]
                          (keep (fn [k] (when-let [n (get prefixes k)] [k n])))
                          first)]
    (if default
      (->> (dissoc prefixes dk)
           (into (ordered-map common/default-pre default)))
      (if (get prefixes common/default-pre)
        (ordered-map prefixes)  ;; identity if already an ordered-map
        (into (ordered-map common/default-pre (as-namespace iri)) prefixes)))))

(defn add
  "Adds a child element to the provided element.
  Dispatches to the child object, since each type can only be embedded in a limited number of parents types"
  [parent child]
  (prot/add-to-parent child parent))

(defn ontology
  ([] (ontology nil nil nil))
  ([id] (ontology id nil nil))
  ([id version] (ontology id version nil))
  ([id version prefixes & elements]
   (let [pfxs (standardize-prefixes id (or prefixes rdf/common-prefixes))
         ont-iri (if id (common/localized-iri pfxs id) common/local-id)
         vi (as-namespace (:iri ont-iri))
         ver-iri (if version
                   (common/localized-iri pfxs
                                         (if (str/index-of version "/") ;; proxy for an IRI form
                                           version
                                           (str vi version)))
                   (rdf/iri (str vi common/initv)))
         doc (->Ontology ont-iri ver-iri om om om mm om pfxs os os os)]
     (reduce add doc elements))))

