(ns cowl.impl
  {:doc "Base implementations for the COWL protocols"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [tiara.data :as data :refer [ordered-map]]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot]
            [cowl.io :as cio])
  (:import [cowl.protocols Annotatable Streamable Property ObjectPropertyProtocol Document]
           [quoll.rdf IRI]))

(def local-id (rdf/iri "#"))
(def initv "0.0.1")

(def default-pre "")

(def owl-annotation-props
  #{:rdfs/label :rdfs/comment :rdfs/seeAlso :rdfs/isDefinedBy
    :owl/versionInfo :owl/deprecated :owl/backwardCompatibleWith
    :owl/incompatibleWith :owl/priorVersion})

(def om data/EMPTY_MAP)

(def os data/EMPTY_SET)

(def mm data/EMPTY_MULTI_MAP)

(defn pname-lname
  "Gets a prefix-name/local-name pair for an IRI string, given the known prefix mappings.
  Returns `nil` if no prefix matches."
  ([s] (pname-lname rdf/common-prefixes s))
  ([prefixes s]
   (->> prefixes
        (keep (fn [[_ nmsp :as pn]]
                (when (str/starts-with? s nmsp) pn)))
        first)))

(defn localized-iri
  "Takes a full IRI form and identifies if it can be converted to a prefix/local pair using the
  provided prefix map. Uses a linear search through the namespaces.
  TODO: This should be part of RuDolF."
  [prefixes i]
  (if (and (instance? IRI i) (:local i))
    i
    (let [iri-str (if (string? i) i (:iri i))]
      (if-let [[pre nmspace] (pname-lname prefixes iri-str)]
        (rdf/iri iri-str pre (subs iri-str (count nmspace)))
        (if (string? i) (rdf/iri iri-str) i)))))

(defn ->iri
  "Ensures that a value is an IRI constructing one if needed."
  ([i] (->iri rdf/common-prefixes i))
  ([prefixes i]
   (cond
     (keyword? i) (rdf/curie prefixes i)
     (instance? IRI i) i
     (string? i) (if-let [[pn ln] (pname-lname prefixes i)]
                   (rdf/iri i pn ln)
                   (rdf/iri i)))))

(defn ->id
  "Gets the ID of a object. If the object is an ID, then return it. Otherwise, extract it."
  [o]
  (if (map? o) (:id o) o))

(deftype Annotation [prop value])

(defn annotation
  "Creates a type-marked annotation"
  [prop value]
  (Annotation. prop value))

(defrecord ObjectProperty [id annotations super-props equivs domain range disjoints
                           fn? inverse-fn? transitive? symmetric? asymmetric?
                           reflexive? irreflexive?]
  Annotatable
  (annotate [this {:keys [prop value]}] (update this :annotations assoc prop value))
  (annotate [this prop text] (update this :annotations assoc prop text))
  (annotate [_ id prop text]
    (throw (ex-info "Object Properties do not contain other entities" {:id id :prop prop :text text})))
  Property
  (sub-property [this other] (update this :super-props conj (->id other)))
  (equivalent [this other] (update this :equivs conj (->id other)))
  (domain-of [this other] (update this :domain conj (->id other)))
  (range-of [this other] (update this :range conj (->id other)))
  (disjoint [this other] (update this :disjoints conj (->id other)))
  (functional [this] (assoc this :fn? true))
  ObjectPropertyProtocol
  (inverse-functional [this] (assoc this :inverse-fn? true))
  (transitive [this] (assoc this :transitive? true))
  (symmetric [this] (assoc this :symmetric? true))
  (asymmetric [this] (assoc this :asymmetric? true))
  (reflexive [this] (assoc this :reflexive? true))
  (irreflexive [this] (assoc this :irreflexive? true)))

(defn obj-property
  [id]
  (->ObjectProperty id mm os os os os os false false false false false false false))

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

(defrecord OWLDocument [id version class-idx oprop-idx dprop-idx annotations
                        instance-idx prefixes annotation-props datatypes]
  Annotatable
  (annotate [this {:keys [prop value]}] (update this :annotations assoc (->iri prefixes prop) value))
  (annotate [this prop text]
    (let [p (->iri prefixes prop)
          new-prop (not (or (contains? owl-annotation-props p)
                            (contains? annotation-props p)))]
      (cond-> (update this :annotations assoc p text)
        new-prop (update :annotation-props conj p))))
  (annotate [this id prop text]
    (if-let [field (cond
                     (contains? oprop-idx id) :oprop-idx
                     (contains? dprop-idx id) :dprop-idx
                     (contains? class-idx id) :class-idx
                     :else nil)]
      (update this field update id prot/annotate (->iri prefixes prop) text)
      (ex-info (str "Unknown entity: " id) {:id id})))
  Document
  (add-object-property [this {:keys [id annotations super-props equivs domain range disjoints] :as prop}]
    (let [->ref #(localized-iri prefixes %)
          annotations* (into om (map (fn [[a v]] [(->ref a) v])) (seq annotations))
          new-aprops (ominus annotation-props (keys annotations*))
          prop* (cond-> prop
                  (seq super-props) (update :super-props mapv ->ref)
                  (seq equivs) (update :equivs mapv ->ref)
                  (seq domain) (update :domain mapv ->ref)
                  (seq range) (update :range mapv ->ref)
                  (seq disjoints) (update :disjoints mapv ->ref))]
      (cond-> (update this :oprop-idx update id struct-merge prop*)
        (seq new-aprops) (update :annotation-props into new-aprops))))
  Streamable
  (emit [_ stream]
    (cio/write-prefixes stream prefixes)
    (cio/start-doc stream id version)
    (cio/write-doc-annotations stream annotations)
    (cio/write-declarations stream (keys class-idx) (keys oprop-idx) (keys dprop-idx) annotation-props datatypes)
    (cio/write-oprops stream oprop-idx)
    (cio/end-doc stream)))

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
           (into (ordered-map default-pre default)))
      (if (get prefixes default-pre)
        (ordered-map prefixes)  ;; identity if already an ordered-map
        (into (ordered-map default-pre (as-namespace iri)) prefixes)))))

(defn owl
  ([] (owl nil nil nil))
  ([id] (owl id nil nil))
  ([id version] (owl id version nil))
  ([id version prefixes]
   (let [pfxs (standardize-prefixes id (or prefixes rdf/common-prefixes))
         ont-iri (if id (localized-iri pfxs id) local-id)
         vi (as-namespace (:iri ont-iri))
         ver-iri (if version
                   (localized-iri pfxs
                                  (if (str/index-of version "/") ;; proxy for an IRI form
                                    version
                                    (str vi version)))
                   (rdf/iri (str vi initv)))]
     (->OWLDocument ont-iri ver-iri om om om mm om pfxs os os))))


(defn add
  "Adds an element to a document"
  [doc entity]
  (cond
    (instance? Annotation entity) (prot/annotate doc (:prop entity) (:value entity))
    (satisfies? ObjectPropertyProtocol entity) (prot/add-object-property doc entity)
    :else (throw (ex-info "Cannot add unknown entity type to document" {:type (type entity) :entity entity}))))
