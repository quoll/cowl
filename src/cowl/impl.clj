(ns cowl.impl
  {:doc "Base implementations for the COWL protocols"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [tiara.data :as data :refer [ordered-map ordered-set]]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot]
            [cowl.io :as cio])
  (:import [cowl.protocols DocumentElement Annotatable Streamable Inlineable Property ObjectPropertyProtocol Document]
           [quoll.rdf IRI]))

(def local-id (rdf/iri "#"))
(def initv "0.0.1")

(def default-pre "")

(def owl-annotation-keywords
  #{:rdfs/label :rdfs/comment :rdfs/seeAlso :rdfs/isDefinedBy
    :owl/versionInfo :owl/deprecated :owl/backwardCompatibleWith
    :owl/incompatibleWith :owl/priorVersion})

(def owl-annotation-props (set (map rdf/curie owl-annotation-keywords)))

(def om data/EMPTY_MAP)

(def os data/EMPTY_SET)

(def mm data/EMPTY_MULTI_MAP)

(extend-protocol prot/Element
  Object
  (id [this] (:id this))
  nil
  (id [_] nil)
  IRI
  (id [this] this)
  String
  (id [this] (rdf/iri this))
  clojure.lang.Keyword
  (id [this] this))

(extend-protocol prot/Inlineable
  Object
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false)
  nil
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false))

(extend-type IRI
  prot/Streamable
  (emit [i stream] (cio/write-iri stream i))
  prot/Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] true)
  (object-subproperty-expr? [_] false)
  (object-property? [_] true))

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

(defrecord Annotation [annotations prop value]
  Annotatable
  (annotate [this annotation] (update this :annotations conj annotation))
  Streamable
  (emit [this stream] (cio/write-annotation stream this)))

(defn annotation
  "Creates a type-marked annotation"
  ([prop value]
   (->Annotation nil prop value))
  ([ann prop value]
   (->Annotation (ordered-set ann) prop value))
  ([f s t & r]
   (let [lenr (dec (count r))
         anns (into (ordered-set f s) (when (pos? lenr) (take lenr (cons t r))))
         [prop value] (if (zero? lenr) (ordered-set t (first r)) (drop (dec lenr) r))]
     (->Annotation (into os anns) prop value))))

(defn annotations
  "Get all annotations from the head of a seq"
  [s]
  (take-while #(instance? Annotation %) s))

(defn retrieve-annotation-props
  "Recursively finds all annotations from an expression"
  ([acc expr]
   (if (nil? expr)
     acc
     (if (sequential? expr)
       (reduce retrieve-annotation-props acc expr)
       (let [acc (if (satisfies? Annotatable expr)
                   (retrieve-annotation-props acc (prot/get-annotations expr))
                   acc)]
         (if (instance? Annotation acc)
           (conj acc (:prop acc))
           acc)))))
  ([expr]
   (retrieve-annotation-props os expr)))

(defn mapos
  "Maps all elements in an ordered set, with the result being an ordered set"
  [f s]
  (into os (map f) s))

(defn remap
  "Maps all key/value pairs in an ordered map, with the result being an ordered map"
  [f m]
  (into om (map (juxt f f)) m))

(defn recontextualize-element
  [refn elt]
  (if (satisfies? DocumentElement elt)
    (prot/recontextualize elt refn)
    (refn elt)))

;; id: IRI or InverseObjectProperty
;; annotations: MultiMap
;; super-aprops: OrderedSet
;; equivs: OrderedSet
;; domain: OrderedSet
;; range: OrderedSet
;; disjoints: OrderedSet
;; inverses: OrderedSet
;; fn? inverse-fn? transitive? symmetric? asymmetric? reflexive? irreflexive?: nil or true/annotation
(defrecord ObjectProperty [id annotations super-props equivs domain range disjoints inverses
                           fn? inverse-fn? transitive? symmetric? asymmetric?
                           reflexive? irreflexive?]
  DocumentElement
  (recontextualize [this refn]
    (let [update-element #(recontextualize-element refn %)]
      (cond-> (update this :id update-element)
        (seq super-props) (update :super-props mapos update-element)
        (seq equivs) (update :equivs mapos update-element)
        (seq domain) (update :domain mapos update-element)
        (seq range) (update :range mapos update-element)
        (seq disjoints) (update :disjoints mapos update-element)
        (seq inverses) (update :inverses mapos update-element)
        (sequential? fn?) (update :fn? mapos update-element)
        (sequential? inverse-fn?) (update :inverse-fn? mapos update-element)
        (sequential? transitive?) (update :transitive? mapos update-element)
        (sequential? symmetric?) (update :symmetric? mapos update-element)
        (sequential? asymmetric?) (update :asymmetric? mapos update-element)
        (sequential? reflexive?) (update :reflexive? mapos update-element)
        (sequential? irreflexive?) (update :irreflexive? mapos update-element))))
  Annotatable
  (annotate [this {:keys [prop] :as ann}] (update this :annotations assoc prop ann))
  (annotate [this prop text] (update this :annotations assoc prop text))
  (annotate [_ id prop text]
    (throw (ex-info "Object Properties do not contain other entities" {:id id :prop prop :text text})))
  (get-annotations [_] (vals annotations))
  Property
  (sub-property [this other] (update this :super-props conj other))
  (equivalent [this other] (update this :equivs conj other))
  (domain-of [this other] (update this :domain conj other))
  (range-of [this other] (update this :range conj other))
  (disjoint [this other] (update this :disjoints conj other))
  (inverse [this other] (update this :inverses conj other))
  (functional [this] (assoc this :fn? true))
  ObjectPropertyProtocol
  (inverse-functional [this] (assoc this :inverse-fn? true))
  (transitive [this] (assoc this :transitive? true))
  (symmetric [this] (assoc this :symmetric? true))
  (asymmetric [this] (assoc this :asymmetric? true))
  (reflexive [this] (assoc this :reflexive? true))
  (irreflexive [this] (assoc this :irreflexive? true))
  Streamable
  (emit [this stream] (cio/write-obj-prop stream this)))

(defn obj-property
  [id]
  (->ObjectProperty id mm os os os os os os nil nil nil nil nil nil nil))

(defrecord ObjectPropertyChain [props]
  Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false)
  Streamable
  (emit [_ stream] (cio/write-property-chain stream props)))

(defn property-chain
  [& props]
  (->ObjectPropertyChain props))

(defrecord SubObjectPropertyOf [annotations child parent]
  Inlineable
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] true)
  (object-property? [_] false)
  Streamable
  (emit [this stream] (cio/write-sub-object-property stream this)))

(defn sub-object-property
  "Accepts either child and a list of parents, with an optional annotation as the first argument"
  [& args]
  (let [anns (annotations args)
        [child parent & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to sub-object-property" {:child child :parent parent :extra r})))
    (->SubObjectPropertyOf anns child parent)))

(defrecord InverseObjectProperty [annotations prop]
  Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] true)
  Streamable
  (emit [this stream] (cio/write-inverse-property stream this)))

(defn inverse-obj-prop
  [& args]
  (let [anns (annotations args)]
    (->InverseObjectProperty anns (drop (count anns) args))))

(defrecord EquivalentObjectProperties [annotations id props]
  Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false)
  Streamable
  (emit [this stream] (cio/write-rel-properties stream :equiv :obj this)))

(defn equiv-obj-props
  [& props]
  (let [anns (annotations props)
        [id equivs] (drop (count anns) props)]
    (->EquivalentObjectProperties anns id equivs)))

(defrecord DisjointObjectProperties [annotations id props]
  Inlineable
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false)
  Streamable
  (emit [this stream] (cio/write-rel-properties stream :disjoint :obj this)))

(defn disjoint-obj-props
  [& props]
  (let [anns (annotations props)
        [id equivs] (drop (count anns) props)]
    (->DisjointObjectProperties anns id equivs)))

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
                        instance-idx prefixes annotation-props annotation-axioms datatypes]
  Annotatable
  (annotate [this ann]
    (let [ann* (prot/recontextualize ann #(->iri prefixes %))
          property (:prop ann*)
          new-ann-props (ominus annotation-props (retrieve-annotation-props ann*))]
      (cond-> (update this :annotations assoc property ann*)
        (seq new-ann-props) (update :annotation-props into new-ann-props))))
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
  (get-annotations [_] (map (fn [[k v]] (if (string? v) (annotation k v) v)) annotations))
  Document
  (add-object-property [this {:keys [id] :as prop}]
    (let [->ref #(localized-iri prefixes %)
          prop* (prot/recontextualize prop ->ref)
          annotation-props* (into os (map (comp first ->ref)) (retrieve-annotation-props prop*))
          new-aprops (ominus annotation-props annotation-props*)]
      (cond-> (update this :oprop-idx update id struct-merge prop*)
        (seq new-aprops) (update :annotation-props into new-aprops))))
  Streamable
  (emit [_ stream]
    (cio/write-prefixes stream prefixes)
    (cio/start-doc stream id version)
    (cio/write-doc-annotations stream annotations)
    (cio/write-declarations stream (keys class-idx) (keys oprop-idx) (keys dprop-idx) annotation-props datatypes (keys instance-idx))
    (cio/write-obj-props stream (vals oprop-idx))
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
     (->OWLDocument ont-iri ver-iri om om om mm om pfxs os os os))))


(defn add
  "Adds an element to a document"
  [doc entity]
  (cond
    (instance? Annotation entity) (prot/annotate doc (:prop entity) (:value entity))
    (satisfies? ObjectPropertyProtocol entity) (prot/add-object-property doc entity)
    :else (throw (ex-info "Cannot add unknown entity type to document" {:type (type entity) :entity entity}))))
