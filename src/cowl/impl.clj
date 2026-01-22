(ns cowl.impl
  {:doc "Base implementations for the COWL protocols"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [tiara.data :as data :refer [ordered-map ordered-set]]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot]
            [cowl.io :as cio])
  (:import [cowl.protocols DocumentElement AddressableElement Annotatable TTLStreamable Inlineable Property
            ObjectPropertyProtocol Document]
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

(extend-protocol prot/AddressableElement
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
  prot/TTLStreamable
  (ttl-emit [i stream] (cio/write-iri stream i))
  prot/Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] true)
  (object-subproperty-expr? [_] false)
  (object-property? [_] true))

(defn mapos
  "Maps all elements in an ordered set, with the result being an ordered set"
  [f s]
  (into os (map f) s))

(defn map->maptype
  "Maps all elements in a seqable of pairs into a map object of the provided type"
  ([empty-map f s] (map->maptype empty-map f f s))
  ([empty-map fk fv s]
   (into empty-map (map #(vector (fk (first %)) (fv (second %)))) s)))

(defn mapom
  "Maps all elements in an ordered map, with the result being an ordered map"
  ([f s] (mapom f f s))
  ([fk fv s] (map->maptype om fk fv s)))

(defn mapmm
  "Maps all elements in a multi map, with the result being a multi map"
  ([f s] (mapmm f f s))
  ([fk fv s] (map->maptype mm fk fv s)))

(defn value-deepmap
  "Maps a function down a nested ordered map where the values at the top level are vectors"
  [f m]
  (let [sf (fn [a] (if (sequential? a) (mapv f a) (f a)))]
    (into om (map (fn [[k v]] [k (mapv sf v)])) m)))

(defn recontextualize-annotations
  "Calls recontextualize on an annotations nested array, using refn to update document elements or
  raw entities (IRIs, keywords, etc)"
  [entity refn]
  (update entity :annotations value-deepmap refn))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn recontextualize-element-fn
  "Returns a function that updates document elements to use IRIs with the given prefixes"
  [prefixes]
  (fn refn [elt]
    (when elt
      (if (satisfies? DocumentElement elt)
        (prot/recontextualize elt refn)
        (->iri prefixes elt)))))

(defrecord Annotation [annotations prop value]
  DocumentElement
  (type-label [_] "Annotation")
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :annotations value-deepmap refn)))
  (add-to-parent [this parent] (prot/annotate parent this))
  (add-to-doc [this doc] (prot/annotate doc this))
  Annotatable
  (annotate [this annotation] (update this :annotations conj annotation))
  (annotate [this prop text] (update this :annotations conj (Annotation. nil prop text)))
  (annotate [_ id prop text] (ex-info "Annotations do not contain other entities" {:id id :prop prop :text text}))
  (get-annotations [_] annotations)
  TTLStreamable
  (ttl-emit [this stream] (cio/write-annotation stream this)))

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

(defn prop-attr
  [obj index other]
  (assert (= (prot/id obj) (prot/id other)))
  (-> obj
      (update index conj other)
      (update-in [:annotations index] conj (:annotations other))))

(defn prop-bool-attr
  ([obj index] (prop-bool-attr obj index nil))
  ([obj index attr]
   (-> obj
       (assoc index attr)
       (update-in [:annotations index] conj attr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ObjectProperties ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {:_id (s/or IRI ObjectInverseOf)
;;  :annotations {:annotations [ [Annotation] ]
;;                :super-aprops [ [Annotation] ]
;;                :equivs [ [Annotation] ]
;;                :domain [ [Annotation] ]
;;                :range [ [Annotation] ]
;;                :disjoints [ [Annotation] ]
;;                :inverses [ [Annotation] ]
;;                :fn? [Annotation], :inverse-fn? [Annotation], :transitive? [Annotation], :symmetric? [Annotation],
;;                :asymmetric? [Annotation], :reflexive? [Annotation], :irreflexive? [Annotation]}
;;  :super-aprops: OrderedSet
;;  :equivs: OrderedSet
;;  :domain: OrderedSet
;;  :range: OrderedSet
;;  :disjoints: OrderedSet
;;  :inverses: OrderedSet
;;  :fn? boolean, :inverse-fn? boolean, :transitive? boolean, :symmetric? boolean,
;;  :asymmetric? boolean, :reflexive? boolean, :irreflexive? boolean }
(defrecord ObjectProperty [_id annotations super-props equivs domain range disjoints inverses
                           fn? inverse-fn? transitive? symmetric? asymmetric?
                           reflexive? irreflexive?]
  AddressableElement
  (id [_] _id)
  DocumentElement
  (type-label [_] "ObjectProperty")
  (recontextualize [this refn]
    (cond-> (update this :_id refn)
      (seq annotations) (recontextualize-annotations refn)
      (seq super-props) (update :super-props mapos refn)
      (seq equivs) (update :equivs mapos refn)
      (seq domain) (update :domain mapos refn)
      (seq range) (update :range mapos refn)
      (seq disjoints) (update :disjoints refn)
      (seq inverses) (update :inverses refn)))
  (add-to-parent [this parent] (prot/add-object-property parent this))
  (add-to-doc [this doc] (prot/add-object-property doc this))
  Annotatable
  (annotate [this {:keys [prop] :as ann}] (update-in this [:annotations :annotations] assoc prop ann))
  (annotate [this prop text] (update-in this [:annotations :annotations] assoc prop (annotation prop text)))
  (annotate [_ id prop text]
    (throw (ex-info "Object Properties do not contain other entities" {:id id :prop prop :text text})))
  (get-annotations [_] (vals (get annotations :annotations)))
  Property
  (sub-property [this other] (prop-attr this :super-props other))
  (equivalent [this other] (prop-attr this :equivs other))
  (domain-of [this other] (prop-attr this :domain other))
  (range-of [this other] (prop-attr this :range other))
  (disjoint [this other] (prop-attr this :disjoints other))
  (functional [this] (assoc this :fn? true))
  (functional [this anns] (prop-bool-attr this :fn? anns))
  ObjectPropertyProtocol
  (inverse [this other] (prop-attr this :inverses other))
  (inverse-functional [this] (assoc this :inverse-fn? true))
  (inverse-functional [this anns] (prop-bool-attr this :inverse-fn? anns))
  (transitive [this] (assoc this :transitive? true))
  (transitive [this anns] (prop-bool-attr this :transitive? anns))
  (symmetric [this] (assoc this :symmetric? true))
  (symmetric [this anns] (prop-bool-attr this :symmetric? anns))
  (asymmetric [this] (assoc this :asymmetric? true))
  (asymmetric [this anns] (prop-bool-attr this :asymmetric? anns))
  (reflexive [this] (assoc this :reflexive? true))
  (reflexive [this anns] (prop-bool-attr this :reflexive? anns))
  (irreflexive [this] (assoc this :irreflexive? true))
  (irreflexive [this anns] (prop-bool-attr this :irreflexive? anns))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-obj-prop stream this)))

(defn object-property
  [& args]
  (let [anns (annotations args)
        [_id] (drop (count anns) args)]
    (->ObjectProperty _id {:annotations [anns]} os os os os os os false false false false false false false)))

(defn ensure-object-prop-in-doc
  [doc prop]
  (cond-> doc
    ;; If the property is new to the document, update the doc to know about it
    (nil? (prot/get-object-property doc prop)) (prot/add-object-property (object-property prop))))

(defrecord SubObjectPropertyOf [annotations prop super-property]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :super-property refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "SubObjectPropertyOf")
  (add-to-parent [this parent] (prot/sub-property parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-object-property doc prop)
                 ;; exists in the doc, so update it in the doc
                 (update-in doc [:oprop-idx prop] prot/add-to-parent this)
                 ;; does not yet exist, so create, then add to the doc
                 (prot/add-object-property doc (prot/sub-property (object-property prop) this)))]
      (ensure-object-prop-in-doc doc* super-property)))
  Inlineable
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] true)
  (object-property? [_] false)
  TTLStreamable
  (ttl-emit [this stream] (cio/write-sub-object-property stream this)))

(defn sub-object-property
  "Accepts either child and a list of parents, with an optional annotation as the first argument"
  [& args]
  (let [anns (annotations args)
        [child parent & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to sub-object-property" {:child child :parent parent :extra r})))
    (->SubObjectPropertyOf anns child parent)))

(defrecord ObjectPropertyChain [props]
  DocumentElement
  (recontextualize [this refn] (update this :props mapv refn))
  (type-label [_] "ObjectPropertyChain")
  (add-to-parent [this parent]
    (if (instance? SubObjectPropertyOf parent)
      (assoc parent :child this)
      (throw (ex-info "ObjectPropertyChain can only be added to a SubObjectProperty"
                      {:parent parent :parent-type (type parent)}))))
  (add-to-doc [this _]
    (throw (ex-info "ObjectPropertyChain is not an axiom. Associate it with a property."
                    {:chain this})))
  Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false)
  TTLStreamable
  (ttl-emit [_ stream] (cio/write-property-chain stream props)))

(defn property-chain
  [& props]
  (->ObjectPropertyChain props))

(defrecord InverseObjectProperties [annotations prop inv-prop]
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :inv-prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "InverseObjectProperties")
  (add-to-parent [this parent] (prot/add-object-property parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-object-property doc prop)
                 ;; exists in the doc, so update it in the doc
                 (update-in doc [:oprop-idx prop] prot/add-to-parent this)
                 ;; does not yet exist, so create, then add to the doc
                 (prot/add-object-property doc (prot/inverse (object-property prop) inv-prop)))]
      (ensure-object-prop-in-doc doc* inv-prop)))
  Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] true)
  TTLStreamable
  (ttl-emit [this stream] (cio/write-inverse-property stream this)))

(defn inverse-obj-props
  [& args]
  (let [anns (annotations args)
        [prop inv-prop :as remaining] (drop (count anns) args)]
    (when (seq remaining)
      (throw (ex-info "Too many arguments to inverse-obj-props" {:args remaining})))
    (->InverseObjectProperties anns prop inv-prop)))

(defmulti add-obj-property-to-parent (fn [parent _] (type parent)))

(defmethod add-obj-property-to-parent :default
  [parent child]
  (assoc parent :prop child))

(defmethod add-obj-property-to-parent ObjectPropertyChain
  [parent child]
  (update parent :props conj child))

(defmethod add-obj-property-to-parent InverseObjectProperties
  [{:keys [prop] :as parent} child]
  ;; this is an unusual workflow
  (if prop
    ;; if this is at least a property in the InverseObjectProperties, then set the inverse
    (assoc parent :inv-prop child)
    ;; if there is no initial property in the InverseObjectProperties, then set the initial property
    (assoc parent :prop child)))

;; This record can be used anywhere a property IRI can be
(defrecord ObjectInverseOf [annotations prop]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "ObjectInverseOf")
  (add-to-parent [this parent] (add-obj-property-to-parent parent this))
  (add-to-doc [this _]
    (throw (ex-info "ObjectInverseOf is not an axiom. Associate it with a property." {:inverse-prop this})))
  Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] true)
  TTLStreamable
  (ttl-emit [this stream] (cio/write-inverse-property stream this)))

(defn inverse-obj-prop
  [& args]
  (let [anns (annotations args)]
    (->ObjectInverseOf anns (drop (count anns) args))))

;; This record exists only to rewrite a object property
(defrecord EquivalentObjectProperties [annotations prop props]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :props refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "EquivalentObjectProperties")
  (add-to-parent [this parent] (prot/add-object-property parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-object-property doc prop)
                 ;; exists in the doc, so update it in the doc
                 (update-in doc [:oprop-idx prop] prot/add-to-parent this)
                 ;; does not yet exist, so create, then add to the doc
                 (let [new-prop (reduce prot/equivalent (object-property prop) props)]
                   (prot/add-object-property doc new-prop)))]
      (reduce ensure-object-prop-in-doc doc* props)))
  Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false)
  TTLStreamable
  (ttl-emit [this stream] (cio/write-rel-properties stream :equiv :obj this)))

(defn equiv-obj-props
  [& props]
  (let [anns (annotations props)
        [_id equivs] (drop (count anns) props)]
    (->EquivalentObjectProperties anns _id equivs)))

(defrecord DisjointObjectProperties [annotations prop props]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :props refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "DisjointObjectProperties")
  (add-to-parent [this parent] (prot/add-object-property parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-object-property doc prop)
                 ;; exists in the doc, so update it in the doc
                 (update-in doc [:oprop-idx prop] prot/add-to-parent this)
                 ;; does not yet exist, so create, then add to the doc
                 (let [new-prop (reduce prot/disjoint (object-property prop) props)]
                   (prot/add-object-property doc new-prop)))]
      (reduce ensure-object-prop-in-doc doc* props)))
  Inlineable
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false)
  TTLStreamable
  (ttl-emit [this stream] (cio/write-rel-properties stream :disjoint :obj this)))

(defn disjoint-obj-props
  [& props]
  (let [anns (annotations props)
        [_id equivs] (drop (count anns) props)]
    (->DisjointObjectProperties anns _id equivs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Data Properties ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord DataProperty [prop annotations super-props equivs disjoints domain range fn?]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn]
    (cond-> (update this :prop refn)
      (seq annotations) (recontextualize-annotations refn)
      (seq super-props) (update :super-props mapos refn)
      (seq equivs) (update :equivs mapos refn)
      (seq domain) (update :domain mapos refn)
      (seq range) (update :range mapos refn)
      (seq disjoints) (update :disjoints mapos refn)))
  (type-label [_] "DataProperty")
  (add-to-parent [this parent] (prot/add-data-property parent this))
  (add-to-doc [this doc] (prot/add-data-property doc this))
  Annotatable
  (annotate [this {:keys [prop] :as ann}] (update-in this [:annotations :annotations] assoc prop ann))
  (annotate [this prop text] (update-in this [:annotations :annotations] assoc prop (annotation prop text)))
  (annotate [_ id prop text]
    (throw (ex-info "Object Properties do not contain other entities" {:id id :prop prop :text text})))
  (get-annotations [_] (vals (get annotations :annotations)))
  Property
  (sub-property [this other] (prop-attr this :super-props other))
  (equivalent [this other] (prop-attr this :equivs other))
  (domain-of [this other] (prop-attr this :domain other))
  (range-of [this other] (prop-attr this :range other))
  (disjoint [this other] (prop-attr this :disjoints other))
  (functional [this] (assoc this :fn? true))
  (functional [this anns] (prop-bool-attr this :fn? anns)))

(defn data-property
  [& args]
  (let [anns (annotations args)
        [_id] (drop (count anns) args)]
    (->DataProperty _id {:annotations [anns]} os os os os os false)))

(defn ensure-data-prop-in-doc
  [doc prop]
  (cond-> doc
    ;; If the property is new to the document, update the doc to know about it
    (nil? (prot/get-data-property doc prop)) (prot/add-data-property (data-property prop))))

(defrecord SubDataPropertyOf [annotations prop super-prop]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :super-prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "SubDataPropertyOf")
  (add-to-parent [this parent] (prot/sub-property parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-data-property doc prop)
                 ;; exists in the doc, so update it in the doc
                 (update-in doc [:dprop-idx prop] prot/add-to-parent this)
                 ;; does not yet exist, so create, then add to the doc
                 (prot/add-data-property doc (prot/sub-property (data-property prop) this)))]
      (ensure-data-prop-in-doc doc* super-prop)))
  Inlineable
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] true)
  (object-property? [_] false)
  TTLStreamable
  (ttl-emit [this stream] (cio/write-sub-object-property stream this)))

(defn sub-data-property
  "Accepts either child and a list of parents, with an optional annotation as the first argument"
  [& args]
  (let [anns (annotations args)
        [child parent & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to sub-object-property" {:child child :parent parent :extra r})))
    (->SubDataPropertyOf anns child parent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord OWLClass [cls super-classes equivs disjoints disjoint-union]
  AddressableElement
  (id [_] cls)
  DocumentElement
  (recontextualize [this refn]
    (cond-> (update this :cls refn)
      (seq super-classes) (update :super-classes mapos refn)
      (seq equivs) (update :equivs mapos refn)
      (seq disjoints) (update :disjoints mapos refn)
      (seq disjoint-union) (update :disjoint-union mapos refn))))

(defn owl-class
  [_id]
  (->OWLClass _id om om om om))

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
  (let [refn (recontextualize-element-fn prefixes)]
    (-> document
        (update :_id refn)
        (update :version refn)
        (update :annotations mapmm refn)
        (update :class-idx mapom refn)
        (update :oprop-idx mapom refn)
        (update :dprop-idx mapom refn)
        (update :instance-idx mapom refn)
        (update :annotation-props mapos refn)
        (update :annotation-axioms mapos refn)
        (update :datatypes mapos refn))))

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
         ont-iri (if id (localized-iri pfxs id) local-id)
         vi (as-namespace (:iri ont-iri))
         ver-iri (if version
                   (localized-iri pfxs
                                  (if (str/index-of version "/") ;; proxy for an IRI form
                                    version
                                    (str vi version)))
                   (rdf/iri (str vi initv)))
         doc (->Ontology ont-iri ver-iri om om om mm om pfxs os os os)]
     (reduce add doc elements))))

