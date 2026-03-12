(ns cowl.impl.common
  {:doc "Common utilities shared by property implementations"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [tiara.data :as data]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot]
            [cowl.io.iop :as iop :refer [EmbeddedAnnotation]])
  (:import [quoll.rdf IRI]))

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

(defn mapos
  "Maps all elements in an ordered set, with the result being an ordered set"
  [f s]
  (into os (map f) s))

(defn map-across-ann
  "Maps a function down vectors of annotations"
  [a f]
  (if (sequential? a) (mapv f a) (f a)))

(defn recontextualize-annotations
  "Calls recontextualize on an annotations nested array, using refn to update document elements or
  raw entities (IRIs, keywords, etc)"
  [{:keys [annotations] :as entity} refn]
  (let [anns (update annotations :annotations #(into om (map (fn [[k a]] [(refn k) (refn a)])) %))
        map-update (fn [m] (into {} (map (fn [[k v]] [k (map-across-ann v refn)])) m))]
    (-> entity
        (dissoc :annotations)
        map-update
        (assoc :annotations anns))))

(defn prop-attr-binary
  "Updates attributes for a property, keeping annotations in sync"
  [obj index {:keys [annotations other] :as arg}]
  (assert (= (prot/id obj) (prot/id arg)))
  (-> obj
      (update index conj other)
      (update-in [:annotations index] conj annotations)))

(defn prop-attr-multi
  "Updates attributes for a property, keeping annotations in sync"
  [obj index {:keys [annotations props]}]
  (-> obj
      (update index (fnil into []) props)
      (update-in [:annotations index] conj annotations)))

(defn prop-bool-attr
  "Updates a boolean attribute for a property, keeping annotations in sync"
  ([obj index] (prop-bool-attr obj index nil))
  ([obj index attr]
   (-> obj
       (assoc index attr)
       (update-in [:annotations index] conj attr))))

(defn add-object-prop-to-doc
  "Common implementation for add-to-doc for object property axiom records with [annotations prop other] fields.

  Parameters:
  - this: the axiom record being added
  - doc: the document to add to
  - prop: the property ID from the record
  - other: the other property/entity from the record (may be nil)
  - prop-constructor: function to construct a new property (e.g., object-property)
  - modifier-fn: function that takes a property and returns the modified property (e.g., #(prot/sub-property % this))
  - ensure-fn: optional function to ensure 'other' exists in doc (may be nil)"
  [this doc prop other prop-constructor modifier-fn ensure-fn]
  (let [doc* (if (prot/get-object-property doc prop)
               (update-in doc [:oprop-idx prop] prot/add-to-parent this)
               (prot/add-object-property doc (modifier-fn (prop-constructor prop))))]
    (if (and other ensure-fn)
      (ensure-fn doc* other)
      doc*)))

(defn add-data-prop-to-doc
  "Common implementation for add-to-doc for data property axiom records with [annotations prop other] fields.

  Parameters:
  - this: the axiom record being added
  - doc: the document to add to
  - prop: the property ID from the record
  - other: the other property/entity from the record (may be nil)
  - prop-constructor: function to construct a new property (e.g., data-property)
  - modifier-fn: function that takes a property and returns the modified property (e.g., #(prot/sub-property % this))
  - ensure-fn: optional function to ensure 'other' exists in doc (may be nil)"
  [this doc prop other prop-constructor modifier-fn ensure-fn]
  (let [doc* (if (prot/get-data-property doc prop)
               (update-in doc [:dprop-idx prop] prot/add-to-parent this)
               (prot/add-data-property doc (modifier-fn (prop-constructor prop))))]
    (if (and other ensure-fn)
      (ensure-fn doc* other)
      doc*)))

(defn add-class-to-doc
  "Common implementation for add-to-doc for class axiom records.

  Parameters:
  - this: the axiom record being added
  - doc: the document to add to
  - cls: the class expression from the record
  - other-or-exprs: either a single class expression or a vector of expressions
  - class-constructor: function to construct a new class (e.g., owl-class)
  - modifier-fn: function that takes a class and returns the modified class (e.g., #(prot/sub-class % this))
  - ensure-fn: function to ensure classes from expression exist in doc
  - multi?: true if other-or-exprs is a vector to reduce over"
  [this doc cls other-or-exprs class-constructor modifier-fn ensure-fn multi?]
  (let [cls-id (prot/id cls)
        doc* (if (and cls-id (prot/get-class doc cls-id))
               (update-in doc [:class-idx cls-id] prot/add-to-parent this)
               (if cls-id
                 (prot/add-class doc (modifier-fn (class-constructor cls-id)))
                 doc))]
    (if multi?
      (reduce ensure-fn doc* other-or-exprs)
      (ensure-fn doc* other-or-exprs))))

(defn add-class-to-doc
  "Common implementation for add-to-doc for class axiom records with [annotations cls other] or [annotations cls exprs] fields.

  Parameters:
  - this: the axiom record being added
  - doc: the document to add to
  - cls: the class expression from the record
  - other-or-exprs: either a single class expression (for binary axioms) or a vector of expressions (for multi axioms)
  - class-constructor: function to construct a new class (e.g., owl-class)
  - modifier-fn: function that takes a class and returns the modified class (e.g., #(prot/sub-class % this))
  - ensure-fn: optional function to ensure classes from expressions exist in doc (may be nil)
  - multi?: true if other-or-exprs is a collection that should be reduced over for ensuring"
  [this doc cls other-or-exprs class-constructor modifier-fn ensure-fn multi?]
  (let [cls-id (prot/id cls)
        doc* (if (prot/get-class doc cls-id)
               (update-in doc [:class-idx cls-id] prot/add-to-parent this)
               (prot/add-class doc (modifier-fn (class-constructor cls-id))))]
    (if (and ensure-fn (or other-or-exprs multi?))
      (if multi?
        (reduce ensure-fn doc* other-or-exprs)
        (ensure-fn doc* other-or-exprs))
      doc*)))

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

(extend-type IRI  ;; extending the interface
  prot/Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] true)
  (object-subproperty-expr? [_] false)
  (object-property? [_] true)
  prot/ClassExpression
  (get-classes [this] this))

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
      (if (satisfies? prot/DocumentElement elt)
        (prot/recontextualize elt refn)
        (->iri prefixes elt)))))

(defrecord Annotation [annotations prop value]
  EmbeddedAnnotation
  prot/DocumentElement
  (type-label [_] "Annotation")
  (recontextualize [this refn] (cond-> (update this :prop refn)
                                   (seq annotations) (recontextualize-annotations refn)))
  (add-to-parent [this parent] (prot/annotate parent this))
  (add-to-doc [this doc] (prot/annotate doc this))
  prot/Annotatable
  (annotate [this annotation] (update this :annotations conj annotation))
  (annotate [this prop text] (update this :annotations conj (Annotation. nil prop text)))
  (annotate [_ id prop text] (ex-info "Annotations do not contain other entities" {:id id :prop prop :text text}))
  (get-annotations [_] annotations))

(extend-protocol prot/AnnotationTest
  Object
  (annotation? [_] false))

(defn annotation
  "Creates a type-marked annotation"
  ([prop value]
   (->Annotation nil prop value))
  ([ann prop value]
   (->Annotation (data/ordered-set ann) prop value))
  ([f s t & r]
   (let [lenr (dec (count r))
         anns (into (data/ordered-set f s) (when (pos? lenr) (take lenr (cons t r))))
         [prop value] (if (zero? lenr) (data/ordered-set t (first r)) (drop (dec lenr) r))]
     (->Annotation (into os anns) prop value))))

(defn leading-annotations
  "Get all annotations from the head of a seq"
  [s]
  (take-while #(prot/annotation? %) s))

(defn annotation-map
  "Gets all annotations from the head of a seq, and multi-map index on the :prop key"
  [s]
  (let [kv-xf (comp (take-while #(instance? Annotation %))
                    (map #(vector (:prop %) %)))]
    (into mm kv-xf s)))

(defn retrieve-annotation-props
  "Recursively finds all annotations from an expression"
  ([acc expr]
   (if (nil? expr)
     acc
     (if (sequential? expr)
       (reduce retrieve-annotation-props acc expr)
       (let [acc (if (prot/annotation? expr)
                   (retrieve-annotation-props acc (prot/get-annotations expr))
                   acc)]
         (if (instance? Annotation acc)
           (conj acc (:prop acc))
           acc)))))
  ([expr]
   (retrieve-annotation-props os expr)))
