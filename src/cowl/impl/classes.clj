(ns cowl.impl.classes
  {:doc "Class implementations for COWL"
   :author "Paula Gearon"}
  (:require [cowl.protocols :as prot]
            [cowl.impl.common :as common :refer [os mapos recontextualize-annotations
                                                 annotation-map annotations]])
  (:import [cowl.protocols DocumentElement AddressableElement ClassExpression ClassProtocol]))

(defn class-attr-binary
  "Updates attributes for a class with a single class expression, keeping annotations in sync"
  [obj index {:keys [annotations other]}]
  (-> obj
      (update index conj other)
      (update-in [:annotations index] conj annotations)))

(defn class-attr-multi
  "Updates attributes for a class with multiple class expressions, keeping annotations in sync"
  [obj index {:keys [annotations exprs]}]
  (-> obj
      (update index (fnil into os) exprs)
      (update-in [:annotations index] conj annotations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {:cls IRI
;;  :annotations {:annotations {s/keyword Annotation}
;;                :super-classes [ [Annotation] ]
;;                :equivs [ [Annotation] ]
;;                :disjoints [ [Annotation] ]
;;                :disjoint-union [ [Annotation] ]}
;;  :super-classes: OrderedSet
;;  :equivs: OrderedSet
;;  :disjoints: OrderedSet
;;  :disjoint-union: OrderedSet }
(defrecord OWLClass [cls annotations super-classes equivs disjoints disjoint-union]
  AddressableElement
  (id [_] cls)
  DocumentElement
  (type-label [_] "Class")
  (recontextualize [this refn]
    (cond-> (update this :cls refn)
      (seq annotations) (common/recontextualize-annotations refn)
      (seq super-classes) (update :super-classes mapos refn)
      (seq equivs) (update :equivs mapos refn)
      (seq disjoints) (update :disjoints mapos refn)
      (seq disjoint-union) (update :disjoint-union mapos refn)))
  (add-to-parent [this doc] (prot/add-class doc this))
  (add-to-doc [this doc] (prot/add-class doc this))
  ClassProtocol
  (sub-class [this other] (class-attr-binary this :super-classes other))
  (equivalent-class [this other] (class-attr-multi this :equivs other))
  (disjoint-class [this other] (class-attr-multi this :disjoints other))
  (disjoint-union-class [this other] (class-attr-multi this :disjoint-union other)))

(defn owl-class
  ([_id]
   (->OWLClass _id {:annotations []} os os os os))
  ([a & args]
   (let [anns (annotation-map (cons a args))
         [_id] (drop (dec (count anns)) args)]
     (->OWLClass _id {:annotations anns} os os os os))))

(defn ensure-class-in-doc
  [doc cls]
  (cond-> doc
    ;; If the class is new to the document, update the doc to know about it
    (nil? (prot/get-class doc cls)) (prot/add-class (owl-class cls))))

(defn ensure-classes-from-expr
  "Ensures all class IRIs from a class expression exist in the document"
  [doc expr]
  (let [classes (prot/get-classes expr)]
    (if (set? classes)
      (reduce ensure-class-in-doc doc classes)
      (ensure-class-in-doc doc classes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Class Axioms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SubClassOf := 'SubClassOf' '(' axiomAnnotations subClassExpression superClassExpression ')'
(defrecord SubClassOf [annotations cls other]
  AddressableElement
  (id [_] (prot/id cls))
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :cls refn)
                                   (update :other refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "SubClassOf")
  (add-to-parent [this parent] (prot/sub-class parent this))
  (add-to-doc [this doc]
    (common/add-class-to-doc this doc cls other
                             owl-class
                             #(prot/sub-class % this)
                             ensure-classes-from-expr
                             false)))

(defn sub-class-of
  [& args]
  (let [anns (annotations args)
        [sub-expr super-expr & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to sub-class-of" {:sub sub-expr :super super-expr :extra r})))
    (->SubClassOf anns sub-expr super-expr)))

;; EquivalentClasses := 'EquivalentClasses' '(' axiomAnnotations ClassExpression ClassExpression { ClassExpression } ')'
(defrecord EquivalentClasses [annotations cls exprs]
  AddressableElement
  (id [_] (prot/id cls))
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :cls refn)
                                   (update :exprs #(mapv refn %))
                                   (recontextualize-annotations refn)))
  (type-label [_] "EquivalentClasses")
  (add-to-parent [this parent] (prot/equivalent-class parent this))
  (add-to-doc [this doc]
    (common/add-class-to-doc this doc cls exprs
                             owl-class
                             #(reduce prot/equivalent-class % exprs)
                             ensure-classes-from-expr
                             true)))

(defn equivalent-classes
  [& exprs]
  (let [anns (annotations exprs)
        [cls & rest-exprs] (drop (count anns) exprs)]
    (when (< (count rest-exprs) 1)
      (throw (ex-info "EquivalentClasses requires at least 2 class expressions" {:exprs exprs})))
    (->EquivalentClasses anns cls (vec rest-exprs))))

;; DisjointClasses := 'DisjointClasses' '(' axiomAnnotations ClassExpression ClassExpression { ClassExpression } ')'
(defrecord DisjointClasses [annotations cls exprs]
  AddressableElement
  (id [_] (prot/id cls))
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :cls refn)
                                   (update :exprs #(mapv refn %))
                                   (recontextualize-annotations refn)))
  (type-label [_] "DisjointClasses")
  (add-to-parent [this parent] (prot/disjoint-class parent this))
  (add-to-doc [this doc]
    (common/add-class-to-doc this doc cls exprs
                             owl-class
                             #(reduce prot/disjoint-class % exprs)
                             ensure-classes-from-expr
                             true)))

(defn disjoint-classes
  [& exprs]
  (let [anns (annotations exprs)
        [cls & rest-exprs] (drop (count anns) exprs)]
    (when (< (count rest-exprs) 1)
      (throw (ex-info "DisjointClasses requires at least 2 class expressions" {:exprs exprs})))
    (->DisjointClasses anns cls (vec rest-exprs))))

;; DisjointUnion := 'DisjointUnion' '(' axiomAnnotations Class disjointClassExpressions ')'
(defrecord DisjointUnion [annotations cls exprs]
  AddressableElement
  (id [_] (prot/id cls))
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :cls refn)
                                   (update :exprs #(mapv refn %))
                                   (recontextualize-annotations refn)))
  (type-label [_] "DisjointUnion")
  (add-to-parent [this parent] (prot/disjoint-union-class parent this))
  (add-to-doc [this doc]
    (common/add-class-to-doc this doc cls exprs
                             owl-class
                             #(reduce prot/disjoint-union-class % exprs)
                             ensure-classes-from-expr
                             true)))

(defn disjoint-union
  [& args]
  (let [anns (annotations args)
        [cls & exprs] (drop (count anns) args)]
    (when (< (count exprs) 2)
      (throw (ex-info "DisjointUnion requires a class and at least 2 disjoint expressions" {:args args})))
    (->DisjointUnion anns cls (vec exprs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Class Expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ObjectIntersectionOf := 'ObjectIntersectionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
(defrecord ObjectIntersectionOf [exprs]
  DocumentElement
  (type-label [_] "ObjectIntersectionOf")
  (recontextualize [this refn] (update this :exprs #(mapv refn %)))
  (add-to-parent [this parent] (throw (ex-info "ObjectIntersectionOf cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectIntersectionOf cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] (into os (mapcat prot/get-classes) exprs)))

(defn object-intersection-of
  [& exprs]
  (when (< (count exprs) 2)
    (throw (ex-info "ObjectIntersectionOf requires at least 2 class expressions" {:exprs exprs})))
  (->ObjectIntersectionOf exprs))

;; ObjectUnionOf := 'ObjectUnionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
(defrecord ObjectUnionOf [exprs]
  DocumentElement
  (type-label [_] "ObjectUnionOf")
  (recontextualize [this refn] (update this :exprs #(mapv refn %)))
  (add-to-parent [this parent] (throw (ex-info "ObjectUnionOf cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectUnionOf cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] (into os (mapcat prot/get-classes) exprs)))

(defn object-union-of
  [& exprs]
  (when (< (count exprs) 2)
    (throw (ex-info "ObjectUnionOf requires at least 2 class expressions" {:exprs exprs})))
  (->ObjectUnionOf exprs))

;; ObjectComplementOf := 'ObjectComplementOf' '(' ClassExpression ')'
(defrecord ObjectComplementOf [expr]
  DocumentElement
  (type-label [_] "ObjectComplementOf")
  (recontextualize [this refn] (update this :expr refn))
  (add-to-parent [this parent] (throw (ex-info "ObjectComplementOf cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectComplementOf cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] (prot/get-classes expr)))

(defn object-complement-of
  [expr]
  (->ObjectComplementOf expr))

;; ObjectOneOf := 'ObjectOneOf' '(' Individual { Individual }')'
(defrecord ObjectOneOf [individuals]
  DocumentElement
  (type-label [_] "ObjectOneOf")
  (recontextualize [this refn] (update this :individuals #(mapv refn %)))
  (add-to-parent [this parent] (throw (ex-info "ObjectOneOf cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectOneOf cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] os))

(defn object-one-of
  [& individuals]
  (when (empty? individuals)
    (throw (ex-info "ObjectOneOf requires at least 1 individual" {:individuals individuals})))
  (->ObjectOneOf individuals))

;; ObjectSomeValuesFrom := 'ObjectSomeValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
(defrecord ObjectSomeValuesFrom [prop expr]
  DocumentElement
  (type-label [_] "ObjectSomeValuesFrom")
  (recontextualize [this refn] (-> this (update :prop refn) (update :expr refn)))
  (add-to-parent [this parent] (throw (ex-info "ObjectSomeValuesFrom cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectSomeValuesFrom cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] (prot/get-classes expr)))

(defn object-some-values-from
  [prop expr]
  (->ObjectSomeValuesFrom prop expr))

;; ObjectAllValuesFrom := 'ObjectAllValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
(defrecord ObjectAllValuesFrom [prop expr]
  DocumentElement
  (type-label [_] "ObjectAllValuesFrom")
  (recontextualize [this refn] (-> this (update :prop refn) (update :expr refn)))
  (add-to-parent [this parent] (throw (ex-info "ObjectAllValuesFrom cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectAllValuesFrom cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] (prot/get-classes expr)))

(defn object-all-values-from
  [prop expr]
  (->ObjectAllValuesFrom prop expr))

;; ObjectHasValue := 'ObjectHasValue' '(' ObjectPropertyExpression Individual ')'
(defrecord ObjectHasValue [prop individual]
  DocumentElement
  (type-label [_] "ObjectHasValue")
  (recontextualize [this refn] (-> this (update :prop refn) (update :individual refn)))
  (add-to-parent [this parent] (throw (ex-info "ObjectHasValue cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectHasValue cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] os))

(defn object-has-value
  [prop individual]
  (->ObjectHasValue prop individual))

;; ObjectHasSelf := 'ObjectHasSelf' '(' ObjectPropertyExpression ')'
(defrecord ObjectHasSelf [prop]
  DocumentElement
  (type-label [_] "ObjectHasSelf")
  (recontextualize [this refn] (update this :prop refn))
  (add-to-parent [this parent] (throw (ex-info "ObjectHasSelf cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectHasSelf cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] os))

(defn object-has-self
  [prop]
  (->ObjectHasSelf prop))

;; ObjectMinCardinality := 'ObjectMinCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
(defrecord ObjectMinCardinality [n prop expr]
  DocumentElement
  (type-label [_] "ObjectMinCardinality")
  (recontextualize [this refn] (-> this (update :prop refn) (update :expr #(when % (refn %)))))
  (add-to-parent [this parent] (throw (ex-info "ObjectMinCardinality cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectMinCardinality cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] (if expr (prot/get-classes expr) os)))

(defn object-min-cardinality
  ([n prop] (->ObjectMinCardinality n prop nil))
  ([n prop expr] (->ObjectMinCardinality n prop expr)))

;; ObjectMaxCardinality := 'ObjectMaxCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
(defrecord ObjectMaxCardinality [n prop expr]
  DocumentElement
  (type-label [_] "ObjectMaxCardinality")
  (recontextualize [this refn] (-> this (update :prop refn) (update :expr #(when % (refn %)))))
  (add-to-parent [this parent] (throw (ex-info "ObjectMaxCardinality cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectMaxCardinality cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] (if expr (prot/get-classes expr) os)))

(defn object-max-cardinality
  ([n prop] (->ObjectMaxCardinality n prop nil))
  ([n prop expr] (->ObjectMaxCardinality n prop expr)))

;; ObjectExactCardinality := 'ObjectExactCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
(defrecord ObjectExactCardinality [n prop expr]
  DocumentElement
  (type-label [_] "ObjectExactCardinality")
  (recontextualize [this refn] (-> this (update :prop refn) (update :expr #(when % (refn %)))))
  (add-to-parent [this parent] (throw (ex-info "ObjectExactCardinality cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "ObjectExactCardinality cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] (if expr (prot/get-classes expr) os)))

(defn object-exact-cardinality
  ([n prop] (->ObjectExactCardinality n prop nil))
  ([n prop expr] (->ObjectExactCardinality n prop expr)))

;; DataSomeValuesFrom := 'DataSomeValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
(defrecord DataSomeValuesFrom [props data-range]
  DocumentElement
  (type-label [_] "DataSomeValuesFrom")
  (recontextualize [this refn] (-> this (update :props #(mapv refn %)) (update :data-range refn)))
  (add-to-parent [this parent] (throw (ex-info "DataSomeValuesFrom cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "DataSomeValuesFrom cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] os))

(defn data-some-values-from
  [& args]
  (when (< (count args) 2)
    (throw (ex-info "DataSomeValuesFrom requires at least 1 data property and 1 data range" {:args args})))
  (let [data-range (last args)
        props (vec (butlast args))]
    (->DataSomeValuesFrom props data-range)))

;; DataAllValuesFrom := 'DataAllValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
(defrecord DataAllValuesFrom [props data-range]
  DocumentElement
  (type-label [_] "DataAllValuesFrom")
  (recontextualize [this refn] (-> this (update :props #(mapv refn %)) (update :data-range refn)))
  (add-to-parent [this parent] (throw (ex-info "DataAllValuesFrom cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "DataAllValuesFrom cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] os))

(defn data-all-values-from
  [& args]
  (when (< (count args) 2)
    (throw (ex-info "DataAllValuesFrom requires at least 1 data property and 1 data range" {:args args})))
  (let [data-range (last args)
        props (vec (butlast args))]
    (->DataAllValuesFrom props data-range)))

;; DataHasValue := 'DataHasValue' '(' DataPropertyExpression Literal ')'
(defrecord DataHasValue [prop literal]
  DocumentElement
  (type-label [_] "DataHasValue")
  (recontextualize [this refn] (update this :prop refn))
  (add-to-parent [this parent] (throw (ex-info "DataHasValue cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "DataHasValue cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] os))

(defn data-has-value
  [prop literal]
  (->DataHasValue prop literal))

;; DataMinCardinality := 'DataMinCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
(defrecord DataMinCardinality [n prop data-range]
  DocumentElement
  (type-label [_] "DataMinCardinality")
  (recontextualize [this refn] (-> this (update :prop refn) (update :data-range #(when % (refn %)))))
  (add-to-parent [this parent] (throw (ex-info "DataMinCardinality cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "DataMinCardinality cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] os))

(defn data-min-cardinality
  ([n prop] (->DataMinCardinality n prop nil))
  ([n prop data-range] (->DataMinCardinality n prop data-range)))

;; DataMaxCardinality := 'DataMaxCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
(defrecord DataMaxCardinality [n prop data-range]
  DocumentElement
  (type-label [_] "DataMaxCardinality")
  (recontextualize [this refn] (-> this (update :prop refn) (update :data-range #(when % (refn %)))))
  (add-to-parent [this parent] (throw (ex-info "DataMaxCardinality cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "DataMaxCardinality cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] os))

(defn data-max-cardinality
  ([n prop] (->DataMaxCardinality n prop nil))
  ([n prop data-range] (->DataMaxCardinality n prop data-range)))

;; DataExactCardinality := 'DataExactCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
(defrecord DataExactCardinality [n prop data-range]
  DocumentElement
  (type-label [_] "DataExactCardinality")
  (recontextualize [this refn] (-> this (update :prop refn) (update :data-range #(when % (refn %)))))
  (add-to-parent [this parent] (throw (ex-info "DataExactCardinality cannot be added to parent" {:expr this})))
  (add-to-doc [this doc] (throw (ex-info "DataExactCardinality cannot be added to document" {:expr this})))
  ClassExpression
  (get-classes [_] os))

(defn data-exact-cardinality
  ([n prop] (->DataExactCardinality n prop nil))
  ([n prop data-range] (->DataExactCardinality n prop data-range)))
