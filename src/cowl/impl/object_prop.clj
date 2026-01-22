(ns cowl.impl.object-prop
  {:doc "Object property implementations for COWL"
   :author "Paula Gearon"}
  (:require [cowl.protocols :as prot]
            [cowl.impl.common :refer [os om prop-attr prop-bool-attr
                                       recontextualize-annotations mapos]]
            [cowl.io :as cio])
  (:import [cowl.protocols DocumentElement AddressableElement Annotatable TTLStreamable Inlineable Property
            ObjectPropertyProtocol]))

(defn annotations
  "Get all annotations from the head of a seq"
  [s]
  (take-while #(and % (= "Annotation" (prot/type-label %))) s))

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
  (annotate [this prop text] (update-in this [:annotations :annotations] assoc prop (cowl.impl/annotation prop text)))
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

(defn sub-object-prop
  "Accepts either child and a list of parents, with an optional annotation as the first argument"
  [& args]
  (let [anns (annotations args)
        [child parent & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to sub-object-prop" {:child child :parent parent :extra r})))
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
                 (update-in doc [:oprop-idx prop] prot/add-to-parent this)
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
                 (update-in doc [:oprop-idx prop] prot/add-to-parent this)
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
                 (update-in doc [:oprop-idx prop] prot/add-to-parent this)
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

(defrecord ObjectPropertyDomain [annotations prop domain]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :domain refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "ObjectPropertyDomain")
  (add-to-parent [this parent] (prot/domain-of parent this))
  (add-to-doc [this doc]
    (if (prot/get-object-property doc prop)
      (update-in doc [:oprop-idx prop] prot/add-to-parent this)
      (prot/add-object-property doc (prot/domain-of (object-property prop) this))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-obj-prop-domain stream this)))

(defn object-prop-domain
  [& args]
  (let [anns (annotations args)
        [prop domain & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to object-prop-domain" {:prop prop :domain domain :extra r})))
    (->ObjectPropertyDomain anns prop domain)))

(defrecord ObjectPropertyRange [annotations prop range]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :range refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "ObjectPropertyRange")
  (add-to-parent [this parent] (prot/range-of parent this))
  (add-to-doc [this doc]
    (if (prot/get-object-property doc prop)
      (update-in doc [:oprop-idx prop] prot/add-to-parent this)
      (prot/add-object-property doc (prot/range-of (object-property prop) this))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-obj-prop-range stream this)))

(defn object-prop-range
  [& args]
  (let [anns (annotations args)
        [prop range & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to object-prop-range" {:prop prop :range range :extra r})))
    (->ObjectPropertyRange anns prop range)))

(defrecord FunctionalObjectProperty [annotations prop]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "FunctionalObjectProperty")
  (add-to-parent [this parent] (prot/functional parent (:annotations this)))
  (add-to-doc [this doc]
    (if (prot/get-object-property doc prop)
      (update-in doc [:oprop-idx prop] prot/add-to-parent this)
      (prot/add-object-property doc (prot/functional (object-property prop) (:annotations this)))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-fn-obj-prop stream this)))

(defn fn-object-prop
  [& args]
  (let [anns (annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to fn-object-prop" {:prop prop :extra r})))
    (->FunctionalObjectProperty anns prop)))

(defrecord InverseFunctionalObjectProperty [annotations prop]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "InverseFunctionalObjectProperty")
  (add-to-parent [this parent] (prot/inverse-functional parent (:annotations this)))
  (add-to-doc [this doc]
    (if (prot/get-object-property doc prop)
      (update-in doc [:oprop-idx prop] prot/add-to-parent this)
      (prot/add-object-property doc (prot/inverse-functional (object-property prop) (:annotations this)))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-inv-fn-obj-prop stream this)))

(defn inv-fn-object-prop
  [& args]
  (let [anns (annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to inv-fn-object-prop" {:prop prop :extra r})))
    (->InverseFunctionalObjectProperty anns prop)))

(defrecord ReflexiveObjectProperty [annotations prop]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "ReflexiveObjectProperty")
  (add-to-parent [this parent] (prot/reflexive parent (:annotations this)))
  (add-to-doc [this doc]
    (if (prot/get-object-property doc prop)
      (update-in doc [:oprop-idx prop] prot/add-to-parent this)
      (prot/add-object-property doc (prot/reflexive (object-property prop) (:annotations this)))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-reflexive-obj-prop stream this)))

(defn reflexive-object-prop
  [& args]
  (let [anns (annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to reflexive-object-prop" {:prop prop :extra r})))
    (->ReflexiveObjectProperty anns prop)))

(defrecord IrreflexiveObjectProperty [annotations prop]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "IrreflexiveObjectProperty")
  (add-to-parent [this parent] (prot/irreflexive parent (:annotations this)))
  (add-to-doc [this doc]
    (if (prot/get-object-property doc prop)
      (update-in doc [:oprop-idx prop] prot/add-to-parent this)
      (prot/add-object-property doc (prot/irreflexive (object-property prop) (:annotations this)))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-irreflexive-obj-prop stream this)))

(defn irreflexive-object-prop
  [& args]
  (let [anns (annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to irreflexive-object-prop" {:prop prop :extra r})))
    (->IrreflexiveObjectProperty anns prop)))

(defrecord SymmetricObjectProperty [annotations prop]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "SymmetricObjectProperty")
  (add-to-parent [this parent] (prot/symmetric parent (:annotations this)))
  (add-to-doc [this doc]
    (if (prot/get-object-property doc prop)
      (update-in doc [:oprop-idx prop] prot/add-to-parent this)
      (prot/add-object-property doc (prot/symmetric (object-property prop) (:annotations this)))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-symmetric-obj-prop stream this)))

(defn symmetric-object-prop
  [& args]
  (let [anns (annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to symmetric-object-prop" {:prop prop :extra r})))
    (->SymmetricObjectProperty anns prop)))

(defrecord AsymmetricObjectProperty [annotations prop]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "AsymmetricObjectProperty")
  (add-to-parent [this parent] (prot/asymmetric parent (:annotations this)))
  (add-to-doc [this doc]
    (if (prot/get-object-property doc prop)
      (update-in doc [:oprop-idx prop] prot/add-to-parent this)
      (prot/add-object-property doc (prot/asymmetric (object-property prop) (:annotations this)))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-asymmetric-obj-prop stream this)))

(defn asymmetric-object-prop
  [& args]
  (let [anns (annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to asymmetric-object-prop" {:prop prop :extra r})))
    (->AsymmetricObjectProperty anns prop)))

(defrecord TransitiveObjectProperty [annotations prop]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "TransitiveObjectProperty")
  (add-to-parent [this parent] (prot/transitive parent (:annotations this)))
  (add-to-doc [this doc]
    (if (prot/get-object-property doc prop)
      (update-in doc [:oprop-idx prop] prot/add-to-parent this)
      (prot/add-object-property doc (prot/transitive (object-property prop) (:annotations this)))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-transitive-obj-prop stream this)))

(defn transitive-object-prop
  [& args]
  (let [anns (annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to transitive-object-prop" {:prop prop :extra r})))
    (->TransitiveObjectProperty anns prop)))
