(ns cowl.impl.object-prop
  {:doc "Object property implementations for COWL"
   :author "Paula Gearon"}
  (:require [cowl.protocols :as prot]
            [cowl.impl.common :refer [os om prop-attr-binary prop-attr-multi prop-bool-attr annotation
                                      leading-annotations annotation-map
                                      recontextualize-annotations mapos add-object-prop-to-doc]]
            [cowl.io.iop :refer [Prop PropOther PropProps Props]])
  (:import [cowl.protocols DocumentElement AddressableElement Annotatable Property
            ObjectPropertyProtocol]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ObjectProperties ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {:prop (s/or IRI ObjectInverseOf)
;;  :annotations {:annotations {s/keyword Annotation}
;;                :super-props [ [Annotation] ]
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
(defrecord ObjectProperty [prop annotations super-props equivs domain range disjoints inverses
                           fn? inverse-fn? transitive? symmetric? asymmetric?
                           reflexive? irreflexive?]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (type-label [_] "ObjectProperty")
  (recontextualize [this refn]
    (cond-> (update this :prop refn)
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
  (sub-property [this other] (prop-attr-binary this :super-props other))
  (equivalent-prop [this other] (prop-attr-multi this :equivs other))
  (domain-of [this other] (prop-attr-binary this :domain other))
  (range-of [this other] (prop-attr-binary this :range other))
  (disjoint-prop [this other] (prop-attr-multi this :disjoints other))
  (functional [this] (prop-bool-attr this :fn?))
  (functional [this anns] (prop-bool-attr this :fn? anns))
  ObjectPropertyProtocol
  (inverse [this other] (prop-attr-binary this :inverses other))
  (inverse-functional [this] (prop-bool-attr this :inverse-fn?))
  (inverse-functional [this anns] (prop-bool-attr this :inverse-fn? anns))
  (transitive [this] (prop-bool-attr this :transitive?))
  (transitive [this anns] (prop-bool-attr this :transitive? anns))
  (symmetric [this] (prop-bool-attr this :symmetric?))
  (symmetric [this anns] (prop-bool-attr this :symmetric? anns))
  (asymmetric [this] (prop-bool-attr this :asymmetric?))
  (asymmetric [this anns] (prop-bool-attr this :asymmetric? anns))
  (reflexive [this] (prop-bool-attr this :reflexive?))
  (reflexive [this anns] (prop-bool-attr this :reflexive? anns))
  (irreflexive [this] (prop-bool-attr this :irreflexive?))
  (irreflexive [this anns] (prop-bool-attr this :irreflexive? anns)))

(defn object-property
  ([_id]
   (->ObjectProperty _id {:annotations om} os os os os os os false false false false false false false))
  ([a & args]
   (let [anns (annotation-map (cons a args))
         [_id] (drop (dec (count anns)) args)]
     (->ObjectProperty _id {:annotations anns} os os os os os os false false false false false false false))))

(defn ensure-object-prop-in-doc
  [doc prop]
  (cond-> doc
    ;; If the property is new to the document, update the doc to know about it
    (nil? (prot/get-object-property doc prop)) (prot/add-object-property (object-property prop))))

(defrecord SubObjectPropertyOf [annotations prop other]
  PropOther
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :other refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "SubObjectPropertyOf")
  (add-to-parent [this parent] (prot/sub-property parent this))
  (add-to-doc [this doc]
    (add-object-prop-to-doc this doc prop other
                            object-property
                            #(prot/sub-property % this)
                            ensure-object-prop-in-doc)))

(defn sub-object-prop
  "Accepts either child and a list of parents, with an optional annotation as the first argument"
  [& args]
  (let [anns (leading-annotations args)
        [child parent & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to sub-object-prop" {:child child :parent parent :extra r})))
    (->SubObjectPropertyOf anns child parent)))

(defrecord ObjectPropertyChain [props]
  Props
  DocumentElement
  (recontextualize [this refn] (update this :props #(mapv refn %)))
  (type-label [_] "ObjectPropertyChain")
  (add-to-parent [this parent]
    (if (instance? SubObjectPropertyOf parent)
      (assoc parent :child this)
      (throw (ex-info "ObjectPropertyChain can only be added to a SubObjectProperty"
                      {:parent parent :parent-type (type parent)}))))
  (add-to-doc [this _]
    (throw (ex-info "ObjectPropertyChain is not an axiom. Associate it with a property."
                    {:chain this}))))

(defn property-chain
  [& props]
  (->ObjectPropertyChain props))

(defrecord InverseObjectProperties [annotations prop other]
  PropOther
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :other refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "InverseObjectProperties")
  (add-to-parent [this parent] (prot/inverse parent this))
  (add-to-doc [this doc]
    (add-object-prop-to-doc this doc prop other
                            object-property
                            #(prot/inverse % other)
                            ensure-object-prop-in-doc)))

(defn inverse-obj-props
  [& args]
  (let [anns (leading-annotations args)
        [prop other :as remaining] (drop (count anns) args)]
    (when (seq remaining)
      (throw (ex-info "Too many arguments to inverse-obj-props" {:args remaining})))
    (->InverseObjectProperties anns prop other)))

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
    (assoc parent :other child)
    ;; if there is no initial property in the InverseObjectProperties, then set the initial property
    (assoc parent :prop child)))

;; This record can be used anywhere a property IRI can be
(defrecord ObjectInverseOf [annotations prop]
  Prop
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "ObjectInverseOf")
  (add-to-parent [this parent] (add-obj-property-to-parent parent this))
  (add-to-doc [this _]
    (throw (ex-info "ObjectInverseOf is not an axiom. Associate it with a property." {:inverse-prop this}))))

(defn inverse-obj-prop
  [& args]
  (let [anns (leading-annotations args)]
    (->ObjectInverseOf anns (drop (count anns) args))))

;; This record exists only to rewrite a object property
(defrecord EquivalentObjectProperties [annotations prop props]
  PropProps
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :props #(mapv refn %))
                                   (recontextualize-annotations refn)))
  (type-label [_] "EquivalentObjectProperties")
  (add-to-parent [this parent] (prot/equivalent-prop parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-object-property doc prop)
                 (update-in doc [:oprop-idx prop] prot/add-to-parent this)
                 (let [new-prop (reduce prot/equivalent-prop (object-property prop) props)]
                   (prot/add-object-property doc new-prop)))]
      (reduce ensure-object-prop-in-doc doc* props))))

(defn equiv-obj-props
  [& props]
  (let [anns (leading-annotations props)
        [_id & equivs] (drop (count anns) props)]
    (->EquivalentObjectProperties anns _id (into os equivs))))

(defrecord DisjointObjectProperties [annotations prop props]
  PropProps
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :props #(mapv refn %))
                                   (recontextualize-annotations refn)))
  (type-label [_] "DisjointObjectProperties")
  (add-to-parent [this parent] (prot/disjoint-prop parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-object-property doc prop)
                 (update-in doc [:oprop-idx prop] prot/add-to-parent this)
                 (let [new-prop (reduce prot/disjoint-prop (object-property prop) props)]
                   (prot/add-object-property doc new-prop)))]
      (reduce ensure-object-prop-in-doc doc* props))))

(defn disjoint-obj-props
  [& props]
  (let [anns (leading-annotations props)
        [_id equivs] (drop (count anns) props)]
    (->DisjointObjectProperties anns _id equivs)))

(defrecord ObjectPropertyDomain [annotations prop other]
  PropOther
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :other refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "ObjectPropertyDomain")
  (add-to-parent [this parent] (prot/domain-of parent this))
  (add-to-doc [this doc]
    (add-object-prop-to-doc this doc prop other
                            object-property
                            #(prot/domain-of % this)
                            nil)))

(defn object-prop-domain
  [& args]
  (let [anns (leading-annotations args)
        [prop other & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to object-prop-domain" {:prop prop :other other :extra r})))
    (->ObjectPropertyDomain anns prop other)))

(defrecord ObjectPropertyRange [annotations prop other]
  PropOther
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :other refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "ObjectPropertyRange")
  (add-to-parent [this parent] (prot/range-of parent this))
  (add-to-doc [this doc]
    (add-object-prop-to-doc this doc prop other
                            object-property
                            #(prot/range-of % this)
                            nil)))

(defn object-prop-range
  [& args]
  (let [anns (leading-annotations args)
        [prop other & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to object-prop-range" {:prop prop :other other :extra r})))
    (->ObjectPropertyRange anns prop other)))

(defrecord FunctionalObjectProperty [annotations prop]
  Prop
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
      (prot/add-object-property doc (prot/functional (object-property prop) (:annotations this))))))

(defn fn-object-prop
  [& args]
  (let [anns (leading-annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to fn-object-prop" {:prop prop :extra r})))
    (->FunctionalObjectProperty anns prop)))

(defrecord InverseFunctionalObjectProperty [annotations prop]
  Prop
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
      (prot/add-object-property doc (prot/inverse-functional (object-property prop) (:annotations this))))))

(defn inv-fn-object-prop
  [& args]
  (let [anns (leading-annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to inv-fn-object-prop" {:prop prop :extra r})))
    (->InverseFunctionalObjectProperty anns prop)))

(defrecord ReflexiveObjectProperty [annotations prop]
  Prop
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
      (prot/add-object-property doc (prot/reflexive (object-property prop) (:annotations this))))))

(defn reflexive-object-prop
  [& args]
  (let [anns (leading-annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to reflexive-object-prop" {:prop prop :extra r})))
    (->ReflexiveObjectProperty anns prop)))

(defrecord IrreflexiveObjectProperty [annotations prop]
  Prop
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
      (prot/add-object-property doc (prot/irreflexive (object-property prop) (:annotations this))))))

(defn irreflexive-object-prop
  [& args]
  (let [anns (leading-annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to irreflexive-object-prop" {:prop prop :extra r})))
    (->IrreflexiveObjectProperty anns prop)))

(defrecord SymmetricObjectProperty [annotations prop]
  Prop
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
      (prot/add-object-property doc (prot/symmetric (object-property prop) (:annotations this))))))

(defn symmetric-object-prop
  [& args]
  (let [anns (leading-annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to symmetric-object-prop" {:prop prop :extra r})))
    (->SymmetricObjectProperty anns prop)))

(defrecord AsymmetricObjectProperty [annotations prop]
  Prop
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
      (prot/add-object-property doc (prot/asymmetric (object-property prop) (:annotations this))))))

(defn asymmetric-object-prop
  [& args]
  (let [anns (leading-annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to asymmetric-object-prop" {:prop prop :extra r})))
    (->AsymmetricObjectProperty anns prop)))

(defrecord TransitiveObjectProperty [annotations prop]
  Prop
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
      (prot/add-object-property doc (prot/transitive (object-property prop) (:annotations this))))))

(defn transitive-object-prop
  [& args]
  (let [anns (leading-annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to transitive-object-prop" {:prop prop :extra r})))
    (->TransitiveObjectProperty anns prop)))
