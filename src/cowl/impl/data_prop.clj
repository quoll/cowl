(ns cowl.impl.data-prop
  {:doc "Data property implementations for COWL"
   :author "Paula Gearon"}
  (:require [cowl.protocols :as prot]
            [cowl.impl.common :refer [os prop-attr-binary prop-attr-multi prop-bool-attr annotation annotation-map
                                      recontextualize-annotations mapos add-data-prop-to-doc leading-annotations]]
            [cowl.io.iop :refer [Prop PropOther PropProps]])
  (:import [cowl.protocols DocumentElement AddressableElement Annotatable Inlineable Property]))

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
  (sub-property [this other] (prop-attr-binary this :super-props other))
  (equivalent-prop [this other] (prop-attr-multi this :equivs other))
  (domain-of [this other] (prop-attr-binary this :domain other))
  (range-of [this other] (prop-attr-binary this :range other))
  (disjoint-prop [this other] (prop-attr-multi this :disjoints other))
  (functional [this] (prop-bool-attr this :fn?))
  (functional [this anns] (prop-bool-attr this :fn? anns)))

(defn data-property
  ([_id]
   (->DataProperty _id {:annotations []} os os os os os false))
  ([a & args]
   (let [anns (annotation-map (cons a args))
         [_id] (drop (dec (count anns)) args)]
     (->DataProperty _id {:annotations anns} os os os os os false))))

(defn ensure-data-prop-in-doc
  [doc prop]
  (cond-> doc
    ;; If the property is new to the document, update the doc to know about it
    (nil? (prot/get-data-property doc prop)) (prot/add-data-property (data-property prop))))

(defrecord SubDataPropertyOf [annotations prop other]
  PropOther
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :other refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "SubDataPropertyOf")
  (add-to-parent [this parent] (prot/sub-property parent this))
  (add-to-doc [this doc]
    (add-data-prop-to-doc this doc prop other
                          data-property
                          #(prot/sub-property % this)
                          ensure-data-prop-in-doc))
  Inlineable
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] true)
  (object-property? [_] false))

(defn sub-data-prop
  "Accepts either child and a list of parents, with an optional annotation as the first argument"
  [& args]
  (let [anns (leading-annotations args)
        [child parent & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to sub-data-prop" {:child child :parent parent :extra r})))
    (->SubDataPropertyOf anns child parent)))

(defrecord EquivalentDataProperties [annotations prop props]
  PropProps
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :props #(mapv refn %))
                                   (recontextualize-annotations refn)))
  (type-label [_] "EquivalentDataProperties")
  (add-to-parent [this parent] (prot/equivalent-prop parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-data-property doc prop)
                 (update-in doc [:dprop-idx prop] prot/add-to-parent this)
                 (let [new-prop (reduce prot/equivalent-prop (data-property prop) props)]
                   (prot/add-data-property doc new-prop)))]
      (reduce ensure-data-prop-in-doc doc* props)))
  Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false))

(defn equiv-data-props
  [& props]
  (let [anns (leading-annotations props)
        [_id equivs] (drop (count anns) props)]
    (->EquivalentDataProperties anns _id equivs)))

(defrecord DisjointDataProperties [annotations prop props]
  PropProps
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :props #(mapv refn %))
                                   (recontextualize-annotations refn)))
  (type-label [_] "DisjointDataProperties")
  (add-to-parent [this parent] (prot/disjoint-prop parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-data-property doc prop)
                 (update-in doc [:dprop-idx prop] prot/add-to-parent this)
                 (let [new-prop (reduce prot/disjoint-prop (data-property prop) props)]
                   (prot/add-data-property doc new-prop)))]
      (reduce ensure-data-prop-in-doc doc* props)))
  Inlineable
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false))

(defn disjoint-data-props
  [& props]
  (let [anns (leading-annotations props)
        [_id equivs] (drop (count anns) props)]
    (->DisjointDataProperties anns _id equivs)))

(defrecord DataPropertyDomain [annotations prop other]
  PropOther
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :other refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "DataPropertyDomain")
  (add-to-parent [this parent] (prot/domain-of parent this))
  (add-to-doc [this doc]
    (add-data-prop-to-doc this doc prop other
                          data-property
                          #(prot/domain-of % this)
                          nil)))

(defn data-prop-domain
  [& args]
  (let [anns (leading-annotations args)
        [prop other & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to data-prop-domain" {:prop prop :other other :extra r})))
    (->DataPropertyDomain anns prop other)))

(defrecord DataPropertyRange [annotations prop other]
  PropOther
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :other refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "DataPropertyRange")
  (add-to-parent [this parent] (prot/range-of parent this))
  (add-to-doc [this doc]
    (add-data-prop-to-doc this doc prop other
                          data-property
                          #(prot/range-of % this)
                          nil)))

(defn data-prop-range
  [& args]
  (let [anns (leading-annotations args)
        [prop other & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to data-prop-range" {:prop prop :other other :extra r})))
    (->DataPropertyRange anns prop other)))

(defrecord FunctionalDataProperty [annotations prop]
  Prop
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "FunctionalDataProperty")
  (add-to-parent [this parent] (prot/functional parent (:annotations this)))
  (add-to-doc [this doc]
    (if (prot/get-data-property doc prop)
      (update-in doc [:dprop-idx prop] prot/add-to-parent this)
      (prot/add-data-property doc (prot/functional (data-property prop) (:annotations this))))))

(defn fn-data-prop
  [& args]
  (let [anns (leading-annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to fn-data-prop" {:prop prop :extra r})))
    (->FunctionalDataProperty anns prop)))
