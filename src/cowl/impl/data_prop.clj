(ns cowl.impl.data-prop
  {:doc "Data property implementations for COWL"
   :author "Paula Gearon"}
  (:require [cowl.protocols :as prot]
            [cowl.impl.common :refer [os prop-attr prop-bool-attr
                                       recontextualize-annotations mapos]]
            [cowl.io :as cio])
  (:import [cowl.protocols DocumentElement AddressableElement Annotatable TTLStreamable Inlineable Property]))

(defn annotations
  "Get all annotations from the head of a seq"
  [s]
  (take-while #(and % (= "Annotation" (prot/type-label %))) s))

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

(defn sub-data-prop
  "Accepts either child and a list of parents, with an optional annotation as the first argument"
  [& args]
  (let [anns (annotations args)
        [child parent & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to sub-data-prop" {:child child :parent parent :extra r})))
    (->SubDataPropertyOf anns child parent)))

(defrecord EquivalentDataProperties [annotations prop props]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :props refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "EquivalentDataProperties")
  (add-to-parent [this parent] (prot/add-data-property parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-data-property doc prop)
                 (update-in doc [:dprop-idx prop] prot/add-to-parent this)
                 (let [new-prop (reduce prot/equivalent (data-property prop) props)]
                   (prot/add-data-property doc new-prop)))]
      (reduce ensure-data-prop-in-doc doc* props)))
  Inlineable
  (legal-inline-subprop? [_] true)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false)
  TTLStreamable
  (ttl-emit [this stream] (cio/write-rel-properties stream :equiv :data this)))

(defn equiv-data-props
  [& props]
  (let [anns (annotations props)
        [_id equivs] (drop (count anns) props)]
    (->EquivalentDataProperties anns _id equivs)))

(defrecord DisjointDataProperties [annotations prop props]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :props refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "DisjointDataProperties")
  (add-to-parent [this parent] (prot/add-data-property parent this))
  (add-to-doc [this doc]
    (let [doc* (if (prot/get-data-property doc prop)
                 (update-in doc [:dprop-idx prop] prot/add-to-parent this)
                 (let [new-prop (reduce prot/disjoint (data-property prop) props)]
                   (prot/add-data-property doc new-prop)))]
      (reduce ensure-data-prop-in-doc doc* props)))
  Inlineable
  (legal-inline-subprop? [_] false)
  (legal-inline-equiv-prop? [_] false)
  (object-subproperty-expr? [_] false)
  (object-property? [_] false)
  TTLStreamable
  (ttl-emit [this stream] (cio/write-rel-properties stream :disjoint :data this)))

(defn disjoint-data-props
  [& props]
  (let [anns (annotations props)
        [_id equivs] (drop (count anns) props)]
    (->DisjointDataProperties anns _id equivs)))

(defrecord DataPropertyDomain [annotations prop domain]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :domain refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "DataPropertyDomain")
  (add-to-parent [this parent] (prot/domain-of parent this))
  (add-to-doc [this doc]
    (if (prot/get-data-property doc prop)
      (update-in doc [:dprop-idx prop] prot/add-to-parent this)
      (prot/add-data-property doc (prot/domain-of (data-property prop) this))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-data-prop-domain stream this)))

(defn data-prop-domain
  [& args]
  (let [anns (annotations args)
        [prop domain & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to data-prop-domain" {:prop prop :domain domain :extra r})))
    (->DataPropertyDomain anns prop domain)))

(defrecord DataPropertyRange [annotations prop range]
  AddressableElement
  (id [_] prop)
  DocumentElement
  (recontextualize [this refn] (-> this
                                   (update :prop refn)
                                   (update :range refn)
                                   (recontextualize-annotations refn)))
  (type-label [_] "DataPropertyRange")
  (add-to-parent [this parent] (prot/range-of parent this))
  (add-to-doc [this doc]
    (if (prot/get-data-property doc prop)
      (update-in doc [:dprop-idx prop] prot/add-to-parent this)
      (prot/add-data-property doc (prot/range-of (data-property prop) this))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-data-prop-range stream this)))

(defn data-prop-range
  [& args]
  (let [anns (annotations args)
        [prop range & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to data-prop-range" {:prop prop :range range :extra r})))
    (->DataPropertyRange anns prop range)))

(defrecord FunctionalDataProperty [annotations prop]
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
      (prot/add-data-property doc (prot/functional (data-property prop) (:annotations this)))))
  TTLStreamable
  (ttl-emit [this stream] (cio/write-fn-data-prop stream this)))

(defn fn-data-prop
  [& args]
  (let [anns (annotations args)
        [prop & r] (drop (count anns) args)]
    (when (seq r)
      (throw (ex-info "Unexpected extra arguments to fn-data-prop" {:prop prop :extra r})))
    (->FunctionalDataProperty anns prop)))
