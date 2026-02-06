(ns cowl.core
  (:require [cowl.protocols :as prot]
            [cowl.impl :as impl]
            [cowl.impl.classes :as cls]
            [cowl.impl.object-prop :as oprop]
            [cowl.impl.data-prop :as dprop]
            [cowl.io :as io]
            [cowl.util :as util :refer [import-fn]])
  (:gen-class))

;; Annotatable protocol
(import-fn prot/annotate)
(import-fn prot/get-annotations)

;; Document protocol
(import-fn prot/add-object-property)
(import-fn prot/add-data-property)
(import-fn prot/add-class)
(import-fn prot/get-object-property)
(import-fn prot/get-data-property)
(import-fn prot/get-class)

;; Property protocol
(import-fn prot/sub-property)
(import-fn prot/equivalent-prop)
(import-fn prot/domain-of)
(import-fn prot/range-of)
(import-fn prot/disjoint-prop)
(import-fn prot/functional)

;; ObjectPropertyProtocol
(import-fn prot/inverse)
(import-fn prot/inverse-functional)
(import-fn prot/transitive)
(import-fn prot/symmetric)
(import-fn prot/asymmetric)
(import-fn prot/reflexive)
(import-fn prot/irreflexive)

;; Core constructors from cowl.impl
(import-fn impl/ontology)
(import-fn impl/add)
(import-fn impl/annotation)

;; Class constructors from cowl.impl.classes
(import-fn cls/owl-class)

;; Class expression constructors from cowl.impl.classes
(import-fn cls/object-intersection-of)
(import-fn cls/object-union-of)
(import-fn cls/object-complement-of)
(import-fn cls/object-one-of)
(import-fn cls/object-some-values-from)
(import-fn cls/object-all-values-from)
(import-fn cls/object-has-value)
(import-fn cls/object-has-self)
(import-fn cls/object-min-cardinality)
(import-fn cls/object-max-cardinality)
(import-fn cls/object-exact-cardinality)
(import-fn cls/data-some-values-from)
(import-fn cls/data-all-values-from)
(import-fn cls/data-has-value)
(import-fn cls/data-min-cardinality)
(import-fn cls/data-max-cardinality)
(import-fn cls/data-exact-cardinality)

;; Object property constructors from cowl.impl.object-prop
(import-fn oprop/object-property)
(import-fn oprop/sub-object-prop)
(import-fn oprop/property-chain)
(import-fn oprop/inverse-obj-props)
(import-fn oprop/inverse-obj-prop)
(import-fn oprop/equiv-obj-props)
(import-fn oprop/disjoint-obj-props)
(import-fn oprop/object-prop-domain)
(import-fn oprop/object-prop-range)
(import-fn oprop/fn-object-prop)
(import-fn oprop/inv-fn-object-prop)
(import-fn oprop/reflexive-object-prop)
(import-fn oprop/irreflexive-object-prop)
(import-fn oprop/symmetric-object-prop)
(import-fn oprop/asymmetric-object-prop)
(import-fn oprop/transitive-object-prop)

;; Data property constructors from cowl.impl.data-prop
(import-fn dprop/data-property)
(import-fn dprop/sub-data-prop)
(import-fn dprop/equiv-data-props)
(import-fn dprop/disjoint-data-props)
(import-fn dprop/data-prop-domain)
(import-fn dprop/data-prop-range)
(import-fn dprop/fn-data-prop)


