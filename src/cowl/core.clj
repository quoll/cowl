(ns cowl.core
  (:require [cowl.protocols :as prot]
            [cowl.impl :as impl]
            [cowl.io :as io]
            [cowl.util :as util :refer [import-fn]])
  (:gen-class))

(import-fn prot/annotate)
(import-fn prot/get-annotations)


(import-fn prot/sub-property)
(import-fn prot/equivalent)
(import-fn prot/domain-of)
(import-fn prot/range-of)
(import-fn prot/disjoint)
(import-fn prot/inverse)
(import-fn prot/functional)

(import-fn impl/owl)
(import-fn impl/add)
(import-fn impl/annotation)
(import-fn impl/obj-property)


(import-fn io/->str)
