(ns cowl.core
  (:require [cowl.protocols :as prot]
            [cowl.impl :as impl]
            [cowl.io :as io]
            [cowl.util :as util :refer [import-fn]])
  (:gen-class))

(import-fn prot/annotate)

(import-fn impl/owl)
(import-fn impl/add)
(import-fn impl/annotation)
(import-fn impl/obj-property)
(import-fn io/->str)
