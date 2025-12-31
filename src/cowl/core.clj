(ns cowl.core
  (:require [tiara.data :as data :refer [ordered-map]]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot]
            [cowl.impl :as impl]
            [cowl.io :as io]
            [cowl.util :as util :refer [import-fn]])
  (:gen-class))

(import-fn prot/annotate)

(import-fn impl/owl)
(import-fn io/->str)
