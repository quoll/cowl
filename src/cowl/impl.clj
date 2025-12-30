(ns cowl.impl
  {:doc "Base implementations for the COWL protocols"
   :author "Paula Gearon"}
  (:require [tiara.data :as data :refer [ordered-map]]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot]
            [cowl.io :as cio])
  (:import [cowl.protocols Annotatable Streamable]))

(def ^:dynamic *default-ns* rdf/common-prefixes)

(def ^:dynamic *annotation-props*
  #{:rdfs/label :rdfs/comment :rdfs/seeAlso :rdfs/isDefinedBy
    :owl/versionInfo :owl/deprecated :owl/backwardCompatibleWith
    :owl/incompatibleWith :owl/priorVersion})

(def om data/EMPTY_MAP)

(def mm data/EMPTY_MULTI_MAP)

(defn check
  "Test that a value is one of a known set"
  ([expected value] (check expected value "Unexpected value"))
  ([expected value msg]
   (when-not (contains? expected value)
     (throw (ex-info (str msg ": " value) {:value value})))))

(defrecord OWLDocument [class-idx oprop-idx dprop-idx annotations instance-idx prefixes]
  Annotatable
  (annotate [this prop text]
    (check *annotation-props* prop "Unexpected annotation property")
    (update this :annotations assoc (rdf/curie *default-ns* prop) text))
  Streamable
  (emit [_ stream]
    (cio/write-doc-annotations stream annotations)))


(defn owl
  []
  (->OWLDocument om om om mm om om))
