(ns cowl.impl.classes
  {:doc "Class implementations for COWL"
   :author "Paula Gearon"}
  (:require [cowl.protocols :as prot]
            [cowl.impl.common :refer [om mapos]])
  (:import [cowl.protocols DocumentElement AddressableElement]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord OWLClass [cls super-classes equivs disjoints disjoint-union]
  AddressableElement
  (id [_] cls)
  DocumentElement
  (recontextualize [this refn]
    (cond-> (update this :cls refn)
      (seq super-classes) (update :super-classes mapos refn)
      (seq equivs) (update :equivs mapos refn)
      (seq disjoints) (update :disjoints mapos refn)
      (seq disjoint-union) (update :disjoint-union mapos refn))))

(defn owl-class
  [_id]
  (->OWLClass _id om om om om))
