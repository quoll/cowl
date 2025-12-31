(ns cowl.protocols
  {:doc "Protocols for COWL, implemented in other namespaces"
   :author "Paula Gearon"})

(defprotocol Annotatable
  (annotate [this prop text] [this id prop text] "Annotates an object directly, or annotates a contained object"))

(defprotocol Property
  (sub-property [this other] "Makes this property a subproperty of another")
  (equivalent [this other] "Declares this property the equivalent of another")
  (domain-of [this other] "Declares which types this property applies to")
  (range-of [this other] "Declares which types thie property can reference")
  (disjoint [this other] "Declares that no two entities can be joined by this property")
  (functional [this] "Declares this property to be functional"))

(defprotocol ObjectPropertyProtocol
  (inverse-functional [this] "Declares this property to be inverse functional")
  (transitive [this] "Declares this property to be transitive")
  (symmetric [this] "Declares this property to be symmetric")
  (asymmetric [this] "Declares this property to be asymmetric")
  (reflexive [this] "Declares this property to be reflexive")
  (irreflexive [this] "Declares this property to be irreflexive")
  )

(defprotocol Streamable
  (emit [this stream] "Emits this object to a stream"))
