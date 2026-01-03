(ns cowl.protocols
  {:doc "Protocols for COWL, implemented in other namespaces"
   :author "Paula Gearon"})

(defprotocol DocumentElement
  (recontextualize [this refn] "Update all references to use the context applied in the recontextualize function"))

(defprotocol Element
  (id [this] "Returns the ID of the element, or `nil` if one does not exist"))

(defprotocol Annotatable
  (annotate
    [this annotation]
    [this prop text]
    [this id prop text] "Annotates an object directly, or annotates a contained object")
  (get-annotations [this] "Retrieve the annotations of this object"))

(defprotocol Document
  (add-object-property [this prop] "Associates a property with this document")
  ;; (add-annotation-axiom [this ann-assertion] "Associates an annotation axiom with this document")
  )

(defprotocol Property
  (sub-property [this other] "Makes this property a subproperty of another")
  (equivalent [this other] "Declares this property the equivalent of another")
  (domain-of [this other] "Declares which types this property applies to")
  (range-of [this other] "Declares which types this property can reference")
  (disjoint [this other] "Declares that no two entities can be joined by this property")
  (inverse [this other] "Declares that this property is the inverse of another")
  (functional [this] "Declares this property to be functional"))

(defprotocol ObjectPropertyProtocol
  (inverse-functional [this] "Declares this property to be inverse functional")
  (transitive [this] "Declares this property to be transitive")
  (symmetric [this] "Declares this property to be symmetric")
  (asymmetric [this] "Declares this property to be asymmetric")
  (reflexive [this] "Declares this property to be reflexive")
  (irreflexive [this] "Declares this property to be irreflexive"))


(defprotocol Inlineable
  (legal-inline-subprop? [this] "Indicates if this object is legal as a subproperty")
  (legal-inline-equiv-prop? [this] "Indicates if this object is legal as an equivalent property")
  (object-subproperty-expr? [this] "Indicates an object subproperty expression")
  (object-property? [this] "Indicates a valid object property"))

(defprotocol Streamable
  (emit [this stream] "Emits this object to a stream"))
