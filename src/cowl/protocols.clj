(ns cowl.protocols
  {:doc "Protocols for COWL, implemented in other namespaces"
   :author "Paula Gearon"})

(defprotocol DocumentElement
  (recontextualize [this refn] "Update all references to use the context applied in the recontextualize function")
  (type-label [this] "The label for this entity type, as description in the Functional Syntax specification")
  (add-to-parent [this parent] "Adds this element to a parent element. Types can only be added to a small number of parent types, which is why dispatch is done through children.")
  (add-to-doc [this doc] "Adds this element to a document. For Classes and Properties, then this is equivalent to add-to-parent."))

(defprotocol AddressableElement
  (id [this] "Returns the ID of the element, or `nil` if one does not exist"))

(defprotocol AnnotationTest
  (annotation? [this] "Indicates if the element is an annotation"))

(defprotocol Annotatable
  (annotate
    [this annotation]
    [this prop text]
    [this id prop text] "Annotates an object directly, or annotates a contained object")
  (get-annotations [this] "Retrieve the annotations of this object"))

(defprotocol Document
  (add-object-property [this prop] "Associates an object property with this document")
  (add-data-property [this prop] "Associates a data property with this document")
  (add-class [this cls] "Associates a class with this document")
  ;; (add-annotation-axiom [this ann-assertion] "Associates an annotation axiom with this document")
  (get-object-property [this id] "Retrieves the object property associated with an id")
  (get-data-property [this id] "Retrieves the data property associated with an id")
  (get-class [this id] "Retrieves the class associated with an id"))

(defprotocol Property
  (sub-property [this other] "Makes this property a subproperty of another")
  (equivalent-prop [this other] "Declares this property the equivalent of another")
  (domain-of [this other] "Declares which types this property applies to")
  (range-of [this other] "Declares which types this property can reference")
  (disjoint-prop [this other] "Declares that no two entities can be joined by this property")
  (functional [this] [this annotations] "Declares this property to be functional"))

(defprotocol ObjectPropertyProtocol
  (inverse [this other] "Declares that this property is the inverse of another")
  (inverse-functional [this] [this annotations] "Declares this property to be inverse functional")
  (transitive [this] [this annotations] "Declares this property to be transitive")
  (symmetric [this] [this annotations] "Declares this property to be symmetric")
  (asymmetric [this] [this annotations] "Declares this property to be asymmetric")
  (reflexive [this] [this annotations] "Declares this property to be reflexive")
  (irreflexive [this] [this annotations] "Declares this property to be irreflexive"))


(defprotocol Inlineable
  (legal-inline-subprop? [this] "Indicates if this object is legal as a subproperty")
  (legal-inline-equiv-prop? [this] "Indicates if this object is legal as an equivalent property")
  (object-subproperty-expr? [this] "Indicates an object subproperty expression")
  (object-property? [this] "Indicates a valid object property"))

(defprotocol ClassExpression
  (get-classes [this] "Retrieves all class IRIs from this expression, recursively descending through nested expressions"))

(defprotocol TTLStreamable
  (ttl-emit [this stream] "Emits this object to a stream"))
