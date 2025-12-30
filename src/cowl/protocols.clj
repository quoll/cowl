(ns cowl.protocols
  {:doc "Protocols for COWL, implemented in other namespaces"
   :author "Paula Gearon"})

(defprotocol Annotatable
  (annotate [this prop text] [this id prop text] "Annotates an object directly, or annotates a contained object"))

(defprotocol Streamable
  (emit [this stream] "Emits this object to a stream"))
