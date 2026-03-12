(ns cowl.io.iop
  {:doc "OWL i/o protocols"
   :author "Paula Gearon"})

(defprotocol Prop "A marker protocol for property records that contain only the :prop field")

(defprotocol PropOther "A marker protocol for property records that contain a :prop and an :other field")

(defprotocol PropProps "A marker protocol for property records that contain :prop and :props fields")

(defprotocol Props "A marker protocol for property record lists in a :props field")

(defprotocol EmbeddedAnnotation "A marker protocol for annotations to be emitted within another entity")

(defprotocol OwlFunctional
  (emit [this w] "Writes the definition of the object to the writer"))
