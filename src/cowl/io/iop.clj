(ns cowl.io.iop
  {:doc "OWL i/o protocols"
   :author "Paula Gearon"})

(defprotocol OwlFunctional
  (write-declaration [this w] "Writes the declarations of the object to the writer")
  (write-definition [this w] "Writes the definition of the object to the writer"))
