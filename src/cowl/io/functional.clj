(ns cowl.io.functional
  {:doc "OWL Functional Syntax output operations"
   :author "Paula Gearon"}
  (:require [cowl.protocols :refer [type-label]]
            [cowl.io.iop :as iop :refer [OwlFunctional emit]]
            [quoll.rdf :as rdf])
  (:import [cowl.io.iop Prop PropOther PropProps Props EmbeddedAnnotation]
           [quoll.rdf IRI]
           [java.io Writer]))

(declare emit-annotations)

(defn emit-annotation-assertion
  "Writes an Annotation as a single document level AnnotationAssertion"
  [{:keys [annotations prop value]} ^Writer w]
  (.write "AnnotationAssertion(")
  (emit-annotations annotations w)
  (emit prop w)
  (.write w " ")
  (emit value w)
  (.write w ")\n"))

(defn emit-annotation-assertions
  "Writes a series of Annotations as AnnotationAssertions at the document level"
  [annotations w]
  (when (seq annotations)
    (doseq [ann annotations]
      (emit-annotation-assertion ann w))))

(defn emit-annotations
  "Writes a series of Annotations within the context of another element.
  Leaves a trailing space if anything is written."
  [annotations ^Writer w]
  (when-let [[a1 & ar] annotations]
    (emit a1 w)
    (doseq [a ar]
      (.write w " ")
      (emit a w))
    (.write w " ")))

(extend-protocol OwlFunctional
  Prop
  (emit [{:keys [annotations prop] :as this} ^Writer w]
    (emit-annotation-assertions annotations w)
    (.write w (type-label this))
    (.write w "(")
    (emit prop w)
    (.write w ")\n"))

  PropOther
  (emit [{:keys [annotations prop other] :as this} ^Writer w]
    (emit-annotation-assertions annotations w)
    (.write w (type-label this))
    (.write w "(")
    (emit prop w)
    (.write w " ")
    (emit other w)
    (.write w ")\n"))

  PropProps
  (emit [{:keys [annotations prop props] :as this} ^Writer w]
    (emit-annotation-assertions annotations w)
    (.write w (type-label this))
    (.write w "(")
    (emit prop w)
    (doseq [p props]
      (.write w " ")
      (emit p w))
    (.write w ")\n"))

  Props
  (emit [{:keys [props] :as this} ^Writer w]
    (.write w (type-label this))
    (.write w "(")
    (when (seq props)
      (let [[p1 & pr] props]
        (emit p1 w)
        (doseq [p pr]
          (emit p w))))
    (.write w ")\n"))

  EmbeddedAnnotation
  (emit [{:keys [annotations prop value]} ^Writer w]
    (.write w "Annotation(")
    (emit-annotations annotations w)
    (emit prop w)
    (.write w " ")
    (emit value w)
    (.write w ")"))

  clojure.lang.Keyword
  (emit [kw ^Writer w]
    (let [k (if (namespace kw) (rdf/curie kw) kw)]
      (.write w (str k))))

  IRI
  (emit [i ^Writer w]
    (.write w (str i)))

  String
  (emit [o ^Writer w]
    (.write w (str \" o \")))

  Object
  (emit [o ^Writer w]
    (.write w (str o))))
