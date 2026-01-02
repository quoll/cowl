(ns cowl.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [cowl.core :refer [owl annotate annotation add obj-property]]))

(deftest annotation-test
  (testing "Basic annotation"
    (let [doc (-> (owl)
                  (annotate :rdfs/label "Test doc")
                  (annotate :rdfs/comment "testing \"data\" here"))
          a (annotation :rdfs/label "test")
          doc2 (-> (owl)
                   (add a))
          doc3 (add doc a)]
      (is (= 2 (count (:annotations doc))))
      (is (= 1 (count (:annotations doc2))))
      (is (= 3 (count (:annotations doc3)))))))

(deftest obj-property-test
  (testing "Object property creation"
    (let [p (obj-property :op)
          p2 (-> (obj-property :op)
                 (annotate :rdf/label "test"))
          p3 (-> (obj-property :op)
                 (annotate :rdf/label "test"))
          ])))
