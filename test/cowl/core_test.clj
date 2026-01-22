(ns cowl.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [cowl.core :refer [owl annotate get-annotations annotation add obj-property
                               sub-property equivalent domain-of range-of disjoint inverse functional]]))

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
                 (annotate :rdf/label "test"))]
      (is (= (:id p) :op))
      (is (= (seq (get-annotations p)) nil))
      (is (= (:id p2) :op))
      (println "P2" p2)
      (is (= (get-annotations p2) [(annotation :rdf/label "test")]))
      (is (= (-> (obj-property :op)
                 (sub-property :parent)
                 :super-props)
             #{:parent}))
      (is (= (-> (obj-property :op)
                 (sub-property :parent)
                 (sub-property :parent2)
                 :super-props)
             #{:parent :parent2}))
      (is (= (-> (obj-property :op)
                 (equivalent :other)
                 :equivs)
             #{:other}))
      (is (= (-> (obj-property :op)
                 (equivalent :other)
                 (equivalent :other2)
                 :equivs)
             #{:other :other2})))))
