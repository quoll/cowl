(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]))

(def pom "build-rsc/pom.xml")
(def lib 'org.clojars.quoll/cowl)
(def version "0.0.1")

;; clj -T:build test
(defn test "Run the tests." [opts]
  (bb/run-tests opts))

;; clj -T:build ci
(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (-> opts
      (assoc :lib lib :version version :src-pom pom)
      (bb/run-tests)
      (bb/clean)
      (bb/jar)))

;; clj -T:build install
(defn install "Install the JAR locally." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/install)))

;; clj -T:build deploy
(defn deploy "Deploy the JAR to Clojars." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/deploy)))
