(ns advent-of-code-2018.day05-test
  (:require [advent-of-code-2018.day05 :as day05]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(deftest solve1-test
  (is (= 10 (day05/solve1 "dabAcCaCBAcCcaDA")))
  (is (= 9238 (day05/solve1 (slurp (io/resource "input05.txt"))))))

(deftest solve2-test
  (is (= 4 (day05/solve2 "dabAcCaCBAcCcaDA")))
  (is (= 4052 (day05/solve2 (slurp (io/resource "input05.txt"))))))
