(ns advent-of-code-2018.day08-test
  (:require [advent-of-code-2018.day08 :as day08]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def s "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

(deftest solve1-test
  (is (= 138 (day08/solve1 s)))
  (is (= 43996 (day08/solve1 (slurp (io/resource "input08.txt"))))))

(deftest solve2-test
  (is (= 66 (day08/solve2 s)))
  (is (= 35189 (day08/solve2 (slurp (io/resource "input08.txt"))))))
