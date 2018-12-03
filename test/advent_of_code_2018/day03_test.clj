(ns advent-of-code-2018.day03-test
  (:require [advent-of-code-2018.day03 :as day03]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def lines
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])

(deftest solve1-test
  (is (= 4 (day03/solve1 lines)))
  (with-open [r (io/reader (io/resource "input03.txt"))]
    (is (= 98005 (day03/solve1 (line-seq r))))))

(deftest solve2-test
  (is (= "3" (day03/solve2 lines)))
  (with-open [r (io/reader (io/resource "input03.txt"))]
    (is (= "331" (day03/solve2 (line-seq r))))))
