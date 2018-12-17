(ns advent-of-code-2018.day17-test
  (:require [advent-of-code-2018.day17 :as day17]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def lines
  ["x=495, y=2..7"
   "y=7, x=495..501"
   "x=501, y=3..7"
   "x=498, y=2..4"
   "x=506, y=1..2"
   "x=498, y=10..13"
   "x=504, y=10..13"
   "y=13, x=498..504"])

(deftest solve1-test
  (is (= 57 (day17/solve1 lines)))
  (with-open [r (io/reader (io/resource "input17.txt"))]
    (is (= 31641 (day17/solve1 (line-seq r))))))

(deftest solve2-test
  (is (= 29 (day17/solve2 lines)))
  (with-open [r (io/reader (io/resource "input17.txt"))]
    (is (= 26321 (day17/solve2 (line-seq r))))))
