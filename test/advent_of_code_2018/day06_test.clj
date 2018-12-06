(ns advent-of-code-2018.day06-test
  (:require [advent-of-code-2018.day06 :as day06]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def lines
  ["1, 1"
   "1, 6"
   "8, 3"
   "3, 4"
   "5, 5"
   "8, 9"])

(deftest solve1-test
  (is (= 17 (day06/solve1 lines)))
  (with-open [r (io/reader (io/resource "input06.txt"))]
    (is (= 3620 (day06/solve1 (line-seq r))))))

(deftest solve2-test
  (is (= 16 (day06/solve2 32 lines)))
  (with-open [r (io/reader (io/resource "input06.txt"))]
    (is (= 39930 (day06/solve2 10000 (line-seq r))))))
