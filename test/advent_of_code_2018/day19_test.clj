(ns advent-of-code-2018.day19-test
  (:require [advent-of-code-2018.day19 :as day19]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def lines
  ["#ip 0"
   "seti 5 0 1"
   "seti 6 0 2"
   "addi 0 1 0"
   "addr 1 2 3"
   "setr 1 0 0"
   "seti 8 0 4"
   "seti 9 0 5"])

(deftest solve1-test
  (is (= 7 (day19/solve1 lines)))
  (with-open [r (io/reader (io/resource "input19.txt"))]
    (is (= 2223 (day19/solve1 (line-seq r))))))

(deftest solve2-test
  (with-open [r (io/reader (io/resource "input19.txt"))]
    (is (= 24117312 (day19/solve2 (line-seq r))))))
