(ns advent-of-code-2018.day16-test
  (:require [advent-of-code-2018.day16 :as day16]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(deftest solve1-test
  (with-open [r (io/reader (io/resource "input16_1.txt"))]
    (is (= 580 (day16/solve1 (line-seq r))))))

(deftest solve2-test
  (with-open [r1 (io/reader (io/resource "input16_1.txt"))
              r2 (io/reader (io/resource "input16_2.txt"))]
    (is (= 537 (day16/solve2 (line-seq r1) (line-seq r2))))))
