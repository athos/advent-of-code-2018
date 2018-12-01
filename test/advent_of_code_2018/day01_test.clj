(ns advent-of-code-2018.day01-test
  (:require [advent-of-code-2018.day01 :as day01]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is  are]]))

(deftest solve1-test
  (are [input expected] (= expected (day01/solve1 input))
    "+1\n+1\n+1" 3
    "+1\n+1\n-2" 0
    "-1\n-2\n-3" -6)
  (is 454 (day01/solve1 (slurp (io/resource "input01.txt")))))

(deftest solve2-test
  (are [input expected] (= expected (day01/solve2 input))
    "+1\n-1" 0
    "+3\n+3\n+4\n-2\n-4" 10
    "-6\n+3\n+8\n+5\n-6" 5
    "+7\n+7\n-2\n-7\n-4" 14)
  (is 566 (day01/solve2 (slurp (io/resource "input01.txt")))))
