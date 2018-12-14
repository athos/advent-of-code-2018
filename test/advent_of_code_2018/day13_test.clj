(ns advent-of-code-2018.day13-test
  (:require [advent-of-code-2018.day13 :as day13]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def lines1
  ["/->-\\       "
   "|   |  /----\\"
   "| /-+--+-\\  |"
   "| | |  | v  |"
   "\\-+-/  \\-+--/"
   "  \\------/  "])

(deftest solve1-test
  (is (= "7,3" (day13/solve1 lines1)))
  (with-open [r (io/reader (io/resource "input13.txt"))]
    (is (= "118,66" (day13/solve1 (line-seq r))))))

(def lines2
  ["/>-<\\  "
   "|   |  "
   "| /<+-\\"
   "| | | v"
   "\\>+</ |"
   "  |   ^"
   "  \\<->/"])

(deftest solve2-test
  (is (= "6,4" (day13/solve2 lines2)))
  (with-open [r (io/reader (io/resource "input13.txt"))]
    (is (= "70,129" (day13/solve2 (line-seq r))))))
