(ns advent-of-code-2018.day02-test
  (:require [advent-of-code-2018.day02 :as day02]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(deftest solve1-test
  (let [input ["abcdef"
               "bababc"
               "abbcde"
               "abcccd"
               "aabcdd"
               "abcdee"
               "ababab"]]
    (is (= 12 (day02/solve1 input))))
  (with-open [r (io/reader (io/resource "input02.txt"))]
    (is (= 4712 (day02/solve1 (line-seq r))))))

(deftest solve2-test
  (let [input ["abcde"
               "fghij"
               "klmno"
               "pqrst"
               "fguij"
               "axcye"
               "wvxyz"]]
    (is (= "fgij" (day02/solve2 input))))
  (with-open [r (io/reader (io/resource "input02.txt"))]
    (is (= "lufjygedpvfbhftxiwnaorzmq"
           (day02/solve2 (line-seq r))))))
