(ns advent-of-code-2018.day04-test
  (:require [advent-of-code-2018.day04 :as day04]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def lines
  ["[1518-11-01 00:00] Guard #10 begins shift"
   "[1518-11-01 00:05] falls asleep"
   "[1518-11-01 00:25] wakes up"
   "[1518-11-01 00:30] falls asleep"
   "[1518-11-01 00:55] wakes up"
   "[1518-11-01 23:58] Guard #99 begins shift"
   "[1518-11-02 00:40] falls asleep"
   "[1518-11-02 00:50] wakes up"
   "[1518-11-03 00:05] Guard #10 begins shift"
   "[1518-11-03 00:24] falls asleep"
   "[1518-11-03 00:29] wakes up"
   "[1518-11-04 00:02] Guard #99 begins shift"
   "[1518-11-04 00:36] falls asleep"
   "[1518-11-04 00:46] wakes up"
   "[1518-11-05 00:03] Guard #99 begins shift"
   "[1518-11-05 00:45] falls asleep"
   "[1518-11-05 00:55] wakes up"])

(deftest solve1-test
  (is (= 240 (day04/solve1 lines)))
  (with-open [r (io/reader (io/resource "input04.txt"))]
    (is (= 36898 (day04/solve1 (line-seq r))))))

(deftest solve2-test
  (is (= 4455 (day04/solve2 lines)))
  (with-open [r (io/reader (io/resource "input04.txt"))]
    (is (= 80711 (day04/solve2 (line-seq r))))))
