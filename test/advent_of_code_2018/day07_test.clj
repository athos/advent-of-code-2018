(ns advent-of-code-2018.day07-test
  (:require [advent-of-code-2018.day07 :as day07]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(deftest scheduler-test
  (let [scheduler (day07/make-scheduler 1 (constantly 2))]
    (is (day07/idle? scheduler))
    (is (= () (day07/tick! scheduler)))
    (is (= 1 (day07/schedule! scheduler [\A \B])))
    (is (not (day07/idle? scheduler)))
    (is (= () (day07/tick! scheduler)))
    (is (not (day07/idle? scheduler)))
    (is (= [\A] (day07/tick! scheduler)))
    (is (day07/idle? scheduler))))

(def lines
  ["Step C must be finished before step A can begin."
   "Step C must be finished before step F can begin."
   "Step A must be finished before step B can begin."
   "Step A must be finished before step D can begin."
   "Step B must be finished before step E can begin."
   "Step D must be finished before step E can begin."
   "Step F must be finished before step E can begin."])

(deftest solve1-test
  (is (= "CABDFE" (day07/solve1 lines)))
  (with-open [r (io/reader (io/resource "input07.txt"))]
    (is (= "CHILFNMORYKGAQXUVBZPSJWDET" (day07/solve1 (line-seq r))))))

(deftest solve2-test
  (is (= 15 (day07/solve2 2 0 lines)))
  (with-open [r (io/reader (io/resource "input07.txt"))]
    (is (= 891 (day07/solve2 5 60 (line-seq r))))))
