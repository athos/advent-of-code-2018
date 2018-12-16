(ns advent-of-code-2018.day16
  (:require [clojure.string :as str]))

(defn parse-instr [s]
  (read-string (str \[ s \])))

(defn parse-sample [[pre instr post _ & more]]
  (let [pre (->> (str/split pre #":")
                 second
                 read-string)
        instr (parse-instr instr)
        post (->> (str/split post #":")
                  second
                  read-string)]
    [[pre instr post] more]))

(defn parse-samples [lines]
  (lazy-seq
   (when (seq lines)
     (let [[sample more] (parse-sample lines)]
       (cons sample (parse-samples more))))))

;;
;; Operators
;;

(defn addr [rs a b c]
  (assoc rs c (+ (nth rs a) (nth rs b))))

(defn addi [rs a b c]
  (assoc rs c (+ (nth rs a) b)))

(defn mulr [rs a b c]
  (assoc rs c (* (nth rs a) (nth rs b))))

(defn muli [rs a b c]
  (assoc rs c (* (nth rs a) b)))

(defn banr [rs a b c]
  (assoc rs c (bit-and (nth rs a) (nth rs b))))

(defn bani [rs a b c]
  (assoc rs c (bit-and (nth rs a) b)))

(defn borr [rs a b c]
  (assoc rs c (bit-or (nth rs a) (nth rs b))))

(defn bori [rs a b c]
  (assoc rs c (bit-or (nth rs a) b)))

(defn setr [rs a b c]
  (assoc rs c (nth rs a)))

(defn seti [rs a b c]
  (assoc rs c a))

(defn bool->num [x]
  (if x 1 0))

(defn gtir [rs a b c]
  (assoc rs c (bool->num (> a (nth rs b)))))

(defn gtri [rs a b c]
  (assoc rs c (bool->num (> (nth rs a) b))))

(defn gtrr [rs a b c]
  (assoc rs c (bool->num (> (nth rs a) (nth rs b)))))

(defn eqir [rs a b c]
  (assoc rs c (bool->num (= a (nth rs b)))))

(defn eqri [rs a b c]
  (assoc rs c (bool->num (= (nth rs a) b))))

(defn eqrr [rs a b c]
  (assoc rs c (bool->num (= (nth rs a) (nth rs b)))))

(def ops
  #{addr addi mulr muli banr bani borr bori
    setr seti gtir gtri gtrr eqir eqri eqrr})

(defn consistent? [f pre [_ a b c] post]
  (= (f pre a b c) post))

(defn compatible-ops [candidate-ops [pre instr post]]
  (->> candidate-ops
       (into #{} (filter #(consistent? % pre instr post)))))

(defn solve1 [lines]
  (->> (parse-samples lines)
       (filter #(>= (count (compatible-ops ops %)) 3))
       count))

(defn resolve-constraints [constrs]
  (loop [constrs constrs, assignment {}]
    (if (empty? constrs)
      assignment
      (let [assignment' (into {}
                              (keep (fn [[op candidates]]
                                      (when (= (count candidates) 1)
                                        [(first candidates) op])))
                              constrs)
            to-be-removed (set (vals assignment'))
            constrs' (into {}
                           (keep (fn [[op candidates]]
                                   (when-not (to-be-removed op)
                                     [op (into #{} (remove assignment')
                                               candidates)])))
                           constrs)]
        (recur constrs' (merge assignment assignment'))))))

(defn identify-ops [samples]
  (let [compats (reduce (fn [candidates [_ [op] _ :as sample]]
                          (update candidates op compatible-ops sample))
                        (zipmap (range 16) (repeat ops))
                        samples)]
    (->> ops
         (reduce (fn [m op]
                   (->> compats
                        (into #{} (keep #(when ((val %) op) (key %))))
                        (assoc m op)))
                 {})
         resolve-constraints)))

(defn solve2 [lines1 lines2]
  (let [samples (parse-samples lines1)
        opcode->op (identify-ops samples)]
    (->> (map parse-instr lines2)
         (reduce (fn [rs [opcode a b c]]
                   (let [op (opcode->op opcode)]
                     (op rs a b c)))
                 [0 0 0])
         first)))
