(ns advent-of-code-2018.day19
  (:require [advent-of-code-2018.day16 :as day16]))

(defn parse-instr [line]
  (-> (read-string (str \[ line \]))
      (update 0 keyword)))

(defn parse-header [line]
  (let [[_ ip] (re-matches #"#ip (\d)" line)]
    (Long/parseLong ip)))

(defn parse-lines [lines]
  {:ip (parse-header (first lines))
   :instrs (mapv parse-instr (rest lines))})

(def opcodes
  {:addr day16/addr, :addi day16/addi
   :mulr day16/mulr, :muli day16/muli
   :setr day16/setr, :seti day16/seti
   :gtir day16/gtir, :gtri day16/gtri, :gtrr day16/gtrr
   :eqir day16/eqir, :eqri day16/eqri, :eqrr day16/eqrr})

(defn solve [rs ip exit? instrs]
  (loop [rs rs]
    (if (exit? rs)
      rs
      (let [[op a b c] (nth instrs (nth rs ip))
            rs' ((get opcodes op) rs a b c)]
        (recur (update rs' ip inc))))))

(defn solve1 [lines]
  (let [{:keys [ip instrs]} (parse-lines lines)
        rs (vec (repeat 6 0))]
    (nth (solve rs ip #(>= (nth % ip) (count instrs)) instrs) 0)))

(defn sum-of-divisors [n]
  (let [sqrt (Math/sqrt n)]
    (loop [i 1, sum 0]
      (if (> i sqrt)
        sum
        (recur (inc i) (cond-> sum (= (rem n i) 0) (+ i (quot n i))))))))

(defn solve2 [lines]
  (let [{:keys [ip instrs]} (parse-lines lines)
        rs (vec (cons 1 (repeat 5 0)))
        rs' (solve rs ip #(= (get % ip) 1) instrs)]
    (sum-of-divisors (nth rs' 2))))
