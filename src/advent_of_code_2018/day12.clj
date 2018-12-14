(ns advent-of-code-2018.day12
  (:require [clojure.string :as str]))

(defn parse [s]
  (mapv (comp boolean #{\#}) s))

(defn parse-line [line]
  (let [[pre post] (str/split line #" => ")]
    [(parse pre) (first (parse post))]))

(defn step [rules state]
  (let [min (first state)
        max (first (rseq state))]
    (reduce (fn [state' i]
              (if (->> [(- i 2) (- i 1) i (+ i 1) (+ i 2)]
                       (mapv (comp boolean state))
                       rules)
                (conj state' i)
                (disj state' i)))
            state
            (range (- min 2) (+ max 3)))))

(defn solve [init lines]
  (let [rules (into {} (map parse-line lines))
        state (into (sorted-set)
                    (keep-indexed (fn [i x] (when x i)))
                    (parse init))]
    (iterate (partial step rules) state)))

(defn solve1 [n init lines]
  (->> (solve init lines)
       (drop n)
       first
       (apply +)))

(defn solve2 [n init lines]
  (->> (solve init lines)
       (map-indexed (fn [i s] [i (apply + s)]))
       (partition 3 1)
       (reduce (fn [_ [[_ n1] [_ n2] [i n3]]]
                 (let [d (- n2 n1), d' (- n3 n2)]
                   (when (= d d')
                     (reduced (+ (* d (- n i)) n3)))))
               nil)))
