(ns advent-of-code-2018.day17
  (:require [clojure.string :as str]))

(defn parse-coord [s]
  (let [[_ c v1 v2] (re-matches #"(\w)=(\d+)(?:\.\.(\d+))?" s)]
    [(keyword (str c \s))
     (let [n (Long/parseLong v1)]
       (if v2
         [n (Long/parseLong v2)]
         [n n]))]))

(defn parse-line [line]
  (let [[s1 s2] (str/split line #", ")]
    (conj {} (parse-coord s1) (parse-coord s2))))

(defn lines->map [lines]
  (->> (map parse-line lines)
       (reduce (fn [m {:keys [ys xs]}]
                 (let [[min-y max-y] ys, [min-x max-x] xs]
                   (->> (for [y (range min-y (inc max-y)),
                              x (range min-x (inc max-x))]
                          [[y x] \#])
                        (into m))))
               {})))

(defn print-map [{m :map}]
  (let [[min-y max-y] (apply (juxt min max) (map first (keys m)))
        [min-x max-x] (apply (juxt min max) (map second (keys m)))]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (print (get m [y x] \space)))
      (newline))))

(defn flood [m delta-fn [y x :as pos]]
  (reduce (fn [prev [y x :as pos]]
            (let [below [(inc y) x]]
              (if (and (not= (get m pos) \#)
                       (#{\# \~} (get m below)))
                x
                (reduced prev))))
          x
          (iterate (fn [[_ x]] [y (delta-fn x)]) pos)))

(defn step [{m :map :keys [stack] :as state}]
  (let [[y x :as pos] (peek stack)
        below [(inc y) x]]
    (cond (#{\# \~} (get m below))
          (let [min-x (flood m dec pos)
                max-x (flood m inc pos)
                l [y (dec min-x)]
                r [y (inc max-x)]
                c (if (and (= (get m l) \#) (= (get m r) \#)) \~ \|)]
            (-> (reduce (fn [state x] (assoc-in state [:map [y x]] c))
                        state
                        (range min-x (inc max-x)))
                (update :stack pop)
                (update :stack into (remove #(= (get m %) \#)) [l r])))

          (not (get m pos))
          (cond-> (assoc-in state [:map pos] \|)
            (not (get m below))
            (update :stack conj below))

          :else (update state :stack pop))))

(defn solve [cs lines]
  (let [m (lines->map lines)
        [min-y max-y] (apply (juxt min max) (map first (keys m)))]
    (loop [{:keys [stack] :as state} {:map m :stack '([0 500])}]
      (cond (empty? stack)
            (->> (:map state)
                 (filter #(>= (first (key %)) min-y))
                 vals
                 (filter cs)
                 count)

            (> (first (peek stack)) max-y)
            (recur (step (update state :stack pop)))

            :else (recur (step state))))))

(defn solve1 [lines]
  (solve #{\~ \|} lines))

(defn solve2 [lines]
  (solve #{\~} lines))
