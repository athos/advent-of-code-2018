(ns advent-of-code-2018.day20)

(declare parse-branch)

(defn parse-regex [re i acc]
  (let [c (nth re i)]
    (case c
      \^ (recur re (inc i) acc)
      (\N \E \W \S) (recur re (inc i) (conj acc c))
      \( (let [[acc' i'] (parse-branch re (inc i) acc)]
           (recur re (inc i') acc'))
      (\| \) \$) [acc i])))

(defn parse-branch [re i acc]
  (loop [i i, ret []]
    (let [[acc' i'] (parse-regex re i [])]
      (case (nth re i')
        \| (recur (inc i') (conj ret acc'))
        \) [(conj acc (conj ret acc')) i']))))

(defn parse [re]
  (first (parse-regex re 0 [])))

(defn step [{m :map [y x :as pos] :pos :as state} route]
  (if (coll? route)
    (reduce (fn [s r] (reduce step (assoc s :pos pos) r)) state route)
    (let [pos' (case route
                 \N [(dec y) x]
                 \E [y (inc x)]
                 \W [y (dec x)]
                 \S [(inc y) x])]
      (-> state
          (assoc :pos pos')
          (update :map conj #{pos pos'})))))

(defn shortest-path-steps [m steps n [y x :as pos]]
  (let [neighbors (for [[dy dx] [[0 1] [1 0] [0 -1] [-1 0]]
                        :let [pos' [(+ y dy) (+ x dx)]]
                        :when (and (not (get steps pos'))
                                   (get m #{pos pos'}))]
                    pos')
        steps' (assoc steps pos n)]
    (case (count neighbors)
      0 steps'
      1 (recur m steps' (inc n) (first neighbors))
      (->> neighbors
           (map (partial shortest-path-steps m steps' (inc n)))
           (apply merge)))))

(defn solve1 [re]
  (let [r (parse re)
        {m :map} (reduce step {:map #{} :pos [0 0]} r)]
    (->> (shortest-path-steps m {} 0 [0 0])
         vals
         (apply max))))

(defn solve2 [re]
  (let [r (parse re)
        {m :map} (reduce step {:map #{} :pos [0 0]} r)]
    (->> (shortest-path-steps m {} 0 [0 0])
         (filter (fn [[_ n]] (>= n 1000)))
         count)))
