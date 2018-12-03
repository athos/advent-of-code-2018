(ns advent-of-code-2018.day03)

;; for debug
(defn print-map [cells width height]
  (println (apply str `(\+ ~@(repeat width \-) \+)))
  (doseq [y (range height)]
    (print \|)
    (doseq [x (range width)]
      (print (or (some-> (get cells [y x]) count) \.)))
    (println \|))
  (println (apply str `(\+ ~@(repeat width \-) \+))))

(defn parse-claim [s]
  (let [[_ id x y width height] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)]
    {:id id
     :x (Long/parseLong x)
     :y (Long/parseLong y)
     :width (Long/parseLong width)
     :height (Long/parseLong height)}))

(defn add-rect [cells {:keys [id y x width height]}]
  (->> (for [dy (range 0 height)
             dx (range 0 width)]
         [(+ y dy) (+ x dx)])
       (reduce #(update %1 %2 (fnil conj #{}) id) cells)))

(defn solve1 [lines]
  (let [claims (map parse-claim lines)]
    (->> (reduce add-rect {} claims)
         (filter #(> (count (val %)) 1))
         count)))

(defn solve2 [lines]
  (let [claims (map parse-claim lines)
        id->area (reduce #(assoc %1 (:id %2) (* (:width %2) (:height %2))) {} claims)]
    (->> (reduce add-rect {} claims)
         (filter #(= (count (val %)) 1))
         (group-by (comp first val))
         (filter #(= (count (val %)) (id->area (key %))))
         first
         key)))
