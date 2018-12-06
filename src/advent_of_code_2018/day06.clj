(ns advent-of-code-2018.day06)

(set! *unchecked-math* true)

(defn parse-line [line]
  (let [[_ x y] (re-matches #"(\d+), (\d+)" line)]
    [(Long/parseLong y) (Long/parseLong x)]))

(defn points->state [ps]
  (let [[min-y max-y] (apply (juxt min max) (map first ps))
        [min-x max-x] (apply (juxt min max) (map second ps))]
   {:min-y min-y, :min-x min-x, :max-y max-y, :max-x max-x
    :ps (into {} (map-indexed (fn [id coord] [id [coord]])) ps)
    :map (into {} (map-indexed (fn [id coord] [coord #{id}])) ps)}))

(defn neighbors [{m :map :as state} pred coords]
  (->> (for [[y x] coords
             [dy dx] [[1 0] [0 -1] [-1 0] [0 1]]
             :let [coord [(+ y dy) (+ x dx)]]
             :when (and (not (get m coord)) (pred state coord))]
         coord)
       (into #{})))

(defn step [pred {m :map :keys [ps] :as state}]
  (let [ps' (into {} (map (fn [[id coords]] [id (neighbors state pred coords)])) ps)]
    (assoc state
           :ps ps'
           :map (->> (for [[id coords] ps', coord coords] [id coord])
                     (group-by second)
                     (reduce (fn [m [coord ps]]
                               (assoc m coord (set (map first ps))))
                             m)))))

(defn infinite-area-ids [{m :map :keys [min-y min-x max-y max-x]}]
  (->> (concat (for [y [min-y max-y], x (range min-x (inc max-x))] [y x])
               (for [y (range min-y (inc max-y)), x [min-x max-x]] [y x]))
       (reduce (fn [ret coord]
                 (let [ids (get m coord)]
                   (cond-> ret
                     (= (count ids) 1)
                     (into ids))))
               #{})))

(defn exclude-infinite-areas [{m :map :keys [min-y min-x max-y max-x] :as state}]
  (let [excluded? (infinite-area-ids state)]
    (->> (for [y (range min-y (inc max-y)), x (range min-x (inc max-x))] [y x])
         (reduce (fn [m coord]
                   (cond-> m
                     (some excluded? (get m coord))
                     (dissoc coord)))
                 m))))

(defn valid? [{:keys [min-y min-x max-y max-x]} [y x]]
  (and (<= min-y y max-y) (<= min-x x max-x)))

(defn solve1 [lines]
  (loop [state' (step valid? (points->state (map parse-line lines)))]
    (if (every? (comp empty? val) (:ps state'))
      (->> (exclude-infinite-areas state')
           (keep (fn [[_ v]] (when (= (count v) 1) (first v))))
           frequencies
           (apply max-key val)
           val)
      (recur (step valid? state')))))

(defn median-point [ps]
  (let [n (count ps)]
    [(quot (apply + (map first ps)) n)
     (quot (apply + (map second ps)) n)]))

(defn in-region? [ps threshold [y x]]
  (->> ps
       (map (fn [[y' x']]
              (+ (Math/abs (- (long y) (long y')))
                 (Math/abs (- (long x) (long x'))))))
       (apply +)
       (> threshold)))

(defn solve2 [threshold lines]
  (let [ps (map parse-line lines)
        state (points->state [(median-point ps)])
        in-region? #(in-region? ps threshold %2)]
    (loop [state' (step in-region? state)]
      (if (empty? (get-in state' [:ps 0]))
        (count (:map state'))
        (recur (step in-region? state'))))))
