(ns advent-of-code-2018.day13
  (:require [clojure.string :as str]))

(def c->dir {\^ :north, \v :south, \< :west, \> :east})
(def dir->c {:north \^, :south \v, :west \<, :east \>})

(defn parse-line [state y line]
  (reduce (fn [state [x c]]
            (let [loc [y x]
                  cart (when-let [dir (c->dir c)]
                         {:id (count (:carts state))
                          :dir dir :loc loc :turn :left})
                  track (case c
                         (\^ \v) \|
                         (\< \>) \-
                         (\| \- \/ \\ \+) c
                         nil)]
              (cond-> state
                cart (assoc-in [:carts loc (:id cart)] cart)
                track (assoc-in [:map loc] track))))
          state
          (map-indexed vector line)))

(defn parse-lines [lines]
  (reduce (fn [state [y line]]
            (parse-line state y line))
          {:carts (sorted-map) :map {}}
          (map-indexed vector lines)))

;; for debug
(defn print-carts [m carts]
  (let [[min-y max-y] (apply (juxt min max) (map first (keys m)))
        [min-x max-x] (apply (juxt min max) (map second (keys m)))]
    (->> (for [y (range min-y (inc max-y))]
           (->> (for [x (range min-x (inc max-x))
                      :let [loc [y x]]]
                  (if-let [carts (get carts loc)]
                    (if (next carts)
                      \X
                      (dir->c (:dir (val (first carts)))))
                    (get m loc \space)))
                str/join))
         (str/join \newline)
         println)))

(def next-turn
  {:left :straight
   :straight :right
   :right :left})

(defn next-dir [dir turn]
  (case [dir turn]
    [:north :left ] :west
    [:west  :left ] :south
    [:south :left ] :east
    [:east  :left ] :north
    [:north :right] :east
    [:east  :right] :south
    [:south :right] :west
    [:west  :right] :north
    dir))

(defn next-loc [loc dir]
  (case dir
    :north (update loc 0 dec)
    :south (update loc 0 inc)
    :west  (update loc 1 dec)
    :east  (update loc 1 inc)))

(defn advance-cart [m {:keys [loc dir turn] :as cart}]
  (case (get m loc)
    (\| \-) (update cart :loc next-loc (:dir cart))
    \/ (let [dir (next-dir dir (if (#{:north :south} dir) :right :left))]
         (-> cart
             (assoc :dir dir)
             (update :loc next-loc dir)))
    \\ (let [dir (next-dir dir (if (#{:north :south} dir) :left :right))]
         (-> cart
             (assoc :dir dir)
             (update :loc next-loc dir)))
    \+ (let [dir (next-dir dir turn)]
         (-> cart
             (assoc :dir dir)
             (update :turn next-turn)
             (update :loc next-loc dir)))))

(defn tick [m on-crash carts]
  (loop [[[loc cs] & more] (seq carts), carts carts, crashed #{}]
    (if-let [[_ cart] (first cs)]
      (if (crashed loc)
        (recur more carts (disj crashed loc))
        (let [{:keys [id] loc' :loc :as cart'} (advance-cart m cart)
              carts' (-> carts
                         (assoc-in [loc' id] cart')
                         (dissoc loc))
              crashed? (> (count (get carts' loc')) 1)
              carts' (cond->> carts' crashed? (on-crash loc'))]
          (if (reduced? carts')
            @carts'
            (recur more carts' (cond-> crashed crashed? (conj loc'))))))
      carts)))

(defn loc-str [[y x]] (str x \, y))

(defn solve1 [lines]
  (let [{m :map :keys [carts]} (parse-lines lines)]
    (->> (iterate (partial tick m #(reduced %2)) carts)
         (drop-while (fn [carts] (every? #(= (count (val %)) 1) carts)))
         first
         (filter #(> (count (val %)) 1))
         first
         key
         loc-str)))

(defn solve2 [lines]
  (let [{m :map :keys [carts]} (parse-lines lines)]
    (->> (iterate (partial tick m #(dissoc %2 %1)) carts)
         (drop-while (fn [carts] (> (count carts) 1)))
         ffirst
         key
         loc-str)))
