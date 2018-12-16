(ns advent-of-code-2018.day14
  (:require [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)

(defn num->digits [^long n]
  (if (< n 10)
    [n]
    [(quot n 10) (rem n 10)]))

(def init-state
  {:recipes [3 7] :fst 0 :snd 1})

(defn solve [finished?]
  (loop [recipes [3 7], fst 0, snd 1]
    (if (finished? recipes)
      recipes
      (let [m (long (recipes fst))
            n (long (recipes snd))
            s (+ m n)
            recipes' (cond-> recipes
                       (>= s 10) (conj (quot s 10))
                       true (conj (rem s 10)))]
        (recur recipes'
               (rem (+ fst m 1) (count recipes'))
               (rem (+ snd n 1) (count recipes')))))))

(defn solve1 [^long n]
  (-> (solve #(>= (count %) (+ n 10)))
      (subvec n (+ n 10))
      str/join))

(defn matching-tail [ds recipes]
  (let [n-recipes (count recipes)
        n-ds (count ds)]
    (or (and (>= n-recipes n-ds)
             (let [s (- n-recipes n-ds), e n-recipes]
               (and (= (subvec recipes s e) ds)
                    [s e])))
        (and (>= n-recipes (inc n-ds))
             (let [s (- n-recipes (inc n-ds))
                   e (dec n-recipes)]
              (and (= (subvec recipes s e) ds)
                   [s e]))))))

(defn solve2 [ds]
  (->> (solve (partial matching-tail ds))
       (matching-tail ds)
       first))
