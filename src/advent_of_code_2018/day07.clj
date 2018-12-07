(ns advent-of-code-2018.day07)

;;
;; Scheduler implementation
;;

(defprotocol IScheduler
  (tick! [this])
  (schedule! [this cs])
  (idle? [this]))

(defn free-workers [workers]
  (filter (fn [[_ {:keys [t]}]] (= t 0)) workers))

(deftype Scheduler [^:volatile-mutable workers duration-fn]
  IScheduler
  (tick! [this]
    (set! workers
          (into {} (map (fn [[id worker :as e]]
                          (if (:c worker)
                            [id (update worker :t dec)]
                            e)))
                workers))
    (keep (fn [[_ {:keys [c]}]] c) (free-workers workers)))
  (schedule! [this cs]
    (let [free-workers (free-workers workers)]
      (set! workers
            (->> (map vector
                      (map key free-workers)
                      (concat cs (repeat nil)))
                 (reduce (fn [ws [id c]]
                           (cond-> (assoc-in ws [id :c] c)
                             c (assoc-in [id :t] (duration-fn c))))
                         workers)))
      (min (count free-workers) (count cs))))
  (idle? [this]
    (every? (fn [[_ {:keys [t]}]] (= t 0)) workers)))

(defn make-scheduler [n-workers duration-fn]
  (->Scheduler (reduce (fn [workers i] (assoc workers i {:t 0}))
                       {}
                       (range n-workers))
               duration-fn))

;;
;; Main problem solution
;;

(defn parse-line [line]
  (let [[_ x y] (re-matches #"Step (.) must be finished before step (.) can begin." line)]
    [(first x) (first y)]))

(defn steps->dependers [steps]
  (reduce (fn [m [x y]]
            (update m x (fnil conj []) y))
          {} steps))

(defn steps->dependees [steps]
  (reduce (fn [m [x y]]
            (update m y (fnil conj []) x))
          {} steps))

(defn roots-of-deps [dependers dependees]
  (let [candidates (keys dependers)]
    (into (sorted-set) (remove (set (keys dependees))) candidates)))

(defn init-state [scheduler steps]
  (let [dependers (steps->dependers steps)
        dependees (steps->dependees steps)]
    {:dependers dependers
     :dependees dependees
     :scheduler scheduler
     :time -1
     :queue (roots-of-deps dependers dependees)
     :seen #{}
     :done []}))

(defn step [{:keys [time queue seen done dependers dependees scheduler] :as state}]
  (let [done' (tick! scheduler)
        seen' (into seen done')
        unlocked (into #{} (comp (mapcat dependers) (remove seen')) done')
        queue' (apply disj (into queue unlocked) done')
        available (filter #(every? seen' (dependees %)) queue')
        n-scheduled (schedule! scheduler available)]
    (assoc state
           :time (inc time)
           :queue (apply disj queue' (take n-scheduled available))
           :seen seen'
           :done (into done done'))))

(defn solve [state]
  (->> (iterate step state)
       (drop-while (fn [{:keys [queue scheduler]}]
                     (or (not (empty? queue)) (not (idle? scheduler)))))
       first))

(defn solve1 [lines]
  (let [scheduler (make-scheduler 1 (constantly 1))
        steps (map parse-line lines)]
    (->> (init-state scheduler steps)
         solve
         :done
         (apply str))))

(defn solve2 [n d lines]
  (let [scheduler (make-scheduler n #(+ (- (int %) 64) d))
        steps (map parse-line lines)]
    (->> (init-state scheduler steps)
         solve
         :time)))
