(ns intro-to-ec.heuristic-search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map]))

(defn abs [n]
  (max n (* -1 n)))

(defn distance-heuristic
  [current-node goal-node]
  (+
   (abs
    (- (first current-node) (first goal-node)))
   (abs
    (- (last current-node) (last goal-node)))))

(defn remove-previous-node
  [new-node frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-node))

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

(defn search
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children]}
   start-node max-calls]
  (loop [frontier [start-node]
         came-from {start-node :start-node}
         num-calls 0]
    (println num-calls ": " frontier)
    (println came-from)
    (let [current-node (get-next-node frontier)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-node
                    (make-children current-node) frontier (keys came-from))]
          (recur
           (add-children
            kids
            (rest frontier))
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)))))))
