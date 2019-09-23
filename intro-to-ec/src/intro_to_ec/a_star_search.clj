(ns intro-to-ec.a-star-search
  (:require [clojure.data.priority-map :as pm]))

(defn search
  [{:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [frontier (pm/priority-map start-node (heuristic start-node))
         came-from {start-node :start-node}
         num-calls 0]
    (println num-calls ": " frontier)
    (println came-from)
    (if (nil? (first frontier))
      :no-solutions-found
      (let [current-node (key (first frontier))]
        (cond
          (goal? current-node) (generate-path came-from current-node)
          (= num-calls max-calls) :max-calls-reached
          :else
          (let [children (remove-previous-node
                      (make-children current-node) frontier (keys came-from))]
            (recur
              (add-children children frontier heuristic)
              (add-to-came-from children current-node came-from)
              (inc num-calls))))))))
