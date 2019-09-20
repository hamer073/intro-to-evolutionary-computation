(ns intro-to-ec.heuristic-search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as pm]))

(defn remove-previous-node
  [new-nodes frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-nodes))

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

(defn add-children
  [children frontier heuristic]
  (let [frontier (pop frontier)]
    (reduce (fn [child] assoc frontier child (heuristic child))
         children)))

(defn add-to-came-from
  [children parent came-from]
  (reduce (fn [cf child] (assoc cf child parent))
          came-from children))

(defn search
  [{:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [frontier (pm/priority-map start-node 0)
         came-from {start-node :start-node}
         num-calls 0]
    (println num-calls ": " frontier)
    (println came-from)
    (let [current-node (first frontier)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [children (remove-previous-node
                    (make-children current-node) frontier (keys came-from))]
          (recur
            (add-children children frontier heuristic)
            (add-to-came-from children current-node came-from)
            (inc num-calls)))))))
