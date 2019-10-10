(ns intro-to-ec.a-star-search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as pm]))

(defn remove-previous-node
  [new-nodes frontier visited cost-so-far]
   (reduce (fn [map keyval]
            (assoc map (first keyval) (second keyval)))
          (pm/priority-map)
           (filter (fn [node]
            (or (not
                     (contains? cost-so-far (first node)))
                (<
                   (second node) (cost-so-far (first node)))
                )) new-nodes)))

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

(defn add-children
  [children frontier heuristic]
  (let [frontier (reduce #(assoc %1 (first %2) (second %2))
                         (pm/priority-map)
                         (remove (fn [frontier] (contains? (set children) (first frontier)))
                                 (pop frontier)))]
    (reduce (fn [frontier child]
              (assoc frontier (key child) (+ (heuristic (key child)) (val child))))
            frontier children)))

(defn add-to-came-from
  [children parent came-from]
  (reduce (fn [cf child] (assoc cf child parent))
          came-from children))

(defn search
  [{:keys [goal? make-children generate-cost heuristic]}
   start-node max-calls]
  (loop [frontier (pm/priority-map start-node (heuristic start-node))
         came-from {start-node :start-node}
         cost-so-far {start-node 0}
         num-calls 0]
    (println num-calls ": " frontier)
    ; (println came-from)
    (if (nil? (first frontier))
      :no-solutions-found
      (let [current-node (key (first frontier))]
        (cond
          (goal? current-node) (generate-path came-from current-node)
          (= num-calls max-calls) :max-calls-reached
          :else
          (let [children (remove-previous-node
                      (generate-cost (make-children current-node) current-node cost-so-far) frontier (keys came-from) cost-so-far)]
            (recur
              (add-children children frontier heuristic)
              (add-to-came-from (keys children) current-node came-from)
              (merge cost-so-far children)
              (inc num-calls))))))))
