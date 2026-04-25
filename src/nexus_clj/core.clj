(ns nexus-clj.core
  (:require [cheshire.core :as json]))

;; ---- Graph loading ----

(defn load-graph
  "Load a Nexus graph from a JSON file path. Returns a map with :nodes and :edges."
  [filepath]
  (-> filepath slurp (json/parse-string true)))

;; ---- Helpers ----

(defn node-names [graph]
  (keys (:nodes graph)))

(defn edges-from [graph node-id]
  (filter (fn [[a b _]] (or (= a node-id) (= b node-id)))
          (:edges graph)))

(defn direction-allows? [edge from-id]
  (let [dir (get-in edge [2 :direction] "both")
        a   (first edge)
        b   (second edge)]
    (or (= dir "both")
        (and (= dir "forward")  (= a from-id))
        (and (= dir "backward") (= b from-id)))))

(defn neighbors-of [graph node-id]
  (->> (edges-from graph node-id)
       (keep (fn [edge]
               (when (direction-allows? edge node-id)
                 (let [a (first edge) b (second edge)]
                   (if (= a node-id) b a)))))
       set))

;; ---- World queries ----

(defn agents-at-node [world node-id]
  (filter #(= (:location %) node-id) (:agents world)))

(defn witnessed-events
  "Return a lazy sequence of witnessed event maps for agents at node-id,
   optionally excluding a source agent."
  ([world event-type node-id]
   (witnessed-events world event-type node-id nil))
  ([world event-type node-id source-agent]
   (->> (agents-at-node world node-id)
        (remove #{source-agent})
        (map (fn [agent]
               {:event-type event-type
                :observer   agent
                :source     source-agent
                :location   node-id})))))

;; ---- Movement ----

(defn move-agent [world agent target-node-id]
  (let [current (:location agent)
        edge (->> (edges-from (:graph world) current)
                  (filter (fn [e] (let [other (if (= (first e) current) (second e) (first e))]
                                    (= other target-node-id))))
                  first)]
    (if (and edge (direction-allows? edge current))
      (let [risk (get-in edge [2 :risk] 0.0)
            moved-world (update world :agents
                                (fn [agents]
                                  (mapv #(if (= % agent) (assoc % :location target-node-id) %)
                                        agents)))]
        ;; random hazard check (1.0 atRandom equivalent)
        (when (and (> risk 0.0) (> risk (rand)))
          (println (str "Hazard! " target-node-id)))
        moved-world)
      (do (println "Movement not allowed")
          world))))