(ns nexus-clj.core
  (:require [cheshire.core :as json]
            [clojure.walk :as walk]))

;; ── World loading ─────────────────────────────────────────────────
(defn- keywordize-keys
  "Recursively convert all string keys to keywords, including in nested structures.
   This normalizes JSON input so the rest of the code only deals with keywords."
  [m]
  (walk/postwalk (fn [x]
                   (if (map? x)
                     (into {} (map (fn [[k v]]
                                     [(if (string? k) (keyword k) k) v])
                                   x))
                     x))
                 m))
(defn load-world
  "Load a Nexus world from a JSON file containing multiple graphs.
   Normalizes all keys to keywords so the rest of the system works with
   consistent data types.
   Returns a map with :nodes (all nodes flattened), :edges (all edges),
   and :graph-metadata (original graph structure for reference).
   Node IDs are keywords and globally unique across all graphs."
  [filepath]
  (let [config (-> (slurp filepath)
                   (json/parse-string true)
                   keywordize-keys)
        graphs (:graphs config)

        ;; Flatten all nodes into one map
        all-nodes (reduce (fn [acc graph]
                            (merge acc (:nodes graph)))
                          {}
                          (vals graphs))

        ;; Flatten all edges into one vector
        ;; Edges are already [from to attrs] with node IDs as keywords
        all-edges (reduce (fn [acc graph]
                            (into acc (:edges graph)))
                          []
                          (vals graphs))]
    {:nodes all-nodes
     :edges all-edges
     :agents []
     :graph-metadata graphs}))

;; ── World queries ────────────────────────────────────────────────
(defn node-names
  "Return the set of all node IDs (keywords) in the world."
  [world]
  (keys (:nodes world)))

(defn edges-from
  "Return all edges incident to node-id."
  [world node-id]
  (let [kw (keyword node-id)]
    (filter (fn [[a b _]]
              (or (= a kw) (= b kw)))
            (:edges world))))

(defn direction-allows?
  "Check if movement along edge is allowed from from-id."
  [edge from-id]
  (let [dir (get-in edge [2 :direction] "both")
        a   (first edge)
        b   (second edge)
        kw  (keyword from-id)]
    (or (= dir "both")
        (and (= dir "forward")  (= a kw))
        (and (= dir "backward") (= b kw)))))

(defn neighbors-of
  "Return the set of node IDs (keywords) directly reachable from node-id."
  [world node-id]
  (let [kw (keyword node-id)]
    (->> (edges-from world kw)
         (keep (fn [edge]
                 (when (direction-allows? edge kw)
                   (let [a (first edge) b (second edge)]
                     (if (= a kw) b a)))))
         set)))

;; ── Agent queries ────────────────────────────────────────────────
(defn agents-at-node
  "Return all agents currently at node-id. node-id may be a string or keyword.
   (Currently agents are identified by :name; a future iteration may introduce
   stable :id values for Persona Engine integration.)"
  [world node-id]
  (let [kw (keyword node-id)]
    (filter #(= (:location %) kw) (:agents world))))

(defn witnessed-events
  "Return a sequence of witnessed event maps for agents at node-id,
   optionally excluding a source agent.
   If the source is the only agent at the node, returns a single event
   with :observer nil (the departure is still recorded). This ensures that
   the Persona Engine always knows that the action happened, even when nobody
   else was present."
  ([world event-type node-id]
   (witnessed-events world event-type node-id nil))
  ([world event-type node-id source-agent]
   (let [source-name (:name source-agent)
         witnesses   (remove #(= (:name %) source-name) (agents-at-node world node-id))]
     (if (seq witnesses)
       (map (fn [observer]
              {:event-type event-type
               :observer   observer
               :source     source-agent
               :location   node-id})
            witnesses)
       ;; No witnesses → still emit the event with nil observer
       (list {:event-type event-type
              :observer   nil
              :source     source-agent
              :location   node-id})))))

;; ── Movement helpers ─────────────────────────────────────────────
(defn- find-edge
  "Linear search for the edge connecting from-node to to-node."
  [world from-node to-node]
  (let [from-kw (keyword from-node)
        to-kw   (keyword to-node)]
    (->> (edges-from world from-kw)
         (filter (fn [[a b _]]
                   (or (and (= a from-kw) (= b to-kw))
                       (and (= b from-kw) (= a to-kw)))))
         first)))

(defn- apply-spatial-move
  "Return world with the named agent's :location set to target-id (keyword)."
  [world agent-name target-id]
  (update world :agents
          (fn [agents]
            (mapv #(if (= (:name %) agent-name)
                     (assoc % :location target-id)
                     %)
                  agents))))

(defn check-hazard
  "Given edge attribute map and destination node id (keyword), returns a
   hazard event map if random check succeeds, otherwise nil."
  [edge-attrs target-id]
  (let [risk (get edge-attrs :risk 0.0)]
    (when (and (> risk 0.0) (> risk (rand)))
      {:event-type :travel-hazard
       :target     target-id
       :risk       risk})))

;; ── Main movement function ──────────────────────────────────────
(defn move-agent
  "Move an agent to a target node, respecting edge direction and risk.
   Generates :agent-departed events for all observers at the origin node
   before the move. If a hazard occurs, a :travel-hazard event is added.
   All events are stored under :pending-events.
   With hypergraphs, there is no graph switching; the world state remains
   constant across regions."
  [world agent target-id]
  (let [current   (:location agent)
        kw-target (keyword target-id)
        edge      (find-edge world current kw-target)]
    (if (and edge (direction-allows? edge current))
      (let [observers (witnessed-events world :agent-departed current agent)
            hazard    (check-hazard (nth edge 2) kw-target)
            events    (cond-> (vec observers)
                        hazard (conj hazard))
            world'    (-> world
                          (update :pending-events (fnil into []) events)
                          (apply-spatial-move (:name agent) kw-target))]
        world')
      (do (println "Movement not allowed")
          world))))

;; ── World introspection ──────────────────────────────────────────
(defn world-summary
  "Return a summary of the world state for debugging."
  [world]
  {:node-count (count (:nodes world))
   :edge-count (count (:edges world))
   :agent-count (count (:agents world))
   :agents (map (fn [a] {:name (:name a) :location (:location a)})
                (:agents world))})
