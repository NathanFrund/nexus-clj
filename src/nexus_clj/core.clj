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
   Returns a map with :nodes (all nodes flattened), :edges (all edges as maps),
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
        ;; Edges are maps with :from, :to, :distance, :risk, :direction etc.
        all-edges (reduce (fn [acc graph]
                            (into acc
                                  (map (fn [edge]
                                         (-> edge
                                             (update :from keyword)
                                             (update :to keyword)))
                                       (:edges graph))))
                          []
                          (vals graphs))]
    {:nodes all-nodes
     :edges all-edges
     :agents []
     :graph-metadata graphs}))

;; ── World queries ────────────────────────────────────────────────
(defn node-names [world]
  (keys (:nodes world)))

(defn edges-from
  "Return all edges incident to node-id."
  [world node-id]
  (let [kw (keyword node-id)]
    (filter (fn [edge]
              (or (= (:from edge) kw)
                  (= (:to edge) kw)))
            (:edges world))))

(defn direction-allows?
  "Check if movement along edge is allowed from from-id."
  [edge from-id]
  (let [dir (:direction edge "both")
        kw  (keyword from-id)]
    (or (= dir "both")
        (and (= dir "forward")  (= (:from edge) kw))
        (and (= dir "backward") (= (:to edge) kw)))))

(defn neighbors-of
  "Return the set of node IDs (keywords) directly reachable from node-id."
  [world node-id]
  (let [kw (keyword node-id)]
    (->> (edges-from world kw)
         (keep (fn [edge]
                 (when (direction-allows? edge kw)
                   (if (= (:from edge) kw)
                     (:to edge)
                     (:from edge)))))
         set)))

;; ── Agent queries ────────────────────────────────────────────────
(defn agents-at-node
  [world node-id]
  (let [kw (keyword node-id)]
    (filter #(= (:location %) kw) (:agents world))))

(defn witnessed-events
  ([world event-type node-id]
   (witnessed-events world event-type node-id nil))
  ([world event-type node-id source-agent]
   (let [source-id  (:id source-agent)
         witnesses (remove #(= (:id %) source-id) (agents-at-node world node-id))]
     (if (seq witnesses)
       (map (fn [observer]
              {:event-type event-type
               :observer   observer
               :source     source-agent
               :location   node-id})
            witnesses)
       (list {:event-type event-type
              :observer   nil
              :source     source-agent
              :location   node-id})))))

;; ── Movement helpers ─────────────────────────────────────────────
(defn- find-edge
  "Return the edge map connecting from-node to to-node, or nil."
  [world from-node to-node]
  (let [from-kw (keyword from-node)
        to-kw   (keyword to-node)]
    (->> (edges-from world from-kw)
         (filter (fn [edge]
                   (or (and (= (:from edge) from-kw) (= (:to edge) to-kw))
                       (and (= (:to edge) from-kw) (= (:from edge) to-kw)))))
         first)))

(defn- apply-spatial-move
  [world agent-id target-id]
  (update world :agents
          (fn [agents]
            (mapv #(if (= (:id %) agent-id)
                     (assoc % :location target-id)
                     %)
                  agents))))

(defn check-hazard
  "Given an edge map and destination node id (keyword), returns a
   hazard event map if random check succeeds, otherwise nil."
  [edge target-id]
  (let [risk (:risk edge 0.0)]
    (when (and (> risk 0.0) (> risk (rand)))
      {:event-type :travel-hazard
       :target     target-id
       :risk       risk})))

;; ── Main movement function ──────────────────────────────────────
(defn move-agent
  [world agent target-id]
  (let [current   (:location agent)
        kw-target (keyword target-id)
        edge      (find-edge world current kw-target)]
    (if (and edge (direction-allows? edge current))
      (let [observers (witnessed-events world :agent-departed current agent)
            hazard    (check-hazard edge kw-target)
            events    (cond-> (vec observers)
                        hazard (conj hazard))
            world'    (-> world
                          (update :pending-events (fnil into []) events)
                          (apply-spatial-move (:id agent) kw-target))]
        world')
      (do (println "Movement not allowed")
          world))))

;; ── World introspection ──────────────────────────────────────────
(defn world-summary [world]
  {:node-count (count (:nodes world))
   :edge-count (count (:edges world))
   :agent-count (count (:agents world))
   :agents (map (fn [a] {:id (:id a) :name (:name a) :location (:location a)})
                (:agents world))})