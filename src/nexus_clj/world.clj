(ns nexus-clj.world
  "World loading, querying, and introspection."
  (:require [cheshire.core :as json]
            [clojure.walk :as walk]))

;; ----------------------------------------------------------------------
;; World Loading (JSON, hypergraph flattening)
;; ----------------------------------------------------------------------

(defn- keywordize-keys
  "Recursively convert all string keys to keywords, normalising JSON input."
  [m]
  (walk/postwalk (fn [x]
                   (if (map? x)
                     (into {} (map (fn [[k v]]
                                     [(if (string? k) (keyword k) k) v])
                                   x))
                     x))
                 m))

(defn load-world
  "Load a Nexus world from a JSON file containing multiple named graphs.
   Flattens all nodes and edges into a single world map.
   Node IDs are keywords, globally unique across all graphs."
  [filepath]
  (let [config (-> (slurp filepath)
                   (json/parse-string true)
                   keywordize-keys)
        graphs (:graphs config)
        all-nodes (reduce (fn [acc graph]
                            (merge acc (:nodes graph)))
                          {}
                          (vals graphs))
        all-edges (reduce (fn [acc graph]
                            (into acc
                                  (map (fn [edge]
                                         (let [from (if (string? (:from edge)) (keyword (:from edge)) (:from edge))
                                               to   (if (string? (:to edge)) (keyword (:to edge)) (:to edge))]
                                           (-> edge
                                               (assoc :nexus/from from)
                                               (assoc :nexus/to to)
                                               (assoc :nexus/distance (:distance edge))
                                               (assoc :nexus/risk (:risk edge))
                                               (cond-> (:direction edge) (assoc :nexus/direction (keyword (:direction edge)))))))
                                       (:edges graph))))
                          []
                          (vals graphs))]
    {:nodes all-nodes
     :edges all-edges
     :agents []
     :pending-events []
     :graph-metadata graphs}))

;; ----------------------------------------------------------------------
;; World queries (adapted for :nexus/ namespaced keys)
;; ----------------------------------------------------------------------

(defn node-names [world]
  (keys (:nodes world)))

(defn edges-from
  "Return all edges incident to node-id."
  [world node-id]
  (let [kw (keyword node-id)]
    (filter (fn [edge]
              (or (= (:nexus/from edge) kw)
                  (= (:nexus/to edge) kw)))
            (:edges world))))

(defn direction-allows?
  "Check if movement along edge is allowed from from-id."
  [edge from-id]
  (let [dir (or (:nexus/direction edge) :both)
        kw  (keyword from-id)]
    (or (= dir :both)
        (and (= dir :forward)  (= (:nexus/from edge) kw))
        (and (= dir :backward) (= (:nexus/to edge) kw)))))

(defn neighbors-of
  "Return the set of node IDs (keywords) directly reachable from node-id."
  [world node-id]
  (let [kw (keyword node-id)]
    (->> (edges-from world kw)
         (keep (fn [edge]
                 (when (direction-allows? edge kw)
                   (if (= (:nexus/from edge) kw)
                     (:nexus/to edge)
                     (:nexus/from edge)))))
         set)))

;; ── Agent queries ─────────────────────────────────────────────
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

;; ----------------------------------------------------------------------
;; World introspection
;; ----------------------------------------------------------------------

(defn world-summary [world]
  {:node-count (count (:nodes world))
   :edge-count (count (:edges world))
   :agent-count (count (:agents world))
   :agents (map (fn [a] {:id (:id a) :name (:name a) :location (:location a)})
                (:agents world))})