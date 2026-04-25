(ns nexus-clj.core
  (:require [cheshire.core :as json]))

;; ── Graph loading ─────────────────────────────────────────────────
(defn load-graph
  "Load a Nexus graph from a JSON file path. Returns a map with :nodes and :edges.
   Node IDs are Clojure keywords (JSON keys are keywordised)."
  [filepath]
  (-> filepath slurp (json/parse-string true)))

;; ── Graph queries ────────────────────────────────────────────────
(defn node-names
  "Return the set of node IDs (keywords) in the graph."
  [graph]
  (keys (:nodes graph)))

(defn edges-from [graph node-id]
  (let [id-str (name node-id)] ;; Normalize input to string
    (filter (fn [[a b _]]
              (or (= (name a) id-str)
                  (= (name b) id-str)))
            (:edges graph))))

(defn direction-allows? [edge from-id]
  (let [dir (get-in edge [2 :direction] "both")
        a   (first edge)
        b   (second edge)
        kw  (keyword from-id)]
    (or (= dir "both")
        (and (= dir "forward")  (= a kw))
        (and (= dir "backward") (= b kw)))))

(defn neighbors-of
  "Return the set of node IDs (keywords) directly reachable from node-id."
  [graph node-id]
  (let [kw (keyword node-id)]
    (->> (edges-from graph kw)
         (keep (fn [edge]
                 (when (direction-allows? edge kw)
                   (let [a (first edge) b (second edge)]
                     (if (= a kw) b a)))))
         set)))

;; ── World queries ────────────────────────────────────────────────
(defn agents-at-node
  "Agents whose :location equals node-id. node-id may be a string or keyword.
   (Currently agents are identified by :name; a future iteration may introduce
   stable :id values for Persona Engine integration.)"
  [world node-id]
  (let [kw (keyword node-id)]
    (filter #(= (:location %) kw) (:agents world))))

(defn witnessed-events
  "Return a lazy sequence of witnessed event maps for agents at node-id,
   optionally excluding a source agent.
   If the source is the only agent at the node, returns a single event
   with :observer nil (the departure is still recorded).  This ensures that
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
  "Linear search for the edge connecting from-node to to-node.
   For graphs with 100+ nodes a hash-index on edges would be faster,
   but the current approach is fine for typical pointcrawl scales."
  [graph from-node to-node]
  (let [from-str (name from-node)
        to-str   (name to-node)]
    (->> (edges-from graph from-str)
         (filter (fn [e]
                   (let [other (if (= (name (first e)) from-str)
                                 (name (second e))
                                 (name (first e)))]
                     (= other to-str))))
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

(defn- handle-transition
  "If the edge has a :transition portal, load the new graph and move
   the agent to the entry node (keyword).  Validates that the entry node
   exists in the target graph; if not, prints a warning and returns the
   world unchanged."
  [world agent-name edge]
  (if-let [portal (get-in edge [2 :transition])]
    (let [new-graph (load-graph (:graph portal))
          entry     (keyword (:entry portal))]
      (if (contains? (:nodes new-graph) entry)
        (-> world
            (assoc :graph new-graph)
            (apply-spatial-move agent-name entry))
        (do
          (println "Warning: portal entry not found:" entry)
          world)))
    world))

;; ── Main movement function ──────────────────────────────────────
(defn move-agent
  "Move an agent to a target node, respecting edge direction and risk.
   Generates :agent-departed events for all observers at the origin node
   before the move. If a hazard occurs, a :travel-hazard event is added.
   All events are stored under :pending-events.
   If the edge has a portal transition, the world's graph is swapped
   and the agent placed at the entry node."
  [world agent target-id]
  (let [current   (:location agent)
        kw-target (keyword target-id)
        edge      (find-edge (:graph world) current kw-target)]
    (if (and edge (direction-allows? edge current))
      (let [observers (witnessed-events world :agent-departed current agent)
            hazard    (check-hazard (nth edge 2) kw-target)
            events    (cond-> (vec observers)
                        hazard (conj hazard))
            world'    (-> world
                          (assoc :pending-events [])   ;; ← reset events
                          (update :pending-events (fnil into []) events)
                          (apply-spatial-move (:name agent) kw-target))
            _         (when hazard (println (str "Hazard! " (name kw-target))))
            world''   (handle-transition world' (:name agent) edge)]
        world'')
      (do (println "Movement not allowed")
          world))))
