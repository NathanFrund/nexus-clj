(ns nexus-clj.core
  "Nexus — a hook‑driven spatial graph engine.
   Port of the Pharo Nexus Pivot to Clojure."
  (:require [cheshire.core :as json]
            [clojure.walk :as walk]))

;; ----------------------------------------------------------------------
;; 1. Property Graph Helpers (namespaced keywords)
;; ----------------------------------------------------------------------

(def ^:const reserved-ns "nexus")

(defn reserved-key?
  "Returns true if k is a namespaced keyword in the 'nexus' namespace."
  [k]
  (= reserved-ns (namespace k)))

(defn validate-property-key!
  "Throws if a user tries to use a key in the reserved 'nexus' namespace."
  [k]
  (when (reserved-key? k)
    (throw (ex-info (str "Property key must not use namespace " reserved-ns)
                    {:key k}))))

;; Nodes and edges are plain maps.  Structural keys use the :nexus/
;; namespace.  User metadata can be any other keyword.

(defn make-node
  "Create a node with a required id and optional label/properties."
  [id & {:keys [label] :as props}]
  (merge {:nexus/id id :nexus/label (or label id)} props))

(defn make-edge
  "Create an edge between two nodes.  Extra keys become properties."
  [from to & {:keys [distance risk direction] :or {distance 1 risk 0.0 direction :both} :as props}]
  (merge {:nexus/from from :nexus/to to :nexus/distance distance :nexus/risk risk :nexus/direction direction}
         (dissoc props :distance :risk :direction)))

;; Serialisation helpers (maps are already the portable format)
(defn node->property-dict [node] node)
(defn edge->property-dict [edge] edge)
(defn property-dict->node [d] d)
(defn property-dict->edge [d] d)

;; ----------------------------------------------------------------------
;; 2. World Loading (JSON, hypergraph flattening)
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
;; 3. World queries (adapted for :nexus/ namespaced keys)
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
                   (or (and (= (:nexus/from edge) from-kw) (= (:nexus/to edge) to-kw))
                       (and (= (:nexus/to edge) from-kw) (= (:nexus/from edge) to-kw)))))
         first)))

(defn- apply-spatial-move
  [world agent-id target-id]
  (update world :agents
          (fn [agents]
            (mapv #(if (= (:id %) agent-id)
                     (assoc % :location target-id)
                     %)
                  agents))))

;; ----------------------------------------------------------------------
;; 4. Movement Context
;; ----------------------------------------------------------------------

(defn make-context
  "Build the immutable context map passed through every hook."
  [world entity target-node edge]
  {:world        world
   :entity       entity
   :target-node  target-node
   :edge         edge
   :move-allowed? true
   :data         {}})

;; ----------------------------------------------------------------------
;; 5. Plugin Registry & Runner
;; ----------------------------------------------------------------------

(defonce plugin-registry (atom {}))

(defn register-plugin
  "Register a hook function (context -> context)."
  [hook-kw f]
  (swap! plugin-registry update hook-kw (fnil conj []) f))

(defn run-hooks
  "Thread context through every registered function for hook-kw.
   If :move-allowed? becomes false, subsequent hooks (except :validate) are skipped."
  [hook-kw ctx]
  (let [plugins (get @plugin-registry hook-kw [])]
    (reduce (fn [ctx f]
              (if (or (:move-allowed? ctx true)
                      (= :validate hook-kw))
                (f ctx)
                (reduced ctx)))
            ctx
            plugins)))

;; ----------------------------------------------------------------------
;; 6. Default Plugins (replicate legacy behaviour)
;; ----------------------------------------------------------------------

(register-plugin :departure
                 (fn [ctx]
                   (let [location  (:location (:entity ctx))
                         events    (witnessed-events (:world ctx) :agent-departed location (:entity ctx))]
                     (update-in ctx [:world :pending-events] (fn [curr] (into (or curr []) events))))))

(register-plugin :hazard
                 (fn [ctx]
                   (let [edge  (:edge ctx)
                         risk  (or (:nexus/risk edge) 0.0)]
                     (if (and (> risk 0.0) (> risk (rand)))
                       (update-in ctx [:world :pending-events]
                                  (fnil conj [])
                                  {:event-type :travel-hazard
                                   :target     (:target-node ctx)
                                   :risk       risk})
                       ctx))))

(register-plugin :arrival
                 (fn [ctx]
                   ctx))

;; ----------------------------------------------------------------------
;; 7. Protocols for Dual‑Tier Movement
;; ----------------------------------------------------------------------

(defprotocol Positionable
  (get-location [this])
  (set-location [this world new-node]))

(defn make-simple-agent [id name location]
  {:id id :name name :location location})

(extend-protocol Positionable
  clojure.lang.IPersistentMap
  (get-location [this] (:location this))
  (set-location [this world new-node]
    (update world :agents
            (fn [agents]
              (mapv #(if (= (:id %) (:id this))
                       (assoc % :location new-node)
                       %)
                    agents)))))

;; ----------------------------------------------------------------------
;; 8. The Refactored move‑agent (Pure, Hook‑Driven)
;; ----------------------------------------------------------------------

(defn move-agent
  "Move an entity from its current location to target-id.
   Runs the full hook pipeline and returns a new world."
  [world entity target-id]
  (let [current (get-location entity)
        edge    (find-edge world current target-id)]
    (if (and edge (direction-allows? edge current))
      (let [ctx (make-context world entity target-id edge)
            ;; 1. Validate
            ctx1  (run-hooks :validate ctx)
            _     (when-not (:move-allowed? ctx1)
                    (throw (ex-info "Movement blocked" {:reason :veto :context ctx1})))
            ;; 2. Departure
            ctx2  (run-hooks :departure ctx1)
            ;; 3. Hazard
            ctx3  (run-hooks :hazard ctx2)
            ;; Spatial move
            world2 (apply-spatial-move (:world ctx3) (:id entity) target-id)
            ctx4  (assoc ctx3 :world world2)
            ;; 4. Arrival
            ctx5  (run-hooks :arrival ctx4)
            ;; 5. Announce
            ctx6  (run-hooks :announce ctx5)]
        (:world ctx6))
      world)))

;; ----------------------------------------------------------------------
;; 9. World introspection
;; ----------------------------------------------------------------------

(defn world-summary [world]
  {:node-count (count (:nodes world))
   :edge-count (count (:edges world))
   :agent-count (count (:agents world))
   :agents (map (fn [a] {:id (:id a) :name (:name a) :location (:location a)})
                (:agents world))})

;; ----------------------------------------------------------------------
;; 10. Example Usage
;; ----------------------------------------------------------------------

(comment

  (def world
    (load-world "path/to/village.json"))

  (def elder (make-simple-agent :elder "Elder" :elder-hut))
  (def world2 (move-agent (update world :agents conj elder) elder :village))

  (println "Pending events:" (:pending-events world2))
  (println "New location:"   (get-in world2 [:agents 0 :location]))

  ;; Register a custom plugin
  (register-plugin :validate
                   (fn [ctx]
                     (if (= (:target-node ctx) :vault)
                       (assoc ctx :move-allowed? false)
                       ctx)))

  ;; Try moving to vault — will be blocked
  (try
    (move-agent world elder :vault)
    (catch Exception e
      (println (.getMessage e)))))