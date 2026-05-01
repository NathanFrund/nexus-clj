(ns nexus-clj.movement
  "Movement context, spatial operations, and agent movement."
  (:require [nexus-clj.world :refer [direction-allows? edges-from]]
            [nexus-clj.plugins :refer [run-hooks]]
            [nexus-clj.protocols :refer [Positionable get-location set-location]]))

;; ── Private helpers ─────────────────────────────────────────────
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

;; ── Movement Context ─────────────────────────────────────────────
(defn make-context
  "Build the immutable context map passed through every hook."
  [world entity target-node edge]
  {:world        world
   :entity       entity
   :target-node  target-node
   :edge         edge
   :move-allowed? true
   :data         {}})

;; ── Dual‑Tier Protocol Extension ─────────────────────────────────
(defn make-simple-agent
  "Create a simple agent map that satisfies Positionable."
  [id name location]
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

;; ── Main Movement ────────────────────────────────────────────────
(defn move-agent
  "Move an entity from its current location to target-id.
   Runs the full hook pipeline and returns a new world."
  [world entity target-id]
  (let [current (get-location entity)
        edge    (find-edge world current target-id)]
    (if (and edge (direction-allows? edge current))
      (let [ctx (make-context world entity target-id edge)
            ctx1  (run-hooks :validate ctx)
            _     (when-not (:move-allowed? ctx1)
                    (throw (ex-info "Movement blocked" {:reason :veto :context ctx1})))
            ctx2  (run-hooks :departure ctx1)
            ctx3  (run-hooks :hazard ctx2)
            ;; Use the protocol to update location
            world2 (set-location entity (:world ctx3) target-id)
            ctx4  (assoc ctx3 :world world2)
            ctx5  (run-hooks :arrival ctx4)
            ctx6  (run-hooks :announce ctx5)]
        (:world ctx6))
      world)))