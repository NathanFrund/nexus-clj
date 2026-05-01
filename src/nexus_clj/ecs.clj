(ns nexus-clj.ecs
  "Optional Entity‑Component‑System layer for Nexus.
   Entities are records that carry a map of components.
   Components are plain Clojure maps implementing the Component protocol.
   This layer is compatible with the simple‑agent path and the full hook pipeline."
  (:require [nexus-clj.protocols :refer [Positionable get-location set-location]]))

;; ── Component protocol ────────────────────────────────────────
(defprotocol Component
  "Tagging protocol for ECS components.
   Returns a keyword identifying the component type."
  (component-type [this]))

;; ── Example components ────────────────────────────────────────
(defrecord Position [node-name graph]
  Component
  (component-type [_] :position))

(defrecord Health [current max]
  Component
  (component-type [_] :health))

;; Add more components here as your game needs them:
;; (defrecord Inventory [items] …)
;; (defrecord Stamina [current max] …)

;; ── Helper for world updates ──────────────────────────────────
(defn update-world-entity
  "Replace an entity in the world's :agents vector by matching :id.
   The :agents vector can contain ECS entities and/or simple maps."
  [world entity]
  (update world :agents
          (fn [agents]
            (mapv #(if (= (:id %) (:id entity)) entity %) agents))))

;; ── Entity record ─────────────────────────────────────────────
(defrecord Entity [id components]
  Positionable
  (get-location [this]
    (when-let [pos (:position components)]
      (:node-name pos)))
  (set-location [this world new-node]
    ;; Return a new world with this entity's position updated.
    ;; Works with the existing :agents vector in the world map.
    (let [new-pos    (assoc (:position components) :node-name new-node)
          new-entity (assoc this :components (assoc components :position new-pos))]
      (update-world-entity world new-entity))))

;; ── Entity constructors ───────────────────────────────────────
(defn make-entity
  "Create an ECS entity with a given id and initial components.
   Components are provided as keyword/value pairs, e.g.
   (make-entity :hero
     :position (->Position 'village' graph)
     :health   (->Health 10 10))"
  [id & {:as component-map}]
  (map->Entity {:id id :components component-map}))

;; ── Component query helpers ───────────────────────────────────
(defn has-component?
  "True if entity has a component of the given type keyword."
  [entity component-kw]
  (contains? (:components entity) component-kw))

(defn get-component
  "Return the component for the given keyword, or nil."
  [entity component-kw]
  (get-in entity [:components component-kw]))

(defn update-component
  "Apply f to the component identified by component-kw and return
   a new entity with the result. f receives the current component
   (or nil) and must return the new component (or nil)."
  [entity component-kw f]
  (let [new-comp (f (get-component entity component-kw))]
    (assoc-in entity [:components component-kw] new-comp)))