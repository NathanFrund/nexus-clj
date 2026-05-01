(ns nexus-clj.plugins
  "Plugin registry and default hook implementations."
  (:require [nexus-clj.world :refer [witnessed-events]]))

;; ── Plugin Registry & Runner ─────────────────────────────────────
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

;; ── Default Plugins (replicate legacy behaviour) ────────────────
(register-plugin :departure
                 (fn [ctx]
                   (let [location (:location (:entity ctx))
                         events   (witnessed-events (:world ctx) :agent-departed location (:entity ctx))]
                     (update-in ctx [:world :pending-events] (fn [curr] (into (or curr []) events))))))

(register-plugin :hazard
                 (fn [ctx]
                   (let [edge (:edge ctx)
                         risk (or (:nexus/risk edge) 0.0)]
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