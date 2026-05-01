(ns nexus-clj.ecs-test
  (:require [clojure.test :refer [deftest is testing]]
            [nexus-clj.ecs :as ecs]
            [nexus-clj.movement :refer [move-agent]]
            [nexus-clj.plugins :refer [register-plugin]]))

;; ── Minimal test world fixture ───────────────────────────────────
(def test-world
  {:nodes {:village-square {:label "Village Square"}
           :village-hut    {:label "Elder's Hut"}
           :vault          {:label "Vault"}}
   :edges [{:nexus/from :village-square :nexus/to :village-hut :nexus/distance 1 :nexus/risk 0.0}
           {:nexus/from :village-square :nexus/to :vault        :nexus/distance 1 :nexus/risk 0.0}]
   :agents []
   :pending-events []})

(defn- make-test-world []
  (assoc test-world :agents []))

;; ── Entity creation & component access ──────────────────────────
(deftest test-create-entity
  (let [entity (ecs/make-entity :hero
                                :position (ecs/->Position "village-square" (:graph test-world))
                                :health   (ecs/->Health 10 10))]
    (is (= :hero (:id entity)))
    (is (ecs/has-component? entity :position))
    (is (ecs/has-component? entity :health))
    (is (not (ecs/has-component? entity :inventory)))
    (is (= "village-square" (:node-name (ecs/get-component entity :position))))
    (is (= 10 (:current (ecs/get-component entity :health))))))

(deftest test-update-component
  (let [entity (ecs/make-entity :hero
                                :health (ecs/->Health 10 10))
        updated (ecs/update-component entity :health
                                      (fn [h] (assoc h :current 5)))]
    (is (= 5 (:current (ecs/get-component updated :health))))
    (is (= 10 (:max (ecs/get-component updated :health))))))

;; ── Entity movement via Positionable ───────────────────────────
(deftest test-ecs-entity-movement
  (let [world  (make-test-world)
        entity (ecs/make-entity :scout
                                :position (ecs/->Position "village-square" (:graph world))
                                :health   (ecs/->Health 5 5))
        world  (assoc world :agents [entity])
        result (move-agent world entity :village-hut)]
    (is (= :village-hut
           (:node-name (ecs/get-component (first (:agents result)) :position))))
    (testing "health unchanged by movement"
      (is (= 5 (:current (ecs/get-component (first (:agents result)) :health)))))))

;; ── Hook integration (veto via :validate) ──────────────────────
(deftest test-ecs-entity-veto
  (let [world  (make-test-world)
        entity (ecs/make-entity :hero
                                :position (ecs/->Position "village-square" (:graph world))
                                :health   (ecs/->Health 10 10))
        world  (assoc world :agents [entity])]
    (register-plugin :validate
                     (fn [ctx]
                       (if (= (:target-node ctx) :vault)
                         (assoc ctx :move-allowed? false)
                         ctx)))
    (is (thrown? Exception (move-agent world entity :vault)))
    ;; Clean up – remove the veto plugin
    (swap! nexus-clj.plugins/plugin-registry update :validate
           (fn [plugins] (vec (drop-last plugins))))))

;; ── Component read inside a hook ──────────────────────────────
(deftest test-hook-reads-health-component
  (let [world      (make-test-world)
        entity     (ecs/make-entity :hero
                                    :position (ecs/->Position "village-square" (:graph world))
                                    :health   (ecs/->Health 3 10))
        world      (assoc world :agents [entity])
        healing-log (atom nil)]
    (register-plugin :arrival
                     (fn [ctx]
                       (when-let [health (ecs/get-component (:entity ctx) :health)]
                         (reset! healing-log (:current health)))
                       ctx))
    (move-agent world entity :village-hut)
    (is (= 3 @healing-log))
    ;; Clean up
    (swap! nexus-clj.plugins/plugin-registry update :arrival
           (fn [plugins] (vec (drop-last plugins))))))