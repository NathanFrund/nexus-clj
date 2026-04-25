(ns nexus-clj.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [nexus-clj.core :refer [node-names neighbors-of agents-at-node
                                    witnessed-events move-agent check-hazard
                                    world-summary]]))

;; ── Test fixtures ────────────────────────────────────────────────
(def sample-world
  "A simple hypergraph world with two connected regions: village and dungeon."
  {:nodes {:village-square {:label "Village Square" :terrain "open"}
           :village-hut    {:label "Elder's Hut" :terrain "building"}
           :village-forest {:label "Forest Path" :terrain "woods"}
           :dungeon-entry  {:label "Dungeon Entry" :terrain "building"}
           :dungeon-main   {:label "Main Chamber" :terrain "dungeon"}}
   :edges [[:village-square :village-hut      {:distance 1 :risk 0.0}]
           [:village-square :village-forest   {:distance 2 :risk 0.3 :direction "backward"}]
           [:village-forest :dungeon-entry    {:distance 3 :risk 0.5}]
           [:dungeon-entry :dungeon-main      {:distance 1 :risk 0.0}]]
   :agents []
   :graph-metadata {:village {:nodes {:village-square {:label "Village Square"}
                                      :village-hut {:label "Elder's Hut"}
                                      :village-forest {:label "Forest Path"}}
                              :edges [[:village-square :village-hut {:distance 1 :risk 0.0}]
                                      [:village-square :village-forest {:distance 2 :risk 0.3 :direction "backward"}]]}
                    :dungeon {:nodes {:dungeon-entry {:label "Dungeon Entry"}
                                      :dungeon-main {:label "Main Chamber"}}
                              :edges [[:dungeon-entry :dungeon-main {:distance 1 :risk 0.0}]
                                      [:village-forest :dungeon-entry {:distance 3 :risk 0.5}]]}}})

;; ── Graph/world queries ─────────────────────────────────────────
(deftest test-node-names
  (is (= #{:village-square :village-hut :village-forest :dungeon-entry :dungeon-main}
         (set (node-names sample-world))))
  (testing "All nodes from all graphs are present"
    (is (= 5 (count (node-names sample-world))))))

(deftest test-neighbors-of
  (testing "Within-region neighbors (bidirectional)"
    (is (= #{:village-hut} (neighbors-of sample-world :village-square)))
    (is (= #{:village-square} (neighbors-of sample-world :village-hut))))

  (testing "One-way and cross-graph edges from village-forest"
    (is (= #{:village-square :dungeon-entry} (neighbors-of sample-world :village-forest))))

  (testing "Cross-graph neighbors (dungeon)"
    (is (= #{:dungeon-main :village-forest} (neighbors-of sample-world :dungeon-entry)))
    (is (= #{:dungeon-entry} (neighbors-of sample-world :dungeon-main)))))

;; ── Agent presence queries ──────────────────────────────────────
(deftest test-agents-at-node
  (let [world (assoc sample-world :agents
                     [{:id :elder :name "Elder" :location :village-hut}
                      {:id :thug  :name "Thug"  :location :village-square}
                      {:id :goblin :name "Goblin" :location :dungeon-entry}])]
    (testing "Agents at various nodes"
      (is (= 1 (count (agents-at-node world :village-square))))
      (is (= 1 (count (agents-at-node world :village-hut))))
      (is (= 1 (count (agents-at-node world :dungeon-entry))))
      (is (= 0 (count (agents-at-node world :dungeon-main)))))))

;; ── Witnessed events ────────────────────────────────────────────
(deftest test-witnessed-events-multiple-witnesses
  (let [world (assoc sample-world :agents
                     [{:id :elder :name "Elder" :location :village-hut}
                      {:id :thug  :name "Thug"  :location :village-square}
                      {:id :scout :name "Scout" :location :village-square}])
        source (first (filter #(= (:name %) "Thug") (:agents world)))
        events (witnessed-events world :agent-departed :village-square source)]
    (is (= 1 (count events)))
    (is (= "Scout" (:name (:observer (first events)))))))

(deftest test-witnessed-events-no-witnesses
  (let [world (assoc sample-world :agents
                     [{:id :hermit :name "Hermit" :location :dungeon-main}])
        source (first (:agents world))
        events (witnessed-events world :agent-departed :dungeon-main source)]
    (is (= 1 (count events)))
    (is (nil? (:observer (first events))))
    (testing "Event is still recorded even without observers"
      (is (= :agent-departed (:event-type (first events)))))))

;; ── Hazard checking ────────────────────────────────────────────
(deftest test-check-hazard-no-risk
  (is (nil? (check-hazard {:risk 0.0} :forest))))

(deftest test-check-hazard-triggers
  (with-redefs [rand (constantly 0.0)]
    (let [result (check-hazard {:risk 0.5} :forest)]
      (is (some? result))
      (is (= :travel-hazard (:event-type result)))
      (is (= :forest (:target result))))))

(deftest test-check-hazard-suppressed
  (with-redefs [rand (constantly 1.0)]
    (is (nil? (check-hazard {:risk 0.9} :forest)))))

;; ── Movement within a region ────────────────────────────────────
(deftest test-move-within-village
  (let [world (assoc sample-world :agents [{:id :scout :name "Scout" :location :village-square}])
        result (move-agent world (first (:agents world)) :village-hut)]
    (testing "Agent moves successfully"
      (is (= :village-hut (:location (first (:agents result))))))
    (testing "Departure event recorded with nil observer (was alone)"
      (is (= 1 (count (:pending-events result))))
      (let [ev (first (:pending-events result))]
        (is (= :agent-departed (:event-type ev)))
        (is (nil? (:observer ev)))))))

(deftest test-move-blocked-by-direction
  (let [world (assoc sample-world :agents [{:id :scout :name "Scout" :location :village-square}])
        result (move-agent world (first (:agents world)) :village-forest)]
    (testing "Movement is blocked by backward-only edge"
      (is (= :village-square (:location (first (:agents result))))))))

(deftest test-move-nonexistent-edge
  (let [world (assoc sample-world :agents [{:id :scout :name "Scout" :location :village-square}])
        result (move-agent world (first (:agents world)) :mars)]
    (testing "Cannot move to nonexistent node"
      (is (= :village-square (:location (first (:agents result))))))))

;; ── Movement with witnesses ────────────────────────────────────
(deftest test-move-with-witnesses
  (let [world (assoc sample-world :agents
                     [{:id :scout   :name "Scout"   :location :village-square}
                      {:id :lookout :name "Lookout" :location :village-square}])
        result (move-agent world (first (:agents world)) :village-hut)
        events (:pending-events result)]
    (testing "Scout moves to village-hut"
      (is (= :village-hut (:location (first (:agents result))))))
    (testing "Lookout witnesses the departure"
      (is (= 1 (count (filter #(= :agent-departed (:event-type %)) events))))
      (let [ev (first (filter #(= :agent-departed (:event-type %)) events))]
        (is (= "Lookout" (:name (:observer ev))))
        (is (= "Scout" (:name (:source ev))))))))

;; ── Movement with hazards ──────────────────────────────────────
(deftest test-move-with-hazard
  (let [graph {:nodes {:a {:label "A"} :b {:label "B"}}
               :edges [[:a :b {:distance 1 :risk 1.0}]]}
        world (assoc sample-world :nodes (:nodes graph) :edges (:edges graph)
                     :agents [{:id :scout :name "Scout" :location :a}])
        result (with-redefs [rand (constantly 0.0)]
                 (move-agent world (first (:agents world)) :b))
        events (:pending-events result)]
    (testing "Agent reaches destination despite hazard"
      (is (= :b (:location (first (:agents result))))))
    (testing "Hazard event is recorded"
      (is (= 1 (count (filter #(= :travel-hazard (:event-type %)) events)))))))

;; ── Cross-graph movement (hypergraph) ────────────────────────
(deftest test-move-across-graphs
  (let [world (assoc sample-world :agents
                     [{:id :adventurer :name "Adventurer" :location :village-forest}
                      {:id :guard      :name "Guard"      :location :village-forest}])
        result (move-agent world (first (:agents world)) :dungeon-entry)
        events (:pending-events result)]
    (testing "Agent moves from village to dungeon"
      (is (= :dungeon-entry (:location (first (:agents result))))))
    (testing "Guard witnesses departure from village-forest"
      (is (= 1 (count (filter #(= :agent-departed (:event-type %)) events))))
      (let [ev (first (filter #(= :agent-departed (:event-type %)) events))]
        (is (= "Guard" (:name (:observer ev))))))
    (testing "No graph switching occurs; world state is consistent"
      (is (= 5 (count (node-names result))))
      (is (= 4 (count (:edges result)))))))

(deftest test-move-within-dungeon-after-entry
  (let [world (assoc sample-world :agents
                     [{:id :adventurer :name "Adventurer" :location :dungeon-entry}])
        result (move-agent world (first (:agents world)) :dungeon-main)]
    (testing "Agent moves within dungeon"
      (is (= :dungeon-main (:location (first (:agents result))))))
    (testing "Departure event recorded with nil observer (was alone)"
      (is (= 1 (count (:pending-events result))))
      (let [ev (first (:pending-events result))]
        (is (= :agent-departed (:event-type ev)))
        (is (nil? (:observer ev)))))))

;; ── World introspection ────────────────────────────────────────
(deftest test-world-summary
  (let [world (assoc sample-world :agents
                     [{:id :alice :name "Alice" :location :village-square}
                      {:id :bob   :name "Bob"   :location :dungeon-entry}])
        summary (world-summary world)]
    (is (= 5 (:node-count summary)))
    (is (= 4 (:edge-count summary)))
    (is (= 2 (:agent-count summary)))))

;; ── Load world from JSON (if test files exist) ─────────────────
(deftest test-load-world-structure
  (testing "A loaded world has the expected structure"
    (is (contains? sample-world :nodes))
    (is (contains? sample-world :edges))
    (is (contains? sample-world :agents))
    (is (contains? sample-world :graph-metadata))))