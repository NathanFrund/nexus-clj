(ns nexus-clj.core-test
  (:require [clojure.test :refer [deftest is]]
            [nexus-clj.core :refer [node-names neighbors-of agents-at-node
                                    witnessed-events move-agent check-hazard
                                    load-graph]]))

;; ── Original graph tests (using keywords) ─────────────────────────
(def sample-graph
  {:nodes {:village  {:label "Village Square" :terrain "open"}
           :hut      {:label "Elder's Hut"    :terrain "building"}
           :forest   {:label "Forest Path"    :terrain "woods"}}
   :edges [[:village :hut      {:distance 1 :risk 0.0}]
           [:village :forest   {:distance 2 :risk 0.3 :direction "backward"}]]})

(deftest test-node-names
  (is (= #{:village :hut :forest} (set (node-names sample-graph)))))

(deftest test-neighbors-of
  (is (= #{:hut}      (neighbors-of sample-graph :village)))
  (is (= #{:village}  (neighbors-of sample-graph :hut))))

(deftest test-one-way-neighbor
  (is (= #{:village} (neighbors-of sample-graph :forest))))

(deftest test-agents-at-node
  (let [world {:graph sample-graph
               :agents [{:name "Elder" :location :hut}
                        {:name "Thug"  :location :village}]}]
    (is (= 1 (count (agents-at-node world :village))))
    (is (= 1 (count (agents-at-node world :hut))))
    (is (= 0 (count (agents-at-node world :forest))))))

(deftest test-witnessed-events
  (let [world {:graph sample-graph
               :agents [{:name "Elder" :location :hut}
                        {:name "Thug"  :location :village}]}
        events (witnessed-events world :heroic-act :village (first (:agents world)))]
    (is (= 1 (count events)))
    (is (= "Thug" (:name (:observer (first events)))))))

;; ── Helper for movement tests ────────────────────────────────────
(defn make-world [graph agents]
  {:graph graph :agents agents})

;; ── Basic movement ──────────────────────────────────────────────
(deftest test-basic-move
  (let [graph  sample-graph
        agents [{:name "Scout" :location :village}]
        world  (make-world graph agents)
        result (move-agent world (first agents) :hut)]
    (is (= :hut (:location (first (:agents result)))))))

(deftest test-move-blocked-by-direction
  (let [graph  sample-graph
        agents [{:name "Scout" :location :village}]
        world  (make-world graph agents)
        result (move-agent world (first agents) :forest)]
    (is (= :village (:location (first (:agents result)))))
    (is (empty? (:pending-events result)))))

(deftest test-move-nonexistent-node
  (let [graph  sample-graph
        agents [{:name "Scout" :location :village}]
        world  (make-world graph agents)
        result (move-agent world (first agents) :mars)]
    (is (= :village (:location (first (:agents result)))))))

;; ── Departure witnesses ─────────────────────────────────────────
(deftest test-departure-witnesses
  (let [graph  sample-graph
        agents [{:name "Scout"   :location :village}
                {:name "Lookout" :location :village}
                {:name "Hermit"  :location :hut}]
        world  (make-world graph agents)
        result (move-agent world (first agents) :hut)
        events (:pending-events result)]
    (is (= 1 (count (filter #(= :agent-departed (:event-type %)) events))))
    (let [ev (first (filter #(= :agent-departed (:event-type %)) events))]
      (is (= "Lookout" (:name (:observer ev))))
      (is (= "Scout"   (:name (:source ev)))))))

;; ── Travel hazard ───────────────────────────────────────────────
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

(deftest test-move-with-hazard
  (let [graph  {:nodes {:v {:label "V"} :w {:label "W"}}
                :edges [[:v :w {:distance 1 :risk 1.0}]]}
        agents [{:name "Scout" :location :v}]
        world  (make-world graph agents)]
    (with-redefs [rand (constantly 0.0)]
      (let [result (move-agent world (first agents) :w)
            events (:pending-events result)]
        (is (= :w (:location (first (:agents result)))))
        (is (= 1 (count (filter #(= :travel-hazard (:event-type %)) events))))))))

;; ── Portal transition ───────────────────────────────────────────
(deftest test-portal-transition
  (let [graph-a (load-graph "test-transition-a.json")
        agents [{:name "Scout"   :location :a1}
                {:name "Lookout" :location :a1}]
        world  (make-world graph-a agents)
        result (move-agent world (first agents) :a2)
        events (:pending-events result)]
    (is (= :b1 (:location (first (:agents result)))))
    (is (= #{:b2 :b1} (set (node-names (:graph result)))))
    (is (= 1 (count (filter #(= :agent-departed (:event-type %)) events))))
    (let [ev (first (filter #(= :agent-departed (:event-type %)) events))]
      (is (= "Lookout" (:name (:observer ev))))
      (is (= "Scout"   (:name (:source ev)))))))

;; ── Movement after portal transition ────────────────────────────
(deftest test-move-after-transition
  (let [graph-a (load-graph "test-transition-a.json")
        agents [{:name "Scout" :location :a1}]
        world  (make-world graph-a agents)

        ;; Step 1: Move from A1 to A2, triggering portal to B1
        result1 (move-agent world (first agents) :a2)

        ;; Step 2: Extract the UPDATED agent from result1. 
        ;; This agent now has :location :b1.
        updated-scout (first (:agents result1))

        ;; Move the updated agent within the new graph from B1 to B2
        result2 (move-agent result1 updated-scout :b2)]

    ;; Assertions for Step 1 (The Portal Jump)
    (is (= :b1 (:location updated-scout))
        "After result1, Scout should be at the entry node :b1")
    (is (= #{:b1 :b2} (set (node-names (:graph result1))))
        "The world graph should have swapped to Graph B")

    ;; Assertions for Step 2 (Movement within the new graph)
    (is (= :b2 (:location (first (:agents result2))))
        "Scout should successfully move to :b2 in the new graph")
    (is (= #{:b1 :b2} (set (node-names (:graph result2))))
        "The graph should remain Graph B")

    ;; Event Assertions for Step 2
    (let [events (:pending-events result2)
          dep    (first (filter #(= :agent-departed (:event-type %)) events))]
      (is (= 1 (count (filter #(= :agent-departed (:event-type %)) events)))
          "A departure event should be recorded for the move from b1 to b2")
      (is (= "Scout" (:name (:source dep)))
          "The source of the departure should be Scout")
      (is (nil? (:observer dep))
          "Since Scout was alone at :b1, there should be no observer recorded"))))