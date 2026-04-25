(ns nexus-clj.core-test
  (:require [clojure.test :refer [deftest is]]
            [nexus-clj.core :refer [node-names neighbors-of agents-at-node witnessed-events]]))

(def sample-graph
  {:nodes {"village"  {:label "Village Square" :terrain "open"}
           "hut"      {:label "Elder's Hut"    :terrain "building"}
           "forest"   {:label "Forest Path"    :terrain "woods"}}
   :edges [["village" "hut"      {:distance 1 :risk 0.0}]
           ["village" "forest"   {:distance 2 :risk 0.3 :direction "backward"}]]})

(deftest test-node-names
  (is (= #{"village" "hut" "forest"} (set (node-names sample-graph)))))

(deftest test-neighbors-of
  (is (= #{"hut" "forest"} (neighbors-of sample-graph "village")))
  (is (= #{"village"}       (neighbors-of sample-graph "hut"))))

(deftest test-one-way-neighbor
  (is (empty? (neighbors-of sample-graph "forest"))))

(deftest test-agents-at-node
  (let [world {:graph sample-graph
               :agents [{:name "Elder" :location "hut"}
                        {:name "Thug"  :location "village"}]}]
    (is (= 1 (count (agents-at-node world "village"))))
    (is (= 1 (count (agents-at-node world "hut"))))
    (is (= 0 (count (agents-at-node world "forest"))))))

(deftest test-witnessed-events
  (let [world {:graph sample-graph
               :agents [{:name "Elder" :location "hut"}
                        {:name "Thug"  :location "village"}]}
        events (witnessed-events world :heroic-act "village" (first (:agents world)))]
    (is (= 1 (count events)))
    (is (= "Thug" (:name (:observer (first events)))))))