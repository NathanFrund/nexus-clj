(ns nexus-clj.demo
  (:require [nexus-clj.core :as nx]))

(defn -main [& args]
  (println "=== NEXUS VILLAGE DEMONSTRATION ===")
  (println)

  ;; Load the map
  (let [graph (nx/load-graph "village.json")]
    (println "🗺️  Map loaded:" (count (nx/node-names graph)) "nodes.")
    (println)

    ;; Create agents
    (let [elder {:name "Elder" :location "elderHut"}
          thug  {:name "Thug"  :location "village"}
          world {:graph graph
                 :agents [elder thug]}
          world-atom (atom world)]   ;; atom allows mutable updates for movement demo

      (println "👥 Agents in the world: Elder, Thug")
      (println)

      ;; Presence queries
      (println "📍 Who is at the village?")
      (println "  " (map :name (nx/agents-at-node @world-atom "village")))
      (println "📍 Who is at the Elder's Hut?")
      (println "  " (map :name (nx/agents-at-node @world-atom "elderHut")))
      (println)

      ;; Neighbour check
      (println "🔗 From the village, you can reach:")
      (println "  " (seq (nx/neighbors-of graph "village")))
      (println "🔗 From the Elder's Hut, you can reach:")
      (println "  " (seq (nx/neighbors-of graph "elderHut")))
      (println)

      ;; Movement attempt
      (println "🚶 Attempting to move Thug from village to Elder's Hut...")
      (let [new-world (nx/move-agent @world-atom thug "elderHut")]
        (if (= (:location (some #(when (= (:name %) "Thug") %) (:agents new-world))) "elderHut")
          (do
            (reset! world-atom new-world)
            (println "  Success! Thug is now at" (:location (first (filter #(= (:name %) "Thug") (:agents @world-atom))))))
          (println "  Failed (no path or one‑way restriction).")))
      (println "  Current locations: Elder=" (:location (first (filter #(= (:name %) "Elder") (:agents @world-atom))))
               ", Thug=" (:location (first (filter #(= (:name %) "Thug") (:agents @world-atom)))))
      (println)

      ;; Path hazard display (risk on edge village→forest)
      (let [edge (->> (nx/edges-from graph "village")
                      (filter (fn [[a b _]] (or (and (= a "village") (= b "forest"))
                                                (and (= a "forest") (= b "village")))))
                      first)          ; <-- now first is applied after filter
            risk (get (last edge) :risk "none")]
        (println "⚠️  Risk on edge village→forest:" risk))
      (println)

      ;; Witnessed events
      (println "👁️  If a heroic act occurs at the Elder's Hut, these agents witness it:")
      (let [witnesses (nx/witnessed-events @world-atom :heroic-act "elderHut" elder)]
        (doseq [w witnesses]
          (println "  " (:name (:observer w)) "sees" (:event-type w))))
      (println "  (Excluding the source agent: Elder)")
      (println)
      (println "=== END OF DEMONSTRATION ==="))))

;; Helper to make the demo runnable as a script
(when (= (first *command-line-args*) "run")
  (-main))