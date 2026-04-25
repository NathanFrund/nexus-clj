(ns nexus-clj.demo
  (:require [nexus-clj.core :as nx]))

(defn -main [& args]
  (println "=== NEXUS VILLAGE DEMONSTRATION ===")
  (println)

  ;; Load the world (flattened hypergraph)
  (let [world (nx/load-world "village.json")
        world (assoc world :agents [{:name "Elder" :location :elderHut}
                                    {:name "Thug"  :location :village}])
        world-atom (atom world)]

    (println "🗺️  World loaded:" (count (nx/node-names world)) "nodes.")
    (println)

    (println "👥 Agents in the world: Elder, Thug")
    (println)

    ;; Presence queries
    (println "📍 Who is at the village?")
    (println "  " (map :name (nx/agents-at-node @world-atom :village)))
    (println "📍 Who is at the Elder's Hut?")
    (println "  " (map :name (nx/agents-at-node @world-atom :elderHut)))
    (println)

    ;; Neighbour check
    (println "🔗 From the village, you can reach:")
    (println "  " (seq (nx/neighbors-of @world-atom :village)))
    (println "🔗 From the Elder's Hut, you can reach:")
    (println "  " (seq (nx/neighbors-of @world-atom :elderHut)))
    (println)

    ;; Movement attempt
    (println "🚶 Attempting to move Thug from village to Elder's Hut...")
    (let [thug (first (filter #(= (:name %) "Thug") (:agents @world-atom)))
          new-world (nx/move-agent @world-atom thug :elderHut)]
      (if (= (:location (first (filter #(= (:name %) "Thug") (:agents new-world)))) :elderHut)
        (do
          (reset! world-atom new-world)
          (println "  Success! Thug is now at" (:location (first (filter #(= (:name %) "Thug") (:agents @world-atom))))))
        (println "  Failed (no path or one‑way restriction).")))
    (println "  Current locations: Elder=" (:location (first (filter #(= (:name %) "Elder") (:agents @world-atom))))
             ", Thug=" (:location (first (filter #(= (:name %) "Thug") (:agents @world-atom)))))
    (println)

    ;; Path hazard display (risk on edge village→forest)
    (let [edge (->> (nx/edges-from @world-atom :village)
                    (filter (fn [[a b _]] (or (and (= a :village) (= b :forest))
                                              (and (= a :forest) (= b :village)))))
                    first)
          risk (get (nth edge 2) :risk "none")]   ;; edge = [from to attrs]
      (println "⚠️  Risk on edge village→forest:" risk))
    (println)

    ;; Witnessed events
    (println "👁️  If a heroic act occurs at the Elder's Hut, these agents witness it:")
    (let [elder (first (filter #(= (:name %) "Elder") (:agents @world-atom)))
          witnesses (nx/witnessed-events @world-atom :heroic-act :elderHut elder)]
      (doseq [w witnesses]
        (println "  " (:name (:observer w)) "sees" (:event-type w))))
    (println "  (Excluding the source agent: Elder)")
    (println)
    (println "=== END OF DEMONSTRATION ===")))

(when (= (first *command-line-args*) "run")
  (-main))