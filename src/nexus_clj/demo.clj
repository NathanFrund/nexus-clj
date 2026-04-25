(ns nexus-clj.demo
  (:require [nexus-clj.core :as nx]))

(defn -main [& args]
  (println "=== NEXUS VILLAGE DEMONSTRATION ===")
  (println)

  ;; Load the world (flattened hypergraph)
  (let [world (nx/load-world "village.json")

        ;; Create agents with stable :id and keyword :location
        elder {:id :elder :name "Elder" :location :elderHut}
        thug  {:id :thug  :name "Thug"  :location :village}

        world (assoc world :agents [elder thug])
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

    ;; Movement attempt – let Nexus do the move, then check the result
    (println "🚶 Attempting to move Thug from village to Elder's Hut...")
    (let [new-world (nx/move-agent @world-atom thug :elderHut)]
      (if (= (:location (first (filter #(= (:id %) :thug) (:agents new-world)))) :elderHut)
        (do
          (reset! world-atom new-world)
          (println "  Success! Thug is now at" (:location (first (filter #(= (:id %) :thug) (:agents @world-atom))))))
        (println "  Failed (no path or one‑way restriction).")))
    (println "  Current locations: Elder=" (:location (first (filter #(= (:id %) :elder) (:agents @world-atom))))
             ", Thug=" (:location (first (filter #(= (:id %) :thug) (:agents @world-atom)))))
    (println)

    ;; Path hazard display (risk on edge village→forest)
    (let [edge (->> (nx/edges-from @world-atom :village)
                    (filter #(= (:to %) :forest))
                    first)
          risk (:risk edge "none")]
      (println "⚠️  Risk on edge village→forest:" risk))
    (println)

    ;; Witnessed events
    (println "👁️  If a heroic act occurs at the Elder's Hut, these agents witness it:")
    (let [witnesses (nx/witnessed-events @world-atom :heroic-act :elderHut elder)]
      (doseq [w witnesses]
        (if (:observer w)
          (println "  " (:name (:observer w)) "sees" (:event-type w))
          (println "  (no witnesses)"))))
    (println "  (Excluding the source agent: Elder)")
    (println)
    (println "=== END OF DEMONSTRATION ===")))

(when (= (first *command-line-args*) "run")
  (-main))