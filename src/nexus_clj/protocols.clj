(ns nexus-clj.protocols
  "Protocols for dual‑tier movement.")

(defprotocol Positionable
  (get-location [this])
  (set-location [this world new-node]))