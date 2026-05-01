(ns nexus-clj.core
  "Nexus — a hook‑driven spatial graph engine.
   Port of the Pharo Nexus Pivot to Clojure."
  (:require [nexus-clj.graph :as g]
            [nexus-clj.world :as w]
            [nexus-clj.plugins :as p]
            [nexus-clj.movement :as m]
            [nexus-clj.protocols :as pr]))

;; Re‑export the full public API
(def make-node            g/make-node)
(def make-edge            g/make-edge)
(def reserved-key?        g/reserved-key?)
(def validate-property-key! g/validate-property-key!)
(def node->property-dict  g/node->property-dict)
(def edge->property-dict  g/edge->property-dict)
(def property-dict->node  g/property-dict->node)
(def property-dict->edge  g/property-dict->edge)

(def load-world           w/load-world)
(def node-names           w/node-names)
(def edges-from           w/edges-from)
(def direction-allows?    w/direction-allows?)
(def neighbors-of         w/neighbors-of)
(def agents-at-node       w/agents-at-node)
(def witnessed-events     w/witnessed-events)
(def world-summary        w/world-summary)

(def register-plugin      p/register-plugin)
(def run-hooks            p/run-hooks)

(def make-context         m/make-context)
(def move-agent           m/move-agent)
(def make-simple-agent    m/make-simple-agent)

;; Protocols are re‑exported as vars (they are first‑class in Clojure)
(def Positionable         pr/Positionable)
(def get-location         pr/get-location)
(def set-location         pr/set-location)

;; ... retain the (comment ...) block if you like