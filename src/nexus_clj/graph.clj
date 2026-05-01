(ns nexus-clj.graph
  "Property graph helpers (namespaced keywords) and serialization.")

;; ----------------------------------------------------------------------
;; Property Graph Helpers (namespaced keywords)
;; ----------------------------------------------------------------------

(def ^:const reserved-ns "nexus")

(defn reserved-key?
  "Returns true if k is a namespaced keyword in the 'nexus' namespace."
  [k]
  (= reserved-ns (namespace k)))

(defn validate-property-key!
  "Throws if a user tries to use a key in the reserved 'nexus' namespace."
  [k]
  (when (reserved-key? k)
    (throw (ex-info (str "Property key must not use namespace " reserved-ns)
                    {:key k}))))

;; Nodes and edges are plain maps.  Structural keys use the :nexus/
;; namespace.  User metadata can be any other keyword.

(defn make-node
  "Create a node with a required id and optional label/properties."
  [id & {:keys [label] :as props}]
  (merge {:nexus/id id :nexus/label (or label id)} props))

(defn make-edge
  "Create an edge between two nodes.  Extra keys become properties."
  [from to & {:keys [distance risk direction] :or {distance 1 risk 0.0 direction :both} :as props}]
  (merge {:nexus/from from :nexus/to to :nexus/distance distance :nexus/risk risk :nexus/direction direction}
         (dissoc props :distance :risk :direction)))

;; Serialisation helpers (maps are already the portable format)
(defn node->property-dict [node] node)
(defn edge->property-dict [edge] edge)
(defn property-dict->node [d] d)
(defn property-dict->edge [d] d)