(ns txtest.core
  (:gen-class))

(require '[datomic.api :as d])

(def uri "datomic:mem://graphtest")

;; create database
(d/create-database uri)

;; connect to database
(def conn (d/connect uri))

;; parse schema dtm file
(def schema-tx (read-string (slurp "db/graph-schema.edn")))

;; submit schema transaction
@(d/transact conn schema-tx)

;; parse property definition file
(def property-schema-tx (read-string (slurp "db/property-schema.edn")))

;; submit data transaction
@(d/transact conn property-schema-tx)

;; parse data dtm file
(def data-tx (read-string (slurp "db/graph.edn")))

;; submit data transaction
@(d/transact conn data-tx)

(def dbval (d/db conn))

(defn list-elements [db element-type]
  (d/q '[:find ?v ?uuid
         :in $ ?t
         :where [?v :graph.element/type ?t]
                [?v :graph.element/id ?uuid]] db element-type))

(defn list-vertices [db]
  (list-elements db :graph.element.type/vertex))

(defn list-edges [db]
  (list-elements db :graph.element.type/edge))

(defn get-out-edges [db v labels]
  (d/q '[:find ?edge ?uuid
         :in $ ?id [?label ...]
         :where [?edge :graph.edge/outVertex ?id]
                [?edge :graph.element/id ?uuid ]
                [ ?edge :graph.edge/label ?label ]] db v labels))

(defn get-in-edges [db v labels]
  (d/q '[:find ?edge ?uuid
         :in $ ?id [?label ...]
         :where [?edge :graph.edge/inVertex ?id]
                [?edge :graph.element/id ?uuid ]
                [ ?edge :graph.edge/label ?label ]] db v labels))

;; NB: Fails if there are no labels!
(defn get-edges [db v direction labels]
  (case direction
    :in (get-in-edges db v labels)
    :out (get-out-edges db v labels)
    :both (d/q '[:find ?e 
                 :in $ ?v [?d ...] [?label ...]
                 :where [?e ?d ?v]
                        [?e graph.edge/label ?label]]
               db v [:graph.edge/outVertex :graph.edge/inVertex] labels)))

;; The elusive get-vertices function. This is complicated in the
;; BOTH direction because:
;;  a) 
(defn get-vertices [db v direction labels]
  (case direction
    :in (d/q '[:find ?other
               :in $ ?v [?label ...]
               :where [?e :graph.edge/outVertex ?other]
                      [?e :graph.edge/inVertex ?v]
                      [?e :graph.edge/label ?label]] db v labels)
    :out (d/q '[:find ?other
               :in $ ?v [?label ...]
               :where [?e :graph.edge/inVertex ?other]
                      [?e :graph.edge/outVertex ?v]
                      [?e :graph.edge/label ?label]] db v labels)
    :both (concat 
            (into [] (get-vertices db v :out labels))
            (into [] (get-vertices db v :in labels)))))

(defn add-vertex [uuid]
  (let [tid (d/tempid :graph)]
    [{:db/id tid
     :graph.element/id uuid
     :graph.element/type :graph.element.type/edge}]))

(defn get-property [db element property-type]
  (property-type (d/entity db element)))

(defn add-property [element property-type value]
  [{:db/id element property-type value}])

(defn add-edge [uuid out-vertex in-vertex label]
  (let [tid (d/tempid :graph)]
    [{:db/id tid
     :graph.element/id uuid
     :graph.element/type :graph.element.type/edge
     :graph.edge/outVertex out-vertex
     :graph.edge/inVertex in-vertex
     :graph.edge/label label}]))


(defn id-from-uuid [db uuid]
  (d/q '[:find ?v 
         :in $ ?uuid 
         :where [?v :graph.element/id ?uuid]] db uuid))

(def marko
  (ffirst (id-from-uuid dbval #uuid "550e8400-e29b-41d4-a716-446655440000")))

(def stephen
  (ffirst (id-from-uuid dbval #uuid "550e8400-e29b-41d4-a716-446655440001")))

(def knowsEdge
  (ffirst (id-from-uuid dbval #uuid "550e8400-e29b-41d4-a716-446655440002")))

