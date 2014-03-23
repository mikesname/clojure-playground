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

(defn list-elements [element-type]
  (d/q '[:find ?v ?uuid
         :in $ ?t
         :where [?v :graph.element/type ?t]
                [?v :graph.element/id ?uuid]] dbval element-type))

(defn list-vertices []
  (list-elements :graph.element.type/vertex))

(defn list-edges []
  (list-elements :graph.element.type/edge))

(defn get-out-edges [v labels]
  (d/q '[:find ?edge ?uuid
         :in $ ?id [?label ...]
         :where [?edge :graph.edge/outVertex ?id]
                [?edge :graph.element/id ?uuid ]
                [ ?edge :graph.edge/label ?label ]] dbval v labels))

(defn get-in-edges [v labels]
  (d/q '[:find ?edge ?uuid
         :in $ ?id [?label ...]
         :where [?edge :graph.edge/inVertex ?id]
                [?edge :graph.element/id ?uuid ]
                [ ?edge :graph.edge/label ?label ]] dbval v labels))

;; NB: Fails if there are no labels!
(defn get-edges [v direction labels]
  (case direction
    :in (get-in-edges v labels)
    :out (get-out-edges v labels)
    :both (d/q '[:find ?e 
                 :in $ ?v [?d ...] [?label ...]
                 :where [?e ?d ?v]
                        [?e graph.edge/label ?label]]
               dbval v [:graph.edge/outVertex :graph.edge/inVertex] labels)))

;; The elusive get-vertices function. This is complicated in the
;; BOTH direction because:
;;  a) 
(defn get-vertices [v direction labels]
  (case direction
    ;;:in (get-in-edges v labels)
    ;;:out (get-out-edges v labels)
    :both (d/q '[:find ?ov 
                 :in $ ?v [?out ?in] [?label ...]
                 :where [?e ?out ?v]
                        [?e ?in ?ov] ;; this is WRONG!
                        [?e graph.edge/label ?label]]
               dbval
               v
               (:graph.edge/outVertex :graph.edge/inVertex)
               labels)))

(defn id-from-uuid [uuid]
  (d/q '[:find ?v 
         :in $ ?uuid 
         :where [?v :graph.element/id ?uuid]] dbval uuid))

(def marko
  (ffirst (id-from-uuid #uuid "550e8400-e29b-41d4-a716-446655440000")))

(def stephen
  (ffirst (id-from-uuid #uuid "550e8400-e29b-41d4-a716-446655440001")))

(def knowsEdge
  (ffirst (id-from-uuid #uuid "550e8400-e29b-41d4-a716-446655440002")))

