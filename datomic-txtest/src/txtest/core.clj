(ns txtest.core
  (:gen-class))

(use '[datomic.api :only [q db] :as d])

(def uri "datomic:mem://graphtest")

;; create database
(d/create-database uri)

;; connect to database
(def conn (d/connect uri))

;; parse schema dtm file
(def schema-tx (read-string (slurp "db/graph-schema.dtm")))

;; submit schema transaction
@(d/transact conn schema-tx)

;; parse data dtm file
(def data-tx (read-string (slurp "db/graph.dtm")))

;; submit data transaction
@(d/transact conn data-tx)

