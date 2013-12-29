(ns lsq.core
  (:gen-class))

(use '[lsq.dir])
(use '[lsq.fseq])


(defn -main
  "Group and show sequences in a directory."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (case args
    nil (lsq-dir ".")
    (doseq [dir args]
      (lsq-dir dir))))

