(ns lsq.core
  (require [clojure.string :as string]
           [lsq.dir :refer :all]
           [lsq.fseq :refer :all]
           [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))


(def cli-options
  [
  ["-l" "--list" "List sequences in directory" :default false]
  ["-d" "--directory DIR" "The operational directory."
   :default "."
   :validate [#(.isDirectory (java.io.File. %)) "Directory does not exist"]]
  [nil "--start START" "Rename sequence to start from index."
   :parse-fn #(Integer/parseInt %) :default nil]
  [nil "--offset OFFSET" "Offset input sequence to by the given number."
   :parse-fn #(Integer/parseInt %) :default 0]
  ["-f" "--from FROM" "Rename from index."
   :parse-fn #(Integer/parseInt %) :default nil]
  ["-t" "--to TO" "Rename to (and including) index."
   :parse-fn #(Integer/parseInt %) :default nil]
  ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Rename a Shake-style file sequence."
        ""
        "Usage: lsqrn [options] <in-pattern> <out-pattern>"
        ""
        "Options:"
        options-summary
        ""]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main
  "Group and show sequences in a directory."
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    ;; Handle help and error conditions
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors))
      (:list options) (lsq-dir (:directory options))
      (not= (count arguments) 2) (exit 1 (usage summary))
      :else (rename-fseq
              (:directory options)
              (nth arguments 0)
              (nth arguments 1)
              (:from options)
              (:to options)
              (:offset options)
              (:start options)))))


