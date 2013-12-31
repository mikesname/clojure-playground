(ns lsq.dir
  (:gen-class))

(use '[clojure.string :only (join)])
(use '[clojure.java.io :only (file)])

(use ['lsq.fseq])

(defrecord Dir [fseqs dirs junk])

(defn add-dir [dir value]
  (update-in dir [:dirs] #(into [] (conj % value))))

(defn add-junk [dir value]
  (update-in dir [:junk] #(into [] (conj % value))))

(defn add-fseq [dir value]
  (update-in dir [:fseqs] #(into [] (conj % value))))

(defn directory [dirname] (clojure.java.io/file dirname))

(defn sorted-file-list [dirname]
  (let [all (.listFiles (directory dirname))]
    (sort-by #(.getName %) all)))

(defn- parse-dir-raw [dirname]
  (let [test-fn (fn [dir item]
                  (let [item-name (.getName item)]
                    ; if it's not a directory it must be either
                    ; a sequence name or something we don't care about
                    (if (.isDirectory item)
                      (add-dir dir item-name)
                      (let [try-parse (parse-filename item-name)]
                        (if (nil? try-parse)
                          (add-junk dir item-name)
                          (add-fseq dir try-parse))))))
        files (sorted-file-list dirname)
        initial (->Dir [] [] [])]
    (reduce test-fn initial files))) 

(defn- condense-dir [dir]
  (let [fseqs (:fseqs dir)]
    (let [pseqs (separate-and-combine fseqs)]
      (assoc-in dir [:fseqs] pseqs))))

(defn parse-dir [dirname]
  (let [raw (parse-dir-raw dirname)]
    (condense-dir raw)))

(defn do-overwrite-check
  "check files won't overwrite existing files."
  [dirname in out offset]
  (let [same-seq (same-fseq? in out)]
    (when (not same-seq) (or (and same-seq (zero? offset)))
      (doseq [f (get-filepaths dirname out)]
        (if (.exists (file f))
          (throw (Exception. (str "File would be overwritten!: " f))))))))

(defn- rename-filepairs [filepairs reverse?]
  (let [rename-order (if reverse? (reverse filepairs) filepairs)]
    (doseq [[pin pout] rename-order]
      (let [fin (file pin) fout (file pout)]
        (if (.exists fout)
          (throw (Exception. (str "File exists: " (.getPath fout)))))
        (.renameTo fin fout)))))

(defn- do-rename
  "rename a sequence of files"
  [dirname fseq-in fseq-out from to offset]
  (let [sin (truncate-idx fseq-in from to)
        sout (idx-from fseq-in fseq-out from to offset)]
    (if (= sin sout)
      (println "Nothing to do!") 
      (let [filepairs (map vector (get-filepaths dirname sin) (get-filepaths dirname sout))]
        (do-overwrite-check dirname sin sout offset)
        (rename-filepairs filepairs (> offset 0))))))

(defn- calc-offset [fseq offset start]
  "Calculate the offset given a (possibly nil) start value."
  (if (nil? start)
    (if-not (nil? offset) offset 0)
    (- start (first-idx fseq))))

(defn rename-fseq [dirname pattern-in pattern-out from to offset start]
  (let [dir (parse-dir dirname)]
    (let [fseq (fseq-for-pattern dir pattern-in)]
      (if (nil? fseq)
        (println "Nothing to do!")
        (let [
              from (if (nil? from) (first-idx fseq) from)
              to (if (nil? to) (last-idx fseq) to)
              off (calc-offset fseq offset start)]
          (do-rename dirname fseq (parse-pattern pattern-out) from to off))))))

(defn lsq-dir [dirname]
  (let [dir (parse-dir dirname)]
    (doseq [fseq (:fseqs dir)]
      (println (show-fseq fseq)))
    (doseq [dirname (:dirs dir)]
      (println dirname))
    (doseq [junkname (:junk dir)]
      (println junkname))))

