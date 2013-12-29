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
  (loop [all (sorted-file-list dirname) dir (->Dir [] [] [])]
    (if (empty? all)
      dir
      (let [nextf (first all) others (rest all)]
        (let [namef (.getName nextf)]
        (if (.isDirectory nextf)
          (recur others (add-dir dir namef))
          (let [p (parse-filename namef)]
            (if (nil? p)
              (recur others (add-junk dir namef))
              (recur others (add-fseq dir p))))))))))

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
  (when (not same-seq) (or (and (same-seq (zero? offset))))
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

(defn rename-fseq [dirname pattern-in pattern-out from to offset]
    (let [dir (parse-dir dirname)]
      (let [fseq (fseq-for-pattern dir pattern-in)]
        (if (nil? fseq)
          (println "Nothing to do!")
          (let [
                from (if (nil? from) (first-idx fseq) from)
                to (if (nil? to) (last-idx fseq) to)]
            (do-rename dirname fseq (parse-pattern pattern-out) from to offset))))))

(defn lsq-dir [dirname]
  (let [dir (parse-dir dirname)]
    (doseq [fseq (:fseqs dir)]
      (println (show-fseq fseq)))
    (doseq [dirname (:dirs dir)]
      (println dirname))
    (doseq [junkname (:junk dir)]
      (println junkname))))

