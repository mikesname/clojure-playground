(ns lsq.fseq
  (:gen-class))

(use '[clojure.string :only (join)])
(use '[clojure.java.io :only (file)])

(def match-fseq #"^(.*?)(-?\d+)(\D*)$")
(def match-pattern #"^(.*?)([@#]+)(\D*)$")

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defrecord Fseq [pre pad suf idx])

(defn same-fseq?
  "check if two sequences are the same"
  [a b]
  (and
    (.equals (:pre a) (:pre b))
    (.equals (:suf a) (:suf b))))

(defn combine-fseq [a b]
  (->Fseq
      (:pre a) (:pad a) (:suf a) (into [] (concat (:idx a) (:idx b)))))

(defn combine-fseqs
  "combine the indices of several sequences"
  [& seqs]
  (reduce combine-fseq seqs))

(defn extract-subseqs
  "separate an ordered collection comprising multiple subsequences
  into a set of subsequences based on a function that determines if
  two ajacent values belong in the same sub sequence."
  [belong-fn coll]
  (loop [sets [] coll coll]
    ; terminal condition
    (if (empty? coll)
      sets
      (if (empty? sets)
        (recur [[(first coll)]] (rest coll))
        (let [last-item (last (last sets)) next-item (first coll)]
          (if (belong-fn last-item next-item)
            (recur (conj (into [] (butlast sets)) (into [] (conj (last sets) next-item))) (rest coll))
            (recur (into [] (conj sets [next-item])) (rest coll))))))))

(defn separate-fseqs [fseqs] (extract-subseqs #(same-fseq? %1 %2) fseqs)) 

(defn separate-and-combine [fseqs]
  (map #(apply combine-fseqs %) (separate-fseqs fseqs)))

(defn get-pad [pad]
  (case pad
    4 "#"
    (apply str (repeat pad "@"))))

(defn parse-pad [pattern]
  (cond
    (= pattern, "#") 4
    (re-find #"^[@#]+$" pattern) (count pattern)
    :else (throw (Exception. (str "bad pattern: " pattern)))))

(defn get-range [arr]
  (case (count arr)
    0 -1
    1 (first arr)
    (str (first arr) "-" (last arr))))

(defn get-subseqs [arr] 
  (extract-subseqs #(= (inc %1) %2) arr))

(defn get-ranges [idx]
  (let [subseqs (get-subseqs idx)]
    (join "," (map get-range subseqs))))

(defn show-fseq [s]
  (format "%s%s%s%s" (:pre s) (get-ranges (:idx s)) (get-pad (:pad s)) (:suf s)))

(defn parse-filename
  "get a single seq from a file, or nil if it doesn't match"
  [filename]
  (let [match (re-find match-fseq filename)]
    (if-not (nil? match)
      (->Fseq
        (nth match 1)
        (count (nth match 2))
        (nth match 3)
        (vector (Integer/parseInt (nth match 2)))))))

(defn parse-pattern
  "get an empty sequence from a sequence pattern, or nil"
  [pattern]
  (let [match (re-find match-pattern pattern)]
    (if-not (nil? match)
      (->Fseq (nth match 1) (parse-pad (nth match 2)) (nth match 3) []))))

(defn first-idx [fseq]
  (apply min (:idx fseq)))

(defn last-idx [fseq]
  (apply max (:idx fseq)))

(defn make-filename [fseq idx]
  (let [fmt (str (:pre fseq) "%0" (:pad fseq) "d" (:suf fseq))]
    (format fmt idx)))

(defn make-filepath [path fseq idx]
  (let [fname (make-filename fseq idx)]
    (.getPath (file path fname))))

(defn get-filenames [fseq]
  (map (partial make-filename fseq) (:idx fseq)))

(defn get-filepaths [dirname fseq]
  (map (partial make-filepath dirname fseq) (:idx fseq)))

(defn fseq-for-pattern 
  "determine whether a fseq is contained in a parsed dir"
  [dir pattern]
  (first (filter (partial same-fseq? (parse-pattern pattern)) (:fseqs dir))))

(defn truncate-idx [fseq from to]
  (let [new-idx 
        (take-while #(<= % to) (drop-while #(< % from) (:idx fseq)))]
    (assoc-in fseq [:idx] new-idx)))

(defn offset-idx [fseq offset]
  (assoc-in fseq [:idx] (map #(+ offset %) (:idx fseq))))

(defn idx-from [in out from to offset]
  (let [new-fseq (offset-idx (truncate-idx in from to) offset)]
    (assoc-in out [:idx] (:idx new-fseq))))

