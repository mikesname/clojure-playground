(ns lsq.core-test
  (:use clojure.test
        lsq.core
        lsq.fseq
        lsq.dir))

(deftest lsq-test
  (testing "Subsequence extraction"
    (is (= [[1 2] [4 5]] (extract-subseqs #(= %2 (inc %1)) [1 2 4 5])))
    (is (= [[2 1] [-1 -2]] (extract-subseqs #(= %1 (inc %2)) [2 1 -1 -2]))))

  (testing "pad parsing"
    (let [ok (->Fseq "pre." 4 ".suf" [])]
      (is (= ok (parse-pattern "pre.#.suf")))
      (is (= ok (parse-pattern "pre.@@@@.suf")))))

  (testing "pad parsing"
    (is (= 4 (parse-pad "#")))
    (is (= 3 (parse-pad "###")))
    (is (= 2 (parse-pad "@@")))))

