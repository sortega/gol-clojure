(ns gol.test.model
  (:use [gol.model])
  (:use [clojure.test]))

(deftest neighbours-test
  (is (= [[ 9 9] [ 9 10] [ 9 11]
          [10 9]         [10 11]
          [11 9] [11 10] [11 11]]
        (neighbours [10 10]))))

(deftest next-gen-test
  (is (= #{} (next-gen #{})))
  (is (= #{} (next-gen #{[0 0]})))
  (is (= #{[0 0] [1 0] [-1 0]} (next-gen #{[0 0] [0 1] [0 -1]}))))

(deftest gen-seq-test
  (is (= #{[0 0] [1 0] [-1 0]}
        (nth (gen-seq #{[0 0] [0 1] [0 -1]}) 201))))

(deftest toggle-test
  (is (= #{} (toggle-cell #{[0 0]} [0 0])))
  (is (= #{[0 0]} (toggle-cell #{} [0 0]))))
