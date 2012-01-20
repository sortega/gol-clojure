(ns gol.test.gui
  (:use [gol.gui])
  (:use [clojure.test]))

(deftest world-ranges-test
  (is (= [[0 0] [0 0]] (world-ranges #{})))
  (is (= [[-20 20] [-20 20]] (world-ranges #{[-20 -20] [20 20]}))))

(deftest grid-ranges-test
  (is (= [[-10 10] [-10 10]] (grid-ranges [[-5 5] [-1 1]])))
  (is (= [[-20 10] [-10 20]] (grid-ranges [[-20 5] [-1 20]]))))

(deftest grid-test
  (let [g (grid 210 210 [[-10 10] [-10 10]])]
    (is (= 10 (cellSize g)))
    (is (= [100 100 10 10] (cellPos g 0  0)))
    (is (= [ 90 110 10 10] (cellPos g 1 -1)))
    (is (= [0  0] (cellOn g 100 100)))
    (is (= [0  0] (cellOn g 103 103)))
    (is (= [1 -1] (cellOn g  98 110)))))