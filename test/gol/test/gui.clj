(ns gol.test.gui
  (:use [gol.gui])
  (:use [clojure.test]))

(deftest world-ranges-test
  (is (= [[-10 10] [-10 10]] (world-ranges #{})))
  (is (= [[-20 20] [-20 20]] (world-ranges #{[-20 -20] [20 20]}))))

(deftest index-of-test
  (is (= 0 (index-of 10 400 198)))
  (is (= 1 (index-of 10 400 211))))