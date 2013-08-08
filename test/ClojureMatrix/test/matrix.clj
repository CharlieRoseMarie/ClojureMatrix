(ns ClojureMatrix.test.matrix
  (:use [ClojureMatrix.matrix])
  (:use [clojure.test]))

(def sample-matrix [[1 2 3] [4 5 6] [7 8 9]])
(def sample-matrix2 [[10 15 20] [11 9 2]])

(deftest identity-test
  (is (= [[1 0] [0 1]] (create-identity 2)))
  (is (= [[1 0 0] [0 1 0] [0 0 1]])))

(deftest get-row-test
	(is (= [1 2 3] (get-row sample-matrix 0)))
	(is (= [7 8 9] (get-row sample-matrix 2))))
;  (is (thrown? AssertionError (get-row sample-matrix 5)))
;  (is (thrown? AssertionError (get-row [[1] [1 2]] 0))))

(deftest get-column-test
	(is (= [1 4 7] (get-column sample-matrix 0))))

(deftest get-element-test
	(is (= 1 (get-element sample-matrix 0 0)))
  (is (= 2 (get-element sample-matrix 0 1))))

;(deftest determinant-test
	;(is (= 1 (determinant (create-identity 2))))
	;(is (= 0 (determinant [[2 4] [1 2]]))))

(deftest transpose-test
  (is (= [[1 4 7] [2 5 8] [3 6 9]] (transpose sample-matrix)))
  (is (= sample-matrix (transpose (transpose sample-matrix)))))

(deftest square-test
	(is (square? sample-matrix))
	(is (not (square? (conj sample-matrix [1 1 1])))))

(deftest ref-test
  (is (= true (ref? [[1 1 1] [0 1 1] [0 0 1]])))
  (is (= true (ref? [[1 1 1 1] [0 0 0 1]])))
  (is (= false (ref? sample-matrix)))
  (is (= true (ref? [[1 1 1] [0 1 1] [0 0 0] [0 0 0]]))))

(deftest multiplerow-test
  (is (= [[2 4 6] [4 5 6] [7 8 9]] (multiply-row sample-matrix 0 2)))
  (is (= [[1 2 3] [-4 -5 -6] [7 8 9]] (multiply-row sample-matrix 1 -1)))
  (is (= [[1 2 3] [4 5 6] [0 0 0]] (multiply-row sample-matrix 2 0))))


(deftest swap-rows-test
  (is (= [[4 5 6] [1 2 3] [7 8 9]] (swap-rows sample-matrix 0 1)))
  (is (= [[11 9 2] [10 15 20]] (swap-rows sample-matrix2 1 0))))