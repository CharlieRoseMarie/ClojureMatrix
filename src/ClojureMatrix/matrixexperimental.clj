(ns ClojureMatrix.matrixexperimental
  (:require [ClojureMatrix.vector :as v]))

(defrecord Matrix [rows columns data])

(defprotocol MatrixOperations
  (valid-row?   [this row] "Returns true if the row is within the dimensions of the matrix")
  (valid-column [this col] "Returns true if the col is within the dimensions of the matrix")
  (get-element  [this row col] "Returns the element at row and col.")
  (scalar-multiply [this value] "Multiplies the matrix by some value"))

(defn valid-row? [matrix row]
  "Returns true if the row is within the dimensions of the matrix"
  (and (>= row 0) (< row (:rows matrix))))

(defn valid-column? [matrix col]
  "Returns true if the col is with the dimensions of the matrix"
  (and (>= col 0) (< col (:columns matrix))))

(defn matrix-from-nested [matrix]
  (let [proper (apply = (map count matrix))
        rows (count matrix)
        cols (count (first matrix))
        data (vec (apply concat matrix))]
    (when proper (Matrix. rows cols data))))

(defn matrix-to-nested [matrix]
  "Turns a matrix object into a nested vector"
  (vec (map vec (partition (:rows matrix) (:data matrix)))))

; Test values
(def m1 (Matrix. 3 3 [1 0 1, 0 1 2, 0 0 1]))
(def m2 (matrix-from-nested [[1 1 6 0][1 1 1 5][1 1 1 1]]))

(defn get-element [matrix row col]
  {:pre [(valid-row? matrix row) (valid-column? matrix col)]}
  (let [e (+ (* (:rows matrix) row) col)
        data (:data matrix)]
    (data e)))

(defn get-row [matrix row]
  {:pre [(valid-row? matrix row)]}
  (let [start (* (:columns matrix) row)
        end   (* (:columns matrix) (inc row))]
   (vec (subvec (:data matrix) start end))))

(defn get-column [matrix col]
  {:pre [(valid-column? matrix col)]}
  (let [adjusted-data (drop col (:data matrix))]
    (vec (take-nth (:columns matrix) adjusted-data))))

(defn equal-dimensions? [m1 m2]
  "Returns true if the dimensions of the two matrices are equal"
  (and (= (:rows m1) (:rows m2))
       (= (:columns m1) (:columns m2))))

(defn transpose [matrix]
  "Returns the transpose of the matrix"
  (let [new-row (:columns matrix)
        new-col (:rows matrix)
        new-data (apply concat(for [i (range new-row)] (get-column matrix i)))]
    (Matrix. new-row new-col (vec new-data))))

; Algebraic operations

(defn multiplicable? [m1 m2]
  "Returns nil if the two matrices can't be multiplied, otherwise returns the dimensions of the matrix that
  would result"
  (when (= (:rows m2) (:columns m1))
    [(:rows m1) (:columns m2)]))

(defn scalar-multiply [matrix value]
  (let [data (:data matrix)
        mult-data (map #(* % value) data)]
    (assoc matrix :data mult-data)))

(defn add
  ([] (Matrix. 0 0 []))
  ([m] m)
  ([m1 m2]
   (when (equal-dimensions? m1 m2)
     (Matrix. (:rows m1) (:columns m1) (vec (map + (:data m1) (:data m2))))))
  ([m1 m2 & args]
   (let [initial (add m1 m2)]
     (reduce add initial args))))

(defn subtract
  ([] (Matrix. 0 0 []))
  ([m] (scalar-multiply m -1))
  ([m1 m2]
   (when (equal-dimensions? m1 m2)
     (Matrix. (:rows m1) (:columns m1) (vec (map - (:data m1) (:data m2))))))
  ([m1 m2 & args]
   (let [initial (subtract m1 m2)]
     (reduce subtract initial args))))

(defn multiply [m1 m2]
  "Multiplies m1 and m2. Uses the naive algorithm"
  (let [dimensions (multiplicable? m1 m2)]
    (when (not (nil? dimensions))
      (let [data (for [i (range (first dimensions))
                       j (range (second dimensions))]
                   (v/dot-product (get-row m1 i) (get-column m2 j)))]
        (Matrix. (first dimensions) (second dimensions) (vec data))))))
