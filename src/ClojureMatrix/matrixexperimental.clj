(ns ClojureMatrix.matrixexperimental)

(defrecord Matrix [rows columns data])

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

; Test values
(def m1 (Matrix. 3 3 [1 0 1 0 1 2 0 0 1]))
(def m2 (matrix-from-nested [[1 0 0 0][0 1 0 0][0 0 1 0 ]]))

(defn get-element [matrix row col]
  {:pre [(valid-row? matrix row) (valid-column? matrix col)]}
  (let [e (+ (* (:rows matrix) row) col)
        data (:data matrix)]
    (data e)))

(defn get-row [matrix row]
  {:pre [(valid-row? matrix row)]}
  (let [start (* (:columns matrix) row)
        end   (* (:columns matrix) (inc row))]
   (vec (subvec (:data matrix) star end))))

(defn get-column [matrix col]
  {:pre [(valid-column? matrix col)]}
  (vec (take-nth (+ col (:row matrix)) (:data matrix))))

(get-column m1 0)

(defn equal-dimensions? [m1 m2]
  "Returns true if the dimensions of the two matrices are equal"
  (and (= (:rows m1) (:rows m2))
       (= (:columns m1) (:columns m2))))


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

