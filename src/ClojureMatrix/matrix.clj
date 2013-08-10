(ns ClojureMatrix.matrix)

(defn- proper? [matrix]
  "Returns true if the matrix has the same number of elements for each row."
	(apply = (map #(count %) matrix)))

(defn- count-preceeding-zeros [v]
  "Returns the number of zeros at the front of a row."
  (count (take-while #(= % 0) v)))

(defn- all-zeros? [v]
  "Returns true if every element in a row is zero"
  (every? #(= % 0) v))

(defn- create-identity-row [size nth-col]
	(for [i (range size)]
		(if (= i nth-col)
			1 0)))

(defn create-identity [n]
  {:pre [(> n 0)]}
	"Creates an identity matrix of size n*n"
  (for [row (range n)]
		(create-identity-row n row)))

(defn get-row [matrix row]
  {:pre [(proper? matrix) (< row (count matrix))]}
	"Returns the nth row"
  (nth matrix row))

(defn get-element [matrix row col]
  {:pre [(proper? matrix)]}
	(get-in matrix [row col]))

(defn get-column [matrix col]
  {:pre [(proper? matrix) (< col (count (first matrix)))]}
	(map #(nth %1 col) matrix))

(defn remove-row [matrix row]
  {:pre [(proper? matrix) (>= row 0) (< row (count matrix))]}
  "Returns the matrix without the given row"
  (let [first-matrix (subvec matrix 0 row)
        second-matrix (subvec matrix (inc row))]
    (concat first-matrix second-matrix)))

(defn remove-column [matrix col]
  {:pre [(proper? matrix) (>= col 0) (< col (count (first matrix)))]}
  "Returns the matrix without the given column"
  (letfn [(rem-ele [row]
          (concat (subvec row 0 col)
                (subvec row (inc col))))]
    (map rem-ele matrix)))

(defn multiplicable? [matrixA matrixB]
  {:pre [(proper? matrixA) (proper? matrixB)]}
	"If true, returns a vector of the dimensions of the matrix that would result from multiplying the
	two matrices. Returns nil otherwise."
	(let [m (count (get-row matrixA))
		  n (count (get-column matrixB))]
		  (if (= m n) [m n] nil)))

(defn square? [matrix]
  {:pre [(proper? matrix)]}
  "Returns true if the matrix is square"
	(when (proper? matrix)
		(= (count (get-row matrix 0)) (count (get-column matrix 0)))))

(defn transpose [matrix]
  {:pre [(proper? matrix)]}
  "Returns the transpose of the matrix."
  (apply map vector matrix))

(defn minor [matrix i j]
  {:pre [(proper? matrix)]})

; Symmetric check

(defn symmetric? [matrix]
  {:pre [(proper? matrix)]}
  "Returns true if the matrix equals it's own transpose."
  (= matrix (transpose matrix)))

; REF check

(defn ref? [matrix]
  {:pre [(proper? matrix)]}
  "Returns true if the matrix is in REF."
  (let [z-rows (map all-zeros? matrix) z-counts (map count-preceeding-zeros (filter #(not (all-zeros? %)) matrix))]
    (and (apply < z-counts)
         (every? #(= true %) (drop-while #(= false %) z-rows)))))

; Elementary row operatations
(defn multiply-row [matrix row value]
  {:pre [(proper? matrix) (>= row 0) (< row (count matrix))]}
  "Multiplies the given row by some value. Returns the matrix."
  (assoc matrix row (map #(* value %) (matrix row))))

(defn swap-rows [matrix r1 r2]
  {:pre [(proper? matrix) (>= r1 0) (< r1 (count matrix)) (>= r2 0) (< r2 (count matrix))]}
  "Swaps the two given rows of the matrix"
  (let [row1 (matrix r1) row2 (matrix r2)]
    (-> matrix
        (assoc r2 row1)
        (assoc r1 row2))))

(defn add-rows [matrix r1 m-val r2]
  {:pre [(proper? matrix) (>= 0 r1) (>= 0 r2) (< r1 (count matrix)) (< r2 (count matrix))]}
  "Returns the matrix with the r1 row multiplied by m-val added to row r2"
  (let [add-row (map #(* % m-val) (matrix r1))
        new-row (map + (matrix r2) add-row)]
    (assoc matrix r2 new-row)))
; REF

(defn make-ref [matrix]
  {:pre [(proper? matrix)]}
  "Transforms the matrix into REF form"
  (loop [m matrix piv 0]
    (if (ref? m) m
      (recur (sort-by count-preceeding-zeros m) (inc piv)))))