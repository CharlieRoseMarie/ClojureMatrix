(ns ClojureMatrix.matrix)

(defn- proper? [matrix]
  "Returns true if the matrix has the same number of elements for each row."
	(apply = (map #(count %) matrix)))

(defn- create-identity-row [size nth-col]
	(for [i (range size)]
		(if (= i nth-col)
			1 0)))

(defn create-identity [n]
	"Creates an identity matrix of size n*n"
	(for [row (range n)]
		(create-identity-row n row)))

(defn get-row [matrix row]
  {:pre [(proper? matrix) (< row (count matrix))]}
	(nth matrix row))

(defn get-element [matrix row col]
  {:pre [(proper? matrix)]}
	(get-in matrix [row col]))

(defn get-column [matrix col]
  {:pre [(proper? matrix) (< col (count (first matrix)))]}
	(map #(nth %1 col) matrix))

(defn multiplicable? [matrixA matrixB]
	"If true, returns a vector of the dimensions of the matrix that would result from multiplying the
	two matrices. Returns nil otherwise."
  {:pre [(proper? matrixA) (proper? matrixB)]}
	(let [m (count (get-row matrixA))
		  n (count (get-column matrixB))]
		  (if (= m n) [m n] nil)))

(defn square? [matrix]
  "Returns true if the matrix is square"
  {:pre [(proper? matrix)]}
	(when (proper? matrix)
		(= (count (get-row matrix 0)) (count (get-column matrix 0)))))

(defn transpose [matrix]
  "Returns the transpose of the matrix."
  {:pre [(proper? matrix)]}
  (apply map vector matrix))


(defn determinant [matrix]
  0)