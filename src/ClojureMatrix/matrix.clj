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
	(nth matrix row))

(defn get-element [matrix row col]
	(get-in matrix [row col]))

(defn get-column [matrix col]
	(map #(nth %1 col) matrix))

(defn multiplicable? [matrixA matrixB]
	"If true, returns a vector of the dimensions of the matrix that would result from multiplying the
	two matrices. Returns nil otherwise."
	(let [m (count (get-row matrixA))
		  n (count (get-column matrixB))]
		  (if (= m n) [m n] nil)))

(defn square? [matrix]
  {:pre (proper? matrix)}
  "Returns true if the matrix is square"
	(when (proper? matrix)
		(= (count (get-row matrix 0)) (count (get-column matrix 0)))))

(defn transpose [matrix]
  "Returns the transpose of the matrix."
  (apply map vector matrix))


(defn determinant [matrix]
  0)