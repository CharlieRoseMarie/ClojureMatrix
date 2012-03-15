(ns ClojureMatrix.matrix)


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
	(nth (get-row matrix row) col))

(defn get-column [matrix col]
	(map #(nth %1 col) matrix))

(defn multiplicable? [matrixA matrixB]
	"If true, returns a vector of the dimensions of the matrix that would result from multiplying the 
	two matrices. Returns false otherwise."
	(let [m (count (get-row matrixA)) 
		  n (count (get-column matrixB))]
		  (if (= m n) (list m n) nil)))

(defn transpose [matrix]
      (for [i (range (count (first matrix)))]
	   (get-column matrix i)))
