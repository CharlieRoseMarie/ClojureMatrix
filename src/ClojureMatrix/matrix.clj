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
