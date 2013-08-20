(ns ClojureMatrix.matrixhelpers)

(defn proper? [matrix]
  "Returns true if the matrix has the same number of elements for each row."
	(apply = (map #(count %) matrix)))

(defn count-preceeding-zeros [v]
  "Returns the number of zeros at the front of a row."
  (count (take-while #(= % 0) v)))

(defn all-zeros? [v]
  "Returns true if every element in a row is zero"
  (every? #(= % 0) v))

(defn create-identity-row [size nth-col]
	(for [i (range size)]
		(if (= i nth-col)
			1 0)))

(defn get-piv [matrix row]
  {:pre (proper? matrix) (>= row 0) (< row (count matrix))}
  (let [piv (first (drop-while #(= 0 %) (matrix row)))
        piv-loc (count-preceeding-zeros (matrix row))]
    (hash-map :piv piv :loc piv-loc)))

(defn add-vectors [v1 v2 mult]
  (map + v2 (map #(* mult %) v1)))

(defn get-first-nonzero [v]
  (first (drop-while #(= 0 %) v)))

(defn eliminate-at-row [matrix row]
  (let [first-half (subvec matrix 0 (inc row))
        v (matrix row)
        second-half (subvec matrix (inc row))
        piv-val ((get-piv matrix row) :piv)
        piv-loc ((get-piv matrix row) :loc)
        modified-half (map vec (map #(add-vectors v % (- (/ (% piv-loc) piv-val))) second-half))]
    (vec (concat first-half modified-half))))

