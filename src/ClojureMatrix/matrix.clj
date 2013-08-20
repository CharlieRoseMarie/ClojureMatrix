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

(defn zero-matrix? [matrix]
  {:pre [(proper? matrix)]}
  (every? #(all-zeros? %) matrix))

(defn vector-dot-product
  "Returns the dot product of one or more vectors"
  ([] 0)
  ([v] (apply + v))
  ([v1 v2]) (reduce + (map * v1 v2)))

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

(defn get-dimensions [matrix]
  {:pre [(proper? matrix)]}
  "Retuns the dimensions of the matrix"
  [(count (get-row matrix 0)) (count (get-column matrix 0))])

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

(defn minor [matrix row col]
  {:pre [(proper? matrix)]}
  "Returns the minor of the matrix for the given row and column"
  (remove-row (remove-column matrix col) row))

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

(defn- get-piv [matrix row]
  {:pre (proper? matrix) (>= row 0) (< row (count matrix))}
  (let [piv (first (drop-while #(= 0 %) (matrix row)))
        piv-loc (count-preceeding-zeros (matrix row))]
    (hash-map :piv piv :loc piv-loc)))

(defn- add-vectors [v1 v2 mult]
  (map + v2 (map #(* mult %) v1)))

(defn- get-first-nonzero [v]
  (first (drop-while #(= 0 %) v)))

(defn- eliminate-at-row [matrix row]
  (let [first-half (subvec matrix 0 (inc row))
        v (matrix row)
        second-half (subvec matrix (inc row))
        piv-val ((get-piv matrix row) :piv)
        piv-loc ((get-piv matrix row) :loc)
        modified-half (map vec (map #(add-vectors v % (- (/ (% piv-loc) piv-val))) second-half))]
    (vec (concat first-half modified-half))))

(defn make-ref [matrix]
  {:pre [(proper? matrix)]}
  "Transforms the matrix into REF form"
  (let [sort-m (vec (sort-by count-preceeding-zeros matrix))]
  (loop [m sort-m piv 0]
    (cond
     (zero-matrix? m) m
     (ref? m) m
     (not= 1 ((get-piv m piv) :piv)) (recur (multiply-row m piv (/ ((get-piv m piv) :piv))) piv)
     :else (recur (vec (sort-by count-preceeding-zeros (eliminate-at-row m piv))) (inc piv))))))

; Algebraic operations

(defn scalar-multiply [matrix v]
  (map (fn [row] (map #(* v %) row)) matrix))

(letfn [(add-vectors [v1 v2] (map + v1 v2))]
 (defn add
  ([m] m)
  ([m1 m2]
    (map #(add-vectors %1 %2) m1 m2))
  ([m1 m2 & ms]
    (let [initial (add m1 m2)]
      (reduce add initial ms)))))

(letfn [(sub-vectors [v1 v2] (map - v1 v2))]
 (defn subtract
  ([m] (scalar-multiply m -1))
  ([m1 m2] (map #(sub-vectors %1 %2) m1 m2))
  ([m1 m2 & ms]
   (let [initial (subtract m1 m2)]
     (reduce subtract initial ms)))))