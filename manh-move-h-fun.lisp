

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPLEMENT MANHATTAN MOVE H-FUN FOR CLIMB12, CLIMB15A, and CLIMB24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PLAN:
;;;  (setup-data-for-manhattan-move-h-fun <solution-target-position>)
;;;    This precomputes (for the target-solution-position) a mapping for each piece-type:
;;;      Mapping piece-index -> minimum-match-move-dist to given targets in solution
;;;    It also sets **target-position** to the supplied <solution-target-position>
;;;      When non-nil this causes A* search code to use the fast-manhattan-move-h-fun
;;;          instead of the t-piece-h-fun
;;;  Then to evaluate h-fun:
;;;   (fast-manhattan-move-h-fun)
;;;     which operates on **intermediate-position**
;;;      by scanning the intermediate-position vector
;;;       summing the precomputed match-move-dist for each piece-type
;;;         and returns the sum of these over all piece-types

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; convert tuple '(0 2 3 4) into an index (in range 0 below (choose 36 4)

;;    (integer-index (loop for i in '(0 2 3 4) sum (ash 1 i)) 36 4)

;; similar to bit-int-from-list defines in SBP-Ext-AStar-blank-index-jimslide.lisp
(defun bit-int-from-index-tuple (tuple)
  (loop for i in tuple
     sum
       (ash 1 i)))

;; this is actually called below
(defun tuple-from-bit-int (bit-int)
  (loop for bit-index from 0 to (integer-length bit-int)
     when (logbitp bit-index bit-int)
     collect bit-index))

(defun integer-index-from-tuple (tuple n)
  (integer-index (bit-int-from-index-tuple tuple)
		 n
		 (length tuple)))

;; Call (INTEGER-FROM-INDEX index n k)
;;     to get bit-int from index

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Hard part is pre-computing all the "group manhattan distances"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pre-compute based on a given completely specified target solution position

;;; Step 0 -- compute (separately) the horizontal and vertical manh-dist for cell pairs
;;;      (aref *coords-from-cell* cell)  -->  (row col)
;;;     Store in single array indexed by from-cell to-cell
;;;            values being pairs  (vert-manhdist hor-manhdist)


;;; Step 1 -- compute the move manhattan distance for each single piece of a given type
;;;      Array indexed by cell giving ref. pos. of piece,
;;;          array value = move-distance from ref.cell to target cell
;;;       Must compute separately for each target cell for given piece-type
;;;         1 idea is to have a tuple listing distances to each target of that type
;;;    Complication:
;;;      Some piece-types are "non-isotropic", ie can move different amounts hor. v. vert.
;;;         (or combining hor & vert)
;;;      So need to compute hor. and vert. component separately

;;; Step 2 -- compute the minimal distance over all match-pairs,
;;;      Pairing a source piece with a specific (unique) target
;;;         (for pieces of multiplicity 4 there will be 4!= 24 matches to check)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analyzing piece-type movements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

For Climb12 puzzle:

Type-num     Piece-descrip.   Multiplicity   Hor.   Vert    Hor,vert
;; 0             x x   NW         1            2      2        1,1           
;;               x

;; 1               x   SE         1            2      2        1,1
;;               x x

;; 2             x                2            2      4        1,2
;;               x

;; 3             x x              2            4      2        2,1

;; 4               x    T         1            2      1        1,1
;;               x x x

;; 5             x                4            4      4        1,3 or 2,2 or 3,1


For Climb15a puzzle:

Type-num     Piece-descrip.   Multiplicity   Hor.   Vert    Hor,vert
;; 0             x x              1            2      2        1,1
;;               x x

;; 1             x x   NW         1            2      2        1,1
;;               x

;; 2             x x   NE         1            2      2        1,1
;;                 x

;; 3               x   SE         1            2      2        1,1
;;               x x

;; 4             x     SW         1            2      2        1,1
;;               x x

;; 5             x                2            2      4        1,2
;;               x

;; 6             x x              2            4      2        2,1

;; 7               x    T         1            2      1        1,1
;;               x x x

;; 8             x                4            4      4        1,3 or 2,2 or 3,1


;; For Climb24 puzzle:

Type-num     Piece-descrip.   Multiplicity   Hor.   Vert    Hor,vert

;; 0              x      T        1             2       1       1,1
;;              x x x 

;; 1            x x     NW        1             2       2       1,1
;;              x

;; 2            x x     NE        2             2       2       1,1
;;                x

;; 3              x     SE        1             2       2       1,1
;;              x x

;; 4            x       SW        2             2       2       1,1
;;              x x

;; 5            x x     2x2       4             2       2       1,1
;;              x x

;; 6            x                 4             2       4       1,2
;;              x

;; 7            x x               2             4       2       2,1

;; 8            x x x             2             4       1       1,1

;; 9            x                 4             4       4       1,3 or 2,2 or 3,1


|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SETUP STRUCTURES WITH PRE-COMPUTED VALUES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; assumes puzzle is already initialized
;;;  *num-cells*
;;;  *num-piece-types*
;;;  *coords-from-cell*

(defparameter **piece-type-move-data**
  nil)

(defparameter **climb12-piece-type-move-data**
  ;; element-format: (type-num simple-md? hor-comb vert-comb hor-only vert-only
  '((0 2 1 1 2 2)     ;; NW
    (1 2 1 1 2 2)     ;; SE
    (2 nil 1 2 2 4)   ;; 2x1 Vert Rect
    (3 nil 2 1 4 2)   ;; 1x2 Hor Rect
    (4 nil 1 1 2 1)   ;; T
    (5 4 2 2 4 4)     ;; 1x1 Singleton
    ))

(defparameter **climb15a-piece-type-move-data**
  ;; element-format: (type-num simple-md? hor-comb vert-comb hor-only vert-only
  '((0 2 1 1 2 2)     ;; 2x2
    (1 2 1 1 2 2)     ;; NW
    (2 2 1 1 2 2)     ;; NE
    (3 2 1 1 2 2)     ;; SE
    (4 2 1 1 2 2)     ;; SW
    (5 nil 1 2 2 4)   ;; 2x1 Vert Rect
    (6 nil 2 1 4 2)   ;; 1x2 Hor Rect
    (7 nil 1 1 2 1)   ;; T
    (8 4 2 2 4 4)     ;; 1x1 Singleton
    ))



(defparameter **climb24-piece-type-move-data**
  '((0 nil 1 1 2 1)   ;; T
    (1 2 1 1 2 2)     ;; NW
    (2 2 1 1 2 2)     ;; NE
    (3 2 1 1 2 2)     ;; SE
    (4 2 1 1 2 2)     ;; SW
    (5 2 1 1 2 2)     ;; 2x2
    (6 nil 1 2 2 4)   ;; 2x1 Vert Rect
    (7 nil 2 1 4 2)   ;; 1x2 Hor Rect
    (8 nil 1 1 4 1)   ;; 1x3 Hor Rect
    (9 4 2 2 4 4)     ;; 1x1 Singleton
    ))

(defun set-piece-type-move-data-for-puzzle-type (puzzle-name)
  (cond ((string-equal puzzle-name "CLIMB12")
	 (setf **piece-type-move-data**
	       **climb12-piece-type-move-data**))
        ((string-equal puzzle-name "CLIMB15A")
         (setf **piece-type-move-data**
               **climb15a-piece-type-move-data**))
        ((string-equal puzzle-name "CLIMB24")
         (setf **piece-type-move-data**
               **climb24-piece-type-move-data**))
        (t (error "unknown puzzle-name ~a" puzzle-name))
        ))

(defparameter **cell-to-cell-row-col-distances**
  nil)

(defun compute-cell-to-cell-row-col-distances ()
  (setf **cell-to-cell-row-col-distances**
	(make-array (list *num-cells* *num-cells*)))
  (loop for cell1 from 0 below *num-cells*
     for (row1 col1) = (aref *coords-from-cell* cell1)
     do
       (loop for cell2 from 0 below *num-cells*
	  for (row2 col2) = (aref *coords-from-cell* cell2)
	  do
	    (setf (aref **cell-to-cell-row-col-distances** cell1 cell2)
		  (list (abs (- row1 row2))
			(abs (- col1 col2)))))))

(defparameter **permutation-lists-by-size**
  nil)

(defun get-permutation-list (size)
  (second (assoc size **permutation-lists-by-size**)))

(defun setup-permutation-lists ()
  ;;; does sizes 1 to 4 by default
  (setf **permutation-lists-by-size**
	(list (list 1 (collect-permutations '(0)))
	      (list 2 (collect-permutations '(0 1)))
	      (list 3 (collect-permutations '(0 1 2)))
	      (list 4 (collect-permutations '(0 1 2 3))))))

(defun collect-permutations (lis)
  (cond ((null lis)
	 (list nil))
	(t (loop for elt in lis
	      append (cons-all elt
			       (collect-permutations (remove elt lis)))))))

(defun cons-all (elt list-of-lists)
  (loop for lis in list-of-lists
     collect
       (cons elt lis)))

(defparameter **target-position** nil)

(defparameter **targets-by-piece-type** nil)

(defun collect-targets-by-piece-type (&optional (target-position **target-position**))
  (jimslide-uncompress target-position)
  (loop with targets-by-piece-type = (make-array *num-piece-types*
						 :initial-element nil)
     for piece-type across **intermediate-position**
     for index from 0
     unless (< piece-type 0)  ;; no piece there
     do
       (push index (aref targets-by-piece-type piece-type))
     finally
       (return targets-by-piece-type)))

(defparameter **single-piece-move-distance-arrays-by-piece-type**
  nil)

(defun precompute-single-piece-move-distance-arrays (&optional
						       (targets-by-piece-type
							**targets-by-piece-type**))
  (setf **single-piece-move-distance-arrays-by-piece-type**
	(make-array *num-piece-types*))
  ;;(print **single-piece-move-distance-arrays-by-piece-type**)
  (loop for piece-type from 0
     for target-cells across targets-by-piece-type
     do
       (setf (aref **single-piece-move-distance-arrays-by-piece-type** piece-type)
	     (single-piece-move-distances-for-piece-type piece-type target-cells))))

(defun single-piece-move-distances-for-piece-type (piece-type targets)
  (loop with move-dist-array = (make-array (list *num-cells* (length targets)))
     with piece-type-data = (cdr (assoc piece-type
					**piece-type-move-data**))
     for cell from 0 below *num-cells*
     do
       (loop for target-cell in targets
	  for target-index from 0
	  do
	    (setf (aref move-dist-array cell target-index)
		  (move-distance-cell-to-target piece-type-data cell target-cell)))
     finally
       (return move-dist-array)))

(defun move-distance-cell-to-target (piece-type-data cell target)
  ;; Format of piece-type-data:
  ;;     ( simple-manh-dist? hor-comb vert-comb hor-only vert-only )
  (let* ((row-col-dist-pair (aref **cell-to-cell-row-col-distances** cell target))
	 (row-dist (first row-col-dist-pair))
	 (col-dist (second row-col-dist-pair))
	 (simple-manh-step? (first piece-type-data)))
    (cond (simple-manh-step?
	   (ceiling (+ row-dist col-dist)
		    simple-manh-step?))
	  (t
	   (complex-manh-move-dist (cdr piece-type-data) col-dist row-dist)))
    ))

(defun complex-manh-move-dist (piece-type-data-cdr hor-dist vert-dist)
  ;; piece-type-data-cdr format:
  ;;    ( hor-comb-step vert-comb-step hor-only-step vert-only-step )
  (let* ((hor-comb-step (first piece-type-data-cdr))
	 (vert-comb-step (second piece-type-data-cdr))
	 (comb-move-count ;; min combine moves to zero out hor or vert
	  (min (ceiling hor-dist hor-comb-step)
	       (ceiling vert-dist vert-comb-step)))
	 (resid-hor-dist (max 0
			      (- hor-dist
				 (* comb-move-count hor-comb-step))))
	 (resid-vert-dist (max 0
			       (- vert-dist
				  (* comb-move-count vert-comb-step)))))
    (cond ((and (zerop resid-hor-dist)
		(zerop resid-vert-dist))
	   ;; done using only combine moves
	   comb-move-count)
	  ((zerop resid-hor-dist)
	   ;; go rest of way with vert-only-step
	   (+ comb-move-count
	      (ceiling resid-vert-dist
		       (fourth piece-type-data-cdr) ;; vert-only-step
		       )))
	  (t ;; go rest of way with hor-only-step
	   (+ comb-move-count
	      (ceiling resid-hor-dist
		       (third piece-type-data-cdr) ;; hor-only-step
		       ))))))


;;;;;;;;;;;;;;;;
;;; MAIN SETUP
;;;;;;;;;;;;;;;;

;;; Data Array indexed by piece-type
;;;    Value is an Array to be indexed by piece-type
;;;       Value of this array is "manahattan-move-counts" for given piece-type

(defparameter **manhattan-h-fun-data**
  nil)

(defparameter **piece-type-ints**      ;;; REUSE and reset all values to 0
  nil)

(defun setup-data-for-manhattan-move-h-fun (&optional
                                              (target-position **target-position**))
  ;;; preliminaries
  (setup-permutation-lists)
  (setf **target-position** target-position)
  (setf **piece-type-ints** (make-array *num-piece-types*))
  (set-piece-type-move-data-for-puzzle-type **puzzle-name**)
  (setf **targets-by-piece-type**
	(collect-targets-by-piece-type target-position))
  (compute-cell-to-cell-row-col-distances)
  (precompute-single-piece-move-distance-arrays)
  (setup-fast-manhattan-move-h-fun-data)    ;; to support fast-manhattan-move-h-fun
  ;;; main setup
  (setf **manhattan-h-fun-data**
	(make-array *num-piece-types*))
  (loop for piece-type from 0 below *num-piece-types*
     for target-list across **targets-by-piece-type**
     do
       (setf (aref **manhattan-h-fun-data** piece-type)
	     (minimum-match-distance-array piece-type
					   target-list))))

(defun minimum-match-distance-array (piece-type target-list)
  (loop with target-count = (length target-list)
     with piece-type-index-count = (choose *num-cells* target-count)
     with target-permutations = (get-permutation-list target-count)
     with move-dist-array = (aref **single-piece-move-distance-arrays-by-piece-type**
				  piece-type)
     with min-match-array = (make-array piece-type-index-count
					:element-type 'integer)
     for piece-type-index from 0 below piece-type-index-count
     for piece-tuple = (tuple-from-bit-int
			(integer-from-index piece-type-index *num-cells* target-count))
     do
       (setf (aref min-match-array piece-type-index)
	     (compute-min-match-move-distance piece-tuple target-permutations
					      move-dist-array))
     finally
       (return min-match-array)
       ))


(defun compute-min-match-move-distance (piece-tuple
					target-perms
					move-dist-array)
  (loop with min-match-dist = (* (length piece-tuple) *num-cells*) ;;  make LARGE value
     for perm in target-perms ;; perm is of 0 to (1- (length piece-tuple))
     for new-dist = (match-dist piece-tuple perm move-dist-array)
     when (< new-dist min-match-dist)
     do
       (setf min-match-dist new-dist)
     finally
       (return min-match-dist)))

(defun match-dist (piece-tuple perm move-dist-array)
  (loop for piece-cell in piece-tuple
     for target-index in perm
     for piece-dist = (aref move-dist-array piece-cell target-index)
     sum piece-dist))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPUTING MANHATTAN-MOVE-H-FUN for a SUCCESSOR-POSITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLIMB24 Solution Targets
(defparameter **climb24-target-230**            ;; 230 sol-pos from hscale = 4
  #(110270073 1116033380 1989367168 341054))

(defparameter **climb24-target-231**            ;; 231 sol-pos from hscale = 5
  #(19171737 2003391846 1116091728 341054))

(defparameter **climb24-target-253**            ;; 253 sol-pos from hscale = 6
  #(26834792 2455069746 1750488208 341054))

(defparameter **climb24-target-256**            ;; 256 sol-pos from hscale = 7
  #(26646167 2304255606 877929808 316250))

(defparameter **climb24-target-249**            ;; 249 sol-pos from hscale = 8
  #(110270326 1502164246 1115972944 341054))    


;; CLIMB15a Solution Targets
(defparameter **climb15a-target-104**  ;; same sol-pos for hscale = 1,2?,3,5,6,7,8,9,12,13,14
  #(1971882004 593823232 23750))

(defparameter **climb15a-target-116**  ;; from solution with h-scale = 4
  #(2022203778 893408768 23750))

(defparameter **climb15a-target-187-scale10**  ;; from solution with h-scale = 10
  #(2022188388 890766848 27404))

(defparameter **climb15a-target-187-scale11**  ;; from solution with h-scale = 11
  #(2022188133 1451234048 23750))

;; CLIMB12 Solution Targets
(defparameter **climb12-target-59**
  #(1163206709 305135616 4844))


;;; when uncompressing position:
					;    (fill **piece-type-ints** 0)
;;; when placing piecetype PT in index I:
					;    (incf (aref **piece-type-ints** PT)
					;          (ash 1 I))


(defun manhattan-move-h-fun ()    ;; operates on **intermediate-position**
  (fill **piece-type-ints** 0)
  ;; collect piece-type bit-ints
  (loop for piece-type across **intermediate-position**
     for shift-int = 1 then (ash shift-int 1)
     unless (< piece-type 0)
     do
       (incf (aref **piece-type-ints** piece-type)
	     shift-int))
  ;; **piece-type-ints** should hold bit-ints for each piece-type
  ;; (print **piece-type-ints**))
  (loop for bitint across **piece-type-ints**
     for min-piece-type-dist-array across **manhattan-h-fun-data**
     sum
       (aref min-piece-type-dist-array
	     (integer-index bitint *num-cells* (logcount bitint))))  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Faster INTEGER-INDEX !!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FAST-MANHATTAN-MOVE-H-FUN runs about 3x faster than MANHATTAN-MOVE-H-FUN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calculate piece-indices directly (avoiding bitints)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IDEA:
;;;   keep pair  (k sum) for each piece type
;;;   let N = (1- *num-cells*)
;;;   iterate for index from 0
;;;      when IntPos[index] = pt  (pt >= 0)
;;;         doto pair[pt]
;;;            incf pair.sum by (fast-choose (- n index) k)
;;;            decf pair.k

(defparameter **k-values-working-array**
  nil)   ;; set by copying **k-values-source-array**

(defparameter **k-values-source-array**
  nil    ;; Set this from **target-position** when doing
  )      ;;    setup-data-for-manhattan-move-h-fun

(defparameter **sum-values-working-array**
  nil)   ;; set to sequence of length *num-piece-types*

(defun setup-fast-manhattan-move-h-fun-data ()
  ;; assume **targets-by-piece-type** already setup
  (setf **k-values-working-array**
        (make-array (length **targets-by-piece-type**))
        **k-values-source-array**
        (make-array (length **targets-by-piece-type**))
        **sum-values-working-array**
        (make-array (length **targets-by-piece-type**)))
  (loop for target-list across **targets-by-piece-type**
     for piece-type from 0
     do
       (setf (aref **k-values-source-array** piece-type)
             (length target-list)))
  )

(defun fast-manhattan-move-h-fun ()
  (fill **sum-values-working-array** ;; reset sums to 0
	0)
  (replace **k-values-working-array** ;; copy source to working-array
	   **k-values-source-array**)
  ;; computes value from **intermediate-position**
  (loop with current-k = nil
     for n-choose-value downfrom (1- *num-cells*)
     for piece-type across **intermediate-position**
     unless (< piece-type 0)
     do
       (setf current-k
	     (aref **k-values-working-array** piece-type)) ;; k-val for piece-type
       (incf (aref **sum-values-working-array** ;; accumulate in sum for piece-type
		   piece-type)
	     (fast-choose n-choose-value  ;; maybe make fast-choose a macro ?
			  current-k))     ;;   it just does an (aref **choose-array** n k)
       (decf (aref **k-values-working-array** piece-type)) ;; decf k-val for piece-type
       )
  ;; At thhis point the sums in **sum-values-working-array**
  ;;    should hold the piece-indices for each piece-type
  (loop for piece-index across **sum-values-working-array**
     for min-piece-type-dist-array across **manhattan-h-fun-data**
     sum
       (aref min-piece-type-dist-array
	     piece-index))
  )


