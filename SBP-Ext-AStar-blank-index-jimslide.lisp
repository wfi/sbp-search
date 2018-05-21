(declaim (optimize (speed 3) (debug 0) (safety 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SLIDING BLOCK PUZZLE DOMAIN IMPLEMENTATION -- ADAPTED FOR EXTERNAL A-STAR
;;;     -- USING BLANK-INDEX AND JIMSLIDE COMPRESSION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; VERSION NOTES

;;;    cf. /BreadthFirstFileSearch/Domains/Sliding-Block-Puzzles/SBP-7-blank-index-jimslide.lisp
;;;      for old version notes

;;;  SBP-7-Ext-Astar-blank-index-jimslide (start this branch in A-STAR directory
;;;   10-5-2016
;;;     Modify GENERATE-SUCCESSORS to conform to Ext-AStar-SBP.lisp interface requirements:
;;;        (generate-successors position output-buffer)
;;;            output-buffer = (h-max A0 A1 A2) where Ai are hash-tables
;;;            h-value computed from **intermediate-position** and used to select Ai for storing position
;;;     New heuristic-function (for use in generate-successors)
;;;       -- specific to T-pieces
;;;        (SBP-H-FUN <t-piece-cell> <target-cell>)
;;;            Return (1- Manhattan distance from t-piece to target)
;;;             NOTE: t-piece can sometimes move 2 steps, but another piece must also move afterwards
;;;                the 1- is for case of last move being 2-cell-steps into target loc
;;;     **T-piece-type**  new global set during init (used for h-fun / manhattan-distance)
;;;   10-12-2016
;;;     Comment out unneeded globals:
;;;          **heap-threshold** **use-parity?**  **force-full-dup-elim-interval**
;;;          **prior-fringe-lookup-limit**

;;; Version 2  [SBP-7-Ext-Astar-blank-index-jimslide-2]
;;;   Modify t-piece-h-fun to take new-blank-index arg
;;;   Precompute blank-mask for each t-piece cell-position
;;;      h-val = (+  (1- (* 2 vertical-row-dist))
;;;                  (if (3-blanks-below-t-cell-pos?) 1 0))
;;;

;;; Version 3 [SBP-7-Ext-Astar-blank-index-jimslide-3]
;;;   Spread H-FUN further by multiplying row-diff by 4 and using more blank pattern-tests
;;;      h-val = (+ (1+ (* 4 (1- vertical-row-dist)))   ;; but 0 if solved (T at goal cell)
;;;                 (hfun-offset-from-blanks t-piece-cellnum blank-bits)
;;;      where offset is:
;;;          +0   if solved (T in goal location)
;;;          +3   if there are 3 blanks immediately below T
;;;          +2   if there are 3 blanks anywhere below the T (but not all immediately below)
;;;          +0   if any of the 3 "ready to move up" blank-patterns are filled
;;;                     a. 3 blanks immediately above T
;;;                     b. 4 blanks in Z-pentommino shape immediately right or left of T
;;;          +1   if none of the above apply

;;; Version 4 [SBP-7-Ext-Astar-blank-index-jimslide-3]
;;;   Add new test to hfun-offset-from-bllanks to check if 2 or more blanks are >= 5 away from T-ref
;;;       if so, use offset +2
;;;       put this test 2nd last (just before default for none-of-above


;;; TODO:
;;;   Solution Recovery
;;;       Figure out where / if to implement fancy-display-compressed-solution-sequence

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SBP DOMAIN INTERFACE WITH EXTERNAL A* SEARCH ENGINE
;;;      A-STAR/Ext-AStar-SBP.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  Domain positions:
;;;      same as in slide-solve-fringe-45.lisp  [arrays / sequences of integer-bit-vectors, with 1 entry (bit-integer) for each piece-type]
;;;  Compressed positions:
;;;      byte sequences  (of size **position-size**)    ;;; NOW IN JIMSLIDE = # bytes to hold IntegerCode
;;;  legal moves:  move any piece a legal sequence of steps (multiple primitive moves count as 1 move)
;;;  start search with *start-pos* 
;;;  solved? =  "position = satisfies (check-for-solution <pos>) matches to target
;;;  

;;; OLD USAGE (version SBP-2-BLANK-INDEX-JIMSLIDE):
;;;   Load in order: 
;;;     [from directory: BreadthFirstFileSearch/ ]
;;;          my-profiler-2.lisp   (now using my-profiler-4.lisp)
;;;          file-search-engine-<n>.lisp   
;;;     [from directory: BreadthFirstFileSearch/Domains/Sliding-Block-Puzzles/  ]
;;;          (added 9-25-2014)
;;;            HMMM ... maybe avoid loading slide-solve-fringe-4x ?? (duplicates code in this file)
;;;             slide-solve-fringe-45-copy.lisp   ;;; defines needed functions for slide-puzzles-init-<n>
;;;             RECOMPILE my-profiler-4.lisp (since START-TIMING got clobbered by slide-solve-fringe-45-copy)   <maybe don't need this if use slide-solve-fringe-45-copy-mod.lisp which comments out timing funs>
;;;          (end addition 9-25-2014)
;;;          slide-puzzle-inits-<n>.lisp
;;;          SBP-2-BLANK-INDEX-JIMSLIDE.lisp   ;;; this file!    [may need to compile twice !? ]
;;;   (old) Run (SBP-init <puzzle-selector>)   ;;; need to add more selectors - just a few so far
;;;   (new) Run (SBP-exper <puzzle-selector> <exper-tag>)
;;;   Set **check-solved?** to NIL if want reverse search from solution, or exhaustive search for reachable positions
;;;   Then can run search: (fancy-breadth-first-file-solve)


;;; Globals Shared with File-Search-Engine (some not used for EXT-ASTAR-SBP)

;;; NOTE: this name is the default directory for storing search fringes for puzzle, so be careful about overwriting
;;;        Maybe should warn and ask if ok to delete all files in folder before starting search!
;;;           Deleting has advantage of avoiding BUG of residual (possibly incompatible) fringes lying around to mess up merge
(defparameter **puzzle-name** nil)    ;; needs to get set by actual puzzle initialization
(defparameter **exper-tag** nil)
(defvar **puzzle-directory-name**)

(defparameter **position-size** nil)  ;; number of bytes to encode a sequence (must be set by domain-init)
(defparameter **byte-size** 32)

(defparameter **start-pos-list** nil)    ;; list of compressed positions in "search start list"

;;; Globals local to domain code
;;;    All need to be re-initialized with any puzzle-init


(defparameter **start-domain-pos-list** nil)      ;; for locally storing uncompressed start-positios-list

(defparameter **compressed-solution-position**  ;; COMPRESSED SOLUTION POSITION 
  nil)  ;;  (should be set by (GENERATE-SUCCESSORS <position>) when (SOLVED? <some-successor>)

(defparameter **compressed-solution-target** nil)  ;; This is the "compressed" equivalent of *solution-target*  [format:  (list (byte-index byte-val) ... ) ]
(defparameter **check-solved?** t)     ;; T = normal search, set to NIL = exhaustive or reverse-search from solutions

(defparameter **sbp-position-register** nil)     ;; byte-position 
(defparameter **sbp-domain-position-register-aux** nil)  ;; for uncompressing for fancy-display position

(defparameter *position-size* nil)   ;;  NOTE:  this is DIFFERENT from **position-size**  (needed by old slide-init code)

;;; used by successor search
(defparameter *static-piece-bits* 0)
;(defparameter *test-capture-successor-positions* nil)
(defparameter *successor-search-queue*
  (make-array 1000 :element-type 'integer :fill-pointer t))

;;; for incremental update of compressed position
(defparameter **piece-type-offsets**     ;; should be a list of byte-index of first byte of piece-type
  nil)					 ;;   incremental update will start writing bytes here for piece-type
(defparameter **piece-type-offsets-vector**    ;; vector equivalent of **piece-type-offets** (to give faster access)
  nil)
(defparameter **piece-type-counts** nil)  ;; multiplicity of each piece-type
(defparameter **num-piece-instances** nil) ;; piece count (not including blanks)
(defparameter **blanks-offset**          ;; byte-offset for blanks in a compressed-position (last of **piece-type-offsets**)
  nil)
(defparameter **num-blanks**
  nil)
(defparameter **max-piece-count**
  nil)
(defparameter **blank-index-range**
  nil)
(defparameter **start-pos-with-blank-index**
  nil)

;;; for BLANK-INDEXING:
(defparameter **move-specs-for-blank-pattern**
  nil)    ;; must set to appropriate size array depending on puzzle init (number of cells!)
(defparameter **collection-vector**    ;; for collecting 32-bit unsigned ints for movespecs
  (make-array 100 :fill-pointer 0 :adjustable t :element-type '(unsigned-byte 32)))
(defparameter **blank-index-bytes** nil)
(defparameter **blanks-index** nil)   ; set this to avoid recomputing

(defparameter **choose-array** nil)   ;; precompute choose values for efficient lookup
(defparameter **piece-bit-int-array** nil)  ;; for precompute piece bit-ints

;;; for JIMSLIDE COMPRESSION

(defparameter **intermediate-position** nil)       ;; array of SIGNED-BYTES, holding type-code or -1 for NONE
(defparameter **intermediate-blank-index** nil)    ;; blank-index as integer
;; (defparameter **intermediate-used-cells** nil)     ;;  bitint with 1's for filled (used) cells
(defparameter **cells-bitint-from-blank-index** nil)  ;; bitint with 1's for cells containing blanks
;;; Note: use **piece-bit-int-array** to get covered-cells-bitint from type cellnum
(defparameter **blank-index-place-value** nil)      ;; place-val for blank-index in jimslide-integer cod
(defparameter **final-shift** nil)           ;; used in jimslide-compress

;;; for EXT-ASTAR-SBP
;;;    for H-fun
(defparameter **t-piece-type** 0)
(defparameter **t-piece-manhattan-distance** nil)
(defparameter **t-piece-hfun-component** nil)
(defparameter **blanks-mask-lists** nil)

;;; TEMP FOR DATA COLLECTION
(defparameter **collect-type-and-gil-hash-data?** nil)
(defparameter **piece-type-move-counts** nil)
(defparameter **gil-hash-parent** nil)
(defparameter **gil-hash-child** nil)
(defparameter **gil-hash-ignore-types** nil)
(defparameter **same-gil-hash** 0)
(defparameter **diff-gil-hash** 0)

;; for reduced search (limiting piece moves as if had only a smaller number of blanks)
(defparameter *move-displacement-limits* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DefVar's from old sliding-block puzzle solver code (near end of file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *solution-target*)
(defvar *fringe-target* )
(defvar *start-pos* )
(defvar *start-list* )
(defvar *num-piece-types* )
(defvar *move-array* )
(defvar *moves-by-dir* )
(defvar *next-cell-by-dir* )
(defvar *move-type* )
(defvar *cell-from-coords* )
(defvar *coords-from-cell* )
(defvar *all-solutions* )
(defvar *solution-list* )
(defvar *first-solution* )
(defvar *num-cells* )
(defvar *piece-types* )
(defvar *linearity-of-piece* )
(defvar *prim-move-translations* )
(defvar *prim-move-horizontal-translations* )
(defvar *prim-move-vertical-translations* )
(defvar *display-array* )
(defvar *periodic-fringe-save-list* )
(defvar *no-printing* )
(defvar *distant-positions* )
(defvar *hash-table-pool-stats* )
(defvar *position-pool-stats* )
(defvar *fringes-to-reclaim-later* )
(defvar *exit-row* )
(defvar *suspend-search* )
(defvar *rush-hour-level* )
(defvar *start-positions-list*)  ;; piece positions for start board, (type row col)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SBP INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sbp-exper (puzzle-selector exper-tag)
  (sbp-init puzzle-selector)
  (setf **exper-tag** exper-tag
	**heap-threshold** 5     ;; Will default to NIL (bad!) if don't use RUN-EXPERIMENT)
	;;**use-parity?** nil
	;;**force-full-dup-elim-interval** nil
	;;**prior-fringe-lookup-limit** 2) ;; This is IMPORTANT for SBP efficiency
	)
  (setf	**puzzle-directory-name**
	(if **exper-tag**
	    (format nil "~a-~a" **puzzle-name** **exper-tag**)
	    **puzzle-name**)))

(defun sbp-init (puzzle-selector)   ; e.g. climb15a
  ;; run selected slide-puzzle-inits function
  (case puzzle-selector
    (block10-v11  (block10-v11-init))
    (block10-v12  (block10-v12-init))
    (climb12   (climb12-init))
    (climb12-2-singletons (climb12-2-singletons-init))
    (climb12-no-singletons (climb12-no-singletons-init))
    (climb15a  (climb15a-init))
    (climb15a-2-singletons  (climb15a-2-singletons-init))
    (climb15a-no-singletons  (climb15a-no-singletons-init))
    (climb15a-b3 (climb15a-b3-init))  ;; only 3 blanks
    (climb24 (climb24-init))
    (climb24-b3 (climb24-b3-init))
    (mini-climb-pro (mini-climb-pro-init))
    (henderson-7x7 (henderson-7x7-init))
    (puzzle-beast (puzzle-beast-init))
    (puzzle-ii (puzzle-ii-init))   ;; has exact target
    (t (error "unrecognized puzzle selector: ~a" puzzle-selector)))
  (setup-h-fun puzzle-selector)
  ;; init FSE interface globals
  (setf **puzzle-name** (format nil "~a" puzzle-selector))

  #|
  ;; old byte-code position-size
  (setf **position-size** 
  (loop for integer across *start-pos* sum (logcount integer)))   ;; this allocates bytes for blanks which are needed!
  |#
  
  (setf **num-blanks**     	;  should be 4 for the Climb-N (n = 12, 15a, 24) puzzles
	(logcount (elt *start-pos* (1- (length *start-pos*)))))
  ;; record max of blank-index range!
  (setf **blank-index-range**
	(choose *num-cells* **num-blanks**))
  ;; record maximum piece-type count
  (setf **max-piece-count**
        (loop for val across *start-pos*
           maximize (logcount val)))
  ;; setup **choose-array** for calls to FAST-CHOOSE
  (setf **choose-array**
        ;;(precompute-choose *num-cells* **num-blanks**)
        (precompute-choose *num-cells* **max-piece-count**))
  ;; compute # bytes needed to store blank-index
  (setf **blank-index-bytes**
	(ceiling (log (1- **blank-index-range**) 256)))
  ;;; for jimslide-compresed-positions
  (setf **byte-size** 32)
  (setf **position-size**
	(1+ (ceiling (* 4 **num-piece-instances**)   ;; add 1 to allow blank-index
		     32)))  ;; for 32-bit bytes
  (setf **final-shift**
	(final-shift **num-piece-instances** **byte-size**))
  #|
  ;; shouldn't need for jimslide
  ;; decrement (or adjust) **position-size** to reflect move from cell-bytes to index-bytes
  (setf **position-size**
  (+ **position-size**
  (- **num-blanks**)    ;; subtract blank byte locations
  **blank-index-bytes**))  ;; add in bytes to hold blank-index
  (setf **piece-type-offsets**
  (loop for previous-bytes = 0 then (+ previous-bytes (logcount integer))
  for integer across *start-pos*
  collect
  previous-bytes))
  (setf **piece-type-offsets-vector**
  (make-array (length **piece-type-offsets**)
  :initial-contents **piece-type-offsets**))
  (setf **piece-type-counts**
  (make-piece-type-counts-vector (append **piece-type-offsets** (list **position-size**))))
  (setf **blanks-offset** (first (last **piece-type-offsets**)))    ;; last offset (and last piece-type) is blanks
  (setf **sbp-position-register**       ;; make with fill-pointer? so can write to with vector-push ?
  (make-array **position-size** :element-type '(unsigned-byte 8) :fill-pointer t))
  |#
  ;; for jimslide support
  (setf **intermediate-position**    ;; treated as a "register" vector
	(make-array *num-cells* :element-type '(signed-byte 8)))
  (setf **sbp-position-register**       ;; maybe still use fill-pointer? so can write to with vector-push ?
        (make-array **position-size** :element-type '(unsigned-byte 32)))
  #|
  (setf **sbp-domain-position-register-aux**   ;; target for uncompressing positions
  (make-array *position-size* :element-type 'integer :fill-pointer t))  ;; NOTE: this is a DIFFERENT "position-size" -- note single *'s (it's num-piece-types + 1)
  |#
  (setf **sbp-domain-position-register-aux**   ;; target for uncompressing positions
	;; remove fill-pointer
        (make-array *position-size* :element-type 'integer))  ;; NOTE: this is a DIFFERENT "position-size" -- note single *'s (it's num-piece-types + 1)
  (setf **compressed-solution-position** nil)     ;;  global (local to domain-code) for saving "private" copy of compressed-solution
  #|
  (setf **compressed-solution-target**
  (loop for (piece-type bit-pattern) in *solution-target*
  collect
  (list piece-type
  ;; was (aref **piece-type-offsets-vector** piece-type)
  (get-single-1-index-in-bit-pattern bit-pattern))))   ; warns if multple 1-bits
  |#
  (setf **compressed-solution-target** ;; list of (piece-type cell-num)
        (loop for (piece-type bit-pattern) in *solution-target*
           append
             (loop for index from 0 below (integer-length bit-pattern)
                when (logbitp index bit-pattern)
                collect
                  (list piece-type index))))
  (setf **start-pos-with-blank-index**   ;; a domain position, with blank-index replacing blank-bits
	(let* ((start-pos-copy (copy-seq *start-pos*))
	       (start-pos-length (length *start-pos*))
	       (start-pos-blank-bits (aref *start-pos* (1- start-pos-length)))
	       (blanks-index (integer-index start-pos-blank-bits *num-cells* **num-blanks**)))
	  (setf (aref start-pos-copy (1- start-pos-length))
		blanks-index)
	  start-pos-copy))
  (setf **start-domain-pos-list**
        (list **start-pos-with-blank-index**))  ;; was (list *start-pos*)
  ;; side-effect fills **intermediate-position**
  (jimslide-intermediate-position-from-domain-pos-with-blank-index
   **start-pos-with-blank-index**)
  (jimslide-compress)  ;; compress **intermediate-position** -> **sbp-position-register**
  (setf **start-pos-list**   ;; this is the start-list shared with search
                                        ;(mapcar #'compress-position-index **start-domain-pos-list**)   ;; should only be 1 start-pos
	(list (copy-seq **sbp-position-register**))
	)
  ;; signed-bytes (-1 = no type val)
  ;;; Finally, generate move-spec array for blank-indexing
  ;;;   Reuse if already computed
  (unless (= (length **move-specs-for-blank-pattern**)
	     (choose *num-cells* **num-blanks**))
    (compute-move-spec-array)   ;; NOTE: this sets the global **move-specs-for-blank-pattern**
    )
  **start-pos-list**    ;; for compatibility with old code in terms of value return (cosmetic)
  )


;;; this compression function used for initialization
;;;    note: assumes that the bitint for blanks is already a blank-index
(defun compress-position-index (domain-pos &optional (byte-vector-wfp **sbp-position-register**))   ;; wfp = "with fill-pointer"
  (setf (fill-pointer byte-vector-wfp) 0)
  (loop with blank-type = *num-piece-types*
     for piece-type from 0
     for bit-int across domain-pos
        do
       (cond ((= piece-type blank-type)   ;; for blanks treat bit-int as a blank-index
	      (incf (fill-pointer byte-vector-wfp) **blank-index-bytes**)  ;; make room for index bytes
	      (write-integer-as-bytes bit-int byte-vector-wfp :start **blanks-offset**))
	     (t
	      (loop for bit-index from 0 to (integer-length bit-int)
		 when
		   (logbitp bit-index bit-int)
		 do
		   (vector-push bit-index byte-vector-wfp)))))
  byte-vector-wfp)

(defun compress-position-insert-blank-index (domain-pos &optional (byte-vector-wfp **sbp-position-register**)) ;; wfp = "with fill-pointer"
  (setf (fill-pointer byte-vector-wfp) 0)
  (loop with blank-type = *num-piece-types*
     for piece-type from 0
     for bit-int across domain-pos
     do
       (cond ((= piece-type blank-type)	;; for blanks treat bit-int as a blank-index
	      (incf (fill-pointer byte-vector-wfp) **blank-index-bytes**) ;; make room for index bytes
	      (write-integer-as-bytes (integer-index bit-int *num-cells* **num-blanks**) ;; get blank-index
				      byte-vector-wfp
				      :start **blanks-offset**))
	     (t
	      (loop for bit-index from 0 to (integer-length bit-int)
		 when
		   (logbitp bit-index bit-int)
		 do
		   (vector-push bit-index byte-vector-wfp)))))
  byte-vector-wfp)

(defun uncompress-position-with-blank-index (byte-pos 
					     &optional
					     (domain-pos-register **sbp-domain-position-register-aux**))
  (setf (fill-pointer domain-pos-register) 0)
  (loop for piece-type-offset across **piece-type-offsets-vector**
        for piece-byte-count across **piece-type-counts**
        for piece-bits = (if (= piece-type-offset **blanks-offset**)
			     ;; extract index and convert to bits
			     ;; FIX THIS:
			     (integer-from-index (extract-index-from-byte-seq byte-pos) *num-cells* **num-blanks**)
			     (extract-bit-int byte-pos piece-type-offset piece-byte-count))
        do
       (vector-push piece-bits domain-pos-register)))




;;; SOLVED?
(defun solved? (&optional (pos **intermediate-position**))   ;; pos now is (an) intermediate-position
  ;; currently assumes **compressed-solution-target** specifies target
  ;;  NOTE: this won't work reliably if target piece-type occurs multiple times
  (loop for (piece-type cell-num) in **compressed-solution-target**
     always
       (= (aref pos cell-num)
	  piece-type)))

;;; old solved
#|
(defun solved? (pos)   ;; pos is expected to be a compressed position
  ;; currently assumes **compressed-solution-target** specifies target
  ;;  NOTE: this won't work reliably if target piece-type occurs multiple times
  (loop for (byte-index byte-value) in **compressed-solution-target**
     always
       (= (aref pos byte-index)
	  byte-value)))
|#


;;; DOMAIN FUNCTION

;;; For Ext-AStar-SBP, output-buffer = (h-max A0 A1 A2) where Ai are hash-tables
;;;    h-value computed from **intermediate-position** and used to select Ai for storing position
(defun generate-successors (position output-buffer
                            &optional h-cutoff? (check-solved? **check-solved?**))
  ;; position is expected to be a byte-position in register:  **sbp-position-register**
  ;; OOPS - its not in that register!  [it's actually a front-position of an input buffer - better not modify it!]
  (setf **compressed-solution-position** nil) ;; reset for case of continuous search
  ;; uncompress position
  (jimslide-uncompress position) ;; target is **intermediate-position** and **intermediate-blank-index**
  ;; don't expand unless position satisfies h-cutoff?
  (when (or (null h-cutoff?)            ;; no h-cutoff?
            (<= (if **target-position** ;; or base-h-fun value <= h-cutoff?
                    (fast-manhattan-move-h-fun)
                    (t-piece-h-fun))
                h-cutoff?))
    ;; compute Gil-Hash
    (when **collect-type-and-gil-hash-data?**
      (record-gil-hash **gil-hash-parent**))
    (loop ; with source-blank-index = (extract-index-from-byte-seq position) ; now **intermediate-blank-index**
       with (h-max A0 A1 A2) =  output-buffer
       for (piece-type . piece-type-specs) in (aref **move-specs-for-blank-pattern** **intermediate-blank-index**)
       do
         (loop for (moved-from . moved-to-index-pairs) in piece-type-specs
            when ; (logbitp moved-from piece-type-bits)   ;; there is a piece there, so is legal move!
              (= (aref **intermediate-position** moved-from) ;; type of piece at position moved-from
                 piece-type) ;; must match piece-type from move-spec
            do
              (setf (aref **intermediate-position** moved-from) -1) ;; remove moving piece
            ;; update piece-type-move-counts
              (when **collect-type-and-gil-hash-data?**
                (incf (aref **piece-type-move-counts** piece-type)
                      (length moved-to-index-pairs)))
              (loop for (moved-to . new-blanks-index) in moved-to-index-pairs
                 do
                 ;; make-move
                   (setf (aref **intermediate-position** moved-to) piece-type)
                 ;; compress with NEW BLANK-INDEX
                   (jimslide-compress    ;; compresses to **sbp-position-register** 
                    new-blanks-index)    ;;    using new-banks-index
                 ;; check if solved
                   (when (and check-solved?
                              (solved?)) ;; checks **intermediate-position**
                     (setf **compressed-solution-position**
                           (copy-seq **sbp-position-register**)))
                 ;; write successor to output buffer
                 ;; (print "SUCCESSOR:")
                 ;; (fancy-display-compressed-position **sbp-position-register**)
                 ;; (write-position output-buffer **sbp-position-register**)
                   (let* ((succ-base-h (if **target-position**
                                           (fast-manhattan-move-h-fun)
                                           (t-piece-h-fun new-blanks-index
                                                          **intermediate-position**)))
                          (succ-h (* succ-base-h **h-scale**))
                          (succ-pos **sbp-position-register**)) ;; don't need to copy with write-to-file
                     (when (or (null h-cutoff?)
                               (< succ-base-h h-cutoff?))   ;; inequality since succ is already g+1
                       ;; valid successor
                       ;;  update gil-hash stats
                       (when **collect-type-and-gil-hash-data?**
                         (record-gil-hash **gil-hash-child**)
                         (if (equalp **gil-hash-child**
                                     **gil-hash-parent**)
                             (incf **same-gil-hash**)
                             (incf **diff-gil-hash**)))
                       ;; write to appropriate output-buffer
                       (let ((x (- succ-h h-max)))
                         (cond ((> x 0) (write-position A2 succ-pos))
                               ((= x 0) (write-position A1 succ-pos))
                               ((< x 0) (write-position A0 succ-pos))))
                       (inc-counter 'all-successors)))
                   (when **debug**
                     (format t "~% called (inc-counter 'all-successors) new count = ~a"
                             (get-counter 'all-successors)))
                 ;; undo-move-to -- remove moved-to entry
                   (setf (aref **intermediate-position** moved-to) -1)
                   )
              (setf (aref **intermediate-position** moved-from) piece-type) ;; reset moving piece
              )
         ))
  ;; return solution if 1 has been found
  (cond (**compressed-solution-position**
         **compressed-solution-position**) ;; return compressed solution, if one found (already copied above when set)
        (t nil)))                          ;; otherwise NIL


;; This returns a list of successors of position
;;   It is used specifically in solution recovery
(defun collect-successors (position)
  ;; position is expected to be a byte-position in register:  **sbp-position-register**
  ;; OOPS - its not in that register!  [it's actually a front-position of an input buffer - better not modify it!]
  ;; uncompress position
  (jimslide-uncompress position) ;; target is **intermediate-position** and **intermediate-blank-index**
  (loop ; with source-blank-index = (extract-index-from-byte-seq position) ; now **intermediate-blank-index**
     ;;with (h-max A0 A1 A2) =  output-buffer
     with successor-list = nil 
     for (piece-type . piece-type-specs) in (aref **move-specs-for-blank-pattern** **intermediate-blank-index**)
     do
       (loop for (moved-from . moved-to-index-pairs) in piece-type-specs
	  when ; (logbitp moved-from piece-type-bits)   ;; there is a piece there, so is legal move!
	    (= (aref **intermediate-position** moved-from) ;; type of piece at position moved-from
	       piece-type) ;; must match piece-type from move-spec
	  do
	    (setf (aref **intermediate-position** moved-from) -1) ;; remove moving piece
	    (loop for (moved-to . new-blanks-index) in moved-to-index-pairs
	       do
	       ;; make-move
		 (setf (aref **intermediate-position** moved-to) piece-type)
	       ;; compress with NEW BLANK-INDEX
		 (jimslide-compress new-blanks-index) ;; compresses to **sbp-position-register**
	       ;;     using new-banks-index

	       ;; Copy and collect position **sbp-position-register**
		 (push (copy-seq **sbp-position-register**)
		       successor-list)
		 
	       ;; undo-move-to -- remove moved-to entry
		 (setf (aref **intermediate-position** moved-to) -1)
		 )
	    (setf (aref **intermediate-position** moved-from) piece-type) ;; reset moving piece
	    )
     finally
       (return successor-list)
       )
  )

;;; DATA COLLECTION [piece-type moves and Gil-Hash data]

(defun initialize-data-collection (puzzle-name-string &optional collect-data?)
  (setf **collect-type-and-gil-hash-data?**
        collect-data?)
  (when collect-data?
    (let ((puzzle-name (read-from-string puzzle-name-string)))
      (print puzzle-name)
      (setf **same-gil-hash** 0
            **diff-gil-hash** 0)
      (setf **piece-type-move-counts**
            (make-array *num-piece-types* :initial-element 0))
      (setf **gil-hash-ignore-types**
            (gil-hash-ignore-types puzzle-name 2)   ;; type 2 (or change to 1)
            )
      (setf **gil-hash-length**
            (compute-gil-hash-length))
      (setf **gil-hash-parent**
            (new-gil-hash-sequence))
      (setf **gil-hash-child**
            (new-gil-hash-sequence))
      )))

(defun new-gil-hash-sequence ()
  (make-array **gil-hash-length**
              :element-type '(signed-byte 4)
              :fill-pointer t))

(defun gil-hash-ignore-types (puzzle-name &optional (gil-hash-type 2))
  (case gil-hash-type
    (1 (gil-hash-ignore-types-1 puzzle-name))
    (2 (gil-hash-ignore-types-2 puzzle-name))
    (t (warn "Unrecognized gil-hash-type ~a" gil-hash-type))))

(defun gil-hash-ignore-types-1 (puzzle-name)
  (case puzzle-name
    (climb24 '(-1 ;; empty-value of intermediate position
               6  ;; 2x1
               7  ;; 1x2
               9  ;; 1x1
               ;; blanks not specified so no need to omit
               ))
    (climb15a '(-1 ;; empty-value of intermediate position
                5  ;; 2x1
                6  ;; 1x2
                8  ;; 1x1
                ;; blanks not specified so no need to omit
                ))
    (climb12 '(-1 ;; empty-value of intermediate position
               2  ;; 2x1
               3  ;; 1x2
               5  ;; 1x1
               ;; blanks not specified so no need to omit
               ))
    (t (warn "Unrcognized Puzzle Name"))
    ))

(defun gil-hash-ignore-types-2 (puzzle-name)
  (case puzzle-name
    (climb24 '(-1 ;; empty-value of intermediate position
               3  ;; SE
               4  ;; SW
               6  ;; 2x1
               7  ;; 1x2
               9  ;; 1x1
               ;; blanks not specified so no need to omit
               ))
    (climb15a '(-1 ;; empty-value of intermediate position
                3  ;; SE
                4  ;; SW
                5  ;; 2x1
                6  ;; 1x2
                8  ;; 1x1
                ;; blanks not specified so no need to omit
                ))
    (climb12 '(-1 ;; empty-value of intermediate position
               1  ;; SE
               2  ;; 2x1
               3  ;; 1x2
               5  ;; 1x1
               ;; blanks not specified so no need to omit
               ))
    (t (warn "Unrcognized Puzzle Name"))
    ))

#|
;; modify for gil-hash-2
(defun initialize-data-collection-2 (puzzle-name-string)
  (let ((puzzle-name (read-from-string puzzle-name-string)))
    (print puzzle-name)
    (setf **same-gil-hash** 0
          **diff-gil-hash** 0)
    (setf **piece-type-move-counts**
          (make-array *num-piece-types* :initial-element 0))
    (setf **gil-hash-ignore-types**
          (case puzzle-name
            (climb24 '(-1 ;; empty-value of intermediate position
                       3  ;; SE
                       4  ;; SW
                       6  ;; 2x1
                       7  ;; 1x2
                       9  ;; 1x1
                       ;; blanks not specified so no need to omit
                       ))
            (climb15a '(-1 ;; empty-value of intermediate position
                        3  ;; SE
                        4  ;; SW
                        5  ;; 2x1
                        6  ;; 1x2
                        8  ;; 1x1
                        ;; blanks not specified so no need to omit
                        ))
            (climb12 '(-1 ;; empty-value of intermediate position
                       1  ;; SE
                       2  ;; 2x1
                       3  ;; 1x2
                       5  ;; 1x1
                       ;; blanks not specified so no need to omit
                       ))
            (t (warn "Unrcognized Puzzle Name"))
            )          )
    (setf **gil-hash-length**
          (compute-gil-hash-length))
    (setf **gil-hash-parent**
          (new-gil-hash-sequence))
    (setf **gil-hash-child**
          (new-gil-hash-sequence))
    ))
|#

(defun compute-gil-hash-length ()
  (jimslide-uncompress **init-position**)
  (loop for val across **intermediate-position**
     count (not (member val **gil-hash-ignore-types**))
       ))

(defun record-gil-hash (target)   ;; target is **gil-hash-parent** or **gil-hash-child** 
  ;; operates on sequence in  **intermediate-position**
  (setf (fill-pointer target)
        0)
  (loop for val across **intermediate-position**
     unless (member val **gil-hash-ignore-types**)
     do
       (vector-push val target)))
        
(defun report-gil-hash-counts ()
  (format t "~%  Gil-Hash locality:  ~f~%    Same: ~a~%    Diff: ~a"
          (unless (zerop (+ **same-gil-hash**
                            **diff-gil-hash**))
            (float (/ **same-gil-hash**
                      (+ **same-gil-hash**
                         **diff-gil-hash**))))
          **same-gil-hash**
          **diff-gil-hash**))

;;; T-PIECE H-FUN
(defun t-piece-h-fun (&optional
                        (new-blanks-index **intermediate-blank-index**)
                        (intermediate-pos **intermediate-position**)
                        (t-piece-type **t-piece-type**))
  (loop for cell-index from 0
     for type-val across intermediate-pos
     until (= type-val t-piece-type)
     finally
                                        ;(print (list type-val cell-index))
       (return 
         (+ (aref **t-piece-hfun-component** cell-index)
            (t-piece-hfun-blanks-offset cell-index
                                        (aref **cells-bitint-from-blank-index**
                                              new-blanks-index))
            ))))

(defun t-piece-hfun-blanks-offset (t-cellnum blank-bits)
  (loop with (default-val . mask-form-list) = (aref **blanks-mask-lists** t-cellnum)
     for (mask-bits count val) in mask-form-list
     when (<= count (logcount (logand blank-bits
				      mask-bits)))
     return val
     finally
       (return default-val)))


;;; Call this during init
(defun setup-h-fun (puzzle-selector)
  (setf **t-piece-type**
	;; get t-type from *solution-target*
	(first (first *solution-target*)))
  (setf **t-piece-hfun-component**
	(make-array *num-cells* :element-type '(signed-byte 8)))
  (setf **blanks-mask-lists**
	(make-array *num-cells*))
  (loop for cellnum from 0 below *num-cells*
     for t-piece-distance = (t-piece-row-distance cellnum 0)
     for blanks-mask-list = (blanks-mask-list-for-t-cell cellnum)
     do
       (setf (aref **blanks-mask-lists** cellnum)
	     blanks-mask-list)
       (setf (aref **t-piece-hfun-component** cellnum)
	     t-piece-distance)))

(defun blanks-mask-list-for-t-cell (cellnum)
  (cond ((= 0 cellnum) ;; solved special case
	 (list 0))     ;; default-val = 0, mask-list empty
	(t
	 (let* ((default-val 1)
		(board-width (second (array-dimensions *cell-from-coords*)))
		(below-t-mask (loop with start-cell = (+ 1
							 (* (floor (1- cellnum) board-width)
							    board-width)
							 board-width)
				 for cell from start-cell below *num-cells*
				 sum
				   (ash 1 cell)))
		(above-t-mask (manhattan-dist-above-mask 5 cellnum))
		(immed-below-t-mask (mask-from-row-col-offsets cellnum '((2 -1)(2 0)(2 1))))
		(immed-above-t-mask (mask-from-row-col-offsets cellnum '((-1 0)(0 -1)(0 1))))
		(immed-right-t-mask (mask-from-row-col-offsets cellnum '((-1 1)(0 1)(0 2)(1 2))))
		(immed-left-t-mask (mask-from-row-col-offsets cellnum '((-1 -1)(0 -1)(0 -2)(1 -2))))
		(cand-mask-list (list (list immed-below-t-mask 3 3)
				      (list below-t-mask 3 2)
				      (list immed-above-t-mask 3 0)
				      (list immed-right-t-mask 4 0)
				      (list immed-left-t-mask 4 0)
				      (list above-t-mask 2 2))))
	   (cons default-val
		 (loop for mask-form in cand-mask-list
		    for (mask count) = mask-form
		    when (<= count (logcount mask))
		    collect mask-form))))))

(defun mask-from-row-col-offsets (t-cellnum row-col-offset-list)
  (loop with default-mask = 0
     with mask = 0
     with (trow tcol) = (aref *coords-from-cell* t-cellnum)
     for (drow dcol) in row-col-offset-list
     for nrow = (+ trow drow)
     for ncol = (+ tcol dcol)
     for ncell = (and (array-in-bounds-p *cell-from-coords* nrow ncol)
		      (aref *cell-from-coords* nrow ncol))
     do
       (if ncell
	   (incf mask (ash 1 ncell))
	   (return default-mask))
     finally
       (return mask)))
  
(defun manhattan-dist-above-mask (dist cell)
  (loop with (row col) = (aref *coords-from-cell* cell)
     with mask = 0
     for newcell from 0 below cell
     for (nrow ncol) = (aref *coords-from-cell* newcell)
     for mh-dist = (+ (abs (- row nrow))
		      (abs (- col ncol)))
     when (and (< nrow row)    ;; newcell is above cell
	       (>= mh-dist dist))
     do
       (incf mask (ash 1 newcell))
     finally
       (return mask)))

;; spreads by factor of 4, but 0 if cells are equal
(defun t-piece-row-distance (cell1 cell2)
  (if (= cell1 cell2)
      0
      (let* ((row-col-1 (aref *coords-from-cell* cell1))
	     (row-col-2 (aref *coords-from-cell* cell2))
	     (row1 (first row-col-1))
	     (row2 (first row-col-2))
	     (row-distance (abs (- row1 row2)))
	     ;;(col1 (second row-col-1))
	     ;;(col2 (second row-col-2))
	     )
	(if (zerop row-distance)
	    row-distance
	    (+ 1 (* 4 (1- row-distance)))))))


;;; Utilities

(defun make-piece-type-counts-vector (piece-type-offsets-plus-size)    ;; this is the list version of offsets augmented by appending an element for the "offset" of the end: **position-size**
  (let ((piece-type-counts-list
	 (loop
	    for piece-type-offset in piece-type-offsets-plus-size
	    for next-piece-type-offset in (cdr piece-type-offsets-plus-size)
	    collect
	      (- next-piece-type-offset
		 piece-type-offset))))
    (make-array (length piece-type-counts-list)
		:initial-contents
		piece-type-counts-list)))

(defun get-single-1-index-in-bit-pattern (bit-pattern)
  (cond ((zerop (logcount bit-pattern))
	 (error "There are no 1-bits in bit-pattern: ~a" bit-pattern))
	((> (logcount bit-pattern) 1)
	 (warn "There are multiple 1-bits in bit-pattern: ~a" bit-pattern))
	(t
	 (loop for i from 0 below *num-cells*
	    when (logbitp i bit-pattern)
	    return i
	    finally
	    (return 0)))))
	      
	

(defun to-binary (int)
  (format nil "~B" int))

;; jimslide version
(defun fancy-display-compressed-position (byte-pos)
  (jimslide-uncompress byte-pos)  ;; puts in **intermediate-position**
  (jimslide-domain-position-from-intermediate-position) ;; puts in **sbp-domain-position-register-aux**
  ;; (uncompress-position-with-blank-index byte-pos  **sbp-domain-position-register-aux**)
  (fancy-display-position **sbp-domain-position-register-aux**))

;; old code
#|
(defun fancy-display-compressed-position (byte-pos)
  (uncompress-position-with-blank-index byte-pos  **sbp-domain-position-register-aux**)
  (fancy-display-position **sbp-domain-position-register-aux**))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; START of merged/copied  BLANK-INDEXING-2.lisp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combined files to make compiling easier



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Indexing Patterns of Blanks
;;;    Convert a k-bit integer with exactly k 1's and rest 0's
;;;       into an index I so with range:   0 <= I <  Choose(n,k)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun choose (n k)
  (loop with result = 1
     for i downfrom n
     for j from 1 to k
     do
       (setf result (/ (* result i) j))
     finally
       (return result)))

(defun integer-index-recursive (integer  ;; n-bit integer with exactly k 1's
		      n     ;; exact number of bits in this n-bit integer (with 0's padding on left if needed)
		      k)    ;; how many of the bits are 1's
  ;;; note there are a total of (choose n k)  such integers to index
  ;;;    imagine sorting bit-strings so that low-order 0's come before 1's
  (cond ((zerop k)
	 0)
	((oddp integer)   ;; low bit is a 1
	 (+ (fast-choose (1- n)    ;; total number of integers having 0 instead of 1 (these are "skipped over")
		    k)
	    (integer-index (floor integer 2)   ;; throw away low-order bit
			   (1- n)
			   (1- k))))
	(t   ;; integer is even
	 (integer-index (floor integer 2)   ;; throw away low-order bit
			(1- n)         ;; 1 less bit left in integer
			k))))              ;; still k 1-bits remaining

;;; now seems to work!
;;; Iterative version (only slightly faster than recursion)
(defun integer-index (integer  ;; n-bit integer with exactly k 1's
		      n1     ;; exact number of bits in this n-bit integer (with 0's padding on left if needed)
		      k1)    ;; how many of the bits are 1's
  ;;; note there are a total of (choose n k)  such integers to index
  ;;;    imagine sorting bit-strings so that low-order 0's come before 1's
  (loop 
     for k = k1 then (if 1-bit? (1- k) k)    ;; remaining 1-bits
     for bit-index from 0 below n1
     for 1-bit? = (logbitp bit-index integer)
     for n downfrom (1- n1)
     until (zerop k)
     when (logbitp bit-index integer)
     sum 
       (fast-choose n k)))



(defun integer-from-index (index n k)
  (let ((index-threshold (fast-choose (1- n) k)))   ;; how many integers have 0 in low-order position
    (cond ((zerop k) 0)   ;; no 1 bits, so integer is 0
	  ((= n k)  (1- (expt 2 k)))  ;; integer must be exactly n = k 1's
	  ((< index index-threshold)    ;; low-order bit is 0
	   (* 2 (integer-from-index index (1- n) k)))
	  (t ;; low-order-bit is 1
	   (1+ (* 2
		  (integer-from-index (- index index-threshold) ;; reduced index (less count of low-order 0)
				      (1- n)
				      (1- k))))))))

;;; FIX THIS - ITERATE INSTEAD OF RECURSE ?!
(defun integer-from-index-iter (index n k)
  (let ((index-threshold (fast-choose (1- n) k)))   ;; how many integers have 0 in low-order position
    (cond ((zerop k) 0)   ;; no 1 bits, so integer is 0
	  ((= n k)  (1- (expt 2 k)))  ;; integer must be exactly n = k 1's
	  ((< index index-threshold)    ;; low-order bit is 0
	   (* 2 (integer-from-index index (1- n) k)))
	  (t ;; low-order-bit is 1
	   (1+ (* 2
		  (integer-from-index (- index index-threshold) ;; reduced index (less count of low-order 0)
				      (1- n)
				      (1- k))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating Blank-indexed MoveSpecs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MoveSpec:  (piece-type piece-from piece-to new-blanks)
;;;    Idea: blank-changes can be replaced with new-blanks (LOGXOR of indexed-blanks and blank-changes)
;;;    Eventually can delete piece-type, and collect (into a list,eg) all move-specs for same piece-type
;;;          (and of course tag that list with the piece-type)

;;; Plan:
;;;   Basically do a "generate-successors" type search for each blank pattern,
;;;       but "write out" (collect) legal move-specs instead of actual successors


(defun precompute-choose (n-max k-max)
  (let ((choose-array (make-array (list (1+ n-max) (1+ k-max)))))
    (loop for n from 0 to n-max
	 do
	 (loop for k from 0 to k-max
	      do
	      (setf (aref choose-array n k)
		    (choose n k))))
    choose-array))

(defun precompute-piece-bit-ints ()
  ;; compute bit-ints for each (piece-type cell-position) pair
  (let ((piece-bit-int-array (make-array (list *num-piece-types* *num-cells*))))
    (loop for piece-type from 0 below *num-piece-types*
	 for piece-cells in *piece-types*
	 do
	 (loop for cell from 0 below *num-cells*
	    for cell-coords = (aref *coords-from-cell* cell)
	    for trans-piece = (translate-piece piece-cells cell-coords)
	    when (on-board? trans-piece *cell-from-coords*)  ;; this will check for "obstacles", too
	    do
	      (setf (aref piece-bit-int-array piece-type cell)
		    (loop for (row col) in trans-piece
			 sum
			 (ash 1 (aref *cell-from-coords* row col))))))
    piece-bit-int-array))

(defun fast-choose (n k)
  (aref **choose-array** n k))

;;; NEW Move-spec format in SBP-3
;;;    Each entry of move-spec-array is a list of move-specs for a given type
;;;         each move-spec-array is a vector of 32-bit integers (unsigned)
;;;             where 1st element is the type and the rest are all individual move-specs
;;;                  where each individual move-spec is  [from-cell(6-bits), to-cell(6-bits), new-index(20-bits)]
(defun compute-move-spec-array ()
  (setf **piece-bit-int-array** 
	(precompute-piece-bit-ints))
  (setf **cells-bitint-from-blank-index**
	(make-array **blank-index-range**))   ;; assume **blank-index-range** already is set correctly
  (setf **move-specs-for-blank-pattern**
	;; (make-array (fast-choose *num-cells* **num-blanks**) :initial-element nil)
	(make-array **blank-index-range** :initial-element nil))
  (loop for blank-index from 0 below (length **move-specs-for-blank-pattern**)
     for blank-pattern = (integer-from-index blank-index *num-cells* **num-blanks**)
     do
       (setf (aref **cells-bitint-from-blank-index** blank-index)
	     blank-pattern)    ;; store for later quick access in jimslide compression
       (setf **blanks-index** blank-index)   ;; not used in this file -- is it used elsewhere??
       (setf (aref **move-specs-for-blank-pattern** blank-index)
	     (move-specs-for-blank-pattern blank-pattern))))

;;; now returns list ((piece-type . move-specs-for-piece-type) ... (piece-type . move-specs-for-piece-type))
(defun move-specs-for-blank-pattern (blank-int)
  (loop for piece-type from 0 below *num-piece-types*
     for move-specs-for-piece-type = (move-specs-for-blanks-and-piece-type blank-int piece-type)
     when move-specs-for-piece-type
     collect
       move-specs-for-piece-type))

;;; In SBP-6 returns list (piece-type (from (to . ind) ...) ... (from (to . ind) ...))
;;;    First element of vector is piece-type
(defun move-specs-for-blanks-and-piece-type (blank-int piece-type)
  (loop with collected-move-forms = nil
     for cell from 0 below *num-cells*
     for move-specs-from-cell = (collect-move-specs-for-blanks-piece-type-and-cell blank-int piece-type cell)
     when move-specs-from-cell
     do
       (push move-specs-from-cell collected-move-forms)
     finally
       (return (if collected-move-forms
		   (cons piece-type collected-move-forms)
		   nil))))

;;; Seems this could be set to handle just the maximum number of cells (eg 64 or less) - gai 8-12-13
(defparameter *local-seen-cell-marks*
  ;; assumes never get more that 100 cells
  (make-array 100 :element-type 'bit :initial-element 0))


;; In SBP-6 this returns a list: (from-cell (to-cell . new-blanks) .... (to-cell . new-blanks)) or niil
(defun collect-move-specs-for-blanks-piece-type-and-cell (start-blanks piece-type cell)
  (cond ((piece-covers-a-blank? piece-type cell start-blanks) ;; piece can never cover a blank
	 nil)
	(t 
	 (fill *local-seen-cell-marks* 0) ;; use array from SBP-2
	 (setf (sbit *local-seen-cell-marks* cell) 1) ;; discipline: when cell marked, that position has been queued for expansion
	 (loop with queue = *successor-search-queue*
	      with collected-pairs = nil
	    initially
	      (setf (fill-pointer queue) 0)
	      (vector-push cell
			   queue)
	      (vector-push start-blanks
			   queue)
	    until (zerop (fill-pointer queue)) ;; queue is empty
	    do
	      (loop with next-blanks = (vector-pop queue)
		 with next-cell = (vector-pop queue)
		 for (to-blanks from-to-cell blank-changes direction) in (aref *move-array* piece-type next-cell) ;old-move-spec
		 for cell-moved-to = (aref *next-cell-by-dir* next-cell direction) ; look ahead!
		 when (and (legal-bits? to-blanks
					next-blanks)
			   (zerop (aref *local-seen-cell-marks* cell-moved-to)))
		 do 
		   (let* ((new-blanks (logxor next-blanks blank-changes))) ;;  new-pos = (cell-moved-to new-blanks)
		     (setf (sbit *local-seen-cell-marks* cell-moved-to) ;; mark new-pos as seen
			   1)
		     ;; record new pair:
		     (push (cons cell-moved-to (integer-index new-blanks *num-cells* **num-blanks**))
			   collected-pairs)
		     ;; queue up new-pos for later expansion
		     (vector-push cell-moved-to
				  queue)
		     (vector-push new-blanks
				  queue)))
	    finally
	      (return (if collected-pairs
			  (cons cell collected-pairs)    ;; cell is the "from-celll"
			  nil))
	      ))))

(defun piece-covers-a-blank? (piece-type cell start-blanks)
  (not (zerop (logand start-blanks
		      (aref **piece-bit-int-array** piece-type cell)))))

;;;;;;;;;;;;;;;;
;;; MOVE-SPECS
;;;;;;;;;;;;;;;;

;; for jimslide -- maybe want to store blank-index as integer instead of bytes ??
;; YES -- Use 32-bit (unsigned) integer for move-spec
;;      ( 6-bits   from-cell
;;        6-bits   to-cell
;;       20-bits   next-blank-index )
;;  TODO: Fix this to derive field sizes from actual puzzle parameters
;;       and/or CHECK that values fit in the fixed-size fields
(defun make-move-spec (cell-from cell-to new-blanks-index)
  ;; make a vector to save space?
  (+ (ash cell-from 26)
     (ash cell-to 20)
     new-blanks-index)
  )


;; This only works for 3-byte indices  -- HMMM - seems it isn't called
(defun extract-index-from-move-spec (move-spec)
  (+ (aref move-spec 2)
     (ash (aref move-spec 3) 8)
     (ash (aref move-spec 4) 16)))
  
(defun extract-index-from-byte-seq (byte-seq 
				    &optional 
				    (start **blanks-offset**)
				    (num-bytes **blank-index-bytes**))
  (loop repeat num-bytes
     for index from start 
     for shift from 0 by 8
     sum
     (ash (aref byte-seq index) shift)))

(defun write-bit-int-to-vector (bit-int vector start)
  (loop
     with vec-index = start
     for bit-index from 0 below (integer-length bit-int)   ;; is "below" more accurate than "to"?
     when
       (logbitp bit-index bit-int)
     do
       (setf (aref vector vec-index) bit-index)
       (incf vec-index)))

(defun extract-bit-int (vector start length)
  (loop 
     for vec-index from start below (+ start length)
     for byte-val = (aref vector vec-index)
     sum (ash 1 byte-val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; utility - not called
(defun make-cells-int (list-of-cells)
  (loop with result = 0
     for cell in list-of-cells
     for cell-bit-int = (ash 1 cell)
     do
       (setf result (logior result cell-bit-int))
     finally
       (return result)))
  


;; only need these so can compile
(defparameter *rush-hour* nil)
(defparameter *careful-mode* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Block Sliding Problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; old unused code deleted


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Solutions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A piece-spec is a triple: ( piece-type row col)

(defun fringe-target (fringe-hash-set)
  (setf *fringe-target* fringe-hash-set))

(defun single-target-from-cell (piece-type cell)
  (setf *solution-target*
        (list (list piece-type (expt 2 cell)))))

(defun single-target-from-row-col (piece-type row col)
  (setf *solution-target*
        (list (list piece-type (expt 2 (aref *cell-from-coords* row col))))))

;;; Assumes that p1 and p2 not equal
(defun double-target-for-distinct-pieces-from-row-cols (p1 row1 col1 p2 row2 col2)
  (when (= p1 p2)
    (warn "double-target-for-distinct-pieces-from-row-cols requires 2 distinct pieces~%~
           received p1 = ~s, p2 = ~s"
          p1 p2))
  (setf *solution-target*
        (list (list p1 (expt 2 (aref *cell-from-coords* row1 col1)))
              (list p2 (expt 2 (aref *cell-from-coords* row2 col2))))))

(defun bad-rush-hour-target ()
  (single-target-from-row-col 0 0 0))

(defun target-from-position (position)
  (setf *solution-target*
        (loop for piece-type from 0 below *num-piece-types*
              collect
              (list piece-type (aref position piece-type)))))

(defun check-fringe-for-solutions (fringe-hash-table
                                   &optional
                                   continue-type?
                                   no-printing?)
  (declare (ignore continue-type?))
  (loop for pos being the hash-keys in fringe-hash-table
        do
        (check-for-solution pos nil no-printing?)))    ; NIL = "don't stop when solved"

(defun check-for-solution (pos &optional
                               (stop-when-solved? t) 
                               (no-printing? *no-printing*))
  (when (or (and *solution-target*
                 (loop for pair in *solution-target*
                       for piece-type = (first pair)
                       for bit-pattern = (second pair)
                       always
                       (legal-bits? bit-pattern
                                    (aref pos piece-type))))
            (and *fringe-target*
                 (gethash pos *fringe-target*)))
    (unless no-printing?
      (format t "~%~%solution found: ~a~%" pos))
    (unless *all-solutions*
      (setf *first-solution* (copy-position pos)))
    (push (copy-position pos) *all-solutions*)          ; avoid having it get reclaimed by accident
    (if stop-when-solved?
      (toplevel))))

(defun smart-recover-solution (&optional 
                               (final-position (first *all-solutions*))
                               (next-fringe-save-interval 1))
  (cond (*periodic-fringe-save-list*
         (recover-solution-from-fringe-list final-position 
                                            *periodic-fringe-save-list*
                                            next-fringe-save-interval))
        (t (error "Sorry, I can't do that Dave ..."))))

(defun recover-solution-from-fringe-list (&optional 
                                          (final-pos (first *all-solutions*))
                                          (fringe-list *periodic-fringe-save-list*)
                                          (next-fringe-save-interval 1))
  nil  ;; not needed except to compile
  )

(defun fringe-count (array-of-fringes)
  (loop for fringe across array-of-fringes
        for count from 0
        when (null fringe)
        return (1- count)
        finally
        (return count)))


;;; NEW INTERFACE FUNCTION FOR SOLUTION RECOVERY

;; not implemented
(defun domain-recover-solution (&optional compressed-solution-sequence
				  **compressed-solution-sequence**)     ;; computed by search-engine
  nil
  ;;(fancy-display-compressed-solution-sequence compressed-solution-sequence)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Making moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-move (piece-type move-spec position)
  (declare (simple-vector position)
           (fixnum piece-type))
  (let ((new-pos (copy-position position)))
    (declare (simple-vector new-pos))
    (setf (aref new-pos piece-type)
          (logxor (second move-spec)
                  (aref new-pos piece-type)))
    (setf (aref new-pos *num-piece-types*)
          (logxor (third move-spec)
                  (aref new-pos *num-piece-types*)))
    new-pos))

;;; make a macro? or in-line?
(defun legal-bits? (bit-pattern bit-seq)          ; T when bit-pattern is "covered" by bit-seq
  (zerop (logandc2 bit-pattern bit-seq)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Move Array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; move-array indexed by Piece-type and Piece Position
;;    entry: ( blank-precondition  delta-piece delta-blanks direction)
;; 
;; move-by-dir-array indexed by piece-type, piece-position, and direction
;;

(defun generate-move-arrays ()
  (when (and *careful-mode* *rush-hour*)
    (warn "Rush Hour mode is turned on!"))
  (let ((move-array (make-array (list *num-piece-types* *num-cells*)
                                :initial-element nil))
        (move-by-dir-array (make-array (list *num-piece-types* *num-cells* 4)
                                       :initial-element nil)))
    (loop for piece from 0 below *num-piece-types*
          do
          (loop for cell from 0 below *num-cells*
                for move-specs = (generate-moves piece cell)
                do
                (setf (aref move-array piece cell)
                      move-specs)
                (loop for move-spec in move-specs
                      for direction = (fourth move-spec)
                      do
                      (setf (aref move-by-dir-array piece cell direction)
                            move-spec))))
    (values move-array move-by-dir-array)))


(defun generate-moves (piece-num cell-num)
  (loop for (direction . move-spec) in (generate-list-moves piece-num cell-num)
        collect
        (nconc (mapcar #'bit-int-from-list
                      move-spec)
               (list direction))))

(defun generate-list-moves (piece-num cell-num)
  (let* ((cell-coords (aref *coords-from-cell* cell-num))
         (piece-cell-list (translate-piece (nth piece-num *piece-types*) cell-coords))
         (move-translations (if *rush-hour*
                              (prim-rush-hour-translations-from-piece-cell-list piece-cell-list)
                              *prim-move-translations*)))
    (if (on-board? piece-cell-list *cell-from-coords*)
      (loop for move-translation in move-translations
            for move-direction = (translation-direction move-translation)
            for moved-cell-list = (translate-piece piece-cell-list move-translation)
            when (on-board? moved-cell-list *cell-from-coords*)
            collect
            (cons move-direction
                  (let* ((moved-cell (translate-pair cell-coords move-translation))
                         (to-blanks (set-difference moved-cell-list
                                                    piece-cell-list
                                                    :test #'equal))
                         (from-blanks (set-difference piece-cell-list
                                                      moved-cell-list
                                                      :test #'equal))
                         (to-blank-indices (mapcar #'cell-from-coord-pair
                                                   to-blanks))
                         (from-blank-indices (mapcar #'cell-from-coord-pair
                                                     from-blanks)))
                    (list to-blank-indices
                          (list (cell-from-coord-pair cell-coords)
                                (cell-from-coord-pair moved-cell))
                          (append from-blank-indices
                                  to-blank-indices)))))
      nil)))

(defun translation-direction (translation)
  (position translation *prim-move-translations* :test #'equal))

(defun prim-rush-hour-translations-from-piece-cell-list (piece-cell-list)
  (cond ((horizontal? piece-cell-list)
         *prim-move-horizontal-translations*)
        ((vertical? piece-cell-list)
         *prim-move-vertical-translations*)
        (t *prim-move-translations*)))

(defun linear-piece? (piece-type)
  (let ((cell-list (nth piece-type *piece-types*)))
    (or (horizontal? cell-list)
        (vertical? cell-list))))
  
(defun horizontal? (piece-cell-list)
  (and (cdr piece-cell-list)
       (loop with row = (first (first piece-cell-list))
             for cell in (cdr piece-cell-list)
             always
             (= (first cell) row))))

(defun vertical? (piece-cell-list)
  (and (cdr piece-cell-list)
       (loop with col = (second (first piece-cell-list))
             for cell in (cdr piece-cell-list)
             always
             (= (second cell) col))))

(defun cell-from-coord-pair (pair)
  (aref *cell-from-coords* (first pair)(second pair)))

;; Checks cells are within array bounds, AND are all valid cells (non-NIL, ie NOT obstacles)
(defun on-board? (coord-list 2d-array)
  (loop for pair in coord-list
        for row = (first pair)
        for col = (second pair)
        always
        (and (array-in-bounds-p 2d-array row col)
             (aref 2d-array row col))))
                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-position-bits (position)
  (print position)
  (loop for i from 0 to *num-piece-types*
        do
        (format t "~%~b" (aref position i))))

(defun display-position (position)
  (print position)
  (loop for i from 0 to *num-piece-types*
        do
        (format t "~%~a" (listify-bits (aref position i)))))

(defun listify-bits (int)
  (loop for i from 0 below (integer-length int)
        when (logbitp i int)
        collect i))

(defun fancy-display-position (pos &optional (display-array *display-array*))
  (fill-with-blanks display-array)
  (loop for piece-type from 0 below *num-piece-types*
        for piece-bits = (aref pos piece-type)
        do
        (loop for cell from 0 below (integer-length piece-bits)
              when (logbitp cell piece-bits)
              do
              (plot-display-piece piece-type cell display-array)))
  (print-display-array display-array))

;; This should work to conveniently display *solution-target*
(defun fancy-display-target (&optional
			     (solution-target-list *solution-target*)
			     (display-array *display-array*))
  (fill-with-blanks display-array)
  (loop for (piece-type piece-bits) in solution-target-list
        do
        (loop for cell from 0 below (integer-length piece-bits)
              when (logbitp cell piece-bits)
              do
              (plot-display-piece piece-type cell display-array)))
  (print-display-array display-array))

(defun plot-display-piece (piece-type cell display-array)
  (loop with cell-coords = (aref *coords-from-cell* cell)
        for coord-pair in (translate-piece (nth piece-type *piece-types*)
                                           cell-coords)
        do
        (setf (aref display-array (first coord-pair)(second coord-pair))
              piece-type)))

(defun fill-with-blanks (display-array)
  (dotimes (cell (array-dimension *coords-from-cell* 0))
    (let ((cell-coords (aref *coords-from-cell* cell)))
      (setf (aref display-array (first cell-coords)(second cell-coords))
            #\.))))

(defun print-display-array (2d-array)
  (format t "~%~%")
  (dotimes (row (array-dimension 2d-array 0))
    (fresh-line)
    (dotimes (col (array-dimension 2d-array 1))
      (princ " ")
      (princ (aref 2d-array row col)))))

(defun generate-next-cell-by-dir-array ()
  (let ((next-cell-by-dir-array (make-array (list *num-cells*
                                                  (length *prim-move-translations*))
                                            :initial-element nil)))
    (loop for cell from 0 below *num-cells*
          do
          (loop with cell-coords = (aref *coords-from-cell* cell)
                for direction from 0 
                for translation in *prim-move-translations*
                for (row col) = (translate-pair cell-coords translation)
                when (array-in-bounds-p *cell-from-coords* row col)
                do
                (setf (aref next-cell-by-dir-array cell direction)
                      (aref *cell-from-coords* row col))))
    next-cell-by-dir-array))
                


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Puzzle Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; board-template  is a 2d-array
;;                      NIL = legal space
;;                       T  = obstacle
;; piece-types is a list of piece-type
;;    where a piece-type is a list of coord-pairs specifying 
;;                     the cells covered by the piece 
;;                     [ coordinates are relative to (0 0)
;;                          which specifies the "position" of the piece,
;;                       NOTE: The origin MUST BE one of the cells covered by the piece! ]
;; start-list-positions is a list of triples specifying the
;;                     start positions of the pieces,
;;                     a triple consisting of:
;;                          ( piece-type row col )


(defun slide-init (board-template piece-types start-positions-list)
  (setf *start-positions-list* start-positions-list)  ;; for translating to jimslide solver
  (setf *display-array* (copy-array board-template))
  (setf *cell-from-coords* (copy-array board-template))
  (setf *num-cells* (count-nulls board-template))
  (setf *coords-from-cell* (make-array *num-cells*))
  (cell-index-arrays *cell-from-coords* *coords-from-cell*)
  (setf *piece-types*
        piece-types)
  (setf *prim-move-translations*
        '((1 0)                         ; down
          (0 1)                         ; right
          (-1 0)                        ; up
          (0 -1)))                      ; left
  (setf *prim-move-horizontal-translations* 
        '((0 1)                         ; right
          (0 -1)))                      ; left
  (setf *prim-move-vertical-translations* 
        '((1 0)                         ; down
          (-1 0)))                      ; up
  (setf *num-piece-types* (length *piece-types*))          ; add 1 later for space
  (setf **num-piece-instances** (length start-positions-list))
  (when *rush-hour*
    (setf *linearity-of-piece*
          (make-array *num-piece-types* :initial-element nil))
    (loop for piece-type from 0 below *num-piece-types*
       when (linear-piece? piece-type)
       do
         (setf (aref *linearity-of-piece* piece-type) t)))
                                        ;(when *careful-mode* (print "about to initialize position pool..."))
                                        ;(init-position-pool (1+ *num-piece-types*))    ;; add 1 to reflect entry for blanks (note: this call sets *position-size*)
  (setf *position-size* (1+ *num-piece-types*))  ;;  need this since not using position-pool anymore
                                        ;(when *careful-mode* (print "done initializing position pool")) 
  (multiple-value-bind (move-array moves-by-dir)
      (generate-move-arrays)
    (setf *move-array* move-array 
          *moves-by-dir* moves-by-dir))
  (setf *next-cell-by-dir*
        (generate-next-cell-by-dir-array))
  (setf *start-pos*
        (compile-list-position (list-position-from-start-positions-list start-positions-list))))

(defun translate-pair (coord-pair translation)
  (mapcar #'+ coord-pair translation))

(defun translate-piece (cell-list translation)
  (loop for cell in cell-list
        collect
        (translate-pair cell translation)))

(defun basic-move-schema (cell-list translation)
  (let ((cell-list-to (translate-piece cell-list translation)))
    (list (list '(0 0) translation)       ; ( piece-from piece-to )
          (set-difference cell-list-to          ; spaces needed
                          cell-list
                          :test #'equal)
          (set-difference cell-list     ;  spaces created
                          cell-list-to
                          :test #'equal))))

(defun cell-index-arrays (2d-array 1d-array)
  (loop with index = 0
        for row from 0 below (array-dimension 2d-array 0)
        do
        (loop for col from 0 below (array-dimension 2d-array 1)
              do
              (cond ((aref 2d-array row col)
                     (setf (aref 2d-array row col)
                           nil))
                    (t
                     (setf (aref 2d-array row col) index)
                     (setf (aref 1d-array index) (list row col))
                     (incf index))))))

(defun position-from-start-list (start-list)
  (compile-list-position (list-position-from-start-positions-list start-list)))

(defun list-position-from-start-positions-list (start-positions-list)
  (let ((start-pos (make-array (1+ *num-piece-types*)
                               :initial-element nil)))
    (setf (aref start-pos *num-piece-types*)         ; initially all blanks
          (loop for i from 0 below *num-cells*
                collect i))
    (loop for piece-pos in start-positions-list
          for type = (first piece-pos)
          for row = (second piece-pos)
          for col = (third piece-pos)
          do
          (place-list-piece type row col start-pos *piece-types*))   ;  must be responsible to erase covered blanks
    start-pos))

(defun compile-list-position (list-position)
  (declare (simple-vector list-position))
  (let ((pos (if (= (length list-position) *position-size*)
               (new-domain-position)
               (copy-seq list-position))))
    (declare (simple-vector pos))
    (loop for i from 0 below (length list-position)
          do
          (setf (aref pos i)
                (bit-int-from-list (aref list-position i))))
    pos))

(defun place-list-piece (type row col list-position piece-list)
  (push (aref *cell-from-coords* row col)
        (aref list-position type))
  (let ((piece-cell-coords-list (translate-piece (nth type piece-list) (list row col)))
        (blanks-index (1- (array-dimension list-position 0))))
    (loop with blank-cell-list = (aref list-position blanks-index)
          for cell-coords in piece-cell-coords-list
          for row = (first cell-coords)
          for col = (second cell-coords)
          do
          (setf blank-cell-list
                (delete (aref *cell-from-coords* row col)
                        blank-cell-list))
          finally
          (setf (aref list-position blanks-index)
                blank-cell-list))))

(defun bit-int-from-list (list-of-ints)
  (loop for int in list-of-ints
        summing
        (expt 2 int)))

(defun copy-array (2d-array)
  (let ((new-array (make-array (array-dimensions 2d-array))))
    (dotimes (row (array-dimension new-array 0))
      (dotimes (col (array-dimension new-array 1))
        (setf (aref new-array row col)
              (aref 2d-array row col))))
    new-array))
  
(defun count-nulls (2d-array)
  (loop for row from 0 below (array-dimension 2d-array 0)
     sum
       (loop for col from 0 below (array-dimension 2d-array 1)
          count (null (aref 2d-array row col)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SLIDE-INIT-REDUCED
;;;    puzzle "reduced" by omitting certain piece-types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slide-init-reduced (board-template piece-types start-positions-list
                           piece-type-numbers-to-omit single-target-form
                           piece-type-move-offsets)
  (let* ((piece-type-map (piece-type-index-map piece-types piece-type-numbers-to-omit))
         (reduced-piece-types (omit-piece-type-forms piece-types piece-type-numbers-to-omit))
         (reduced-start-positions-list (reduce-start-list start-positions-list
                                                          piece-type-map))
         (new-target-piece-type (get-mapped-type (first single-target-form)
                                                 piece-type-map))
         (reduced-piece-type-move-offsets (when piece-type-numbers-to-omit
                                            (omit-piece-type-forms piece-type-move-offsets
                                                                   piece-type-numbers-to-omit))))
    (slide-init board-template reduced-piece-types reduced-start-positions-list)
    (single-target-from-row-col new-target-piece-type
                                (second single-target-form)
                                (third single-target-form))
    (setup-move-displacement-limits reduced-piece-type-move-offsets)
    ))

(defun piece-type-index-map (piece-types omit-piece-type-nums)
  (loop with keep-types = (loop for i from 0
                             for type in piece-types
                             unless (member i omit-piece-type-nums)
                             collect i)
     for new-i from 0
     for old-i in keep-types
     collect (list old-i new-i)    ;; map old-i to new-i
       ))

(defun omit-piece-type-forms (piece-type-forms omit-piece-type-nums)
  (loop for i from 0
     for piece-type-form in piece-type-forms
     unless
       (member i omit-piece-type-nums)
     collect
     piece-type-form))

(defun reduce-start-list (start-positions-list piece-type-map)
  ;; modify start position: omit specified pieces, change other piece-types using map
  (loop for (type . row-col) in start-positions-list
     for mapped-type = (get-mapped-type type piece-type-map)
     when mapped-type
     collect
       (cons mapped-type row-col))
  )

;; maps old-type into reduced-type
(defun get-mapped-type (type type-map)
  (second (assoc type type-map)))

;;; setup move limits

;; *move-displacement-limits* is array indexed by piece-type & from-cell
;;   value is a "mask" with 1's for cells that are "beyond reach"
;;   usage: init "marked cells" so these will never be put on queue in "extension search"

;; only call after reduced-puzzle is otherwise initialized
(defun setup-move-displacement-limits (reduced-piece-row-col-delta-lists)
  (cond (reduced-piece-row-col-delta-lists
         (setf *move-displacement-limits* (make-array (list *num-piece-types*
                                                            *num-cells*)))
         (loop for piece-type from 0 below *num-piece-types*
            for piece-type-row-col-deltas in reduced-piece-row-col-delta-lists
            do
              (loop for from-cell from 0 below *num-cells*
                 for move-limit-mask = (make-move-limit-mask from-cell piece-type-row-col-deltas)
                 do
                   (setf (aref *move-displacement-limits* piece-type from-cell)
                         move-limit-mask))))
        (t (setf *move-displacement-limits* nil))))

(defun make-move-limit-mask (from-cell piece-type-row-col-deltas)
  (let* ((new-mask (make-array *num-cells* :element-type 'bit :initial-element 1))
         (from-row-col (aref *coords-from-cell* from-cell))
         (reachable-pairs (translate-piece piece-type-row-col-deltas from-row-col)))  ;translate pairset
    (loop for pair in reachable-pairs
       for pair-cellnum = (safe-cell-from-coord-pair pair)
       when pair-cellnum
       do
         (setf (sbit new-mask pair-cellnum) 0))    ;; pair-cell is potentially reachable
    (setf (sbit new-mask from-cell) 0)  ;; make sure orig cell is also ok (is this necessary?)
    new-mask))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Used to be in "Position Pool" code - still needed by slide-init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-domain-position ()
  (make-array *position-size* :element-type 'integer))

(defun copy-position (position)
  (declare (simple-vector position))
  (let ((new-pos (new-domain-position)))
    (declare (simple-vector new-pos))
    (replace new-pos position)
    new-pos))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JIMSLIDE REPRESENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jimslide-integer-from-byte-sequence 
    (&optional (byte-sequence **sbp-position-register**)) ;; byte-seq starts with low order byte
  (loop for byte-val across byte-sequence
     for shift from 0 by 8
     sum
       (ash byte-val shift)))

(defun write-jimslide-integer-to-byte-seq (jimslide-int byte-seq) ;; assumes byte-seq is long enough
  (fill byte-seq 0) ;; in case of small integer (hence lots of high-order 0 bytes)
  ;; note: this writes bytes in "reverse" order -- low-order byte 1st
  (loop 
     for index from 0
     for int = jimslide-int then (floor int 256)
     for byte-val = (mod int 256)
     until (zerop int)
     do
       (setf (aref byte-seq index)
	     byte-val)))

;;; modify in SBP-5-JIMSLIDE to place blank-index at high-end of integer
(defun jimslide-integer-from-intermediate-position-and-blank-index
    (&optional 
       (intermed-pos **intermediate-position**)
       (blank-index **intermediate-blank-index**))
  (loop with jimslide-int = 0
     with place-val = 1  ;; was **blank-index-range**
     for type-val across intermed-pos
     when (>= type-val 0)
     do
       (incf jimslide-int (* place-val type-val))
       (setf place-val (* place-val *num-piece-types*))
     finally
       (return (+ jimslide-int (* place-val blank-index)))))  ;; place-val should be **blank-index-place-value**

;;; Note: may need to handle "leading 0's" as type codes when converting integer to intermed-pos
(defun jimslide-intermediate-position-from-jimslide-integer (jimslide-int)
  (setf **intermediate-blank-index**
	(floor jimslide-int **blank-index-place-value**))
  (fill **intermediate-position** -1)
  (loop with used-cells = (aref **cells-bitint-from-blank-index** **intermediate-blank-index**)
     for next-fill = (find-next-empty-cell used-cells 0) then (find-next-empty-cell used-cells next-fill)
     for rem-int = jimslide-int then next-int
     for next-type = (mod rem-int *num-piece-types*)
					;for next-type-cover = (aref **piece-bit-int-array** next-type next-fill)
     for next-int = (floor rem-int *num-piece-types*)
     while (< next-fill *num-cells*)
     do
     ;; debug
       (when nil  ;; set T to print for degugging
	 (format t "~%~%used-cells = ~b" used-cells)
	 (format t "~%next-fill = ~a" next-fill)
	 (format t "~%rem-int = ~a" rem-int)
	 (format t "~%next-type = ~a" next-type)
	 (format t "~%next-type-cover = ~b" (aref **piece-bit-int-array** next-type next-fill))
	 (format t "~%next-int = ~a" next-int))
     ;; actual fill work
       (setf (aref **intermediate-position** next-fill)
	     next-type)
       (setf used-cells
	     (logior used-cells
		     (aref **piece-bit-int-array** next-type next-fill)))))
	     
;; note this searches from low to high, beginning at start, and returning index of 1st 0 bit
(defun find-next-empty-cell (used-bits start)
  (loop for bit from start
     when (not (logbitp bit used-bits))
       return bit))

(defun jimslide-intermediate-position-from-domain-pos-with-blank-index (domain-pos-with-blank-index)
  ;; input is a vector domain-pos, with elements bitints for piecetypes, and blanks as blank-index int
  (setf **intermediate-blank-index**
	(aref domain-pos-with-blank-index
	      (1- (length domain-pos-with-blank-index))))
  (fill **intermediate-position** -1)
  ;; write to **intermediate-position**
  (loop for piece-type from 0 below *num-piece-types*
     for piece-bitint across domain-pos-with-blank-index
     do
       (loop for bit-index from 0 to (integer-length piece-bitint)
	  when
	    (logbitp bit-index piece-bitint)
	  do
	    (setf (aref **intermediate-position** bit-index)
		  piece-type)))
  )

;; this is used for displaying solution sequence (doesn't need to be that efficient)
;; NOTE: does NOT (yet) set blank-bits since fancy-display-position doesn't use them
(defun jimslide-domain-position-from-intermediate-position () ;; puts in **sbp-domain-position-register-aux**
  (fill **sbp-domain-position-register-aux** 0)
  (loop for cellnum from 0
     for piece-type across **intermediate-position**
     when (>= piece-type 0)
     do
       (setf (aref **sbp-domain-position-register-aux** piece-type)
	     (logior (ash 1 cellnum)
		     (aref **sbp-domain-position-register-aux** piece-type)))
       ))
  

;; **intermediate-position** + **intermediate-blank-index** --> **sbp-position-register**
(defun jimslide-compress (&optional (blank-index **intermediate-blank-index**))
  (fill **sbp-position-register** 0)
  (loop with write-byte-seq = **sbp-position-register**
     with next-byte-index = 0
     with byte-size = **byte-size**  ;; must be multiple of 4
     ;with byte-mask = (1- (ash 1 byte-size))
     ;with elem-mask = (1- (ash 1 4)) ;; all 1's (length 4 for each element)
     with byte-accumulator = 0
     with byte-elem-shift = 0
     for type-val across **intermediate-position**
     when (>= type-val 0)
     do  ;; process new type-val
       (setf byte-accumulator
	     (+ (ash byte-accumulator 4) type-val))
       (incf byte-elem-shift 4)
       (when (= byte-elem-shift byte-size)  ;; byte is full
	 ;; write byte & shift
	 ;(print 'trying-to-shift)
	 (setf (aref write-byte-seq next-byte-index) ;; write next byte
	       byte-accumulator)
	 (setf byte-accumulator 0
	       byte-elem-shift 0)
	 (incf next-byte-index))
     finally
       (when (> byte-elem-shift 0)  ;; there is a partial byte to write
	 (setf (aref write-byte-seq next-byte-index) ;; write out last accumulated byte
	       (ash byte-accumulator  ;; leftover shift to left-justify elements in last byte
		    (- byte-size byte-elem-shift))))
       ;; record blank-index
       (setf (aref write-byte-seq (1- **position-size**))
	     blank-index)
       (return write-byte-seq)))


;; **sbp-position-register**  --> **intermediate-position** + **intermediate-blank-index**
(defun jimslide-uncompress (&optional (byte-seq **sbp-position-register**))
  ;; extract blank-index
  (setf **intermediate-blank-index**
	(aref byte-seq (1- **position-size**)))
  ;; decode bytes
  (fill **intermediate-position** -1)
  (loop named outer
     with used-cells = (aref **cells-bitint-from-blank-index** **intermediate-blank-index**)
     with next-fill = (find-next-empty-cell used-cells 0)
     with elem-mask = 15    ;; 4 1's
     ;with stop? = nil
     for byte across byte-seq
     do
       (loop for shift from (- **byte-size** 4) downto 0 by 4
	  for next-type = (logand elem-mask (ash byte (- shift)))
	  do
	  ;; debug
	    #|
	    (when nil  ;; set T to print for degugging
	      (format t "~%~%used-cells = ~b" used-cells)
	      (format t "~%next-fill = ~a" next-fill)
	      (format t "~%next-type = ~a" next-type)
	      (format t "~%next-type-cover = ~b" (aref **piece-bit-int-array** next-type next-fill))
	      )
	    |#
	  ;; actual fill work
	    (setf (aref **intermediate-position** next-fill)
		  next-type)
	    (setf used-cells
		  (logior used-cells
			  (aref **piece-bit-int-array** next-type next-fill)))
	    (setf next-fill (find-next-empty-cell used-cells next-fill))
	    (when (>= next-fill *num-cells*)
	      (return-from outer)))))

;;; use to set **final-shift** in puzzle init
(defun final-shift (count byte-size)
  (let* ((elems-per-byte (floor byte-size 4))
	 (unused-elements (mod  (- count) elems-per-byte)))
    (* 4 unused-elements)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Do own garbage management -- GC is a killer.
;;     recycle my own conses, etc.
;;     recycle hash tables!!

;;     Note - presently no automatic calls to reclaim-scheduled-fringes

;; Add declarations for speed/efficiency

;; Consider own "hash-tree" instead of hash-tables.
;;    Can this become a general facility?
;;    Can we efficiently "loop" over these overlapping tree elements?

;; Put Time printing statements before and after to provide record of 
;;   search times  (or just wrap TIME function around form 
;;      (this also gives memory allocation info)

;; Recover 1-step solutions when start-list has multiple positions
;;      (generate moves or else loop over start-list doing searches
;;		from single-positions)
;;	   [the latter would require recognizing failure, and limiting
;;		search to depth 1 ]

;; Maybe use bits instead of hash-table for locally-seen positions of given piece
;;    in same-piece-extension.

;; Use logical shift instead of (expt 2 n) ??

;; Use macros to compile conditionally for rush-hour or non-rush-hour

;; Modify the use of piece-list and/or start-list so can use a SUBSET of available pieces in list
;;   CURRENTLY: I think it creates entries for every piece, not just the actual pieces used.
;;      This would be convenient for defining large sets of pieces, then only picking out the ones needed for a specific puzzle
