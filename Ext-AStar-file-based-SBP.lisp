(declaim (optimize (speed 3) (debug 0) (safety 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXTERNAL A*  (cf Edelkamp, et.al)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; see /A-STAR/Ext-AStar-SBP-v6.lisp
;;;    for OLD VERSION NOTES

;;; NEW FILE-BASED EXT-A*

;;; File /A-STAR-File-Based/Ext-AStar-file-based-SBP-v6.lisp
;;;    BRANCHED FROM PREVIOUS /A-STAR/ version

;;; Version v6  [Ext-AStar-file-based-SBP-v6]
;;;   11-12-2016
;;;     Change Hash-tables to calls to "file buckets"
;;;       -- uses code in /A-STAR-File-Based/Ext-AStar-code-from-file-search-engine-1.lisp
;;;     Add 2nd argument exper-tag in SBP-SETUP-ext-astar
;;;    BAH -- v6 is buggy (doesn't properlly do merges)

;;; Version v7  [Ext-AStar-file-based-SBP-7]
;;;    Create separate EXPAND-BUCKET function
;;;       This will be responsible for doing the merge before expanding

#|
PSEUDO CODE:

Procedure External A*
Open(0, h(I)) ← {I}
fmin ← h(I)
while (fmin != ∞)
gmin ← min{i | Open(i, fmin − i) 6= ∅}
while (gmin ≤ fmin)
hmax ← fmin − gmin
A(fmin), A(fmin + 1), A(fmin + 2) ← N(Open(gmin, hmax))
Open(gmin + 1, hmax + 1) ← A(fmin + 2)
Open(gmin + 1, hmax) ← A(fmin + 1) ∪ Open(gmin + 1, hmax)
Open(gmin + 1, hmax − 1) ← A(fmin) ∪ Open(gmin + 1, hmax − 1)
Open(gmin + 1, hmax − 1) ← remove duplicates from Open(gmin + 1, hmax − 1)
Open(gmin + 1, hmax − 1) ← Open(gmin + 1, hmax − 1)\
(Open(gmin, hmax − 1) ∪ Open(gmin − 1, hmax − 1))
gmin ← gmin + 1
fmin ← min{i + j > fmin | Open(i, j) != ∅} ∪ {∞}
|#


(defparameter **h-scale** 1)
(defparameter **target-position**     ;; if non-nil used as manh-move-h-fun target
  nil)                                ;;  gets set in  setup-data-for-manhattan-move-h-fun

(defparameter **open** nil)
(defparameter **solution** nil)
(defparameter **open-delete-counts** nil)
(defparameter **open-create-sizes** nil)
(defparameter **open-delete-sizes** nil)

(defparameter **init-position** nil)
(defparameter **max-g** nil)
(defparameter **max-h** nil)
(defparameter **h-fun** nil)
(defparameter **solved?-fun** nil)
(defparameter **successors-fun** nil)

(defparameter **debug** nil)

(defparameter **equality-test** nil)  ;; needed to compile -- gets set in SBP-SETUP-EXT-ASTAR


;; for hash-tables
#|
(defparameter **new-ht-size** 100000)
;(defparameter **new-ht-size** 65684160)
(defparameter **empty-dummy-hash-table**
  (make-hash-table))
|#

;;; Parameter print utility
(defun print-parameter-info (param-list)  ;; list of param-symbols
  (loop for param in param-list
     do
       (format t "~%  ~a = ~a" param (eval param))))


;;; Main search funtion

(defun external-a-star (&key
                          (initial-position **init-position**)
                          (g-bound **max-g**)
                          (h-bound **max-h**)
                          (h-fun **h-fun**)
                          (successors-fun **successors-fun**)
                          )
  (init-out-buffs) ;; sets up 3 out-buffs (**out-buff-0** **out-buff-1** **out-buff-2**)
  (setf **solution** nil)
  (setf **max-g** g-bound)
  (setf **max-h** h-bound)
  (setf **successors-fun** successors-fun)
  ;; print parameter info
  (print-parameter-info '(**init-position**
                          **max-g**
                          **max-h**
                          **heap-threshold**
                          **byte-size**
                          **position-size**
                          **final-8bit-byte-position-size**
                          **puzzle-name**
                          **puzzle-directory-name**
                          **path-to-file-storage**
                          **exper-tag**
                          **moves-invertible?**
                          **debug**
                          **prior-fan?**
                          **h-scale**
                          **max-buffer-position-count**
                          **target-position**    ;; used to select h-fun type
                          ))
  ;; if **target-position** display the position in ascii art
  (when **target-position**
    (fancy-display-compressed-position **target-position**))
  ;; start-timing
  (start-timing 'elapsed-time)
  (loop with open = (make-array (list (1+ g-bound) (1+ h-bound))
                                :initial-element nil)
     with init-h-val = (funcall h-fun initial-position)
     with f-max = (+ g-bound h-bound) ;; max-f for open array at g-bound, h-bound  (was -2 before)
     with f-min = init-h-val
     with g-min = nil
     with h-max = nil
     initially
       (setf **open** open) ;; make available for debugging
       (setf **open-delete-counts**
             (make-array (array-dimensions open) :initial-element 0))
       (setf **open-create-sizes**
             (make-array (array-dimensions open) :initial-element nil))
       (setf **open-delete-sizes**
             (make-array (array-dimensions open) :initial-element nil))
       (store-bucket initial-position 0 init-h-val)
     while (and (not **solution**)
                (not (eql f-min 'infinity)))
     do
       (print 'start-outer-loop)
       (setf g-min
             (loop for i from 0 below g-bound
                ;; min{i | Open(i, fmin − i) 6= ∅}
                unless (empty-bucket? i (- f-min i)) ;; empty-bucket? checks for array-in-bounds-p
                return i))
                                        ;(setf g-min (max (1- g-min) 0))   ;; Kludge to try force dup elims that were missed
       (format t "~% g-min = ~a" g-min)
       (format t "~% f-min = ~a" f-min)
       (loop 
          while (and (<= g-min f-min)
                     (not **solution**))
          do
          ;; hmax ← fmin − gmin
            (setf h-max (- f-min g-min))
            (format t "~% h-max = ~a" h-max)
            (report-all-timers)
          ;; A(fmin), A(fmin + 1), A(fmin + 2) ← N(Open(gmin, hmax))
          ;; A0, A1, A2
          ;;(g h write-segments? out-buff-to-repoint)

            (expand-bucket g-min h-max)


          ;;gmin ← gmin + 1
            (incf g-min)
            (format t "~% Inc g-min to ~a" g-min)
            )
       
     ;; fmin ← min{i + j > fmin | Open(i, j) != ∅} ∪ {∞}
       (unless **solution**
         (let ((old-f-min f-min))
           (setf f-min
                 (loop with min-f = 'infinity
                    for try-f from (1+ f-min) to f-max
                    while (eql min-f 'infinity)
                    do
                      (loop for i from 0 to (min (1- g-bound) try-f)
                         for j = (- try-f i)
                         when (not (empty-bucket? i j)) ;; this checks array-in-bounds-p
                         ;; (and (<= i g-bound)   ;; allow equality
                         ;;     (<= j h-bound)   ;; allow equality
                         ;;   (not (empty-bucket? i j)))
                         do
                           (setf min-f try-f))
                    finally
                      (return min-f)))
           ;; delete buckets from (old-f-min - 2) to (f-min - 3)
           (clear-open-bucket-diagonals (- old-f-min 2) (- f-min 3))
           ))
       (format t "~% End Pass Through Outer Loop, New F-MIN = ~a" f-min)
     finally
       (report-all-timers)
       (return **solution**)
       ))

;;; EXPAND BUCKET
;;;   This now triggers the merge before expanding

(defun expand-bucket (g-min h-max)
  (merge-segments g-min h-max) ;; Merge / filter bucket before expanding (check if merge-file exists)
  (let* ((A2 ;; [A2] Open(gmin + 1, hmax + 1) ← A(fmin + 2)
          (get-bucket-out (1+ g-min) (1+ h-max) t **out-buff-2**))
         (A1 ;; [A1] Open(gmin + 1, hmax) ← A(fmin + 1) ∪ Open(gmin + 1, hmax)
          (get-bucket-out (1+ g-min) h-max t **out-buff-1**))
         (A0 ;; [A0] Open(gmin + 1, hmax − 1) ← A(fmin) ∪ Open(gmin + 1, hmax − 1)
          (get-bucket-out (1+ g-min) (1- h-max) t **out-buff-0**))
         (output-object
          (list h-max A0 A1 A2)))
    (when **debug**
      (print 'output-object) 
      (princ output-object))
    (loop with sol-pos? = nil
       with in-buff = (get-bucket-in g-min h-max)
       for pos = (when in-buff (get-front-position in-buff)) then (next-position in-buff)
       while (and pos
                  (not **solution**))
       do
         (inc-counter 'expanded-positions)
                                        ;(print 'before-calling-generate-successors)
         (when **debug**
           (print 'generating-successors-of-pos)
           (princ pos))
         (setf sol-pos? (generate-successors pos output-object))
                                        ;(print 'after-calling-generate-successors)
         (when sol-pos?
           (setf  **solution** (list sol-pos? (1+ g-min) h-max))      ;; h-max is h-index of parent
           (format t "~%FOUND SOLUTION WITH G-VAL = ~a" (1+ g-min)))

       finally
         (when in-buff
           (close-buffer in-buff))
         )
    ;; Close-buffers A0, A1, A2
    (when A0
      (close-buffer A0))
    (when A1
      (close-buffer A1))
    (when A2
      (close-buffer A2))
    ))


;;;; OPEN BUCKET INFO

(defun new-open-info-record ()
  (list :record? t))   ;; to guarantee never empty (so can store by nconc)

(defun store-open-info (g h key val)
  (let ((info-rec (aref **open** g h)))
    (unless info-rec
      (setf info-rec (new-open-info-record))
      (setf (aref **open** g h)
            info-rec))
    (record-store info-rec key val)))

(defun lookup-next-segment (g h)   ;; assumes in-bounds
  ;; returns 1 more than :segment-count or 1 if not found
  (let* ((info-record (aref **open** g h))
         (segment-count? (lookup :segment-count info-record)))
    (if segment-count?
        (1+ segment-count?)
        1)))

;; basic record functions

(defun lookup (key record)    ;; key can never be :record-head
  (second (member key record)))   

(defun key-exists? (key record)
  (cond ((null record) nil)
        ((eq key (first record))
         t)
        (t (key-exists? key (cddr record)))))

;; do this carefully!  ie make sure key exists, and add it with a NIL value if it doesn't
(defun record-store (record key value)
  (if (key-exists? key record)
      (setf (second (member key record)) ; replace value following key
            value)
    (nconc record (list key value)))) ; assumes record is non-NIL


;;;;;;;;;;;;;;;;

(defun get-bucket-out (g h write-segments? out-buff-to-repoint)
  (when (array-in-bounds-p **open** g h) ;; return nil if indices out of bounds
    (let ((next-segment? (if write-segments?
                             (lookup-next-segment g h) ;; defaults to 1 if not found
                             nil))) ;; nil means don't write segments
      (point-output-buffer out-buff-to-repoint
                           g h next-segment?))))

(defun get-bucket-in (g h)
  (when (bucket-exists? g h)
    (new-input-buffer (bucket-pathname g h)))
  )


;; deleted new-bucket 

(defun clear-open-bucket-diagonals (lo-f-val hi-f-val)
  (loop for f-val from lo-f-val to hi-f-val
     do
       (clear-open-bucket-diagonal f-val)))

(defun clear-open-bucket-diagonal (f-val)
  (loop for g from f-val downto 0
     for h = (- f-val g)
     while (array-in-bounds-p **open** g h)
     do
       (clear-open-bucket g h)))

(defun clear-open-bucket (g h)
  ;; DO NOTHING FOR NOW
  nil
  #|
  (when (bucket-exists? g h)
  (let ((rem-bucket (aref open-array g h))) ;; info or nil ; ;
  (when rem-bucket                            ;; if it's a ht ; ;
  (setf (aref **open-delete-counts** g h)
  (hash-table-count rem-bucket))
  (setf (aref open-array g h) nil))))
  |#
  )

;; only called to store initial-position (should start new bucket)
(defun store-bucket (position g h)   ;; NOTE: doesn't check for array-in-bounds-p (assumed ok)
  (let ((out-buff (point-output-buffer **out-buff-0** g h nil)))   ;; nil for "No Segments"
    (write-position out-buff position)
    (close-buffer out-buff)))


(defun empty-bucket? (g h)
  (cond ((not (array-in-bounds-p **open** g h))
         t)
        (t ;; return t if both bucket and segments empty
         (and (let ((bucket-filepath (bucket-pathname g h))) ;; let checks if bucket file empty
                (or (not (probe-file bucket-filepath)) ;; file doesn't exist
                    (zerop (get-file-size bucket-filepath)))) ;; file exists but is empty
              (loop for seg-num from 1 ;; T if there is some segment that is NOT empty
                 for bucket-seg-path = (bucket-segment-pathname g h seg-num)
                 while (probe-file bucket-seg-path)
                 always (zerop (get-file-size bucket-seg-path)))))))


(defun recover-solution (solution-triple)
  (let ((sol-seq (recover-solution-sequence solution-triple)))
    (loop for pos in sol-seq
       do
         (fancy-display-compressed-position pos))))

(defun recover-solution-sequence (solution-triple)
  (loop with (sol-pos g final-h) = solution-triple   ;; final-h is h-index of solution parent
     ;; with final-h = (find-h-index sol-pos g)
     with sol-seq = (list sol-pos)
     for g-val from g downto 0
     for h-index = final-h then parent-h
     for curr-pos = sol-pos then parent-pos
     for (parent-pos parent-h) = (find-parent curr-pos g-val h-index)
     while parent-pos
     do
       (push parent-pos sol-seq)
     finally
       (return sol-seq)))

;;; This fails since no buckets, only segments for solution g
(defun find-h-index (pos g)
  (loop
     for h from 0 to **max-h**
     for inbuff = (get-bucket-in g h)
     for pos-found? = (find-parent? (list pos) inbuff)
     until pos-found?
     do
       (when inbuff
         (close-buffer inbuff))
     finally
       (when inbuff
         (close-buffer inbuff))
       (return h)))

;; only use when moves are INVERTIBLE (not true for SBP <<-- HUH??)
;; Modified to return pair ( parent-pos  h-index-of-parent-pos )
(defun find-parent (pos g &optional h-index (delta-h-list '(-1 0 1)))
  ;; need to look at buckets (g-1, h+1) (g-1, h) and (g-1, h-1)
  ;;    since h can only change by at most 1
  (loop
     with h = (if h-index
                  h-index
                  (funcall **h-fun** pos))
     with succ-list = (collect-successors pos)   ;; possible parents, since invertible
     with parent = nil
     with parent-h = nil
     for delta-h in delta-h-list
     for try-h = (+ h delta-h)
     for inbuff = (get-bucket-in (1- g) try-h)
     until parent
     do
       (when (setf parent (find-parent? succ-list inbuff))
         (setf parent-h try-h))
       (when inbuff
         (close-buffer inbuff))  ;; close inbuff whether or not parent found
     finally
       (return (list parent parent-h))))

(defun find-parent? (poss-parent-list in-buffer)
  (when in-buffer
    (loop for pos = (get-front-position in-buffer) then (next-position in-buffer)
       while pos
       when (member pos poss-parent-list :test **equality-test**)
         return (copy-seq pos))))
                    

(defun count-all-positions ()
  (loop with array-dims = (array-dimensions **open**)
     with (rows cols) = array-dims
     for row from 0 below rows
     sum
       (loop for col from 0 below cols
          for bucket =  (aref **open** row col)
          when bucket
          sum
            (hash-table-count bucket))))

(defun get-open-ht-counts ()
  (loop with array-dims = (array-dimensions **open**)
     with (rows cols) = array-dims
     for row from 0 below rows
     append
       (loop for col from 0 below cols
          for bucket =  (aref **open** row col)
          when bucket
            collect (list (list row col)
                          (hash-table-count bucket)))))

(defun get-open-ht-g-positions (g)
  (loop with array-dims = (array-dimensions **open**)
     with (rows cols) = array-dims
     with row = g
     for col from 0 below cols
     for bucket =  (aref **open** row col)
     when bucket
     sum (hash-table-count bucket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COLLECT FINAL DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter **final-bucket-pattern** nil)
(defparameter **ht-growth-data** nil)
(defparameter **generation-sizes** nil)  ;; sum over each g-val


(defun collect-final-data ()
  ;; Capture Final bucket pattern
  (setf **final-bucket-pattern**
        (loop with array-dims = (array-dimensions **open**)
           with final-bucket-pattern = (make-array array-dims :initial-element nil)
           with (rows cols) = array-dims
           for row from 0 below rows
           do
             (loop for col from 0 below cols
                when (aref **open** row col)
                do
                  (setf (aref final-bucket-pattern row col)
                        t))
           finally
             (return final-bucket-pattern)))
  ;; delete remaining buckets from **open**
  (loop with (rows cols) = (array-dimensions **open**)
     for row from 0 below rows
     do
       (loop for col from 0 below cols
          for open-val = (aref **open** row col)
          when open-val    ;; it's a bucket
          do
            (clear-open-bucket row col)
            ))
  ;; Collect HT growth data
  (setf **ht-growth-data**
        (loop with array-dims = (array-dimensions **open**)
           with ht-growth-array = (make-array array-dims :initial-element nil)
           with (rows cols) = array-dims
           for row from 0 below rows
           do
             (loop for col from 0 below cols
                for create-size = (aref **open-create-sizes** row col)
                for final-size = (aref **open-delete-sizes** row col)
                when (and create-size
                          final-size
                          (not (= create-size final-size)))
                do
                  (setf (aref ht-growth-array row col)
                        (list create-size final-size)))
           finally
             (return ht-growth-array)
             ))
  ;; collect Generation sizes
  (setf **generation-sizes**
        (loop with (rows cols) = (array-dimensions **open-delete-counts**)
           for g from 0 below rows
           for g-count = (loop for col from 0 below cols
                            for count-val = (aref **open-delete-counts** g col)
                            when count-val
                              sum count-val)
           collect
             (list g g-count)))

  )

(defun display-final-data ()
  (print '**final-bucket-pattern**)
  (pprint **final-bucket-pattern**)
  (print '**open-delete-counts**)
  (pprint **open-delete-counts**)
  (print '**open-create-sizes**)
  (pprint **open-create-sizes**)
  (print '**open-delete-sizes**)
  (pprint **open-delete-sizes**)
  (print '**ht-growth-data**)
  (pprint **ht-growth-data**)
  (print '**generation-sizes**)
  (mapcar #'print **generation-sizes**))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SBP INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; New Usage for File-based Ext-A* (11-12-2016)
;;;   Load in order: 
;;;     [from directory: BreadthFirstFileSearch/ ]
;;;          my-profiler-5.lisp
;;;     [from directory: A-STAR-File-Based]
;;;         Ext-AStar-code-from-file-search-engine-1.lisp    (adapted from BFS-FB code)
;;;         Ext-AStar-file-based-SBP-v7.lisp  (this file)
;;;         SBP-7-Ext-Astar-blank-index-jimslide-4.lisp  (copied to ASTAR-File-Based directory)
;;;     [from directory: BreadthFirstFileSearch/Domains/Sliding-Block-Puzzles/  ]
;;;            slide-puzzle-inits-<n>.lisp
;;;   Setup puzzle:  (sbp-setup-ext-astar <puzzle-selector> <exper-tag>)
;;;   Then can run search: (external-a-star)   ;; taking defaults as setup by SBP-SETUP


;; Defparameters (to allow SBP-7 to compile, even if not used)

(defparameter **HEAP-THRESHOLD** 5)
(defparameter **USE-PARITY?** nil)
(defparameter **FORCE-FULL-DUP-ELIM-INTERVAL** nil)
(defparameter **PRIOR-FRINGE-LOOKUP-LIMIT** nil)
(defparameter **BYTE-SIZE** 32)
(defparameter **COMPRESSED-SOLUTION-SEQUENCE** nil)
(defparameter **start-pos-list** nil)

#|
  style-warning: Undefined function CLIMB12-2-SINGLETONS-INIT
  style-warning: Undefined function CLIMB12-NO-SINGLETONS-INIT
  style-warning: Undefined function CLIMB15A-2-SINGLETONS-INIT
  style-warning: Undefined function CLIMB15A-NO-SINGLETONS-INIT
  style-warning: Undefined function WRITE-POSITION
  style-warning: Undefined function FANCY-DISPLAY-COMPRESSED-SOLUTION-SEQUENCE
  |#

;; Counters
;;    setup in SBP-SETUP   


;; SBP-SETUP
                  
(defun sbp-setup-ext-astar (puzzle-selector exper-tag
                            &key
                              (max-g 110)
                              (h-scale **h-scale**)
                              (max-h (* h-scale 50)) ; NOTE: **h-scale** has default value
                              (prior-fan? (> h-scale 1))
                              (h-fun #'sbp-h-fun-from-compressed-pos)
                              h-fun-target-pos
                              )
  (sbp-exper puzzle-selector exper-tag) ;; among other things, sets up **puzzle-directory-name**
  ;; setup globals for External-A-star
  (setf **max-g** max-g
        **h-scale** h-scale
        **max-h** max-h
        **prior-fan?** prior-fan?
        **init-position** (first **start-pos-list**)
        **h-fun** h-fun
        ;; **successors-fun** nil ;; don't call -- generate-successors is hard-wired here
        **equality-test** #'equalp
        **target-position** h-fun-target-pos)
  ;; MORE SETUP (ADAPTED FROM FILE-SEARCH-ENGINE-SETUP)
  ;; Check Puzzle is initialized (shared globals all ok)
  (unless
      (and **puzzle-name** ;; puzzle-name   (only nil if it hasn't been set by puzzle initialization)
           (and **position-size**
                (integerp **position-size**)
                (> **position-size** 0))
           **init-position**
           (= (length **init-position**) **position-size**))
    (error "Puzzle Initialization is not valid"))
  ;; ensure puzzle-directory exits
  (ensure-directories-exist (puzzle-directory-pathname))
  ;; Calculate position size in 8-bit bytes (using **position-size** and **byte-size**)
  (setf **final-8bit-byte-position-size** ;; needed to calculate position counts for fringe files
        (* **position-size** (ceiling **byte-size** 8)))
  ;; Set candidate-position-register for use by merge-segments
  (setf **candidate-position-register**
        (make-byte-vector **position-size**))
  (setf **heap** (create-empty-heap 8200)) ;; 2000 input buffers should suffice for a while, but maybe needs larger later (2-28-2016 do need more for climb15a with buff=50000)
  ;; check **free-input-buffers** to see if position-size is ok
  (let ((first-inbuff (aref **free-input-buffers** 0)))
    (when (and first-inbuff
               (not (= **position-size** (position-size first-inbuff))))
      (fill **free-input-buffers** nil))) ;; need to allocate new inbuffs with correct position sizes
  ;; if directory is non-empty ask if should be cleared before search
  (let ((puzzle-directory-files (puzzle-directory-file-list)))
    (when (and T ;; was (not search-restart?)  no restart implemented yet
               puzzle-directory-files)
      (warn "(Ext-AStar-file-based-SBP:sbp-setup-ext-astar): Files already exist in puzzle directory")
      (pprint puzzle-directory-files)
      (if (yes-or-no-p "Type Yes to delete these files and continue search, or No to abort")
          ;; NOTE: this will delete all files starting with "." as well!  (maybe should omit deleting those?)
          ;;    Seems if there are sub-directories, will not delete those (even if they are empty)
          (loop for filepath in puzzle-directory-files
             do
               (delete-file filepath))
          (error "aborting -- not safe to do new search with non-empty puzzle directory"))))

  ;; setup timers
  (loop for timer-name in '(ELAPSED-TIME
                            ;; GENERATE-FRINGE  -- maybe time F-MIN diagonal ??
                            EXPAND 
                            EXPAND-SORT-BUFFER
                            EXPAND-WRITE-BUFFER
                            REDUCE
                            REDUCE-WRITE-BUFFER)
     do
       (allow-timing timer-name)
       (reset-timer timer-name)) ;; now does reset  (since start-timing doesn't reset accumulator)
  ;; setup counters
  (allow-counting 'all-successors)
  (allow-counting 'expanded-positions)
  ;; reset counters
  (reset-counter 'all-successors)
  (reset-counter 'expanded-positions)
  ;; setup manhattan-move-h-fun if target supplied
  (when h-fun-target-pos
    (setup-data-for-manhattan-move-h-fun h-fun-target-pos))
  )

;; this now includes scaling, and branches on **target-position** to select h-fun to use
;; can recover "raw (underlying) h-fun value" by supplying 1 as the optional hscale argument
(defun sbp-h-fun-from-compressed-pos (compressed-pos &optional (hscale **h-scale**))
  (jimslide-uncompress compressed-pos)
  (cond (**target-position** ;; use fast-manhattan-move-h-fun
         (* hscale
            (fast-manhattan-move-h-fun))) ;; uses **intermediate-position**
        (t ;; use original t-piece-h-fun
         (* hscale
            (t-piece-h-fun)))) ;; uses **intermediate-position**
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BIT FLIP -- TEST DOMAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; turn off all the bits
;;;   moves: flip any single bit


(defparameter **bit-flip-start** 27)
(defparameter **bit-flip-target** 0)

(defparameter **bit-flip-size** 5)


(defun bit-flip-solved? (position)
  (= position **bit-flip-target**))

(defun bit-flip-h-val (position)    ;; This is the heuristic (h) function [modify for specific problem]
  (logcount (logxor position        ;; number of 1 bits different from target
                    **bit-flip-target**)))

(defun bit-flip-successors (position)
  (loop for shift from 0 below **bit-flip-size**
     for move = (ash 1 shift)
     collect
       (logxor position move)))

(defun bit-flip-setup (&optional
                         (size **bit-flip-size**)
                         (start **bit-flip-start**)
                         (target **bit-flip-target**))
  (setf **max-g** size
        **max-h** size
        **bit-flip-size** size ;; needed for bit-flip-successors
        **init-position** start
        **bit-flip-target** target
        **h-fun** #'bit-flip-h-val
        **successors-fun** #'bit-flip-successors
        **equality-test** #'eql))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BIT FLIP 2 -- TEST DOMAIN
;;;    [now allow flipping 1 or 2 bits]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; goal: match target 
;;;   moves: flip any 1 or 2 bits


(defparameter **bit-flip-2-start** 27)
(defparameter **bit-flip-2-target** 0)

(defparameter **bit-flip-2-size** 5)

(defparameter **bit-flip-2-moves** nil)


(defun bit-flip-2-solved? (position)
  (= position **bit-flip-2-target**))

(defun bit-flip-2-h-val (position)  ;; This is the heuristic (h) function [modify for specific problem]
  (floor (logcount (logxor position        ;; floor "number of 1 bits different from target" by 2
                           **bit-flip-2-target**))
         2))

(defun bit-flip-2-successors (position)
  (loop for move in **bit-flip-2-moves**
     collect
       (logxor position move)))

(defun bit-flip-2-setup (&optional
                         (size **bit-flip-2-size**)
                         (start **bit-flip-2-start**)
                         (target **bit-flip-2-target**))
  (setf **max-g** size
        **max-h** size
        **bit-flip-2-size** size  ;; needed for bit-flip-2-successors
        **init-position** start
        **bit-flip-2-target** target
        **h-fun** #'bit-flip-2-h-val
        **successors-fun** #'bit-flip-2-successors
        **bit-flip-2-moves** (bit-flip-2-moves)
        **equality-test** #'eql))

(defun bit-flip-2-moves ()
  (loop with all-moves = (list 1)  ;; flip single 1's bit
     for i from 1 below **bit-flip-2-size**
     do
       (loop for j from 0 below i
            for move = (+ (ash 1 i) (ash 1 j))
          do
            (push move all-moves))
     finally
       (return all-moves)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6-PUZZLE -- TEST DOMAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter **6-puzzle-start**
  (make-array '(2 3)
              :initial-contents
              '((3 1 5)
                (0 2 4))))

(defparameter **6-puzzle-target**
  (make-array '(2 3)
              :initial-contents
              '((1 2 3)
                (4 5 0))))

(defparameter **6-puzzle-size**
  '(2 3))         ;; (rows cols)

(defparameter **6-puzzle-moves**
  '((0 1)   ;; right
    (0 -1)  ;; left
    (1 0)   ;; down
    (-1 0)) ;; up
  )

(defparameter **6-puzzle-cells**
  '((0 0)
    (0 1)
    (0 2)
    (1 0)
    (1 1)
    (1 2)))

(defun 6-puzzle-solved? (position)
  (equalp position **6-puzzle-target**))

(defun 6-puzzle-h-val (position)  ;; This is the heuristic (h) function [modify for specific problem]
  (6-puzzle-manhattan-distance position))

(defun 6-puzzle-manhattan-distance (position)
  (loop for cell in **6-puzzle-cells**
     for val = (apply #'aref position cell)
     when (> val 0)
     sum (distance cell
                   (6-puzzle-find-loc val **6-puzzle-target**))))

(defun distance (cell1 cell2)
  (apply #'+
         (mapcar #'abs (mapcar #'- cell1 cell2))))

(defun 6-puzzle-legal-move? (move position)
  (let ((blank-loc (6-puzzle-find-loc 0 position)))
    (apply #'array-in-bounds-p
           position
           (mapcar #'+ move blank-loc))))

(defun 6-puzzle-find-loc (val position)
  (loop for loc-cell in **6-puzzle-cells**
     when (= (apply #'aref position loc-cell)
             val)
       return loc-cell))


(defun 6-puzzle-successors (position)
  (loop for move in **6-puzzle-moves**
     when (6-puzzle-legal-move? move position)
     collect
       (6-puzzle-make-move move position)))

(defun 6-puzzle-make-move (move position)   ;; move assumed legal
  (let* ((blank-loc (6-puzzle-find-loc 0 position))
         (new-loc (mapcar #'+ move blank-loc))
         (new-loc-val (apply #'aref position new-loc))
         (new-position (copy-position position)))
    ;(print blank-loc)
    ;(print new-loc)
    ;(print new-loc-val)
    (setf (apply #'aref new-position blank-loc)
          new-loc-val)
    (setf (apply #'aref new-position new-loc)
          0)
    new-position))

(defun copy-position (position)
  (loop with array-dims =  (array-dimensions position)
     with (rows cols) = array-dims
     with new-position = (make-array array-dims)
     for row from 0 below rows
     do
       (loop for col from 0 below cols
          do
            (setf (aref new-position row col)
                  (aref position row col)))
     finally
       (return new-position)))
           
        


(defun 6-puzzle-setup (&optional
                         (size **6-puzzle-size**)
                         (start **6-puzzle-start**)
                         (target **6-puzzle-target**))
  (setf **max-g** 40
        **max-h** 16
        **6-puzzle-size** size ;; maybe not needed
        **6-puzzle-target** target
        ;; for external-a-star
        **init-position** start
        **h-fun** #'6-puzzle-h-val
        **successors-fun** #'6-puzzle-successors
        **equality-test** #'equalp
        ))

(defun display-array (array)
  (terpri)
  (loop with array-dims = (array-dimensions array)
     with (rows cols) = array-dims
     for row from 0 below rows
     do
       (terpri)
       (loop for col from 0 below cols
          for val = (aref array row col)
          do
            (princ val)
            (princ " ")))) 





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8-PUZZLE -- TEST DOMAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter **8-puzzle-start**
  (make-array '(3 3)                 ;; solvable in 26 moves
              :initial-contents
              '((3 8 7)
                (5 1 4)
                (6 2 0)
                )))

(defparameter **8-puzzle-target**
  (make-array '(3 3)
              :initial-contents
              '((1 2 3)
                (4 5 6)
                (7 8 0))))

;; not used?
(defparameter **8-puzzle-size**
  '(3 3))         ;; (rows cols)

(defparameter **8-puzzle-moves**
  '((0 1)   ;; right
    (0 -1)  ;; left
    (1 0)   ;; down
    (-1 0)) ;; up
  )

(defparameter **8-puzzle-cells**
  '((0 0)
    (0 1)
    (0 2)
    (1 0)
    (1 1)
    (1 2)
    (2 0)
    (2 1)
    (2 2)))

(defun 8-puzzle-solved? (position)
  (equalp position **8-puzzle-target**))

(defun 8-puzzle-h-val (position)  ;; This is the heuristic (h) function [modify for specific problem]
  (8-puzzle-manhattan-distance position))

(defun 8-puzzle-manhattan-distance (position)
  (loop for cell in **8-puzzle-cells**
     for val = (apply #'aref position cell)
     when (> val 0)
     sum (distance cell
                   (8-puzzle-find-loc val **8-puzzle-target**))))


(defun 8-puzzle-legal-move? (move position)
  (let ((blank-loc (8-puzzle-find-loc 0 position)))
    (apply #'array-in-bounds-p
           position
           (mapcar #'+ move blank-loc))))

(defun 8-puzzle-find-loc (val position)
  (loop for loc-cell in **8-puzzle-cells**
     when (= (apply #'aref position loc-cell)
             val)
       return loc-cell))


(defun 8-puzzle-successors (position)
  (loop for move in **8-puzzle-moves**
     when (8-puzzle-legal-move? move position)
     collect
       (8-puzzle-make-move move position)))

(defun 8-puzzle-make-move (move position)   ;; move assumed legal
  (let* ((blank-loc (8-puzzle-find-loc 0 position))
         (new-loc (mapcar #'+ move blank-loc))
         (new-loc-val (apply #'aref position new-loc))
         (new-position (copy-position position)))
    ;(print blank-loc)
    ;(print new-loc)
    ;(print new-loc-val)
    (setf (apply #'aref new-position blank-loc)
          new-loc-val)
    (setf (apply #'aref new-position new-loc)
          0)
    new-position))


(defun 8-puzzle-setup (&optional
                         (size **8-puzzle-size**)
                         (start **8-puzzle-start**)
                         (target **8-puzzle-target**))
  (setf **max-g** 100
        **max-h** 30
        **8-puzzle-size** size ;; maybe not needed
        **8-puzzle-target** target
        ;; for external-a-star
        **init-position** start
        **h-fun** #'8-puzzle-h-val
        **successors-fun** #'8-puzzle-successors
        **equality-test** #'equalp
        ))

 
