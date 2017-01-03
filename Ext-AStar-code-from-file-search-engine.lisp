;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VERSION NOTES:
;;;   file:  Ext-AStar-file-search-engine-<n>.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copied and adapted from /BreadthFirstFileSearch/file-search-engine-16.lisp
;;; cf. BreadthFirstFileSearch/file-search-engine-16.lisp for old version notes

;;; Version 1  (11-12-2016)
;;;   Change **heap-threshold** default from nil to 5
;;    Modify pathname functions to use bucket instead of fringe, and g h instead of depth
;;;   Modify MERGE-SEGMENTS to take g and h args in place of depth
;;;   Remove prior-duplicate-limit and use-parity? from COLLECT-PRIOR-BUCKETS
;;;   Modify NEW-OUTPUT-BUFFER to never try to reuse (use POINT-OUTPUT-BUFFER to re-use)
;;;   POINT-OUTPUT-BUFFER -- reset output-buffer to point to specified bucket
;;;          [still takes segment? arg for "next write segment" or NIL for no segments]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; USAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  Load files in order:
;;;       my-profiler-<n>.lisp
;;;       Ext-AStar-from-file-search-engine-<n>.lisp   (this file)
;;;       Ext-AStar-file-based-SBP-v6.lisp

;;;    cf. Ext-AStar-file-based-SBP-v6.lisp for full USAGE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODULAR DESIGN FOR FILE-SEARCH-ENGINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; SOLVER-MODULE

;;;    Search Engine
;;;      (maybe specify how many previous fringes to check for duplicates)
;;;               [NOTE: Relix and Vubu may require checking all to avoid inefficient loops
;;;                     but should still find optimal solutions  without complete checks - just will have larger fringes]
;;;        

;;; COMPARE BYTE-POSITIONS
;;;   return -1, 0, or 1 according to <, =, or >

;;; POSITION-BUFFER-MODULE

;;;  INPUT BUFFER
;;;    new-input-buffer (filespec)  ;; may reuse from a pool, if desired, or allocate in a LARGE VECTOR
;;;    get-front-position [returns a byte-position (sequence) -- read only!]
;;;    next-position  (must detect both "buffer-emptied" and EOF - will re-fill buffer from file when buffer emptied)
;;;              returns new front-position after "advancing"
;;;    advance-beyond-position (given-position)  (return T or NIL as to whether a position was passed that was equal to given-position)
;;;    close-buffer (close open file)

;;;  OUTPUT BUFFER 
;;;   NEW-OUTPUT-BUFFER (filespec)
;;;   CLOSE BUFFER (writes out any partial buffer remaining and closes file)
;;;   2-modes (depending on flag SEGMENTS?)
;;;      1. Single-File mode (Segments? = NIL)
;;;           When buffer full, simply appends buffer positions to single output file
;;;           Resets buffer (fill-pointer = 0)
;;;      2. Segment mode (Segments? = non-NIL value)
;;;           When buffer full, sorts buffer, and then writes as a new fringe segment (updates segment-count for next write)
;;;   WRITE-POSITION  [copies position to buffer - if buffer full will either append buffer to file (appending) or write as a new segment]


;;;   MERGE-SEGMENTS [Function  (merge-segments <depth>)]
;;;        Does a merge-sort of all fringe-segments at given depth
;;;        While merging, it eliminates all duplicates (both internal and those that appear in "specified earlier fringes")


;;; DOMAIN-SPECIFIC INTERFACE
;;;    GENERATE-SUCCESSORS (<position> <output-buffer>) --> <write (byte-encoded) successors to output buffer>
;;;          Also should CHECK FOR SOLUTION(S)
;;;    PUZZLE-INIT
;;;         - sets up support data structures for position representation - both byte and normal
;;;         - creates start-position (or start-position-list)
;;;    SOLVED? <position>   -> T or NIL
;;;    
;;;    Compressed Position Representation:
;;;     [try to localize this to domain implementation so file-search-engine can operate ONLY on compressed positions]
;;;      Byte-sequence (for compressed position)
;;;         FIXED size (# bytes) for this encoding
;;;      BYTE-ENCODE-POSITION (<domain-position>)  --> <byte-position>
;;;      BYTE-DECODE-POSITION (<byte-position>) --> <domain-position>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter **open** nil)

(defparameter **candidate-position-register** nil) ;; allocated later based on **position-size**

(defparameter  **compressed-solution-sequence** nil)  ;; for holding list of byte-positions in recovered solution

(defparameter  **heap** nil)     ;; this will hold the heap array to be used for merging 
                                 ;;   setup once and reuse (check if need to grow it larger?)

(defparameter **heap-threshold** 5)   ;; nil => don't use heap ever, n => use heap if segment-count > n

(defparameter **max-buffer-position-count** 100000000) ;; make this as large as feasible once everything is working!

(defparameter **free-input-buffers**     ;; for reusing input-buffers
  (make-array 8200 :initial-element nil))    ;; ran out with 1000 on level 117 of BoxedIn 1-46
					     ;;  Climb15a exhausted 2000 with buff=50000 pos's

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOMAIN INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SHARED GLOBALS 
(defparameter **solution-position** nil)    ;; This should be a COMPRESSED SOLUTION POSITION  (set when found in GENERATE-SUCCESSORS)
(defparameter **byte-size** 8)              ;; can vary for possible effiecinecy according to domain
(defparameter **position-size** 4)      ; should be set by domain-init
					; (count of bytes of size **byte-size**)
(defparameter **final-8bit-byte-position-size** nil)   ;; size of position in 8-bit bytes (compute in setup since might differ from **8bit-byte-position-size** calculated by domain)

(defparameter **start-pos-list** nil)       ;; compressed list of start positions (set by domain-init)
(defparameter **puzzle-name** nil)  ;; should be set by puzzle or domain
(defparameter **puzzle-directory-name** "DUMMY_DIRECTORY_NAME")  ;; should be set by puzzle or domain
(defparameter **exper-tag** nil) ;; if non-nil, insert in name of puzzle search folder 
                                 ;; (keep experiment results / files separate)
(defparameter **moves-invertible?** t)   ;; should always set T or NIL for each domain-init

;; eliminate for Ext-A* (always lookback 2, no full-dup-elim, no parity)
#|
(defparameter **prior-fringe-lookup-limit** nil)    ;; nil = look all the way, 2 looks back only as far as prior-fringe?
(defparameter **force-full-dup-elim-interval** nil)  ;; never force full pruning (if 10 do every 10th gen, for exaple)  -- sliding block puzzles should be nil (never need to look back all the way)
(defparameter **use-parity?** nil)          ;; parity hack for BoxedIn  (nil -> 1, T -> 2)
|#

;;; DOMAIN FUNCTIONS to be implemented (and called from this engine)

;;; (defun GENERATE-SUCCESSORS (<compressed-position> <output-buffer>)
;;;     ;; implementation here
;;;     ;;  expands given position (uncompresses and generates domain-successors), then compresses and writes to outut-buffer
;;;     ;;  returns first solution-position found (as a COMPRESSED POSITION), also sets **solution-

;;; Define when moves are NOT-INVERTIBLE:
;;;    (defun COLLECT-SUCCESSORS (<compressed-position>)  ... )
;;;      ;; returns a list of compressed-successors  (no writing to output-buffer so that arg is omitted)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-byte-vector (length)
  (make-array length :element-type (list 'unsigned-byte **byte-size**) :initial-element 0))

;;; Position Comparison

;; predicate
(defun compare-positions-for-sort (pos1 pos2)  ;; assumes both pos1 and pos2 are byte-sequence positions
  (loop for byte1 across pos1
        for byte2 across pos2
        while (= byte1 byte2)
        finally
        (return (< byte1 byte2))))

;;;    Returns -1 0 or 1 depending
(defun compare-positions-3-way (pos1 pos2)
  (loop for byte1 across pos1
        for byte2 across pos2
        while (= byte1 byte2)
        finally
        (return
         (cond ((< byte1 byte2)  -1)   ;; <
               ((= byte1 byte2)   0)   ;; =
               (t   1)                 ;; >
               ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter **path-to-file-storage**
  ;"/Users/glenniba/temp/SEARCH-FILE-STORAGE/"
  ; /Volumes/EXT1-OTHER/SEARCH-FILE-STORAGE/"
  "/Volumes/EXT-3TB-B/SEARCH-FILE-STORAGE/"
  ; "/Volumes/Seagate6TB/SEARCH-FILE-STORAGE/"
  )

;; convenience function
;;   Note: can "manually set directory" with SET-DIR
;;           then if no :exper-tag given to run-experiment,**puzzle-directory-name** will not be changed
(defun set-dir (dir-name)    ;; dir-name is a string naming the directory to be used for file-search
  (setf **puzzle-directory-name**
	dir-name))

(defun puzzle-directory-pathname ()
  (file-storage-pathname (format nil "~a/" 
				 **puzzle-directory-name**)))

(defun puzzle-directory-file-list ()
  (directory (file-storage-pathname (format nil "~a/*.*" **puzzle-directory-name**))))

(defun file-storage-pathname (local-path-string)
  (format nil "~a~a" **path-to-file-storage** local-path-string))

;; was fringe-pathname
(defun bucket-pathname (g h)
  (format nil "~a~a/bucket-~a-~a.data" **path-to-file-storage** **puzzle-directory-name** g h))

;; was fringe-segment-pathname
(defun bucket-segment-pathname (g h segment-number)
  (format nil "~a~a/bucket-~a-~a-segment-~a.data" 
	  **path-to-file-storage** **puzzle-directory-name** g h segment-number))

;; was fringe-exists?
(defun bucket-exists? (g h)
  (and (array-in-bounds-p **open** g h)
       (probe-file (bucket-pathname g h))))

;; was fringe-segment-exists?
(defun bucket-segment-exists? (g h segment-number)
  (probe-file (bucket-segment-pathname g h segment-number)))

;; was sum-fringe-segment-byte-sizes
(defun sum-bucket-segment-byte-sizes (g h)
  (loop for seg-num from 1
        while (bucket-segment-exists? g h seg-num)
        sum
       (get-file-size (bucket-segment-pathname g h seg-num))))

(defun sum-bucket-segment-position-sizes (g h)
  (loop for seg-num from 1
        while (bucket-segment-exists? g h seg-num)
        sum
        (get-file-position-size (bucket-segment-pathname g h seg-num))))

(defun open-input-file-stream (pathname)
  (open  pathname
         :direction :input
         :element-type (list 'unsigned-byte **byte-size**) 
         :if-does-not-exist :error))

(defun open-output-file-stream (pathname)
  (open pathname
        :direction :output
        :element-type (list 'unsigned-byte **byte-size**)
        :if-does-not-exist :create
        :if-exists :error))       ;; changed from :supersede to :error (11-14-16)

(defun get-file-size (pathname)   ;; size in bytes
  (with-open-file (infile pathname :direction :input :if-does-not-exist :error)
    (file-length infile)))

(defun get-file-position-size (pathname)  ;; assume exists, size count of positions
  (/ (get-file-size pathname)
     **final-8bit-byte-position-size**))

;; was get-fringe-byte-size
(defun get-bucket-byte-size (g h)
  (get-file-size (bucket-pathname g h)))

;; was get-fringe-position-size
(defun get-bucket-position-size (g h &optional (pos-size **final-8bit-byte-position-size**)) ;; 8bit bytes
  (cond ((probe-file (bucket-pathname g h))
         (/ (get-bucket-byte-size g h)  pos-size))   ;; need to have size in 8bit-bytes
        (t 'unknown)))

;; won't work if files get deleted  -- maybe use **open** data for this ?
;;  need to re-work for 2-d open array ??
#|
(defun display-bucket-sizes (&optional (start-depth 0))
  (loop for depth from start-depth
        for fringe-path = (fringe-pathname depth)
        while (probe-file fringe-path)
        do
        (print (list depth (get-fringe-position-size depth)))))
|#

;; was  format-file-info-for-depth
(defun format-file-info-for-bucket (g h)
  (let* ((generated-positions
	  (/ (sum-bucket-segment-byte-sizes g h)   ;; won't work after segments deleted
	     **final-8bit-byte-position-size**))     ;; BUG FIX: was **position-size**
         (bucket-positions (get-bucket-position-size g h))
         (ratio (cond ((and (numberp bucket-positions)
			    (> bucket-positions 0)) ;; don't divide by 0
                       (float (/ generated-positions bucket-positions)))
                      (t 'unknown))))
    (format t "~%            Bucket = (~a ~a),  ~a proto-positions yielded ~a fringe positions (ratio = ~a)"
            g h
            generated-positions
            bucket-positions
            ratio)))

#|
  (loop for depth from 0 to 104
for generated-positions = (/ (sum-fringe-segment-byte-sizes depth) **position-size**)
for fringe-positions = (get-fringe-position-size depth)
for ratio = (cond ((numberp fringe-positions)
(float (/ generated-positions fringe-positions)))
(t 'unknown))
do
(format t "~% Depth = ~a,  ~a proto-positions yielded ~a fringe positions (ratio = ~a)"
depth
generated-positions
fringe-positions
ratio))
  |#

#|
;; was delete-fringe-segments
(defun delete-bucket-segments (g h)
  (loop for segnum from 1
        while
        (bucket-segment-exists? g h segnum)
        do
        (delete-file (bucket-segment-pathname g h segnum))))

(defun delete-bucket (g h)
  (when (bucket-exists? g h)   ;; TODO - check for name-conflict with EXT-A* code
    (delete-file (bucket-pathname g h))))
|#


;;; debugging files

#|
(defun find-first-diff (pathname1 pathname2)
  (loop
     with inbuff1 = (new-input-buffer pathname1)
     with inbuff2 = (new-input-buffer pathname2)
     for pos1 = (get-front-position inbuff1) then (next-position inbuff1)
     for pos2 = (get-front-position inbuff2) then (next-position inbuff2)
     while (and pos1 pos2             ;; terminate if either file ends  
		(equalp pos1 pos2))  
     finally
       (return (list pos1 pos2))))    ;; Note: should return (NIL NIL) if files identical
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INPUT BUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass input-buffer ()
  ((position-size :initform 2 :accessor position-size)
   (front-position :initform nil :accessor front-position)   ;; one of the positions in vector-of-positions
   (front-position-index :initform nil :accessor front-position-index)
   (byte-vector :initform nil :accessor byte-vector)
   (vector-of-positions :initform nil :accessor vector-of-positions)
   (vector-position-length :initform nil :accessor vector-position-length)
   (vector-pos-end :initform nil :accessor vector-pos-end)
   (buffer-empty? :initform nil :accessor buffer-empty?)
   (input-buffer-file-stream :initform nil :accessor input-buffer-file-stream)))

(defparameter **input-buffer-position-count** 1000)

(defun new-input-buffer (filepath  &optional (pos-size **position-size**) (position-count **input-buffer-position-count**))
  ;; as of verion 8, there actually is a buffer
  (let ((new-inbuff (make-instance 'input-buffer)))
    (with-slots (position-size front-position byte-vector vector-of-positions vector-position-length
			       input-buffer-file-stream)
        new-inbuff
      (setf position-size pos-size
	    vector-position-length position-count
            input-buffer-file-stream (open-input-file-stream filepath))
      (setf byte-vector (make-byte-vector (* position-size vector-position-length)))
      (setf vector-of-positions
	    (make-array vector-position-length))
      (loop for pos-i from 0 below vector-position-length
	   for byte-index from 0 by position-size
	   do
	   (setf (aref vector-of-positions pos-i)
		 (make-array position-size
			     :element-type (list 'unsigned-byte **byte-size**)
			     :displaced-to byte-vector
			     :displaced-index-offset byte-index)))
      (refill-buffer new-inbuff)
      new-inbuff)))

;; assumes that position-size and position-count will be same as before
(defun reuse-input-buffer (inbuff filepath)
  (close-buffer inbuff)   ;; in case still open
  (with-slots (position-size front-position byte-vector vector-of-positions vector-position-length
			     input-buffer-file-stream)
      inbuff
    (setf input-buffer-file-stream (open-input-file-stream filepath))
    (refill-buffer inbuff)
    inbuff))
  
(defmethod refill-buffer ((inbuff input-buffer))
  ;; fill byte-buffer from file stream
  (with-slots (byte-vector input-buffer-file-stream position-size buffer-empty? 
			   vector-pos-end
			   vector-of-positions
			   front-position
			   front-position-index
			   )
      inbuff
    (let ((seq-pos (read-sequence byte-vector input-buffer-file-stream)))
      (setf vector-pos-end (/ seq-pos position-size))
      (setf front-position (aref vector-of-positions 0))
      (setf front-position-index 0)
      (setf buffer-empty?
	    (= 0 seq-pos)))))   ;; if 0, nothing was read - so buffer-empty? set to T, otherwise nil



;; returns position-object if valid, otherwise NIL if buffer empty
;;  NOTE: must COPY front-position to retain contents (next read will overwrite sequence)
(defmethod get-front-position ((inbuff input-buffer))
  (with-slots (buffer-empty? front-position)
      inbuff
    (cond (buffer-empty?
           nil)
          (t front-position))))

(defmethod next-position ((inbuff input-buffer))
  (with-slots (buffer-empty? vector-of-positions front-position front-position-index vector-pos-end)
      inbuff
    (unless buffer-empty?
      (incf front-position-index)
      (cond ((< front-position-index vector-pos-end)
	     (setf front-position (aref vector-of-positions front-position-index)))   ; return front-position
	    (t
	     (refill-buffer inbuff)
	     (get-front-position inbuff))))))

(defmethod close-buffer ((inbuff input-buffer))
  (with-slots (buffer-empty? input-buffer-file-stream)
      inbuff
    (setf buffer-empty? t)
    (close input-buffer-file-stream)))  ;; ok to close multiple times (returns T then NIL)


(defmethod advance-beyond-position ((inbuff input-buffer) position)
  ;; Reads positions from inbuff, discarding all positions <= position
  ;; return T or NIL as to whether a position was passed that was equal to position  (NIL also in case of EOF)
  (loop with equal-position-found? = nil
        for try-position = (get-front-position inbuff) then (next-position inbuff)
        for compare-val = (and try-position
                               (compare-positions-3-way try-position position))
        until (or (null compare-val)
                  (= compare-val 1))   ; try-position is > position
        do
        (when (eql compare-val 0)
          (setf equal-position-found? t))
        finally
        (return equal-position-found?)))

;; Version of advance that doesn't check for equality (so hopefully will be faster?)
(defmethod simple-advance-beyond-position ((inbuff input-buffer) position)
  ;; Reads positions from inbuff, discarding all positions <= position
  ;; DOES NOT check for equality
  (loop for try-position = (get-front-position inbuff) then (next-position inbuff)
        until (or (null try-position)  ;; inbuff is empty
                  (compare-positions-for-sort position try-position))   ;; try-position is > position
       ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  OUTPUT BUFFER 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass output-buffer ()
  (; (position-size :initform nil :accessor position-size)  ;; needed for allocating new positions?  (or simply copy-seq the position being written)
   (buffer-size :initform nil :accessor buffer-size)    ; in positions (will hold byte-position vectors/sequences)
   (buffer-vector :initform nil :accessor buffer-vector)    ; with fill-pointer, initially 0
   (write-segments? :initform nil :accessor write-segments?) ; T means sort and write to new segment, NIL means don't sort and append to singel output stream
   (segment-count :initform nil :accessor segment-count)   ; not used except with segments
   ;(fringe-depth :initform nil :accessor fringe-depth)
   (bucket-g :initform nil :accessor bucket-g)
   (bucket-h :initform nil :accessor bucket-h)
   (output-filepath :initform nil :accessor output-filepath)
   (output-buffer-file-stream :initform nil :accessor output-buffer-file-stream)))


;(defparameter **output-buffer** nil)
(defparameter **out-buff-0** nil)
(defparameter **out-buff-1** nil)
(defparameter **out-buff-2** nil)

#|
(defun init-out-buffs (&optional (max-position-count **max-buffer-position-count**))
  (setf **out-buff-0**
	(init-out-buff **out-buff-0** max-position-count))
  (setf **out-buff-1**
	(init-out-buff **out-buff-1** max-position-count))
  (setf **out-buff-2**
	(init-out-buff **out-buff-2** max-position-count)))

(defun init-out-buff (out-buff &optional (max-position-count **max-buffer-position-count**))
  (cond ((and out-buff
	      (= (length (buffer-vector out-buff))
		 max-position-count))
	 (unless (pos-type-ok? (aref (buffer-vector out-buff) 0))
	   (fill (buffer-vector out-buff) nil))
	 out-buff)  ;; return the out-buff
	(t    ;; can't re-use so create-new output-buffer
	 (new-output-buffer max-position-count))))

(defun pos-type-ok? (pos)
  (and pos
       (= (length pos) **position-size**)
       (equal (array-element-type pos)
	      (list 'unsigned-byte **byte-size**))))
|#
       

;; moved earlier to avoid compiler error
;;; (defparameter **max-buffer-position-count** 10000000) ;; make this as large as feasible once everything is working!


;;; NEVER RE-USE buffer-vector - only use NEW-OUTPUT-BUFFER for init
;;;   NOW ONLY MAKES INSTANCE & SETS BUFFER-VECTOR
;;;     -- all other slots set by POINT-OUTPUT-BUFFER
#|
(defun new-output-buffer (&optional 
			    (max-position-count **max-buffer-position-count**))
  (let ((output-buffer
	 (make-instance 'output-buffer)))
    (with-slots (buffer-size buffer-vector)
	output-buffer
      (setf buffer-size max-position-count)
      (setf buffer-vector
	    (make-array buffer-size :initial-element nil :fill-pointer 0)))
    output-buffer ;; return the output-buffer !!
    ))

(defun point-output-buffer (out-buff g h next-segment?)
  ;; TODO -- update data in **open** ?
  (with-slots (buffer-vector buffer-size write-segments? bucket-g bucket-h output-filepath
			     segment-count output-buffer-file-stream)
      out-buff
    (setf (fill-pointer buffer-vector) 0)  ;; reset the buffer-vector !!
    (setf bucket-g g
	  bucket-h h)
    (setf write-segments?
	  (if next-segment?
	      t
	      nil))
    (cond (write-segments?
	   (setf segment-count next-segment?)
	   (setf output-filepath
		 (bucket-segment-pathname bucket-g bucket-h segment-count))
	   (setf output-buffer-file-stream
		 (open-output-file-stream output-filepath)))
	  (t   ;; no segments
	   (setf segment-count nil)    ;; elim. spurious data from before repointing
	   (setf output-filepath
		 (bucket-pathname bucket-g bucket-h))
	   (setf output-buffer-file-stream
		 (open-output-file-stream output-filepath))))
    out-buff ;; return the output-buffer !!  (so can use for value, not just side-effect)
    ))
      

(defmethod write-position ((outbuff output-buffer) position)
  ; write-position copies position into position-element in buffer-vector - if buffer full will flush to file and update
  (with-slots (buffer-vector buffer-size write-segments? bucket-g bucket-h output-filepath
			     segment-count output-buffer-file-stream)
      outbuff
    (when (>= (fill-pointer buffer-vector) buffer-size)   ;; buffer is full!
      ;; this next is now responsible for sorting when needed
      (write-buffer-vector-to-output-stream outbuff)
      ;; Update file / segments
      (when write-segments?
        (close output-buffer-file-stream)
        (incf segment-count)
        (setf output-filepath (bucket-segment-pathname bucket-g bucket-h segment-count))
        (setf output-buffer-file-stream
              (open-output-file-stream output-filepath)))
      ;; either way, reset buffer-vector
      (setf (fill-pointer buffer-vector) 0)   ;; don't need to empty buffer, since elements will get over-written
      )
    ;; whether or not buffer WAS full, it should NOT be full now, and we still need to WRITE Position
    (replace (get-target-position buffer-vector)  ;; this will allocate and store a new position if one not previously stored
             position)
    (incf (fill-pointer buffer-vector))  ;; fill-pointer always points to next write location
    ))

;;; NOTE: this now SORTS buffer before write, when write-segments? is T
(defmethod write-buffer-vector-to-output-stream ((outbuff output-buffer))
  ;; specific to buffer format with byte-position vector in each entry of buffer-vector
  ;;  will only write from 0 to fill-pointer
  (with-slots (buffer-vector output-buffer-file-stream write-segments?)
      outbuff
    (cond (write-segments? ;; only sort if writing a segment, otherwise should already be sorted
	   (start-timing 'expand-sort-buffer)
	   (sort buffer-vector #'compare-positions-for-sort)
	   (stop-timing 'expand-sort-buffer)
	   ;; write eliminating duplicates
	   (start-timing 'expand-write-buffer)
	   (loop for last-byte-pos = nil then byte-pos
	      for byte-pos across buffer-vector
	      unless (equalp byte-pos last-byte-pos)
	      do
		(write-sequence byte-pos output-buffer-file-stream))
	   (stop-timing 'expand-write-buffer)
	   )
	  (t (start-timing 'reduce-write-buffer)
	     (loop for byte-pos across buffer-vector ; just write everything out (no duplicates anyway)
		do
		  (write-sequence byte-pos output-buffer-file-stream))
	     (stop-timing 'reduce-write-buffer)
	     )))
  ;; Does NOT close or update, since may want to keep appending to outstream (if no write-segments?)
  )

(defun get-target-position (buffer-vector)
  ;; checks for, and allocates if necessary, a byte-position
  ;; returns byte-position object at location of fill-pointer
  (let* ((fill-point (fill-pointer buffer-vector))
         (target-pos (aref buffer-vector fill-point)))
    (cond (target-pos) ;; T means ok, so return target-pos
          (t           ;; NIL means need to allocation a new one
            (setf (aref buffer-vector fill-point)
                  (make-byte-vector **position-size**)))  ;; returns the byte-vector setf to fill-point loc
          )))


;; only call when completely done with buffer!
(defmethod close-buffer ((outbuff output-buffer))
  ;;;   close buffer (write out any partial buffer remaining, and close file)
  (with-slots (buffer-vector output-buffer-file-stream write-segments? segment-count 
			     bucket-g bucket-h)
      outbuff
    ;; if there is anything still in buffer
    (when (< 0 (fill-pointer buffer-vector))
      ;; next will sort if needed
      (write-buffer-vector-to-output-stream outbuff))
    (when write-segments?
      (store-open-info bucket-g bucket-h :segment-count segment-count))
    (close output-buffer-file-stream)))

|#
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MERGING SEGMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
;;; this decides which version (heap or not) should be used based on **heap-threshold**
;;;   Now records data and deletes segments
(defun merge-segments (g h)
  ;; check for valid bucket ?
  (when (and (array-in-bounds-p **open** g h)
	     (not (probe-file (bucket-pathname g h))))
    (start-timing 'reduce)
    (let ((segment-filepaths
	   (loop for segment-num from 1
	      for segment-path = (bucket-segment-pathname g h segment-num)
	      while (probe-file segment-path)
	      collect segment-path)))
      (cond ((and **heap-threshold**  
		  (>= (length segment-filepaths) **heap-threshold**)) ;; use heap
	     (merge-segments-heap g h segment-filepaths))
	    (t ;; merge without heap
	     (merge-segments-no-heap g h segment-filepaths))))
    ;; store sum-segment-count and merged-segment-count
    (store-open-info g h
		     :summed-segment-positions
		     (sum-bucket-segment-position-sizes g h))
    (store-open-info g h
		     :merged-position-count
		     (get-bucket-position-size g h))
    (delete-bucket-segments g h)
    (clear-open-bucket (- g 2) h)   ;; no longer needed
    (stop-timing 'reduce)))

(defun merge-segments-heap (g h
			    &optional
			    (segment-filepaths-passed-in nil))
  (format t "~%Using HEAP to Merge ~a segments" (length segment-filepaths-passed-in))
  ;; collect-segment filenames
  (let* ((debug? nil)
         (segment-filepaths
	  (if segment-filepaths-passed-in
	      segment-filepaths-passed-in
	      (loop for segment-num from 1
		 for segment-path = (bucket-segment-pathname g h segment-num)
		 while (probe-file segment-path)
		 collect segment-path)))
         (previous-bucket-filepaths
	  (collect-prior-buckets g h)
	   ))
    (when debug? (print segment-filepaths))
    (loop with segment-input-buffers = (allocate-or-reuse-input-buffers segment-filepaths 0)

       with prior-bucket-input-buffers =  (allocate-or-reuse-input-buffers 
					   previous-bucket-filepaths
					   (length segment-filepaths))
       with output-buffer = (point-output-buffer **out-buff-0**  ;; re-use 
						 g h
						 nil) ; NIL = no segments  -- note: re-use defaults to T
       initially
       (setup-heap-for-merge segment-input-buffers)
       ;; for candidate-pos = (minimize-front-position segment-input-buffers) ;; NIL if all buffers empty
       for heap-front-pos = (get-heap-front-position) ;; NIL if all buffers empty
       for candidate-pos = (when heap-front-pos	;; have actual position
			     (replace **candidate-position-register** ;; make copy so doesn't get clobbered by advance
				      heap-front-pos))
       while candidate-pos ;; candidate-pos only NIL if all buffers empty, so DONE!
       do
       (when debug?
	 (print candidate-pos))
       ;; advance all prior-buckets past candidate-pos, T if equal found
       (unless (advance-all-in-buffers-check-equality? prior-bucket-input-buffers
						       candidate-pos)
	 ;; only write if there was no duplicate in a prior-bucket
					;(format t "writing out position ~a in merge-segments" candidate-pos)
	 (write-position output-buffer candidate-pos))
       ;; advance all segment-buffers past candidate-pos  [Note: prior-buckets already advanced]
       ;; (advance-all-in-buffers segment-input-buffers candidate-pos)
       (advance-heap-beyond-position candidate-pos)
       finally
       (loop for inbuff in segment-input-buffers
	  do
	  (close-buffer inbuff))
       (loop for inbuff in prior-bucket-input-buffers
	  do
	  (close-buffer inbuff))
       (close-buffer output-buffer)
       )))

(defun merge-segments-no-heap (g h
		       &optional
		       (segment-filepaths-passed-in nil))
  (format t "~%Merging ~a segments" (length segment-filepaths-passed-in))
  ;; collect-segment filenames
  (let* ((debug? nil)
         (segment-filepaths
	  (if segment-filepaths-passed-in
	      segment-filepaths-passed-in
	      (loop for segment-num from 1
		 for segment-path = (bucket-segment-pathname g h segment-num)
		 while (probe-file segment-path)
		 collect segment-path)))
         (previous-bucket-filepaths
	  (collect-prior-buckets g h)
	  ))
    (when debug? (print segment-filepaths))
    (loop with segment-input-buffers = (allocate-or-reuse-input-buffers segment-filepaths 0)
       with prior-bucket-input-buffers = (allocate-or-reuse-input-buffers 
					  previous-bucket-filepaths
					  (length segment-filepaths))
       with output-buffer = (point-output-buffer **out-buff-0**  ;; re-use
						 g h
						 nil) ; NIL = no segments  -- note: re-use defaults to T
       for candidate-pos = (minimize-front-position segment-input-buffers) ;; NIL if all buffers empty
       while candidate-pos ;; candidate-pos only NIL if all buffers empty, so DONE!
       do
       (when debug?
	 (print candidate-pos))
       ;; advance all prior-buckets past candidate-pos, T if equal found
       (unless (advance-all-in-buffers-check-equality? prior-bucket-input-buffers 
						       candidate-pos)
	 ;; only write if there was no duplicate in a prior-bucket
					;(format t "writing out position ~a in merge-segments" candidate-pos)
	 (write-position output-buffer candidate-pos))
       ;; advance all segment-buffers past candidate-pos  [Note: prior-buckets already advanced ]
       (advance-all-in-buffers segment-input-buffers candidate-pos)
       finally
       (loop for inbuff in segment-input-buffers
	  do
	  (close-buffer inbuff))
       (loop for inbuff in prior-bucket-input-buffers
	  do
	  (close-buffer inbuff))
       (close-buffer output-buffer)
       )))
|#

(defun collect-prior-buckets (g h)
  (loop with least-depth-prior-fringe = (- g 2)  ;; look at previous 2 buckets with same h
     for prior-g from (1- g) downto least-depth-prior-fringe
     for prior-bucket-path = (bucket-pathname prior-g h)
     when (probe-file prior-bucket-path)   ;; changed while to when (should work the same)
     collect prior-bucket-path)
  )

(defun allocate-or-reuse-input-buffers (list-of-input-filepaths &optional (start-inbuff-index 0))
  (loop for buff-index from start-inbuff-index
       for filepath in list-of-input-filepaths
       for inbuff = (aref **free-input-buffers** buff-index)
       do
       (cond (inbuff
	      (reuse-input-buffer inbuff filepath))
	     (t   ;;need to allocate new inbuff
	      (setf inbuff (new-input-buffer filepath))    ;; allocate new inbuff
	      (setf (aref **free-input-buffers** buff-index)  ;; store it in free inbuffs
		    inbuff)))
       collect
       inbuff))

          
(defun advance-all-in-buffers (in-buffer-list candidate)
  (loop for inbuff in in-buffer-list
        do
        (simple-advance-beyond-position inbuff candidate)))

(defun advance-all-in-buffers-check-equality? (in-buffer-list candidate)
  (loop with equality-found? = nil
        for inbuff in in-buffer-list
        for equality? = (advance-beyond-position inbuff candidate)
        do
        (setf equality-found? (or equality-found? equality?))
        finally
        (return equality-found?)))

(defun minimize-front-position (in-buffer-list)
  ;;; Maybe later do this with a heap
  ;;; For now, keep simple and just loop through list minimizing
  (loop with min-pos = nil
        for inbuff in in-buffer-list
        for next-front = (get-front-position inbuff)
        do
        (setf min-pos (min-position min-pos next-front))
        finally
        (return
         (cond (min-pos
                (replace **candidate-position-register**   ;; need copy, else may get clobbered while advancing
                         min-pos))
               (t nil)))))

(defun min-position (pos1 pos2)    ;; either or both may be nil
  (cond ((null pos1) pos2)   ;; ok whether pos1 is nil or actual position
        ((null pos2) pos1)
        ((compare-positions-for-sort pos1 pos2)
         pos1)       ;; choose pos1 if strictly less than pos2
        (t pos2)))   ;;  otherwise pos2, works even if positions are equal


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEBUG FILES UTILITY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fancy-display-file-positions (pathname)
  (loop with inbuff = (new-input-buffer  pathname)
     for pos = (get-front-position inbuff) then (next-position inbuff)
     while pos
     do
       (terpri)
       (print pos)
       (fancy-display-compressed-position pos)   ;; this uncompresses pos
       (format t "~%  H-value = ~a" (t-piece-h-fun))  ;; this uses intermediate-position info
     finally
       (close inbuff)
       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  SEARCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; SAVE THIS -- BUT NEEDS TO BE UPDATED
#| 
(defun write-initial-fringe (start-position-list &optional (depth 0))
  ;; needs to be sorted!
  (loop with outbuff = (new-output-buffer depth 1)   ;; write as segments, in case too large to fit in buffer all at once
     for byte-pos in start-position-list
     do
     (write-position outbuff byte-pos)
     finally
     (close-buffer outbuff)
     (merge-segments depth)  
     (delete-fringe-segments depth)))
  
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TIMING FUNCTIONS MOVED ->  MY-PROFILER.LISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RECOVER SOLUTION SEQUENCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  NOTE: this will ONLY work when MOVES ARE INVERTIBLE (as in SBP)
;;;     Won't work for Relix or Vubu (unless can write a "predecessor function")
;;;   OK - Fixed now - by making find-prior-position branch on **moves-invertible?**
;;;      (and using either find-prior-position-if-invertible or find-prior-position-not-invertible)

;;; SAVE -- though not used now -- update for solution recovery when needed
#|
(defun recover-solution-sequence (final-depth final-position)   ;; note works with compressed (byte) positions
  (loop with compressed-solution-sequence = (list final-position)
        for depth from final-depth downto 1
        for prior-position = (find-prior-position depth (copy-seq (first compressed-solution-sequence)))
        while prior-position
        do
        (push prior-position compressed-solution-sequence)
        finally
        (return compressed-solution-sequence)
        ))

(defun find-prior-position (depth position)
  (if **moves-invertible?**
      (find-prior-position-if-invertible depth position)
      (find-prior-position-not-invertible depth position)))

(defun find-prior-position-if-invertible (depth position)    ;; finds the first successor of position that is in the prior-fringe
  (let ((temp-output-buffer (new-output-buffer 'temp 1)))
    ;; this next writes out successors of position to temp-output-buffer as segments
    (generate-successors position temp-output-buffer nil)  ;; nil signals generate-successors to ignore solution-checking
    (close-buffer temp-output-buffer)   ;; force writing out final segment
    (merge-segments 'temp) ;; read in the TEMP segments and merge into a single TEMP Fringe (won't check prior fringes since 'temp is non-numeric)
    (find-common-position 'temp (1- depth))   ;; this reads in the TEMP fringe file, and the fringe at (1- depth), returning 1st common position, if any
    ))

;; find prior-position when operations are NOT INVERTIBLE!
(defun find-prior-position-not-invertible (depth position) ;; depth is the depth of fringe with position
  ;; iterate through positions of fringe (1- depth)
  (format t "~%~%Looking for parent position in fringe at depth ~a"  (1- depth))
  (loop with start-prior-seek-time = (get-universal-time) ;; seconds
     with prior-fringe-filename = (fringe-pathname (1- depth))
     with prior-inbuff = (when (probe-file prior-fringe-filename)
			   (new-input-buffer prior-fringe-filename))
     for prior-fringe-position = (get-front-position prior-inbuff) then (next-position prior-inbuff) ; nil=EOF
     while prior-fringe-position
     when (member position (collect-successors prior-fringe-position) ;; collect-successors defined in Domain
		  :test #'equalp)
     return (progn
	      (format t "~%Parent found at depth ~a in ~a seconds"
		      (1- depth)
		      (- (get-universal-time)
			 start-prior-seek-time))
	      prior-fringe-position)))


(defun find-common-position (depth-a depth-b)
  (loop with filename-a = (fringe-pathname depth-a)
        with filename-b = (fringe-pathname depth-b)
        with inbuff-a = (when (probe-file filename-a)
                          (new-input-buffer filename-a))
        with inbuff-b = (when (probe-file filename-b)
                          (new-input-buffer filename-b))
        with common-position = nil    ;; set to 1st position occurring in both files
        for front-a = (get-front-position inbuff-a)     ; nil if EOF
        for front-b = (get-front-position inbuff-b)     ; nil if EOF
        while (and (not common-position)
                   front-a
                   front-b)   ;; if either is NIL then can stop - can't be any common position
        do
        (case (compare-positions-3-way front-a front-b)
          (-1 ; a < b
           (when (advance-beyond-position inbuff-a front-b)  ;; checks if equal position found
             (setf common-position (copy-seq front-b))))   ;; since found position equal to front-b
          (0  ; a = b !!   Found a match, so return it!
           (setf common-position (copy-seq front-a)))
          (1  ; a > b
           (when (advance-beyond-position inbuff-b front-a)  ;; checks if equal position found
             (setf common-position (copy-seq front-a))))
          (t (error "compare-positions-3-way returned bad value")))
     finally
       (close-buffer inbuff-a)
       (close-buffer inbuff-b)
       (return common-position)))   ;; NIL if none found, else the position itself
|#

(defun fancy-display-compressed-solution-sequence (&optional (compressed-solution-sequence **compressed-solution-sequence**))
  (loop for byte-pos in compressed-solution-sequence
        do
        (fancy-display-compressed-position byte-pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ITERFACE FUNCTIONS TO HEAP - FOR MERGING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; MACROS need to be defined before referenced

;; macro so will be setf-able
;;   (should only be used in heap-implementation code -- else need to define macros earlier in file)
(defmacro heap-size (heap)
  `(aref ,heap 0))

;; Don't know why this needs to be a macro (except eventually for efficiency?)
(defmacro heap-empty? (heap)
  `(< (heap-size ,heap) 1))

;;;   Node indexing:
;;;        (Eventually these could become macros or get in-lined)
;;;     Given a node index i, compute:

(defmacro parent (i)  ;; parent node index (up the tree structure)
  `(floor ,i 2))

(defmacro left (i)   ;; left child node index
  `(* 2 ,i))

(defmacro right (i)   ;; right child node index
  `(1+ (* 2 ,i)))


;;;;;;;;;;;;;;;;
;;;;; Interface functions (between search and heap implementation):

(defun setup-heap-for-merge (inbuff-list)
  (setf (heap-size **heap**) 0)   ;; empty for reuse
  (loop for inbuff in inbuff-list
       for inbuff-front-pos = (get-front-position inbuff)
       when inbuff-front-pos  ;; only insert inbuff's which are non-empty (ie have a non-nil front-position)
       do
       (heap-insert **heap** inbuff))
  )

(defun get-heap-front-position ()
  (let ((front-inbuff (heap-maximum **heap**)))
    (when front-inbuff    ;; there are still 1 or more inbuffs in heap (otherwise return nil if heap empty)
      (get-front-position front-inbuff))))

(defun advance-heap-beyond-position (limit-pos)
  ; ...    note: this should remove inbuffs once they are empty
  ;; logic:
  ;;   until heap-front-pos is beyond limit-pos
  ;;     do
  ;;      advance heap-front  (if becomes empty, remove inbuff from heap)
  ;;      heapify (since front inbuff may be out of order after advancing)
  (loop for max-inbuff = (heap-maximum **heap**)
       for max-inbuff-front-pos = (when max-inbuff (get-front-position max-inbuff))
       until (or (heap-empty? **heap**)
		 (compare-positions-for-sort limit-pos max-inbuff-front-pos))
       do
       (simple-advance-beyond-position max-inbuff limit-pos)
       (cond ((get-front-position max-inbuff)  ;; max-inbuff is not empty
	      (heapify **heap** 1))
	     (t    ;; max-inbuff IS empty!  So remove it from heap
	      (heap-extract-max **heap**)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HEAP FOR USE IN MERGING SORTED SEGMENTS (PROTO-FRINGES)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Use functions for now, while debugging
;;    Later replace with macros or inlining for efficiency

;;  Code initially seeded from ".../LispProgs/hacks/toy-heap-2.lisp"


;;; Heap data structure:  Array
;;;   Root at index i=1
;;;   Attributes:
;;;     Heap-Length  = index of last valid position of the array  (Heap can occupy positions 1 to Length, but may use less than all)
;;;     Heap-size  = number of elements stored in Heap  (<= Length)
;;;        [Hack: Store the heap-size on Array[0]

(defun create-empty-heap (max-size)
  (let ((heap (make-array (1+ max-size))))
    (setf (aref heap 0) 0)   ;; heap is empty, i.e. 0 elements
    heap))

(defun heap-length (heap)
  (1- (length heap)))   ;;; index of last position in array
  
#|
;;; MOVED EARLIER AND CHANGED TO MACROS	;

;;;   Node indexing:			;
;;;        (Eventually these could become macros or get in-lined) ;
;;;     Given a node index i, compute:	;

  (defun parent (i)  ;; parent node index (up the tree structure) ;
(floor i 2))

  (defun left (i)   ;; left child node index ;
(* 2 i))

  (defun right (i)   ;; right child node index ;
(1+ (* 2 i)))
  |#

;;; HEAP PROPERTY:
;;;   A[Parent(i)] >= A[i]  for all i (other than root)
;;;  

;; need this to restore heap-property after advancing the front input-buffer
(defun heapify (heap i)
  ;;; assumes that left(i) and right(i) are already heaps!
  ;;;   manipulates the "tree" to make i itself a heap (may swap and move entries around)
  ;;;  [note: i indexes a location in the heap tree, not it's contents A[i] ]
  (let ((l (left i))
        (r (right i))
        (largest nil))
    (if (and (<= l (heap-size heap))
             (heap-compare? (aref heap l) (aref heap i)))
        (setf largest l)
      (setf largest i))
    (when (and (<= r (heap-size heap))
               (heap-compare? (aref heap r) (aref heap largest)))
      (setf largest r))
    (when (not (= largest i))
      (rotatef (aref heap i) (aref heap largest))
      (heapify heap largest))))

;;; Don't need this if use HEAP-INSERT to enter all positions
(defun build-heap (heap)    ; heap is an array with some some elements filled in from 1 through (heap-size heap)
  ;;; calls heapify repeatedly until the root node is a heap
  (print heap)
  (loop for i from (floor (heap-size heap) 2) downto 1
        do
        (heapify heap i)
        (print heap)))


;;;; HEAP-SORT

#|
  (defun heap-sort (heap)    ;; just a heap-array, perhaps lacking heap property (but with heap-size set to the index of last element to be sorted ;
(build-heap heap)
(loop for i from (heap-size heap) downto 2
do
(rotatef (aref heap 1) (aref heap i))
(decf (heap-size heap))
(heapify heap 1)))
  |#

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRIORITY QUEUE (FROM HEAP)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;   Set of Elements S
;;;   Each element has a KEY
;;;   Operations:
;;;     INSERT (S, x)    ;; maintains set property  S <- S U {x}
;;;     MAXIMUM (S)      ;; returns element with largest KEY
;;;     EXTRACT-MAX (S)  ;; removes and returns the element of S with largest KEY

(defun heap-maximum (heap)
   ;; should this check for heap-empty?
   ;;  Now it does!
   (unless (heap-empty? heap)
     (aref heap 1)))

;;; maybe use to remove inbuffers that are exhausted (return nil from get-front-position)
(defun heap-extract-max (heap)
  (cond ((heap-empty? heap)
          nil)  ;; heap is empty
        (t (let ((max (aref heap 1)))
             (setf (aref heap 1) (aref heap (heap-size heap)))
             (decf (heap-size heap))
             (heapify heap 1)
             max))))


(defun heap-insert (heap key)
  (incf (heap-size heap))
  (loop with i = (heap-size heap)
        for p = (parent i)
        while (and (> i 1)
                   (heap-compare? key (aref heap p)))
        do
        (setf (aref heap i) (aref heap p))
        (setf i p)
        finally
        (setf (aref heap i) key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERALIZED HEAP COMPARE OPERATOR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; make this a macro? or inline?
(defun heap-compare? (inbuff1 inbuff2)  ;; two heap entries (input-buffers)
  (compare-positions-for-sort (get-front-position inbuff1) (get-front-position inbuff2))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OLD CODE BELOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
;;; General Framework for Breadth-First Search Using File System to extend memory ;
;;;    Thus far, RAM in my Lisp image has been a bottleneck limiting size of search. ;
;;;    Addressing this by using file-system to store much larger "fringes" (level-sets of positions). ;
;;;      Infrastructure plan:  have buffers that "connect" to files, and either read or write from/to them ;
;;;   Goal is to have a general framework for doing LARGE Best-First Searches, ;
;;;        including SBP (sliding-block puzzles), Relix & Relix-2, Vubu, and maybe Boxed-In ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;

;;; Key function:			;
;;;    GENERATE-NEXT-FRINGE		;
;;; 					;
;;; Supporting infrastructure:		;
;;;    Input-File-Buffer  <points to a byte-file (representing list/sequence of byte-positions)> ;
;;;      Will have a pointer to "next read position" and "last buffer position" (if < size of buffer, then indicates EOF) ;
;;;         This can be used for comparisons (with other "position pointers") or BYTE-DECODE-POSITION (domain-specific decoding) ;
;;;      Operations:			;
;;;         NEXT-POSITION		;
;;;               - advances "next read pointer" to next position ;
;;;               - detects "end of buffer", and refills buffer from associated file  (detecting end-of-file) ;
;;;         ADVANCE beyond given <position-pointer> (may point to any position in any buffer or sequence) ;
;;;             This does successive Next-Position ops until reach a position with value greater than <position-value> ;
;;;    Output-File-Buffer  <points to a byte-file to be used for output> ;
;;;      Will have a pointer to "next write position" in buffer ;
;;;      Operations:			;
;;;        BUFFER-FULL-ACTION (what to do when buffer is full, typically DUMP-BUFFER to file, but may do sorting-in-place first, e.g.) ;
;;;          This may also substitute a NEW FILE for the buffer to output to (eg when dumping multiple sorted position segments of next-fringe) ;
;;;        WRITE-POSITION <pos-pointer>	;
;;;            copies the byte-position at <pos-pointer> into the next write position of buffer, and advance write-position ;
;;;            if buffer is "full", DUMP-BUFFER (using WRITE-SEQUENCE) to file ;
;;;        DUMP-BUFFER -- needed when buffer is full, but also when buffer is "Done" (eg no more positions to write) ;
;;;        FINISH-BUFFER -- force output of any residual contents of buffer (even if not full) ;
;;;    COMPARE-BYTE-POSITIONS (<pos-pointer-1> <pos-pointer-2>)  -> -1, 0, 1 corresponding to <, =, or > comparison ;
;;;        byte postiions compared "lexicographcally" - stop at first byte not-equal, only return = if reach end and ALL bytes equal. ;
;;;    SORT-POSITION-BUFFER  <output-filebuffer> ;
;;;          Do a QuickSort in place of the positions in ;
;;;    MERGE-SORTED-FILES-ELIMINATE-DUPLICATES ;
;;         Behavior: Does MERGE-SORT of the sorted segments of next-fringe, writing to ;
;;;        Inputs:			;
;;;               prior-fringe   <input-file-buffer> ;
;;;               current-fringe <input-file-buffer> ;
;;;               multi-fringe   <multi-file-fringe>  (<input-file-buffer> for each sorted segment of next-fringe) ;
;;;        Output (side-effect):	;
;;;               next-fringe    <output-file-buffer>  to write result positions (sorted. merged, with duplicates removed) to file ;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
;;; Globals				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;

  (defvar *solution-list* )    ;; list of positions in solution ending with *first-solution* ;
  (defvar *first-solution* )   ;; first solution position found ;

  (defvar *search-failed* )    ;; set to T if current-fringe is ever size 0 ;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
;;; Best-First File Solve		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;



  (defun best-first-file-solve-engine (&optional 
(start-list (list *start-pos*))
(stop-when-solved? t))        ; Note: irrelevant unless collect-solution-positions? is T ;

(when (and recover-solution-sequence?
(not collect-solution-positions?))
(setf collect-solution-positions? t)
(setf *solution-list* nil)
(warn "Can't recover solution sequence unless collect-solution-positions? is T (or non-NIL)~%~
           Setting collect-solution-positions? to T~%"))
(setf *start-list* start-list)
(slide-solve-engine-loop nil          ; prior fringe ;
(new-hash-table)     ; current-fringe	;
(let ((pos-set (new-hash-table)))    ; new-fringe ;
(loop for pos in start-list
do
(setf (gethash (copy-position pos) pos-set)
t))
pos-set)
0            ; start at depth 0		;
solution-stop?
stop-when-solved?
collect-solution-positions?
recover-solution-sequence?))


  (defun slide-solve-engine-loop (start-prior-fringe start-current-fringe start-new-fringe
start-depth
solution-stop?
fringe-save-interval
stop-when-solved?
collect-solution-positions?
recover-solution-sequence?
&optional 
restrict-piece-type-first-time?)
(loop for prior-fringe = start-prior-fringe then current-fringe
for current-fringe = start-current-fringe then new-fringe
for new-fringe = start-new-fringe 
then (unless (and *all-solutions* ; don't waste computation on an extra fringe! ;
stop-when-solved?)
(new-fringe current-fringe prior-fringe *move-type* restrict-piece-type?))
for depth from start-depth
for new-fringe-count = (hash-table-count new-fringe)
for restrict-piece-type? = restrict-piece-type-first-time? then nil
until (zerop new-fringe-count)
do

finally
(format t "~%Exit at depth = ~a" depth)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;
;;; TODO				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;


;;  FACTOR CODE:			;
;;    Separate Buffer infrastructure from search engine !? ;



;; Do own garbage management -- GC is a killer. ;
;;     recycle my own conses, etc.	;
;;     recycle hash tables!!		;

;;     Note - presently no automatic calls to reclaim-scheduled-fringes ;

;; Add declarations for speed/efficiency ;

;; Recover 1-step solutions when start-list has multiple positions ;
;;      (generate moves or else loop over start-list doing searches ;
;;		from single-positions)	;
;;	   [the latter would require recognizing failure, and limiting ;
;;		search to depth 1 ]	;

;; Maybe use bits instead of hash-table for locally-seen positions of given piece ;
;;    in same-piece-extension.		;

;; Use logical shift instead of (expt 2 n) ?? ;

;; Modify the use of piece-list and/or start-list so can use a SUBSET of available pieces in list ;
;;   CURRENTLY: I think it creates entries for every piece, not just the actual pieces used. ;
;;      This would be convenient for defining large sets of pieces, then only picking out the ones needed for a specific puzzle ;

  |#
