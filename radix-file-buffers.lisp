



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RADIX FILE BUFFERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; RADIX OUTPUT BUFFER
;;;   Uses low-order bits of blank index to "hash" position into "raw buckets"
;;;   Do without any duplicate detection or segment creation
;;;   RADIX-OUTPUT-BUFFER will have a separate LINEAR-OUTPUT-BUFFER for each "radix/hash value"

;;; LINEAR OUTPUT BUFFER
;;;   Like the older output-buffers, but writing sequentially to a large byte-vector
;;;     No sorting or duplicate elimination
;;;   When buffer full write-sequence buffer (appending) to radix-bucket-file
;;;     If bucket stays empty, don't ever create file (output-stream stays nil)


;;; INTERFACE
;;;   WRITE-POSITION 
;;;   POINT-RADIX-OUTPUT-BUFFER
;;;       resets buffer and all radix-sub-buffers to point to new bucket values/pathnames
;;;   MERGE-FILTER-RADIX-BUCKETS
;;;       Reads each raw-bucket into a (large) hash-table
;;;       Then filters by remoing any positions from the 2 previous raw-buckets with
;;;          same radix-value and relative g-vals -1 and -2
;;;       Finally writes out the filtered hash-table to a bucket.data file (no dups left)
;;;    NOTE: IF Hash-Table can't fit all the unique positions from raw-buckt file,
;;;       Then will need to:
;;;          write the hash-table to bucket.data file
;;;          empty the hash-table for reuse
;;;          read  new batch of raw-positions into hash-table
;;;          Now filter using partial bucket.data file as well as the g-1 and g-2 buckets


;;; DO I NEED THIS HERE ??
#|
(defparameter **out-buff-0** nil)
(defparameter **out-buff-1** nil)
(defparameter **out-buff-2** nil)

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


(defparameter **max-radix-buffer-position-count** 5000)
(defparameter **max-buffer-position-count** 1000000) ;; smaller since used for each radix buffer
;; (defvar **max-buffer-position-count**)
(defvar **position-size**)
(defvar **byte-size**)

(defparameter **out-buff-0** nil)
(defparameter **out-buff-1** nil)
(defparameter **out-buff-2** nil)

;;; Used for quick check if bucket is empty (Must be updated when bucket is written to)
(defparameter **empty-bucket** nil)   ;; also defparameter for this in Ext-AStar-file-based-SBP file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RADIX OUTPUT BUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass radix-output-buffer ()
  ((lin-outbuff-vector :initform nil :accessor lin-outbuff-vector)
   (radix-count :initform 256 :accessor radix-count)
   (radix-mask :initform 255 :accessor radix-mask)
   (position-index-for-radix :initform nil :accessor position-index-for-radix)
   (bucket-g :initform nil :accessor bucket-g)
   (bucket-h :initform nil :accessor bucket-h)))

;;; only use for original init, re-use by calling point-radix-output-buffer
(defun new-radix-output-buffer (&optional
				  (pos-size **position-size**) ;; 4 for climb24
				  (radix-bit-count 8))
  (let ((radix-output-buffer
	 (make-instance 'radix-output-buffer)))
    (with-slots (lin-outbuff-vector radix-count radix-mask position-index-for-radix)
	radix-output-buffer
      (setf radix-count (expt 2 radix-bit-count))
      (setf radix-mask (1- radix-count))
      (setf lin-outbuff-vector
	    (make-array radix-count))
      (setf position-index-for-radix (1- pos-size)) ;; last element of position is blank-index
      (loop for radix from 0 below radix-count
	 do
	   (setf (aref lin-outbuff-vector radix)
		 (new-linear-output-buffer **max-radix-buffer-position-count**
					   pos-size
					   **byte-size**))
	   ))
    radix-output-buffer ;; return the radix-output-buffer !!
    ))

;; should this check if buffer is valid (wrt **open**)?
(defun point-radix-output-buffer (radix-outbuff g h)
  (with-slots (lin-outbuff-vector radix-count bucket-g bucket-h)
      radix-outbuff
    (setf bucket-g g
	  bucket-h h)
    (loop for radix from 0 below radix-count
	 for lin-outbuff = (aref lin-outbuff-vector radix)
       do
	 (point-linear-output-buffer lin-outbuff g h radix t)   ;; t so raw? = t
	 ))
  radix-outbuff)    ;; return the radix-outbuff

(defmethod write-position ((radix-outbuff radix-output-buffer) position)
  (with-slots (lin-outbuff-vector radix-mask position-index-for-radix)
      radix-outbuff
    (write-position (aref lin-outbuff-vector
			  (logand radix-mask      ;; get low-order bits of blank-index
				  (aref position position-index-for-radix)))
		    position)))

(defmethod close-buffer ((radix-outbuff radix-output-buffer))
  (with-slots (lin-outbuff-vector)
      radix-outbuff
    (loop for lin-outbuff across lin-outbuff-vector
       do
	 (close-buffer lin-outbuff))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LINEAR OUTPUT BUFFER 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Writes positions sequentially to byte-vector (of elemnt-type **byte-size**)
;;; Smaller buffers (since will need unique linear-output-buffer for each radix (eg 256)

(defclass linear-output-buffer ()
  ((position-size :initform nil :accessor position-size)  ;; for incrementing write-index
   (buffer-size :initform nil :accessor buffer-size)    ; in **byte-size** bytes
   (buffer-vector :initform nil :accessor buffer-vector)    ; with fill-pointer, initially 0
   (bucket-g :initform nil :accessor bucket-g)
   (bucket-h :initform nil :accessor bucket-h)
   (radix-val :initform nil :accessor radix-val)
   (output-filepath :initform nil :accessor output-filepath)
   (output-buffer-file-stream :initform nil :accessor output-buffer-file-stream)))

;;;   NOW ONLY MAKES INSTANCE & SETS BUFFER-VECTOR and POSITION-SIZE
;;;     -- all other slots set by POINT-OUTPUT-BUFFER
(defun new-linear-output-buffer (&optional
				   (max-position-count **max-radix-buffer-position-count**)
				   (pos-size **position-size**)
				   (byte-size **byte-size**))
  (let ((linear-output-buffer
	 (make-instance 'linear-output-buffer)))
    (with-slots (buffer-size buffer-vector position-size)
	linear-output-buffer
      (setf position-size pos-size)
      (setf buffer-size (* position-size max-position-count))
      (setf buffer-vector
	    (make-array buffer-size
			:element-type (list 'unsigned-byte byte-size)
			:fill-pointer 0)))
    linear-output-buffer ;; return the output-buffer !!
    ))

(defun point-linear-output-buffer (lin-out-buff g h radix &optional (raw? t))
  ;; TODO -- update data in **open** ?
  (with-slots (buffer-vector buffer-size bucket-g bucket-h radix-val
			     output-filepath output-buffer-file-stream)
      lin-out-buff
    (setf (fill-pointer buffer-vector) 0) ;; reset the buffer-vector !!
    (setf bucket-g g
	  bucket-h h
	  radix-val radix)
    ;; no segments
    (setf output-filepath
	  (bucket-radix-pathname bucket-g bucket-h radix-val raw?))
    (setf output-buffer-file-stream
	  nil))	;; only create file-stream when needed for actual write
  lin-out-buff ;; return the output-buffer !!  (so can use for value, not just side-effect)
  )

(defmethod write-position ((lin-outbuff linear-output-buffer) position)
  ; write-position copies position into position-element in buffer-vector - if buffer full will flush to file and update
  (with-slots (buffer-vector position-size buffer-size bucket-g bucket-h radix-val
			     output-filepath)
      lin-outbuff
    (when (>= (fill-pointer buffer-vector) buffer-size)   ;; buffer is full!
      ;; this writes without sorting (may have duplicates)
      (write-buffer-vector-to-output-stream lin-outbuff)
      (setf (fill-pointer buffer-vector) 0)   ;; don't need to empty buffer, since elements will get over-written
      )
    ;; whether or not buffer WAS full, it should NOT be full now, and we still need to WRITE Position
    (replace buffer-vector position
	     :start1 (fill-pointer buffer-vector)
	     :end1 (incf (fill-pointer buffer-vector)
			 position-size))
    ;; note fill-pointer always points to next write-location
    ))

;;; NOTE: this no longer SORTS buffer before write, but rather appends
;;; NOTE2: This should ONLY be called when there is something to WRITE,
;;;     Since it now updates **empty-bucket** when called
(defmethod write-buffer-vector-to-output-stream ((lin-outbuff linear-output-buffer))
  ;;  will only write from 0 to fill-pointer
  (with-slots (buffer-vector output-filepath output-buffer-file-stream bucket-g bucket-h)
      lin-outbuff
    ;;(start-timing 'reduce-write-buffer)
    (unless output-buffer-file-stream
      (setf output-buffer-file-stream
	    (open-output-file-stream output-filepath)))
    (write-sequence buffer-vector output-buffer-file-stream)
    ;;; Update **empty-bucket** (no longer empty)
    (setf (aref **empty-bucket** bucket-g bucket-h)
	  nil)     ;; NOT EMPTY any more
    ;;(stop-timing 'reduce-write-buffer)
    )
  ;; Does NOT close or update, since may want to keep appending to outstream
  )


;; only call when completely done with buffer!
(defmethod close-buffer ((lin-outbuff linear-output-buffer))
;;;   close buffer (write out any partial buffer remaining, and close file)
  (with-slots (buffer-vector output-buffer-file-stream
			     bucket-g bucket-h radix-val)
      lin-outbuff
    ;; if there is anything still in buffer
    (when (< 0 (fill-pointer buffer-vector)) ;; not empty
      (write-buffer-vector-to-output-stream lin-outbuff))
    (when output-buffer-file-stream   ;; if stream was opened
      (close output-buffer-file-stream))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HASH-TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter **large-hash-table-size**
  100000)     ;; small for testing

(defparameter **large-hash-table** nil)

(defparameter **large-vector-of-positions** nil)

(defparameter **next-free-position-in-large-vector** 0)

(defparameter **ht-linear-output-buffer** nil)

(defun setup-large-hash-table (&optional
				 (position-count **large-hash-table-size**)
				 (position-size **position-size**)
				 (byte-size **byte-size**))
  (print (list position-count position-size byte-size))
  (setf **large-hash-table-size** position-count)
  (setf **large-hash-table**
	(make-hash-table :test #'equalp
			 :size position-count))
  (setf **large-vector-of-positions**
	(make-array position-count))
  (loop for i from 0 below position-count
     do
       (setf (aref **large-vector-of-positions** i)
	     (make-array position-size
			 :element-type (list 'unsigned-byte byte-size))))
  (setf **next-free-position-in-large-vector** 0)
  (setf **ht-linear-output-buffer**
	(new-linear-output-buffer  (* 10 **max-buffer-position-count**)))
  )
       

(defun new-reusable-position ()
  (when (>= **next-free-position-in-large-vector**
	   **large-hash-table-size**)
    (error "No more reusable positions available"))
  (let ((new-pos (aref **large-vector-of-positions**
		       **next-free-position-in-large-vector**)))
    (incf **next-free-position-in-large-vector**)
    new-pos    ;; return the position
    ))


;;; TODO -- detect when ht is full -- then special handle
(defun load-hash-table-from-file (file-pathname)
  (loop with inbuff = (new-input-buffer file-pathname)
     with large-ht = **large-hash-table**    ;; assume already cleared ??
     for pos = (get-front-position inbuff) then (next-position inbuff)
     while pos
     do
       (unless (gethash pos large-ht)
	 (setf (gethash (replace (new-reusable-position) pos)  ;; copy pos to use as key
			large-ht)
	       t))
     finally
       (close-buffer inbuff)))

;; This removes all positions of file-pathname from the large-hash-table
(defun filter-hash-table-with-file (file-pathname)
  (loop with inbuff = (new-input-buffer file-pathname)
     with large-ht = **large-hash-table**
     for pos = (get-front-position inbuff) then (next-position inbuff)
     while pos
     do
       (remhash pos large-ht)   ;; no effect if not there, else removes pos as key
     finally
       (close-buffer inbuff)))

;; Should this always clear the hash-table when done writing ?
(defun write-hash-table-to-radix-bucket (g h radix)
  (point-linear-output-buffer **ht-linear-output-buffer** g h radix nil)    ;; raw? = nil to write to <bucket>.data
  (loop with lin-outbuff = **ht-linear-output-buffer**
     for pos being the hash-keys of **large-hash-table**
     do
       (write-position lin-outbuff pos)
     finally
       (close-buffer lin-outbuff)
       (clrhash **large-hash-table**)
       (setf **next-free-position-in-large-vector** 0)  ;; reuse all positions
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO -- FIX THESE UTILITIES FOR RADIX / CLEAN UP INTERFACE


(defparameter **open** nil)
(defparameter **puzzle-directory-name** "Dummy-Dir")

(defparameter **path-to-file-storage**
  ;"/Users/glenniba/temp/SEARCH-FILE-STORAGE/"
  ; /Volumes/EXT1-OTHER/SEARCH-FILE-STORAGE/"
  "/Volumes/EXT-3TB-B/SEARCH-FILE-STORAGE/"
  ;"/Volumes/Seagate6TB/SEARCH-FILE-STORAGE/"
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
#|
(defun bucket-pathname (g h)
  (format nil "~a~a/bucket-~a-~a.data" **path-to-file-storage** **puzzle-directory-name** g h))
|#

(defun bucket-radix-pathname (bucket-g bucket-h radix-val raw?)
  (format nil
	  "~a~a/bucket-~a-~a-radix-~a~a.data" 
	  **path-to-file-storage**
	  **puzzle-directory-name** 
	  bucket-g bucket-h radix-val
	  (if raw?
	      "-raw"
	      "")))

;; was fringe-segment-pathname
#|
(defun bucket-segment-pathname (g h segment-number)
  (format nil "~a~a/bucket-~a-~a-segment-~a.data" 
	  **path-to-file-storage** **puzzle-directory-name** g h segment-number))
|#

;; was fringe-exists?
;; TODO: Rewrite for RADIX
#|
(defun bucket-exists? (g h)
  ;; now check for any file with any radix satisfying g & h
  (and (array-in-bounds-p **open** g h)
       (all-bucket-files? g h)))
|#


;; Maybe make this a Macro -- but first verify that it works!
(defun empty-bucket? (g h)
  (or (not (array-in-bounds-p **open** g h))
      (aref **empty-bucket** g h))   ;; Must update this array when there is output to a bucket
  )

;;; This uses PROBE-FILE instead of DIRECTORY
(defun empty-bucket-probe? (g h)
  (cond ((not (array-in-bounds-p **open** g h))
	 t)
	(t ;; return t if all raw and and non-raw radix-files are empty
	 (loop for radix from 0 below 256    ;; TODO: Generalize for **radix-count** or **radix-bits**
	    never   ;; assuming NEVER write out empty files !!  CAREFUL!!
	      (or (probe-file (bucket-radix-pathname g h radix nil))
		  (probe-file (bucket-radix-pathname g h radix t)))))))

#|
;; kludge -- use directory to search for any files in bucket
(defun all-bucket-files? (g h)        ;; raw or not, any radix
  (directory (wild-card-bucket-pathname g h)))

;; not called ?
(defun all-raw-bucket-files? (g h)
  (directory (wild-card-bucket-pathname-raw g h)))

(defun wild-card-bucket-pathname (g h)   ;; with or without radix
  (format nil
	  "~a~a/bucket-~a-~a-*.data"
	  **path-to-file-storage**
	  **puzzle-directory-name** 
	  g
	  h))

(defun wild-card-bucket-pathname-raw (g h)
  (format nil
	  "~a~a/bucket-~a-~a-*-raw.data" 
	  **path-to-file-storage**
	  **puzzle-directory-name** 
	  g h))


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

|#

(defun open-input-file-stream (pathname)
  (open  pathname
         :direction :input
         :element-type (list 'unsigned-byte **byte-size**) 
         :if-does-not-exist :error))

;; modified for radix files
(defun open-output-file-stream (pathname)
  (open pathname
        :direction :output
        :element-type (list 'unsigned-byte **byte-size**)
        :if-does-not-exist :create
        :if-exists :append))    ;; no longer :error or :supersede for radix files

;; NEED THIS
(defun get-file-size (pathname)   ;; size in bytes
  (with-open-file (infile pathname :direction :input :if-does-not-exist :error)
    (file-length infile)))

(defun get-file-position-size (pathname)  ;; assume exists, size count of positions
  (/ (get-file-size pathname)
     **final-8bit-byte-position-size**))

#|
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

(defun display-bucket-sizes (&optional (start-depth 0))
  (loop for depth from start-depth
        for fringe-path = (fringe-pathname depth)
        while (probe-file fringe-path)
        do
        (print (list depth (get-fringe-position-size depth)))))
|#

;; was  format-file-info-for-depth
#|
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
|#

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
     while (equalp pos1 pos2)   ;; might this never terminate if contents identical ??
     finally
       (return (list pos1 pos2))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Merging and Expanding Buckets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MERGE
;;;  Merge acts on each radix-file in bucket to eliminate duplicates
;;;    It reads in the  bucket-g-h-radix-r-raw.data version of file into large-hash-table,
;;;       thus eliminating duplicates
;;;    Then filters by removing any positions in the "previous" 2 g-buckets of same h and radix
;;;       [those are  bucket-g1-h-radix-r.data and bucket-g2-h-radix-r.data,
;;;          where g1 = g-1,  and g2 = g-2]
;;;    Then writes out the reduced hash-table to file bucket-g-h-radix-r.data


(defun merge-bucket (g-min h-max)
  (loop for radix from 0 below 256    ;; generalize to Radix-count (using radix-bit-count ?)
     for raw-pathname = (bucket-radix-pathname g-min h-max radix t)  ;; T = add-raw
     for prior-g-pathname = (bucket-radix-pathname (1- g-min) h-max radix nil)  ;; NOT RAW
     for prior-2-g-pathname = (bucket-radix-pathname (- g-min 2) h-max radix nil)  ;; NOT RAW
     when (probe-file raw-pathname)
     do
       (load-hash-table-from-file raw-pathname)    ;; TODO: generalize so can load/filter in stages
       (when (probe-file prior-g-pathname)
	 (filter-hash-table-with-file prior-g-pathname))
       (when (probe-file prior-2-g-pathname)
	 (filter-hash-table-with-file prior-2-g-pathname))
       (write-hash-table-to-radix-bucket g-min h-max radix)
       (when nil
	 (print (list 'radix radix 'open-files (length (open-file-streams)))))))




;;; EXPAND
;;;  Expand g h radix
;;;   Repoints A0, A1, and A2 (using **out-buff-0** **out-buff-1** **out-buff-2**)
;;;     to 

(defun expand-bucket (g-min h-max)
  (loop with output-object = (list h-max
				   (get-bucket-out (1+ g-min) (1- h-max) **out-buff-0**)   ;; A0
				   (get-bucket-out (1+ g-min) h-max **out-buff-1**)        ;; A1
				   (get-bucket-out (1+ g-min) (1+ h-max) **out-buff-2**)   ;; A2
				   )
     for radix from 0 below 256
     for bucket-radix-pathname = (bucket-radix-pathname g-min h-max radix nil)   ;; expand only non-raw file
     when (probe-file bucket-radix-pathname)
     do
       (expand-radix-bucket g-min h-max radix output-object)   ;; output-object same for all radix vals
     finally
     ;; Close-buffers A0, A1, A2
       (loop for out-buff in (cdr output-object)
	  when out-buff
	  do
	    (close-buffer out-buff))))


(defun expand-radix-bucket (g-min h-max radix output-object)
  (when **debug**
    (print 'output-object) 
    (princ output-object))
  (loop with sol-pos? = nil
     with in-buff = (get-bucket-in g-min h-max radix nil)     ;; only expand non-raw file
     for pos = (when in-buff (front-position in-buff)) then (next-position in-buff)
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
	 (setf  **solution** (list sol-pos? (1+ g-min)))
	 (format t "~%FOUND SOLUTION WITH G-VAL = ~a" (1+ g-min)))

     finally
       (when in-buff
	 (close-buffer in-buff))
       ))

(defun get-bucket-out (g h out-buff-to-repoint)
  (when (array-in-bounds-p **open** g h) ;; return nil if indices out of bounds
    (point-radix-output-buffer out-buff-to-repoint g h)))


(defun get-bucket-in (g h radix raw?)   ;; only call when corresponding bucket file exists!
  (new-input-buffer (bucket-radix-pathname g h radix raw?)))


;;; STORE INITIAL BUCKET (only done once when starting search)
;; only called to store initial-position (should start new bucket)
(defun store-bucket (position g h)   ;; NOTE: doesn't check for array-in-bounds-p (assumed ok)
  (let ((out-buff (point-radix-output-buffer **out-buff-0** g h)))
    (write-position out-buff position)
    (close-buffer out-buff)))

(defun init-out-buffs ()
  (setf **out-buff-0**
	(init-radix-out-buff **out-buff-0**))
  (setf **out-buff-1**
	(init-radix-out-buff **out-buff-1**))
  (setf **out-buff-2**
	(init-radix-out-buff **out-buff-2**)))

(defun init-radix-out-buff (out-buff)
  (cond ((and nil       ;; for now, never ok to re-use
	      out-buff
	      ; (= (length (buffer-vector out-buff))
					;    max-position-count)
	      )
	 #|
	 (unless (pos-type-ok? (aref (buffer-vector out-buff) 0))
	   (fill (buffer-vector out-buff) nil))
	 |#
	 out-buff)  ;; return the out-buff
	(t    ;; can't re-use so create-new output-buffer
	 (new-radix-output-buffer))))

