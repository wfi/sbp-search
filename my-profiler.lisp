(declaim (optimize (speed 3) (debug 0) (safety 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My "Roll Your Own" Profiling Utilities
;;;    Timers and Counters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 10-23-2013
;;  initial version took code out of file-search-engine-9.lisp
;;    Note that need both OLD AND NEW timers, since the old search code
;;      calls both!

;; 10-23-2013
;;   version 2
;;     change implementation of timers to use symbol property-lists (to avoid assocs?)
;;       also convert to microseconds (what Clozure uses) using GET-INTERNAL-REAL-TIME

;; 5-16-2014
;;   version 3
;;     add predicate TIMER-STOPPED? (T if third of timer-form is NIL)
;;       use to report elapsed time for a timer that is still running

;; 8-19-2014
;;   version 4
;;     modify stop-timer to take optional arg inc-count? defaulting to T for backward compatibility
;;     When using a "timer-counter" (1 start, 1 stop, and many inc-counter calls in between)
;;         the stop-timer should be called with NIL (to avoid an extra count)

;; 12-7-2015
;;   version 5
;;     add function RESET-COUNTER for clarity 
;;        (note it just calls reset-timer, which does the job, but may create naming confusion)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OLD Timing Utilities  -- still called by file-search-engine
;;;    (retain for backwards compatibility, but use NEW TIMING from here on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *start-utime* )
(defvar *fringe-start-utime* )
(defvar *end-utime* )

;;; This was the OLD START-TIMING function
(defun start-search-timing ()
  (setf *start-utime* (get-universal-time))
  (format t "~%~%Started timing at ~a~%~%"
          (get-current-time-string *start-utime*)))

(defun start-fringe-timing ()
   (setf *fringe-start-utime*
         (get-universal-time)))

(defun get-current-time-string (&optional (current-utime (get-universal-time)))
  (multiple-value-bind (secs mins hrs date month year)
                       (decode-universal-time current-utime)
    (format nil "~2,'0d:~2,'0d:~2,'0d on ~2,'0d/~2,'0d/~4d"
            hrs mins secs month date year)))

;;; WAS OLD STOP-TIMING
(defun stop-search-timing ()
  (setf *end-utime* (get-universal-time))
  (format t "~%~%Stopped timing at ~a~%~%"
          (get-current-time-string *end-utime*))
  (format t "Elapsed time was ~a seconds"
          (- *end-utime* *start-utime*)))

(defun get-elapsed-time ()   ;; relative to *start-utime*
  (- (get-universal-time) *start-utime*))

(defun get-elapsed-fringe-time ()   ;; relative to *fringe-start-utime* 
  (- (get-universal-time) *fringe-start-utime*))

;; not called
(defun resume-timing (old-start old-end)
  (let ((old-elapsed-time (- old-end old-start))
        (current-utime (get-universal-time)))
    (setf *start-utime* (- current-utime old-elapsed-time))
    (format t "~%~%Resumed timing at ~a~%"
            (get-current-time-string current-utime))
    (format t "  (previously elapsed time was ~a seconds)~%~%"
            old-elapsed-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NEW TIMING UTILITIES 
;;;    Now using MICROSECONDS (iternal-time-steps-per-second = 1000000 in Clozure) [LispWorks = 1000]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Timers will ALL include counters (so can compute average time per "call")
;;; Sometimes will ONLY use the COUNTER to count things (when timing isn't relevant)

(defparameter **my-timers** nil)    ;; includes counters

;; MAYBE PUT TIMERS ON PROPERTY LISTS FOR QUICKER ACCESS

(defun allow-timing (name)   ; name should be a symbol for a kind of thing to time
  (unless (member name **my-timers**)
    (push name
	  **my-timers**)
    (setf (get name 'timer)
	  (make-timer-form))))

(defun allow-counting (name)
  (unless (member name **my-timers**)
    (push name
	  **my-timers**)
    (setf (get name 'timer)
	  (make-timer-form t)))) ;; T = counter-only

(defun make-timer-form (&optional counter-only?)
  (list 0     ;counter to track number of calls
	(if counter-only?
	    nil   ;;  nil means "no timing" permitted
	    0)     ; accumulator  [incremented by (time - start-time) when (stop-timing 'name) is called
	nil   ; start-time [set by (start-timing 'name) ]
	))

(defun timer-stopped? (name)
  (let ((timer-form (get name 'timer)))
    (cond ((null timer-form)
	   (warn "No timer-form found for ~a" name))
	  (t
	   (null (third timer-form))))))

(defun total-time (name) 
  ;; in microseconds
  (second (get name 'timer)))

(defun inc-counter (name &optional (increment 1))
  (incf (first (get name 'timer))
	increment))

(defun get-counter (name)
  (first (get name 'timer)))

;; note conflict with slide-solve-fringe variants
(defun start-timing (name)
  (setf (third (get name 'timer))
	(get-internal-real-time)))

;; note conflict with slide-solve-fringe variants
(defun stop-timing (name &optional (inc-count? t))
  (let* ((timer-form (get name 'timer))
	 (start-time (third timer-form)))
    (cond (start-time
	   (incf (second timer-form)
		 (- (get-internal-real-time)
		    start-time))
	   (when inc-count?
	     (incf (first timer-form)))   ;; bump up "call count" but not if inc-count? is nil (eg if a timer/counter)
	   (setf (third timer-form) nil))
	  (t
	   (warn "Called STOP-TIMING without setting start-time for name ~a" name)))))

(defun time-in-units (internal-time &optional (units 'seconds))
  (and (numberp internal-time) ;; make sure time is an actual number
       (case units
	 (seconds (float (/ internal-time
			    internal-time-units-per-second)))
	 (t internal-time))))
				

(defun  report-total-time (name &optional (units 'seconds))     ;; anything else means internal-time-units
  (cond ((timer-stopped? name)
	 (report-total-time-stopped name units))
	(t
	 (report-total-time-running name units))))

;; Only report elapsed time (total and since last start)
(defun report-total-time-running (name &optional (units 'seconds))
  (let* ((internal-time-unit (case internal-time-units-per-second
			       (1000000 'microseconds)
			       (1000    'milliseconds)
			       (t '?seconds)))
	 (timer-form (get name 'timer))
	 (total-count (first timer-form)) 
	 (total-time (second timer-form)) ;; (accumulated-time), nil if just a counter (ie NOT a timer)
	 (last-start-time (third timer-form))
	 (current-time (get-internal-real-time))
	 (elapsed-time-since-last-start (and last-start-time
					     (- current-time last-start-time)))
	 (total-elapsed-time (+ total-time elapsed-time-since-last-start))
	 (time-units (case units
		       (seconds 'seconds)
		       (t internal-time-unit)))	;; see above
	 (total-time-in-units (when total-time ;; make sure it's a timer, not just a counter
				(time-in-units total-elapsed-time units)))
	 (elapsed-time-since-last-start-in-units
	  (when elapsed-time-since-last-start
	    (time-in-units elapsed-time-since-last-start units))))
    (cond (total-time			; this is a real timer
	   (format t "~% Timer ~a is running, total-time = ~a ~a (count = ~a+), most recent time = ~a ~a"
		   name
		   total-time-in-units
		   time-units
		   total-count
		   elapsed-time-since-last-start-in-units
		   time-units))
	  (t ;; only a counter
	   (format t "~% Counter ~a = ~a+ (still running)"
		   name
		   total-count))))
  )

(defun report-total-time-stopped (name &optional (units 'seconds))
  (let* ((timer-form (get name 'timer))
	 (total-count (first timer-form))
	 (total-time (second timer-form))   ;; nil if a counter (ie NOT a timer)
	 (time-units (case units
		       (seconds 'seconds)
		       (t 'microseconds)))
	 (total-time-in-units (when total-time   ;; make sure it's a timer, not just a counter
				(case units
				  (seconds (float (/ total-time
						     internal-time-units-per-second)))
				  (t total-time)))))  ;; microseconds
    (cond (total-time			 ; this is a real timer
	   (format t "~% Total time for ~a = ~a ~a, count = ~a, average = ~a"
		   name
		   total-time-in-units
		   time-units
		   total-count
		   (if (zerop total-count)
		       'unknown  ;; can't divide by 
		       (float (/ total-time-in-units   ;; average time per call
				 total-count)))))
	  (t   ;; only a counter
	   (format t "~% Counter ~a = ~a"
		   name
		   total-count)))))

(defun report-listed-timers (list-of-timer-names)
  (loop for name in list-of-timer-names
       do
       (report-total-time name)))

(defun report-all-timers ()
  (loop for timer-name in **my-timers**
       do
       (report-total-time timer-name)))

(defun reset-timer (name)
  (let ((timer-form (get name 'timer)))
    (cond (timer-form
	   (when (second timer-form) ;; Actual timer
	     (setf (second timer-form) 0
		   (third timer-form) nil))
	   (setf (first timer-form) 0))	;; always reset counter
	  (t
	   (allow-timing name)))))
	   
;; use for naming clarity when reseting a counter
(defun reset-counter (name)
  (reset-timer name))
