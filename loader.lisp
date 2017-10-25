
;; Set default directory for load forms
(setf *default-pathname-defaults* *load-pathname*)

(load "my-profiler")
(load "Ext-AStar-code-from-file-search-engine")
(load "Ext-AStar-file-based-SBP")
(load "SBP-Ext-AStar-blank-index-jimslide")
(load "radix-file-buffers")
(load "slide-puzzle-inits")

;; create your own local-config.lisp with settings for at least
;; the two defparamers below. **path-to-file-storage** should NOT
;; be to a solid-state drive (SSD) and should have a trailing slash.
;; If you want to automatically start a run, you should add the 
;; appropriate code to local-config. For example, you might add something like the
;; sbp-setup-ext-astar and external-a-star function calls below to your local-config.lisp.
;; Do NOT uncomment them here and commit that change.
;(defparameter **path-to-file-storage** "/space/bigspace/userspace/iba/sbp-files/")
;(defparameter **max-buffer-position-count** 5000000)
;
;(sbp-setup-ext-astar 'climb12 'foo)
;(external-a-star)

(load "local-config.lisp")
