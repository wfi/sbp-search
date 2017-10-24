
;; Set default directory for load forms
(setf *default-pathname-defaults* *load-pathname*)

(load "my-profiler")
(load "Ext-AStar-code-from-file-search-engine")
(load "Ext-AStar-file-based-SBP")
(load "SBP-Ext-AStar-blank-index-jimslide")
(load "radix-file-buffers")
(load "slide-puzzle-inits")


(defparameter **path-to-file-storage** "storage")
(defparameter **max-buffer-position-count** 5000000)

(sbp-setup-ext-astar 'climb12 'foo)

(external-a-star)


