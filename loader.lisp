
;; Set default directory for load forms
(setf *default-pathname-defaults* *load-pathname*)

(load "my-profiler")
(load "Ext-AStar-code-from-file-search-engine")
(load "Ext-AStar-file-based-SBP")
(load "SBP-Ext-AStar-blank-index-jimslide")
(load "radix-file-buffers")
(load "slide-puzzle-inits")


(load "local-config.lisp") ;; NOT tracked by git, but containing local values
;(defparameter **path-to-file-storage** "/Volumes/EXT-3TB-B/SEARCH-FILE-STORAGE/")
;(defparameter **max-buffer-position-count** 5000000)
