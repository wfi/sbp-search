
;; This should set the default directory of the compile-file forms
(setf *default-pathname-defaults* *load-pathname*)

(compile-file "my-profiler")
(compile-file "Ext-AStar-code-from-file-search-engine")
(compile-file "Ext-AStar-file-based-SBP")
(compile-file "SBP-Ext-AStar-blank-index-jimslide")
(compile-file "slide-puzzle-inits")

