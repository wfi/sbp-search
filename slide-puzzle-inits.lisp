;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Sliding Block and Rush Hour Initialization Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Copyright (c) 1998  Glenn A. Iba
;;
;;  This file should only be loaded/compiled after the 
;;  solver code has been loaded/compiled.  These routines
;;  depend on that code, and can be conveniently used to
;;  setup particular problems for solving.

;;  Change Log
;;
;;   4-4-98  Modified new-rush-hour-init so as to assume 1-based row/col indexing.
;;     		Changed the previous level specifications to conform (by wrapping
;;		list with a call to shift-row-col-indices with a delta of 1, which
;;		gets undone when the init shifts back by -1).
;;
;;   4-4-98  Modified new-rush-hour-init so that if some piece types are not needed,
;;		then they will not be used for initialization.  This should save on
;;		the memory required to represent each position for such problems.
;;
;;   4-6-98  Modified new-rush-hour-init so it now takes a single argument of the
;;		following form:
;;			( exit-row  piece-1 piece-2 ... piece-n )
;;		where the exit-row is a 1-based integer specifying the exit row,
;;  		and each piece-k is a triple of the form
;;			( piece-type row col)
;;		   with piece-type being a non-zero number (currently 1 - 5)
;;		      specifying a piece shape,
;;	 	   and row and col being 1-based integers specifying the row and col
;;		      of the piece's "origin cell".
;;		NOTE that the first piece-spec specifies the target piece.
;;  5-30-2013  Added Climb-12, Climb-24 puzzles
;;
;;  6-29-2013  Version 19
;;           Added code to test whether explicit "origins" [i.e. (0 0) offset] are required in piece-type specs
;;           Turns out the answer is YES!  Piece-types must always include (0 0) explicitly, because of obstacles!
;;           The test code defines functions derived from Climb-12:
;;                    CLIMB12-PIECE-TYPES-TEST
;;                    CLIMB12-INIT-TEST
;;                    CLIMB12-START-LIST-TEST
;;               Calling climb12-init-test generates an error, answers the "question" regarding explicit origin offsets
;;           FINISHED spec for CLIMB-PRO-24 puzzle (was incomplete)
;;  6-29-2013  Version 20
;;           Added inits for Block10  (from Minoru Abe Gallery)
;;                Variants 11, 12, 13, 14  (verified optimal move counts)
;;
;;  10-23-2013 Version 21
;;          Updated CLIMB24-INIT to make the target piece appear first (for redundancy detection) 
;;  11-01-2013 Version 22
;;          Fixed bug in SET-CLIMB24-TARGET -- needed to reflect new piece-type ordering introduced with verion 21

;;  12-4-2013  Version 23 [INCOMPLETE!!]
;;     Started to define "Hole-in-One" Puzzle
;;        Need to "recall" how to create "exact" targets (with all pieces specified)
;;     FIX BEFORE USING

;;  12-22-2013
;;     Was going to add Block10-V12 (which Wayne was using),
;;      but it was ALREADY THERE!

;;  5-1-2016  Version 25
;;     Changed order of CLIMB12-PIECE-PATTERNS  (also the numbering of types)
;;       Purpose to reconcile with the ordering of setcov-resource-problems-4
;;           Desired consistent order:
;;             1. All piece types (in same order) EXCEPT T-piece and 1x1 Singletons
;;             2. T piece (optional in setcov-resource-problems-4)
;;             3. Singletons (omitted in setcov-resource-problems-4)
;;  5-10-2016 (still) Version 25
;;     Made above changes to piece-types and re-ordering start-lst for Climb15 (climb15a,b,c,&d)
;;   NOTE: Must do similar reconciliation for Climb24 if want to do similar searches from solution positions
;;  6-15-2016 (still) Version 25
;;     Added new inits:
;;         (CLIMB12-NO-SINGLETONS-INIT)
;;         (CLIMB15A-NO-SINGLETONS-INIT)
;;         (CLIMB15A-2-SINGLETONS-INIT)
;;                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(special 
            *solution-target*
            *num-piece-types*
            *exit-row*
            *rush-hour-level*
            ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Donkey Puzzle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    _ ___ _ 
;;   | |   | |
;;   |_|___|_|
;;   | |___| |
;;   |_|_|_|_|
;;   |_|. .|_|


(defun donkey-init ()
  (slide-init (donkey-board-template)
              (donkey-piece-types)
              (donkey-start-list))
  (single-target-from-row-col 0 3 1))

(defun donkey-start-list ()
  '((0 0 1)                             ; ( piece-type row col)
    (1 0 0)
    (1 0 3)
    (1 2 0)
    (1 2 3)
    (2 2 1)
    (3 3 1)
    (3 3 2)
    (3 4 0)
    (3 4 3)))

(defun donkey-board-template (&optional obstacles)
  (let ((template (make-array '(5 4) :initial-element nil)))
    (loop for obstacle in obstacles
          do
          (setf (aref template (first obstacle)(second obstacle))
                "X"))
    template))

(defun donkey-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))        ; 2x2 square
    ((0 0)(1 0))                  ; 2x1 vertical rectangle
    ((0 0)(0 1))                  ; 1x2 horizontal rectangle
    ((0 0))))                     ; 1x1 unit square

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Donkey 2 - test obstacles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun donkey-2-init ()
  (slide-init (donkey-board-template '((0 0)))
              (donkey-piece-types)
              (donkey-2-start-list))
  (single-target-from-row-col 0 3 1))

(defun donkey-2-start-list ()
  '((0 0 1)                             ; ( piece-type row col)
    (3 1 0)
    (1 0 3)
    (1 2 0)
    (1 2 3)
    (2 2 1)
    (3 3 1)
    (3 3 2)
    (3 4 0)
    (3 4 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blockado  (cf. Slocum and Botermans "Ingenious & Diabolical" p. 118)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   _______          _______
;;  |   |___|        |___|___|
;;  |___|___|        | | |_|_|
;;  |x|x|___|   -->  |_|_|___|
;;  | | |___|        |x|x|___|
;;  |_|_|___|        |   |___|
;;  |_|_|___|        |___|___|
;;
;;          Above represents the "exact" target
;;          (alternative is the "key" target, just position the 2x2 piece correctly)

(defun blockado-key-init ()
  (slide-init (make-array '(6 4) :initial-element nil)
              (donkey-piece-types)
              (blockado-start-list))
  (single-target-from-row-col 0 4 0))

(defun blockado-exact-init ()
  (slide-init (make-array '(6 4) :initial-element nil)
              (donkey-piece-types)
              (blockado-start-list))
  (setf *solution-target*
        (blockado-exact-target)))

(defun blockado-start-list ()
  '((0 0 0)
    (1 3 0)
    (1 3 1)
    (2 0 2)
    (2 1 2)
    (2 2 2)
    (2 3 2)
    (2 4 2)
    (2 5 2)
    (3 5 0)
    (3 5 1)))

(defun blockado-exact-target ()
  (let ((compiled-target-pos 
         (compile-list-position
          (list-position-from-start-positions-list 
           (blockado-exact-target-placement-list)))))
    (loop for piece-type from 0 below *num-piece-types*
          collect
          (list piece-type (aref compiled-target-pos piece-type)))))

(defun blockado-exact-target-placement-list ()
  '((0 4 0)
    (1 1 0)
    (1 1 1)
    (2 0 0)
    (2 0 2)
    (2 2 2)
    (2 3 2)
    (2 4 2)
    (2 5 2)
    (3 1 2)
    (3 1 3)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Puzzle II (Mac program)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   _______          _______
;;  | |   | |        |_|x_x|_|
;;  |_|___|_|        |___|___|
;;  |_|_|_|_|   -->  |_|_|_|_|
;;  |___|___|        | |   | |
;;  |_|x_x|_|        |_|___|_|
;;
;; Can be done in 78 moves (exact bound for normal moves)
;;

(defun puzzle-ii-init ()
  (slide-init (donkey-board-template)
              (donkey-piece-types)
              (puzzle-ii-start-list))
  (setf *solution-target*
        (puzzle-ii-target)))

(defun puzzle-ii-target ()
  (let ((compiled-target-pos 
         (compile-list-position
          (list-position-from-start-positions-list 
           (puzzle-ii-target-placement-list)))))
    (loop for piece-type from 0 below *num-piece-types*
          collect
          (list piece-type (aref compiled-target-pos piece-type)))))

(defun puzzle-ii-target-placement-list ()
  '((0 3 1)
    (1 3 0)
    (1 3 3)
    (2 1 0)
    (2 1 2)
    (3 2 0)
    (3 2 1)
    (3 2 2)
    (3 2 3)
    (3 0 0)
    (3 0 3)))

(defun puzzle-ii-start-list ()
  '((0 0 1)
    (1 0 0)
    (1 0 3)
    (2 3 0)
    (2 3 2)
    (3 2 0)
    (3 2 1)
    (3 2 2)
    (3 2 3)
    (3 4 0)
    (3 4 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Century and Century-and-a-Half Puzzle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   _______
;;  |_|   |_|
;;  | |___| |
;;  |_| |x|_|
;;  |_|_|x|_|
;;  |___|___|
;;  
;; In original, vertical piece is position 1/2 unit to right,
;;    so add extra move to solution

(defun century-init ()
  (slide-init (donkey-board-template)
              (donkey-piece-types)
              (century-start-list))
  (century-target))

(defun century-start-list ()
  '((0 0 1)
    (1 1 0)
    (1 1 3)
    (1 2 1)
    (2 4 0)
    (2 4 2)
    (3 0 0)
    (3 0 3)
    (3 3 0)
    (3 3 3)))

(defun century-target ()
  (single-target-from-row-col 0 3 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fuji25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   _______
;;  | |   | |
;;  |_|___|_|
;;  |x|___|x|
;;  |_|___|_|
;;  |_|___|_|
;;  
;;  Fuji page claims solvable in 101 moves


(defun fuji25-init ()
  (slide-init (donkey-board-template)
              (donkey-piece-types)
              (fuji25-start-list))
  (fuji25-target))

(defun fuji25-start-list ()
  '((0 0 1)
    (1 0 0)
    (1 0 3)
    (2 2 1)
    (2 3 1)
    (2 4 1)
    (3 3 0)
    (3 3 3)
    (3 4 0)
    (3 4 3)))

(defun fuji25-target ()
  (single-target-from-row-col 0 3 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SuperCompo by Junk Kato (cf. Nobnet 705)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   _______
;;  |x|   |x|
;;  |_|___|_|
;;  | |___| |
;;  |_|___|_|
;;  |_|___|_|
;;  


(defun supercompo-init ()
  (slide-init (donkey-board-template)
              (donkey-piece-types)
              (supercompo-start-list))
  (supercompo-target))

(defun supercompo-start-list ()
  '((0 0 1)
    (1 2 0)
    (1 2 3)
    (2 2 1)
    (2 3 1)
    (2 4 1)
    (3 1 0)
    (3 1 3)
    (3 4 0)
    (3 4 3)))

(defun supercompo-target ()
  (single-target-from-row-col 0 3 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SuperCompoSymmetric by Toshi (Junk) Kato (cf. Nobnet 740)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Start:      Finish:
;;   _______     _______
;;  |x|   |x|   |_|___|_|
;;  |_|___|_|   | |___| |
;;  | |___| |   |_|___|_|
;;  |_|___|_|   |_|   |_|
;;  |_|___|_|   |x|___|x|
;;  

(defun supercompo-symmetric-init ()
  (slide-init (donkey-board-template)
              (donkey-piece-types)
              (supercompo-start-list))
  (setf *solution-target*
        (supercompo-symmetric-target)))

(defun supercompo-symmetric-target ()
  (let ((compiled-target-pos 
         (compile-list-position
          (list-position-from-start-positions-list 
           (supercompo-symmetric-target-placement-list)))))
    (loop for piece-type from 0 below *num-piece-types*
          collect
          (list piece-type (aref compiled-target-pos piece-type)))))

(defun supercompo-symmetric-target-placement-list ()
  '((0 3 1)
    (1 1 0)
    (1 1 3)
    (2 0 1)
    (2 1 1)
    (2 2 1)
    (3 0 0)
    (3 0 3)
    (3 3 0)
    (3 3 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HifiSuperCompo (by Junk Kato - NOBNET 889)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;I'd like to present a sliding block puzzle here like Hordern's book.
;It comes from junk Hi-Fi SUPERCOMPO.
;
; <Start>              <Finish>
; +---+-------+---+    +---+-------+---+
; |///|       |///|    |///|       |///|
; +---+   1   +---+    +---+   1   +---+
; | 5 |       | 6 |    | 5 |       | 6 |
; +---+-------+---+    +---+-------+---+
; |   |   2   |   |    |   |   2   |   |
; | A +-------+ B |    | B +-------+ A |
; |   |   3   |   |    |   |   3   |   |
; +---+-------+---+    +---+-------+---+
; | 7 |   4   | 8 |    | 7 |   4   | 8 |
; +---+-------+---+    +---+-------+---+
;Title:         HIFI ---NOBNET 889 by Junk Kato, Japan.
;Date:          1998.
;Start position:As shown in the diagram.
;Object:        Exchange the posisions of pieces A and B
;               with all the other pieces in their original positions.
;Solution:      Unknown
;Rating:        *****?
;

(defun hifi-supercompo-init ()
  (slide-init (donkey-board-template)
              (hifi-supercompo-piece-types)
              (hifi-supercompo-start-list))
  (setf *solution-target*
        (hifi-supercompo-target)))

(defun hifi-supercompo-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))        ; 0 "1" 2x2 square
    ((0 0)(0 1))                  ; 1 "2" 1x2 horizontal rectangle
    ((0 0)(0 1))                  ; 2 "3" 1x2 horizontal rectangle
    ((0 0)(0 1))                  ; 3 "4" 1x2 horizontal rectangle
    ((0 0)(1 0))                  ; 4 "A" 2x1 vertical rectangle
    ((0 0)(1 0))                  ; 5 "B" 2x1 vertical rectangle
    ((0 0))                       ; 6 "5" 1x1 unit square
    ((0 0))                       ; 7 "6" 1x1 unit square
    ((0 0))                       ; 8 "7" 1x1 unit square
    ((0 0))))                     ; 9 "8" 1x1 unit square

(defun hifi-supercompo-start-list ()
  '((0 0 1)
    (1 2 1)
    (2 3 1)
    (3 4 1)
    (4 2 0)
    (5 2 3)
    (6 1 0)
    (7 1 3)
    (8 4 0)
    (9 4 3)))

(defun hifi-supercompo-target ()
  (let ((compiled-target-pos 
         (compile-list-position
          (list-position-from-start-positions-list 
           (hifi-supercompo-target-placement-list)))))
    (loop for piece-type from 0 below *num-piece-types*
          collect
          (list piece-type (aref compiled-target-pos piece-type)))))

(defun hifi-supercompo-target-placement-list ()
  '((0 0 1)
    (1 2 1)
    (2 3 1)
    (3 4 1)
    (4 2 3)
    (5 2 0)
    (6 1 0)
    (7 1 3)
    (8 4 0)
    (9 4 3)))


;;; HIFI-AB SuperCompo (only A and B pieces are distinct, others not labelled so are interchangeable)

(defun hifi-ab-supercompo-init ()
  (slide-init (donkey-board-template)
              (hifi-ab-supercompo-piece-types)
              (hifi-ab-supercompo-start-list))
  (setf *solution-target*
	(hifi-ab-supercompo-target)))

(defun hifi-ab-supercompo-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))        ; 0 "1" 2x2 square
    ((0 0)(0 1))                  ; 1 "2" 1x2 horizontal rectangle
    ((0 0)(1 0))                  ; 2 "A" 2x1 vertical rectangle
    ((0 0)(1 0))                  ; 3 "B" 2x1 vertical rectangle
    ((0 0))))                     ; 4 "5" 1x1 unit square


(defun hifi-ab-supercompo-start-list ()
  '((0 0 1)
    (1 2 1)
    (1 3 1)
    (1 4 1)
    (2 2 0)  ; A
    (3 2 3)  ; B
    (4 1 0)
    (4 1 3)
    (4 4 0)
    (4 4 3)))

(defun hifi-ab-supercompo-target ()
  (let ((compiled-target-pos 
         (compile-list-position
          (list-position-from-start-positions-list 
           (hifi-ab-supercompo-target-placement-list)))))
    (loop for piece-type from 0 below *num-piece-types*
          collect
          (list piece-type (aref compiled-target-pos piece-type)))))

(defun hifi-ab-supercompo-target-placement-list ()
  '((0 0 1)
    (1 2 1)
    (1 3 1)
    (1 4 1)
    (3 2 0)  ; B
    (2 2 3)  ; A
    (4 1 0)
    (4 1 3)
    (4 4 0)
    (4 4 3)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loyd's Lunacy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   _________
;;  |   |_|___|
;;  |___|_|___|
;;  | | |x| | |
;;  |_|_|x|_|_|

(defun loyd-lunacy-init ()
  (slide-init (make-array '(4 5) :initial-element nil)
              (donkey-piece-types)
              (loyd-lunacy-start-list))
  (single-target-from-row-col 0 0 3))

(defun loyd-lunacy-start-list ()
  '((0 0 0)
    (1 2 0)
    (1 2 1)
    (1 2 3)
    (1 2 4)
    (2 0 3)
    (2 1 3)
    (3 0 2)
    (3 1 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hardtime (from Taniguchi's slide solver)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Start:
;;   _______ _______ ___
;;  | Green |  Red  |   |
;;  |_______|_______| P |
;;  | Blue  | N | B |   |
;;  |_______|___|___|___|
;;  | x | x |Orange | Y |
;;  |___|___|_______|___|
;;
;;  Finish:
;;   _______ _______ ___
;;  | Blue  |Orange | Y |
;;  |_______|_______|___|
;;  | Green | B | N |   |
;;  |_______|___|___| P |
;;  | x | x |  Red  |   |
;;  |___|___|_______|___|

(defun hardtime-init ()
  (slide-init (make-array '(3 5) :initial-element nil)
              (hardtime-piece-types)
              (hardtime-start-list))
  (setf *solution-target*
        (hardtime-target)))

(defun hardtime-piece-types ()
  '(((0 0)(0 1))                  ; 0 "Green"  1x2 horizontal rectangle
    ((0 0)(0 1))                  ; 1 "Red"    1x2 horizontal rectangle
    ((0 0)(0 1))                  ; 2 "Blue"   1x2 horizontal rectangle
    ((0 0)(0 1))                  ; 3 "Orange" 1x2 horizontal rectangle
    ((0 0)(1 0))                  ; 4 "Purple" 2x1 vertical rectangle
    ((0 0))                       ; 5 "Y"      1x1 unit square
    ((0 0))                       ; 6 "N"      1x1 unit square
    ((0 0))))                     ; 7 "B"      1x1 unit square

(defun hardtime-start-list ()
  '((0 0 0)      ; Green
    (1 0 2)      ; Red
    (2 1 0)      ; Blue
    (3 2 2)      ; Orange
    (4 0 4)      ; P (purple)
    (5 2 4)      ; Y
    (6 1 2)      ; N
    (7 1 3)))    ; B

(defun hardtime-target ()
  (let ((compiled-target-pos 
         (compile-list-position
          (list-position-from-start-positions-list 
           (hardtime-target-placement-list)))))
    (loop for piece-type from 0 below *num-piece-types*
          collect
          (list piece-type (aref compiled-target-pos piece-type)))))

(defun hardtime-target-placement-list ()
  '((0 1 0)      ; Green
    (1 2 2)      ; Red
    (2 0 0)      ; Blue
    (3 0 2)      ; Orange
    (4 1 4)      ; P (purple)
    (5 0 4)      ; Y
    (6 1 3)      ; N
    (7 1 2)))    ; B



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen (1 through 12)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dirty-dozen-template ()
  (make-array '(5 6) :initial-element nil))

(defun set-dirty-dozen-target ()
  (single-target-from-row-col 0 3 4))
       
(defun dirty-dozen-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 2x2 square
    ((0 0)(0 1)(1 0))                   ; Upper Left L
    ((0 1)(1 0)(1 1))                   ; Lower Right L
    ((0 0)(1 0))                        ; 2x1 vertical rectangle
    ((0 0)(0 1))                        ; 1x2 horizontal rectangle
    ((0 0))))                           ; 1x1 unit square

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________
;;  |   |_|_|___|
;;  |___|_|_|___|
;;  |x_x|_|_|___|
;;  |  _| |  _| |
;;  |_|___|_|___|

(defun dirty-dozen-1-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-1-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-1-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 2x2 square
    ((0 0)(0 1)(1 0))                   ; Upper Left L
    ((0 1)(1 0)(1 1))                   ; Lower Right L
    ((0 0)(0 1))                        ; 1x2 horizontal rectangle
    ((0 0))))                           ; 1x1 unit square

(defun dirty-dozen-1-start-list ()
  '((0 0 0)
    (1 3 0)
    (1 3 3)
    (2 3 1)
    (2 3 4)
    (3 0 4)
    (3 1 4)
    (3 2 4)
    (4 0 2)
    (4 1 2)
    (4 2 2)
    (4 0 3)
    (4 1 3)
    (4 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________
;;  |   |_|_|_|x|
;;  |___|_|_|_|x|
;;  |___|___|___|
;;  |  _| |  _| |
;;  |_|___|_|___|

(defun dirty-dozen-2-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-2-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-2-start-list ()
  '((0 0 0)
    (1 3 0)
    (1 3 3)
    (2 3 1)
    (2 3 4)
    (3 2 0)
    (3 2 2)
    (3 2 4)
    (4 0 2)
    (4 1 2)
    (4 0 3)
    (4 1 3)
    (4 0 4)
    (4 1 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________
;;  |   |_|  _| |
;;  |___|_|_|___|
;;  |___|_|  _| |
;;  |___|_|_|___|
;;  |___|_|_|x_x|

(defun dirty-dozen-3-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-3-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-3-start-list ()
  '((0 0 0)
    (1 0 3)
    (1 2 3)
    (2 0 4)
    (2 2 4)
    (3 2 0)
    (3 3 0)
    (3 4 0)
    (4 0 2)
    (4 1 2)
    (4 2 2)
    (4 3 2)
    (4 4 2)
    (4 4 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________
;;  |   |  _| |_|
;;  |___|_|___|_|
;;  |  _|___|_|_|
;;  |_| |___|_|x|
;;  |___|___|_|x|

(defun dirty-dozen-4-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-4-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-4-start-list ()
  '((0 0 0)
    (1 0 2)
    (1 2 0)
    (2 0 3)
    (2 3 0)
    (3 2 2)
    (3 3 2)
    (3 4 2)
    (4 0 5)
    (4 1 5)
    (4 2 5)
    (4 2 4)
    (4 3 4)
    (4 4 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________
;;  |   |___|_|_|
;;  |___|___|_|_|
;;  |x_x|___|_|_|
;;  |  _| |  _| |
;;  |_|___|_|___|

(defun dirty-dozen-5-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-5-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-5-start-list ()
  '((0 0 0)
    (1 3 0)
    (1 3 3)
    (2 3 1)
    (2 3 4)
    (3 0 2)
    (3 1 2)
    (3 2 2)
    (4 0 4)
    (4 1 4)
    (4 2 4)
    (4 0 5)
    (4 1 5)
    (4 2 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________
;;  |   |_|_|_|x|
;;  |___|_|_|_|x|
;;  |___|  _|  _|
;;  |___|_| |_| |
;;  |___|___|___|

(defun dirty-dozen-6-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-6-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-6-start-list ()
  '((0 0 0)
    (1 2 2)
    (1 2 4)
    (2 3 2)
    (2 3 4)
    (3 2 0)
    (3 3 0)
    (3 4 0)
    (4 0 2)
    (4 1 2)
    (4 0 3)
    (4 1 3)
    (4 0 4)
    (4 1 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________
;;  |   |_|  _| |
;;  |___|_|_|___|
;;  |___|_|_|x_x|
;;  |___|_|  _| |
;;  |___|_|_|___|

(defun dirty-dozen-7-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-7-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-7-start-list ()
  '((0 0 0)
    (1 0 3)
    (1 3 3)
    (2 0 4)
    (2 3 4)
    (3 2 0)
    (3 3 0)
    (3 4 0)
    (4 0 2)
    (4 1 2)
    (4 2 2)
    (4 2 3)
    (4 3 2)
    (4 4 2)))

(defun dirty-dozen-7-nine-from-end ()
  '((0 3 2)
    (1 0 0)
    (1 0 3)
    (2 0 1)
    (2 0 4)
    (3 3 0)
    (3 4 0)
    (3 3 4)
    (4 2 0)
    (4 2 1)
    (4 2 2)
    (4 2 3)
    (4 4 4)
    (4 4 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________
;;  |   |x|  _| |
;;  |___|x|_|___|
;;  |_|_|___|  _|
;;  |_|_|___|_| |
;;  |_|_|___|___|

(defun dirty-dozen-8-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-8-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-8-start-list ()
  '((0 0 0)
    (1 0 3)
    (1 2 4)
    (2 0 4)
    (2 3 4)
    (3 2 2)
    (3 3 2)
    (3 4 2)
    (4 2 0)
    (4 2 1)
    (4 3 0)
    (4 3 1)
    (4 4 0)
    (4 4 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   ___________
;;  |   |  _|_|_|
;;  |___|_| |_|_|
;;  |___|___|_|_|
;;  |___|  _| |x|
;;  |___|_|___|x|

(defun dirty-dozen-9-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-9-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-9-start-list ()
  '((0 0 0)
    (1 0 2)
    (1 3 2)
    (2 1 2)
    (2 3 3)
    (3 2 0)
    (3 3 0)
    (3 4 0)
    (4 0 4)
    (4 0 5)
    (4 1 4)
    (4 1 5)
    (4 2 4)
    (4 2 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   ___________
;;  |x_x|_|  _| |
;;  |_|_|_|_|___|
;;  |_|_|   |  _|
;;  |___|___|_| |
;;  |___|___|___|

(defun dirty-dozen-10-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-1-piece-types)
              (dirty-dozen-10-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-10-start-list ()
  '((0 2 2)
    (1 0 3)
    (1 2 4)
    (2 0 4)
    (2 3 4)
    (3 3 0)
    (3 4 0)
    (3 4 2)
    (4 0 2)
    (4 1 0)
    (4 1 1)
    (4 1 2)
    (4 2 0)
    (4 2 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   ___________
;;  |   |x|  _|_|
;;  |___|x|_|_| |
;;  |___|_|_|___|
;;  |  _| |_| | |
;;  |_|___|_|_|_|


(defun dirty-dozen-11-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-11-piece-types)
              (dirty-dozen-11-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-11-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 2x2 square
    ((0 0)(0 1)(1 0))                   ; Upper Left L
    ((0 1)(1 0)(1 1))                   ; Lower Right L    NOTE: Lacks ORIGIN (ok since no obstacles)
    ((0 0)(1 0))                        ; 2x1 vertical rectangle
    ((0 0)(0 1))                        ; 1x2 horizontal rectangle
    ((0 0))))                           ; 1x1 unit square

(defun dirty-dozen-11-start-list ()
  '((0 0 0)
    (1 3 0)
    (1 0 3)
    (2 3 1)
    (2 1 4)
    (3 3 4)
    (3 3 5)
    (4 2 0)
    (5 0 5)
    (5 1 4)
    (5 2 2)
    (5 2 3)
    (5 3 3)
    (5 4 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dirty-Dozen #12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   ___________
;;  |x|  _| |  _|
;;  |x|_|___|_| |
;;  |_|_|   | |_|
;;  |_|_|___|_| |
;;  |_|_|___|___|


(defun dirty-dozen-12-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-11-piece-types)
              (dirty-dozen-12-start-list))
  (set-dirty-dozen-target))

(defun dirty-dozen-12-start-list ()
  '((0 2 2)
    (1 0 1)
    (1 0 4)
    (2 0 2)
    (2 3 4)
    (3 1 5)
    (3 2 4)
    (4 4 2)
    (5 2 0)
    (5 2 1)
    (5 3 0)
    (5 3 1)
    (5 4 0)
    (5 4 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; David's Puzzle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;   ___________
;;  |   |_|_|  _|
;;  |___|___|_| |
;;  |  _| |_|___|
;;  |_| |_|___|_|
;;  |___|x_x|_|_|


(defun david-puzzle-init ()
  (slide-init (dirty-dozen-template)
              (dirty-dozen-11-piece-types)
              (david-puzzle-start-list))
  (set-dirty-dozen-target))

(defun david-puzzle-start-list ()
  '((0 0 0)
    (1 0 4)
    (1 2 0)
    (2 1 4)
    (2 3 0)
    (3 2 2)
    (4 1 2)
    (4 3 3)
    (5 0 2)
    (5 0 3)
    (5 2 3)
    (5 3 5)
    (5 4 4)
    (5 4 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  "Ma" type puzzles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ma-template ()
  (make-array '(5 5) :initial-element nil))

(defun mini-ma-template ()
  (make-array '(5 4) :initial-element nil))


(defun ma-ab-target-2-by-3 (row col)
  (double-target-for-distinct-pieces-from-row-cols 0 row col 1 row (1+ col)))

(defun ma-ab-target-3-by-2 (row col)
  (double-target-for-distinct-pieces-from-row-cols 0 row col 1 (1+ row) col))



;;; NOTE: that "Lower Right L" won't work with obstacles unless redefined!
(defun mini-ma-piece-types ()      ;Omit 1x3 rectangle
  '(((0 0)(0 1)(1 0))                   ; Upper Left L
    ((0 1)(1 0)(1 1))                   ; Lower Right L    NOTE: Lacks Origin (ok since no obstacles)
    ((0 0)(1 0))                        ; 2x1 vertical rectangle
    ((0 0)(0 1))                        ; 1x2 horizontal rectangle
    ((0 0))))                           ; 1x1 unit square

(defun ma-piece-types ()
  '(((0 0)(0 1)(1 0))                   ; Upper Left L
    ((0 1)(1 0)(1 1))                   ; Lower Right L    NOTE: Lacks Origin (ok since no obstacles)
    ((0 0)(1 0))                        ; 2x1 vertical rectangle
    ((0 0)(0 1))                        ; 1x2 horizontal rectangle
    ((0 0))                             ; 1x1 unit square
    ((0 0)(0 1)(0 2))))                 ; 1x3 long horizontal rectangle


;;;;;;;; Mini Ma (cf. Henderson, Nobnet 572)


;;; This one has no solutions
(defun mini-ma-henderson-horiz-target-init ()
  (slide-init (mini-ma-template)
              (mini-ma-piece-types)
              (mini-ma-henderson-start-list))
  (ma-ab-target-2-by-3 0 0))

;;; This requires 199 piece moves (279 single steps)
(defun mini-ma-henderson-vert-target-init ()
  (slide-init (mini-ma-template)
              (mini-ma-piece-types)
              (mini-ma-henderson-start-list))
  (ma-ab-target-3-by-2 0 0))

(defun mini-ma-henderson-start-list ()
  '((0 2 0)
    (1 0 2)
    (2 2 2)
    (2 2 3)
    (2 3 1)
    (3 1 0)
    (4 0 0)
    (4 4 0)
    (4 4 2)
    (4 4 3)))


;;;;;;;; Maxi Ma (cf. Henderson, Nobnet 572)


;;; This requires 683 piece moves (888 single steps)
(defun maxi-ma-henderson-horiz-target-init ()
  (slide-init (ma-template)
              (ma-piece-types)
              (maxi-ma-henderson-start-list))
  (ma-ab-target-2-by-3 0 2))

;;; This requires 678 piece moves (884 single steps)
(defun maxi-ma-henderson-vert-target-init ()
  (slide-init (ma-template)
              (ma-piece-types)
              (maxi-ma-henderson-start-list))
  (ma-ab-target-3-by-2 0 3))

(defun maxi-ma-henderson-start-list ()
  '((0 2 2)
    (1 1 0)
    (2 0 2)                             ; vertical 2x1
    (2 0 3)
    (2 0 4)
    (3 3 0)                             ; horizontal 1x2
    (3 3 3)
    (4 1 0)                             ; 1x1 unit squares
    (4 2 4)
    (4 4 0)
    (4 4 1)
    (5 4 2)))                           ; horizontal 1x3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Puzzle Beast (18 move puzzle created by mutation/hill-climbing)
;;    cf. www.puzzlebeast.com/about/index.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Goal: get piece 1 (vertical 2x1 rectangle) into top-left corner

;   . 3 0 0
;   2 2 . 0
;   0 0 . 1
;   . 0 . 1


(defun puzzle-beast-init ()
  (slide-init (puzzle-beast-template)
              (puzzle-beast-piece-types)
              (puzzle-beast-start-list))
  (set-puzzle-beast-target))

(defun puzzle-beast-template ()
  (make-array (list 4 4) :initial-element nil))

(defun puzzle-beast-piece-types ()
  '(((0 0)(0 1)(1 1))                   ; Upper Right L
    ((0 0)(1 0))                        ; 2x1 vertical rectangle
    ((0 0)(0 1))                        ; 1x2 horizontal rectangle
    ((0 0))))                           ; 1x1 small square

(defun puzzle-beast-start-list ()
  '((1 2 3)     ; 2x1 vertical rectangle
    (2 1 0)     ; horizontal rectangle
    (0 0 2)     ; 3-cell upper right L
    (0 2 0)     ; 3-cell upper right L
    (3 0 1)))   ; 1x1 small square

(defun set-puzzle-beast-target ()
  (single-target-from-row-col 1 0 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Block-10 (from Minoru Abe Gallery)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Move counts given for variants 11,12,13,14  (42,30,48,40 moves respectively)

;;; common to all variants

(defun block10-template ()
  (obstacle-template 6 4 '((0 0)(0 3))))

(defun set-block10-target ()
  (single-target-from-row-col 0 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Block10 -- Variant 11:
;;;    verified 42 moves
;;       ___
;; 0   _|x x|_
;; 1  | |x x| |
;; 2  |___|_|_|
;; 3  |_|   |_|
;; 4  | |___| | 
;; 5  |_|_|___|
;;

(defun block10-v11-init ()
  (slide-init (block10-template)
              (block10-v11-piece-types)
              (block10-v11-start-list))
  (set-block10-target))

(defun block10-v11-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 0  2x2 square
    ((0 0)(1 0)(1 1))                   ; 1  Lower Left pointing L
    ((0 0)(1 -1)(1 0))                  ; 2  Lower Right pointing L
    ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
    ((0 0))))                           ; 4  1x1 unit square

(defun block10-v11-start-list ()
  '((0 3 1)
    (1 1 0)
    (2 4 3)
    (3 1 3)
    (3 4 0)
    (4 2 2)
    (4 3 0)
    (4 3 3)
    (4 5 1)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Block10 -- Variant 12:
;;;    verified 30 moves
;;       ___
;; 0   _|x x|_
;; 1  | |x x| |
;; 2  |_|_|___|
;; 3  |  _|_| |
;; 4  |_|   |_| 
;; 5  |_|___|_|
;;

(defun block10-v12-init ()
  (slide-init (block10-template)
              (block10-v12-piece-types)
              (block10-v12-start-list))
  (set-block10-target))

(defun block10-v12-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 0  2x2 square
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(1 -1)(1 0))                  ; 2  Lower Right pointing L
    ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
    ((0 0))))                           ; 4  1x1 unit square

(defun block10-v12-start-list ()
  '((0 4 1)
    (1 3 0)
    (2 1 3)
    (3 1 0)
    (3 3 3)
    (4 2 1)
    (4 3 2)
    (4 5 0)
    (4 5 3)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Block10 -- Variant 13:
;;;    verified 48 moves
;;       ___
;; 0   _|x x|_
;; 1  | |x x|_|
;; 2  |___| __|
;; 3  | |_|_|_|
;; 4  |_|   | | 
;; 5  |_|___|_|
;;

(defun block10-v13-init ()
  (slide-init (block10-template)
              (block10-v13-piece-types)
              (block10-v13-start-list))
  (set-block10-target))

(defun block10-v13-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 0  2x2 square
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(1 0)(1 1))                   ; 2  Lower Left pointing L
    ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
    ((0 0))))                           ; 4  1x1 unit square

(defun block10-v13-start-list ()
  '((0 4 1)
    (1 2 2)
    (2 1 0)
    (3 3 0)
    (3 4 3)
    (4 1 3)
    (4 3 1)
    (4 3 3)
    (4 5 0)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Block10 -- Variant 14:
;;;    verified 40 moves
;;       ___
;; 0   _|x x|_
;; 1  |_|x x| |
;; 2  |__ | |_|
;; 3  |_|_|___|
;; 4  | |   |_| 
;; 5  |_|___|_|
;;

(defun block10-v14-init ()
  (slide-init (block10-template)
              (block10-v14-piece-types)
              (block10-v14-start-list))
  (set-block10-target))

(defun block10-v14-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 0  2x2 square
    ((0 0)(0 1)(1 1))                   ; 1  Upper Right pointing L
    ((0 0)(1 0)(1 1))                   ; 2  Lower Left pointing L
    ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
    ((0 0))))                           ; 4  1x1 unit square

(defun block10-v14-start-list ()
  '((0 4 1)
    (1 2 0)
    (2 2 2)
    (3 1 3)
    (3 4 0)
    (4 1 0)
    (4 3 0)
    (4 4 3)
    (4 5 3)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| for reference (all the pieces needed for Block10 Variants):
(defun climb24-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 0  2x2 square
    ((0 0)(1 -1)(1 0)(1 1))             ; 1  4 square T (stem up)
    ((0 0)(0 1)(1 0))                   ; 2  Upper Left pointing L
    ((0 0)(0 1)(1 1))                   ; 3  Upper Right pointing L
    ((0 0)(1 -1)(1 0))                  ; 4  Lower Right pointing L
    ((0 0)(1 0)(1 1))                   ; 5  Lower Left pointing L
    ((0 0)(1 0))                        ; 6  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 7  1x2 horizontal rectangle
    ((0 0)(0 1)(0 2))                   ; 8  1x3 horizontal rectangle
    ((0 0))))                           ; 9  1x1 unit square

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; climb-12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  From Minoru Abe -- http://www.johnrausch.com/slidingblockpuzzles/abe.htm
;;
;;    verified 59 moves
;;         _
;; 0   ___|x|___
;; 1  | |x x x| |
;; 2  |_|  _| |_|
;; 3  |_|_|_ _|_|
;; 4  |_ _| |_ _|
;; 5  |_|_____|_|
;;

(defun climb12-template ()
  (obstacle-template 6 5 '((0 0)(0 1)(0 3)(0 4))))

(defun set-climb12-target ()
  (single-target-from-row-col 4 0 2))   ;; piece type for T changed from 0 to 4 in version 25

;;  NOTE: pieces re-ordered in version 25 -- not compatible with previous position representations
(defun climb12-piece-types ()
  '(((0 0)(0 1)(1 0))                   ; 0  Upper Left pointing L
    ((0 0)(1 -1)(1 0))                  ; 1  Lower Right pointing L
    ((0 0)(1 0))                        ; 2  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 3  1x2 horizontal rectangle
    ((0 0)(1 -1)(1 0)(1 1))             ; 4  4 square T (stem up)
    ((0 0))))                           ; 5  1x1 unit square

(defun climb12-init ()
  (slide-init (climb12-template)
              (climb12-piece-types)
              (climb12-start-list))
  (set-climb12-target))

(defun climb12-start-list ()
  '((0 2 1)
    (1 2 3)
    (2 1 0)
    (2 1 4)
    (3 4 0)
    (3 4 3)
    (4 4 2)   ;; T piece [NOTE: piece-types changed (re-ordered) in Version 25]
    (5 3 0)
    (5 3 4)
    (5 5 0)
    (5 5 4)))

;;  CLIMB12 WITHOUT SINGLETONS:
;;  Derived from Minoru Abe -- http://www.johnrausch.com/slidingblockpuzzles/abe.htm
;;
;;    verified 17 moves
;;         _
;; 0   ___|x|___
;; 1  | |x x x| |
;; 2  |_|  _| |_|
;; 3  |x|_|_ _|x|
;; 4  |_ _| |_ _|
;; 5  |x|_____|x|
;;

;;  NOTE: pieces re-ordered in version 25 -- not compatible with previous position representations
(defun climb12-no-singletons-piece-types ()
  '(((0 0)(0 1)(1 0))                   ; 0  Upper Left pointing L
    ((0 0)(1 -1)(1 0))                  ; 1  Lower Right pointing L
    ((0 0)(1 0))                        ; 2  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 3  1x2 horizontal rectangle
    ((0 0)(1 -1)(1 0)(1 1))             ; 4  4 square T (stem up)
    ;; ((0 0))                          ;  OMIT ALL 1x1 unit squares
    ))

(defun climb12-no-singletons-init ()
  (slide-init (climb12-template)
              (climb12-no-singletons-piece-types)
              (climb12-no-singletons-start-list))
  (set-climb12-target))

(defun climb12-no-singletons-start-list ()
  '((0 2 1)
    (1 2 3)
    (2 1 0)
    (2 1 4)
    (3 4 0)
    (3 4 3)
    (4 4 2)   ;; T piece [NOTE: piece-types changed (re-ordered) in Version 25]
    ; no singletons
    ;(5 3 0)
    ;(5 3 4)
    ;(5 5 0)
    ;(5 5 4)
    ))

;;  CLIMB12 WITH 2 SINGLETONS:
;;  Derived from Minoru Abe -- http://www.johnrausch.com/slidingblockpuzzles/abe.htm
;;
;;    verified 17 moves
;;         _
;; 0   ___|x|___
;; 1  | |x x x| |
;; 2  |_|  _| |_|
;; 3  |x|_|_ _|x|
;; 4  |_ _| |_ _|
;; 5  |_|_____|_|
;;

(defun climb12-2-singletons-init ()
  (slide-init (climb12-template)
              (climb12-piece-types)
              (climb12-2-singletons-start-list))
  (set-climb12-target))

(defun climb12-2-singletons-start-list ()
  '((0 2 1)
    (1 2 3)
    (2 1 0)
    (2 1 4)
    (3 4 0)
    (3 4 3)
    (4 4 2)   ;; T piece [NOTE: piece-types changed (re-ordered) in Version 25]
    ; 2 singletons
    ;(5 3 0)
    ;(5 3 4)
    (5 5 0)
    (5 5 4)
    ))



;;; Climb-12(b)

;;    verified 69 moves
;;         _
;; 0   ___|x|___
;; 1  | |x x x| |
;; 2  |_| __| |_|
;; 3  |_|_|_ _|_|
;; 4  |_ _| |_ _|
;; 5  |_|_____|_|
;;


(defun climb12b-piece-types ()
  '(((0 0)(1 -1)(1 0)(1 1))             ; 0  4 square T (stem up)
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(1 -1)(1 0))                  ; 2  Lower Right pointing L
    ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 4  1x2 horizontal rectangle
    ((0 0))))                           ; 5  1x1 unit square

(defun climb12b-init ()
  (slide-init (climb12-template)
              (climb12b-piece-types)
              (climb12b-start-list))
  (set-climb12-target))

(defun climb12b-start-list ()
  '((0 4 2)
    (1 4 0)
    (2 1 4)
    (3 2 0)
    (3 3 4)
    (4 2 1)
    (4 3 2)
    (5 1 0)
    (5 3 1)
    (5 4 3)
    (5 5 4)))

;;; Climb-12(c)

;;    verified 92 moves
;;         _
;; 0   ___|x|___
;; 1  | |x x x| |
;; 2  |_|_|___|_|
;; 3  | |___|_| |
;; 4  |_ _| |_ _|
;; 5  |_|_____|_|
;;


(defun climb12c-piece-types ()
  '(((0 0)(1 -1)(1 0)(1 1))             ; 0  4 square T (stem up)
    ((0 0)(1 0)(1 1))                   ; 1  Lower Left pointing L
    ((0 0)(1 -1)(1 0))                  ; 2  Lower Right pointing L
    ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 4  1x2 horizontal rectangle
    ((0 0))))                           ; 5  1x1 unit square

(defun climb12c-init ()
  (slide-init (climb12-template)
              (climb12c-piece-types)
              (climb12c-start-list))
  (set-climb12-target))

(defun climb12c-start-list ()
  '((0 4 2)
    (1 3 0)
    (2 3 4)
    (3 1 0)
    (3 1 4)
    (4 2 2)
    (4 3 1)
    (5 2 1)
    (5 3 3)
    (5 5 0)
    (5 5 4)))

;;; Climb-12(d)

;;   verified 70 moves
;;         _
;; 0   ___|x|___
;; 1  |_|x x x|_|
;; 2  | | |_|___|
;; 3  |_|_|_|___|
;; 4  |  _| |_  |
;; 5  |_|_____|_|
;;

(defun climb12d-piece-types ()
  '(((0 0)(1 -1)(1 0)(1 1))             ; 0  4 square T (stem up)
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(0 1)(1 1))			; 2  Upper Right pointing L
    ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 4  1x2 horizontal rectangle
    ((0 0))))                           ; 5  1x1 unit square

(defun climb12d-init ()
  (slide-init (climb12-template)
              (climb12d-piece-types)
              (climb12d-start-list))
  (set-climb12-target))

(defun climb12d-start-list ()
  '((0 4 2)
    (1 4 0)
    (2 4 3)
    (3 2 0)
    (3 2 1)
    (4 2 3)
    (4 3 3)
    (5 1 0)
    (5 1 4)
    (5 2 2)
    (5 3 2)))

;;; Climb-12(e)

;;;  My variation - created from (d) by a typo (joining 2 upper right pieces in 1)
;;;    Solved in 92 moves

;;         _
;; 0   ___|x|___
;; 1  |_|x x x| |
;; 2  | | |_|___|
;; 3  |_|_|_|___|
;; 4  |  _| |_  |
;; 5  |_|_____|_|
;;

(defun climb12e-piece-types ()
  '(((0 0)(1 -1)(1 0)(1 1))             ; 0  4 square T (stem up)
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(1 0)(1 -1))			; 2  Down right pointing L
    ((0 0)(0 1)(1 1))			; 3  Upper Right pointing L
    ((0 0)(1 0))                        ; 4  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 5  1x2 horizontal rectangle
    ((0 0))))                           ; 6  1x1 unit square

(defun climb12e-init ()
  (slide-init (climb12-template)
              (climb12e-piece-types)
              (climb12e-start-list))
  (set-climb12-target))

(defun climb12e-start-list ()
  '((0 4 2)
    (1 4 0)
    (2 1 4)
    (3 4 3)
    (4 2 0)
    (4 2 1)
    (5 3 3)
    (6 1 0)
    (6 2 2)
    (6 3 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Climb15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun obstacle-template (rows cols &optional obstacles)
  (let ((template (make-array (list rows cols) :initial-element nil)))
    (loop for obstacle in obstacles
          do
          (setf (aref template (first obstacle)(second obstacle))
                "X"))
    template))

(defun climb15-template ()
  (obstacle-template 8 5 '((0 0)(0 1)(0 3)(0 4))))

(defun set-climb15-target ()
  (single-target-from-row-col 7 0 2))         ;; T piece type changed to 7 in Version 25

;;  NOTE: pieces re-ordered (with re-numbered types) version 25 -- not compatible with previous position representations
(defun climb15-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 0  2x2 square
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(0 1)(1 1))                   ; 2  Upper Right pointing L
    ((0 0)(1 -1)(1 0))                  ; 3  Lower Right pointing L
    ((0 0)(1 0)(1 1))                   ; 4  Lower Left pointing L
    ((0 0)(1 0))                        ; 5  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 6  1x2 horizontal rectangle
    ((0 0)(1 -1)(1 0)(1 1))             ; 7  4 square T (stem up)
    ((0 0))))                           ; 8  1x1 unit square

(defun climb15-no-singletons-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 0  2x2 square
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(0 1)(1 1))                   ; 2  Upper Right pointing L
    ((0 0)(1 -1)(1 0))                  ; 3  Lower Right pointing L
    ((0 0)(1 0)(1 1))                   ; 4  Lower Left pointing L
    ((0 0)(1 0))                        ; 5  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 6  1x2 horizontal rectangle
    ((0 0)(1 -1)(1 0)(1 1))             ; 7  4 square T (stem up)
    ;; ((0 0))                          ; OMIT ALL 1x1 SINGLETONS
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; climb-15 (a)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hordern Puzzle D45    (target to verify: 104 moves)
;;         _
;; 0   ___|x|___
;; 1  |_|x x x|_|
;; 2  |  _| | | |
;; 3  |_|___|_|_|
;; 4  |   | |_  |
;; 5  |___|___|_|
;; 6  |___| |___|
;; 7  |_|_____|_|
;;

(defun climb15a-init ()
  (slide-init (climb15-template)
              (climb15-piece-types)
              (climb15a-start-list))
  (set-climb15-target))

(defun climb15a-start-list ()
  '((0 4 0)
    (1 2 0)
    (2 4 3)
    (3 2 2)
    (4 4 2)
    (5 2 3)
    (5 2 4)
    (6 6 0)
    (6 6 3)
    (7 6 2)   ; T-piece  -- types and starts re-ordered / modified in Version 25
    (8 1 0)
    (8 1 4)
    (8 7 0)
    (8 7 4)))

;; CLIMB15A with NO SINGLETONS:
;; Derived from Hordern Puzzle D45    (target to verify: 104 moves)
;;         _
;; 0   ___|x|___
;; 1  |x x x x x|
;; 2  |  _| | | |
;; 3  |_|___|_|_|
;; 4  |   | |_  |
;; 5  |___|___|_|
;; 6  |___| |___|
;; 7  |x|_____|x|
;;

(defun climb15a-no-singletons-init ()
  (slide-init (climb15-template)
              (climb15-no-singletons-piece-types)
              (climb15a-no-singletons-start-list))
  (set-climb15-target))

(defun climb15a-no-singletons-start-list ()
  '((0 4 0)
    (1 2 0)
    (2 4 3)
    (3 2 2)
    (4 4 2)
    (5 2 3)
    (5 2 4)
    (6 6 0)
    (6 6 3)
    (7 6 2)   ; T-piece  -- types and starts re-ordered / modified in Version 25
    ;; no singletons
    ;(8 1 0)
    ;(8 1 4)
    ;(8 7 0)
    ;(8 7 4)
    ))


;; CLIMB15A with 2 SINGLETONS:
;; Derived from Hordern Puzzle D45    (target to verify: 104 moves)
;;         _
;; 0   ___|x|___
;; 1  |x x x x x|
;; 2  |  _| | | |
;; 3  |_|___|_|_|
;; 4  |   | |_  |
;; 5  |___|___|_|
;; 6  |___| |___|
;; 7  |_|_____|_|
;;

(defun climb15a-2-singletons-init ()
  (slide-init (climb15-template)
              (climb15-piece-types)
              (climb15a-2-singletons-start-list))
  (set-climb15-target))

(defun climb15a-2-singletons-start-list ()
  '((0 4 0)
    (1 2 0)
    (2 4 3)
    (3 2 2)
    (4 4 2)
    (5 2 3)
    (5 2 4)
    (6 6 0)
    (6 6 3)
    (7 6 2)   ; T-piece  -- types and starts re-ordered / modified in Version 25
    ;; 2 singletons  (at bottom corners)
    ;(8 1 0)
    ;(8 1 4)
    (8 7 0)
    (8 7 4)
    ))


;;;  Climb15(b)

;; Hordern Puzzle D46     (target to verify: 132 moves)
;;         _
;; 0   ___|x|___
;; 1  |_|x x x|_|
;; 2  |___| |   |
;; 3  |___|_|___|
;; 4  |_  | |  _|
;; 5  | |_|_|_| |
;; 6  |___| |___|
;; 7  |_|_____|_|
;;

(defun climb15b-init ()
  (slide-init (climb15-template)
              (climb15-piece-types)
              (climb15b-start-list))
  (set-climb15-target))

(defun climb15b-start-list ()
  '((0 2 3)
    (1 4 3)
    (2 4 0)
    (3 5 4)
    (4 5 0)
    (5 2 2)
    (5 4 2)
    (6 2 0)
    (6 3 0)
    (7 6 2)   ; T-piece  -- types and starts re-ordered / modified in Version 25
    (8 1 0)
    (8 1 4)
    (8 7 0)
    (8 7 4)))

;;;  Climb15(c)

;; Hordern Puzzle D47    (target to verify: 101 moves)
;;         _
;; 0   ___|x|___
;; 1  |_|x x x|_|
;; 2  | |_| |_| |
;; 3  |___|_|___|
;; 4  |   | |___|
;; 5  |___|_|___|
;; 6  |  _| |_  |
;; 7  |_|_____|_|
;;

(defun climb15c-init ()
  (slide-init (climb15-template)
              (climb15-piece-types)
              (climb15c-start-list))
  (set-climb15-target))

(defun climb15c-start-list ()
  '((0 4 0)
    (1 6 0)
    (2 6 3)
    (3 2 4)
    (4 2 0)
    (5 2 2)
    (5 4 2)
    (6 4 3)
    (6 5 3)
    (1 6 2) ; T-piece  -- types and starts re-ordered / modified in Version 25
    (8 1 0)
    (8 1 4)
    (8 2 1)
    (8 2 3)))

;;;  Climb15(d)

;; Hordern Puzzle D48     (target to verify: 148 moves)
;;         _
;; 0   ___|x|___
;; 1  |_|x x x|_|
;; 2  |___| |___|
;; 3  |  _|_|_  |
;; 4  |_| |   |_|
;; 5  | |_|___| |
;; 6  |_ _| |___|
;; 7  |_|_____|_|
;;

(defun climb15d-init ()
  (slide-init (climb15-template)
              (climb15-piece-types)
              (climb15d-start-list))
  (set-climb15-target))

(defun climb15d-start-list ()
  '((0 4 2)
    (1 3 0)
    (2 3 4)
    (3 5 4)
    (4 5 0)
    (5 2 2)
    (5 4 1)
    (6 2 0)
    (6 2 3)
    (7 6 2)   ; T-piece  -- types and starts re-ordered / modified in Version 25
    (8 1 0)
    (8 1 4)
    (8 7 0)
    (8 7 4)))


  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; climb-pro-24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  From Minoru Abe -- http://www.johnrausch.com/slidingblockpuzzles/abe.htm
;;           _
;; 0   _____|x|_____
;; 1  |___|x x x|___|
;; 2  |   |_____|   |
;; 3  |___| |_  |___|
;; 4  |_  |___|_|  _|
;; 5  | |_| |_| |_| |
;; 6  |_| |_|_|_| |_|
;; 7  |___|_____|___|
;; 8  |   |_| |_|   |
;; 9  |___|_____|___|


(defun climb24-template ()
  (obstacle-template 10 7 '((0 0)(0 1)(0 2)(0 4)(0 5)(0 6))))

(defun set-climb24-target ()
  (single-target-from-row-col 0 0 3))

;; reordered piece types in version 21 (see version 20 for old ordering)
(defun climb24-piece-types ()
  '(((0 0)(1 -1)(1 0)(1 1))             ; 0  4 square T (stem up)
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(0 1)(1 1))                   ; 2  Upper Right pointing L
    ((0 0)(1 -1)(1 0))                  ; 3  Lower Right pointing L
    ((0 0)(1 0)(1 1))                   ; 4  Lower Left pointing L
    ((0 0)(0 1)(1 0)(1 1))              ; 5  2x2 square
    ((0 0)(1 0))                        ; 6  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 7  1x2 horizontal rectangle
    ((0 0)(0 1)(0 2))                   ; 8  1x3 horizontal rectangle
    ((0 0))))                           ; 9  1x1 unit square


(defun climb24-init ()
  (slide-init (climb24-template)
              (climb24-piece-types)
              (climb24-start-list))
  (set-climb24-target))

(defun climb24-start-list ()
  '((0 8 3)    ; T piece
    (1 4 5)    ; up-left L
    (2 3 3)    ; up-right L
    (2 4 0)    ; up-right L
    (3 6 1)    ; down-right L
    (4 3 2)    ; down-left L
    (4 6 5)    ; down-left L
    (5 2 0)    ; 2x2
    (5 2 5)    ; 2x2
    (5 8 0)    ; 2x2
    (5 8 5)    ; 2x2
    (6 5 0)    ; 2x1 (vertical)
    (6 5 2)    ; 2x1 (vertical)
    (6 5 4)    ; 2x1 (vertical)
    (6 5 6)    ; 2x1 (vertical)
    (7 1 0)    ; 1x2 (horizontal)
    (7 1 5)    ; 1x2 (horizontal)
    (8 2 2)    ; 1x3 (horizontal)
    (8 7 2)    ; 1x3 (horizontal)
    (9 5 3)    ; 1x1
    (9 6 3)    ; 1x1
    (9 8 2)    ; 1x1
    (9 8 4)    ; 1x1
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mini Climb Pro  [requires 561 moves according to Ext-A* search]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cf Henderson e-mail to Nobnet (27 July 2005)

;;         _
;; 0   ___| |___
;; 1  |_|x|___|x|
;; 2  |___|x|   |
;; 3  |_|_  |___|
;; 4  |_| |_|_| |
;; 5  |_|_|_| |_|
;; 6  |___|_____|
;;

#|
# # # # # # #
# # # 3 # # #
# 8   3 3   #
# 4 4   1 1 #
# 9 2 2 1 1 #
# A 6 2 B 7 #
# C 6 D 0 7 #
# 5 5 0 0 0 #
# # # # # # #
|#

(defun mini-climb-pro-init ()
  (slide-init (mini-climb-pro-template)
              (mini-climb-pro-piece-types)
              (mini-climb-pro-start-list))
  (set-mini-climb-pro-target))

(defun mini-climb-pro-template ()
  (obstacle-template 7 5 '((0 0)(0 1)(0 3)(0 4))))

(defun set-mini-climb-pro-target ()
  (single-target-from-row-col 5 0 2))         ;; T piece now type 5

;;  NOTE: pieces re-ordered (with re-numbered types) version 25 -- not compatible with previous position representations
(defun mini-climb-pro-piece-types ()
  '(((0 0)(0 1)(1 0)(1 1))              ; 0  2x2 square

    ((0 0)(0 1)(1 1))                   ; 1  Upper Right pointing L

    ((0 0)(1 0)(1 1))                   ; 2  Lower Left pointing L
    ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 4  1x2 horizontal rectangle
    ((0 0)(1 -1)(1 0)(1 1))             ; 5  4 cell T (stem up)
    ((0 0))))                           ; 6  1x1 unit square

(defun mini-climb-pro-start-list ()
  '((0 2 3)
    (1 3 1)   ;; NE
    (2 0 2)   ;; SW
    (3 4 1)   ;; 2x1
    (3 4 4)
    (4 2 0)   ;; 1x2
    (4 6 0)
    (5 5 3)   ;; T
    (6 1 0)   ;; 1x1
    (6 3 0)
    (6 4 0)
    (6 4 3)
    (6 5 0)
    (6 5 2)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hole in One
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;     _______
;; 0  |  _|_  |
;; 1  |_|x x|_|
;; 2  | |x x| |
;; 3  |___|___|
;; 4  |x|   |x|
;; 5  |x|___|x|

;;; Object: get 2x2 square into hole between the 4 "bent-L" pieces

(defun hole-in-one-template ()
  (obstacle-template 6 4 nil))

(defun set-hole-in-one-target ()
  (single-target-from-row-col 0 1 1))

;; reordered piece types in version 21 (see version 20 for old ordering)
(defun hole-in-one-piece-types ()
  '(((0 0)(1 -1)(1 0)(1 1))             ; 0  4 square T (stem up)
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(0 1)(1 1))                   ; 2  Upper Right pointing L
    ((0 0)(1 -1)(1 0))                  ; 3  Lower Right pointing L
    ((0 0)(1 0)(1 1))                   ; 4  Lower Left pointing L
    ((0 0)(0 1)(1 0)(1 1))              ; 5  2x2 square
    ((0 0)(1 0))                        ; 6  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 7  1x2 horizontal rectangle
    ((0 0)(0 1)(0 2))                   ; 8  1x3 horizontal rectangle
    ((0 0))))                           ; 9  1x1 unit square


(defun hole-in-one-init ()
  (slide-init (climb24-template)
              (climb24-piece-types)
              (climb24-start-list))
  (set-climb24-target))

(defun hole-in-one-start-list ()
  '((0 8 3)    ; T piece
    (1 4 5)    ; up-left L
    (2 3 3)    ; up-right L
    (2 4 0)    ; up-right L
    (3 6 1)    ; down-right L
    (4 3 2)    ; down-left L
    (4 6 5)    ; down-left L
    (5 2 0)    ; 2x2
    (5 2 5)    ; 2x2
    (5 8 0)    ; 2x2
    (5 8 5)    ; 2x2
    (6 5 0)    ; 2x1 (vertical)
    (6 5 2)    ; 2x1 (vertical)
    (6 5 4)    ; 2x1 (vertical)
    (6 5 6)    ; 2x1 (vertical)
    (7 1 0)    ; 1x2 (horizontal)
    (7 1 5)    ; 1x2 (horizontal)
    (8 2 2)    ; 1x3 (horizontal)
    (8 7 2)    ; 1x3 (horizontal)
    (9 5 3)    ; 1x1
    (9 6 3)    ; 1x1
    (9 8 2)    ; 1x1
    (9 8 4)    ; 1x1
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slide-T Puzzles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slide-t-template ()
  (obstacle-template 5 6 '((1 0))))

(defun set-slide-t-target ()
  (double-target-for-distinct-pieces-from-row-cols 4 3 0 0 3 1))

(defun slide-t-piece-types ()
  '(((0 0)(0 1)(1 1)(0 2))              ; 0  4 square T (stem down)
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ((0 0)(0 1)(1 1))                   ; 2  Upper Right pointing L
    ((0 0)(1 -1)(1 0))                  ; 3  Lower Right pointing L
    ((0 0)(1 0)(1 1))                   ; 4  Lower Left pointing L
    ((0 0)(1 0))                        ; 5  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 6  1x2 horizontal rectangle
    ((0 0))))                           ; 7  1x1 unit square

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slide T (original)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hordern Puzzle ??

(defun slide-t-init ()
  (slide-init (slide-t-template)
              (slide-t-piece-types)
              (slide-t-start-list))
  (set-slide-t-target))

(defun slide-t-start-list ()
  '((0 0 0)
    (2 0 4)
    (3 3 5)
    (4 3 0)
    (5 2 1)
    (5 2 4)
    (6 2 2)
    (6 3 2)
    (6 4 2)
    (7 2 0)
    (7 2 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; T-Zer by Nob (sent to Scott Kim 8-28-98)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________          ___________ 
;;  |x| |xxx| |x|        |x| |xxx| |x|
;;  |_____|_____|        |_____|_____|
;;  |xxxxxxxxxxx|        |xxxxxxxxxxx|
;;  |___|___|___|   -->  |___|___|___|
;;  |_|_|___|_|_|        |_|_|___|_|_|
;;
;;   "T-zer" by Nob Yoshigahara
;;
;;   X  Y
;;  XXXYYY
;;  ------
;;  AABBCC
;;  mnDDop


(defun tzer-init ()
  (slide-init (tzer-board-template)
              (tzer-basic-piece-types)
              (tzer-start-list))
  (setf *solution-target*
        (tzer-target)))

(defun tzer-board-template (&optional obstacles)
  (let ((template (make-array '(5 6) :initial-element nil)))
    (loop for obstacle in obstacles
          do
          (setf (aref template (first obstacle)(second obstacle))
                "X"))
    template))


(defun tzer-basic-piece-types ()
  '(((0 0)(1 -1)(1 0)(1 1))        ; 0  X piece (T tetromino)
    ((0 0)(1 -1)(1 0)(1 1))        ; 1  Y piece (other T tetromino)
    ((0 0)(0 1))                   ; 2  A piece (1x2 horizontal rectangle)
    ((0 0))))                      ; 3  m piece (1x1 unit square)

(defun tzer-piece-types ()
  '(((0 0)(1 -1)(1 0)(1 1))        ; 0  X piece (T tetromino)
    ((0 0)(1 -1)(1 0)(1 1))        ; 1  Y piece (other T tetromino)
    ((0 0)(0 1))                   ; 2  A piece (1x2 horizontal rectangle)
    ((0 0))                        ; 3  m piece (1x1 unit square)
    ((0 0)(0 1))                   ; 4  B piece (1x2 horizontal rectangle)
    ((0 0)(0 1))                   ; 5  C piece (1x2 horizontal rectangle)
    ((0 0)(0 1))                   ; 6  D piece (1x2 horizontal rectangle)
    ((0 0))                        ; 7  n piece (1x1 unit square)
    ((0 0))                        ; 8  o piece (1x1 unit square)
    ((0 0))))                      ; 9  p piece (1x1 unit square)


(defun tzer-target ()
  (let ((compiled-target-pos 
         (compile-list-position
          (list-position-from-start-positions-list 
           (tzer-target-placement-list)))))
    (loop for piece-type from 0 below *num-piece-types*
          collect
          (list piece-type (aref compiled-target-pos piece-type)))))

(defun tzer-start-list ()
  '((0 0 1)
    (1 0 4)
    (2 3 0)
    (2 3 2)
    (2 3 4)
    (2 4 2)
    (3 4 0)
    (3 4 1)
    (3 4 4)
    (3 4 5)))

(defun tzer-target-placement-list ()
  '((0 0 4)
    (1 0 1)
    (2 3 0)
    (2 3 2)
    (2 3 4)
    (2 4 2)
    (3 4 0)
    (3 4 1)
    (3 4 4)
    (3 4 5)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Barbell Puzzle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  cf. http://csm.astate.edu/~wpaulsen/slider/slider.html

(defun barbell-template ()
  (make-array '(5 4) :initial-element nil))

(defun set-barbell-target ()
  (single-target-from-row-col 2 0 3))


(defun barbell-piece-types ()
  '(((0 0)(-1 1))			; 0  Upper right pointing barbell
    ((0 0)(1 1))			; 1  Lower  right pointing barbell
    ((0 0)(1 0))                        ; 2  2x1 vertical rectangle
    ((0 0))))                           ; 3  1x1 unit square


(defun barbell-init ()
  (slide-init (barbell-template)
              (barbell-piece-types)
              (barbell-start-list))
  (set-barbell-target))

(defun barbell-start-list ()
  '((0 1 0)
    (0 4 2)
    (1 1 2)
    (2 3 0)
    (3 0 0)
    (3 0 2)
    (3 0 3)
    (3 1 1)
    (3 1 3)
    (3 2 0)
    (3 3 1)
    (3 3 2)
    (3 4 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Harold Cataquet 31-move L puzzle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun L-puzzle-init ()
  (slide-init (L-puzzle-template)
              (L-puzzle-piece-types)
              (L-puzzle-start-list))
  (setf *solution-target*
	(L-puzzle-exact-target)))

(defun L-puzzle-template ()
  (obstacle-template 5 9))

(defun L-puzzle-piece-types ()  ; All pieces are 4-cell L's in various orientations
  '(((0 0)(1 0)(2 0)(2 1))      ;   0  Verticle L (point is down-left)
    ((0 0)(0 1)(1 1)(2 1))      ;   1  Verticle L (point is up-right)
    ((0 0)(1 0)(1 1)(1 2))      ;   2  Horizontal L (point is down-left)
    ((0 0)(0 1)(0 2)(-1 2))     ;   3  Horizontal L (point is down-right)
    ((0 0)(0 1)(0 2)(1 0))      ;   4  Horizontal L (point is up-left)
    ((0 0)(0 1)(0 2)(1 2))))              ;   5  Horizontal L (point is up-right)

(defun L-puzzle-start-list ()
  '((0 1 0)
    (1 2 2)
    (2 1 4)
    (2 3 4)
    (3 4 0)
    (4 1 1)
    (5 1 5)
    (5 3 5)))

(defun L-puzzle-exact-target ()
  (let ((compiled-target-pos 
         (compile-list-position
          (list-position-from-start-positions-list 
           (L-puzzle-exact-target-placement-list)))))
    (loop for piece-type from 0 below *num-piece-types*
          collect
          (list piece-type (aref compiled-target-pos piece-type)))))

(defun L-puzzle-exact-target-placement-list ()
  (L-puzzle-exact-target-placement-list-1)
;  (L-puzzle-exact-target-placement-list-2)
  )
  
; Solvable in 31 moves (the best!)
(defun L-puzzle-exact-target-placement-list-1 ()
  '((0 2 0)
    (1 1 0)
    (2 3 2)
    (2 2 3)
    (3 4 5)
    (4 1 2)
    (5 1 5)
    (5 2 4)))

; Solvable in 51 moves (non-optimal for puzzle spec)
(defun L-puzzle-exact-target-placement-list-2 ()
  '((0 2 0)
    (1 1 6)
    (2 2 1)
    (2 3 5)
    (3 4 2)
    (4 1 3)
    (5 1 0)
    (5 2 4)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rush Hour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___________
;;  |_|_|_|_|_|_|
;;  |_|_|_|_|_|_|
;;  |_|_|_|_|_|_| <-- EXIT
;;  |_|_|_|_|_|_|
;;  |_|_|_|_|_|_|
;;  |_|_|_|_|_|_|
;;


(defun rush-hour-init (level)
  (setf *rush-hour* t)
  (slide-init (make-array '(6 6) :initial-element nil)
              (rush-hour-piece-types)
              (rush-hour-start-list level))
  (set-rush-hour-target))

(defun set-rush-hour-target ()
  (single-target-from-row-col 0 2 4))

(defun rush-hour-piece-types ()
  '(((0 0)(0 1))                        ; 0  1x2 horizontal Red Car
    ((0 0)(0 1))                        ; 1  1x2 horizontal Non-Red Car
    ((0 0)(0 1)(0 2))                   ; 2  1x3 horizontal Truck
    ((0 0)(1 0))                        ; 3  2x1 vertical Non-red Car
    ((0 0)(1 0)(2 0))))                 ; 4  3x1 vertical Truck

(defun rush-hour-start-list (level)
  (case level
    (1 (rush-hour-1-start-list))
    (2 (rush-hour-2-start-list))
    (3 (rush-hour-3-start-list))
    (4 (rush-hour-4-start-list))
    (5 (rush-hour-5-start-list))
    (6 (rush-hour-6-start-list))
    (7 (rush-hour-7-start-list))
    (8 (rush-hour-8-start-list))
    (9 (rush-hour-9-start-list))
    (10 (rush-hour-10-start-list))
    (11 (rush-hour-11-start-list))
    (12 (rush-hour-12-start-list))
    (13 (rush-hour-13-start-list))
    (14 (rush-hour-14-start-list))
    (15 (rush-hour-15-start-list))
    (16 (rush-hour-16-start-list))
    (17 (rush-hour-17-start-list))
    (18 (rush-hour-18-start-list))
    (19 (rush-hour-19-start-list))
    (20 (rush-hour-20-start-list))
    (21 (rush-hour-21-start-list))
    (22 (rush-hour-22-start-list))
    (23 (rush-hour-23-start-list))
    (24 (rush-hour-24-start-list))
    (25 (rush-hour-25-start-list))
    (26 (rush-hour-26-start-list))
    (27 (rush-hour-27-start-list))
    (28 (rush-hour-28-start-list))
    (29 (rush-hour-29-start-list))
    (30 (rush-hour-30-start-list))
    (31 (rush-hour-31-start-list))
    (32 (rush-hour-32-start-list))
    (33 (rush-hour-33-start-list))
    (34 (rush-hour-34-start-list))
    (35 (rush-hour-35-start-list))
    (36 (rush-hour-36-start-list))
    (37 (rush-hour-37-start-list))
    (38 (rush-hour-38-start-list))
    (39 (rush-hour-39-start-list))
    (40 (rush-hour-40-start-list))))





(defun rush-hour-1-start-list ()
  '((0 2 1)
    (1 0 0)
    (1 4 4)
    (2 5 2)
    (3 4 0)
    (4 0 5)
    (4 1 0)
    (4 1 3)))

(defun rush-hour-2-start-list ()
  '((0 2 0)
    (1 4 4)
    (1 5 0)
    (1 5 3)
    (2 0 3)
    (2 3 0)
    (3 0 0)
    (3 1 3)
    (3 2 4)
    (3 4 2)
    (4 1 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add new Rush Hour start lists:

(defun rush-hour-3-start-list ()
  nil)

(defun rush-hour-4-start-list ()
  nil)

(defun rush-hour-5-start-list ()
  nil)

(defun rush-hour-6-start-list ()
  nil)

(defun rush-hour-7-start-list ()
  nil)

(defun rush-hour-8-start-list ()
  nil)

(defun rush-hour-9-start-list ()
  nil)

(defun rush-hour-10-start-list ()
  nil)

(defun rush-hour-11-start-list ()
  nil)

(defun rush-hour-12-start-list ()
  nil)

(defun rush-hour-13-start-list ()
  nil)

(defun rush-hour-14-start-list ()
  nil)

(defun rush-hour-15-start-list ()
  nil)

(defun rush-hour-16-start-list ()
  nil)

(defun rush-hour-17-start-list ()
  nil)

(defun rush-hour-18-start-list ()
  nil)

(defun rush-hour-19-start-list ()
  nil)

(defun rush-hour-20-start-list ()
  nil)

(defun rush-hour-21-start-list ()
  nil)

(defun rush-hour-22-start-list ()
  nil)

(defun rush-hour-23-start-list ()
  nil)

(defun rush-hour-24-start-list ()
  nil)

(defun rush-hour-25-start-list ()
  nil)

(defun rush-hour-26-start-list ()
  nil)

(defun rush-hour-27-start-list ()
  nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rush-hour-28-start-list ()
  '((0 2 0)
    (1 1 4)
    (1 3 3)
    (1 5 0)
    (1 5 2)
    (2 0 0)
    (2 4 2)
    (3 0 3)
    (3 3 0)
    (3 3 1)
    (4 1 2)
    (4 3 5)))

(defun rush-hour-29-start-list ()
  '((0 2 0)
    (1 3 1)
    (1 3 3)
    (1 4 1)
    (2 0 0)
    (2 5 0)
    (3 1 2)
    (3 2 5)
    (3 3 0)
    (3 4 3)
    (3 4 5)
    (4 0 4)))

(defun rush-hour-30-start-list ()
  '((0 2 1)
    (1 3 0)
    (1 3 2)
    (1 5 0)
    (1 5 2)
    (2 0 3)
    (3 0 2)
    (3 1 3)
    (4 0 0)
    (4 3 5)))

(defun rush-hour-31-start-list ()
  '((0 2 1)
    (1 0 0)
    (1 1 4)
    (1 3 3)
    (1 4 0)
    (2 0 3)
    (2 5 3)
    (3 1 3)
    (3 2 0)
    (4 3 2)
    (4 2 5)))

(defun rush-hour-32-start-list ()
  '((0 2 0)
    (1 0 0)
    (1 0 4)
    (1 3 1)
    (1 3 3)
    (1 5 0)
    (3 0 3)
    (3 3 0)
    (3 4 3)
    (4 0 2)
    (4 3 5)))

(defun rush-hour-33-start-list ()
  '((0 2 0)
    (1 0 4)
    (1 3 1)
    (1 3 3)
    (1 4 1)
    (2 5 0)
    (3 0 1)
    (3 3 0)
    (3 4 3)
    (3 4 4)
    (4 0 2)
    (4 3 5)))

(defun rush-hour-34-start-list ()
  '((0 2 0)
    (1 4 4)
    (1 5 0)
    (1 5 3)
    (2 0 3)
    (2 3 0)
    (3 0 0)
    (3 1 3)
    (3 2 4)
    (3 3 3)
    (3 4 2)
    (4 1 5)))

(defun rush-hour-35-start-list ()
  '((0 2 0)
    (1 0 3)
    (1 4 1)
    (1 5 0)
    (2 3 1)
    (3 1 3)
    (3 3 0)
    (3 4 3)
    (3 4 4)
    (4 0 2)
    (4 0 5)))


(defun rush-hour-36-start-list ()
  '((0 2 2)
    (1 0 4)
    (1 1 2)
    (1 4 4)
    (1 5 0)
    (2 0 1)
    (2 3 0)
    (3 1 1)
    (3 3 3)
    (3 4 2)
    (4 0 0)
    (4 1 5)))

(defun rush-hour-37-start-list ()
  '((0 2 1)
    (1 0 0)
    (1 0 4)
    (1 1 0)
    (1 4 4)
    (1 5 4)
    (1 5 0)
    (2 3 1)
    (3 0 2)
    (3 4 3)
    (4 1 4)
    (4 1 5)
    (4 2 0)))

(defun rush-hour-38-start-list ()
  '((0 2 0)
    (1 1 1)
    (1 3 3)
    (1 4 3)
    (2 0 3)
    (2 5 3)
    (3 0 0)
    (3 1 3)
    (3 2 2)
    (3 4 2)
    (4 2 5)))

(defun rush-hour-39-start-list ()
  '((0 2 0)
    (1 3 0)
    (1 3 3)
    (1 4 2)
    (1 5 2)
    (2 0 3)
    (3 0 2)
    (3 1 3)
    (3 2 2)
    (3 4 0)
    (3 4 1)
    (4 2 5)))

(defun rush-hour-40-start-list ()
  '((0 2 3)
    (1 0 1)
    (1 4 4)
    (1 5 0)
    (1 5 3)
    (2 3 0)
    (3 0 4)
    (3 1 1)
    (3 1 2)
    (3 3 3)
    (3 4 2)
    (4 0 0)
    (4 1 5)))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extended Rush Hour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-mini-rush-hour-init (level)
  (setf *rush-hour* t)
  (slide-init (make-array '(6 6) :initial-element nil)
              (new-rush-hour-piece-types)
              (new-mini-rush-hour-start-list level))
  (set-rush-hour-target))

(defun new-mini-rush-hour-start-list (level)
  (case level
    (1 (new-mini-rush-hour-1-start-list))))

;;;;;;;;;;;;;;;
;; 6x6 Level 1
;;;;;;;;;;;;;;;
;
; a a a b X X
; . . c b X X
; R R c . . d  <-- exit
; . . e f f d
; g . e . h h
; g i i . . .

(defun new-mini-rush-hour-1-start-list ()
  '((0 2 0)
    (1 3 3)
    (1 4 4)
    (1 5 1)
    (2 0 0)
    (3 0 3)
    (3 1 2)
    (3 2 5)
    (3 3 2)
    (3 4 0)
    (5 0 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7x7 Extended Rush Hour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-rush-hour-init-by-level (level)
  (setf *rush-hour-level* level)
  (new-rush-hour-init (new-rush-hour-start-list level)))

(defun new-rush-hour-init (exit-row-and-start-list)
  (setf *rush-hour* t)
  (let* ((exit-row (1- (first exit-row-and-start-list)))        ; change to 0 indexing
         (level-start-list (change-to-0-indexing
                            (cdr exit-row-and-start-list)))
         (target-piece-type-index (first (first level-start-list)))
         (original-piece-types (new-rush-hour-piece-types))
         (all-piece-types (cons (nth target-piece-type-index    ; puts target type first
                                     original-piece-types)
                                (cdr original-piece-types)))
         (needed-piece-type-indices (remove-duplicates 
                                     (cons 0
                                           (loop for triple in (cdr level-start-list)
                                                 collect
                                                 (first triple)))))
         (needed-piece-type-indices-sorted (sort needed-piece-type-indices #'<))
         (final-level-start-list
          (cons (cons 0 (cdr (first level-start-list)))         ; make target be 0
                (loop for triple in (cdr level-start-list)
                      collect
                      (cons (position (first triple) needed-piece-type-indices-sorted)
                            (cdr triple)))))
         (final-piece-types (loop for piece-type-index in needed-piece-type-indices-sorted
                                  collect (nth piece-type-index all-piece-types))))
    ;(print final-level-start-list)
    ;(print final-piece-types)
    (slide-init (make-array '(7 7) :initial-element nil)
                final-piece-types
                final-level-start-list)
    (setf *exit-row* exit-row)
    (set-new-rush-hour-target-from-piece-types final-piece-types exit-row)))

(defun set-new-rush-hour-target-from-piece-types (piece-types exit-row)
  ; Assumes the first piece is the spec for the target block
  (single-target-from-row-col 0 
                              exit-row 
                              (- 6 (max-column-coordinate (first piece-types)))))

(defun max-column-coordinate (piece-cells)
  (loop for row-col-pair in piece-cells
        maximize
        (second row-col-pair)))

(defun new-rush-hour-piece-types ()
  ;; note that the 0 piece type is obsolete, and should never be used
  ;;   it is replaced by the convention that the first piece-spec in the
  ;;   start-list of piece-specs will become the target and be re-labeled as type 0.
  '(((0 0)(0 1))                        ; 0  1x2 horizontal Red Car (default goal piece)
    ((0 0)(0 1))                        ; 1  1x2 horizontal Non-Red Car
    ((0 0)(0 1)(0 2))                   ; 2  1x3 horizontal Truck
    ((0 0)(1 0))                        ; 3  2x1 vertical Non-red Car
    ((0 0)(1 0)(2 0))                   ; 4  3x1 vertical Truck
    ((0 0)(1 0)(0 1)(1 1))))            ; 5  2x2 4-way moving Raft

(defun new-rush-hour-start-list (level)
  (case level
    (1 (new-rush-hour-1-start-list))
    (2 (new-rush-hour-2-start-list))
    (3 (new-rush-hour-3-start-list))
    (4 (new-rush-hour-4-start-list))
    (5 (new-rush-hour-5-start-list))
    (6 (new-rush-hour-6-start-list))    ; test different target
    (7 (new-rush-hour-7-start-list))    ; test different target
    (8 (new-rush-hour-8-start-list))
    (9 (new-rush-hour-9-start-list))
    (10 (new-rush-hour-10-start-list))
    (11 (new-rush-hour-11-start-list))
    (12 (new-rush-hour-12-start-list))
    (13 (new-rush-hour-13-start-list))
    (14 (new-rush-hour-14-start-list))
    (15 (new-rush-hour-15-start-list))
    (16 (new-rush-hour-16-start-list))
    (17 (new-rush-hour-17-start-list))
    (18 (new-rush-hour-18-start-list))
    (19 (new-rush-hour-19-start-list))
    (20 (new-rush-hour-20-start-list))
    (21 (new-rush-hour-21-start-list))
    (22 (new-rush-hour-22-start-list))
    (23 (new-rush-hour-23-start-list))
    (24 (new-rush-hour-24-start-list))
    (25 (new-rush-hour-25-start-list))
    (26 (new-rush-hour-26-start-list))
    (27 (new-rush-hour-27-start-list))
    (28 (new-rush-hour-28-start-list))
    (29 (new-rush-hour-29-start-list))
    (30 (new-rush-hour-30-start-list))
    (t (warn "Undefined level = ~s" level))))

(defun change-to-0-indexing (piece-start-list)
  (shift-row-col-indices piece-start-list -1))

(defun shift-row-col-indices (piece-start-list index-delta)
  (loop for triple in piece-start-list
        collect
        (list (first triple)
              (+ index-delta (second triple))
              (+ index-delta (third triple)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Saving New Rush Hour positions to file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-position-to-file (position)
  (with-open-file (ofile "ccl:lisp-to-hypercard-position"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (loop for piece-encoding in (encode-position position)
          do
          (format ofile "~s" piece-encoding))))

(defun encode-position (position)
  (loop for piece-type from 0 below *num-piece-types*
        for piece-bits = (aref position piece-type)
        append
        (loop for cell from 0 below (integer-length piece-bits)
              when (logbitp cell piece-bits)
              collect
              (encode-piece piece-type cell))))

(defun encode-piece (type cell 
                          &optional
                          (row-col-order? nil)          ; default to col-row-order
                          (index-delta 1))
  (let* ((original-piece-types (cdr (new-rush-hour-piece-types)))
         (original-piece-type (1+ (position (nth type *piece-types*)
                                            original-piece-types
                                            :test #'equal)))
         (row-col-coord-pair (aref *coords-from-cell* cell)))
    (list original-piece-type
          (+ index-delta
             (if row-col-order?
               (first row-col-coord-pair)
               (second row-col-coord-pair)))
          (+ index-delta
             (if row-col-order?
               (second row-col-coord-pair)
               (first row-col-coord-pair))))))

;;  This just swaps row and column indices
;;  Recall that file-position format is:   ( exit-row  (piece-type-1 col-1 row-1) .... (piece-type-n col-n row-n) )
(defun decode-file-position (file-position)
  (cons (first file-position)
        (loop for triple in (cdr file-position)
              collect
              (list (first triple) (third triple) (second triple)))))
                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; New Rush Hour 1 - level 1 (22 moves, 43 single steps)
;       by Glenn A.Iba (March 21, 1998)
;    
; . a . b b c .
; . a d e f c .
; R R d e f X X
; g . d h h X X
; g i i j k . l
; . . . j k . l
; m m . j k n n

(defun new-rush-hour-1-start-list ()
  (cons 3                               ; exit row 2 in 0-based indexing
        (shift-row-col-indices
         '((1 2 0)
           (1 0 3)
           (1 3 3)
           (1 4 1)
           (1 6 0)
           (1 6 5)
           (3 0 1)
           (3 0 5)
           (3 1 3)
           (3 1 4)
           (3 3 0)
           (3 4 6)
           (4 1 2)
           (4 4 3)
           (4 4 4)
           (5 2 5))
         1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 2  (17 moves, 28 single steps)
;       by Glenn A.Iba (March 21, 1998)
;
; a b b b c d d
; a . X X c . .
; R R X X c e .
; . f g g g e .
; h f . X X i i
; h j j X X . k
; h . . l l l k

(defun new-rush-hour-2-start-list ()
  (cons 3                               ; exit row 2 in 0-based indexing
        (shift-row-col-indices
         '((1 2 0)
           (1 0 5)
           (1 4 5)
           (1 5 1)
           (2 0 1)
           (2 3 2)
           (2 6 3)
           (3 0 0)
           (3 2 5)
           (3 3 1)
           (3 5 6)
           (4 0 4)
           (4 4 0)
           (5 1 2)
           (5 4 3))
         1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 3  (19 piece moves, 48 single steps)
;       by Scott Kim
;
; . . . a . . b
; c . . a . . b
; c R R X X . b
; d . . X X e e
; d f f f g . .
; h . i . g j j
; h . i . g . .

(defun new-rush-hour-3-start-list ()
  (cons 3                               ; exit row 2 in 0-based indexing
        (shift-row-col-indices
         '((1 2 1)
           (1 3 5)
           (1 5 5)
           (2 4 1)
           (3 0 3)
           (3 1 0)
           (3 3 0)
           (3 5 0)
           (3 5 2)
           (4 0 6)
           (4 4 4)
           (5 2 3))
         1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 4  (17 piece moves, 33 single-steps)
;       by Scott Kim
;
; . . a a b b b
; . c c c X X d
; R R X X X X d
; . e X X f . d
; . e g g f . .
; . h h h f . .
; . . . . . . .

(defun new-rush-hour-4-start-list ()
  (cons 3                               ; exit row 2 in 0-based indexing
        (shift-row-col-indices
         '((1 2 0)
           (1 0 2)
           (1 4 2)
           (2 0 4)
           (2 1 1)
           (2 5 1)
           (3 3 1)
           (4 1 6)
           (4 3 4)
           (5 1 4)
           (5 2 2))
         1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 5  (22 piece moves, 44 single-steps)
;       by Scott Kim
;
; . . . . . . .
; . a a b b b c
; R R X X X X c
; . d X X X X c
; . d e e f f f
; . . g h h h .
; . . g . . . .

(defun new-rush-hour-5-start-list ()
  (cons 3                               ; exit row 2 in 0-based indexing
        (shift-row-col-indices
         '((1 2 0)
           (1 1 1)
           (1 4 2)
           (2 1 3)
           (2 4 4)
           (2 5 3)
           (3 3 1)
           (3 5 2)
           (4 1 6)
           (5 2 2)
           (5 2 4))
         1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Same start-position as level 5, but with target being 2x2 block at 2,4
(defun new-rush-hour-6-start-list ()
  (cons 3                               ; exit row 2 in 0-based indexing
        (shift-row-col-indices
         '((5 2 4)                             ; 2x2 is target piece
           (1 2 0)                             ;   also piece order is mixed up to test init
           (4 1 6)
           (2 1 3)
           (1 1 1)
           (1 4 2)
           (2 4 4)
           (2 5 3)
           (3 3 1)
           (3 5 2)
           (5 2 2)
           )
         1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Same start-position as level 5, but with target being 2x2 block at 2,2
(defun new-rush-hour-7-start-list ()
  (cons 3                               ; exit row 2 in 0-based indexing
        (shift-row-col-indices
         '((5 2 2)                             ; 2x2 is target piece
           (1 2 0)                             ;   also piece order is mixed up to test init
           (4 1 6)
           (2 1 3)
           (1 1 1)
           (1 4 2)
           (2 4 4)
           (2 5 3)
           (3 3 1)
           (3 5 2)
           (5 2 4)
           )
         1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 8  (? piece moves, 49 single-steps)
;       by Glenn Iba  4-8-98
;
; X X . . a a a    X piece is the target
; X X b c c . d     
; . . b e e . d   <<  exits open at
; f f b . g g h   <<  rows 3 and 4
; i i i . . . h
; . . j k . l .
; . . j k . l .

(defun new-rush-hour-8-start-list ()
  '(3 
    (5 1 1)
    (2 1 5)
    (4 2 3)
    (1 2 4)
    (3 2 7)
    (1 3 4)
    (1 4 1)
    (1 4 5)
    (3 4 7)
    (2 5 1)
    (3 6 3)
    (3 6 4)
    (3 6 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;;; New Rush Hour - level 9  (43 piece moves, 78 single-steps)
;       by Glenn Iba  4-9-98
;
; X X a a a . .    X piece is the target
; X X b c c d d     
; . . b e e . f   <<  exits open at
; g g b . h h f   <<  rows 3 and 4
; i i i . . . j
; . . k l . m j
; n n k l . m .

(defun new-rush-hour-9-start-list ()
  '(3 
    (5 1 1)
    (2 1 3)
    (4 2 3)
    (1 2 4)
    (1 2 6)
    (1 7 1)
    (3 3 7)
    (1 3 4)
    (1 4 1)
    (1 4 5)
    (3 5 7)
    (2 5 1)
    (3 6 3)
    (3 6 4)
    (3 6 6)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 10  (77 piece moves, 135 single-steps)
;       by Glenn Iba  4-19-98
;      (this is an "optimized" version of Level 9)
;
; . . a b b b .    X piece is the target
; . . a c c d d     
; . . a e e . f   <<  exits open at
; . g g h i i f   <<  rows 3 and 4
; j j j h . k .
; . . l X X k m
; n n l X X . m


(defun new-rush-hour-10-start-list ()
  (decode-file-position
   '(3 (5 4 6)(1 4 2)(1 6 2)(1 4 3)(1 2 4)(1 5 4)(1 1 7)(2 4 1)(2 1 5)
     (3 7 3)(3 4 4)(3 6 5)(3 3 6)(3 7 6)(4 3 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 11  (40 piece moves, 58 single-steps)
;       by Glenn Iba  4-19-98
;
; . . a . b . .    X piece is the target
; c . a . b d d     
; c e e f f g h 
; i X X f f g h   <<  Exit at row 4
; i j j k . l l
; i . . k . . m
; n n n o o o m


(defun new-rush-hour-11-start-list ()
  (decode-file-position 
   '(4 (1 2 4)(1 6 2)(1 2 3)(1 2 5)(1 6 5)(2 1 7)(2 4 7)(3 3 1)(3 5 1)
    (3 1 2)(3 6 3)(3 7 3)(3 4 5)(3 7 6)(4 1 4)(5 4 3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 12 (42 piece moves, 62 single-steps)
;       by Glenn Iba  4-19-98
;     (optimized from level 11)
;
;  . . 3 . 3 . .     0 is the target piece
;  3 . 3 . 3 1 1
;  3 . 1 1 5 5 3
;  4 . 0 0 5 5 3   << exit at row 4
;  4 1 1 1 1 3 3
;  4 . . 3 . 3 3
;  2 2 2 3 2 2 2

(defun new-rush-hour-12-start-list ()
  (decode-file-position 
   '(4 (1 3 4)(1 6 2)(1 3 3)(1 2 5)(1 4 5)(2 1 7)(2 5 7)
     (3 3 1)(3 5 1)(3 1 2)(3 7 3)(3 6 5)(3 7 5)(3 4 6)(4 1 4)(5 5 3))))


;;;; New Rush Hour - level 13 (33 piece moves, 52 single-steps)
;       by Glenn Iba  4-19-98
;     (verified to be optimized - maximal distance from solved)
;
;  4 1 1 1 1 3 .     0 is the target piece
;  4 3 5 5 3 3 .
;  4 3 5 5 3 1 1
;  4 0 0 3 4 . .   << exit at row 4
;  4 . . 3 4 . 4
;  4 . . . 4 . 4
;  1 1 1 1 1 1 4

(defun new-rush-hour-13-start-list ()
  (decode-file-position 
   '(4 (1 2 4)(1 2 1)(1 4 1)(1 6 3)(1 1 7)(1 3 7)(1 5 7)
     (3 6 1)(3 2 2)(3 5 2)(3 4 4)(4 1 1)(4 1 4)(4 5 4)(4 7 5)(5 3 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 14 (28 piece moves, ?? single-steps)
;       by Glenn Iba  4-22-98
;     (optimized version of level 1 - maximal distance from solved)
;
;  . 2 3 2 2 1 1
;  . 2 3 2 2 2 .
;  0 0 3 3 3 2 .   << exit at row 3
;  2 1 1 3 3 4 4
;  2 1 1 3 3 4 4
;  . . . . . . 2
;  . . 1 1 1 1 2

(defun new-rush-hour-14-start-list ()
  (decode-file-position
   '(3 
     (1 1 3)(1 6 1)(1 2 4)(1 2 5)(1 3 7)(1 5 7)
     (3 2 1)(3 4 1)(3 5 1)(3 6 2)(3 1 4)(3 7 6)
     (4 3 1)(4 4 3)(4 5 3)
     (5 6 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 15 (?? piece moves, ?? single-steps)
;       by Glenn Iba  4-24-98
;
;  2 2 2 . . . 3
;  2 2 2 . . . 3
;  . . 4 1 1 5 5
;  . . 4 0 0 5 5  << exit row 4
;  1 1 4 3 3 3 3
;  3 3 3 3 3 3 3
;  3 3 3 . . . .

(defun new-rush-hour-15-start-list ()
  (decode-file-position
   '(4 
     (1 4 4)(1 4 3)(1 1 5)
     (2 1 1)(2 1 2)
     (3 7 1)(3 4 5)(3 5 5)(3 6 5)(3 7 5)(3 1 6)(3 2 6)(3 3 6)
     (4 3 3)
     (5 6 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 16 (96 piece moves, ?? single-steps)
;       optimized from a position by Serhiy (his level 4) (April 98)
;
;  . 3 . . . . 4
;  3 3 2 2 2 . 4
;  3 0 0 3 4 . 4
;  3 5 5 3 4 . .
;  3 5 5 . 4 1 1
;  5 5 2 2 2 . .
;  5 5 . . . . .

(defun new-rush-hour-16-start-list ()
  '(3 (1 3 2) (3 2 1) (3 1 2) (2 2 3) (2 6 3) (5 6 1) (3 3 4) (3 4 1) (4 1 7) (4 3 5) (1 5 6) (5 4 2) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 17 (26 piece moves, ?? single-steps)
;       by Glenn Iba (4-28-98)
;
;  . 1 1 2 2 1 1
;  . . . 2 2 . .
;  1 1 2 3 3 3 3
;  0 0 2 3 3 3 3
;  1 1 2 3 3 3 3
;  2 . 2 1 1 4 4
;  2 1 1 1 1 4 4

(defun new-rush-hour-17-start-list ()
  '(4 (1 4 1) (1 1 6) (3 3 3) (4 3 7) (3 1 4) (1 1 2) (3 1 5) (3 6 1) (3 5 3) (4 3 4) 
    (1 5 1) (1 7 2) (4 3 5) (4 3 6) (1 6 4) (1 7 4) (1 3 1) (5 6 6) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 18  (37 piece moves, ?? single-steps)
;       by Glenn Iba  (4-28-98)
;
;  . . 2 1 1 1 1
;  . . 2 2 . . .
;  1 1 2 2 2 3 3
;  0 0 2 3 2 3 3
;  2 1 1 3 3 3 3
;  2 1 1 3 3 4 4
;  1 1 1 1 3 4 4

(defun new-rush-hour-18-start-list ()
  (decode-file-position
   '(4
     (1 1 4)(1 4 1)(1 6 1)(1 1 3)(1 2 5)(1 2 6)(1 1 7)(1 3 7)(3 3 1)(3 4 2)(3 3 3)(3 5 3)(3 1 5)
     (4 6 3)(4 7 3)(4 4 4)(4 5 5)(5 6 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 19  ( ?? piece moves, ?? single-steps)
;       by Glenn Iba  (4-28-98)
;
;  3 . . 5 5 . .
;  3 . 3 5 5 1 1
;  3 . 3 4 . . .
;  3 0 0 4 3 . .
;  . . . 4 3 1 1
;  1 1 . 5 5 3 3
;  2 2 2 5 5 3 3

(defun new-rush-hour-19-start-list ()
  '(4 (1 4 2) (3 1 1) (3 3 1) (2 7 1) (5 1 4) (3 6 7) (3 4 5) (3 6 6) (3 2 3) 
    (4 3 4) (1 5 6) (1 2 6) (1 6 1) (5 6 4) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 20 ( ?? piece moves, ?? single-steps)
;       optimized from a position by Serhiy (his level 2) (April 98)
;

(defun new-rush-hour-20-start-list ()
  '(3 (1 3 5) (3 1 6) (3 3 3) (2 2 3) (5 4 1) (3 3 4) (1 2 1) (3 6 5) (4 3 7)
    (1 5 4) (1 7 6) (4 5 3) (1 6 6) (1 4 5) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 21  ( 32 piece moves, ?? single-steps)
;       by Glenn Iba  (4-29-98)
;     Confirmed to be already optimized (maximum distance from solved)
;
;  . 3 . 1 1 1 1
;  4 3 . . 5 5 3
;  4 0 0 3 5 5 3
;  4 . . 3 3 1 1
;  4 3 . 3 3 . 3
;  4 3 . 3 1 1 3
;  4 2 2 2 2 2 2

(defun new-rush-hour-21-start-list ()
  '(3 (1 3 2) (3 1 2) (3 3 4) (2 7 2) (2 7 5) (5 2 5) (3 4 5) (1 1 6) (3 5 7) (3 5 4) (3 5 2)
    (4 2 1) (1 6 5) (4 5 1) (3 2 7) (1 4 6) (1 1 4) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 22  ( 32 piece moves, ?? single-steps)
;       by Glenn Iba  (4-29-98)
;   (program proved its already optimized)
;
; 1 1 4 . 2 2 2
; . 3 4 1 1 3 .
; 3 3 4 4 4 3 .
; 3 0 0 4 4 5 5
; 1 1 3 4 4 5 5
; . . 3 3 . . 3
; . 1 1 3 . . 3

(defun new-rush-hour-22-start-list ()
  '(4 (1 4 2) (3 3 1) (3 2 2) (2 1 5) (5 4 6) (3 2 6) (1 2 4) (3 6 7) (3 6 4) (3 5 3)
    (4 1 3) (1 5 1) (4 3 4) (4 3 5) (1 7 2) (1 1 1) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 23  ( ?? piece moves, ?? single-steps)
;       by Glenn Iba  (4-29-98)
;
; 1 1 3 . . 1 1
; . 3 3 1 1 3 .
; . 3 5 5 . 3 .
; 0 0 5 5 4 5 5
; 3 1 1 . 4 5 5
; 3 . . 3 4 . 3
; 2 2 2 3 1 1 3

(defun new-rush-hour-23-start-list ()
  '(4 (1 4 1) (3 2 2) (3 1 3) (2 7 1) (5 3 3) (3 2 6) (1 1 1) (3 6 7) (3 5 1) (3 6 4)
    (4 4 5) (1 2 4) (1 7 5) (1 1 6) (1 5 2) (5 4 6) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 24  ( 28 piece moves, ?? single-steps)
;       by Glenn Iba  (4-29-98)
;    (shown to be already optimized)
;
; 3 . 3 . 2 2 2
; 3 . 3 4 4 1 1
; 3 1 1 4 4 3 3
; 3 . 3 0 0 3 3
; 1 1 3 4 4 1 1
; 3 . . 4 4 . .
; 3 2 2 2 . . .

(defun new-rush-hour-24-start-list ()
  '(4 (1 4 4) (3 1 1) (3 3 1) (2 1 5) (2 7 2) (5 2 4) (3 3 6) (1 5 1) (3 3 7) (3 4 3) (3 6 1) (3 1 3)
    (1 5 6) (1 2 6) (1 3 2) (5 5 4) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 25  ( ?? piece moves, ?? single-steps)
;       by Glenn Iba  (4-29-98)
;   (attempt to manually improve level 15 - by hand solution of 38 moves)
;
; 3 3 2 2 2 . 3
; 3 3 2 2 2 . 3
; . . 4 1 1 5 5
; 0 0 4 . . 5 5
; . . 4 3 3 1 1
; . . 3 3 3 3 3
; . . 3 . . 3 3

(defun new-rush-hour-25-start-list ()
  '(4 (1 4 1) (3 6 7) (1 3 4) (2 1 3) (2 2 3) (5 3 6) (3 5 5) (3 6 6) (3 5 4)
    (1 5 6) (4 3 3) (3 1 1) (3 1 2) (3 6 3) (3 1 7) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Rush Hour - level 26  ( ?? piece moves, ?? single-steps)
;       by Glenn Iba  (6-20-98)
;
; 3 . . 1 1 1 1
; 3 . . 4 5 5 3
; 0 0 . 4 5 5 3
; 3 . . 4 2 2 2
; 3 2 2 2 4 . .
; 3 3 5 5 4 3 .
; 3 3 5 5 4 3 .

(defun new-rush-hour-26-start-list ()
  '(3 (1 3 1) (1 1 4) (1 1 6) (2 4 5) (2 5 2) (3 1 1) (3 2 7) (3 4 1) (3 6 1)
    (3 6 2) (3 6 6) (4 2 4) (4 5 5) (5 2 5) (5 6 3) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Question: If piece is specified without covering its "origin" (0 0),
;;;     Will it still work with Obstacles??
;;; Answer:  NO - FAILS
;;;   See test code below (derived from climb-12)
;;; [see also, my comment regarding "Logical Bug" in the TODO comments at end of file]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NOTE: piece-type 2 does not include its "origin"
;;;    Calling climb12-init-test FAILS
;;;       [error: calling (EXPT 2 NIL) where exponent NIL should be a number, but isn't]

(defun climb12-piece-types-test ()
  '(((0 0)(1 -1)(1 0)(1 1))             ; 0  4 square T (stem up)
    ((0 0)(0 1)(1 0))                   ; 1  Upper Left pointing L
    ;((0 0)(1 -1)(1 0))                 ; 2  Lower Right pointing L
    ((0 1)(1 1)(1 0))                   ; 2  Lower Right pointing L [with (0 0) origin NOT covered]
    ((0 0)(1 0))                        ; 3  2x1 vertical rectangle
    ((0 0)(0 1))                        ; 4  1x2 horizontal rectangle
    ((0 0))))                           ; 5  1x1 unit square

;; Calling this generates an error, proving that pieces REALLY REQUIRE an EXPLICIT ORIGIN
(defun climb12-init-test ()
  (slide-init (climb12-template)
              (climb12-piece-types-test)
              (climb12-start-list-test))
  (set-climb12-target))

(defun climb12-start-list-test ()
  '((0 4 2)
    (1 2 1)
    (2 2 2)
    (3 1 0)
    (3 1 4)
    (4 4 0)
    (4 4 3)
    (5 3 0)
    (5 3 4)
    (5 5 0)
    (5 5 4)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTES and TODOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fix logical bug -- Obstacles in grid interacts with pieces whose
;;                     "location" is not an actual cell occupied by the piece.
;;                     In this case, move generation misses some moves,\
;;                     since it only loops over actual cells of template,
;;                     so may miss some starting positions of piece which
;;                     have the piece "reference location" situated at an obstacle.
;;        Simplest fix is to REQUIRE that the (0 0) "reference location"
;;                 ACTUALLY be a part of the piece (eg the first cell in first row).
;;        Did this for the CLIMB15 puzzle pieces ...
;;        The Donkey-2 case is OK since pieces are all convex (ie located by actual cell)

;;    How much memory is consumed by loading all these definitions at once?
;;       If significant, only load problem definition(s) actually needed.
