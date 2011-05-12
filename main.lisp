#+sbcl (defparameter +player-one-mark+ "X") ;; This is just a hack to silence SBCL's warnings
#-sbcl (defconstant +player-one-mark+ "X")
#+sbcl (defparameter +player-two-mark+ "O")
#-sbcl (defconstant +player-two-mark+ "O")
#+sbcl (defparameter +empty-mark+ ".")
#-sbcl (defconstant +empty-mark+ ".")

(defun play-vs-human ()    
  (welcome)
  (game-loop (make-board) #'human-move #'human-move))

(defun play-vs-AI (AI)
  (game-loop (make-board) #'human-move (gene-to-function AI)))

(defun game-loop (&optional
		  (board (make-board))
		  (player-one-move #'human-move)
		  (player-two-move #'human-move)
		  (verbose t))
  "The main game loop. Checks for winners, asks for moves, displays results, etc."
  (if (not-finished-p board)
      (progn
	(cycle board player-one-move player-two-move)
	(game-loop board player-one-move player-two-move verbose))
      (display-winner board verbose)))

(defun cycle (board player-one-move player-two-move)
  "Decides whose turn it is and asks for a move, marking the board."
  	(let ((active-player (get-player (get-turn board))))
	  (do ()
	      ((mark-board board
		      (get-mark active-player)
		      (get-move board active-player player-one-move player-two-move))))))

(defun welcome ()
  "Displays a welcome message and instructions."
  (format t "Welcome to Tic-Tac-Toe!~&Use the numbers 1-9 (and then enter) to make a move, like so:")
  (print-board (loop for i from 1 to 9 collecting i)))

(defun make-board ()
  "Returns an empty gameboard. It's guaranteed to be a 9-element sequence full of +empty-mark+s (at least as
long as the game is standard tic-tac-toe). Currently, this means it's an array (historically it was a list,
in the future it could be a hash table)."
  (make-array 9 :initial-element +empty-mark+))

(defun get-turn (board)
  "Analyzes the board and returns the mark of the player whose turn it is."
  (let ((p1-count 0)
	(p2-count 0))
    (loop for i from 0 to (1- (length board)) doing
	 (let ((mark (aref board i)))
	   (cond ((string-equal mark +player-one-mark+)
		  (incf p1-count))
		 ((string-equal mark +player-two-mark+)
		  (incf p2-count)))))
	 (if (< p2-count p1-count)
		+player-two-mark+
	       +player-one-mark+)))

(defun get-player (mark)
  "Returns the appropriate player symbol for the given mark (or nil if there's an error)."
  (cond
    ((string-equal mark +player-one-mark+)
     'player-one)
    ((string-equal mark +player-two-mark+)
     'player-two)
    (t (format t "Error in (get-player): ~a is a bad mark." mark))))

(defun get-mark (player)
  "Returns the appropriate mark for the given player symbol (or nil if there's an error)."
  (cond
    ((equal player 'player-one)
     +player-one-mark+)
    ((equal player 'player-two)
     +player-two-mark+)
    (t nil)))

(defun human-move (&optional board)
  "Asks the human for a move (also displays the board--which can be any sequence--if it's supplied)."
  (print-board board) ; If board isn't supplied (and is thus nil), print-board just won't print it
  (prompt-for-cell))

(defun get-move (board player player-one-move player-two-move)
  "Decides whose move it is and calls their appropriate move function. Returns the index of the board
that they'd like to mark."
  (if (equal player 'player-one)
      (1- (funcall player-one-move board))
      (1- (funcall player-two-move board))))

(defun prompt-read (prompt)
  "Reads/returns stdin after displaying the given prompt."
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-int (prompt fail-value)
  "Attempts to read an integer from stdin. If that doesn't work, returns the fail-value."
  (or (parse-integer (prompt-read prompt) :junk-allowed t) fail-value))

(defun prompt-for-cell ()
  "Asks for a move repeatedly until a valid cell is provided.
Returns the slot number, not the board's index (a number on [1, 9], not [0, 8])."
  (let ((value (read-int "Move" -1)))
    (cond ((= value -1)
	   (format t "Bad value; try again.~&")
	   (prompt-for-cell))
	  (t value))))

(defun print-board (board)
  "Displays the board."
  (fresh-line)
  (dotimes (i (length board))
    (when (= (mod i 3) 0)
	   (fresh-line))
    (format t "~a " (elt board i)))
  (fresh-line))

(defun not-finished-p (board)
  "Decides if the game is over yet. Returns true if it's not finished,
nil otherwise."
  (cond
    ((get-winner board) nil)
    ((> (length (remove-if-not #'empty-cell-p  board)) 0) t)
    ((get-winner board) nil)
    (t nil)))

(defun empty-cell-p (cell)
  "Determines if the given cell is empty. "
  (string-equal cell +empty-mark+))

(defun empty-index-p (index board)
  "Determines if the cell represented by the given index is empty."
  (if board
      (empty-cell-p (elt board index))))

(defun mark-board (board mark cell)
  "Marks the given cell with the given mark."
  (when (empty-index-p cell board)
	(setf (aref board cell) mark)
	board))

(defun to-string (player)
  "Returns a string representation of the given player symbol."
  (cond
    ((equal player 'player-one) "Player One")
    ((equal player 'player-two) "Player Two")
    (t "Nobody")))

(defun display-winner (board &optional (verbose t))
  "Displays the board and a congratulatory message to the winner. Also returns
a number indicating who won--1 for the first player, 2 for the second player, nil for a tie.
When the optional `verbose' isn't true, there's no output (printing)."
  (let ((winner (get-winner board)))
    (when verbose
      (print-board board)
      (format t "~a wins!" (to-string (get-player winner)))
      (fresh-line))
    (cond
      ((equal winner +player-one-mark+) 1)
      ((equal winner nil) 0)
      ((equal winner +player-two-mark+) -1))))

(defun get-owners-mark (board cell)
  "Returns the mark in the board at the given cell, which should
be on the interval [1, 9]."
  (aref board (1- cell)))

(defun all-equal (board a b c)
  "Determines if the given indices all have the same non-empty mark."
  (let ((x (aref board a))
	(y (aref board b))
	(z (aref board c)))
 (and x (equal x y) (equal y z) (not (empty-cell-p x)))))
	  
(defun get-winner (board)
  "Returns the mark of the winner."
  (loop for i below 3 do ;This could all be more elegant :(
       (let ((row (* i 3))) ;Rows
	     (if (all-equal board row (1+ row)(+ 2 row))
		   (return-from get-winner (aref board row))))
       (if (all-equal board i (+ i 3) (+ i 6)) ;Columns
	     (return-from get-winner (aref board i))))
  (when (or (all-equal board 0 4 8)
	    (all-equal board 2 4 6))
    (return-from get-winner (aref board 4))))
 
(defun evolve (population &optional (matches 50))
  "Returns the most fit (hopefully!) individual from the given population. Uses tournament selection."
  (cond
    ((> (length population) 4)
     (let* ((population1 (subseq population 0 (middle population)))
	    (population2 (subseq population (middle population) (length population)))
	    (A-babies (make-babies population1))
	    (B-babies (make-babies population2)))
       (format t "~&~%~%Evolving a population with ~a members" (length population))
       (evolve
	(loop for a in A-babies
	   for b in B-babies
	   collect (select a b matches)))))
    ((= (length population) 4) ;Final Four needs to be handled differently
     (format t "~&In the Final Four.")
     (crossover (select (first population) (fourth population))
		(select (second population) (third population))))
    (t population)))

(defun make-babies (population)
  (loop for a in (subseq population 0 (middle population))
     for b in (subseq population (middle population) (length population))
     collect (crossover a b)))

(defun crossover (a b &optional (mutation-rate 2))
  "Crosses over (breeds)  the individuals `a' and `b', returning their ``child''.
`mutation-rate' is the percent chance that a mutation will happen during the crossover."
  (let* ((index (random (length a)))
	 (baby
	  (append (subseq a 0 index)
		  (subseq b (min index (1- (length b))) (length b)))))
    (if (< (random 100) mutation-rate)
	(let* ((chromosomes (get-chromosomes))
	       (new-chrom (elt chromosomes (random (length chromosomes)))))
	  (format t "Mutating!")
	  (case (random 3)
	    (0
	     (append (list new-chrom) baby))
	    (1
	     (setf (nth (random (length baby)) baby) new-chrom)
	     baby)
	    (2
	     (setf (nth (random (length baby)) baby) nil)
	     baby)))
	baby)))

(defun middle (sequence)
  "Returns the middle index (not value) of the given sequence.
Rounds to the right: (elt '(a b) (middle '(a b))) is b, not a.
Warning: Returns 0 if (length sequence) is 0."
  (truncate (/ (length sequence) 2)))

(defun make-population (size chromosomes min-chrom max-chrom)
  "Generates a population of the given size. Each individual will have a gene composed
of a random number of chromosomes between min-chrom and max-chrom of the given chromosomes."
  (loop for i from 1 to size collecting (make-individual chromosomes min-chrom max-chrom)))


(defun select (a b &optional (matches 100))
  "Returns the more fit individual--a or b. Optional `matches' is the number of rounds to play (defaults to 100).
In the event of a tie, b is returned for no good reason at all."
  (if (positive-p (fitness a b matches))
      a
      b))

(defun positive-p (n)
  "Returns true if n is positive, nil if it's not."
  (if (> n 0)
      t
      nil))

(defun fitness (a b matches)
  "Returns a's fitness, which is of equal magnitude but opposite sign of b's fitness. This is
accomplished by pitting a and b against each other for the given number of matches (or 1+ that if matches is odd),
and returning a's *average* score."
  (/ (loop for i from 1 to (ceiling matches 2) summing (faceoff a b)) matches))
					;Each faceoff consists of two games, which is matches is divided by two.

(defun faceoff (a b)
  "Plays two games: a vs b, and b vs a. Returns all of a's points: +1 for each win, +0 for each tie, -1 for each loss"
  (let ((a-move (gene-to-function a))
	(b-move (gene-to-function b)))
  (- (game-loop (make-board) a-move b-move nil)
     (game-loop (make-board) b-move a-move nil))));The subtraction is to fix the sign.
					;game-loop returns 1 if the first player won--not if `a' won

(defun make-individual (chromosomes min-chrom max-chrom)
  "Creates an individual with a number of chromosomes between (1- min-chrom) and (1- max-chrom) drawn from chromosomes.
Also adds one `base' chromosome that ensures each board slot gets at least one vote."
  (when (< min-chrom 2)
      (print "min-chrom is too small! Raising it to 2")
      (setf min-chrom 2))
  (let ((reps (+ min-chrom (random (- max-chrom min-chrom)))))
    (append
     (loop for i from 1 to (1- reps) collecting (get-random-element chromosomes))
     (list (lambda(board)
	     (declare (ignorable board))
		      (loop for i from 1 to 9 collecting i))))))

 (defun get-random-element (sequence)
   "Returns one random element/chromosome from the sequence/gene provided"
  (elt sequence (random (length sequence))))

(defun flatten (list)
  "Takes a list of atoms and/or lists and removes all the lists, keeping their elements.
 (flatten '(1 2 3 (a b (c d e) (f g)))) -> (1 2 3 a b c d e f g)"
	   (let ((new-list '()))
	     (dolist (i list)
	       (if (not (listp i))
		   (setf new-list (append new-list (list i)))
		 (setf new-list (append new-list (flatten i)))))
	     new-list))

(defun get-chromosomes ()
  "Returns a list of all the chromosomes to be used."
  (let* ((corners '(1 3 7 9))
	 (edges '(2 4 6 8))
	 (center '(5))
	 (top-row '(1 2 3))
	 (mid-row '(4 5 6))
	 (bot-row '(7 8 9))
	 (left-col '(1 4 7))
	 (mid-col '(2 5 8))
	 (right-col '(3 6 9))
	 (diag1 '(1 5 9))
	 (diag2 '(3 5 7))
	 (lines (list top-row mid-row bot-row left-col mid-col right-col diag1 diag1)))
    (declare (ignorable corners edges center top-row mid-row bot-row left-col mid-col
			right-col diag1 diag2 lines))
    (labels
	((how-many-owned? (cells board mark)
	   (let ((counter 0))
	     (dolist (cell cells)
	       (if (string-equal mark (get-owners-mark board cell))
		   (incf counter)))
	     counter))
	 (how-many-empty? (cells board)
	   (how-many-owned? cells board +empty-mark+))
	 (own-mark (board)
	   (get-turn board));It'll always be the AI's turn.
	 (enemy-mark (board)
		      (if (string-equal +player-one-mark+ (own-mark board))
			  +player-two-mark+
			  +player-one-mark+)))
      (flatten ;Flattening gives us more freedom in creating chromosomes--we can create a list of them at once
       (list
	(lambda (board) ;Play with corners
	 (if (and (>= (how-many-owned? (apply #'list corners) board (enemy-mark board))
		       2)
		  (>= (how-many-empty? (apply #'list corners) board)
		       1))
	      (apply #'list corners)))	(lambda (board) ;Play with corners
	 (if (and (>= (how-many-owned? (apply #'list corners) board (enemy-mark board))
		       2)
		  (>= (how-many-empty? (apply #'list corners) board)
		       1))
	      (apply #'list corners)))
	(lambda (board) ;More playing with corners
	  (if (and (>= (how-many-owned? corners board (enemy-mark board)) 1)
		   (>= (how-many-empty? corners board) 2))
	      center))
	(lambda (board) ;Even more playing with corners
	  (if (and (empty-cell-p (get-owners-mark board (first center)))
		   (>= (how-many-empty? corners board) 3))
	      corners))
	(loop for line in lines collecting
	     (list
	      (lambda (board) ;Prevent opponent's victory
		(if (and (= (how-many-owned? line board (enemy-mark board)) 2)
			 (= (how-many-empty? line board) 1))
		    line))
	      (lambda (board) ;Make a winning move
		(if (and (= (how-many-owned? line board (own-mark board)) 2)
			 (= (how-many-empty? line board) 1))
		    line))))
	;; These three almost cancel each other out when they appear in the same
	;; quantity (although the center gets an edge). They're included with the
	;; hope that the ratio will be skewed in genes...and maybe they'll end up
	;; being more effective like that.
	(lambda (board) (declare (ignorable board))  corners)
	(lambda (board) (declare (ignorable board)) edges)
	(lambda (board) (declare (ignorable board)) (make-list 2 :initial-element (first center))))))))

;;A note about the (1- )s and (1+ )s sprinkled around: In general, everything to do with the player (whether he's a human or AI)
;;considers the board to have slots numbered 1-9. Everything to do with the game engine considers the board to have slots
;;numbered 0-8. So when the two meet, there's the potential for off-by-one errors unless (1- ) or (1+ ) is used.

(defun gene-to-function (gene &optional (move-map (make-array 9 :initial-element 0))) ;(loop for i from 1 to 9 collecting 0)))
  "Converts the given gene into a function that can be called by game-loop to actually play the game." 
  (lambda (board) ;Cells 0-8 of move-map contain the number of "votes" for board slots 1-9.
    (labels ;Since "To iterate is human, to recurse divine." ~L. Peter Deutsch
	((gene-logic (gene board move-map)
	   (let* ((move-map (get-favorite-moves move-map gene board))
		  (potential-move (1+ (get-best-index move-map)))) ;Find the most popular slot
	     (if (empty-cell-p (aref board (1- potential-move))) ;Make sure its empty
					;Though this is also checked by the game engine, it's checked here so that
					;a second-favorite (or third-favorite, or nth-favorite) move can be provided.
		 potential-move ;The move's fine; return it
		 (progn ;The move's illegal
		   (setf (elt move-map (1- potential-move)) 0) ;Remove all its votes
		   (gene-logic gene board move-map)))))) ;And recursively call this again, to get the next-best move
      (gene-logic gene board move-map))))

(defun get-favorite-moves (move-map gene board)
  "Applies the gene to the given move-map, and returns the modified move-map. This is where the gene ``votes''
on its favorite moves." 
  (dolist (chrom gene)
    (if chrom ; It could be nil (deleted as part of a mutation)
	(let ((slots (funcall chrom board))) ;`slots' is the moves the given chromosome would like to make
	  (dolist (slot slots)
	    (if (integerp slot) ;Error-check the move (it could be nil if the given chromosome doesn't like the way the board looks)
		(incf (elt move-map (1- slot)))))))) ;Get the slot the chromosome wants and increment its spot in dict
  move-map)

(defun max-p (dict i list-max)
  "Returns true if the ith element of dict is equal to the given list-max."
  (= (elt dict i) list-max))

(defun get-best-index (move-map)
  "For the given dictionary of move candidates, returns the most popular move."
  (let ((list-max (apply #'max (coerce move-map 'list))))
    (get-random-element ;There could be several indices tied for first-place. We'll just use a random one. (Genetics is a lottery!)
     (loop for i from 0 to (1- (length move-map)) 
	if (max-p move-map i list-max)
	collect i)))) ;...then keep the index

(defun test (&optional (pop-size 8))
  "Tests the AI creation. (Returns an AI gene (not function; use gene-to-function to convert it.))"
  (first (evolve (make-population pop-size (get-chromosomes) 300 400))))