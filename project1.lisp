;;;;
;;;; Alex Keyes
;;;; ark73@georgetown.edu
;;;; Platform: Mac OS X
;;;; Lisp Environment: clisp
;;;;
;;;; In accordance with the class policies and Georgetown's Honor Code,
;;;; I certify that, with the exceptions of the course materials and those
;;;; items noted below, I have neither given nor received any assistance
;;;; on this project.
;;;;

;(load "project1.lisp") (find-min-max-do '(1 -4 3 2 0))
(defun find-min-max-do (my-list)
	(let ((len (length my-list))
				(curr-min (car my-list))
				(curr-max (car my-list)))
		
		;;body of let
		(do ((i 0 (1+ i))
				 (my-list my-list (cdr my-list)))
				((eq i len) 'done) ;termination condition

			;;update min and max
			(if (< (car my-list) curr-min)
				(setf curr-min (car my-list)))

			(if (> (car my-list) curr-max)
				(setf curr-max (car my-list))))
			
		;return answer
		(list curr-min curr-max)))

;(load "project1.lisp") (find-min-max-r '(1 -4 3 2 0))
(defun find-min-max-r (my-list)
	(find-min-max-r-aux my-list (car my-list) (car my-list))
)

(defun find-min-max-r-aux (my-list curr-min curr-max)
	(if (eq my-list nil)
	   (return-from find-min-max-r-aux (list curr-min curr-max)))

	 (let ((head (car my-list))
				)
		 (if (< head curr-min)
			(setf curr-min head)
		 )
		 (if (> head curr-max)
			 (setf curr-max head)
		 )
		
		 (find-min-max-r-aux (cdr my-list) curr-min curr-max)
	 )
)

;(load "project1.lisp") (find-min-max-apply '(1 -4 3 2 0))
(defun find-min-max-apply (my-list)
(let ((min-val (apply 'min my-list))
	(max-val (apply 'max my-list)))

	(list min-val max-val))
)



(defun build-tree ()
	(setf *tree* '(O 
							 (R
									(E 
										(G nil nil)
										(O nil nil))
									(E
										(G nil nil)
										(T nil nil))
							 )
							 (O
								 (N
									 (W nil nil)
									 (H nil nil))
								 (A
									 (Y nil nil)
									 (S nil nil))
							 )
							)
	)
)

;(load "project1.lisp") (build-tree) (preorder *tree*)
;(O R E G O E G T O N W H A Y S)
(defun preorder (tree)
	(reverse (preorder-aux tree ()))
)

(defun preorder-aux (tree traversal)
	(if (null tree)
		(return-from preorder-aux))

	(push (first tree) traversal)
	(let ((left-subtree (preorder (second tree)))
			(right-subtree (preorder (third tree))))

		(if left-subtree
			(setf traversal (append (reverse left-subtree) traversal)) ;inefficient
		)
		(if right-subtree
			(setf traversal (append (reverse right-subtree) traversal))
		)

		traversal
	)
)

;implement postorder when you finish preorder (needs to be more effient)
;(load "project1.lisp") (build-tree) (postorder *tree*)
;(G O E G T E R W H N Y S A O O)
(defun postorder (tree)
	(reverse (postorder-aux tree ()))
)

(defun postorder-aux (tree traversal)
	(if (null tree)
		(return-from postorder-aux))

	(let ((left-subtree (postorder (second tree)))
			(right-subtree (postorder (third tree))))
		
		(if left-subtree
			(setf traversal (append (reverse left-subtree) traversal))
		)
		(if right-subtree
			(setf traversal (append (reverse right-subtree) traversal))
		)
		
		(push (first tree) traversal)

		traversal
	)
)



;*graph*?
(defun build-graph ()
	(prog1
		(setf (get 'A 'adj-list) '(B C))
		(setf (get 'B 'adj-list) '(C D))
		(setf (get 'C 'adj-list) '(D F))
		(setf (get 'D 'adj-list) '(E))
		(setf (get 'E 'adj-list) '(G))
		(setf (get 'F 'adj-list) '(E))
	)	
)

;(load "project1.lisp") (build-graph) (dfs 'a 'f)

;rewrite to use a set instead of visited properties
(defun reset-visited ()
	(setf (get 'A 'visited) nil)
	(setf (get 'B 'visited) nil)
	(setf (get 'C 'visited) nil)
	(setf (get 'D 'visited) nil)
	(setf (get 'E 'visited) nil)
	(setf (get 'F 'visited) nil)
	(setf (get 'G 'visited) nil) 
)

(defun dfs (start end)
	(reset-visited)
	(setf (get start 'visited) T)
	(dfs-aux start end)
)

(defun dfs-aux (curr end)
	(if (eq curr end)
		(return-from dfs-aux (list curr)))

	(dolist (elem (get curr 'adj-list))
		(if (null (get elem 'visited))
			(progn
				(setf (get elem 'visited) T)
				(let ((res (dfs-aux elem end)))
					(if res
						(return-from dfs-aux (push curr res))
					)
				)
			)
		)
	)
)


(defun build-ttt-board ()
 (setf *ttt-board* (make-array '(3 3) :intial-element nil)) 
)

;(load "project1.lisp") (setf *board* (make-array '(3 3) :initial-contents '((X O O) (O X nil) (nil X nil)))) (successors *board* 'X)

;return states
(defun successors (board move)
	(let ((successors-list ())
				(tanimoto-inputs (get-tanimoto-inputs board))
			 )
		(if	(or 
					(eq (aref tanimoto-inputs 0) 1) 
					(eq (aref tanimoto-inputs 3) 1))
			(return-from successors nil))

		(dotimes (i (first (array-dimensions board)))
			(dotimes (j (second (array-dimensions board)))
				(if (null (aref board i j))
					;why doesn't (add-result-as-successor successors-list board i j move) work
					(setf successors-list (add-result-as-successor successors-list board i j move))
				)
			)
		)

		successors-list
	)
)

(defun copy-2d-array (arr)
	(let ((new-array (make-array (array-dimensions arr) :initial-element nil)))
		(dotimes (i (first (array-dimensions arr)))
			(dotimes (j (second (array-dimensions arr)))
				(setf (aref new-array i j) (aref arr i j))
			)
		)
		new-array
	)
)


(defun add-result-as-successor (successors board i j move)
	(if (null (aref board i j))
		(progn
			(let ((result-ttt-board (copy-2d-array board)))
				(setf (aref result-ttt-board i j) move)
				(push result-ttt-board successors)
			)
		)
	)
	successors ;shouldn't need to grab this
)



;evalute function
(defun evaluate-tanimoto-function (board)
	(let ((weights (make-array '(6) :initial-contents '(100 10 1 -100 -10 -1)))
			(res 0)
			(tanimoto-inputs (get-tanimoto-inputs board)))
	 (dotimes (i 6)
		 (setf res (+ res (* (aref weights i) (aref tanimoto-inputs i))))
	 )
	 
	 res
	)
)




;makes an array [a, b, c, d, e, f]
(defun get-tanimoto-inputs (board)
	(let ((tanimoto-inputs (make-array '(6) :initial-element 0))
			(numXs 0)
			(numOs 0)
			(X 'X)
			(O 'O)
			(numRows (first (array-dimensions board)))
			(numCols (second (array-dimensions board))))
		;handle rows
		(dotimes (row numRows)
			(setf numXs 0)
			(setf numOs 0)
			(dotimes (col numCols)
				(if (eq X (aref board row col))
					(setf numXs (1+ numXs))
				)
				(if (eq O (aref board row col))
					(setf numOs (1+ numOs))
				)
			)

			(update-tanimoto-inputs tanimoto-inputs numXs numOs)
		)

		;handle cols
		(dotimes (col numCols)
			(setf numXs 0)
			(setf numOs 0)
			(dotimes (row numRows)
				(if (eq X (aref board row col))
					(setf numXs (1+ numXs))
				)
				(if (eq O (aref board row col))
					(setf numOs (1+ numOs))
				)
			)
			
			(update-tanimoto-inputs tanimoto-inputs numXs numOs)
		)

		;handle diagonals
		(setf numXs 0)
		(setf numOs 0)
		(dotimes (row-col numRows)
			(if (eq X (aref board row-col row-col))
				(setf numXs (1+ numXs))
			)
			(if (eq O (aref board row-col row-col))
				(setf numOs (1+ numOs))
			)
		)
		(update-tanimoto-inputs tanimoto-inputs numXs numOs)


		(setf numXs 0)
		(setf numOs 0)
		(dotimes (row-col numRows)
			(if (eq X (aref board (- (1- numRows) row-col) row-col))
				(setf numXs (1+ numXs))
			)
			(if (eq O (aref board (- (1- numRows) row-col) row-col))
				(setf numOs (1+ numOs))
			)
		)
		(update-tanimoto-inputs tanimoto-inputs numXs numOs)

		tanimoto-inputs
	)
)

;updates inputs array based on numXs and numOs
(defun update-tanimoto-inputs (tanimoto-inputs numXs numOs)
	(if (eq numOs 0)
		(cond
			((eq numXs 3)	(setf (aref tanimoto-inputs 0) (1+ (aref tanimoto-inputs 0))))
			((eq numXs 2) (setf (aref tanimoto-inputs 1) (1+ (aref tanimoto-inputs 1))))
			((eq numXs 1)	(setf (aref tanimoto-inputs 2) (1+ (aref tanimoto-inputs 2))))
		)
	)
	(if (eq numXs 0)
		(cond
			((eq numOs 3)	(setf (aref tanimoto-inputs 3) (1+ (aref tanimoto-inputs 3))))
			((eq numOs 2) (setf (aref tanimoto-inputs 4) (1+ (aref tanimoto-inputs 4))))
			((eq numOs 1)	(setf (aref tanimoto-inputs 5) (1+ (aref tanimoto-inputs 5))))
		)
	)
	tanimoto-inputs
)

;(load "project1.lisp") (load-maze-problem "tinyMaze.lay")

(defun load-maze-problem (filename)
	(let ((istream (open filename :direction :input :if-does-not-exist nil))
				(x-dimen nil)
				(y-dimen nil)
				(maze nil)
				(problem nil)
				(curr-char nil)
				(start-location nil)
				(goal-location nil))
		(if (streamp istream)
		  (progn
				(let ((curr-input nil))

					(setf curr-input (read istream nil 'eof))
					(setf x-dimen curr-input)

					(setf curr-input (read istream nil 'eof))
					(setf y-dimen curr-input)
				)

				(setf maze (make-array (list x-dimen y-dimen) :initial-element nil))

				(do ((curr-input (read-line istream nil 'eof) (read-line istream nil 'eof))
						 (row 0 (1+ row)))
					((eq curr-input 'eof) 'done) ;termination condition
					
					(dotimes (col x-dimen)
						(setf curr-char (char curr-input col))
						(cond 
							((eq curr-char #\S) 
							 (progn 
								 (setf start-location (list row col))
								 (setf curr-char #\Space)
							 )
							)
							((eq curr-char #\G) 
							 (progn
							 		(setf goal-location (list row col))
									(setf curr-char #\Space)
							 )
							)
						)
						
						(setf (aref maze row col) curr-char) 
					)
				)
			
				(close istream)
			)

			(format t "could not open file ~s~%" filename)
		)
		
		(setf (get problem 'maze) maze)
		(setf (get problem 'start-state) (copy-2d-array maze))
		(setf (aref (get problem 'start-state) (first start-location) (second start-location)) #\S)
		(setf (get problem 'goal-state) (copy-2d-array maze))
		(setf (aref (get problem 'goal-state) (first goal-location) (second goal-location)) #\G)

		(format t "~% ~a ~a ~a" (get problem 'maze) (get problem 'start-state) (get problem 'goal-state))

		problem
	)
)





