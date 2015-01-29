;remember to collapse the parens
(defun find-min-max-recursion (my-list)
	(find-min-max-recursion-aux my-list (car my-list) (car my-list))
)

(defun find-min-max-recursion-aux (my-list curr-min curr-max)
	(progn	
;		(format t "~%current list: ~S" my-list) 
		(if (eq my-list nil)
			 ;then
			 (progn
;			 	(format t "~%~s" "list is null")
			 	(list curr-min curr-max)
			 )

			 ;else
			 (let ((head (car my-list))
						)
			   (progn 
					 (if (< head curr-min)
						(setf curr-min head)
					 )
					 (if (> head curr-max)
						 (setf curr-max head)
					 )
;					 (format t "~%cdr of list: ~S" (cdr my-list))
;					 (format t "~%~s ~a" "curr-min" curr-min)
;				 	 (format t "~%~s ~a" "curr-max" curr-max)
					
					 (find-min-max-recursion-aux (cdr my-list) curr-min curr-max)
				 )
			 )
		)	)
)

(defun find-min-max-apply (my-list)
	(let ((min-val (apply 'min my-list))
		(max-val (apply 'max my-list)))

		(list min-val max-val))
)

(defun find-min-max-do-naive (my-list)
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

			
		;;output answer
		(format t "~%Min: ~a" curr-min)
		(format t "~%Max: ~a" curr-max)

		;return answer
		(list curr-min curr-max)))

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

(defun build-small-tree ()
	(setf *small-tree* '(D
											 (B
												(A nil nil)
												(C nil nil)
											 )
											 nil)
	)
)

(defun preorder (tree)
	(reverse (preorder-aux tree (list)))
)

;;needs to be more effient
(defun preorder-aux (tree traversal)
	(if (eq tree nil)
		;;then
		nil

		;;else
		(progn
			(format t "~%root: ~a" (car tree))
			(push (car tree) traversal)
				
			(let ((left-subtree (preorder (second tree)))
						(right-subtree (preorder (third tree)))
					 )
				(progn
					(format t "~%left: ~a" left-subtree)
					(format t "~%right: ~a" right-subtree)
					(format t "~%traversal ~a" traversal)

					(if (not (eq left-subtree nil))
						(setf traversal (append (reverse left-subtree) traversal))
					)
					(if (not (eq right-subtree nil))
						(setf traversal (append (reverse right-subtree) traversal))
					)
					(format t "~% returning traversal: ~a" traversal)
					traversal
				)
			)
		)
	)
)

;implement postorder when you finish preorder (needs to be more effient)

;*graph*?
(defun build-graph ()
	(prog1
		(setf (get 'A 'adj-list) '(B C))
		(setf (get 'B 'adj-list) '(C D))
		(setf (get 'C 'adj-list) '(D F))
		(setf (get 'D 'adj-list) '(E))
		(setf (get 'E 'adj-list) '(G))
		(setf (get 'F 'adj-list) '(E))
		(setf (get 'G 'adj-list) nil) ;probably not necessary



	)
)

;rewrite to use a set instead of visited properties
(defun reset-visited ()
	(setf (get 'A 'visited) nil)
	(setf (get 'B 'visited) nil)
	(setf (get 'C 'visited) nil)
	(setf (get 'D 'visited) nil)
	(setf (get 'E 'visited) nil)
	(setf (get 'F 'visited) nil)
	(setf (get 'G 'visited) nil) ;probably not necessary
)

(defun dfs (start end)
	(progn 
		(reset-visited)
		(setf (get start 'visited) T)
		(dfs-aux start end)
	)
)

(defun dfs-aux (curr end)
	(progn
		(if (eq curr end)
			;then
			(list curr)

			;else
			(progn 
				(dolist (elem (get curr 'adj-list))
					(if ( null (get elem 'visited))
						(progn
;								(format t "~%Visiting: ~a" elem)
							(setf (get elem 'visited) T)
							(let ((res (dfs-aux elem end))
									 )
								(if (not (null res) )
									(progn
;												(format t "~%curr: ~a, res: ~a" curr res)
										(let ((res-list (push curr res))
												 )
											(return-from dfs-aux res-list)
										)
									)
								)
							)
						)
					)
				)
			)

		)

	)
)


(defun build-ttt-board ()
	 (setf *ttt-board* (make-array '(3 3) :intial-element nil)) 
)

;(load "project1.lisp") (setf *board* (make-array '(3 3) :initial-contents '((X O O) (O X nil) (nil X X)))) (successors *board* 'X)

;return states
;error check to see if someone already won (if so, no successors)
(defun successors (board move)
	(let ((successors-list ())
				(tanimoto-inputs (get-tanimoto-inputs board))
			 )
		(if	(or 
					(eq (aref tanimoto-inputs 0) 1) 
					(eq (aref tanimoto-inputs 3) 1)
				)
			(return-from successors nil)
		)

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
	(progn
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
)



;evalute function
(defun evaluate-tanimoto-function (board)
 (let ((weights (make-array '(6) :initial-contents '(100 10 1 -100 -10 -1)))
 			(res 0)
			(tanimoto-inputs (get-tanimoto-inputs board))
 		 )
 	 (dotimes (i 6)
 	 	 (setf res (+ res (* (aref weights i) (aref tanimoto-inputs i))))
 	 )
 	 
	 res
 )
)




;try to get rid of magic numbers
;makes an array [a, b, c, d, e, f]
(defun get-tanimoto-inputs (board)
 (let ((tanimoto-inputs (make-array '(6) :initial-element 0))
			(numXs 0)
			(numOs 0)
			(X 'X)
			(O 'O)
			(numRows (first (array-dimensions board)))
			(numCols (second (array-dimensions board)))
		 )
	;handle rows
	(dotimes (row numRows)
		(progn
			(setf numXs 0)
			(setf numOs 0)
			(dotimes (col numCols)
				(progn
					(if (eq X (aref board row col))
						(setf numXs (1+ numXs))
					)
					(if (eq O (aref board row col))
						(setf numOs (1+ numOs))
					)
				)
			)

			(update-tanimoto-inputs tanimoto-inputs numXs numOs)
		)	
	)
	
	;handle cols
	(dotimes (col numCols)
		(progn
			(setf numXs 0)
			(setf numOs 0)
			(dotimes (row numRows)
				(progn
					(if (eq X (aref board row col))
						(setf numXs (1+ numXs))
					)
					(if (eq O (aref board row col))
						(setf numOs (1+ numOs))
					)
				)
			)
			
			(update-tanimoto-inputs tanimoto-inputs numXs numOs)
		)	
	)

	;handle diagonals
	(setf numXs 0)
	(setf numOs 0)
	(dotimes (row-col numRows)
		(progn
			(if (eq X (aref board row-col row-col))
				(setf numXs (1+ numXs))
			)
			(if (eq O (aref board row-col row-col))
				(setf numOs (1+ numOs))
			)
		)
	)
	(update-tanimoto-inputs tanimoto-inputs numXs numOs)


	(setf numXs 0)
	(setf numOs 0)
	(dotimes (row-col numRows)
		(progn
			(if (eq X (aref board (- (1- numRows) row-col) row-col))
				(setf numXs (1+ numXs))
			)
			(if (eq O (aref board (- (1- numRows) row-col) row-col))
				(setf numOs (1+ numOs))
			)
		)
	)
	(update-tanimoto-inputs tanimoto-inputs numXs numOs)

	tanimoto-inputs
 )
)

;updates inputs array based on numXs and numOs
(defun update-tanimoto-inputs (tanimoto-inputs numXs numOs)
 (progn
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
)





