
(defun find-min-max-do (my-list)
	(let* ((len (length my-list))
			 	 (curr-min (car my-list))
			   (curr-max (car my-list)))
		
	;;TODO: handle if odd, empty, etc

		;;body of let
		(progn
			(do ((i 0 (+ i 2))
					 (my-list my-list (cddr my-list)))
					((>= i (- len 2)) 'done) ;termination condition

				(let* ((next (car my-list))
							(next-next (second my-list))
							(smaller next) ;just setting them to arbitrary values now...
							(larger next-next))
					(progn (if (< next next-next)
									 ;then
									(progn (setf smaller next)
												 (setf larger next-next)
												 (format t "~%~S" my-list)) ;probably fixed it
									;else
									(progn (setf smaller next-next)
												 (setf larger next))) ;end if

								 (if (< smaller curr-min)
									 (setf curr-min smaller))
								 
								 (if (> larger curr-max)
									 (setf curr-max larger))))
									
						;(format t "~%~S" (my-list))
					;	(if (not (eq (cdr my-list) nil))
				;			(format t "here"))
				)	
			)


		;return answer
		(list curr-min curr-max)))

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
	 (setf *ttt-board* (make-array '(3 3))) 
)

;need to implement copy-array
(defun add-result-as-successor (successors board i j move)
	(progn
		(if (null (aref board i j))
			(let ((result-ttt-board copy-array(board)))
				(progn
					(setf (aref result-ttt-board i j) move)
					(push result-ttt-board successors)
				)
			)
		)
	)
)

;return states
;error check to see if someone already won (if so, no successors)
(defun successors (board move)
	(let ((successors-list (list))
			 )
		(progn
			(dotimes (i 3)
				(dotimes (j 3)
					(if (null (aref board i j))
						(add-result-as-successor sucessors-list board i j move)
					)
				)
			)
			successors-list
		)
	)
)

;make the board an array to allow for better columns/diagonals traversing
;make an array [a, b, c, d, e, f]
;defun tanimoto-heuristic (board)
;(let ((tanimoto-inputs (make-array '(6) :initial-element 0))
;			(numXs 0)
;			(numOs 0)
;			(X 'X)
;			(O 'O)
;		 )
;	(progn
;		;update inputs for each line
;		(dotimes (row 3)
;			(progn
;				(setf numXs 0)
;				(setf numOs 0)
;				(dotimes (col 3)
;					(progn
;						(if (eq X (aref board row col))
;							(setf numXs (1+ numXs))
;						)
;						(if (eq O (aref board row col))
;							(setf numOs (1+ numOs))
;						)
;					)
;				)

;				(format t "~%row ~a: numXs=~a numOs=~a" row numXs numOs)
;				(update-tanimoto-inputs tanimoto-inputs numXs numOs)
;				(format t "~%~a" tanimoto-inputs)
;			)	
;		)
;		
;		(dotimes (col 3)
;			(progn
;				(setf numXs 0)
;				(setf numOs 0)
;				(dotimes (row 3)
;					(progn
;						(if (eq X (aref board row col))
;							(setf numXs (1+ numXs))
;						)
;						(if (eq O (aref board row col))
;							(setf numOs (1+ numOs))
;						)
;					)
;				)
;				
;				(format t "~%col ~a: numXs=~a numOs=~a" col numXs numOs)
;				(update-tanimoto-inputs tanimoto-inputs numXs numOs)
;				(format t "~%~a" tanimoto-inputs)
;			)	
;		)

;		(dotimes (row-col 3)
;			(progn
;				(setf numXs 0)
;				(setf numOs 0)
;				(if (eq X (aref board row-col row-col))
;					(setf numXs (1+ numXs))
;				)
;				(if (eq O (aref board row-col row-col))
;					(setf numOs (1+ numOs))
;				)
;			)
;		)
;		
;		(format t "~%row-col ~a: numXs=~a numOs=~a" '0 numXs numOs)
;		(update-tanimoto-inputs tanimoto-inputs numXs numOs)
;		(format t "~%~a" tanimoto-inputs)


;		(dotimes (row-col 3)
;			(progn
;				(setf numXs 0)
;				(setf numOs 0)
;				(let ((other-diag-row-col (- 2 row-col)))
;					(progn
;						(if (eq X (aref board other-diag-row-col other-diag-row-col))
;							(setf numXs (1+ numXs))
;						)
;						(if (eq O (aref board other-diag-row-col other-diag-row-col))
;							(setf numOs (1+ numOs))
;						)
;					)
;				)
;			)
;		)
;			
;			(format t "~%row-col ~a: numXs=~a numOs=~a" '1 numXs numOs)
;			(update-tanimoto-inputs tanimoto-inputs numXs numOs)
;			(format t "~%~a" tanimoto-inputs)




;			(evaluate-tanimoto-function tanimoto-inputs)
;	)
;)
;

;works
;defun update-tanimoto-inputs (tanimoto-inputs numXs numOs)
;(progn
;	(if (eq numOs 0)
;		(cond
;			((eq numXs 3)	(setf (aref tanimoto-inputs 0) (1+ (aref tanimoto-inputs 0))))
;			((eq numXs 2) (setf (aref tanimoto-inputs 1) (1+ (aref tanimoto-inputs 1))))
;			((eq numXs 1)	(setf (aref tanimoto-inputs 2) (1+ (aref tanimoto-inputs 2))))
;		)
;	)
;	(if (eq numXs 0)
;		(cond
;			((eq numOs 3)	(setf (aref tanimoto-inputs 3) (1+ (aref tanimoto-inputs 3))))
;			((eq numOs 2) (setf (aref tanimoto-inputs 4) (1+ (aref tanimoto-inputs 4))))
;			((eq numOs 1)	(setf (aref tanimoto-inputs 5) (1+ (aref tanimoto-inputs 5))))
;		)
;	)
;	tanimoto-inputs
;)
;

;works
;defun evaluate-tanimoto-function (tanimoto-inputs)
;(let ((weights (make-array '(6) :initial-contents '(100 10 1 -100 -10 -1)))
;			(res 0)
;		 )
;	(progn
;		(dotimes (i 6)
;			(setf res (+ res (* (aref weights i) (aref tanimoto-inputs i))))
;		)
;		res
;	)
;)
;





















(defun test-nth ()
	(let ((my-list '(a b c)))
		(format t "~% first is ~a" (nth 0 my-list))))

(defun test-do ()
	(let ((my-list '(x y z)))
		(do ((i 0 (1+ i)))
				 ((eq i 10) 'hello)
			(format t "~%~a" i))))


(defun test-dolist ()
	(let ((my-list '(a b c d)))
				(dolist (elem my-list 'whatsup)
					(format t "~%~a" elem))))
								 

(defun testdotimes ()
	(let ((upper-bound 2))
		(dotimes (cnt upper-bound result)
			(setf result (1+ cnt))
			(format t "~%The output: ~a" result))))

(defun test-if ()
	(let* ((x 5)
				(y 6)
				(z 5)
				(arg1 x)
				(arg2 z))
		(if (eq arg1 arg2)
			(format t "~%~a and ~a ~a" arg1 arg2 '"are equal!")
			(format t "~%~a and ~a ~a " arg1 arg2 '"are not equal!"))))


(defun testlet ()
	(let ((m 2)
				(result 3)) 
		5))


(defun test-assign-vars ()
	(setf my-var 1)
	(setf my-var-2 2)
	(setf result (+ my-var my-var-2))
	(setf my -1)
	(format t "The output: ~a~%" result))




;works
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

;works
(defun evaluate-tanimoto-function (tanimoto-inputs)
	(let ((weights (make-array '(6) :initial-contents '(100 10 1 -100 -10 -1)))
				(res 0)
			 )
		(progn
			(dotimes (i 6)
				(setf res (+ res (* (aref weights i) (aref tanimoto-inputs i))))
			)
			res
		)
	)
)





















(defun test-nth ()
	(let ((my-list '(a b c)))
		(format t "~% first is ~a" (nth 0 my-list))))

(defun test-do ()
	(let ((my-list '(x y z)))
		(do ((i 0 (1+ i)))
				 ((eq i 10) 'hello)
			(format t "~%~a" i))))


(defun test-dolist ()
	(let ((my-list '(a b c d)))
				(dolist (elem my-list 'whatsup)
					(format t "~%~a" elem))))
								 

(defun testdotimes ()
	(let ((upper-bound 2))
		(dotimes (cnt upper-bound result)
			(setf result (1+ cnt))
			(format t "~%The output: ~a" result))))

(defun test-if ()
	(let* ((x 5)
				(y 6)
				(z 5)
				(arg1 x)
				(arg2 z))
		(if (eq arg1 arg2)
			(format t "~%~a and ~a ~a" arg1 arg2 '"are equal!")
			(format t "~%~a and ~a ~a " arg1 arg2 '"are not equal!"))))


(defun testlet ()
	(let ((m 2)
				(result 3)) 
		5))


(defun test-assign-vars ()
	(setf my-var 1)
	(setf my-var-2 2)
	(setf result (+ my-var my-var-2))
	(setf my -1)
	(format t "The output: ~a~%" result))
