(defun 8-puzzle (initial-state heuristicf)
	(general-search initial-state #'successor #'goalp heuristicf)
)
(defun successor (state &key (heuristicf #'misplaced) )
  (cond 
    ((eql (elt state 0) 0) 	
		(list (list "right" (swap state 0 1) (funcall heuristicf (swap state 0 1))) 
		(list "down"  	   	(swap state 0 3) (funcall heuristicf (swap state 0 3)))))  
     ((eql (elt state 1) 0) 
		(list (list "left"  (swap state 0 1) (funcall heuristicf (swap state 0 1))) 
		(list "down" 		(swap state 1 4) (funcall heuristicf (swap state 1 4))) 
		(list "right" 		(swap state 1 2) (funcall heuristicf (swap state 1 2)))))  
     ((eql (elt state 2) 0) 
		(list (list "left" 	(swap state 2 1) (funcall heuristicf  (swap state 2 1))) 
		(list "down" 		(swap state 2 5) (funcall heuristicf (swap state 2 5)))))  
     ((eql (elt state 3) 0) 
		(list (list "up" 	(swap state 3 0) (funcall heuristicf (swap state 3 0)))
		(list "right" 		(swap state 3 4) (funcall heuristicf (swap state 3 4))) 
		(list "down" 		(swap state 3 6) (funcall heuristicf (swap state 3 6)))))  
     ((eql (elt state 4) 0) 
		(list (list "up" 	(swap state 4 1) (funcall heuristicf (swap state 4 1)))
		(list "right" 		(swap state 4 5) (funcall heuristicf (swap state 4 5))) 
		(list "down" 	 	(swap state 4 7) (funcall heuristicf (swap state 4 7))) 
		(list "left" 	  	(swap state 4 3) (funcall heuristicf (swap state 4 3)))))  
     ((eql (elt state 5) 0) 
		(list (list "up" 	(swap state 5 2) (funcall heuristicf (swap state 5 2))) 
		(list "left" 	 	(swap state 5 4) (funcall heuristicf (swap state 5 4))) 
		(list "down" 	 	(swap state 5 8) (funcall heuristicf (swap state 5 8))))) 
     ((eql (elt state 6) 0)
		(list (list "up" 	(swap state 6 3) (funcall heuristicf (swap state 6 3))) 
		(list "right" 	 	(swap state 6 7) (funcall heuristicf (swap state 6 7)))))  
     ((eql (elt state 7) 0)
		(list (list "up" 	(swap state 7 4) (funcall heuristicf (swap state 7 4))) 
		(list "right" 	 	(swap state 7 8) (funcall heuristicf (swap state 7 8))) 
		(list "left" 		(swap state 7 6) (funcall heuristicf (swap state 7 6)))))  
     ((eql (elt state 8) 0) 
		(list (list "up" 	(swap state 8 5) (funcall heuristicf (swap state 8 5)))
		(list "left"  		(swap state 8 7) (funcall heuristicf (swap state 8 7))))))) 

(defun swap (state x y) 
	(let ((temp (elt state x)))
		(setf temp-list (copy-list state))
		(setf (elt temp-list x) (elt temp-list y))
		(setf (elt temp-list y) temp) temp-list)) 

;; Generate set of 5 random cases
(defun random-case ()
	(list (random-generator) (random-generator)(random-generator)(random-generator)(random-generator)))

;; Generate 1 set of random start
(defun random-generator ()
  (let ((temp 1))
    (loop while (equal temp 1) do 
		(setf temp 0)
		(let ((randstate '())
        (rndm (random 9)) 
        (z 0)
        (invers 0))
		(loop for i in '(0 1 2 3 4 5 6 7 8) 
			do (loop while (not (equal (member rndm  randstate :test #'equal :key #'identity) nil)) 
				do  (setf rndm (random 9)))
		(setf randstate (append randstate (list rndm))))
	(if (evenp (car (last (mapcar (lambda (a) (let ((m z)) 
		(loop while (< m 9) 
			do (if (< (elt randstate m)a) 
				(setf invers (1+ invers))) 
				(setf m (1+ m))) 
				(setf z (1+ z))) invers)
			randstate)))) 
		randstate
		(setf temp 1))
	(unless (equal temp 1) 
		(return randstate))))))
		
(defun extracredit (node-state)
  (let ((heur (+ (manhattan node-state) (linear-conflict node-state)))) 
    heur))		

;; Linear conflict dominates manhattan distance by adding 2 to manhattan for each conflicting set of tiles
(defun linear-conflict (node-state)
   (let ((goal '(0 1 2 3 4 5 6 7 8))
        (tj-cost 0))
		(progn
			(if (and (member (elt node-state 0) (subseq goal 0 3))  (member (elt node-state 1) (subseq goal 0 3)) 
                 (< (elt node-state 1) (elt node-state 0)))
					(unless (or (equal (elt node-state 0) 0)(equal (elt node-state 1) 0)) (setf tj-cost (+ tj-cost 2))))
			(if (and (member (elt node-state 1) (subseq goal 0 3))  (member (elt node-state 2) (subseq goal 0 3)) 
                 (< (elt node-state 2) (elt node-state 1)))
					(unless (or (equal (elt node-state 1) 0)(equal (elt node-state 1) 2)) (setf tj-cost (+ tj-cost 2))))
			(if (and (member (elt node-state 3) (subseq goal 3 6))  (member (elt node-state 4) (subseq goal 3 6)) 
                 (< (elt node-state 8) (elt node-state 4)))
					(unless (or (equal (elt node-state 3) 0)(equal (elt node-state 4) 0)) (setf tj-cost (+ tj-cost 2))))
			(if (and (member (elt node-state 4) (subseq goal 3 6))  (member (elt node-state 5) (subseq goal 3 6)) 
                 (< (elt node-state 5) (elt node-state 4)))
					(unless (or (equal (elt node-state 4) 0)(equal (elt node-state 5) 0)) (setf tj-cost (+ tj-cost 2))))
			(if (and (member (elt node-state 6) (subseq goal 6 9))  (member (elt node-state 7) (subseq goal 6 9)) 
                 (< (elt node-state 7) (elt node-state 6)))
					(unless (or (equal (elt node-state 6) 0)(equal (elt node-state 1) 7)) (setf tj-cost (+ tj-cost 2))))
			(if (and (member (elt node-state 7) (subseq goal 6 9))  (member (elt node-state 8) (subseq goal 6 9)) 
                 (< (elt node-state 8) (elt node-state 7)))
					(unless (or (equal (elt node-state 7) 0)(equal (elt node-state 8) 0)) (setf tj-cost (+ tj-cost 2)))))
	tj-cost))
		
(defun manhattan (state)
	(let ((goal (mapcar (lambda (counter)
		(make-board 
			:position counter 
			:xcoords (cond ((<= counter 2) counter)
						((and (> counter 2) (<= counter 5)) (- counter 3))
						((and (> counter 5) (<= 8)) (- counter 6)))
			:ycoords (cond ((<= counter 2) 2)
						((and (> counter 2) (<= counter 5)) 1)
						((and (> counter 5) (<= 8)) 0))))
					'(0 1 2 3 4 5 6 7 8)))
				(count -1)) 
	(apply '+ (mapcar (lambda (temp-counter) 
			(setf count (1+ count)) 
				(if (equal temp-counter 0) 0 
					(+ (abs (- (board-xcoords (elt goal temp-counter)) (board-xcoords (elt goal count)))) 
					(abs (- (board-ycoords (elt goal temp-counter)) (board-ycoords (elt goal count))))))) 
					state))))

(defstruct (board)
      state
      xcoords
	  position
      ycoords)
			 
(defun xcoord (state indx)
  (elt (board-xcoords state) indx))

(defun ycoord (state indx)
  (elt (board-ycoords state) indx))
			 
(defun misplaced (state)
	(count-if #'null (mapcar #'eq state '(0 1 2 3 4 5 6 7 8))))

(defun samep (x y)
	(if (equal x y) t nil))
	
(defun goalp (x) 
	(if (equal x '(0 1 2 3 4 5 6 7 8)) t nil))

;; General search and graph search code from class with modification for heuristic	

(defun general-search (initial-state successor goalp heuristicf
						&key (samep #'eql)
							(enqueue #'enqueue-priority)
							(key #'node-final-cost))
	(setf exp-node 0)
	(let ((fringe (make-q :enqueue enqueue :key key)))
		(q-insert fringe (list (make-node :state initial-state :final-cost (funcall heuristicf initial-state))))
		(graph-search fringe nil successor goalp samep exp-node heuristicf)))
		
(defun graph-search (fringe closed successor goalp samep exp-node heuristicf)
	(unless (q-emptyp fringe)
		(let ((node (q-remove fringe)))
			(cond   ((funcall goalp (node-state node)) (action-sequence node exp-node))
					((member (node-state node) closed 
						:test #'samep :key #'node-state)
						(graph-search fringe closed successor goalp samep exp-node heuristicf))
					(t (let ((successors (expand successor node fringe heuristicf)))
						(setf exp-node (+ exp-node(length successors))) ;Modification to graph-search to keep track of expanded nodes
						(graph-search (q-insert fringe successors)
							(cons node closed)
							successor goalp samep exp-node heuristicf)))))))
										
(defun expand (successorf node fringe heuristicf)
  (let ((triples (funcall successorf (node-state node) :heuristicf heuristicf)))
		(mapcar (lambda (action-state-cost)
				(let ((action (car action-state-cost))
					(state (cadr action-state-cost))
					(cost (caddr action-state-cost)))
					(make-node  :state state 
								:parent node
								:action action
								:path-cost (+ (node-path-cost node) 1)
								:final-cost (+ (+ (node-path-cost node) 1) cost) 
								:depth (1+ (node-depth node)))))
             triples))) 
			 
(defun action-sequence (node expnode &optional (actions nil))
	(if (node-parent node)
		(action-sequence (node-parent node) expnode
						 (cons (node-action node) actions))
		 (cons actions (cons (length actions) (cons expnode nil)))
		 ))

;; Node definitition with final cost = path cost + heuristic cost
(defstruct node
	(state nil)
	(parent nil)
	(action nil)
	(path-cost 0)
	(final-cost 0)
	(depth 0))

;; Queue and Heap Functions	from class
	
(defstruct q
	(enqueue #'enqueue-FIFO)
	(key #'identity)
	(last nil)
	(elements nil))
	
(defun q-emptyp (q)
	(= (length (q-elements q)) 0))

(defun q-front (q)
	(elt (q-elements q) 0))

(defun q-remove (q)
	(if (listp (q-elements q))
		(pop (q-elements q))
			(heap-pop (q-elements q) (q-key q))))

(defun q-insert (q items)
	(funcall (q-enqueue q) q items)
		q)

(defun enqueue-LIFO (q items)
	(setf (q-elements q) (nconc items (q-elements q)))
	items)
	
(defun enqueue-FIFO (q items)
	(if (q-emptyp q)
		(setf (q-elements q) items)
		(setf (cdr (q-last q)) items))
	(setf (q-last q) (last items))
	items)

(defun enqueue-priority (q items)
	(when (null (q-elements q))
		(setf (q-elements q) (make-heap)))
	(mapc (lambda (item)
			(heap-insert (q-elements q) item (q-key q)))
		items)
	items)

(defun make-heap (&optional (size 100))
	(make-array size :fill-pointer 0 :adjustable t))

(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
	(unless (heap-leafp heap i)
		(let ((left-index (heap-left i))
				(right-index (heap-right i)))
			(let ((smaller-index
				(if (and (< right-index (length heap))
						 (< (heap-val heap right-index key)
							(heap-val heap left-index key)))
					right-index
					left-index)))
			(when (> (heap-val heap i key)
					 (heap-val heap smaller-index key))
				(rotatef (elt heap i)
						 (elt heap smaller-index))
				(heapify heap smaller-index key))))))

(defun heap-pop (heap key)
	(let ((min (elt heap 0)))
		(setf (elt heap 0) (elt heap (1- (length heap))))
		(decf (fill-pointer heap))
		(heapify heap 0 key)
		min))
		
(defun heap-insert (heap item key)
  (vector-push-extend nil heap)
  (setf (elt heap (heap-find-pos heap (1- (length heap))
		(funcall key item) key))
			item))
			
(defun heap-find-pos (heap i val key)
  (cond ((or (zerop i) (< (heap-val heap (heap-parent i) key) val)) i)
        (t (setf (elt heap i) (elt heap (heap-parent i)))
           (heap-find-pos heap (heap-parent i) val key))))