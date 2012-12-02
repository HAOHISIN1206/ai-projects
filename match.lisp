(defun match (pattern fact &optional bindings)
	(cond 	((and (listp pattern) (listp fact)) (match-list pattern fact bindings))
			((and (atom pattern) (eq '#\= (elt (symbol-name pattern) 0))) (match-var pattern fact bindings))
			((and (atom pattern) (eq '#\! (elt (symbol-name pattern) 0))) (match-not pattern fact bindings))
			((and (atom pattern) (eq '#\< (elt (symbol-name pattern) 0))) (match-less pattern fact bindings))
			((and (atom pattern) (eq '#\> (elt (symbol-name pattern) 0))) (match-great pattern fact bindings))
			((and (atom pattern) (atom fact)) (match-atom pattern fact bindings))
			((eql '& (car pattern)) (match-and (cdr pattern) fact bindings))
			(t (nil))))
	
(defun match-atom (x y bindings)
	(if (equal x y) bindings nil))

(defun match-var (x y bindings)
	(setf temp-bind (assoc (var-name x) bindings))
	(if temp-bind
		(if (equal (cdr temp-bind) y) bindings nil)
		(acons x y bindings)))
		
(defun var-name (x)
	(intern (concatenate 'string "=" (subseq (symbol-name x) 1 (length (symbol-name x))))))

(defun match-and (x y bindings)
	(if (null (cdr x)) (match (car x) y bindings)
       (let ((rest-cdr  (match-and (cdr x) y bindings) )) 
			(if rest-cdr 
				(match (car x) y (if (consp rest-cdr) rest-cdr bindings))
				nil))))
	
(defun match-not (x y bindings)
	(setf temp-bind (assoc (var-name x) bindings))
	(if (not (equal (cdr temp-bind) y))
		bindings
		nil))

(defun match-less (x y bindings)
	(setf temp-bind (assoc (var-name x) bindings))
	(if (< y (cdr temp-bind))
		bindings
		nil))
		
(defun match-great (x y bindings)
	(setf temp-bind (assoc (var-name x) bindings))
	(if (> y (cdr temp-bind))
		bindings
		nil))

(defun match-list (x y bindings)
	(if (and (null x) (null y)) 
		(setf temp-list nil) 
		(setf temp-list (match (car x) (car y) bindings)))
	(if (not (cdr x))
		temp-list
		(if (null temp-list) nil (match (cdr x) (cdr y) temp-list))))