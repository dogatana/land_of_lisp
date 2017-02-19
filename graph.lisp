(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp)
		 (string-trim "() " (prin1-to-string exp))))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	  edges))

(defun graph->dot (nodes edges)
  (princ "digraph {")
  (nodes->dot nodes)
  (edges->dot edges)
  (fresh-line)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (ccl:run-program "dot" (list "-Tpng" "-O" fname)))

(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     ;; (print "search: ")
		     ;; (prin1 (car edge))
		     ;; (print "from: ")
		     ;; (prin1 (cdr lst))
		     ;; (print "result: ")
		     ;; (prin1 (assoc (car edge) (cdr lst)))
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst)))
	     edges))

(defun ugraph->dot (nodes edges)
  (princ "graph {")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
					     a wizard is snoring loudly on the couch.))
			       (garden (you are in a beautiful garden.
					there is a well in front of you.))
			       (attic (you are in the attic. there
				       is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((living-room (garden west door)
				(attic upstairs ladder))
			       (garden (living-room east door))
			       (attic (living-room downstairs ladder))))

