(defparameter *out-file* nil)
(defparameter *indentation-level* 0)
(defparameter *indentation-spaces* 4)

(defun resource (path)
  (asdf:system-relative-pathname 'canebreak path))

(defun canebreak (in-filename out-filename)
  (with-open-file (out out-filename
		       :direction :output
		       :if-exists :supersede)
    (setf *out-file* out)
    (mapcar (lambda (x) (emit (process x))) (read-file in-filename))
    (setf *out-file* nil)))

(defun process (form)
  (format t "~a~%" form)
  (etypecase form
    (string
     form)
    (list
     (directive-or-section form))))

(defun directive-or-section (form)
  (let ((command-name (first form)))
    (cond 
      ((eql command-name 'text)
       ".text")
      ((eql command-name 'global)
       (concatenate 'string ".global " (string-downcase (second form))))
      ((eql command-name 'section)
       (let ((name (second form)))
	 (concatenate 'string (string-downcase name) ":")
	 (incf *indentation-level*)
	 (decf *indentation-level*)))
      )))

(defun )

(defun emit (object)
  (doitimes ((* *indentation-level* *indentation-spaces*))
    (format *out-file* " "))
  (format *out-file* "~a~%" object))
