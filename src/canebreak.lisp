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
     (command form))))

(defun command (form)
  (case (first form)
    (section
     (incf *indentation-level*)
     (decf *indentation-level*))))

(defun emit (object)
  (doitimes ((* *indentation-level* *indentation-spaces*))
    (format *out-file* " "))
  (format *out-file* "~a~%" object))
