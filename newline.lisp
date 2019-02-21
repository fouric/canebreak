(defparameter *newline-object* (gensym "NEWLINE"))

(defun newline-reader (stream char)
  (declare (ignore char stream))
  *newline-object*)

(let ((contents (let ((*readtable* (copy-readtable)))
                  (set-macro-character #\newline #'newline-reader)
                  (with-open-file (in "test.sexp"
                                      :direction :input
                                      :if-exists :supersede)
                    (read in)))))
  (parse contents :lisp))

(defun parse (form mode)
  (etypecase form
    (list (let ((sans-newline (if (and (member (first form) '(section)) (eq mode :assembly))
                                  form
                                  (remove *newline-object* form))))
            (mapcar (lambda (form) (parse form (if (eq mode :assembly) :lisp :assembly))) sans-newline)))
    (nil
     nil)
    (t
     form)))
