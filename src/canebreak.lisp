(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :fouriclib))

(defparameter *out-file* nil)
(defparameter *indentation-level* 0)
(defparameter *indentation-spaces* 4)

(defun join-commas (&rest args)
  "joins strings with commas and spaces"
  (if (zerop (length args))
      ""
      (let ((out (first args)))
        (dolist (arg (rest args))
          (setf out (concatenate 'string out ", " arg)))
        out)))

(defun resource (path)
  (asdf:system-relative-pathname 'canebreak path))

(defun canebreak (&optional (in-filename (resource "src/test.sexp")) (out-filename (resource "src/test-generated.s")))
  (setf *indentation-level* 0)
  (with-open-file (out out-filename
                       :direction :output
                       :if-exists :supersede)
    (setf *out-file* out)
    (mapcar #'process (fouriclib:read-file in-filename))
    (setf *out-file* nil)))

(defun strcat (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun emit (object)
  (fouriclib:doitimes ((* *indentation-level* *indentation-spaces*))
    (format *out-file* " ")
    (format t " "))
  (format *out-file* "~a~%" object)
  (format t "~a~%" object)
  object)

(defun process (form)
  ;;(format t "~a~%" form)
  (etypecase form
    (string
     ;; if we get a string, just emit it; we use strings to just inline emit assembly that we haven't written transformers for yet
     form)
    (number
     ;; just emit numbers literally? feels like it should be #~a
     (format nil "#~a" form))
    (symbol
     (if (member form *registers*)
         (string-upcase form)
         (string-downcase form)))
    (list
     (let ((command-name (first form)))
       (cond
         ((member command-name *instructions*)
          (instruction form))
         ((member command-name *directives*)
          (directive form)))))))

(defun argument (form)
  (etypecase form
    (symbol
     (string-upcase form))
    (number
     (format nil "~a" form))
    (string
     form)))

(defun directive (form)
  (let ((name (first form)))
    (fouriclib:fn-case (first form) #'member
      ('(text end)
        (emit (strcat "." (string-downcase name))))
      ('(global type size)
        (emit (strcat "." (string-downcase name) " " (apply #'join-commas (mapcar #'string-downcase (rest form))))))
      ('(equ)
        (emit (strcat "." (string-downcase name) " " (apply #'join-commas (mapcar #'argument (rest form))))))
      ('(label)
        (emit (let ((label-name (nth 1 form)))
                (emit (concatenate 'string (string-downcase label-name) ":"))
                (let ((*indentation-level* (1+ *indentation-level*)))
                  (mapcar #'process (cddr form)))
                "")))
      ('(data)
        (emit ".data")
        (dolist (form (rest form))
          (let ((*indentation-level* (1+ *indentation-level*)))
            (emit (strcat (string-downcase (nth 0 form)) ": ." (string-upcase (nth 1 form)) " " (apply #'join-commas (mapcar #'string-downcase (rest (rest form))))))))))))

(defun instruction (form)
  "takes the *whole* form of an instruction"
  (check-type form list)
  (labels ((process-arg (arg)
             (etypecase arg
               (string arg)
               (symbol
                (if (member arg (append *instructions* *registers*))
                    (string-upcase arg)
                    (string-downcase arg)))
               (list (strcat "[" (apply #'strcat (mapcar #'process-arg arg)) "]")))))
    (emit (strcat (process-arg (first form)) " " (apply #'join-commas (mapcar #'process-arg (rest form)))))))

(defparameter *registers* '(r0 r1 r2 r3 r4 r5 r6 r7
                            r8 r9 r10 r11 r12 r13 r14 r15))
(defparameter *directives* '(text global type size equ label data text end))
(defparameter *instructions* '(ldr ldrh mov mul add subs str bne nop))
