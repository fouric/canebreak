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

(defun canebreak (&optional (in-filename (resource "src/in.sexp")) (out-filename (resource "src/out.s")))
  (setf *indentation-level* 0)
  (with-open-file (out out-filename
                       :direction :output
                       :if-exists :supersede)
    (setf *out-file* out)
    (mapcar (lambda (f) (process f t)) (fouriclib:read-file in-filename))
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

(defun process (form &optional (emit nil))
  ;;(format t "~a~%" form)
  (let ((retval
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
                  (directive form))))))))
    (when emit
      (emit retval))
    retval))

(defun argument (form)
  (etypecase form
    (symbol
     (string-upcase form))
    (number
     (format nil "~a" form))
    (string
     form)))

(defun directive (form)
  (case (first form)
    (text
     ".text")
    (global
     (strcat ".global " (string-downcase (nth 1 form))))
    (type
     (strcat ".type " (apply #'join-commas (mapcar #'string-downcase (rest form)))))
    (size
     (strcat ".size " (apply #'join-commas (mapcar #'string-downcase (rest form)))))
    (equ
     (strcat ".equ " (apply #'join-commas (mapcar #'argument (rest form)))))
    (label
     (let ((label-name (nth 1 form)))
       (emit (concatenate 'string (string-downcase label-name) ":"))
       (let ((*indentation-level* (1+ *indentation-level*)))
         (mapcar (lambda (f) (process f t)) (cddr form)))
       ""))))

(defun instruction (form)
  "takes the *whole* form of an instruction"
  (check-type form list)
  (labels ((process-arg (arg)
             (etypecase arg
               (string arg)
               (symbol (string-upcase arg))
               (list (strcat "[" (apply #'strcat (mapcar #'process-arg arg)) "]")))))
    (strcat (process-arg (first form)) " " (apply #'join-commas (mapcar #'process-arg (rest form))))))

(defparameter *registers* '(r0 r1 r2 r3 r4 r5 r6 r7
                            r8 r9 r10 r11 r12 r13 r14 r15))
(defparameter *directives* '(text global type size equ label))
(defparameter *instructions* '(ldr ldrh mov))
