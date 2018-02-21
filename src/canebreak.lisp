(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/home/fouric/other-code/quicklisp/setup.lisp")
  (funcall (find-symbol "QUICKLOAD" 'ql) :fouriclib))

(defparameter *out-file* nil)
(defparameter *indentation-level* 0)
(defparameter *indentation-spaces* 4)
(defparameter *registers* '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 pc lr))
(defparameter *directives* '(text global type size equ label data text end))
(defparameter *instructions* '(ldr ldrh ldrb
                               str strh strb
                               bne beq ble bal b
                               add subs mul
                               and
                               mov
                               cmp tst
                               nop))


(defun join-commas (&rest args)
  "joins strings with commas and spaces"
  (if (zerop (length args))
      ""
      (let ((out (first args)))
        (dolist (arg (rest args))
          (setf out (concatenate 'string out ", " arg)))
        out)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun convert-symbol (symbol &optional upcase)
  "convert a symbol to an ARM-assembly-friendly string representation"
  (replace-all (funcall (if upcase #'string-upcase #'string-downcase) (if (symbolp symbol) symbol (format nil "~a" symbol))) "-" "_"))

(defun resource (path)
  "get the full pathname from a path relative to an ASDF system"
  (asdf:system-relative-pathname 'canebreak path))

(defun canebreak (&optional (in-filename (resource "src/mainline.sexp")) (out-filename (resource "src/out.s")))
  (setf *indentation-level* 0)
  (with-open-file (out out-filename
                       :direction :output
                       :if-exists :supersede)
    (setf *out-file* out)
    (mapcar #'process (fouriclib:read-file in-filename))
    (setf *out-file* nil)))

(defun strcat (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun emit (object &key (newline t) (indentation t))
  (when indentation
    (fouriclib:doitimes ((* *indentation-level* *indentation-spaces*))
      (format *out-file* " ")
      (format t " ")))
  (format *out-file* "~a" object)
  (format t "~a" object)
  (when newline
    (format *out-file* "~%")
    (format t "~%"))
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
          (directive form))
         (t
          (error "unknown thing: ~s" command-name)))))))

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
            (emit (strcat (convert-symbol (nth 0 form)) ": ") :newline nil)
            (if (member (nth 1 form) '(byte hword word))
                (emit (strcat "." (convert-symbol (nth 1 form)) " " (apply #'join-commas (mapcar #'convert-symbol (rest (rest form))))) :indentation nil)
                )))))))

(defun instruction (form)
  "takes the *whole* form of an instruction"
  (check-type form list)
  (labels ((process-arg (arg)
             (etypecase arg
               (string arg)
               (symbol
                (convert-symbol arg))
               (list
                (cond
                  ((member (first form) '(str strb strh ldr ldrh ldrb))
                   (strcat "[" (apply #'strcat (mapcar #'process-arg arg)) "]"))
                  (t
                   arg)))
               (number
                (format nil "#~a" arg)))))
    (emit (strcat (process-arg (first form)) " " (apply #'join-commas (mapcar #'process-arg (rest form)))))))

;;(canebreak "/home/fouric/code/ccs-workspace/playground/playground.sexp" "/home/fouric/code/ccs-workspace/playground/playground.s")
;;(sb-ext:exit)
