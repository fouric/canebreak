(defparameter *out-file* nil)
(defparameter *indentation-level* 0)
(defparameter *indentation-spaces* 4)

(defun join-commas (&rest args)
  (if (zerop (length args))
      ""
      (let ((out (first args)))
	(dolist (arg (rest args))
	  (setf out (concatenate 'string out ", " arg)))
	out)))

(defun resource (path)
  (asdf:system-relative-pathname 'canebreak path))

(defun canebreak (in-filename out-filename)
  (setf *indentation-level* 0)
  (with-open-file (out out-filename
		       :direction :output
		       :if-exists :supersede)
    (setf *out-file* out)
    (mapcar (lambda (f) (process f t)) (read-file in-filename))
    (setf *out-file* nil)))

(defun strcat (&rest strings)
  (apply #'concatenate (cons 'string strings)))

(defun process (form &optional (emit nil))
  (format t "~a~%" form)
  (let ((retval
	 (etypecase form
	   (string
	    form)
	   (number
	    (format nil "~a" form))
	   (symbol
	    (if (member form *registers*)
		(string-downcase form)))
	   (list
	    (let ((command-name (first form)))
	      (cond 
		((eql command-name 'file)
		 (strcat ".file \"" (nth 1 form) "\""))
		((eql command-name 'intel-syntax)
		 (strcat ".intel_syntax " (string-downcase (nth 1 form))))
		((eql command-name 'text)
		 ".text")
		((or (eql command-name 'global) (eql command-name 'globl))
		 (strcat ".global " (string-downcase (nth 1 form))))
		((eql command-name 'type)
		 (strcat ".type " (string-downcase (nth 1 form)) ", " (string-downcase (nth 2 form))))
		((eql command-name 'section)
		 (let ((name (nth 1 form)))
		   (emit (concatenate 'string (string-downcase name) ":"))
		   (incf *indentation-level*)
		   (mapcar (lambda (f) (process f t)) (rest (rest form)))
		   (decf *indentation-level*)))
		((member (first form) *instructions*)
		 (apply #'strcat (append (list (string-downcase (first form)) " " (apply #'join-commas (mapcar #'process (rest form)))))))))))))
    (when emit
      (emit retval))
    retval))

(defparameter *registers* '(rax rbx rcx rdx rsi rdi rbp rsp r8 r9 r10 r11 r12 r13 r14 r15
			    eax ebx ecx edx esi edi edp esp))
(defparameter *instructions* '(push mov pop ret))

(defun emit (object)
  (doitimes ((* *indentation-level* *indentation-spaces*))
    (format *out-file* " "))
  (format *out-file* "~a~%" object)
  object)
