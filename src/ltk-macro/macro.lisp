;;;; macro.lisp

(defpackage :mnas-spring/ltk-macro
  (:use #:cl #:mnas-spring #:ltk) ;;;; #:mnas-logical
  (:export mk-group
           bind-entry-slot
           bind-entry-set
           bind-entry-txt
           defun-with-ltk-my))

(in-package :mnas-spring/ltk-macro)

(defmacro mk-group (entry parent lbl-str entry-txt &body body)
  `((,(read-from-string (format nil "~s~s" entry '-group))
      (make-instance 'frame :master ,parent))
    (,(read-from-string (format nil "~s~s" entry '-label))
      (make-instance 'label :master ,(read-from-string (format nil "~s~s" entry '-group)) :text ,lbl-str))
    (,(read-from-string (format nil "~s~s" entry '-entry))
      (make-instance 'entry :master ,(read-from-string (format nil "~s~s" entry '-group)) :width 5 :text ,entry-txt))
    ,@body))

;;;; (mk-group d-w row-1-1 "d, мм" (format nil "~A" (d-w *s*)) (D-W-razm (MAKE-INSTANCE 'label :MASTER D-W-GROUP :WIDTH 5 :TEXT (FORMAT NIL "~A" (D-W *S*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro bind-entry-slot (entry spr-func &body body)
  `(bind ,entry "<Return>"
	 #'(lambda (event)
	     (declare (ignore event))
	     (setf (,spr-func *s*) (read-from-string (format nil "~a" (text ,entry))))
	     (setf (text ,entry) (format nil "~a" (,spr-func *s*)))
	     ,@body)))

;;;; (bind-entry-slot n-w-entry n-w (setf (text n-f) (format nil "~a" (n-f *s*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro bind-entry-set (entry spr-func &body body)
  `(bind ,entry "<Return>"
	 #'(lambda (event)
	     (declare (ignore event))
	     (,(read-from-string (format nil "~s~s" 'set- spr-func)) *s* (read-from-string (format nil "~a" (text ,entry))))
	     (setf (text ,entry) (format nil "~a" (,spr-func *s*)))
	     ,@body)))

;;;; (bind-entry-set n-f-entry n-f (setf (text n-w-entry) (format nil "~a" (n-w *s*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro bind-entry-txt (entry &body body)
  `(bind ,entry "<Return>"
	 #'(lambda (event)
	     (declare (ignore event))
	     (setf (text ,entry) (format nil "~a" (read-from-string (format nil "~a" (text ,entry)))))
	     ,@body)))

;;;; (bind-entry-txt s-1-entry (ac-s-1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defun-with-ltk-my (&whole whole func-name let-lst &body body)
  (declare (ignore whole))
  `(defun ,func-name ()
     (with-ltk ()
       (let*
	   ,(let ((rez nil)
		  (temp nil))
		 (dolist (var let-lst)
		   (cond
		     ((eq (car var) 'mk-group)
		      (setf temp nil)
		      (setf temp (cons (read-from-string (format nil "~s~s" (second var) '-group)) nil))
		      (setf temp (append temp (list (list 'make-instance ''frame :master (third var) :pack ''(:side :left) ))))
		      (setf rez (cons temp rez))
		      (setf temp nil)
		      (setf temp (cons (read-from-string (format nil "~s~s" (second var) '-label)) nil))
		      (setf temp (append temp (list (list 'make-instance ''label :master (read-from-string (format nil "~s~s" (second var) '-group)) :text (fourth var) :pack ''(:side :left) ))))
		      (setf rez (cons temp rez))
		      (setf temp nil)
		      (setf temp (cons (read-from-string (format nil "~s~s" (second var) '-entry)) nil))
		      (setf temp (append temp (list (list 'make-instance ''entry :master (read-from-string (format nil "~s~s" (second var) '-group)) :width  10 :text (fifth var) :pack ''(:side :left) ))))
		      (setf rez (cons temp rez))
		      (dolist (var-var (cddddr(cdr var)))
			(setf rez (cons var-var rez))))
		     ((and (listp var) (atom (first var)))
		      (setf rez (cons var rez)))
		     ((and (listp var) (and-func (mapcar #'(lambda (el) (atom (first el))) var)))
		      (setf rez (append (reverse var) rez)))))
		 (reverse rez))
	 ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
