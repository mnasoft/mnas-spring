;;;; mnas-spring-dialog-01.lisp

(in-package #:mnas-spring)

(defun-with-ltk-my spring-dialog-01
    ((bar     (make-instance 'frame :pack '(:side :top)))
     (col-1   (make-instance 'labelframe :master bar :text "Геометрия пружины" :pack '(:side :top :expand t :fill :x) ))
     (row-1-1 (make-instance 'frame :master col-1 :pack '(:side :top) ))
     (mk-group l-0 row-1-1 "L0, мм" (format nil "~A" (l-0 *s*)))
     (mk-group d-m row-1-1 "D, мм" (format nil "~A" (d-m *s*)))
     (mk-group d-w row-1-1 "d, мм" (format nil "~A" (d-w *s*)))
     (mk-group n-w row-1-1 "n, 1" (format nil "~A" (n-w *s*)))
     (mk-group m-w row-1-1 "Материал" (format nil "~A" (m-w *s*))))
  (labels
      ((get-e (key) (read-from-string (format nil "~a" (text key))))
       (put-e (key val) (setf (text key) (format nil "~,4f" val)))
       (ac-l-0 () (put-e l-0-entry (setf (l-0 *s*) (get-e l-0-entry))))
       (ac-d-m () (put-e d-m-entry (setf (d-m *s*) (get-e d-m-entry))))
       (ac-d-w () (put-e d-w-entry (setf (d-w *s*) (get-e d-w-entry))))
       (ac-n-w () (put-e n-w-entry (setf (n-w *s*) (get-e n-w-entry))))
       )
    (bind-entry-slot l-0-entry l-0 (ac-l-0))
    (bind-entry-slot d-m-entry d-m (ac-d-m))
    (bind-entry-slot d-w-entry d-w (ac-d-w))
    (bind-entry-slot n-w-entry n-w (ac-n-w))
    ))


(spring-dialog-01)

(PROGN
  (EVAL-WHEN (:COMPILE-TOPLEVEL) (SB-C:%COMPILER-DEFUN 'SPRING-DIALOG-01 T NIL NIL))
  (SB-IMPL::%DEFUN
   'SPRING-DIALOG-01
   (SB-INT:NAMED-LAMBDA SPRING-DIALOG-01 NIL
     (BLOCK SPRING-DIALOG-01
       (CALL-WITH-LTK
	(LAMBDA ()
          (LET* ((BAR       (MAKE-INSTANCE 'FRAME                                                                    :PACK '(:SIDE :TOP)))
                 (COL-1     (MAKE-INSTANCE 'LABELFRAME :MASTER BAR :TEXT "Геометрия пружины"                         :PACK '(:SIDE :TOP :EXPAND T :FILL :X)))
                 (ROW-1-1   (MAKE-INSTANCE 'FRAME      :MASTER COL-1                                                 :PACK '(:SIDE :TOP)))
                 (L-0-GROUP (MAKE-INSTANCE 'FRAME      :MASTER ROW-1-1                                               :PACK '(:SIDE :LEFT)))
                 (L-0-LABEL (MAKE-INSTANCE 'LABEL      :MASTER L-0-GROUP :TEXT "L0, мм"                              :PACK '(:SIDE :LEFT)))
                 (L-0-ENTRY (MAKE-INSTANCE 'ENTRY      :MASTER L-0-GROUP :WIDTH 10 :TEXT (FORMAT NIL "~A" (L-0 *S*)) :PACK '(:SIDE :LEFT)))
                 (D-M-GROUP (MAKE-INSTANCE 'FRAME      :MASTER ROW-1-1                                               :PACK '(:SIDE :LEFT)))
                 (D-M-LABEL (MAKE-INSTANCE 'LABEL      :MASTER D-M-GROUP :TEXT "D, мм"                               :PACK '(:SIDE :LEFT)))
                 (D-M-ENTRY (MAKE-INSTANCE 'ENTRY      :MASTER D-M-GROUP :WIDTH 10 :TEXT (FORMAT NIL "~A" (D-M *S*)) :PACK '(:SIDE :LEFT)))
                 (D-W-GROUP (MAKE-INSTANCE 'FRAME      :MASTER ROW-1-1                                               :PACK '(:SIDE :LEFT)))
                 (D-W-LABEL (MAKE-INSTANCE 'LABEL      :MASTER D-W-GROUP :TEXT "d, мм"                               :PACK '(:SIDE :LEFT)))
                 (D-W-ENTRY (MAKE-INSTANCE 'ENTRY      :MASTER D-W-GROUP :WIDTH 10 :TEXT (FORMAT NIL "~A" (D-W *S*)) :PACK '(:SIDE :LEFT)))
                 (N-W-GROUP (MAKE-INSTANCE 'FRAME      :MASTER ROW-1-1                                               :PACK '(:SIDE :LEFT)))
                 (N-W-LABEL (MAKE-INSTANCE 'LABEL      :MASTER N-W-GROUP :TEXT "n, 1"                                :PACK '(:SIDE :LEFT)))
                 (N-W-ENTRY (MAKE-INSTANCE 'ENTRY      :MASTER N-W-GROUP :WIDTH 10 :TEXT (FORMAT NIL "~A" (N-W *S*)) :PACK '(:SIDE :LEFT)))
		 (M-W-GROUP (MAKE-INSTANCE 'FRAME      :MASTER ROW-1-1                                               :PACK '(:SIDE :LEFT)))
                 (M-W-LABEL (MAKE-INSTANCE 'LABEL      :MASTER M-W-GROUP :TEXT "Материал"                            :PACK '(:SIDE :LEFT)))
                 (M-W-ENTRY (MAKE-INSTANCE 'ENTRY      :MASTER M-W-GROUP :WIDTH 10 :TEXT (FORMAT NIL "~A" (M-W *S*)) :PACK '(:SIDE :LEFT)))
		 )
            (LABELS ((GET-E (KEY) (READ-FROM-STRING (FORMAT NIL "~a" (TEXT KEY)))) (PUT-E (KEY VAL) (LET* ((#:KEY542 KEY) (#:NEW1 (FORMAT NIL "~,4f" VAL))) (FUNCALL #'(SETF TEXT) #:NEW1 #:KEY542)))
                     (AC-L-0 () (PUT-E L-0-ENTRY (LET* ((#:*S*543 *S*) (#:NEW1 (GET-E L-0-ENTRY))) (FUNCALL #'(SETF L-0) #:NEW1 #:*S*543))))
                     (AC-D-M () (PUT-E D-M-ENTRY (LET* ((#:*S*544 *S*) (#:NEW1 (GET-E D-M-ENTRY))) (FUNCALL #'(SETF D-M) #:NEW1 #:*S*544))))
                     (AC-D-W () (PUT-E D-W-ENTRY (LET* ((#:*S*545 *S*) (#:NEW1 (GET-E D-W-ENTRY))) (FUNCALL #'(SETF D-W) #:NEW1 #:*S*545))))
                     (AC-N-W () (PUT-E N-W-ENTRY (LET* ((#:*S*546 *S*) (#:NEW1 (GET-E N-W-ENTRY))) (FUNCALL #'(SETF N-W) #:NEW1 #:*S*546)))))
              (BIND L-0-ENTRY "<Return>"
		    #'(LAMBDA (EVENT)
			(DECLARE (IGNORE EVENT))
			(LET* ((#:*S*547 *S*) (#:NEW1 (READ-FROM-STRING (FORMAT NIL "~a" (TEXT L-0-ENTRY))))) (FUNCALL #'(SETF L-0) #:NEW1 #:*S*547))
			(LET* ((#:L-0-ENTRY548 L-0-ENTRY) (#:NEW1 (FORMAT NIL "~a" (L-0 *S*)))) (FUNCALL #'(SETF TEXT) #:NEW1 #:L-0-ENTRY548)) (AC-L-0)))
              (BIND D-M-ENTRY "<Return>"
                    #'(LAMBDA (EVENT)
			(DECLARE (IGNORE EVENT))
			(LET* ((#:*S*549 *S*)
                               (#:NEW1
				(READ-FROM-STRING
                                 (FORMAT NIL "~a"
                                         (TEXT D-M-ENTRY)))))
                          (FUNCALL #'(SETF D-M) #:NEW1
                                   #:*S*549))
			(LET* ((#:D-M-ENTRY550 D-M-ENTRY)
                               (#:NEW1
				(FORMAT NIL "~a" (D-M *S*))))
                          (FUNCALL #'(SETF TEXT) #:NEW1
                                   #:D-M-ENTRY550))
			(AC-D-M)))
              (BIND D-W-ENTRY "<Return>" #'(LAMBDA (EVENT) (DECLARE (IGNORE EVENT))
						   (LET* ((#:*S*551 *S*) (#:NEW1 (READ-FROM-STRING (FORMAT NIL "~a" (TEXT D-W-ENTRY))))) (FUNCALL #'(SETF D-W) #:NEW1 #:*S*551))
						   (LET* ((#:D-W-ENTRY552 D-W-ENTRY) (#:NEW1 (FORMAT NIL "~a" (D-W *S*)))) (FUNCALL #'(SETF TEXT) #:NEW1 #:D-W-ENTRY552)) (AC-D-W)))
              (BIND N-W-ENTRY "<Return>" #'(LAMBDA (EVENT)
					     (DECLARE (IGNORE EVENT))
					     (LET* ((#:*S*553 *S*) (#:NEW1 (READ-FROM-STRING (FORMAT NIL "~a" (TEXT N-W-ENTRY))))) (FUNCALL #'(SETF N-W) #:NEW1 #:*S*553))
					     (LET* ((#:N-W-ENTRY554 N-W-ENTRY) (#:NEW1 (FORMAT NIL "~a" (N-W *S*)))) (FUNCALL #'(SETF TEXT) #:NEW1 #:N-W-ENTRY554)) (AC-N-W)))))))))))

