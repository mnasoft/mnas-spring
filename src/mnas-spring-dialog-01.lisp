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
    (bind-entry-slot n-w-entry n-w (ac-n-w))))


;;;; (spring-dialog-01)
