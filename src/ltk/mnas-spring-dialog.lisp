;;;; mnas-spring-dialog.lisp

(defpackage #:mnas-spring/ltk
  (:use #:cl #:mnas-spring  #:mnas-logical #:ltk #:mnas-spring/ltk-macro)
  (:export spring-dialog
           spring-dialog-01))

(in-package #:mnas-spring/ltk)

(defun-with-ltk-my spring-dialog
    ((bar     (make-instance 'frame :pack '(:side :top)))
     (col-1   (make-instance 'labelframe :master bar :text "Геометрия пружины" :pack '(:side :top :expand t :fill :x) ))
     (row-1-1 (make-instance 'frame :master col-1 :pack '(:side :top) ))
     (mk-group d-w row-1-1 "d, мм" (format nil "~A" (d-w *s*)))
     (mk-group d-o row-1-1 "D1, мм" (format nil "~A" (d-o *s*)))
     (mk-group d-m row-1-1 "D, мм" (format nil "~A" (d-m *s*)))
     (row-1-2 (make-instance 'frame :master col-1 :pack '(:side :top) ))
     (mk-group n-w row-1-2 "n, 1" (format nil "~A" (n-w *s*)))
     (mk-group n-f row-1-2 "n1, 1" (format nil "~A" (n-f *s*)))
     (row-1-3 (make-instance 'frame :master col-1 :pack '(:side :top) ))
     (mk-group t-s row-1-3 "t, мм"  (format nil "~A" (t-s *s*)))
     (mk-group l-0 row-1-3 "L0, мм"  (format nil "~A" (l-0 *s*)))
     (mk-group l-4 row-1-3 "L4, мм"  (format nil "~A" (l-4 *s*)))
     (col-2   (make-instance 'labelframe :master bar :text "Х-ки материала" :pack '(:side :top :expand t :fill :x) ))
     (row-2-1 (make-instance 'frame :master col-2 :pack '(:side :top) ))
     (mk-group G[MPa] row-2-1 "G, МПа" (format nil "~A" (G[MPa] *s*)))
     (mk-group [tau] row-2-1 "[tau], МПа" "0")
     (col-3   (make-instance 'labelframe :master bar :text "Х-ки пружины" :pack '(:side :top :expand t :fill :x)))
     (row-3-1 (make-instance 'frame :master col-3 :pack '(:side :top) ))
     (mk-group l-1 row-3-1 "L1, мм" (format nil "~A" (l-0 *s*)))
     (mk-group s-1 row-3-1 "S1, мм" "0")
     (mk-group f-1 row-3-1 "F1, Н" "0")
     (mk-group tau1 row-3-1 "tau1, МПа" "0")
     (row-3-2 (make-instance 'frame :master col-3 :pack '(:side :top) ))
     (mk-group l-2  row-3-2 "L2, мм" (format nil "~A" (* 1/2 (+ (l-0 *s*) (l-4 *s*)))))
     (mk-group s-2  row-3-2 "S2, мм"     "0")
     (mk-group f-2  row-3-2 "F2, Н"      "0")
     (mk-group tau2 row-3-2 "tau2, МПа" "0")
     (row-3-3 (make-instance 'frame :master col-3 :pack '(:side :top) ))
     (mk-group l-3  row-3-3 "L3, мм" (format nil "~A" (l-4 *s*)))
     (mk-group s-3  row-3-3 "S3, мм"     "0")
     (mk-group f-3  row-3-3 "F3, Н"      "0")
     (mk-group tau3 row-3-3 "tau3, МПа" "0")
     (row-3-4 (make-instance 'frame :master col-3 :pack '(:side :top) ))
     (mk-group h-s row-3-4 "h, мм" "0")
     (mk-group c-s row-3-4 "C, Н/мм" "0")
     (col-4   (make-instance 'labelframe :master bar :text "Некоторые коэффициенты" :pack '(:side :top :expand t :fill :x)))
     (row-4-1 (make-instance 'frame :master col-4 :pack '(:side :top) ))
     (mk-group k-1 row-4-1 "K1, 1" (format nil "~A" (k-1 *s*)))
     (mk-group k-2 row-4-1 "K2, 1" (format nil "~A" (k-2 *s*)))
     (mk-group i-s row-4-1 "i, 1"  (format nil "~A" (i-s *s*)))
     (mk-group i-1 row-4-1 "i1, 1"  "0")
     (row-4-2 (make-instance 'frame :master col-4 :pack '(:side :top) ))
     (mk-group l-w row-4-2 "Lw, мм" (format nil "~A" (l-w *s*)))
     (mk-group mass-s row-4-2 "m, кг" (format nil "~A" (mass-s *s*)))
     (mk-group s-k row-4-2 "Sk, мм" "0"))
  (labels
      ((get-e (key) (read-from-string (format nil "~a" (text key))))
       (put-e (key val) (setf (text key) (format nil "~,4f" val)))
       (d-m-e () (put-e d-m-entry (- (get-e d-o-entry) (get-e d-w-entry))))
       (d-o-e () (put-e d-o-entry (+ (get-e d-m-entry) (get-e d-w-entry))))
       (d-w-e () (put-e d-w-entry (- (get-e d-o-entry) (get-e d-m-entry))))
       (i-s-e () (put-e i-s-entry (/ (get-e d-m-entry) (get-e d-w-entry))))
       (i-1-e () (put-e i-1-entry (/ (get-e l-0-entry) (get-e d-m-entry))))
       (l-0-e () (put-e l-0-entry (+ (* (get-e n-w-entry) (get-e t-s-entry)) (* 1.5 (get-e d-w-entry)))))
       (l-1-e () (put-e l-1-entry (- (get-e l-0-entry) (get-e s-1-entry))))
       (l-2-e () (put-e l-2-entry (- (get-e l-0-entry) (get-e s-2-entry))))
       (l-3-e () (put-e l-3-entry (- (get-e l-0-entry) (get-e s-3-entry))))
       (l-4-e () (put-e l-4-entry (* (+ (get-e n-w-entry) 1.5) (get-e d-w-entry))))
       (s-1-e () (put-e s-1-entry (- (get-e l-0-entry) (get-e l-1-entry))))
       (s-2-e () (put-e s-2-entry (- (get-e l-0-entry) (get-e l-2-entry))))
       (s-3-e () (put-e s-3-entry (- (get-e l-0-entry) (get-e l-3-entry))))
       (l-w-e () (put-e l-w-entry (* (get-e n-f-entry) (sqrt (+ (* pi pi (get-e d-m-entry) (get-e d-m-entry)) (* (get-e t-s-entry)(get-e t-s-entry)))))))
       (n-w-e () (put-e n-w-entry (- (get-e n-f-entry) 2.0)))
       (n-f-e () (put-e n-f-entry (+ (get-e n-w-entry) 2.0)))
       (t-s-e () (put-e t-s-entry (/ (- (get-e l-0-entry) (* 1.5 (get-e d-w-entry))) (get-e n-w-entry))))
       (h-s-e () (put-e h-s-entry (- (get-e s-2-entry) (get-e s-1-entry))))
       (k-1-e () (put-e k-1-entry (+ (/ (- (* 4.0 (get-e i-s-entry)) 1.0) (- (* 4.0 (get-e i-s-entry)) 4.0)) (/ 0.615 (get-e i-s-entry)))))
       (k-2-e () (put-e k-2-entry (+ 1.0 (/ 1.0 2.0 (get-e i-s-entry)) (/ -1.0 2.0 (get-e i-s-entry) (get-e i-s-entry)))))
       (f-1-e () (put-e f-1-entry (/ (* (get-e G[MPa]-entry) (get-e d-w-entry) (get-e d-w-entry) (get-e d-w-entry) (get-e d-w-entry) (get-e s-1-entry))
				     8.0 (get-e d-m-entry)(get-e d-m-entry)(get-e d-m-entry) (get-e n-w-entry) (get-e k-2-entry))))
       (f-2-e () (put-e f-2-entry (/ (* (get-e G[MPa]-entry) (get-e d-w-entry) (get-e d-w-entry) (get-e d-w-entry) (get-e d-w-entry) (get-e s-2-entry))
				     8.0 (get-e d-m-entry) (get-e d-m-entry) (get-e d-m-entry) (get-e n-w-entry) (get-e k-2-entry))))
       (f-3-e () (put-e f-3-entry (/ (* (get-e G[MPa]-entry) (get-e d-w-entry) (get-e d-w-entry) (get-e d-w-entry) (get-e d-w-entry) (get-e s-3-entry))
				     8.0 (get-e d-m-entry) (get-e d-m-entry) (get-e d-m-entry) (get-e n-w-entry) (get-e k-2-entry))))
       (tau1-e () (put-e tau1-entry (/ (* 2.55 (get-e f-1-entry) (get-e d-m-entry) (get-e k-1-entry)) (* (get-e d-w-entry) (get-e d-w-entry) (get-e d-w-entry)))))
       (tau2-e () (put-e tau2-entry (/ (* 2.55 (get-e f-2-entry) (get-e d-m-entry) (get-e k-1-entry)) (* (get-e d-w-entry) (get-e d-w-entry) (get-e d-w-entry)))))
       (tau3-e () (put-e tau3-entry (/ (* 2.55 (get-e f-3-entry) (get-e d-m-entry) (get-e k-1-entry)) (* (get-e d-w-entry)(get-e d-w-entry)(get-e d-w-entry)))))
       (c-s-e () (put-e c-s-entry (/ (- (get-e f-2-entry) (get-e f-1-entry)) (get-e h-s-entry))))
       (mass-s-e () (put-e mass-s-entry (* 8.0e-3 pi (get-e d-w-entry) (get-e d-w-entry) 0.25 (get-e l-w-entry) (/ (- (get-e n-f-entry) 0.5) (get-e n-f-entry)))))
       (ac-l-0 () (t-s-e) (i-1-e) (l-w-e) (l-1-e) (l-2-e) (l-3-e) (s-1-e) (s-2-e) (s-3-e) (h-s-e) (f-1-e) (f-2-e) (f-3-e) (tau1-e) (tau2-e) (tau3-e) (c-s-e) (mass-s-e) )
       (ac-d-w () (t-s-e) (l-4-e) (d-o-e) (d-m-e) (i-1-e) (l-w-e) (i-s-e) (k-2-e) (f-1-e) (f-2-e) (f-3-e) (k-1-e) (tau1-e) (tau2-e) (tau3-e) (c-s-e) (mass-s-e))
       (ac-d-m () (d-o-e) (i-1-e) (l-w-e) (i-s-e) (k-2-e) (f-1-e) (f-2-e) (f-3-e) (k-1-e) (tau1-e) (tau2-e) (tau3-e) (c-s-e) (mass-s-e) )
       (ac-n-w () (n-f-e) (l-w-e) (l-0-e) (t-s-e) (i-1-e) (l-w-e) (l-1-e) (l-2-e) (l-3-e) (s-1-e) (s-2-e) (s-3-e) (h-s-e) (f-1-e) (f-2-e) (f-3-e) (tau1-e) (tau2-e) (tau3-e) (c-s-e) (mass-s-e) )
       (ac-d-o () (d-m-e) (i-1-e) (l-w-e) (i-s-e) (k-2-e) (f-1-e) (f-2-e) (f-3-e) (k-1-e) (tau1-e) (tau2-e) (tau3-e) (c-s-e) (mass-s-e) )
       (ac-n-f () (n-w-e) (l-w-e) (l-0-e) (t-s-e) (i-1-e) (l-w-e) (l-1-e) (l-2-e) (l-3-e) (s-1-e) (s-2-e) (s-3-e) (h-s-e) (f-1-e) (f-2-e) (f-3-e) (tau1-e) (tau2-e) (tau3-e) (c-s-e) (mass-s-e) )
       (ac-t-s () (l-w-e) (l-0-e) (t-s-e) (i-1-e) (l-w-e) (l-1-e) (l-2-e) (l-3-e) (s-1-e) (s-2-e) (s-3-e) (h-s-e) (f-1-e) (f-2-e) (f-3-e) (tau1-e) (tau2-e) (tau3-e) (c-s-e) (mass-s-e) )
       (ac-s-1 () (l-1-e) (h-s-e) (f-1-e) (tau1-e) (c-s-e) (mass-s-e) )
       (ac-l-1 () (s-1-e) (h-s-e) (f-1-e) (tau1-e) (c-s-e) (mass-s-e) )
       (ac-s-2 () (l-2-e) (h-s-e) (f-2-e) (tau2-e) (c-s-e) (mass-s-e) )
       (ac-l-2 () (s-2-e) (h-s-e) (f-2-e) (tau2-e) (c-s-e) (mass-s-e) )
       (ac-s-3 () (l-3-e) (f-3-e) (tau3-e) )
       (ac-l-3 () (s-3-e) (f-3-e) (tau3-e) )
       (ac-g   () (f-1-e) (f-2-e) (f-3-e) (tau1-e) (tau2-e) (tau3-e) (c-s-e) (mass-s-e)))
    (bind-entry-slot l-0-entry l-0 (ac-l-0))	 ;;;; l-0-entry
    (bind-entry-slot d-w-entry d-w (ac-d-w))	 ;;;; d-w-entry
    (bind-entry-slot d-m-entry d-m (ac-d-m))	 ;;;; d-m-entry
    (bind-entry-slot n-w-entry n-w (ac-n-w))	 ;;;; n-w-entry
;;;;
    (bind-entry-set d-o-entry d-o (ac-d-o))	;;;; d-o-entry
    (bind-entry-set n-f-entry n-f (ac-n-f))	;;;; n-f-entry
    (bind-entry-set t-s-entry t-s (ac-t-s))	;;;; t-s-entry
;;;;
    (bind-entry-txt l-1-entry (ac-l-1))	    ;;;; l-1-entry
    (bind-entry-txt s-1-entry (ac-s-1))	    ;;;; s-1-entry
    (bind-entry-txt s-2-entry (ac-s-2))	    ;;;; s-2-entry
    (bind-entry-txt l-2-entry (ac-l-2))	    ;;;; l-2-entry
    (bind-entry-txt s-3-entry (ac-s-3))	    ;;;; s-3-entry
    (bind-entry-txt l-3-entry (ac-l-3))	    ;;;; l-3-entry
;;;;	(bind-entry-set g[mpa]-entry g[mpa] (ac-g)) ;;;; g[mpa]-entry
;;;;
;;;;	[tau]-entry f-1-entry tau1-entry f-2-entry tau2-entry f-3-entry tau3-entry h-s-entry c-s-entry k-1-entry k-2-entry i-s-entry i-1-entry l-w-entry mass-s-entry s-k-entry l-4-entry
    )
  )

;;;; (spring-dialog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
