;;;; mnas-spring.lisp

(defpackage #:mnas-spring/db
  (:use #:cl #:mnas-spring))

(in-package #:mnas-spring/db)

(defparameter *a*
  (make-instance '<spring-konstr-l0-f1>
		 :n-w     14
		 :d-m     2.6
		 :d-w     0.2
		 :l-0     14
		 :l-0-es  0.5
		 :l-0-ei -0.5
		 :L1      8.0
		 :f1      0.5
		 :f1-es   0.05
		 :f1-ei  -0.05
		 :m-w    "1 класс")
  "890030007_V0")

(defparameter *05303214_V6*
  (make-instance '<spring-konstr-l0-f1>
		 :n-w     14
		 :d-m     2.6
		 :d-w     0.2
		 :l-0     14
		 :l-0-es  0.5
		 :l-0-ei -0.5
		 :L1      7.5
		 :f1      0.4
		 :f1-es   0.04
		 :f1-ei  -0.04
		 :m-w    "1 класс"))


(defparameter *i*
  (make-instance '<spring-konstr-l0-f1>
		 :n-w     14
		 :d-m     2.6
		 :d-w     0.2
		 :l-0     14
		 :l-0-es -0.5
		 :l-0-ei -0.5
		 :L1      8.0
		 :f1      0.37
		 :f1-es   0.00
		 :f1-ei  -0.00
		 :m-w    "1 класс")
  "Клапаны изготовления ИРАН")

(loop :for i :from 0 :to 14.5 :by 0.5
   :collect (list i (fi-min-li *a* i) (fi-max-li *a* i)))

(progn
  (defparameter *l-nom*     7.5424004)
  (defparameter *l-nom-es*  0.84430003)
  (defparameter *l-nom-ei* -0.7789001)
  (defparameter *f-sphire*  3.5342917352885173)
  )

(/ (fi-min-li *a* (+ *l-nom* *l-nom-es*)) *f-sphire*)

(/ (fi-max-li *a* (+ *l-nom* *l-nom-ei*)) *f-sphire*)

(/ (fi-min-li *i* (+ *l-nom* *l-nom-es*)) *f-sphire*)

(/ (fi-max-li *i* (+ *l-nom* *l-nom-ei*)) *f-sphire*)

(/ (fi-min-li *05303214_V6* (+ *l-nom* *l-nom-es*)) *f-sphire*)

(/ (fi-max-li *05303214_V6* (+ *l-nom* *l-nom-ei*)) *f-sphire*)
