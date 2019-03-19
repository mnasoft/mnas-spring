;;;; mnas-spring.lisp

(in-package #:mnas-spring)

(defparameter *a*
  (make-instance 'spring-konstr-l0-f1
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
  (make-instance 'spring-konstr-l0-f1
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
  (make-instance 'spring-konstr-l0-f1
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

(defmethod fi-min-li ((spr spring-konstr-l0-f1) X)
  "Сила, необходимая для придания пружине длины X
Пример использования:
;;;; (fi-si *s* 5)
"
  (cond
    ((>= X (L1 spr))
     (* (/ 
	 (- (+ (l-0 spr) (l-0-ei spr)) X )
	 (- (+ (l-0 spr) (l-0-ei spr)) (L1 spr)))
	(+ (f1 spr) (f1-ei spr))))
    ((<= X (L1 spr))
     (* (/ 
	 (- (+ (l-0 spr) (l-0-es spr) ) X )
	 (- (+ (l-0 spr) (l-0-es spr) ) (L1 spr)))
	(+ (f1 spr) (f1-ei spr))))))

(defmethod fi-max-li ((spr spring-konstr-l0-f1) X)
  "Сила, необходимая для придания пружине длины X
Пример использования:
;;;; (fi-si *s* 5)
"
  (cond
    ((<= X (L1 spr))
     (* (/ 
	 (- (+ (l-0 spr) (l-0-ei spr) ) X )
	 (- (+ (l-0 spr) (l-0-ei spr) ) (L1 spr)))
	(+ (f1 spr) (f1-es spr))))
    ((>= X (L1 spr))
     (* (/ 
	 (- (+ (l-0 spr) (l-0-es spr)) X )
	 (- (+ (l-0 spr) (l-0-es spr)) (L1 spr)))
	(+ (f1 spr) (f1-es spr))))))


(defmethod fi-li ((spr spring-konstr-l0-f1) X)
  (* 1/2 (+ (fi-max-li spr X) (fi-min-li spr X)))
  )

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


