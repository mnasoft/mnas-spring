;;;; mnas-spring.lisp

(in-package #:mnas-spring)

;;; "mnas-spring" goes here. Hacks and glory await!

(defclass mnas-spring () 
  ((d-outer        :accessor d-outer     :initarg :d-outer     :initform 0.024       :documentation "Наружный диаметр пружины, м")
   (d-wire         :accessor d-wire      :initarg :d-wire      :initform 0.0016      :documentation "Наружный диаметр проволоки, м")
    (pitch         :accessor pitch       :initarg :pitch       :initform 0.0035      :documentation "Шаг проволоки, м")
    (coil-number   :accessor coil-number :initarg :coil-number :initform 4           :documentation "Количество рабочих витков пружины")
    (material      :accessor material    :initarg :material    :initform "12Х18Н10Т" :documentation "Материал проволоки, м"))
  (:documentation "Представляет из себя цилиндричекую пружину сжатия."))

(defmethod print-object :before ((x mnas-spring)s) 
(format s "#mnas-spring(DO=~A; DW=~A; P=~A; CN=~A)" (d-outer x) (d-wire x) (pitch x) (coil-number x)))

(defgeneric d-middle (spring)
  (:documentation "Возвращает средний диаметр пружины."))

(defmethod d-middle ((spr mnas-spring ))  (- (d-outer spr) (d-wire spr)))

(defmethod coil-number-full ((spr mnas-spring)) 
"Полное количество витков"  
(+ (coil-number spr) 2) )

(defmethod length-free ((spr mnas-spring))
"Длина пружины в свободном состоянии"
 (+ (* (pitch spr) (coil-number spr)) (* 3/2 (d-wire spr))))

(defmethod length-tight ((spr mnas-spring))
"Длина пружины при соприкосновении витков"
 (* (+ 3/2 (coil-number spr)) (d-wire spr)))

(defmethod lenght-wire ((spr mnas-spring))
"Длина проволоки развернутой пружины"
  (* (coil-number-full spr) 
     (sqrt (+ 
	    (* pi pi (d-middle spr) (d-middle spr)) 
	    (* (pitch spr) (pitch spr))))))

(defmethod mass ((spr mnas-spring))
  "Длина проволоки развернутой пружины"
  (* 8000.0
     pi 
     (d-wire spr) 
     (d-wire spr) 
     (lenght-wire spr)
     (/ (- (coil-number-full spr) 1/2 ) (coil-number-full spr))))

(/ (- (coil-number-full *s*) 1/2 ) (coil-number-full *s*))

(lenght-wire *s*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *s* (make-instance 'mnas-spring :coil-number 6 :d-outer (- 0.0135 0.0016) :d-wire 0.0016 :pitch 0.0035))

(setf 
 (coil-number *s*) 4)

(coil-number-full  *s*)

(length-free       *s*)

(length-tight      *s*)

(d-middle          *s*)

(lenght-wire       *s*)

(mass              *s*)

