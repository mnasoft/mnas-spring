;;;; mnas-spring.lisp

(in-package #:mnas-spring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spring ()
  ((l-0 :accessor l-0 :initarg :l-0 :initform 30          :documentation "Длина пружины в свободном состоянии, мм")
   (d-m :accessor d-m :initarg :d-m :initform 10          :documentation "Средний диаметр пружины, мм")
   (d-w :accessor d-w :initarg :d-w :initform 2.5         :documentation "Диаметр проволоки, мм")
   (n-w :accessor n-w :initarg :n-w :initform 6           :documentation "Количество полных витков, 1")
   (m-w :accessor m-w :initarg :m-w :initform "12Х18Н10Т" :documentation "Материал из которого изготовлена проволока")
   )
  (:documentation "Представляет пружину сжатия"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spring-konstr-l0-f1  (spring)
  (
   (l-0-es :accessor l-0-es :initarg :l-0-es :initform  0.5 :documentation "Верхнее предельное отклонение поля допуска пружины на размер l-0,  мм")
   (l-0-ei :accessor l-0-ei :initarg :l-0-ei :initform -0.5 :documentation "Нижнее  предельное отклонение поля допуска пружины на размер l-0,  мм")
   
   (L1     :accessor L1     :initarg :L1     :initform  8.0 :documentation "Номинальная длина пружины под нагрузкой L1, мм")
   
   (f1     :accessor f1     :initarg :f1     :initform 11.5 :documentation "Номинальное значения усилия, развиваемое пружиной при деформации S, Н")
   (f1-es  :accessor f1-es  :initarg :f1-es  :initform  2.0 :documentation "Верхнее предельное отклонение поля допуска на усилие, развиваемое пружиной при деформации S, Н")
   (f1-ei  :accessor f1-ei  :initarg :f1-ei  :initform -2.0 :documentation "Нижнее  предельное отклонение поля допуска на усилие, развиваемое пружиной при деформации S, Н")
   )
  (:documentation "Представляет пружину сжатия с чертежными параметрами, заданными в виде:
1) Длины пружины в свободном состоянии;
2) Одного значения деформации пружины, для которого определяется усилие развиваемое пружиной;
3) Усилия, развиваемого пружиной при деформации S силы.
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;