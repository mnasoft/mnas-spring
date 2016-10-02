;;;; class-spring.lisp

(in-package #:mnas-spring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; d-o		-	наружный диаметр пружины;
;;; d-m 	-	средний диаметр пружины;
;;; d-w 	-	диаметр проволоки;
;;; l-0		-	Высота пружины в свободном состоянии;
;;; l_1		-	
;;; l_2		-	
;;; n		-	число рабочих витков пружины;
;;; n-f		-	полное число витков пружины;
;;; l-w 	-	длина проволоки развернутой пружины;
;;; t_пр	-	шаг пружины в свободном состоянии;
;;; h_пр	-	рабочий ход пружины;
;;; ro_пр	-	плотность материала пружины;
;;; m_пр	-	масса пружины;
;;; l_i		-	Высота пружины под нагрузкой;

(defparameter *G-lst*
   '(("1 класс"         78500e6  50 20 0.0)
     ("60С2А"           78500e6 250 20 0.1)
     ("65Г"             78500e6  50 20 0.0)
     ("51ХФА"           81400e6 250 20 0.1)
     ("10Х11Н23Т3МР-ВД" 72600e6 600 20 0.3)
     ("10Х15Н27Т3МР-ВД" 72600e6 600 20 0.3)
     ("12Х18Н10Т"       68600e6 250 20 0.1))
    "Список значений модулей сдвига некоторых материалов:
Каждый элемент списка состоит из
1 - строки, обозначающей марку материала;
2 - значение модуля сдвига материала при температуре 20 град С, [Па]
3 - максимальная температура применения пружины из проволоки данного материала [C],
4 - нормальная температута [C],
5 - коэффициент уменьшения модуля сдвига при увеличении температуры от нормальной до максимальной")

(defun G (matirial-string &optional (temperature 20))
  "Возвращает значение модуля сдвига - модуль Юнга второго рода
Пример использования:
(G \"12Х18Н10Т\")"
  (let* ((data (assoc matirial-string *G-lst* :test #'string=))
	 (G_nor (second data))
	 (t_max (third  data))
	 (t_nor (fourth data))
	 (koeff (fifth  data)))
    (* G_nor (- 1.0 (* koeff (/ (- temperature t_nor) (- t_max t_nor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spring ()
  ((l-0 :accessor l-0 :initarg :l-0 :initform 30  :documentation "Длина пружины в свободном состоянии, мм")
   (d-m :accessor d-m :initarg :d-m :initform 10  :documentation "Средний диаметр пружины, мм")
   (d-w :accessor d-w :initarg :d-w :initform 2.5 :documentation "Диаметр проволоки, мм")
   (n-w :accessor n-w :initarg :n-w   :initform 6   :documentation "Количество полных витков, 1")
   (m-w :accessor m-w :initarg :m-w :initform "12Х18Н10Т" :documentation "Материал из которого изготовлена проволока")
   )
  (:documentation "Представляет пружину сжатия"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *s* (make-instance 'spring))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((x spring) s) (format s "#spring" ))

(defmethod print-object         ((x spring) s) (format s "(l-0=~S d-m=~S d-w=~S n-w=~S m-w=~S)" (l-0 x) (d-m x) (d-w x) (n-w x) (m-w x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric i-s (spring) (:documentation "Индекс пружины"))

(defmethod i-s ((spr spring))
  "Индекс пружины;
Пример использования:
Индекс пружины
;;;; (i-s *s*) => 4.0"
  (/ (d-m spr) (d-w spr)))
    
(defgeneric k-1 (spring) (:documentation "k-1 - Коэффициент прочности пружины"))

(defmethod k-1 ((spr spring))
  "Коэффициент прочности пружины
Пример использования:
;;;; (k-1 *s*)
;;;;=> 1.40375"
  (let ((i (i-s spr))) (+ (/ (- (* 4 i) 1) (- (* 4 i) 4)) (/ 615/1000 i))))

(defgeneric k-2 (spring)
  (:documentation "k-2 - Коэффициент жесткости пружины"))

(defmethod k-2 ((spr spring))
  "k-2 - Коэффициент жесткости пружины
Пример использования:
;;;; (k-2 *s*)
;;;; => 1.09375"
  (let ((i (i-s spr))) (+ 1 (/ 1 2 i) (/ -1 2 i i))))

(defgeneric n-f (spring) (:documentation "Количество полных витков пружины"))

(defmethod n-f ((spr spring))
  "Пример использования
;;;; (n-f *s*)
;;;; => 8"
  (+ (n-w spr) 2))

(defgeneric set-n-f (spring n-f) (:documentation "Устанавливает полное число витков пружины равным n-f за счёт изменения числа рабочих витков пружины"))

(defmethod set-n-f ((spr spring) n-f)
  "Устанавливает полное число витков пружины равным n-f за счёт изменения числа рабочих витков пружины;
Пример использования:
;;;; *s* => #spring(l-0=30.0 d-m=10 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
;;;; (set-n-f *s* 12) => 10
;;;; *s* => #spring(l-0=30.0 d-m=10 d-w=2.5 n-w=10 m-w=\"12Х18Н10Т\")
;;;; (set-n-f *s* 8) => 6"
  (setf (n-w spr) (- n-f 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric d-o (spring) (:documentation "Наружный диаметр пружины"))

(defmethod d-o((spr spring))
  "d-o - наружный диаметр пружины;
Пример использования:
;;;; (d-o *s*)
;;;; => 12.5"
  (+ (d-m spr) (d-w spr)))

(defgeneric set-d-o (spring d-o) (:documentation "Устанавливает наружный диаметр пружины равным d-o за счет изменения среднего диаметра витков"))

(defmethod set-d-o ((spr spring) d-o)
  "Устанавливает наружный диаметр пружины равным d-o 
за счет изменения среднего диаметра витков;
Пример использования:
;;;; *s* => #spring(l-0=30 d-m=7.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
;;;; (set-d-o *s* 20) => 17.5 
;;;; *s* => #spring(l-0=30 d-m=17.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
;;;; (set-d-o *s* 10)"
  (setf (d-m spr) (- d-o (d-w spr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric d-i (spring) (:documentation "Внутренний диаметр пружины"))

(defmethod d-i((spr spring))
  "Внутренний диаметр пружины;
Пример использования:
;;;; (d-i *s*)
;;;; => 12.5"
  (- (d-m spr) (d-w spr)))

(defgeneric set-d-i (spring d-i) (:documentation "Устанавливает внутренний диаметр пружины равным d-i за счет изменения среднего диаметра витков"))

(defmethod set-d-i ((spr spring) d-i)
  "Устанавливает внутренний диаметр пружины равным d-i
за счет изменения среднего диаметра витков;
Пример использования:
;;;; *s* => #spring(l-0=30 d-m=7.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
;;;; (set-d-i *s* 15) => 17.5 
;;;; *s* => #spring(l-0=30 d-m=17.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
;;;; (set-d-i *s* 7.5)"
  (setf (d-m spr) (+ d-i (d-w spr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric t-s (spring) (:documentation "Шаг пружины в свободном сосотоянии"))

(defmethod t-s ((spr spring))
  "Шаг пружины
Пример использования:
;;;; (t-s *s*)
;;;; => 4.375"
  (/ (- (l-0 spr) (* 3/2 (d-w spr))) (n-w spr)))

(defgeneric set-t-s (spring t-s) (:documentation "Устанавливает шаг витков пружины равным t-s за счет изменения длины пружины"))

(defmethod set-t-s ((spr spring) t-s)
  "Устанавливает шаг витков пружины равным t-s 
за счет изменения длины пружины;
Пример использования:
;;;; *s* => #spring(l-0=30 d-m=7.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
;;;; (set-t-s *s* 5) => 33.75
;;;; *s* => #spring(l-0=33.75 d-m=10 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
;;;; (set-t-s *s* 4.375)"
  (setf (l-0 spr) (+ (* t-s (n-w spr)) (* 3/2 (d-w spr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric l-w (spring) (:documentation "Длина проволоки развернутой пружины"))

(defmethod l-w ((spr spring))
  "Длина проволоки развернутой пружины
Пример использования:
;;;; (l-w *s*)
;;;; => 253.7527697720202D0"
  (let ((d-m (d-m spr))
	(t-s (t-s spr)))
  (* (n-f spr)
     (sqrt (+ (* pi pi d-m d-m) (* t-s t-s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric l-4 (spring) (:documentation "Длина пружины при соприкосновении витков"))

(defmethod l-4 ((spr spring))
  "Длина пружины при соприкосновении витков;
Пример использования:
;;;; (l-4 *s*) => 28.75"
  (* (+ 1.5 (n-w spr)) (d-w spr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric ro-w[kg/mm3] (spring) (:documentation "Плотность материала проволоки пружины"))

(defmethod ro-w[kg/mm3] ((spr spring))
  "Плотность материала проволоки пружины
Пример использования:
;;;; (ro-w[kg/mm3] *s*)
"
  (values 7.8e-6 "kg/mm3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defgeneric mass-s (spring) (:documentation "Масса пружины"))

(defmethod mass-s ((spr spring))
  "Масса пружины;
Пример использования:
;;;; (mass-s *s*) "
  (let ((ro (ro-w[kg/mm3] spr))
	(d-w (d-w spr))
	(n-f (n-f spr)))
    (values (* ro pi d-w d-w 1/4 (l-w spr) (/ (- n-f 1/2) n-f)) "kg/m3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric li-si (spring si) (:documentation "Высота пружины при деформации si"))

(defmethod li-si ((spr spring) si)
  "Высота пружины при деформации si"
  (- (l-0 spr) si))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric G[MPa] (spring) (:documentation "Модуль Юнга второго рода в МПа для материала пружины"))

(defmethod  G[MPa] ((spr spring))
  (* (G (m-w spr)) 1/1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric fi-si (spring si) (:documentation "Сила, необходимая для деформации пружины на величину si"))

(defmethod fi-si ((spr spring) si)
  "Сила, необходимая для деформации пружины на величину si
Пример использования:
;;;; (fi-si *s* 5)
"
  (let ((G (G[MPa] spr))
	(d-w (d-w spr))
	(d-m (d-m spr)))
    (/ (* G d-w d-w d-w d-w si)
       (* 8.0 d-m d-m d-m (n-w spr) (k-2 spr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric taui-fi (spring fi) (:documentation "Напряжение при кручении при приложении к пружине силы fi"))

(defmethod taui-fi ((spr spring) fi)
  "Напряжение при кручении при приложении к пружине силы fi
Пример использования:
;;;; (taui-fi *s* (fi-si *s* 5))
"
  (let ((d-w (d-w spr)))
    (/ (* 2.55 fi (d-m spr) (k-1 spr))
       (* d-w d-w d-w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric fi-taui (spring taui) (:documentation "Сила при которой напряжения при кручении равны taui"))

(defmethod fi-taui ((spr spring) taui)
  "Сила при которой напряжения при кручении равны taui;
Пример использования:
;;;; (fi-taui *s* 15)
"
  (let ((d-w (d-w spr)))
    (/ (* taui d-w d-w d-w)
       (* (d-m spr) (k-1 spr)) 2.55)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric si-fi (spring fi) (:documentation "Деформация пружины под нагрузкой"))

(defmethod  si-fi ((spr spring) fi)
  "Деформация пружины под нагрузкой;
Пример использования:
;;;; (si-fi *s* 50)
"
  (let ((G (G[MPa] spr))
	(d-m (d-m spr))
	(d-w (d-w spr)))
    (/ (* fi 8 d-m d-m d-m (n-w spr) (k-2 spr))
       (* d-w d-w d-w d-w G))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
