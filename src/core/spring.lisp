;;;; ./src/core/spring.lisp

(defpackage #:mnas-spring
  (:nicknames "MSPR" "MNAS-SPRING/CORE")
  (:use #:cl #:mnas-logical)
  (:export <spring>
           <spring-konstr-l0-f1>
	   l-0 
	   d-m 
	   d-w 
	   n-w 
	   m-w
	   )
  (:export *G-lst*
           *s*
           )
  (:export i-s 
	   k-1 
	   k-2 
	   n-f 
	   set-n-f 
	   d-o
	   set-d-o
	   d-i
	   set-d-i 
	   t-s 
	   set-t-s 
	   l-w 
	   l-4 
	   ro-w[kg/mm3] 
	   mass-s
	   li-si
	   G[MPa] 
	   fi-si 
	   taui-fi
	   fi-taui
	   si-fi
           )
  (:export fi-li
           fi-max-li
           fi-min-li))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mnas-spring)

(defparameter *G-lst*
   '(("1 класс"         78500e6  50 20 0.0)
     ("60С2А"           78500e6 250 20 0.1)
     ("65Г"             78500e6  50 20 0.0)
     ("51ХФА"           81400e6 250 20 0.1)
     ("10Х11Н23Т3МР-ВД" 72600e6 600 20 0.3)
     ("10Х15Н27Т3МР-ВД" 72600e6 600 20 0.3)
     ("12Х18Н10Т"       68600e6 250 20 0.1))
    " @b(Описание:) глобальная переменная @b(*G-lst*) содежит список
 значений модулей сдвига некоторых материалов: 

 Каждый элемент списка состоит из:
@begin(enum)
 @item(строки, обозначающей марку материала;)
 @item(значения модуля сдвига материала при температуре 20 град С, [Па];)
 @item(максимальной температуры применения пружины из проволоки
       данного материала, [C];)
 @item(нормальной температуры для применения данного материала, [C];)
 @item(коэффициента уменьшения модуля сдвига при увеличении
       температуры от нормальной до максимальной.)
@end(enum)
")

(defun G (matirial-string &optional (temperature 20))
  "@b(Описание:) функция @b(G) возвращает значение модуля сдвига -
  модуль Юнга второго рода.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (G \"12Х18Н10Т\")
@end(code)
"
  (let* ((data (assoc matirial-string *G-lst* :test #'string=))
	 (G_nor (second data))
	 (t_max (third  data))
	 (t_nor (fourth data))
	 (koeff (fifth  data)))
    (* G_nor (- 1.0 (* koeff (/ (- temperature t_nor) (- t_max t_nor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic

(defgeneric i-s     (<spring>)      (:documentation "Индекс пружины"))
(defgeneric k-1     (<spring>)      (:documentation "k-1 - Коэффициент прочности пружины"))
(defgeneric k-2     (<spring>)      (:documentation "k-2 - Коэффициент жесткости пружины"))
(defgeneric n-f     (<spring>)      (:documentation "Количество полных витков пружины"))
(defgeneric set-n-f (<spring> n-f)  (:documentation "Устанавливает полное число витков пружины равным n-f за счёт изменения числа рабочих витков пружины"))
(defgeneric d-o     (<spring>)      (:documentation "Наружный диаметр пружины"))
(defgeneric set-d-o (<spring> d-o)  (:documentation "Устанавливает наружный диаметр пружины равным d-o за счет изменения среднего диаметра витков"))
(defgeneric d-i     (<spring>)      (:documentation "Внутренний диаметр пружины"))
(defgeneric set-d-i (<spring> d-i)  (:documentation "Устанавливает внутренний диаметр пружины равным d-i за счет изменения среднего диаметра витков"))
(defgeneric t-s     (<spring>)      (:documentation "Шаг пружины в свободном сосотоянии"))
(defgeneric set-t-s (<spring> t-s)  (:documentation "Устанавливает шаг витков пружины равным t-s за счет изменения длины пружины"))
(defgeneric l-w     (<spring>)      (:documentation "Длина проволоки развернутой пружины"))
(defgeneric l-4     (<spring>)      (:documentation "Длина пружины при соприкосновении витков"))
(defgeneric ro-w[kg/mm3] (<spring>) (:documentation "Плотность материала проволоки пружины"))
(defgeneric mass-s  (<spring>)      (:documentation "Масса пружины"))
(defgeneric li-si   (<spring> si)   (:documentation "Высота пружины при деформации si"))
(defgeneric G[MPa]  (<spring> &optional temperarure) (:documentation "Модуль Юнга второго рода в МПа для материала пружины"))
(defgeneric fi-si   (<spring> si)   (:documentation "Сила, необходимая для деформации пружины на величину si"))
(defgeneric taui-fi (<spring> fi)   (:documentation "Напряжение при кручении при приложении к пружине силы fi"))
(defgeneric fi-taui (<spring> taui) (:documentation "Сила при которой напряжения при кручении равны taui"))
(defgeneric si-fi   (<spring> fi)   (:documentation "Деформация пружины под нагрузкой"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric fi-li     (<spring> X) (:documentation "Возвращает силу для придания пружине <spring> длины X"))
(defgeneric fi-min-li (<spring> X) (:documentation "Возвращает минимальное значение силы для придания пружине <spring> длины X"))
(defgeneric fi-max-li (<spring> X) (:documentation "Возвращает максимальное значение силы для придания пружине <spring> длины X"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; classes

(defclass <spring> ()
  ((l-0 :accessor l-0 :initarg :l-0 :initform 30          :documentation "Длина пружины в свободном состоянии, мм")
   (d-m :accessor d-m :initarg :d-m :initform 10          :documentation "Средний диаметр пружины, мм")
   (d-w :accessor d-w :initarg :d-w :initform 2.5         :documentation "Диаметр проволоки, мм")
   (n-w :accessor n-w :initarg :n-w :initform 6           :documentation "Число рабочих витков пружины, 1")
   (m-w :accessor m-w :initarg :m-w :initform "12Х18Н10Т" :documentation "Материал из которого изготовлена проволока")
   )
  (:documentation "@b(Описание:) представляет пружину сжатия."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <spring-konstr-l0-f1>  (<spring>)
  (
   (l-0-es :accessor l-0-es :initarg :l-0-es :initform  0.5 :documentation "Верхнее предельное отклонение поля допуска пружины на размер l-0,  мм")
   (l-0-ei :accessor l-0-ei :initarg :l-0-ei :initform -0.5 :documentation "Нижнее  предельное отклонение поля допуска пружины на размер l-0,  мм")
   
   (L1     :accessor L1     :initarg :L1     :initform  8.0 :documentation "Номинальная длина пружины под нагрузкой L1, мм")
   
   (f1     :accessor f1     :initarg :f1     :initform 11.5 :documentation "Номинальное значения усилия, развиваемое пружиной при деформации S, Н")
   (f1-es  :accessor f1-es  :initarg :f1-es  :initform  2.0 :documentation "Верхнее предельное отклонение поля допуска на усилие, развиваемое пружиной при деформации S, Н")
   (f1-ei  :accessor f1-ei  :initarg :f1-ei  :initform -2.0 :documentation "Нижнее  предельное отклонение поля допуска на усилие, развиваемое пружиной при деформации S, Н")
   )
  (:documentation "@b(Описание:) представляет пружину сжатия с чертежными параметрами, заданными в виде:
@begin(enum)
@item(Длины пружины в свободном состоянии;)
@item(Одного значения деформации пружины, для которого определяется усилие развиваемое пружиной;)
@item(Усилия, развиваемого пружиной при деформации S силы. Усилия, развиваемого пружиной при деформации S силы.)
@end(enum) 
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; methods

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((x <spring>) s) (format s "#<spring>" ))

(defmethod print-object         ((x <spring>) s) (format s "(l-0=~S d-m=~S d-w=~S n-w=~S m-w=~S)" (l-0 x) (d-m x) (d-w x) (n-w x) (m-w x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod i-s ((spr <spring>))
  "@b(Описание:) метод @b(i-s) возвращает индекс пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (i-s *s*) => 4.0
@end(code)
"
  (/ (d-m spr) (d-w spr)))

(defmethod k-1 ((spr <spring>))
  "@b(Описание:) метод @b(k-1) возвращвет коэффициент прочности пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (k-1 *s*) => 1.40375
@end(code)"
  (let ((i (i-s spr)))
    (+ (/ (- (* 4 i) 1)
          (- (* 4 i) 4))
       (/ 615/1000 i))))

(defmethod k-2 ((spr <spring>))
  "@b(Описание:) метод @b(k-2) возвращает коэффициент жесткости пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (k-2 *s*) => 1.09375
@end(code)
"
  (let ((i (i-s spr)))
    (+ 1 (/ 1 2 i)
       (/ -1 2 i i))))

(defmethod n-f ((spr <spring>))
  "@b(Описание:) метод @b(n-f) возвращает полное количество витков пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (n-f *s*) => 8
@end(code)
"
  (+ (n-w spr) 2))

(defmethod set-n-f ((spr <spring>) n-f)
  "@b(Описание:) метод @b(set-n-f) устанавливает полное число витков
  пружины равным @b(n-f) за счёт изменения числа рабочих витков пружины.
  Возвращает рабочее число витков пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 *s* => #<spring>(l-0=30.0 d-m=10 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (set-n-f *s* 12) => 10
 *s* => #<spring>(l-0=30.0 d-m=10 d-w=2.5 n-w=10 m-w=\"12Х18Н10Т\")
 (set-n-f *s* 8) => 6
@end(code)
"
  (setf (n-w spr) (- n-f 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod d-o ((spr <spring>))
  "@b(Описание:) метод @b(d-o) возвращает наружный диаметр пружины @(spr).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (d-o *s*) => 12.5
@end(code)
"
  (+ (d-m spr) (d-w spr)))

(defmethod set-d-o ((spr <spring>) d-o)
  "@b(Описание:) метод @b(set-d-o) устанавливает наружный диаметр
   пружины равным d-o за счет изменения среднего диаметра витков.
   Возвращает новый средний диаметр пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 *s* => #<spring>(l-0=30 d-m=7.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (set-d-o *s* 20) => 17.5 
 *s* => #<spring>(l-0=30 d-m=17.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (set-d-o *s* 10)
@end(code)
"
  (setf (d-m spr) (- d-o (d-w spr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod d-i ((spr <spring>))
  "@b(Описание:) метод @b(d-i) возвращает внутренний диаметр пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (d-i *s*) => 12.5
@end(code)
"
  (- (d-m spr) (d-w spr)))

(defmethod set-d-i ((spr <spring>) d-i)
  "@b(Описание:) метод @b(set-d-i) устанавливает внутренний диаметр
   пружины равным d-i за счет изменения среднего диаметра витков.
   Возвращает новый средний диаметр витков пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 *s* => #<spring>(l-0=30 d-m=7.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (set-d-i *s* 15) => 17.5 
 *s* => #<spring>(l-0=30 d-m=17.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (set-d-i *s* 7.5)
@end(code)
"
  (setf (d-m spr) (+ d-i (d-w spr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod t-s ((spr <spring>))
  "@b(Описание:) метод @b(t-s) возвращает шаг пружины @(spr).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (t-s *s*) => 4.375
@end(code)
"
  (/ (- (l-0 spr)
        (* 3/2 (d-w spr)))
     (n-w spr)))

(defmethod set-t-s ((spr <spring>) t-s)
  "@b(Описание:) метод @b(set-t-s) устанавливает шаг витков пружины
   равным t-s за счет изменения длины пружины. Возвращает новую длину
   пружины в свободном состоянии.

 @b(Пример использования:)
@begin[lang=lisp](code)
 *s* => #<spring>(l-0=30 d-m=7.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (set-t-s *s* 5) => 33.75
 *s* => #<spring>(l-0=33.75 d-m=10 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (set-t-s *s* 4.375)
@end(code)
"
  (setf (l-0 spr)
        (+ (* t-s (n-w spr))
           (* 3/2 (d-w spr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod l-w ((spr <spring>))
  "@b(Описание:) метод @b(l-w) возвращает длину проволоки развернутой пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (l-w *s*) => 253.7527697720202D0
@end(code)
"
  (let ((d-m (d-m spr))
	(t-s (t-s spr)))
  (* (n-f spr)
     (sqrt (+ (* pi pi d-m d-m) (* t-s t-s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod l-4 ((spr <spring>))
  "@b(Описание:) метод @b(l-4) возвращает длину пружины при
   соприкосновении витков.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (l-4 *s*) => 28.75
@end(code)
"
  (* (+ 1.5 (n-w spr)) (d-w spr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ro-w[kg/mm3] ((spr <spring>))
  "@b(Описание:) метод @b(ro-w[kg/mm3]) возвращает плотность материала
  проволоки пружины @b(spr).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (ro-w[kg/mm3] *s*)
@end(code)
"
  (values 7.8e-6 "kg/mm3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mass-s ((spr <spring>))
  "@b(Описание:) метод @b(mass-s) возвращает массу пружины @b(spr).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mass-s *s*) 
@end(code)
"
  (let ((ro (ro-w[kg/mm3] spr))
	(d-w (d-w spr))
	(n-f (n-f spr)))
    (values (* ro pi d-w d-w 1/4 (l-w spr) (/ (- n-f 1/2) n-f)) "kg/m3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod li-si ((spr <spring>) si)
  "@b(Описание:) метод @b(li-si) возвращает высоту пружины @b(spr) при деформации @b(si).
"
  (- (l-0 spr) si))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod G[MPa] ((spr <spring>) &optional (temperature 20))
  "@b(Описание:) метод @b(G[MPa]) возвращает модуль Юнга второго рода
   для пружины @b(spr).
"
  (* (G (m-w spr) temperarure) 1/1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fi-si ((spr <spring>) si)
  "@b(Описание:) метод @b(fi-si) возвращает силу, вызывающую
   деформацию пружины на величину si.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (fi-si *s* 5)
@end(code)
"
  (let ((G (G[MPa] spr))
	(d-w (d-w spr))
	(d-m (d-m spr)))
    (/ (* G d-w d-w d-w d-w si)
       (* 8.0 d-m d-m d-m (n-w spr) (k-2 spr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod taui-fi ((spr <spring>) fi)
  "Напряжение при кручении при приложении к пружине силы fi
Пример использования:
;;;; (taui-fi *s* (fi-si *s* 5))
"
  (let ((d-w (d-w spr)))
    (/ (* 2.55 fi (d-m spr) (k-1 spr))
       (* d-w d-w d-w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fi-taui ((spr <spring>) taui)
  "@b(Описание:) метод @b(fi-taui) возвращает силу, при которой
   напряжения при кручении равны taui.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (fi-taui *s* 15)
@end(code)
"
  (let ((d-w (d-w spr)))
    (/ (* taui d-w d-w d-w)
       (* (d-m spr) (k-1 spr)) 2.55)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod si-fi ((spr <spring>) fi)
  "@b(Описание:) метод @b(si-fi) возвращает деформацию пружины под
   нагрузкой @b(fi).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (si-fi *s* 50) => 2.361516
@end(code)
"
  (let ((G (G[MPa] spr))
	(d-m (d-m spr))
	(d-w (d-w spr)))
    (/ (* fi 8 d-m d-m d-m (n-w spr) (k-2 spr))
       (* d-w d-w d-w d-w G))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fi-min-li ((spr <spring-konstr-l0-f1>) X)
  "@b(Описание:) метод @b(fi-min-li) возвращет минимальную силу, при
   длине пружины X.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (fi-min-li *s-k* 25) 1.9883721 
@end(code)
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

(defmethod fi-max-li ((spr <spring-konstr-l0-f1>) X)
  "@b(Описание:) метод @b(fi-max-li) возвращает максимальную сила, при
   длине пружины X.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (fi-max-li *s-k* 25) => 3.3
@end(code)
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


(defmethod fi-li ((spr <spring-konstr-l0-f1>) X)
  "@b(Описание:) метод @b(fi-li) возврвщвет номинальное значение силы при длине пружины X.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (fi-li *s-k* 25) 2.644186
@end(code)
"
  (* 1/2
     (+ (fi-max-li spr X)
        (fi-min-li spr X))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; spring-examples

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *s* (make-instance '<spring>))

(defparameter *s-k* (make-instance '<spring-konstr-l0-f1>))

