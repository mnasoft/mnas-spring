;;;; ./src/core/spring.lisp

(defpackage #:mnas-spring
  (:nicknames "MSPR" "MNAS-SPRING/CORE")
  (:use #:cl #:mnas-logical)
  (:export <spring>
           )
  (:export <spring>-l-0 
	   <spring>-d-m 
	   <spring>-d-w 
	   <spring>-n-w
	   <spring>-m-w
           )
  (:export <spring-l0-l1>
	   )
  (:export *G-lst*
           *s*
           )
  (:export <spring>-n-f ;;setf-функции
           <spring>-d-o
           <spring>-d-i
           <spring>-t-s
           )
  (:export <spring>-i-s
           <spring>-i-1
	   <spring>-k-1 
	   <spring>-k-2 
	   <spring>-n-f 
	   <spring>-d-o
	   <spring>-d-i
	   <spring>-t-s 
	   <spring>-l-w 
	   <spring>-l-4 
	   <spring>-ro-w[kg/mm3] 
	   <spring>-mass-s
	   <spring>-li-si
           <spring>-si-li
	   <spring>-G[MPa] 
	   <spring>-fi-si
           <spring>-fi-li
	   <spring>-taui-fi
	   <spring>-fi-taui
	   <spring>-si-fi
           <spring>-li-fi
           <spring>-s-k
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

(defgeneric <spring>-i-s     (<spring>)      (:documentation "Индекс пружины"))
(defgeneric <spring>-k-1     (<spring>)      (:documentation "Коэффициент прочности пружины"))
(defgeneric <spring>-k-2     (<spring>)      (:documentation "Коэффициент жесткости пружины"))
(defgeneric <spring>-n-f     (<spring>)      (:documentation "Количество полных витков пружины"))
(defgeneric <spring>-d-o     (<spring>)      (:documentation "Наружный диаметр пружины"))
(defgeneric <spring>-d-i     (<spring>)      (:documentation "Внутренний диаметр пружины"))
(defgeneric <spring>-t-s     (<spring>)      (:documentation "Шаг пружины в свободном сосотоянии"))
(defgeneric <spring>-l-w     (<spring>)      (:documentation "Длина проволоки развернутой пружины"))
(defgeneric <spring>-l-4     (<spring>)      (:documentation "Длина пружины при соприкосновении витков"))
(defgeneric <spring>-ro-w[kg/mm3] (<spring>) (:documentation "Плотность материала проволоки пружины"))
(defgeneric <spring>-mass-s  (<spring>)      (:documentation "Масса пружины"))
(defgeneric <spring>-li-si   (<spring> si)   (:documentation "Высота пружины при деформации si"))
(defgeneric <spring>-G[MPa]  (<spring> &optional temperature) (:documentation "Модуль Юнга второго рода в МПа для материала пружины"))
(defgeneric <spring>-fi-si   (<spring> si)   (:documentation "Сила, необходимая для деформации пружины на величину si"))
(defgeneric <spring>-taui-fi (<spring> fi)   (:documentation "Напряжение при кручении при приложении к пружине силы fi"))
(defgeneric <spring>-fi-taui (<spring> taui) (:documentation "Сила, при которой напряжения при кручении равны taui"))
(defgeneric <spring>-si-fi   (<spring> fi)   (:documentation "Деформация пружины под нагрузкой"))

(defgeneric set-n-f (<spring> n-f)  (:documentation "Устанавливает полное число витков пружины равным n-f за счёт изменения числа рабочих витков пружины"))
(defgeneric set-d-o (<spring> d-o)  (:documentation "Устанавливает наружный диаметр пружины равным d-o за счет изменения среднего диаметра витков"))
(defgeneric set-d-i (<spring> d-i)  (:documentation "Устанавливает внутренний диаметр пружины равным d-i за счет изменения среднего диаметра витков"))
(defgeneric set-t-s (<spring> t-s)  (:documentation "Устанавливает шаг витков пружины равным t-s за счет изменения длины пружины"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric fi-li     (spring X) (:documentation "Возвращает силу для придания пружине <spring> длины X"))
(defgeneric fi-min-li (spring X) (:documentation "Возвращает минимальное значение силы для придания пружине <spring> длины X"))
(defgeneric fi-max-li (spring X) (:documentation "Возвращает максимальное значение силы для придания пружине <spring> длины X"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; classes

(defclass <spring> ()
  ((l-0 :accessor <spring>-l-0 :initarg :l-0 :initform 30          :documentation "Длина пружины в свободном состоянии, мм")
   (d-m :accessor <spring>-d-m :initarg :d-m :initform 10          :documentation "Средний диаметр пружины, мм")
   (d-w :accessor <spring>-d-w :initarg :d-w :initform 2.5         :documentation "Диаметр проволоки, мм")
   (n-w :accessor <spring>-n-w :initarg :n-w :initform 6           :documentation "Число рабочих витков пружины, 1")
   (m-w :accessor <spring>-m-w :initarg :m-w :initform "12Х18Н10Т" :documentation "Материал из которого изготовлена проволока")
   )
  (:documentation "@b(Описание:) представляет пружину сжатия."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <spring-l0-l1>  (<spring>)
  (
   (l-0-es :accessor l-0-es :initarg :l-0-es :initform  0.5 :documentation "Верхнее предельное отклонение поля допуска пружины на размер l-0,  мм")
   (l-0-ei :accessor l-0-ei :initarg :l-0-ei :initform -0.5 :documentation "Нижнее  предельное отклонение поля допуска пружины на размер l-0,  мм")
   
   (L1     :accessor L1     :initarg :L1     :initform  8.0 :documentation "Номинальная длина пружины под нагрузкой L1, мм")
   
   (f1     :accessor f1     :initarg :f1     :initform 11.5 :documentation "Номинальное значения усилия, развиваемое пружиной при деформации S, Н")
   (f1-es  :accessor f1-es  :initarg :f1-es  :initform  2.0 :documentation "Верхнее предельное отклонение поля допуска на усилие, развиваемое пружиной при деформации S, Н")
   (f1-ei  :accessor f1-ei  :initarg :f1-ei  :initform -2.0 :documentation "Нижнее  предельное отклонение поля допуска на усилие, развиваемое пружиной при деформации S, Н")
   )
  (:documentation "@b(Описание:) представляет пружину сжатия с
                   чертежными параметрами, заданными в виде:

@begin(enum)
@item(Длины пружины L0 в свободном состоянии;)
@item(Длины пружины L1 под нагрузкой F1.)
@end(enum) 
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; methods

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((x <spring>) s) (format s "#<spring>" ))

(defmethod print-object         ((x <spring>) s)
  (format s "(l-0=~S d-m=~S d-w=~S n-w=~S m-w=~S)"
          (<spring>-l-0 x)
          (<spring>-d-m x)
          (<spring>-d-w x)
          (<spring>-n-w x)
          (<spring>-m-w x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-i-s ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-i-s) возвращает индекс пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (setf (<spring>-i-s *s*) 6.0) => #<spring>(l-0=30 d-m=15.0 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
@end(code)
"
  (/ (<spring>-d-m spr) (<spring>-d-w spr)))

(defmethod (setf <spring>-i-s) (i-s (spr <spring>))
  "@b(Описание:) метод @b(<spring>-i-s) устанавливает средний диаметр
   витов для пружины такой, чтобы индекс пружины был равен i-s.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-i-s *s*) => 4.0
@end(code)
"
  (let ((i (if (< i-s 2.0) 2.0 i-s)))
    (setf (<spring>-d-m spr) (* (<spring>-d-w spr) i))
    spr))

(defmethod <spring>-k-1 ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-k-1) возвращвет коэффициент прочности пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-k-1 *s*) => 1.40375
@end(code)"
  (let ((i (<spring>-i-s spr)))
    (+ (/ (- (* 4 i) 1)
          (- (* 4 i) 4))
       (/ 615/1000 i))))

(defmethod <spring>-k-2 ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-k-2) возвращает коэффициент жесткости пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-k-2 *s*) => 1.09375
@end(code)
"
  (let ((i (<spring>-i-s spr)))
    (+ 1 (/ 1 2 i)
       (/ -1 2 i i))))

(defmethod <spring>-n-f ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-n-f) возвращает полное количество витков пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-n-f *s*) => 8
@end(code)
"
  (+ (<spring>-n-w spr) 2))

(defmethod (setf <spring>-n-f) (n-f (spr <spring>))
  "@b(Описание:) метод @b(set-n-f) устанавливает полное число витков
  пружины равным @b(n-f) за счёт изменения числа рабочих витков пружины.
  Возвращает измененную ссылку на пружину.

 @b(Пример использования:)
@begin[lang=lisp](code)
 *s* => #<spring>(l-0=30.0 d-m=10 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (setf (<spring>-n-f *s*) 12) => #<spring>(l-0=30 d-m=17.5 d-w=2.5 n-w=10 m-w=\"12Х18Н10Т\")
 (<spring>-n-f (setf (<spring>-n-f *s*) 8)) => 8 
@end(code)
"
  (setf (<spring>-n-w spr) (- n-f 2))
  spr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-d-o ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-d-o) возвращает наружный диаметр пружины @(spr).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-d-o *s*) => 12.5
@end(code)
"
  (+ (<spring>-d-m spr) (<spring>-d-w spr)))

(defmethod (setf <spring>-d-o) (d-o (spr <spring>))
  "@b(Описание:) метод @b(set-d-o) устанавливает наружный диаметр
   пружины равным d-o за счет изменения среднего диаметра витков.
   Возвращает измененную ссылку на пружину.

 @b(Пример использования:)
@begin[lang=lisp](code)
 *s* => #<spring>(l-0=30 d-m=7.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (setf (<spring>-d-o *s*) 20.0) #<spring>(l-0=30 d-m=17.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
=> 
 *s* => #<spring>(l-0=30 d-m=17.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (set-d-o *s* 10)
@end(code)
"
  (setf (<spring>-d-m spr) (- d-o (<spring>-d-w spr)))
  spr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-d-i ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-d-i) возвращает внутренний диаметр пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-d-i *s*) => 12.5
@end(code)
"
  (- (<spring>-d-m spr) (<spring>-d-w spr)))

(defmethod (setf <spring>-d-i) (d-i (spr <spring>))
  "@b(Описание:) метод @b((setf <spring>-d-i)) устанавливает
   внутренний диаметр пружины равным d-i за счет изменения среднего
   диаметра витков.  Возвращает новый средний диаметр витков пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 *s* => #<spring>(l-0=30 d-m=7.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (setf (<spring>-d-i *s*) 15) #<spring>(l-0=30 d-m=17.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (<spring>-d-i (setf (<spring>-d-i *s*) 7.5))
@end(code)
"
  (setf (<spring>-d-m spr) (+ d-i (<spring>-d-w spr)))
  spr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-t-s ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-t-s) возвращает шаг пружины @(spr).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-t-s *s*) => 4.375
@end(code)
"
  (/ (- (<spring>-l-0 spr)
        (* 3/2 (<spring>-d-w spr)))
     (<spring>-n-w spr)))

(defmethod (setf <spring>-t-s) (t-s (spr <spring>))
  "@b(Описание:) метод @b((setf <spring>-t-s)) устанавливает шаг витков пружины
   равным t-s за счет изменения длины пружины. Возвращает новую длину
   пружины в свободном состоянии.

 @b(Пример использования:)
@begin[lang=lisp](code)
 *s* => #<spring>(l-0=30 d-m=7.5 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (setf (<spring>-t-s *s*) 6) #<spring>(l-0=39.75 d-m=10.0 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (<spring>-t-s (setf (<spring>-t-s *s*) 4.375)) => 4.375
@end(code)
"
  (setf (<spring>-l-0 spr)
        (+ (* t-s (<spring>-n-w spr))
           (* 3/2 (<spring>-d-w spr))))
  spr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-l-w ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-l-w) возвращает длину проволоки
   развернутой пружины.

 @b(Пример использования:) @begin[lang=lisp](code)
 (<spring>-l-w *s*) => 253.7527697720202D0
@end(code)
"
  (let ((d-m (<spring>-d-m spr))
	(t-s (<spring>-t-s spr)))
  (* (<spring>-n-f spr)
     (sqrt (+ (* pi pi d-m d-m) (* t-s t-s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-l-4 ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-l-4) возвращает длину пружины при
   соприкосновении витков.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-l-4 *s*) => 28.75
@end(code)
"
  (* (+ 1.5 (<spring>-n-w spr)) (<spring>-d-w spr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-ro-w[kg/mm3] ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-ro-w[kg/mm3]) возвращает плотность материала
  проволоки пружины @b(spr).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (<spring>-ro-w[kg/mm3] *s*)
@end(code)
"
  (values 7.8e-6 "kg/mm3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-mass-s ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-mass-s) возвращает массу пружины @b(spr).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-mass-s *s*) 0.009108494166826525d0, \"kg/m3\"
@end(code)
"
  (let ((ro (<spring>-ro-w[kg/mm3] spr))
	(d-w (<spring>-d-w spr))
	(n-f (<spring>-n-f spr)))
    (values (* ro pi d-w d-w 1/4 (<spring>-l-w spr) (/ (- n-f 1/2) n-f)) "kg/m3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-li-si ((spr <spring>) si)
  "@b(Описание:) метод @b(<spring>-li-si) возвращает высоту пружины
   @b(spr) при деформации @b(si).
"
  (- (<spring>-l-0 spr) si))

(defmethod <spring>-si-li ((spr <spring>) li)
  "@b(Описание:) метод @b(<spring>-li-si) деформацию пружины @b(spr)
   при высоте @b(li)."
  (- (<spring>-l-0 spr) li))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-G[MPa] ((spr <spring>) &optional (temperature 20))
  "@b(Описание:) метод @b(<spring>-G[MPa]) возвращает модуль Юнга второго рода
   для пружины @b(spr)."
  (* (G (<spring>-m-w spr) temperature) 1/1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-fi-si ((spr <spring>) si)
  "@b(Описание:) метод @b(<spring>-fi-si) возвращает силу, вызывающую
   деформацию пружины на величину si.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-fi-si *s* 5)
@end(code)
"
  (let ((G (<spring>-G[MPa] spr))
	(d-w (<spring>-d-w spr))
	(d-m (<spring>-d-m spr)))
    (/ (* G d-w d-w d-w d-w si)
       (* 8.0 d-m d-m d-m (<spring>-n-w spr) (<spring>-k-2 spr)))))

(defmethod <spring>-fi-li ((spr <spring>) li)
  "@b(Описание:) метод @b(<spring>-fi-si) возвращает силу, при высоте
   пружины @b(spr) равной li.

 @b(Пример использования:)
@begin[lang=lisp](code)
 *s* => #<spring>(l-0=30 d-m=10 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
 (<spring>-fi-si *s* 5)  => 255.20834
 (<spring>-fi-li *s* 25) => 255.20834
@end(code)
"
  (<spring>-fi-si spr (<spring>-si-li spr li)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-taui-fi ((spr <spring>) fi)
  "Напряжение при кручении при приложении к пружине силы fi
Пример использования:
;;;; (<spring>-taui-fi *s* (<spring>-fi-si *s* 5))
"
  (let ((d-w (<spring>-d-w spr)))
    (/ (* 2.55 fi (<spring>-d-m spr) (<spring>-k-1 spr))
       (* d-w d-w d-w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-fi-taui ((spr <spring>) taui)
  "@b(Описание:) метод @b(<spring>-fi-taui) возвращает силу, при которой
   напряжения при кручении равны taui.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-fi-taui *s* 15)
@end(code)
"
  (let ((d-w (<spring>-d-w spr)))
    (/ (* taui d-w d-w d-w)
       (* (<spring>-d-m spr) (<spring>-k-1 spr)) 2.55)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-si-fi ((spr <spring>) fi)
  "@b(Описание:) метод @b(<spring>-si-fi) возвращает деформацию пружины под
   нагрузкой @b(fi).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-si-fi *s* 50) => 2.361516
@end(code)
"
  (let ((G (<spring>-G[MPa] spr))
	(d-m (<spring>-d-m spr))
	(d-w (<spring>-d-w spr)))
    (/ (* fi 8 d-m d-m d-m (<spring>-n-w spr) (<spring>-k-2 spr))
       (* d-w d-w d-w d-w G))))


(defmethod <spring>-li-fi ((spr <spring>) fi)
  "@b(Описание:) метод @b(<spring>-li-fi) высоту пружины @b(spr) под нагрузкой 
   нагрузкой @b(fi).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-si-fi *s* 250)
 (<spring>-li-fi *s* 250)
@end(code)
"
  (<spring>-li-si spr (<spring>-si-fi spr fi)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fi-min-li ((spr <spring-l0-l1>) X)
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
	 (- (+ (<spring>-l-0 spr) (l-0-ei spr)) X )
	 (- (+ (<spring>-l-0 spr) (l-0-ei spr)) (L1 spr)))
	(+ (f1 spr) (f1-ei spr))))
    ((<= X (L1 spr))
     (* (/ 
	 (- (+ (<spring>-l-0 spr) (l-0-es spr) ) X )
	 (- (+ (<spring>-l-0 spr) (l-0-es spr) ) (L1 spr)))
	(+ (f1 spr) (f1-ei spr))))))

(defmethod fi-max-li ((spr <spring-l0-l1>) X)
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
	 (- (+ (<spring>-l-0 spr) (l-0-ei spr) ) X )
	 (- (+ (<spring>-l-0 spr) (l-0-ei spr) ) (L1 spr)))
	(+ (f1 spr) (f1-es spr))))
    ((>= X (L1 spr))
     (* (/ 
	 (- (+ (<spring>-l-0 spr) (l-0-es spr)) X )
	 (- (+ (<spring>-l-0 spr) (l-0-es spr)) (L1 spr)))
	(+ (f1 spr) (f1-es spr))))))


(defmethod fi-li ((spr <spring-l0-l1>) X)
  "@b(Описание:) метод @b(fi-li) возврвщвет номинальное значение силы при длине пружины X.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (fi-li *s-k* 25) 2.644186
@end(code)
"
  (* 1/2
     (+ (fi-max-li spr X)
        (fi-min-li spr X))))

(defmethod <spring>-i-1 ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-i-1) относительную высоту пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-i-1 *s*) => 3 
@end(code)
"
  (/ (<spring>-l-0 spr) (<spring>-d-m spr)))

(defmethod (setf <spring>-i-1) (i-1 (spr <spring>))
  "@b(Описание:) метод @b(<spring>-i-1) устанавливает такую высоту
   пружины в свободном состоянии, что относительная высота становится равной
   i-1.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (setf (<spring>-i-1 *s*) 1.0) => #<spring>(l-0=15.0 d-m=15.0 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
@end(code)
"
  (let ((i (if (< i-1 0.0) 1.0 i-1)))
  (setf (<spring>-l-0 spr) (* i (<spring>-d-m spr)))
    spr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring>-s-k ((spr <spring>))
  "@b(Описание:) метод @b(<spring>-s-k) возвращает толщину концевого
   витка пружины.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (<spring>-s-k *s*)= >0.625 (62.5%)
@end(code)
"
  (if (< (<spring>-d-w spr) 1.0)
      (<spring>-d-w spr)
      (/ (<spring>-d-w spr) 4.0)))

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

(defparameter *s-k* (make-instance '<spring-l0-l1>))

