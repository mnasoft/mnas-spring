;;;; defmethods.lisp

(in-package #:mnas-spring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((x spring) s) (format s "#spring" ))

(defmethod print-object         ((x spring) s) (format s "(l-0=~S d-m=~S d-w=~S n-w=~S m-w=~S)" (l-0 x) (d-m x) (d-w x) (n-w x) (m-w x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod i-s ((spr spring))
  "Индекс пружины;
Пример использования:
Индекс пружины
;;;; (i-s *s*) => 4.0"
  (/ (d-m spr) (d-w spr)))

(defmethod k-1 ((spr spring))
  "Коэффициент прочности пружины
Пример использования:
;;;; (k-1 *s*)
;;;;=> 1.40375"
  (let ((i (i-s spr))) (+ (/ (- (* 4 i) 1) (- (* 4 i) 4)) (/ 615/1000 i))))

(defmethod k-2 ((spr spring))
  "k-2 - Коэффициент жесткости пружины
Пример использования:
;;;; (k-2 *s*)
;;;; => 1.09375"
  (let ((i (i-s spr))) (+ 1 (/ 1 2 i) (/ -1 2 i i))))

(defmethod n-f ((spr spring))
  "Пример использования
;;;; (n-f *s*)
;;;; => 8"
  (+ (n-w spr) 2))

(defmethod set-n-f ((spr spring) n-f)
  "Устанавливает полное число витков пружины равным n-f за счёт изменения числа рабочих витков пружины;
Пример использования:
;;;; *s* => #spring(l-0=30.0 d-m=10 d-w=2.5 n-w=6 m-w=\"12Х18Н10Т\")
;;;; (set-n-f *s* 12) => 10
;;;; *s* => #spring(l-0=30.0 d-m=10 d-w=2.5 n-w=10 m-w=\"12Х18Н10Т\")
;;;; (set-n-f *s* 8) => 6"
  (setf (n-w spr) (- n-f 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod d-o((spr spring))
  "d-o - наружный диаметр пружины;
Пример использования:
;;;; (d-o *s*)
;;;; => 12.5"
  (+ (d-m spr) (d-w spr)))

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

(defmethod d-i((spr spring))
  "Внутренний диаметр пружины;
Пример использования:
;;;; (d-i *s*)
;;;; => 12.5"
  (- (d-m spr) (d-w spr)))

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

(defmethod t-s ((spr spring))
  "Шаг пружины
Пример использования:
;;;; (t-s *s*)
;;;; => 4.375"
  (/ (- (l-0 spr) (* 3/2 (d-w spr))) (n-w spr)))

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

(defmethod l-4 ((spr spring))
  "Длина пружины при соприкосновении витков;
Пример использования:
;;;; (l-4 *s*) => 28.75"
  (* (+ 1.5 (n-w spr)) (d-w spr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ro-w[kg/mm3] ((spr spring))
  "Плотность материала проволоки пружины
Пример использования:
;;;; (ro-w[kg/mm3] *s*)
"
  (values 7.8e-6 "kg/mm3"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mass-s ((spr spring))
  "Масса пружины;
Пример использования:
;;;; (mass-s *s*) "
  (let ((ro (ro-w[kg/mm3] spr))
	(d-w (d-w spr))
	(n-f (n-f spr)))
    (values (* ro pi d-w d-w 1/4 (l-w spr) (/ (- n-f 1/2) n-f)) "kg/m3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod li-si ((spr spring) si)
  "Высота пружины при деформации si"
  (- (l-0 spr) si))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod  G[MPa] ((spr spring))
  (* (G (m-w spr)) 1/1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod taui-fi ((spr spring) fi)
  "Напряжение при кручении при приложении к пружине силы fi
Пример использования:
;;;; (taui-fi *s* (fi-si *s* 5))
"
  (let ((d-w (d-w spr)))
    (/ (* 2.55 fi (d-m spr) (k-1 spr))
       (* d-w d-w d-w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod fi-taui ((spr spring) taui)
  "Сила при которой напряжения при кручении равны taui;
Пример использования:
;;;; (fi-taui *s* 15)
"
  (let ((d-w (d-w spr)))
    (/ (* taui d-w d-w d-w)
       (* (d-m spr) (k-1 spr)) 2.55)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
