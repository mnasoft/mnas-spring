;;;; defgenerics.lisp

(in-package #:mnas-spring)

(defgeneric i-s     (spring)      (:documentation "Индекс пружины"))
(defgeneric k-1     (spring)      (:documentation "k-1 - Коэффициент прочности пружины"))
(defgeneric k-2     (spring)      (:documentation "k-2 - Коэффициент жесткости пружины"))
(defgeneric n-f     (spring)      (:documentation "Количество полных витков пружины"))
(defgeneric set-n-f (spring n-f)  (:documentation "Устанавливает полное число витков пружины равным n-f за счёт изменения числа рабочих витков пружины"))
(defgeneric d-o     (spring)      (:documentation "Наружный диаметр пружины"))
(defgeneric set-d-o (spring d-o)  (:documentation "Устанавливает наружный диаметр пружины равным d-o за счет изменения среднего диаметра витков"))
(defgeneric d-i     (spring)      (:documentation "Внутренний диаметр пружины"))
(defgeneric set-d-i (spring d-i)  (:documentation "Устанавливает внутренний диаметр пружины равным d-i за счет изменения среднего диаметра витков"))
(defgeneric t-s     (spring)      (:documentation "Шаг пружины в свободном сосотоянии"))
(defgeneric set-t-s (spring t-s)  (:documentation "Устанавливает шаг витков пружины равным t-s за счет изменения длины пружины"))
(defgeneric l-w     (spring)      (:documentation "Длина проволоки развернутой пружины"))
(defgeneric l-4     (spring)      (:documentation "Длина пружины при соприкосновении витков"))
(defgeneric ro-w[kg/mm3] (spring) (:documentation "Плотность материала проволоки пружины"))
(defgeneric mass-s  (spring)      (:documentation "Масса пружины"))
(defgeneric li-si   (spring si)   (:documentation "Высота пружины при деформации si"))
(defgeneric G[MPa]  (spring)      (:documentation "Модуль Юнга второго рода в МПа для материала пружины"))
(defgeneric fi-si   (spring si)   (:documentation "Сила, необходимая для деформации пружины на величину si"))
(defgeneric taui-fi (spring fi)   (:documentation "Напряжение при кручении при приложении к пружине силы fi"))
(defgeneric fi-taui (spring taui) (:documentation "Сила при которой напряжения при кручении равны taui"))
(defgeneric si-fi   (spring fi)   (:documentation "Деформация пружины под нагрузкой"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric fi-li     (spring X) (:documentation "Возвращает силу для придания пружине spring длины X"))
(defgeneric fi-min-li (spring X) (:documentation "Возвращает минимальное значение силы для придания пружине spring длины X"))
(defgeneric fi-max-li (spring X) (:documentation "Возвращает максимальное значение силы для придания пружине spring длины X"))
