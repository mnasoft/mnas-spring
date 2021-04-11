;;;; ./src/gtk/dialog.lisp

(defpackage :mnas-spring/gtk
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp
        :mnas-spring)
  (:export spring-culc
           ))

(in-package :mnas-spring/gtk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <spring-dlg> (<spring>)
  ((l-1 :accessor <spring-dlg>-l-1 :initarg :l-1 :initform 25 :documentation "Высота пружины под нагрузной f-1, мм")
   (l-2 :accessor <spring-dlg>-l-2 :initarg :l-2 :initform 20 :documentation "Высота пружины под нагрузной f-2, мм")
   (l-3 :accessor <spring-dlg>-l-3 :initarg :l-3 :initform 15 :documentation "Высота пружины под нагрузной f-3, мм"))
  (:documentation "@b(Описание:) класс @b(<spring-dlg>) содержит дополнительные слоты для работы диалога."))

(defmethod print-object :before ((x <spring-dlg>) s) (format s "#<spring-dlg>" ))

(defmethod print-object         ((x <spring-dlg>) s)
  (call-next-method)
  (format s "(l-1=~S l-2=~S l-3=~S)"
          (<spring-dlg>-l-1 x)
          (<spring-dlg>-l-2 x)
          (<spring-dlg>-l-3 x)))

(defparameter *s-dlg* (make-instance '<spring-dlg>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf <spring-dlg>-copy) ((ref <spring-dlg>) (spr <spring-dlg>))
  (setf (<spring>-l-0 spr) (<spring>-l-0 ref))
  (setf (<spring>-d-m spr) (<spring>-d-m ref))
  (setf (<spring>-d-w spr) (<spring>-d-w ref))
  (setf (<spring>-n-w spr) (<spring>-n-w ref))
  (setf (<spring>-m-w spr) (<spring>-m-w ref))
  (setf (<spring-dlg>-l-1 spr) (<spring-dlg>-l-1 ref))
  (setf (<spring-dlg>-l-2 spr) (<spring-dlg>-l-2 ref))
  (setf (<spring-dlg>-l-3 spr) (<spring-dlg>-l-3 ref))
  spr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring-dlg>-s-1 ((spring-dlg <spring-dlg>))
  (<spring>-si-li spring-dlg (<spring-dlg>-l-1 spring-dlg)))

(defmethod <spring-dlg>-s-2 ((spring-dlg <spring-dlg>))
  (<spring>-si-li spring-dlg (<spring-dlg>-l-2 spring-dlg)))

(defmethod <spring-dlg>-s-3 ((spring-dlg <spring-dlg>))
  (<spring>-si-li spring-dlg (<spring-dlg>-l-3 spring-dlg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf <spring-dlg>-s-1) (s1 (spring-dlg <spring-dlg>))
  (setf (<spring-dlg>-l-1 spring-dlg) (- (<spring>-l-0 spring-dlg) s1))
   spring-dlg)

(defmethod (setf <spring-dlg>-s-2) (s2 (spring-dlg <spring-dlg>))
  (setf (<spring-dlg>-l-2 spring-dlg) (- (<spring>-l-0 spring-dlg) s2))
  spring-dlg)

(defmethod (setf <spring-dlg>-s-3) (s3 (spring-dlg <spring-dlg>))
  (setf (<spring-dlg>-l-3 spring-dlg) (- (<spring>-l-0 spring-dlg) s3))
  spring-dlg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring-dlg>-f-1 ((spring-dlg <spring-dlg>))
  (<spring>-fi-li spring-dlg (<spring-dlg>-l-1 spring-dlg)))

(defmethod <spring-dlg>-f-2 ((spring-dlg <spring-dlg>))
  (<spring>-fi-li spring-dlg (<spring-dlg>-l-2 spring-dlg)))

(defmethod <spring-dlg>-f-3 ((spring-dlg <spring-dlg>))
  (<spring>-fi-li spring-dlg (<spring-dlg>-l-3 spring-dlg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf <spring-dlg>-f-1) (f1 (spring-dlg <spring-dlg>))
  (setf (<spring-dlg>-l-1 spring-dlg) (<spring>-li-fi spring-dlg f1))
  spring-dlg)

(defmethod (setf <spring-dlg>-f-2) (f2 (spring-dlg <spring-dlg>))
  (setf (<spring-dlg>-l-2 spring-dlg) (<spring>-li-fi spring-dlg f2))
  spring-dlg)

(defmethod (setf <spring-dlg>-f-3) (f3 (spring-dlg <spring-dlg>))
  (setf (<spring-dlg>-l-3 spring-dlg) (<spring>-li-fi spring-dlg f3))
  spring-dlg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod <spring-dlg>-τ-1 ((spring-dlg <spring-dlg>))
  (<spring>-taui-fi spring-dlg (<spring-dlg>-f-1 spring-dlg)))

(defmethod <spring-dlg>-τ-2 ((spring-dlg <spring-dlg>))
  (<spring>-taui-fi spring-dlg (<spring-dlg>-f-2 spring-dlg)))

(defmethod <spring-dlg>-τ-3 ((spring-dlg <spring-dlg>))
  (<spring>-taui-fi spring-dlg (<spring-dlg>-f-3 spring-dlg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf <spring-dlg>-τ-1) (τ1 (spring-dlg <spring-dlg>))
  (setf (<spring-dlg>-f-1 spring-dlg) (<spring>-fi-taui spring-dlg τ1)))

(defmethod (setf <spring-dlg>-τ-2) (τ2 (spring-dlg <spring-dlg>))
  (setf (<spring-dlg>-f-2 spring-dlg) (<spring>-fi-taui spring-dlg τ2)))

(defmethod (setf <spring-dlg>-τ-3) (τ3 (spring-dlg <spring-dlg>))
  (setf (<spring-dlg>-f-3 spring-dlg) (<spring>-fi-taui spring-dlg τ3)))


(defmethod <spring-dlg>-h ((spring-dlg <spring-dlg>))
  "Рабочий ход пружины"
  (- (<spring-dlg>-l-1 spring-dlg) (<spring-dlg>-l-2 spring-dlg)))

(defmethod <spring-dlg>-c ((spring-dlg <spring-dlg>))
  "Жесткость пружины"
  (let ((h (<spring-dlg>-h spring-dlg)))
    (if (<= (abs h) 0.05)
        0.0
        (/ (- (<spring-dlg>-f-2 spring-dlg)
              (<spring-dlg>-f-1 spring-dlg))
           h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro str (method obj)
;;  `(format nil "~A" (,method ,obj))  
  `(mnas-format:round-val (,method ,obj)))

(require :mnas-format)


(defmacro add-entry (row lbl entry)
  `(progn (gtk-box-pack-start ,row (gtk-label-new ,lbl))
          (gtk-box-pack-start ,row ,entry)))

(defmacro gsc-activate (entry setf-method obj)
  `(g-signal-connect ,entry "activate"
                            (lambda (widget)
                              (declare (ignore widget))
                              (setf (,setf-method ,obj)
                                    (mnas-string/parse:parse-number (gtk-entry-text ,entry)))
                              (update-spring))))

(defmacro file-img (path-string)
  `(make-instance 'gtk-image
                  :file (namestring (asdf:system-relative-pathname :mnas-spring ,path-string))))

(defmacro entry-file-tip (entry file tip)
  `(setf (gtk-entry-primary-icon-pixbuf ,entry) (gtk-image-pixbuf (file-img ,file))
         (gtk-entry-primary-icon-tooltip-text ,entry) ,tip))

(defparameter *license-text*
  "Распространяется по лицензии GNU GPL версии 3 или более высокой.")

;;;;(make-instance 'gtk-message-dialog :text *info*)

(defun create-about-dialog ()
  (let ((dialog (make-instance 'gtk-about-dialog
                               :program-name  "MNAS-SPRING-Dialog"
                               :version       (mnas-package/sys:version :mnas-spring)
                               :copyright     "(ɔ) Nick Matvyeyev, mnasoft@gmail.com"
                               :website       "https://github.com/mnasoft/mnas-spring"
                               :website-label "Project web site"
                               :license       *license-text* 
                               ;;:authors       '("Nick Matvyeyev, mnasoft@gmail.com")
                               ;;:documenters '("Nick Matvyeyev, mnasoft@gmail.com")
                               ;;:artists     '("Nick Matvyeyev, mnasoft@gmail.com")
                               :logo          (gtk-image-pixbuf (file-img "img/mnas-logo.png"))
                               :icon          (gtk-image-pixbuf (file-img "img/gtk-spring.bmp"))
                               ;; :logo-icon-name "applications-development"
                               :wrap-license t)))
    (gtk-dialog-run dialog)
    (gtk-widget-destroy dialog)))

(defun create-about-dialog-01 (website website-label)
  (let ((dialog (make-instance 'gtk-about-dialog
                               :program-name  "MNAS-SPRING-Dialog"
                               :website       website
                               :website-label website-label
                               :logo          (gtk-image-pixbuf (file-img "img/mnas-logo.png"))
                               :icon          (gtk-image-pixbuf (file-img "img/gtk-spring.bmp"))
                               )))
    (gtk-dialog-run dialog)
    (gtk-widget-destroy dialog)))


(defun create-message-dialog (text secondary-text)
  (let ((dialog (make-instance 'gtk-message-dialog
                               :message-type :info
                               :buttons :ok
                               :text text
                               :modal t
                               :secondary-text (format nil secondary-text))))
    (gtk-dialog-run dialog)
    (gtk-widget-destroy dialog)))


(defparameter *tbl*
  '((<spring>-d-w     "d"   "мм"   "Диаметр проволоки")
    (<spring>-d-o     "D1"  "мм"   "Диаметр пружины наружный")
    (<spring>-d-m     "D"   "мм"   "Диаметр пружины средний")
    (<spring>-l-0     "L0"  "мм"   "Высота пружины в свободном состоянии")
    (<spring-dlg>-l-1 "L1"  "мм"   "Высота пружины при предварительной деформации")
    (<spring-dlg>-l-2 "L2"  "мм"   "Высота пружины при рабочей деформации")
    (<spring-dlg>-l-3 "L3"  "мм"   "Высота пружины при максимальной деформации")
    (<spring>-l-4     "L4"  "мм"   "Высота пружины при соприкосновении витков")
    (<spring-dlg>-s-1 "S1"  "мм"   "Предварительная деформация пружины")
    (<spring-dlg>-s-2 "S2"  "мм"   "Рабочая деформация пружины" )
    (<spring-dlg>-s-3 "S3"  "мм"   "Максимальная деформация пружины" )
    (<spring>-t-s     "t"   "мм"   "Шаг пружины в свободном состоянии" )
    (<spring>-s-k     "Sk"  "мм"   "Толщина конца опорного витка пружины")
    (<spring-dlg>-h   "h"   "мм"   "Рабочий ход пружины")
    (<spring>-l-w     "L"   "мм"   "Длина проволоки развернутой пружины")
    (<spring>-n-w     "n"   "1"    "Число витков пружины")
    (<spring>-n-f     "n1"  "1"    "Полное число витков пружины")
    (<spring>-i-s     "i"   "1"    "Индекс пружины")
    (<spring>-k-1     "K1"  "1"    "Коэффициент, учитывающий влияние кривизны витка")
    (<spring>-k-2     "K2"  "1"    "Коэффициент, учитывающий влияние поперечной силы")
    (<spring-dlg>-f-1 "F1"  "Н"    "Сила пружины при предварительной деформации")
    (<spring-dlg>-f-2 "F2"  "Н"    "Сила пружины при рабочей деформации")
    (<spring-dlg>-f-3 "F3"  "Н"    "Сила пружины при максимальной деформации")
    (<spring-dlg>-τ-1 "τ1"  "МПа"  "Напряжение при кручении при предварительной деформации")
    (<spring-dlg>-τ-2 "τ2"  "МПа"  "Напряжение при кручении при рабочей деформации")
    (<spring-dlg>-τ-3 "τ3"  "МПа"  "Напряжение при кручении при максимальной деформации")
    ;; (<spring-dlg>-[τ] "[τ]" "МПа"  "Допускаемое напряжение при кручении")
    (<spring-dlg>-c   "C"   "Н/мм" "Жесткость пружины")
    (<spring>-g[mpa]  "G"   "МПа"  "Модуль сдвига")
    (<spring>-i-1     "i1"  "1"    "Отношение длины пружины в свободном состоянии к её среднему диаметру")
    (<spring>-mass-s  "m"   "кг"   "Масса пружины")))

(defun spring-dialog ()
  (within-main-loop
    (let ((spr (setf (<spring-dlg>-copy (make-instance '<spring-dlg>)) *s-dlg*)))
      (let ((window   (make-instance 'gtk-window :type :toplevel :title "Расчет пружины" :border-width 12
                                                 :width-request 100 :height-request 10
                                                 :icon (gtk-image-pixbuf (file-img "img/gtk-spring.bmp"))
                                                 ))
            (v-box-01 (make-instance 'gtk-box        :orientation :vertical :spacing 3))
            (frame-01  (make-instance 'gtk-frame     :label "Геометрия пружинны"))
            (col-01     (make-instance 'gtk-box      :orientation :vertical :spacing 3))
            (row-11      (make-instance 'gtk-box     :orientation :horizontal :spacing 3))
            (e-d-w        (make-instance 'gtk-entry  :max-length 10 :width-chars 10))
            (e-d-o        (make-instance 'gtk-entry  :max-length 10 :width-chars 10))
            (e-d-m        (make-instance 'gtk-entry  :max-length 10 :width-chars 10))
            (row-12      (make-instance 'gtk-box     :orientation :horizontal :spacing 3))
            (e-n-w        (make-instance 'gtk-entry  :max-length 10 :width-chars 10))
            (e-n-f        (make-instance 'gtk-entry  :max-length 10 :width-chars 10))
            (row-13      (make-instance 'gtk-box     :orientation :horizontal :spacing 3))
            (e-t-s        (make-instance 'gtk-entry  :max-length 10 :width-chars 10))
            (e-l-0        (make-instance 'gtk-entry  :max-length 10 :width-chars 10))
            (e-l-4        (make-instance 'gtk-entry  :max-length 10 :width-chars 10))
            (frame-02  (make-instance 'gtk-frame     :label "Характеристики материала"))
            (col-02     (make-instance 'gtk-box      :orientation :vertical :spacing 3))
            (row-21      (make-instance 'gtk-box     :orientation :horizontal :spacing 3))
            (e-g[mpa]     (make-instance 'gtk-entry  :max-length 10 :width-chars 10))
            (e-τ-s        (make-instance 'gtk-entry  :text "Hello" :max-length 10 :width-chars 10))
            (frame-03   (make-instance 'gtk-frame    :label "Характеристики пружины"))
            (col-03      (make-instance 'gtk-box     :orientation :vertical :spacing 3))
            (row-31       (make-instance 'gtk-box    :orientation :horizontal :spacing 3))
            (e-L1-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-S1-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-F1-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-τ1-s        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (row-32       (make-instance 'gtk-box    :orientation :horizontal :spacing 3))
            (e-L2-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-S2-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-F2-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-τ2-s        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (row-33       (make-instance 'gtk-box    :orientation :horizontal :spacing 3))
            (e-L3-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-S3-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-F3-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-τ3-s        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (row-34       (make-instance 'gtk-box    :orientation :horizontal :spacing 3))
            (e-h-s        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-C-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (frame-04   (make-instance 'gtk-frame    :label "Некоторые коэффициенты"))
            (col-04      (make-instance 'gtk-box     :orientation :vertical :spacing 3))
            (row-41       (make-instance 'gtk-box    :orientation :horizontal :spacing 3))
            (e-k-1         (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-k-2         (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-i-s         (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-i-1         (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (row-42       (make-instance 'gtk-box    :orientation :horizontal :spacing 3))
            (e-l-w         (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-mass-s      (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            (e-Sk-b        (make-instance 'gtk-entry :max-length 10 :width-chars 10))
            ;;
            (row-61     (make-instance 'gtk-box      :orientation :horizontal :spacing 3))
            (btn-draw    (make-instance 'gtk-button :image-position :left :always-show-image t :image (file-img "img/gtk-dxf-out.bmp") :label "Draw"))
            
            )
        (labels ((update-spring ()
                   (setf (gtk-entry-text e-d-w) (str <spring>-d-w spr))
                   (setf (gtk-entry-text e-d-o) (str <spring>-d-o spr))
                   (setf (gtk-entry-text e-d-m) (str <spring>-d-m spr))
                   (setf (gtk-entry-text e-n-w) (str <spring>-n-w spr))
                   (setf (gtk-entry-text e-n-f) (str <spring>-n-f spr))
                   (setf (gtk-entry-text e-t-s) (str <spring>-t-s spr))
                   (setf (gtk-entry-text e-l-0) (str <spring>-l-0 spr))
                   (setf (gtk-entry-text e-l-4) (str <spring>-l-4 spr))
                   (setf (gtk-entry-text e-g[mpa]) (str <spring>-g[mpa] spr))
                   (setf (gtk-entry-text e-k-1) (str <spring>-k-1 spr))
                   (setf (gtk-entry-text e-k-2) (str <spring>-k-2 spr))
                   (setf (gtk-entry-text e-i-s) (str <spring>-i-s spr))
                   (setf (gtk-entry-text e-i-1) (str <spring>-i-1 spr))
                   (setf (gtk-entry-text e-l-w) (str <spring>-l-w spr))
                   (setf (gtk-entry-text e-mass-s) (str <spring>-mass-s spr))
                   (setf (gtk-entry-text e-Sk-b) (str <spring>-s-k spr))
                   ;;
                   (setf (gtk-entry-text e-L1-b) (str <spring-dlg>-l-1 spr))
                   (setf (gtk-entry-text e-S1-b) (str <spring-dlg>-s-1 spr))
                   (setf (gtk-entry-text e-F1-b) (str <spring-dlg>-f-1 spr))
                   (setf (gtk-entry-text e-τ1-s) (str <spring-dlg>-τ-1 spr))
                   ;;
                   (setf (gtk-entry-text e-L2-b) (str <spring-dlg>-l-2 spr))
                   (setf (gtk-entry-text e-S2-b) (str <spring-dlg>-s-2 spr))
                   (setf (gtk-entry-text e-F2-b) (str <spring-dlg>-f-2 spr))
                   (setf (gtk-entry-text e-τ2-s) (str <spring-dlg>-τ-2 spr))
                   ;;
                   (setf (gtk-entry-text e-L3-b) (str <spring-dlg>-l-3 spr))
                   (setf (gtk-entry-text e-S3-b) (str <spring-dlg>-s-3 spr))
                   (setf (gtk-entry-text e-F3-b) (str <spring-dlg>-f-3 spr))
                   (setf (gtk-entry-text e-τ3-s) (str <spring-dlg>-τ-3 spr))
                   (setf (gtk-entry-text e-h-s)  (str <spring-dlg>-h   spr))
                   (setf (gtk-entry-text e-C-b)  (str <spring-dlg>-C   spr))
                   )
                 (init-entry ()
                   (entry-file-tip e-d-w    "img/gtk-help.bmp" "Диаметр проволоки")
                   (entry-file-tip e-d-o    "img/gtk-help.bmp" "Наружный диаметр пружины")
                   (entry-file-tip e-d-m    "img/gtk-help.bmp" "Средний диаметр пружины")
                   (entry-file-tip e-n-w    "img/gtk-help.bmp" "Количество рабочих витков пружины")
                   (entry-file-tip e-n-f    "img/gtk-help.bmp" "Общее количество витков пружины")
                   (entry-file-tip e-t-s    "img/gtk-help.bmp" "Шаг витков пружины")
                   (entry-file-tip e-l-0    "img/gtk-help.bmp" "Высота пружины в свободном состоянии")
                   (entry-file-tip e-l-4    "img/gtk-help.bmp" "Высота пружины при соприкосновении витков")
                   (entry-file-tip e-g[mpa] "img/gtk-help.bmp" "Модуль Юнга второго рода")
                   (entry-file-tip e-k-1    "img/gtk-help.bmp" "Коэффициент прочности пружины")
                   (entry-file-tip e-k-2    "img/gtk-help.bmp" "Коэффициент жесткости пружины")
                   (entry-file-tip e-i-s    "img/gtk-help.bmp" "Индекс пружины")
                   (entry-file-tip e-i-1    "img/gtk-help.bmp" "Относительная высота пружины")
                   (entry-file-tip e-l-w    "img/gtk-help.bmp" "Длина проволоки развернутой пружины")
                   (entry-file-tip e-mass-s "img/gtk-help.bmp" "Массу пружины")
                   (entry-file-tip e-L1-b   "img/gtk-help.bmp" "Высота пружины под нагрузкой F1")
                   (entry-file-tip e-S1-b   "img/gtk-help.bmp" "Деформация пружины от нагрузки F1")
                   (entry-file-tip e-F1-b   "img/gtk-help.bmp" "Нагрузка F1")
                   (entry-file-tip e-τ1-s   "img/gtk-help.bmp" "Касательные напряжения от нагрузки F1")
                   (entry-file-tip e-L2-b   "img/gtk-help.bmp" "Высота пружины под нагрузкой F2")
                   (entry-file-tip e-S2-b   "img/gtk-help.bmp" "Деформация пружины от нагрузки F2")
                   (entry-file-tip e-F2-b   "img/gtk-help.bmp" "Нагрузка F2")
                   (entry-file-tip e-τ2-s   "img/gtk-help.bmp" "Касательные напряжения от нагрузки F2")
                   (entry-file-tip e-L3-b   "img/gtk-help.bmp" "Высота пружины под нагрузкой F3")
                   (entry-file-tip e-S3-b   "img/gtk-help.bmp" "Деформация пружины от нагрузки F3")
                   (entry-file-tip e-F3-b   "img/gtk-help.bmp" "Нагрузка F3")
                   (entry-file-tip e-τ3-s   "img/gtk-help.bmp" "Касательные напряжения от нагрузки F3")
                   ;;
                   (entry-file-tip e-Sk-b "img/gtk-help.bmp" "Толщина конца последнего витка пружины")
                   (entry-file-tip e-h-s "img/gtk-help.bmp" "Рабочий ход пружины")
                   (entry-file-tip e-C-b "img/gtk-help.bmp" "Жёсткость пружины")
                   ))
          (update-spring)
          (init-entry)
          (g-signal-connect window "destroy" (lambda (widget) (declare (ignore widget)) (leave-gtk-main)))
          ;;
          (gsc-activate e-d-w <spring>-d-w spr)
          (gsc-activate e-d-o <spring>-d-o spr)
          (gsc-activate e-d-m <spring>-d-m spr)
          (gsc-activate e-n-w <spring>-n-w spr)
          (gsc-activate e-n-f <spring>-n-f spr)
          (gsc-activate e-t-s <spring>-t-s spr)
          (gsc-activate e-l-0 <spring>-l-0 spr)
          ;;
          (gsc-activate e-L1-b <spring-dlg>-l-1 spr)
          (gsc-activate e-L2-b <spring-dlg>-l-2 spr)
          (gsc-activate e-L3-b <spring-dlg>-l-3 spr)

          (gsc-activate e-S1-b <spring-dlg>-s-1 spr)
          (gsc-activate e-S2-b <spring-dlg>-s-2 spr)
          (gsc-activate e-S3-b <spring-dlg>-s-3 spr)
          ;;
          (gsc-activate e-F1-b <spring-dlg>-f-1 spr)
          (gsc-activate e-F2-b <spring-dlg>-f-2 spr)
          (gsc-activate e-F3-b <spring-dlg>-f-3 spr)
          ;;
          (gsc-activate e-τ1-s <spring-dlg>-τ-1 spr)
          (gsc-activate e-τ2-s <spring-dlg>-τ-2 spr)
          (gsc-activate e-τ3-s <spring-dlg>-τ-3 spr)
          ;;
          (gsc-activate e-i-s <spring>-i-s spr)
          (gsc-activate e-i-1 <spring>-i-1 spr)

          (block col-01
            (block row-11
              (add-entry row-11 "d, мм"  e-d-w)
              (add-entry row-11 "D1, мм" e-d-o)
              (add-entry row-11 "D, мм"  e-d-m)
              (gtk-box-pack-start col-01 row-11))
            (block row-12
              (add-entry row-12 "n, 1"   e-n-w)
              (add-entry row-12 "n1, 1"  e-n-f)
              (gtk-box-pack-start col-01 row-12))
            (block row-13
              (add-entry row-13 "t, мм" e-t-s)
              (add-entry row-13 "L0, mm" e-l-0)
              (add-entry row-13 "L4, mm" e-l-4)
              (gtk-box-pack-start col-01 row-13)))
          (block col-02
            (block row-21
              (add-entry row-21 "G, МПа"   e-g[mpa])
              (add-entry row-21 "[τ], МПа" e-τ-s)
              (gtk-box-pack-start col-02 row-21)))
          (block col-03
            (block row-31
              (add-entry row-31 "L1, мм"  e-L1-b)
              (add-entry row-31 "S1, мм"  e-S1-b)
              (add-entry row-31 "F1, Н"   e-F1-b)
              (add-entry row-31 "τ1, МПа" e-τ1-s)
              (gtk-box-pack-start col-03 row-31))
            (block row-32
              (add-entry row-32 "L2, мм"  e-L2-b)
              (add-entry row-32 "S2, мм"  e-S2-b)
              (add-entry row-32 "F2, Н"   e-F2-b)
              (add-entry row-32 "τ2, МПа" e-τ2-s)
              (gtk-box-pack-start col-03 row-32))
            (block row-33
              (add-entry row-33 "L3, мм"  e-L3-b)
              (add-entry row-33 "S3, мм"  e-S3-b)
              (add-entry row-33 "F3, Н"   e-F3-b)
              (add-entry row-33 "τ3, МПа" e-τ3-s)
              (gtk-box-pack-start col-03 row-33))
            (block row-34
              (add-entry row-34 "h, мм"   e-h-s)
              (add-entry row-34 "C, H/мм" e-C-b)
              (gtk-box-pack-start col-03 row-34)))
          (block col-04
            (block row-41
              (add-entry row-41 "K1, 1" e-k-1)
              (add-entry row-41 "K2, 1" e-k-2)
              (add-entry row-41 "i, 1"  e-i-s)
              (add-entry row-41 "i1, 1" e-i-1)
              (gtk-box-pack-start col-04 row-41))
            (block row-42
              (add-entry row-42 "Lw, мм" e-l-w)
              (add-entry row-42 "m, кг"  e-mass-s)
              (add-entry row-42 "Sk, мм" e-Sk-b)
              (gtk-box-pack-start col-04 row-42)))
          (block row-61
            (let ((btn-ok (make-instance 'gtk-button :image-position :left
                                                     :always-show-image t
                                                     :image (make-instance 'gtk-image :icon-name "gtk-ok") :label "Ok")))
              (gtk-box-pack-start row-61 btn-ok)
              (g-signal-connect btn-ok "clicked" (lambda (widget) (declare (ignore widget))
                                                   (setf *s-dlg* spr)
                                                   (gtk-widget-destroy window))))
            (let ((btn-cancel  (make-instance 'gtk-button :image-position :left
                                                          :always-show-image t
                                                          :image (make-instance 'gtk-image :icon-name "gtk-cancel") :label "Cancel")))
              (gtk-box-pack-start row-61 btn-cancel)
              (g-signal-connect btn-cancel "clicked" (lambda (widget) (declare (ignore widget))
                                                       (gtk-widget-destroy window))))
            (let ((btn-print   (make-instance 'gtk-button :image-position
                                              :left :always-show-image t :image (file-img "img/gtk-print.bmp") :label "Print")))
              (gtk-box-pack-start row-61 btn-print)
              (g-signal-connect btn-print "clicked" (lambda (widget) (declare (ignore widget))
                                                      (setf (cl-who:html-mode) :html5)
                                                      (let ((f-name "~/Spring.html"))
                                                        (with-open-file (os f-name :direction :output :if-exists :supersede)
                                                          (cl-who:with-html-output (str os :prologue t :indent t)
                                                            (:html
                                                             (:head (:meta :charset "UTF-8")
                                                                    (:title "Результаты расчета пружины"))
                                                             (:body
                                                              (:table :border 2 :cellpadding 5 :cellspacing 5
                                                                      (:tr (:th "Наименование") (:th "Об.") (:th "Разм.") (:th "Знач."))
                                                                      (loop :for (func des dim naim) :in *tbl*
                                                                            do (cl-who:htm
                                                                                (:tr (:td (cl-who:str naim))
                                                                                     (:td (cl-who:str des))
                                                                                     (:td (cl-who:str dim))
                                                                                     (:td (cl-who:str (mnas-format:round-val
                                                                                                       (funcall func spr))))))))))))
                                                        f-name)
                                                      (create-about-dialog-01
                                                       (concatenate 'string "file:///" (namestring (truename "~/Spring.html")))
                                                       "Результаты расчета пружины"))))
            
            (gtk-box-pack-start row-61 btn-draw)
            (let ((btn-help (make-instance 'gtk-button :image-position :left
                                                       :always-show-image t
                                                       :image (file-img "img/gtk-help.bmp") :label "Help")))
              (gtk-box-pack-start row-61 btn-help)
              (g-signal-connect btn-help "clicked" (lambda (widget) (declare (ignore widget))
                                                     (create-about-dialog-01
                                                      "https://mnasoft.ddns.mksat.net/Common-Lisp-Programs/mnas-spring/обзор.html"
                                                      "Документация к проекту Mnas-Spring"))))
            (let ((btn-info (make-instance 'gtk-button :image-position :left
                                                       :always-show-image t
                                                       :image (file-img "img/gtk-info.bmp") :label "About")))
              (gtk-box-pack-start row-61 btn-info)
              (g-signal-connect btn-info "clicked" (lambda (widget) (declare (ignore widget)) (create-about-dialog)))))

          (gtk-container-add frame-01 col-01)
          (gtk-container-add frame-02 col-02)
          (gtk-container-add frame-03 col-03)
          (gtk-container-add frame-04 col-04)
      
          (gtk-container-add v-box-01 frame-01)
          (gtk-container-add v-box-01 frame-02)
          (gtk-container-add v-box-01 frame-03)
          (gtk-container-add v-box-01 frame-04)
          (gtk-container-add v-box-01 row-61)

          (gtk-container-add window v-box-01)
          (gtk-widget-show-all window))))))

(defun spring-culc ()
    "@b(Описание:) функция @b(spring-culc) запускает на выполнение
     диалоговое окно, предназначенное для расчета пружины."
  #-sbcl
  (spring-dialog)

  #+sbcl
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (spring-dialog)))

;;;; (spring-culc)

;;;;*s-dlg*
