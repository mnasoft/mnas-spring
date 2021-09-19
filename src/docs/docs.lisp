;;;; ./src/docs/docs.lisp

(defpackage #:mnas-spring/docs
  (:use #:cl ) 
  (:nicknames "MSPR/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(mnas-spring/docs) содержит функции
  генерирования и публикации документации."))

(in-package :mnas-spring/docs)

(defun make-document ()
  (loop
    :for i :in
    '((:mnas-spring     :mnas-spring)
      (:mnas-spring/gtk :mnas-spring/gtk)
      #+nil
      (:mnas-spring/db  :mnas-spring/db)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:mnas-spring
      :mnas-spring/gtk
      #+nil
      :mnas-spring/db)
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/mnas-spring.
"
  (mnas-package:make-html-path :mnas-spring)
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp
   '(:mnas-spring :mnas-spring/docs)
   "Mnas-Spring"
   '("Nick Matvyeyev")
   (mnas-package:find-sources "mnas-spring")
   :output-format of)
  (codex:document :mnas-spring)
  (make-graphs)
  (mnas-package:copy-doc->public-html "mnas-spring")
  (mnas-package:rsync-doc "mnas-spring"))

;;;; (make-all)
