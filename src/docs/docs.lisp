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
                                 :test #'string= :key #'first)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (let* ((sys-symbol :mnas-spring)
         (sys-string (string-downcase (format nil "~a" sys-symbol))))
    (mnas-package:make-html-path sys-symbol)
    (make-document)
    (mnas-package:make-mainfest-lisp `(,sys-symbol)
                                     (string-capitalize sys-string)
                                     '("Mykola Matvyeyev")
                                     (mnas-package:find-sources sys-symbol)
                                     :output-format of)
    (codex:document sys-symbol)
    (make-graphs)
    (mnas-package:copy-doc->public-html sys-string)
    (mnas-package:rsync-doc sys-string)
    :make-all-finish))

;;;; (make-all)
