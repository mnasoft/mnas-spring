;;;; mnas-spring.asd

(defsystem "mnas-spring"
  :description "Система mnas-spring предназачена для выполнения
                расчета пружин сжатия."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :version "0.10.14"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"

  :defsystem-depends-on ("deploy")
  :build-operation "deploy-op"
  :build-pathname "mnas-spring"
  :entry-point "mnas-spring/gtk:dialog"
  
  :serial nil
  :depends-on ("mnas-spring/core"
               "mnas-spring/db"
               "mnas-spring/gtk"
               ;;"mnas-spring/ltk"
               ))

(defsystem "mnas-spring/core"
  :description
  "Система mnas-spring предназачена для выполнения расчета пружин
   сжатия."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"    
  :serial nil
  :depends-on ("mnas-logical")
  :components ((:module "src/core" 
                :serial t
                :components ((:file "spring")))))

(defsystem "mnas-spring/db"
  :description
  "Система mnas-spring содержит характеристики некоторых пружин сжатия,
   применяемых ZM."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"    
  :serial nil
  :depends-on ("mnas-spring/core")
  :components ((:module "src/db" 
                :serial t
                :components ((:file "db")
                             ))))

(defsystem "mnas-spring/gtk"
  :description
  "Система mnas-spring/ltk реализует диалоговое окно, предназначенное
   для расчета пружины сжатия."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"    
  :serial nil
  :depends-on ("mnas-package/sys" "mnas-spring/core" "cl-cffi-gtk" "mnas-string" "mnas-format" "cl-who")
  :components ((:module "src/gtk" 
                :serial t
                :components ((:file "dialog")
	                     ))))

(defsystem "mnas-spring/ltk"
  :description "Система mnas-spring/ltk реализует диалоговое окно,
                преддназначенное для расчета пружины сжатия."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"    
  :serial nil
  :depends-on ("mnas-spring/core" "mnas-logical" "ltk" "mnas-spring/ltk-macro")
  :components ((:module "src/ltk" 
                :serial t
                :components ((:file "mnas-spring-dialog")
	                     (:file "mnas-spring-dialog-01") 
	                     ))))

(defsystem "mnas-spring/ltk-macro"
  :description "Система mnas-spring/ltk реализует диалоговое окно,
                преддназначенное для расчета пружины сжатия."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"    
  :serial nil
  :depends-on ("mnas-spring/core" "mnas-logical" "ltk")
  :components ((:module "src/ltk-macro" 
                :serial t
                :components ((:file "macro")
	                     ))))

(defsystem "mnas-spring/docs"
  :description
  "Система mnas-spring/doc предназачена для подготовки документации."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"    
  :serial nil
  :depends-on ("mnas-spring" "codex" "mnas-package")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))
