;;;; mnas-spring.asd

(defsystem #:mnas-spring
  :description "Describe mnas-spring here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"    
  :serial nil
  :depends-on (#:ltk #:mnas-logical)
  :components ((:module "src" 
                :serial t
                :components ((:file "package")
	                     (:file "defparameters") 
	                     (:file "classes")
	                     (:file "defgenerics")
	                     (:file "defmethods")
	                     (:file "mnas-spring")
	                     (:file "class-spring")
	                     (:file "macro")
	                     (:file "mnas-spring-dialog")
	                     (:file "mnas-spring-dialog-01")	       
      	                     (:file "test")
	                     ))))
