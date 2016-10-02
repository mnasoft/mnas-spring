;;;; mnas-spring.asd

(asdf:defsystem #:mnas-spring
    :description "Describe mnas-spring here"
    :author "Your Name <your.name@example.com>"
    :license "Specify license here"
    :serial t
    :depends-on (#:ltk #:mnas-logical)
    :components ((:file "package")
		 (:file "mnas-spring")
		 (:file "class-spring")
		 (:file "macro")
		 (:file "mnas-spring-dialog")))
