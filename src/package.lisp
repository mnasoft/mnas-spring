;;;; package.lisp
 
(defpackage #:mnas-spring
  (:nicknames "MSPR")
  (:use #:cl #:ltk #:mnas-logical)
  (:export spring-dialog
	   )
 
  (:export spring 
	   l-0 
	   d-m 
	   d-w 
	   n-w 
	   m-w
	   )
  (:export
  	   *G-lst*
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
  )

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
