;;;; package.lisp

(defpackage #:mnas-spring)
  
(defpackage #:mnas-spring
  (:nicknames "MSPR")
  (:use #:cl #:ltk #:mnas-logical)
  (:export mnas-spring::spring-dialog
	   )
 
  (:export mnas-spring::spring 
	   mnas-spring::l-0 
	   mnas-spring::d-m 
	   mnas-spring::d-w 
	   mnas-spring::n-w 
	   mnas-spring::m-w
	   )
  (:export
  	   mnas-spring::*G-lst*
	   mnas-spring::*s*
	   )
  (:export mnas-spring::i-s 
	   mnas-spring::k-1 
	   mnas-spring::k-2 
	   mnas-spring::n-f 
	   mnas-spring::set-n-f 
	   mnas-spring::d-o
	   mnas-spring::set-d-o
	   mnas-spring::d-i
	   mnas-spring::set-d-i 
	   mnas-spring::t-s 
	   mnas-spring::set-t-s 
	   mnas-spring::l-w 
	   mnas-spring::l-4 
	   mnas-spring::ro-w[kg/mm3] 
	   mnas-spring::mass-s
	   mnas-spring::li-si
	   mnas-spring::G[MPa] 
	   mnas-spring::fi-si 
	   mnas-spring::taui-fi
	   mnas-spring::fi-taui
	   mnas-spring::si-fi 
	   )
  )

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
