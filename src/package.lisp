(defpackage :cm
  (:use :cl)
  (:local-nicknames
   (#:a #:alexandria-1)
   (#:s #:serapeum)
   (#:t #:transducers))
  (:export
   #:cents->scaler
   #:scaler->cents
   #:*chromatic-scale*))
