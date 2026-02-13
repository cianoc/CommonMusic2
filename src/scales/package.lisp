(defpackage :cm/scales
  (:use :cl)
  (:local-nicknames
   (#:a #:alexandria-1)
   (#:S #:serapeum)
   (#:t #:transducers))
  (:export
   #:tuning
   #:scale
   #:cents->scaler
   #:scaler->cents
   #:*chromatic-scale*))

