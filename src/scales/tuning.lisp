(in-package :cm/scales)

(defparameter *tuning* (make-hash-table))

;; used to convert lists where the difference is provided
;; rather than exact values for each interval
(defun convert-ratios-list (fn init-value lst)
  (let ((vec (t:transduce (t:scan fn init-value)  #'t:vector lst)))
    (subseq vec 0 (- (length vec) 1))))

;; (convert-ratios-list #'+ 0 #(100 100 100 100 100))

(defun cents->ratios (cents)
  (t:transduce (t:map #'cents->scaler) #'t:vector cents))

(defun steps->ratios (steps octave-width)
  (t:transduce (t:comp
                (t:take steps)
                (t:map (lambda (x) (expt octave-width
                                                 (/ x steps)))))
               #'t:vector
               (t:ints 0)))

(defun convert-ratios-list (fn init-value lst)
  (if (/= (aref lst 1) init-value)
      (let ((vec (t:transduce (t:scan fn init-value)  #'t:vector lst)))
        (subseq vec 0 (- (length vec) 1)))
      lst))

(defstruct tuning
  name
  ratios
  (octave-width :2 :type fixnum))

(defmethod tuning-length ((tuning tuning))
  (length (tuning-ratios tuning)))

(defmethod degree-ratio ((tuning tuning) i)
  (aref (tuning-ratios tuning) i))

(defun tuning (&key name (octave-width 2) cents ratios steps)
  (let* ((ratios
           (cond (cents
                  (cents->ratios
                   (convert-ratios-list #'+ 0 cents)))
                 (ratios
                  (convert-ratios-list #'* 1 ratios))
                 (steps
                  (steps->ratios steps octave-width))))
        (tuning
          (make-tuning :name name
                       :ratios ratios
                       :octave-width octave-width)))
    (when name (setf (gethash name *tuning*)
                     tuning))
    tuning))

