(in-package :cm/scales)

;; tools for chromatic intervals

(defstruct interval
  size
  quality
  interval-steps
  up
  octaves
  )

(defun perfect-unison (octaves up)
  (make-interval :octaves octaves :up up :size 0
                 :quality :perfect
                 :interval-steps 0))

(defun diminished-second (octaves up)
  (make-interval :octaves octaves :up up :size 0
                 :quality :diminished
                 :interval-steps 1))

(defun minor-second (octaves up)
  (make-interval :octaves octaves :up up :size 1
                 :quality :minor
                 :interval-steps 1))

(defun augmented-unison (octaves up)
  (make-interval :octaves octaves :up up :size 1
                 :quality :augmented
                 :interval-steps 0))

(defun major-second (octaves up)
  (make-interval :octaves octaves :up up :size 2
                 :quality :major
                 :interval-steps 1))

(defun diminished-third (octaves up)
  (make-interval :octaves octaves :up up :size 2
                 :quality :diminished
                 :interval-steps 2))

(defun minor-third (octaves up)
  (make-interval :octaves octaves :up up :size 3
                 :quality :minor
                 :interval-steps 2))

(defun augmented-second (octaves up)
  (make-interval :octaves octaves :up up :size 3
                 :quality :augmented
                 :interval-steps 1))

(defun major-third (octaves up)
  (make-interval :octaves octaves :up up :size 4
                 :quality :major
                 :interval-steps 2))

(defun diminished-fourth (octaves up)
  (make-interval :octaves octaves :up up :size 4
                 :quality :major
                 :interval-steps 3))

(defun perfect-fourth (octaves up)
  (make-interval :octaves octaves :up up :size 5
                 :quality :perfect
                 :interval-steps 3))

(defun augmented-third (octaves up)
  (make-interval :octaves octaves :up up :size 5
                 :quality :augmented
                 :interval-steps 2))

(defun augmented-fourth (octaves up)
  (make-interval :octaves octaves :up up :size 6
                 :quality :augmented
                 :interval-steps 3))

(defun diminished-fifth (octaves up)
  (make-interval :octaves octaves :up up :size 6
                 :quality :diminished
                 :interval-steps 4))

(defun perfect-fifth (octaves up)
  (make-interval :octaves octaves :up up :size 7
                 :quality :perfect
                 :interval-steps 4))

(defun diminished-sixth (octaves up)
  (make-interval :octaves octaves :up up :size 7
                 :quality :diminished
                 :interval-steps 5))

(defun minor-sixth (octaves up)
  (make-interval :octaves octaves :up up :size 8
                 :quality :minor
                 :interval-steps 5))

(defun augmented-fifth (octaves up)
  (make-interval :octaves octaves :up up :size 8
                 :quality :augmented
                 :interval-steps 4))

(defun major-sixth (octaves up)
  (make-interval :octaves octaves :up up :size 9
                 :quality :major
                 :interval-steps 5))

(defun diminished-seventh (octaves up)
  (make-interval :octaves octaves :up up :size 9
                 :quality :diminished
                 :interval-steps 6))

(defun minor-seventh (octaves up)
  (make-interval :octaves octaves :up up :size 10
                 :quality :minor
                 :interval-steps 6))

(defun augmented-sixth (octaves up)
  (make-interval :octaves octaves :up up :size 10
                 :quality :augmented
                 :interval-steps 5))

(defun major-seventh (octaves up)
  (make-interval :octaves octaves :up up :size 11
                 :quality :major
                 :interval-steps 6))

(defun diminished-octave (octaves up)
  (make-interval :octaves octaves :up up :size 11
                 :quality :diminished
                 :interval-steps 7))

(defun perfect-octave (octaves up)
  (make-interval :octaves octaves :up up :size 12
                 :quality :perfect
                 :interval-steps 7))

(defun augmented-seventh (octaves up)
  (make-interval :octaves octaves :up up :size 12
                 :quality :augmented
                 :interval-steps 6))

(defun interval-definition (arg1 arg2)
  (let ((up (plusp arg1)))
    (multiple-value-bind (octaves size)
        (floor (abs arg1) 12)
      (match size
        (1 (match arg2
             ((or :perfect :p) (perfect-unison octaves up))
             ((or :augmented :aug :a) (augmented-unison octaves up))
             (_ (error 
                 (format nil
                         "Bad Interval Definition\n
Arguments Passed: ~d ~a"
                         size arg2)))))
        (2 (match arg2
             ((or :diminished :dim :d) (diminished-second octaves up))
             ((or :minor :min :mi) (minor-second octaves up))
             ((or :major :maj :ma :mj) (major-second octaves up))
             ((or :augmented :aug :a) (augmented-second octaves up))
             (_ (error 
                 (format nil
                         "Bad Interval Definition\n
Arguments Passed: ~d ~a"
                         size arg2)))))
        (3 (match arg2
             ((or :major :maj :ma :mj) (major-third octaves up))
             ((or :minor :min :mi) (minor-third octaves up))
             ((or :augmented :aug :a) (augmented-third octaves up))
             (_ (error 
                 (format nil
                         "Bad Interval Definition\n
Arguments Passed: ~d ~a"
                         size arg2)))))
        (4 (match arg2
             ((or :diminished :dim :d) (diminished-fourth octaves up))
             ((or :perfect :p) (perfect-fourth octaves up))
             ((or :augmented :aug :a) (augmented-fourth octaves up))
             (_ (error 
                 (format nil
                         "Bad Interval Definition\n
Arguments Passed: ~d ~a"
                         size arg2)))))
        (5 (match arg2
             ((or :perfect :p) (perfect-fifth octaves up))
             ((or :diminished :dim :d) (diminished-fifth octaves up))
             ((or :augmented :aug :a) (augmented-fifth octaves up))
             (_ (error 
                 (format nil
                         "Bad Interval Definition\n
Arguments Passed: ~d ~a"
                         size arg2)))))        
        (6 (match arg2
             ((or :major :maj :ma :mj) (major-sixth octaves up))
             ((or :minor :min :mi) (minor-sixth octaves up))
             ((or :diminished :dim :d) (diminished-sixth octaves up))
             ((or :augmented :aug :a) (augmented-sixth octaves up))
             (_ (error 
                 (format nil
                         "Bad Interval Definition\n
Arguments Passed: ~d ~a"
                         size arg2)))))
        (7 (match arg2
             ((or :major :maj :ma :mj) (major-seventh octaves up))
             ((or :minor :min :mi) (minor-seventh octaves up))
             ((or :diminished :dim :d) (diminished-seventh octaves up))
             ((or :augmented :aug :a) (augmented-seventh octaves up))
             (_ (error 
                 (format nil
                         "Bad Interval Definition\n
Arguments Passed: ~d ~a"
                         size arg2)))))
         (8 (match arg2
              ((or :perfect :p) (perfect-octave octaves up))
              ((or :diminished :dim :d) (diminished-octave octaves up))
              (_ (error 
                  (format nil
                          "Bad Interval Definition\n
Arguments Passed: ~d ~a"
                          size arg2)))))       
        (_ (error (format nil
                          "Bad Interval Definition\n
Arguments Passed: ~d ~a"
                          size arg2)))))))

(defun interval-size-notes (arg1 arg2)
  (let ((up (plusp arg1)))
    (multiple-value-bind (octaves size)
        (floor (abs arg1) 12)
      (match size
        (0 (perfect-unison octaves up))
        (1 (minor-second octaves up))
        (2 (major-second octaves up))
        (3 (minor-third octaves up))
        (4 (major-third octaves up))
        (5 (perfect-fourth octaves up))
        (6 (diminished-fifth octaves up))        
        (7 (perfect-fifth octaves up))
        (8 (minor-sixth octaves up))
        (9 (major-sixth octaves up))
        (10 (minor-seventh octaves up))
        (11 (major-seventh octaves up))))))

(defun interval-between (arg1 arg2))

(defun interval (arg1 arg2 )
  (cond
    ((integerp arg1)
     (interval-between arg1 arg2))
    ((integerp arg2)
     (interval-definition arg1 arg2))
    ((symbolp arg2))))

(defmethod semitones ((interval interval))
  (interval-size interval))

(defmethod invert ((interval interval)))
