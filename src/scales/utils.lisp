(in-package :cm/scales)

(defun cents->scaler (cents)
  "Converts cents to a ratio"
  (expt 2 (/ cents 1200)))

(defun scaler->cents (scaler)
  "Converts linear ratio to the number of cents"
  (round (* (log scaler 2) 1200)))

(defun between-hz (low-hz high-hz)
  "Returns the halfway point between two frequencies, using cents."
  (cents->scaler
   (/ (+
       (scaler->cents low-hz)
       (scaler->cents high-hz)) 2)))
