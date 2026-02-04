;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.62 $
;;; $Date: 2006/12/29 03:08:48 $

(defpackage :cm
  ;; trivial-do is currently only used for doplist, which is in
  ;; alexandria. May remove, though other utilities are useful if
  ;; not in alexandria/serapeum.
  (:use :common-lisp :trivial-do)
  (:shadow :make-load-form)
  (:shadow :standard-generic-function)
  (:export #:accumulation
	   #:amplitude
	   #:append-object
	   #:*beat*
	   #:best-normal-form
	   #:between
	   #:cd
	   #:cents->scaler
           #:chord
	   #:*chromatic-scale*
	   #:cm
           #:cycle
           #:decimals
	   #:decode-interval
	   #:drunk
	   #:expl
	   #:explseg
           #:explsegs
	   #:fm-spectrum
           #:fold-objects
	   #:harmonics
	   #:heap
	   #:hertz
	   #:histogram
	   #:interp
	   #:interpl
	   #:interval
	   #:invert
	   #:keynum
	   #:note
	   #:line
           #:log2
           #:log10
	   #:markov-analyze
	   #:markov
	   #:new
	   #:next
	   #:odds
	   #:palindrome
	   #:pattern-state
	   #:pattern-value
	   #:pattern?
           #:pick
	   #:pickl
	   #:pitch-class
           #:pval
	   #:pwd
	   #:quantize
	   #:ran
	   #:range
	   #:ransegs
	   #:remove-object
	   #:remove-subobjects
	   #:rescale-envelope
	   #:rescale
	   #:rhythm
	   #:rm-spectrum
	   #:rotation
	   #:scale-max
	   #:scale-min
           #:scale-mod
	   #:scale-order
	   #:*scale*
	   #:scale=
	   #:scale>
	   #:scale>=
           #:scale<
	   #:scale<=
	   #:scaler->cents  
	   #:shell
	   #:shuffle 
           #:stop
	   #:*tempo*
	   #:transpose
	   #:transposer
	   #:weighting))






