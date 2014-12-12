;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; mk-string-metrics - library of efficient implementations of various
;;; string metric algorithms.
;;;
;;; Copyright (c) 2014 Mark Karpov
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(cl:defpackage #:mk-string-metrics
  (:use        #:common-lisp)
  (:export     #:hamming
               #:levenshtein
               #:damerau-levenshtein))

(in-package #:mk-string-metrics)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hamming (x y)
    "Calculates Hamming distance between two given strings, they have to be
of the same length."
    (declare (type (simple-array character) x y)
             (inline length)
             (optimize (safety 0) (speed 3) (space 3)))
    (let ((result 0))
      (declare (type fixnum result))
      (dotimes (i (length x) result)
        (declare (type fixnum i))
        (unless (char= (char x i)
                       (char y i))
          (incf result)))))
  (defun levenshtein (x y)
    "This function calculates Levenshtein distance between two given
strings."
    (declare (type (simple-array character) x y)
             (inline length)
             (optimize (safety 0) (speed 3) (space 3)))
    (let* ((x-len (length x))
           (y-len (length y))
           (v0 (make-array (1+ y-len) :element-type 'fixnum))
           (v1 (make-array (1+ y-len) :element-type 'fixnum)))
      (declare (type fixnum x-len y-len)
               (type (simple-array fixnum) v0 v1))
      (dotimes (i (1+ y-len))
        (declare (type fixnum i))
        (setf (aref v0 i) i))
      (dotimes (i x-len (aref v0 y-len))
        (declare (type fixnum i))
        (setf (aref v1 0) (1+ i))
        (dotimes (j y-len)
          (declare (type fixnum j))
          (setf (aref v1 (1+ j))
                (min (1+ (aref v1 j))
                     (1+ (aref v0 (1+ j)))
                     (+  (aref v0 j)
                         (if (char= (char x i)
                                    (char y j))
                             0 1)))))
        (rotatef v0 v1))))
  (defun damerau-levenshtein (x y)
    "This function calculates Damerau-Levenshtein distance between two given
strings."
    (declare (type (simple-array) x y)
             (inline length)
             (optimize (safety 0) (speed 3) (space 3)))
    (let* ((x-len (length x))
           (y-len (length y))
           (v0 (make-array (1+ y-len) :element-type 'fixnum))
           (v1 (make-array (1+ y-len) :element-type 'fixnum))
           (v* (make-array (1+ y-len) :element-type 'fixnum)))
      (declare (type fixnum x-len y-len)
               (type (simple-array fixnum) v0 v1 v*))
      (dotimes (i (1+ y-len))
        (declare (type fixnum i))
        (setf (aref v0 i) i))
      (dotimes (i x-len (aref v0 y-len))
        (declare (type fixnum i))
        (setf (aref v1 0) (1+ i))
        (dotimes (j y-len)
          (declare (type fixnum j))
          (let* ((x-i (char x i))
                 (y-j (char y j))
                 (cost (if (char= x-i y-j) 0 1)))
            (declare (type character x-i y-j)
                     (type fixnum cost))
            (setf (aref v1 (1+ j))
                  (min (1+ (aref v1 j))
                       (1+ (aref v0 (1+ j)))
                       (+  (aref v0 j) cost)))
            (when (and (plusp i) (plusp j))
              (let ((x-i-1 (char x (1- i)))
                    (y-j-1 (char y (1- j)))
                    (val (+ (aref v* (1- j)) cost)))
                (when (and (char= x-i y-j-1)
                           (char= x-i-1 y-j)
                           (< val (aref v1 (1+ j))))
                  (setf (aref v1 (1+ j)) val))))))
        (rotatef v* v0 v1)))))
