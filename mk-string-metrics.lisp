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

(cl:defpackage :mk-string-metrics
  (:nicknames  :mksm)
  (:use        #:common-lisp)
  (:export     #:hamming
               #:levenshtein
               #:damerau-levenshtein
               #:norm-levenshtein
               #:norm-damerau-levenshtein
               #:overlap))

(in-package #:mk-string-metrics)

(deftype array-index (&optional (length (1- array-dimension-limit)))
  (list 'integer 0 length))

(defun hamming (x y)
  "Calculates Hamming distance between two given strings, they have to be
of the same length."
  (declare (type (simple-array character) x y)
           (inline length)
           (optimize (safety 0) (speed 3) (space 3)))
  (let ((result 0))
    (declare (type array-index result))
    (dotimes (i (length x) result)
      (declare (type array-index i))
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
         (v0 (make-array (1+ y-len) :element-type 'array-index))
         (v1 (make-array (1+ y-len) :element-type 'array-index)))
    (declare (type array-index x-len y-len)
             (type (simple-array array-index) v0 v1))
    (dotimes (i (1+ y-len))
      (declare (type array-index i))
      (setf (aref v0 i) i))
    (dotimes (i x-len (aref v0 y-len))
      (declare (type array-index i))
      (setf (aref v1 0) (1+ i))
      (dotimes (j y-len)
        (declare (type array-index j))
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
  (declare (type (simple-array character) x y)
           (inline length)
           (optimize (safety 0) (speed 3) (space 3)))
  (let* ((x-len (length x))
         (y-len (length y))
         (v0 (make-array (1+ y-len) :element-type 'array-index))
         (v1 (make-array (1+ y-len) :element-type 'array-index))
         (v* (make-array (1+ y-len) :element-type 'array-index)))
    (declare (type array-index x-len y-len)
             (type (simple-array array-index) v0 v1 v*))
    (dotimes (i (1+ y-len))
      (declare (type array-index i))
      (setf (aref v0 i) i))
    (dotimes (i x-len (aref v0 y-len))
      (declare (type array-index i))
      (setf (aref v1 0) (1+ i))
      (dotimes (j y-len)
        (declare (type array-index j))
        (let* ((x-i (char x i))
               (y-j (char y j))
               (cost (if (char= x-i y-j) 0 1)))
          (declare (type character x-i y-j)
                   (type array-index cost))
          (setf (aref v1 (1+ j))
                (min (1+ (aref v1 j))
                     (1+ (aref v0 (1+ j)))
                     (+  (aref v0 j) cost)))
          (when (and (plusp i) (plusp j))
            (let ((x-i-1 (char x (1- i)))
                  (y-j-1 (char y (1- j)))
                  (val (+ (aref v* (1- j)) cost)))
              (declare (type character x-i-1 y-j-1)
                       (type array-index val))
              (when (and (char= x-i y-j-1)
                         (char= x-i-1 y-j)
                         (< val (aref v1 (1+ j))))
                (setf (aref v1 (1+ j)) val))))))
      (rotatef v* v0 v1))))

(defun norm-levenshtein (x y)
  "Returns normalized Levenshtein distance between X and Y. Result is a real
number from 0 to 1, where 0 signifies no similarity between the strings,
while 1 means exact match."
  (declare (type (simple-array character) x y)
           (optimize (safety 0) (speed 3) (space 3)))
  (- 1 (the array-index
            (/ (levenshtein x y)
               (max (length x)
                    (length y))))))

(defun norm-damerau-levenshtein (x y)
  "Returns normalized Damerau-Levenshtein distance between X and Y. Result
is a real number from 0 to 1, where 0 signifies no similarity between the
strings, while 1 means exact match."
  (declare (type (simple-array character) x y)
           (optimize (safety 0) (speed 3) (space 3)))
  (- 1 (the array-index
            (/ (damerau-levenshtein x y)
               (max (length x)
                    (length y))))))

(defun overlap (x y)
  "This function calculates overlap coefficient between two given strings."
  (declare (type (simple-array character) x y)
           (optimize (safety 0) (speed 3) (space 3)))
  (let* ((len-x (length x))
         (len-y (length y))
         (n 0))
    (declare (type array-index n))
    (multiple-value-bind (g-set l-set g-len l-len)
        (if (>= len-x len-y)
            (values x y len-x len-y)
            (values y x len-y len-x))
      (/ (dotimes (i g-len n)
           (do ((j 0 (1+ j))
                result)
               ((or result (= j l-len))
                result)
             (when (char= (char g-set i)
                          (char l-set j))
               (setf result t)
               (incf n))))
         l-len))))

(defmacro test-it (fnc n)
  `(time (dotimes (x ,n)
           (,fnc "this is a long string" "that's a long string"))))
