;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; mk-string-metrics — library of efficient implementations of various
;;; string metric algorithms.
;;;
;;; Copyright © 2014 Mark Karpov
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

(defpackage   :mk-string-metrics
  (:nicknames :mksm)
  (:use       #:common-lisp)
  (:export    #:hamming
              #:levenshtein
              #:damerau-levenshtein
              #:norm-levenshtein
              #:norm-damerau-levenshtein
              #:overlap
              #:jaccard
              #:jaro
              #:jaro-winkler))

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
    (declare (type (simple-array array-index) v0 v1))
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
    (declare (type (simple-array array-index) v0 v1 v*))
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
          (declare (type array-index cost))
          (setf (aref v1 (1+ j))
                (min (1+ (aref v1 j))
                     (1+ (aref v0 (1+ j)))
                     (+  (aref v0 j) cost)))
          (when (and (plusp i)
                     (plusp j))
            (let ((x-i-1 (char x (1- i)))
                  (y-j-1 (char y (1- j)))
                  (val (+ (aref v* (1- j)) cost)))
              (declare (type array-index val))
              (when (and (char= x-i y-j-1)
                         (char= x-i-1 y-j)
                         (< val (aref v1 (1+ j))))
                (setf (aref v1 (1+ j)) val))))))
      (rotatef v* v0 v1))))

(defun norm-levenshtein (x y)
  "Returns normalized Levenshtein distance between X and Y. Result is a real
number from 0 to 1, where 0 signifies no similarity between the strings,
while 1 means exact match."
  (let ((r (levenshtein x y)))
    (if (zerop r)
        1
        (- 1 (/ r
                (max (length x)
                     (length y)))))))

(defun norm-damerau-levenshtein (x y)
  "Returns normalized Damerau-Levenshtein distance between X and Y. Result
is a real number from 0 to 1, where 0 signifies no similarity between the
strings, while 1 means exact match."
  (let ((r (damerau-levenshtein x y)))
    (if (zerop r)
        1
        (- 1 (/ r
                (max (length x)
                     (length y)))))))

(defun string-to-set (str)
  "Converts string into a set. This function is supposed to be inlined."
  (declare (type (simple-array character) str)
           (inline length)
           (optimize (safety 0) (speed 3) (space 3)))
  (let ((result (make-hash-table)))
    (dotimes (i (length str))
      (let ((ch (char str i)))
        (if (gethash ch result)
            (incf (the array-index (gethash ch result)))
            (setf (gethash ch result) 1))))
    result))

(defun intersection-length (x y)
  "Returns length of intersection of two strings. This function is supposed
to be inlined."
  (let ((result 0))
    (declare (type array-index result)
             (optimize (safety 0) (speed 3) (space 3)))
    (maphash (lambda (key x-val)
               (declare (type array-index x-val))
               (let ((y-val (gethash key y)))
                 (declare (type (or array-index null) y-val))
                 (when y-val
                   (incf result (min x-val y-val)))))
             x)
    result))

(defun union-length (x y)
  "Returns length of union of two strings. This function is supposed to be
inlined."
  (let ((temp (make-hash-table))
        (result 0))
    (declare (type array-index result)
             (optimize (safety 0) (speed 3) (space 3)))
    (flet ((extract (h)
             (maphash (lambda (key val)
                        (declare (type array-index val))
                        (let ((t-val (gethash key temp)))
                          (declare (type (or array-index null) t-val))
                          (setf (gethash key temp)
                                (if t-val
                                    (max val t-val)
                                    val))))
                      h)))
      (extract x)
      (extract y)
      (maphash (lambda (key val)
                 (declare (ignore key)
                          (type array-index val))
                 (incf result val))
               temp)
      result)))

(defun overlap (x y)
  "This function calculates overlap coefficient between two given
strings. Returned value is in range from 0 (no similarity) to 1 (exact match)."
  (declare (type (simple-array character) x y)
           (inline length)
           (optimize (safety 0) (speed 3) (space 3)))
  (/ (the array-index (intersection-length (string-to-set x)
                                           (string-to-set y)))
     (min (length x)
          (length y))))

(defun jaccard (x y)
  "Calculates Jaccard similarity coefficient for two strings. Returned value
is in range from 0 (no similarity) to 1 (exact match)."
  (declare (type (simple-array character) x y)
           (optimize (safety 0) (speed 3) (space 3)))
  (let ((x (string-to-set x))
        (y (string-to-set y)))
    (if (and (zerop (hash-table-count x))
             (zerop (hash-table-count y)))
        1
        (/ (the array-index (intersection-length x y))
           (the array-index (union-length x y))))))

(defun fast-find (char str str-len &optional (start 0))
  "Checks if CHAR is in STR. This function is supposed to be inlined."
  (declare (type character char)
           (type (simple-array character) str)
           (type array-index str-len start)
           (optimize (safety 0) (speed 3) (space 3)))
  (do ((i start (1+ i)))
      ((>= i str-len))
    (declare (type array-index i))
    (when (char= char (char str i))
      (return-from fast-find i))))

(defun jaro (x y)
  "Calculates Jaro distance between two strings. Returned value is in range
from 0 (no similarity) to 1 (exact match)."
  (declare (type (simple-array character) x y)
           (inline length)
           (optimize (safety 0) (speed 1) (space 3)))
  (let* ((x-len (length x))
         (y-len (length y))
         (d (if (and (>= x-len 2)
                     (>= y-len 2))
                (- (floor (max x-len y-len) 2) 1)
                0))
         (m 0)
         (p 0)
         (pj 0))
    (declare (type array-index d m p pj))
    (dotimes (i x-len)
      (declare (type array-index i))
      (let ((ch (char x i)))
        (do ((j (fast-find ch y y-len 0)
                (fast-find ch y y-len (1+ j)))
             done)
            ((or (null j) done))
          (declare (type (or array-index null) j))
          (when (and j (<= (the array-index (abs (- i j)))
                           d))
            (when (and (plusp pj)
                       (< j pj))
              (incf p))
            (setf pj   j
                  done t)
            (incf m)))))
    (if (zerop m)
        0
        (/ (+ (/ m x-len)
              (/ m y-len)
              (/ (- m p) m))
           3))))

(defun prefix-length (x y)
  "Calculates length of common prefix for strings X and Y."
  (declare (type (simple-array character) x y)
           (inline length)
           (optimize (safety 0) (speed 3) (space 3)))
  (let ((x-len (length x))
        (y-len (length y))
        (result 0))
    (declare (type array-index result))
    (dotimes (i x-len)
      (if (and (< i y-len)
               (char= (char x i)
                      (char y i)))
          (incf result)
          (return-from prefix-length result)))
    result))

(defun jaro-winkler (x y)
  "Calculates Jaro-Winkler distance between two strings. Returned value is
in range from 0 (no similarity) to 1 (exact match)."
  (let ((jd (jaro x y))
        (l  (prefix-length x y)))
    (+ jd (* l 1/10 (- 1 jd)))))
