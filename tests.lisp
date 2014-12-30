;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; This is some tests for mk-string-metrics library.
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

(in-package #:mk-string-metrics)

(defun run-tests ()
  "Run mk-string-metrics tests. Signals failure if any test fails or returns
NIL of all tests pass."
  ;; Damerau-Levenshtein distance
  (assert (= 4 (damerau-levenshtein "veryvery long" "very long")))
  (assert (= 4 (damerau-levenshtein "very long" "veryvery long")))
  (assert (= 1 (damerau-levenshtein "thing" "think")))
  (assert (= 1 (damerau-levenshtein "think" "thing")))
  (assert (= 2 (damerau-levenshtein "nose" "ones")))
  (assert (= 2 (damerau-levenshtein "ones" "nose")))
  (assert (= 3 (damerau-levenshtein "thing" "sign")))
  (assert (= 3 (damerau-levenshtein "sign" "thing")))
  (assert (= 3 (damerau-levenshtein "red" "wax")))
  (assert (= 3 (damerau-levenshtein "wax" "red")))
  (assert (= 0 (damerau-levenshtein "lucky" "lucky")))
  (assert (= 0 (damerau-levenshtein "" "")))
  ;; Hamming distance
  (assert (= 3 (hamming "karolin" "kathrin")))
  (assert (= 3 (hamming "kathrin" "karolin")))
  (assert (= 3 (hamming "karolin" "kerstin")))
  (assert (= 3 (hamming "kerstin" "karolin")))
  (assert (= 2 (hamming "1011101" "1001001")))
  (assert (= 2 (hamming "1001001" "1011101")))
  (assert (= 3 (hamming "2173896" "2233796")))
  (assert (= 3 (hamming "2233796" "2173896")))
  (assert (= 3 (hamming "toned" "roses")))
  (assert (= 3 (hamming "roses" "toned")))
  (assert (= 3 (hamming "red" "wax")))
  (assert (= 3 (hamming "wax" "red")))
  (assert (= 0 (hamming "lucky" "lucky")))
  (assert (= 0 (hamming "" "")))
  ;; Jaccard similarity coefficient
  (assert (= 1/2 (jaccard "xxx" "xyx")))
  (assert (= 3/7 (jaccard "night" "nacht")))
  (assert (= 3/7 (jaccard "nacht" "night")))
  (assert (= 5/9 (jaccard "context" "contact")))
  (assert (= 5/9 (jaccard "context" "contact")))
  (assert (= 1 (jaccard "lucky" "lucky")))
  (assert (= 1 (jaccard "" "")))
  ;; Jaro distance
  (assert (= 5/6 (jaro "aa" "a")))
  (assert (= 5/6 (jaro "a" "aa")))
  (assert (= 17/18 (jaro "martha" "marhta")))
  (assert (= 37/45 (jaro "dwayne" "duane")))
  (assert (= 23/30 (jaro "dixon" "dicksonx")))
  (assert (= 83/105 (jaro "jones" "johnson")))
  (assert (= 14/15 (jaro "brain" "brian")))
  (assert (= 0 (jaro "five" "ten")))
  (assert (= 1 (jaro "lucky" "lucky")))
  (assert (= 0 (jaro "" "")))
  ;; Jaro-Winkler distance
  (assert (= 17/20 (jaro-winkler "aa" "a")))
  (assert (= 17/20 (jaro-winkler "a" "aa")))
  (assert (= 173/180 (jaro-winkler "martha" "marhta")))
  (assert (= 21/25 (jaro-winkler "dwayne" "duane")))
  (assert (= 61/75 (jaro-winkler "dixon" "dicksonx")))
  (assert (= 437/525 (jaro-winkler "jones" "johnson")))
  (assert (= 71/75 (jaro-winkler "brain" "brian")))
  (assert (= 0 (jaro-winkler "five" "ten")))
  (assert (= 1 (jaro-winkler "lucky" "lucky")))
  (assert (= 0 (jaro-winkler "" "")))
  ;; Levenshtein distance
  (assert (= 3 (levenshtein "kitten" "sitting")))
  (assert (= 3 (levenshtein "sitting" "kitten")))
  (assert (= 2 (levenshtein "cake" "drake")))
  (assert (= 2 (levenshtein "drake" "cake")))
  (assert (= 3 (levenshtein "saturday" "sunday")))
  (assert (= 3 (levenshtein "sunday" "saturday")))
  (assert (= 3 (levenshtein "red" "wax")))
  (assert (= 3 (levenshtein "wax" "red")))
  (assert (= 0 (levenshtein "lucky" "lucky")))
  (assert (= 0 (levenshtein "" "")))
  ;; Normalized Damerau-Levenshtein distance
  (assert (= 9/13 (norm-damerau-levenshtein "veryvery long" "very long")))
  (assert (= 9/13 (norm-damerau-levenshtein "very long" "veryvery long")))
  (assert (= 4/5 (norm-damerau-levenshtein "thing" "think")))
  (assert (= 4/5 (norm-damerau-levenshtein "think" "thing")))
  (assert (= 1/2 (norm-damerau-levenshtein "nose" "ones")))
  (assert (= 1/2 (norm-damerau-levenshtein "ones" "nose")))
  (assert (= 2/5 (norm-damerau-levenshtein "thing" "sign")))
  (assert (= 2/5 (norm-damerau-levenshtein "sign" "thing")))
  (assert (= 0 (norm-damerau-levenshtein "red" "wax")))
  (assert (= 0 (norm-damerau-levenshtein "wax" "red")))
  (assert (= 1 (norm-damerau-levenshtein "lucky" "lucky")))
  (assert (= 1 (norm-damerau-levenshtein "" "")))
  ;; Normalized Levenshtein distance
  (assert (= 4/7 (norm-levenshtein "kitten" "sitting")))
  (assert (= 4/7 (norm-levenshtein "sitting" "kitten")))
  (assert (= 3/5 (norm-levenshtein "cake" "drake")))
  (assert (= 3/5 (norm-levenshtein "drake" "cake")))
  (assert (= 5/8 (norm-levenshtein "saturday" "sunday")))
  (assert (= 5/8 (norm-levenshtein "sunday" "saturday")))
  (assert (= 0 (norm-levenshtein "red" "wax")))
  (assert (= 0 (norm-levenshtein "wax" "red")))
  (assert (= 1 (norm-levenshtein "lucky" "lucky")))
  (assert (= 1 (norm-levenshtein "" "")))
  ;; Overlap coefficient
  (assert (= 1 (overlap "fly" "butterfly")))
  (assert (= 1 (overlap "butterfly" "fly")))
  (assert (= 3/5 (overlap "night" "nacht")))
  (assert (= 3/5 (overlap "nacht" "night")))
  (assert (= 5/7 (overlap "context" "contact")))
  (assert (= 5/7 (overlap "contact" "context")))
  (assert (= 0 (overlap "red" "wax")))
  (assert (= 0 (overlap "wax" "red")))
  (assert (= 1 (overlap "lucky" "lucky"))))

(export 'run-tests)
