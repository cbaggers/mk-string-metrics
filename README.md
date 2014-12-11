# mk-string-metrics

For now, the library implements efficient Hamming, Levenshtein, and
Damerau-Levenshtein algorithms.

## Installation

Copy files of this library in any place where ASDF can find them. Then you
can use it in system definitions and ASDF will take care of the rest.

## Syntax and Description

`hamming x y`

Calculates Hamming distance between two given strings, they have to be of
the same length.

`levenshtein x y`

This function calculates Levenshtein distance between two given strings.

`damerau-levenshtein x y`

This function calculates Damerau-Levenshtein distance between two given
strings.

## License

Copyright (c) 2014 Mark Karpov

Distributed under MIT License.
