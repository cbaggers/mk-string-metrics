# mk-string-metrics

This library implements efficient algorithms that calculate various string
metrics in Common Lisp.

## Installation

Copy files of this library in any place where ASDF can find them. Then you
can use it in system definitions and ASDF will take care of the rest.

Via Quicklisp:

```
(ql:quickload "mk-string-metrics")
```

## Description

```
hamming x y
```

Calculates Hamming distance between two given strings, they have to be of
the same length.

```
levenshtein x y
```

This function calculates Levenshtein distance between two given strings.

```
damerau-levenshtein x y
```

This function calculates Damerau-Levenshtein distance between two given
strings.

```
norm-levenshtein x y
```

Returns normalized Levenshtein distance between X and Y. Result is a real
number from 0 to 1, where 0 signifies no similarity between the strings,
while 1 means exact match.

```
norm-damerau-levenshtein x y
```

Returns normalized Damerau-Levenshtein distance between X and Y. Result is a
real number from 0 to 1, where 0 signifies no similarity between the
strings, while 1 means exact match.

```
overlap x y
```

This function calculates overlap coefficient between two given strings.

## License

Copyright (c) 2014 Mark Karpov

Distributed under MIT License.
