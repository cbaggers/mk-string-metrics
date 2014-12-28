# mk-string-metrics

This library implements efficient algorithms that calculate various string
metrics in Common Lisp.

The library provides functions to calculate:

* [Damerau-Levenshtein
  distance](http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance);
* [Hamming distance](http://en.wikipedia.org/wiki/Hamming_distance);
* [Jaccard similarity
  coefficient](http://en.wikipedia.org/wiki/Jaccard_index);
* [Jaro
  distance](http://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance);
* [Jaro-Winkler
  distance](http://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance);
* [Levenshtein distance](http://en.wikipedia.org/wiki/Levenshtein_distance);
* [Normalized Damerau-Levenshtein
  distance](http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance);
* [Normalized Levenshtein
  distance](http://en.wikipedia.org/wiki/Levenshtein_distance);
* [Overlap coefficient](http://en.wikipedia.org/wiki/Overlap_coefficient).

## Installation

Copy files of this library in any place where ASDF can find them. Then you
can use it in system definitions and ASDF will take care of the rest.

Via Quicklisp:

```
(ql:quickload "mk-string-metrics")
```

## Description

```
damerau-levenshtein x y
```

This function calculates Damerau-Levenshtein distance between two given
strings.

```
hamming x y
```

Calculates Hamming distance between two given strings, they have to be of
the same length.

```
jaccard x y
```

Calculates Jaccard similarity coefficient for two strings. Returned value is
in range from 0 (no similarity) to 1 (exact match).

```
jaro x y
```

Calculates Jaro distance between two strings. Returned value is in range
from 0 (no similarity) to 1 (exact match).

```
jaro-winkler x y
```

Calculates Jaro-Winkler distance between two strings. Returned value is in
range from 0 (no similarity) to 1 (exact match).

```
levenshtein x y
```

This function calculates Levenshtein distance between two given strings.

```
norm-damerau-levenshtein x y
```

Returns normalized Damerau-Levenshtein distance between X and Y. Result is a
real number from 0 to 1, where 0 signifies no similarity between the
strings, while 1 means exact match.

```
norm-levenshtein x y
```

Returns normalized Levenshtein distance between X and Y. Result is a real
number from 0 to 1, where 0 signifies no similarity between the strings,
while 1 means exact match.

```
overlap x y
```

This function calculates overlap coefficient between two given strings.

## Something missing?

Let me know if you would like to see more functions in this library. Open an
issue and tell me what's missing.

## License

Copyright (c) 2014 Mark Karpov

Distributed under MIT License.
