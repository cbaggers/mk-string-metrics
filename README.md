# mk-string-metrics

This library implements efficient algorithms that calculate various string
metrics in Common Lisp:

* [Damerau-Levenshtein
  distance](http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance)
* [Hamming distance](http://en.wikipedia.org/wiki/Hamming_distance)
* [Jaccard similarity
  coefficient](http://en.wikipedia.org/wiki/Jaccard_index)
* [Jaro
  distance](http://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance)
* [Jaro-Winkler
  distance](http://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance)
* [Levenshtein distance](http://en.wikipedia.org/wiki/Levenshtein_distance)
* [Normalized Damerau-Levenshtein
  distance](http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance)
* [Normalized Levenshtein
  distance](http://en.wikipedia.org/wiki/Levenshtein_distance)
* [Overlap coefficient](http://en.wikipedia.org/wiki/Overlap_coefficient)

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

Calculate Damerau-Levenshtein distance between two given strings `x` and
`y`.

----

```
hamming x y
```

Calculate Hamming distance between two given strings `x` and `y`, they have
to be of the same length.

----

```
jaccard x y
```

Calculate Jaccard similarity coefficient for two strings `x` and
`y`. Returned value is in range from `0` (no similarity) to `1` (exact
match).

----

```
jaro x y
```

Calculate Jaro distance between two strings `x` and `y`. Returned value is
in range from `0` (no similarity) to `1` (exact match).

----

```
jaro-winkler x y
```

Calculate Jaro-Winkler distance between two strings `x` and `y`. Returned
value is in range from `0` (no similarity) to `1` (exact match).

----

```
levenshtein x y
```

Calculate Levenshtein distance between two given strings `x` and `y`.

----

```
norm-damerau-levenshtein x y
```

Return normalized Damerau-Levenshtein distance between `x` and `y`. Result
is a real number from `0` to `1`, where `0` signifies no similarity between
the strings, while `1` means exact match.

----

```
norm-levenshtein x y
```

Return normalized Levenshtein distance between `x` and `y`. Result is a real
number from `0` to `1`, where `0` signifies no similarity between the
strings, while `1` means exact match.

----

```
overlap x y
```

This function calculates overlap coefficient between two given strings `x`
and `y`. Returned value is in range from `0` (no similarity) to `1` (exact
match).

## Something missing?

Let me know if you would like to see more functions in this library. Open an
issue and tell me what's missing.

## License

Copyright © 2014 Mark Karpov

Distributed under MIT License.
