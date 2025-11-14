# Compute the "resolution" of a numeric vector

The resolution is the smallest non-zero distance between adjacent
values. If there is only one unique value, then the resolution is
defined to be one. If x is an integer vector, then it is assumed to
represent a discrete variable, and the resolution is 1. If the
differences are all smaller than the tolerance, set resolution to 1.

## Usage

``` r
resolution(x, zero = TRUE, discrete = FALSE)
```

## Arguments

- x:

  numeric vector

- zero:

  should a zero value be automatically included in the computation of
  resolution

- discrete:

  should vectors mapped with a discrete scale be treated as having a
  resolution of 1?

## Examples

``` r
resolution(1:10)
#> [1] 1
resolution((1:10) - 0.5)
#> [1] 0.5
resolution((1:10) - 0.5, FALSE)
#> [1] 1

# Note the difference between numeric and integer vectors
resolution(c(2, 10, 20, 50))
#> [1] 2
resolution(c(2L, 10L, 20L, 50L))
#> [1] 1
```
