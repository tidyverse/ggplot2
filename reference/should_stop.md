# Used in examples to illustrate when errors should occur.

Used in examples to illustrate when errors should occur.

## Usage

``` r
should_stop(expr)
```

## Arguments

- expr:

  code to evaluate.

## Examples

``` r
should_stop(stop("Hi!"))
should_stop(should_stop("Hi!"))
#> [1] "Hi!"
```
