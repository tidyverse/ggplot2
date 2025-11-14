# A waiver object.

A waiver is a "flag" object, similar to `NULL`, that indicates the
calling function should just use the default value. It is used in
certain functions to distinguish between displaying nothing (`NULL`) and
displaying a default value calculated elsewhere (`waiver()`).
`is_waiver()` reports whether an object is a waiver.

## Usage

``` r
waiver()

is_waiver(x)
```

## Arguments

- x:

  An object to test
