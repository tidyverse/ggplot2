# Convert a ggproto object to a list

This will not include the object's `super` member.

## Usage

``` r
# S3 method for class 'ggproto'
as.list(x, inherit = TRUE, ...)
```

## Arguments

- x:

  A ggproto object to convert to a list.

- inherit:

  If `TRUE` (the default), flatten all inherited items into the returned
  list. If `FALSE`, do not include any inherited items.

- ...:

  Arguments passed on to
  [`base::as.list.environment`](https://rdrr.io/r/base/list.html)

  `all.names`

  :   a logical indicating whether to copy all values or (default) only
      those whose names do not begin with a dot.

  `sorted`

  :   a logical indicating whether the
      [`names`](https://rdrr.io/r/base/names.html) of the resulting list
      should be sorted (increasingly). Note that this is somewhat
      costly, but may be useful for comparison of environments.
