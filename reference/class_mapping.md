# The mapping class

The mapping class holds a list of quoted expressions
([quosures](https://rlang.r-lib.org/reference/topic-quosure.html)) or
constants. An object is typically constructed using the
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.md) function.

## Usage

``` r
class_mapping(x = list(), ..., env = globalenv())
```

## Arguments

- x:

  A list of quosures and constants.

- ...:

  Reserved for future expansion.

- env:

  An environment for symbols that are not quosures or constants.
