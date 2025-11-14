# Define aesthetic mappings programmatically

**\[deprecated\]**

Aesthetic mappings describe how variables in the data are mapped to
visual properties (aesthetics) of geoms.
[`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md) uses
non-standard evaluation to capture the variable names. `aes_()` and
`aes_string()` require you to explicitly quote the inputs either with
`""` for `aes_string()`, or with `quote` or `~` for `aes_()`. (`aes_q()`
is an alias to `aes_()`). This makes `aes_()` and `aes_string()` easy to
program with.

`aes_string()` and `aes_()` are particularly useful when writing
functions that create plots because you can use strings or quoted
names/calls to define the aesthetic mappings, rather than having to use
[`substitute()`](https://rdrr.io/r/base/substitute.html) to generate a
call to [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md).

I recommend using `aes_()`, because creating the equivalents of
`aes(colour = "my colour")` or `` aes(x = `X$1`) `` with `aes_string()`
is quite clunky.

## Usage

``` r
aes_(x, y, ...)

aes_string(x, y, ...)

aes_q(x, y, ...)
```

## Arguments

- x, y, ...:

  List of name value pairs. Elements must be either quoted calls,
  strings, one-sided formulas or constants.

## Life cycle

All these functions are deprecated. Please use tidy evaluation idioms
instead. Regarding `aes_string()`, you can replace it with `.data`
pronoun. For example, the following code can achieve the same mapping as
`aes_string(x_var, y_var)`.

    x_var <- "foo"
    y_var <- "bar"
    aes(.data[[x_var]], .data[[y_var]])

For more details, please see
[`vignette("ggplot2-in-packages")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-in-packages.md).

## See also

[`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md)
