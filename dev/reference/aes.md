# Construct aesthetic mappings

Aesthetic mappings describe how variables in the data are mapped to
visual properties (aesthetics) of geoms. Aesthetic mappings can be set
in [`ggplot()`](https://ggplot2.tidyverse.org/dev/reference/ggplot.md)
and in individual layers.

## Usage

``` r
aes(x, y, ...)
```

## Arguments

- x, y, ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/topic-data-mask.html)\>
  List of name-value pairs in the form `aesthetic = variable` describing
  which variables in the layer data should be mapped to which aesthetics
  used by the paired geom/stat. The expression `variable` is evaluated
  within the layer data, so there is no need to refer to the original
  dataset (i.e., use `ggplot(df, aes(variable))` instead of
  `ggplot(df, aes(df$variable))`). The names for x and y aesthetics are
  typically omitted because they are so common; all other aesthetics
  must be named.

## Value

An S7 object representing a list with class `mapping`. Components of the
list are either quosures or constants.

## Details

This function also standardises aesthetic names by converting `color` to
`colour` (also in substrings, e.g., `point_color` to `point_colour`) and
translating old style R names to ggplot names (e.g., `pch` to `shape`
and `cex` to `size`).

## Note

Using [`I()`](https://rdrr.io/r/base/AsIs.html) to create objects of
class 'AsIs' causes scales to ignore the variable and assumes the
wrapped variable is direct input for the grid package. Please be aware
that variables are sometimes combined, like in some stats or position
adjustments, that may yield unexpected results with 'AsIs' variables.

## Quasiquotation

`aes()` is a [quoting
function](https://rlang.r-lib.org/reference/topic-defuse.html). This
means that its inputs are quoted to be evaluated in the context of the
data. This makes it easy to work with variables from the data frame
because you can name those directly. The flip side is that you have to
use
[quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html) to
program with `aes()`. See a tidy evaluation tutorial such as the [dplyr
programming
vignette](https://dplyr.tidyverse.org/articles/programming.html) to
learn more about these techniques.

## See also

[`vars()`](https://ggplot2.tidyverse.org/dev/reference/vars.md) for
another quoting function designed for faceting specifications.

Run
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/dev/articles/ggplot2-specs.md)
to see an overview of other aesthetics that can be modified.

[Delayed
evaluation](https://ggplot2.tidyverse.org/dev/reference/aes_eval.md) for
working with computed variables.

Other aesthetics documentation:
[`aes_colour_fill_alpha`](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md),
[`aes_group_order`](https://ggplot2.tidyverse.org/dev/reference/aes_group_order.md),
[`aes_linetype_size_shape`](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md),
[`aes_position`](https://ggplot2.tidyverse.org/dev/reference/aes_position.md)

## Examples

``` r
aes(x = mpg, y = wt)
#> Aesthetic mapping: 
#> * `x` -> `mpg`
#> * `y` -> `wt`
aes(mpg, wt)
#> Aesthetic mapping: 
#> * `x` -> `mpg`
#> * `y` -> `wt`

# You can also map aesthetics to functions of variables
aes(x = mpg ^ 2, y = wt / cyl)
#> Aesthetic mapping: 
#> * `x` -> `mpg^2`
#> * `y` -> `wt/cyl`

# Or to constants
aes(x = 1, colour = "smooth")
#> Aesthetic mapping: 
#> * `x`      -> 1
#> * `colour` -> "smooth"

# Aesthetic names are automatically standardised
aes(col = x)
#> Aesthetic mapping: 
#> * `colour` -> `x`
aes(fg = x)
#> Aesthetic mapping: 
#> * `colour` -> `x`
aes(color = x)
#> Aesthetic mapping: 
#> * `colour` -> `x`
aes(colour = x)
#> Aesthetic mapping: 
#> * `colour` -> `x`

# aes() is passed to either ggplot() or specific layer. Aesthetics supplied
# to ggplot() are used as defaults for every layer.
ggplot(mpg, aes(displ, hwy)) + geom_point()

ggplot(mpg) + geom_point(aes(displ, hwy))


# Tidy evaluation ----------------------------------------------------
# aes() automatically quotes all its arguments, so you need to use tidy
# evaluation to create wrappers around ggplot2 pipelines. The
# simplest case occurs when your wrapper takes dots:
scatter_by <- function(data, ...) {
  ggplot(data) + geom_point(aes(...))
}
scatter_by(mtcars, disp, drat)


# If your wrapper has a more specific interface with named arguments,
# you need the "embrace operator":
scatter_by <- function(data, x, y) {
  ggplot(data) + geom_point(aes({{ x }}, {{ y }}))
}
scatter_by(mtcars, disp, drat)


# Note that users of your wrapper can use their own functions in the
# quoted expressions and all will resolve as it should!
cut3 <- function(x) cut_number(x, 3)
scatter_by(mtcars, cut3(disp), drat)
```
