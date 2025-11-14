# Add components to a plot

`+` is the key to constructing sophisticated ggplot2 graphics. It allows
you to start simple, then get more and more complex, checking your work
at each step.

## Usage

``` r
add_gg(e1, e2)

e1 %+% e2
```

## Arguments

- e1:

  An object of class
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.md) or a
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.md).

- e2:

  A plot component, as described below.

## What can you add?

You can add any of the following types of objects:

- An [`aes()`](https://ggplot2.tidyverse.org/reference/aes.md) object
  replaces the default aesthetics.

- A layer created by a `geom_` or `stat_` function adds a new layer.

- A `scale` overrides the existing scale.

- A [`theme()`](https://ggplot2.tidyverse.org/reference/theme.md)
  modifies the current theme.

- A `coord` overrides the current coordinate system.

- A `facet` specification overrides the current faceting.

To replace the current default data frame, you must use `%+%`, due to S3
method precedence issues.

You can also supply a list, in which case each element of the list will
be added in turn.

## See also

[`theme()`](https://ggplot2.tidyverse.org/reference/theme.md)

## Examples

``` r
base <-
 ggplot(mpg, aes(displ, hwy)) +
 geom_point()
base + geom_smooth()
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


# To override the data, you must use %+%
base %+% subset(mpg, fl == "p")
#> Warning: <ggplot> %+% x was deprecated in ggplot2 4.0.0.
#> ℹ Please use <ggplot> + x instead.


# Alternatively, you can add multiple components with a list.
# This can be useful to return from a function.
base + list(subset(mpg, fl == "p"), geom_smooth())
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```
