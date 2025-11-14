# Label with mathematical expressions

`label_bquote()` offers a flexible way of labelling facet rows or
columns with plotmath expressions. Backquoted variables will be replaced
with their value in the facet.

## Usage

``` r
label_bquote(rows = NULL, cols = NULL, default)
```

## Arguments

- rows:

  Backquoted labelling expression for rows.

- cols:

  Backquoted labelling expression for columns.

- default:

  Unused, kept for compatibility.

## See also

[labellers](https://ggplot2.tidyverse.org/dev/reference/labellers.md),
[`labeller()`](https://ggplot2.tidyverse.org/dev/reference/labeller.md),

## Examples

``` r
# The variables mentioned in the plotmath expression must be
# backquoted and referred to by their names.
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p + facet_grid(vs ~ ., labeller = label_bquote(alpha ^ .(vs)))

p + facet_grid(. ~ vs, labeller = label_bquote(cols = .(vs) ^ .(vs)))

p + facet_grid(. ~ vs + am, labeller = label_bquote(cols = .(am) ^ .(vs)))
```
