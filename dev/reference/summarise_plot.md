# Summarise built plot objects

These functions provide summarised information about built ggplot
objects.

## Usage

``` r
summarise_layout(p)

summarise_coord(p)

summarise_layers(p)
```

## Arguments

- p:

  A ggplot_built object.

## Details

There are three types of summary that can be obtained: A summary of the
plot layout, a summary of the plot coord, and a summary of plot layers.

## Layout summary

The function `summarise_layout()` returns a table that provides
information about the plot panel(s) in the built plot. The table has the
following columns:

- `panel`:

  A factor indicating the individual plot panels.

- `row`:

  Row number in the grid of panels.

- `col`:

  Column number in the grid of panels.

- `vars`:

  A list of lists. For each panel, the respective list provides the
  variables and their values that specify the panel.

- `xmin`, `xmax`:

  The minimum and maximum values of the variable mapped to the x
  aesthetic, in transformed coordinates.

- `ymin`, `ymax`:

  The minimum and maximum values of the variable mapped to the y
  aesthetic, in transformed coordinates.

- `xscale`:

  The scale object applied to the x aesthetic.

- `yscale`:

  The scale object applied to the y aesthetic.

Importantly, the values for `xmin`, `xmax`, `ymin`, `ymax`, `xscale`,
and `yscale` are determined by the variables that are mapped to `x` and
`y` in the [`aes()`](https://ggplot2.tidyverse.org/dev/reference/aes.md)
call. So even if a coord changes how x and y are shown in the final plot
(as is the case for
[`coord_flip()`](https://ggplot2.tidyverse.org/dev/reference/coord_flip.md)
or
[`coord_polar()`](https://ggplot2.tidyverse.org/dev/reference/coord_radial.md)),
these changes have no effect on the results returned by
`summarise_plot()`.

## Coord summary

The function `summarise_coord()` returns information about the log base
for coordinates that are log-transformed in
[`coord_transform()`](https://ggplot2.tidyverse.org/dev/reference/coord_transform.md),
and it also indicates whether the coord has flipped the x and y axes.

## Layer summary

The function `summarise_layers()` returns a table with a single column,
`mapping`, which contains information about aesthetic mapping for each
layer.

## Examples

``` r
p <-
  ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  facet_wrap(~class)
b <- ggplot_build(p)

summarise_layout(b)
#>   panel row col       vars xmin xmax ymin ymax
#> 1     1   1   1    2seater 1.33 7.27 10.4 45.6
#> 2     2   1   2    compact 1.33 7.27 10.4 45.6
#> 3     3   1   3    midsize 1.33 7.27 10.4 45.6
#> 4     4   2   1    minivan 1.33 7.27 10.4 45.6
#> 5     5   2   2     pickup 1.33 7.27 10.4 45.6
#> 6     6   2   3 subcompact 1.33 7.27 10.4 45.6
#> 7     7   3   1        suv 1.33 7.27 10.4 45.6
#>                          xscale                        yscale
#> 1 <environment: 0x55c014ddff90> <environment: 0x55c012807460>
#> 2 <environment: 0x55c014ddff90> <environment: 0x55c012807460>
#> 3 <environment: 0x55c014ddff90> <environment: 0x55c012807460>
#> 4 <environment: 0x55c014ddff90> <environment: 0x55c012807460>
#> 5 <environment: 0x55c014ddff90> <environment: 0x55c012807460>
#> 6 <environment: 0x55c014ddff90> <environment: 0x55c012807460>
#> 7 <environment: 0x55c014ddff90> <environment: 0x55c012807460>
summarise_coord(b)
#> $xlog
#> [1] NA
#> 
#> $ylog
#> [1] NA
#> 
#> $flip
#> [1] FALSE
#> 
summarise_layers(b)
#>        mapping
#> 1 ~displ, ~hwy
```
