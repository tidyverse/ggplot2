# Resolve and get geom defaults

Resolve and get geom defaults

## Usage

``` r
get_geom_defaults(geom, theme = theme_get())
```

## Arguments

- geom:

  Some definition of a geom:

  - A `function` that creates a layer, e.g.
    [`geom_path()`](https://ggplot2.tidyverse.org/dev/reference/geom_path.md).

  - A layer created by such function

  - A string naming a geom class in snake case without the
    `geom_`-prefix, e.g. `"contour_filled"`.

  - A geom class object.

- theme:

  A [`theme`](https://ggplot2.tidyverse.org/dev/reference/theme.md)
  object. Defaults to the current global theme.

## Value

A list of aesthetics

## Examples

``` r
# Using a function
get_geom_defaults(geom_raster)
#>        fill alpha
#> 1 #333333FF    NA

# Using a layer includes static aesthetics as default
get_geom_defaults(geom_tile(fill = "white"))
#>    fill colour linewidth linetype alpha width height
#> 1 white     NA       0.2        1    NA     1      1

# Using a class name
get_geom_defaults("density_2d")
#>    colour linewidth linetype alpha
#> 1 #3366FF       0.5        1    NA

# Using a class
get_geom_defaults(GeomPoint)
#>   shape colour fill size alpha stroke
#> 1    19  black   NA  1.5    NA    0.5

# Changed theme
get_geom_defaults("point", theme(geom = element_geom(ink = "purple")))
#>   shape colour fill size alpha stroke
#> 1    19 purple   NA  1.5    NA    0.5
```
