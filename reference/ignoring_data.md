# Ignoring and exposing data

The `.ignore_data()` function is used to hide `<AsIs>` columns during
scale interactions in
[`ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.md).
The `.expose_data()` function is used to restore hidden columns.

## Usage

``` r
.ignore_data(data)

.expose_data(data)
```

## Arguments

- data:

  A list of `<data.frame>`s.

## Value

A modified list of `<data.frame>s`

## Examples

``` r
data <- list(
  data.frame(x = 1:3, y = I(1:3)),
  data.frame(w = I(1:3), z = 1:3)
)

ignored <- .ignore_data(data)
str(ignored)
#> List of 2
#>  $ :'data.frame':    3 obs. of  2 variables:
#>   ..$ x       : int [1:3] 1 2 3
#>   ..$ .ignored:'data.frame': 3 obs. of  1 variable:
#>   .. ..$ y: 'AsIs' int [1:3] 1 2 3
#>  $ :'data.frame':    3 obs. of  2 variables:
#>   ..$ z       : int [1:3] 1 2 3
#>   ..$ .ignored:'data.frame': 3 obs. of  1 variable:
#>   .. ..$ w: 'AsIs' int [1:3] 1 2 3

.expose_data(ignored)
#> [[1]]
#>   x y
#> 1 1 1
#> 2 2 2
#> 3 3 3
#> 
#> [[2]]
#>   z w
#> 1 1 1
#> 2 2 2
#> 3 3 3
#> 
```
