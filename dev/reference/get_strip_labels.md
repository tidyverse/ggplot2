# Accessing a plot's facet strip labels

This functions retrieves labels from facet strips with the labeller
applied.

## Usage

``` r
get_strip_labels(plot = get_last_plot())
```

## Arguments

- plot:

  A ggplot or build ggplot object.

## Value

`NULL` if there are no labels, otherwise a list of data.frames
containing the labels.

## Examples

``` r
# Basic plot
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point()

get_strip_labels(p) # empty facets
#> NULL
get_strip_labels(p + facet_wrap(year ~ cyl))
#> $facets
#>   year cyl
#> 1 1999   4
#> 2 1999   6
#> 3 1999   8
#> 4 2008   4
#> 5 2008   5
#> 6 2008   6
#> 7 2008   8
#> 
get_strip_labels(p + facet_grid(year ~ cyl))
#> $cols
#>   cyl
#> 1   4
#> 2   5
#> 3   6
#> 4   8
#> 
#> $rows
#>   year
#> 1 1999
#> 2 2008
#> 
```
