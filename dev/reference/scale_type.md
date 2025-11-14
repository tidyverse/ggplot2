# Determine default scale type

You will need to define a method for this method if you want to extend
ggplot2 to handle new types of data. If you simply want to pass the
vector through as an additional aesthetic, return `"identity"`.

## Usage

``` r
scale_type(x)
```

## Arguments

- x:

  A vector

## Value

A character vector of scale types. These will be tried in turn to find a
default scale. For example, if `scale_type()` returns `c("foo", "bar")`
and the vector is used with the colour aesthetic, ggplot2 will first
look for `scale_colour_foo` then `scale_colour_bar`.

## Examples

``` r
scale_type(1:5)
#> [1] "continuous"
scale_type("test")
#> [1] "discrete"
scale_type(Sys.Date())
#> [1] "date"       "continuous"
```
