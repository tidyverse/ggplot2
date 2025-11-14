# Merge a parent element into a child element

This is a generic and element classes must provide an implementation of
this method

## Usage

``` r
merge_element(new, old, ...)
```

## Arguments

- new:

  The child element in the theme hierarchy

- old:

  The parent element in the theme hierarchy

## Value

A modified version of `new` updated with the properties of `old`

## Examples

``` r
new <- element_text(colour = "red")
old <- element_text(colour = "blue", size = 10)

# Adopt size but ignore colour
merge_element(new, old)
#> <ggplot2::element_text>
#>  @ family       : NULL
#>  @ face         : NULL
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : chr "red"
#>  @ size         : num 10
#>  @ hjust        : NULL
#>  @ vjust        : NULL
#>  @ angle        : NULL
#>  @ lineheight   : NULL
#>  @ margin       : NULL
#>  @ debug        : NULL
#>  @ inherit.blank: logi FALSE
```
