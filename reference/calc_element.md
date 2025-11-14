# Calculate the element properties, by inheriting properties from its parents

Calculate the element properties, by inheriting properties from its
parents

## Usage

``` r
calc_element(
  element,
  theme,
  verbose = FALSE,
  skip_blank = FALSE,
  call = caller_env()
)
```

## Arguments

- element:

  The name of the theme element to calculate

- theme:

  A theme object (like
  [`theme_grey()`](https://ggplot2.tidyverse.org/reference/ggtheme.md))

- verbose:

  If TRUE, print out which elements this one inherits from

- skip_blank:

  If TRUE, elements of type `element_blank` in the inheritance hierarchy
  will be ignored.

## Examples

``` r
t <- theme_grey()
calc_element('text', t)
#> <ggplot2::element_text>
#>  @ family       : chr ""
#>  @ face         : chr "plain"
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : chr "black"
#>  @ size         : num 11
#>  @ hjust        : num 0.5
#>  @ vjust        : num 0.5
#>  @ angle        : num 0
#>  @ lineheight   : num 0.9
#>  @ margin       : <ggplot2::margin> num [1:4] 0 0 0 0
#>  @ debug        : logi FALSE
#>  @ inherit.blank: logi TRUE

# Compare the "raw" element definition to the element with calculated inheritance
t$axis.text.x
#> <ggplot2::element_text>
#>  @ family       : NULL
#>  @ face         : NULL
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : NULL
#>  @ size         : NULL
#>  @ hjust        : NULL
#>  @ vjust        : num 1
#>  @ angle        : NULL
#>  @ lineheight   : NULL
#>  @ margin       : <ggplot2::margin> num [1:4] 2.2 0 0 0
#>  @ debug        : NULL
#>  @ inherit.blank: logi TRUE
calc_element('axis.text.x', t, verbose = TRUE)
#> axis.text.x -->
#> axis.text
#> axis.text -->
#> text
#> text -->
#> nothing (top level)
#> <ggplot2::element_text>
#>  @ family       : chr ""
#>  @ face         : chr "plain"
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : chr "#4D4D4DFF"
#>  @ size         : num 8.8
#>  @ hjust        : num 0.5
#>  @ vjust        : num 1
#>  @ angle        : num 0
#>  @ lineheight   : num 0.9
#>  @ margin       : <ggplot2::margin> num [1:4] 2.2 0 0 0
#>  @ debug        : logi FALSE
#>  @ inherit.blank: logi TRUE

# This reports that axis.text.x inherits from axis.text,
# which inherits from text. You can view each of them with:
t$axis.text.x
#> <ggplot2::element_text>
#>  @ family       : NULL
#>  @ face         : NULL
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : NULL
#>  @ size         : NULL
#>  @ hjust        : NULL
#>  @ vjust        : num 1
#>  @ angle        : NULL
#>  @ lineheight   : NULL
#>  @ margin       : <ggplot2::margin> num [1:4] 2.2 0 0 0
#>  @ debug        : NULL
#>  @ inherit.blank: logi TRUE
t$axis.text
#> <ggplot2::element_text>
#>  @ family       : NULL
#>  @ face         : NULL
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : chr "#4D4D4DFF"
#>  @ size         : 'rel' num 0.8
#>  @ hjust        : NULL
#>  @ vjust        : NULL
#>  @ angle        : NULL
#>  @ lineheight   : NULL
#>  @ margin       : NULL
#>  @ debug        : NULL
#>  @ inherit.blank: logi TRUE
t$text
#> <ggplot2::element_text>
#>  @ family       : chr ""
#>  @ face         : chr "plain"
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : chr "black"
#>  @ size         : num 11
#>  @ hjust        : num 0.5
#>  @ vjust        : num 0.5
#>  @ angle        : num 0
#>  @ lineheight   : num 0.9
#>  @ margin       : <ggplot2::margin> num [1:4] 0 0 0 0
#>  @ debug        : logi FALSE
#>  @ inherit.blank: logi TRUE
```
