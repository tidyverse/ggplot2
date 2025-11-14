# Generate correct scale type for specified limits

Generate correct scale type for specified limits

## Usage

``` r
limits(lims, var, call = caller_env())
```

## Arguments

- lims:

  vector of limits

- var:

  name of variable

## Examples

``` r
ggplot2:::limits(c(1, 5), "x")
#> <ScaleContinuousPosition>
#>  Range:  
#>  Limits:    1 --    5
ggplot2:::limits(c(5, 1), "x")
#> <ScaleContinuousPosition>
#>  Range:  
#>  Limits:   -5 --   -1
ggplot2:::limits(c("A", "b", "c"), "x")
#> <ggproto object: Class ScaleDiscretePosition, ScaleDiscrete, Scale, gg>
#>     aesthetics: x xmin xmax xend xintercept xmin_final xmax_final xlower ...
#>     axis_order: function
#>     break_info: function
#>     break_positions: function
#>     breaks: waiver
#>     call: environment
#>     clone: function
#>     continuous_limits: NULL
#>     dimension: function
#>     drop: TRUE
#>     expand: waiver
#>     fallback_palette: function
#>     get_breaks: function
#>     get_breaks_minor: function
#>     get_labels: function
#>     get_limits: function
#>     get_transformation: function
#>     guide: waiver
#>     is_discrete: function
#>     is_empty: function
#>     labels: waiver
#>     limits: A b c
#>     make_sec_title: function
#>     make_title: function
#>     map: function
#>     map_df: function
#>     minor_breaks: waiver
#>     n.breaks.cache: NULL
#>     na.translate: TRUE
#>     na.value: NA
#>     name: waiver
#>     palette: function
#>     palette.cache: NULL
#>     position: bottom
#>     range: environment
#>     range_c: environment
#>     rescale: function
#>     reset: function
#>     sec_name: function
#>     train: function
#>     train_df: function
#>     transform: function
#>     transform_df: function
#>     super:  <ggproto object: Class ScaleDiscretePosition, ScaleDiscrete, Scale, gg>
ggplot2:::limits(c("A", "b", "c"), "fill")
#> <ggproto object: Class ScaleDiscrete, Scale, gg>
#>     aesthetics: fill
#>     axis_order: function
#>     break_info: function
#>     break_positions: function
#>     breaks: waiver
#>     call: environment
#>     clone: function
#>     dimension: function
#>     drop: TRUE
#>     expand: waiver
#>     fallback_palette: function
#>     get_breaks: function
#>     get_breaks_minor: function
#>     get_labels: function
#>     get_limits: function
#>     get_transformation: function
#>     guide: legend
#>     is_discrete: function
#>     is_empty: function
#>     labels: waiver
#>     limits: A b c
#>     make_sec_title: function
#>     make_title: function
#>     map: function
#>     map_df: function
#>     minor_breaks: waiver
#>     n.breaks.cache: NULL
#>     na.translate: TRUE
#>     na.value: grey50
#>     name: waiver
#>     palette: NULL
#>     palette.cache: NULL
#>     position: left
#>     range: environment
#>     rescale: function
#>     reset: function
#>     train: function
#>     train_df: function
#>     transform: function
#>     transform_df: function
#>     super:  <ggproto object: Class ScaleDiscrete, Scale, gg>
ggplot2:::limits(as.Date(c("2008-01-01", "2009-01-01")), "x")
#> <ScaleContinuousDate>
#>  Range:  
#>  Limits: 1.39e+04 -- 1.42e+04
```
