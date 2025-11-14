# Fortify method for map objects

**\[deprecated\]**

This function turns a map into a data frame that can more easily be
plotted with ggplot2.

## Usage

``` r
# S3 method for class 'map'
fortify(model, data, ...)
```

## Arguments

- model:

  map object

- data:

  not used by this method

- ...:

  not used by this method

## See also

[`map_data()`](https://ggplot2.tidyverse.org/reference/map_data.md) and
[`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.md)

## Examples

``` r
if (require("maps")) {
ca <- map("county", "ca", plot = FALSE, fill = TRUE)
head(fortify(ca))
ggplot(ca, aes(long, lat)) +
  geom_polygon(aes(group = group))
}
#> Warning: `fortify(<map>)` was deprecated in ggplot2 4.0.0.
#> ℹ Please use `map_data()` instead.


if (require("maps")) {
tx <- map("county", "texas", plot = FALSE, fill = TRUE)
head(fortify(tx))
ggplot(tx, aes(long, lat)) +
  geom_polygon(aes(group = group), colour = "white")
}
```
