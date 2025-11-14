# Transform spatial position data

Helper function that can transform spatial position data (pairs of x, y
values) among coordinate systems. This is implemented as a thin wrapper
around
[`sf::sf_project()`](https://r-spatial.github.io/sf/reference/sf_project.html).

## Usage

``` r
sf_transform_xy(data, target_crs, source_crs, authority_compliant = FALSE)
```

## Arguments

- data:

  Data frame or list containing numerical columns `x` and `y`.

- target_crs, source_crs:

  Target and source coordinate reference systems. If `NULL` or `NA`, the
  data is not transformed.

- authority_compliant:

  logical; `TRUE` means handle axis order authority compliant (e.g.
  EPSG:4326 implying `x = lat`, `y = lon`), `FALSE` means use
  visualisation order (i.e. always `x = lon`, `y = lat`). Default is
  `FALSE`.

## Value

A copy of the input data with `x` and `y` replaced by transformed
values.

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)) {
# location of cities in NC by long (x) and lat (y)
data <- data.frame(
  city = c("Charlotte", "Raleigh", "Greensboro"),
  x =  c(-80.843, -78.639, -79.792),
  y = c(35.227, 35.772, 36.073)
)

# transform to projected coordinates
data_proj <- sf_transform_xy(data, 3347, 4326)
data_proj

# transform back
sf_transform_xy(data_proj, 4326, 3347)
}
#>         city       x      y
#> 1  Charlotte -80.843 35.227
#> 2    Raleigh -78.639 35.772
#> 3 Greensboro -79.792 36.073
```
