# Reports wether `x` is a type of object

Reports wether `x` is a type of object

## Usage

``` r
is_ggproto(x)

is.ggproto(x) # Deprecated

is_mapping(x)

is_geom(x)

is_layer(x)

is_coord(x)

is.Coord(x) # Deprecated

is_facet(x)

is.facet(x) # Deprecated

is_stat(x)

is_margin(x)

is_theme_element(x, type = "any")

is_guide(x)

is_guides(x)

is_ggplot(x)

is.ggplot(x) # Deprecated

is_position(x)

is_scale(x)

is_theme(x)

is.theme(x) # Deprecated
```

## Arguments

- x:

  An object to test

- type:

  For testing elements: the type of element to expect. One of `"blank"`,
  `"rect"`, `"line"`, `"text"`, `"polygon"`, `"point"` or `"geom"`.
