# Munch coordinates data

This function "munches" lines, dividing each line into many small pieces
so they can be transformed independently. Used inside geom functions.

## Usage

``` r
coord_munch(coord, data, range, segment_length = 0.01, is_closed = FALSE)
```

## Arguments

- coord:

  Coordinate system definition.

- data:

  Data set to transform - should have variables `x` and `y` are chopped
  up into small pieces (as defined by `group`). All other variables are
  duplicated as needed.

- range:

  Panel range specification.

- segment_length:

  Target segment length

- is_closed:

  Whether data should be considered as a closed polygon.
