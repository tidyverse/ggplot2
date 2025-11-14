# Convenience function to transform all position variables.

Convenience function to transform all position variables.

## Usage

``` r
transform_position(df, trans_x = NULL, trans_y = NULL, ...)
```

## Arguments

- trans_x, trans_y:

  Transformation functions for x and y aesthetics. (will transform x,
  xmin, xmax, xend etc)

- ...:

  Additional arguments passed to `trans_x` and `trans_y`.
