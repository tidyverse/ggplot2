# Generate grid grob from theme element

The `element_grob()` function is vestigial and `draw_element()` should
be used instead.

## Usage

``` r
element_grob(element, ...)
```

## Arguments

- element:

  Theme element, i.e. `element_rect` or similar.

- ...:

  Other arguments to control specific of rendering. This is usually at
  least position. See the source code for individual methods.
