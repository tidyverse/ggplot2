# Build ggplot for rendering.

`build_ggplot()` takes the plot object, and performs all steps necessary
to produce an object that can be rendered. This function outputs two
pieces: a list of data frames (one for each layer), and a panel object,
which contain all information about axis limits, breaks etc. The
`ggplot_build()` function is vestigial and `build_ggplot()` should be
used instead.

## Usage

``` r
ggplot_build(plot, ...)

get_layer_data(plot = get_last_plot(), i = 1L)

layer_data(plot = get_last_plot(), i = 1L)

get_panel_scales(plot = get_last_plot(), i = 1L, j = 1L)

layer_scales(plot = get_last_plot(), i = 1L, j = 1L)

get_layer_grob(plot = get_last_plot(), i = 1L)

layer_grob(plot = get_last_plot(), i = 1L)
```

## Arguments

- plot:

  ggplot object

- ...:

  Not currently in use.

- i:

  An integer or a name of a layer. In `get_layer_data()`, the data to
  return (in the order added to the plot). In `get_layer_grob()`, the
  grob to return (in the order added to the plot). In
  `get_panel_scales()` (only integers allowed), the row of a facet to
  return scales for.

- j:

  An integer. In `get_panel_scales()`, the column of a facet to return
  scales for.

## Details

`get_layer_data()`, `get_layer_grob()`, and `get_panel_scales()` are
helper functions that return the data, grob, or scales associated with a
given layer. These are useful for tests.

## See also

[`print.ggplot()`](https://ggplot2.tidyverse.org/dev/reference/print.ggplot.md)
and
[`benchplot()`](https://ggplot2.tidyverse.org/dev/reference/benchplot.md)
for functions that contain the complete set of steps for generating a
ggplot2 plot.

The [build step
section](https://ggplot2-book.org/internals#sec-ggplotbuild) of the
online ggplot2 book.
