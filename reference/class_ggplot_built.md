# The ggplot built class

The ggplot built class is an intermediate class and represents a
processed ggplot object ready for rendering. It is constructed by
calling
[`ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.md)
on a [ggplot](https://ggplot2.tidyverse.org/reference/class_ggplot.md)
object and is not meant to be instantiated directly. The class can be
rendered to a gtable object by calling the
[`ggplot_gtable()`](https://ggplot2.tidyverse.org/reference/ggplot_gtable.md)
function on a ggplot built class object.

## Usage

``` r
class_ggplot_built(..., data = NULL, layout = NULL, plot = NULL)
```

## Arguments

- ...:

  Reserved for future expansion.

- data:

  A list of plain data frames; one for each layer.

- layout:

  A Layout ggproto object.

- plot:

  A completed ggplot class object.
