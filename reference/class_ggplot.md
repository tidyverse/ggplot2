# The ggplot class

The ggplot class collects the needed information to render a plot. This
class can be constructed using the
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.md)
function.

## Usage

``` r
class_ggplot(
  data = waiver(),
  ...,
  layers = list(),
  scales = NULL,
  guides = NULL,
  mapping = aes(),
  theme = NULL,
  coordinates = coord_cartesian(default = TRUE),
  facet = facet_null(),
  layout = NULL,
  labels = labs(),
  meta = list(),
  plot_env = parent.frame()
)
```

## Arguments

- data:

  A property containing any data coerced by
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.md).

- ...:

  Reserved for future expansion.

- layers:

  A list of layer instances created by
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.md).

- scales:

  A ScalesList ggproto object.

- guides:

  A Guides ggproto object created by
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.md).

- mapping:

  A mapping class object created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.md).

- theme:

  A theme class object created by
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.md).

- coordinates:

  A Coord ggproto object created by `coord_*()` family of functions.

- facet:

  A Facet ggproto object created by `facet_*()` family of functions.

- layout:

  A Layout ggproto object.

- labels:

  A labels object created by
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.md).

- meta:

  A list for additional metadata. This will be deprecated in the future.

- plot_env:

  An environment.
