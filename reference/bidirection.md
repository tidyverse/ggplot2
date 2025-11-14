# Utilities for working with bidirectional layers

These functions are what underpins the ability of certain geoms to work
automatically in both directions. See the *Extending ggplot2* vignette
for how they are used when implementing `Geom`, `Stat`, and `Position`
classes.

## Usage

``` r
has_flipped_aes(
  data,
  params = list(),
  main_is_orthogonal = NA,
  range_is_orthogonal = NA,
  group_has_equal = FALSE,
  ambiguous = FALSE,
  main_is_continuous = FALSE,
  main_is_optional = FALSE,
  default = FALSE
)

flip_data(data, flip = NULL)

flipped_names(flip = FALSE)
```

## Arguments

- data:

  The layer data

- params:

  The parameters of the `Stat`/`Geom`. Only the `orientation` parameter
  will be used.

- main_is_orthogonal:

  If only `x` or `y` are present do they correspond to the main
  orientation or the reverse. E.g. If `TRUE` and `y` is present it is
  not flipped. If `NA` this check will be ignored.

- range_is_orthogonal:

  If `xmin`/`xmax` or `ymin`/`ymax` is present do they correspond to the
  main orientation or reverse. If `NA` this check will be ignored.

- group_has_equal:

  Is it expected that grouped data has either a single `x` or `y` value
  that will correspond to the orientation.

- ambiguous:

  Is the layer ambiguous in its mapping by nature. If so, it will only
  be flipped if `params$orientation == "y"`

- main_is_continuous:

  If there is a discrete and continuous axis, does the continuous one
  correspond to the main orientation?

- main_is_optional:

  Is the main axis aesthetic optional and, if not given, set to `0`

- default:

  The logical value to return if no orientation can be discerned from
  the data.

- flip:

  Logical. Is the layer flipped.

## Value

`has_flipped_aes()` returns `TRUE` if it detects a layer in the other
orientation and `FALSE` otherwise. `flip_data()` will return the input
unchanged if `flip = FALSE` and the data with flipped aesthetic names if
`flip = TRUE`. `flipped_names()` returns a named list of strings. If
`flip = FALSE` the name of the element will correspond to the element,
e.g. `flipped_names(FALSE)$x == "x"` and if `flip = TRUE` it will
correspond to the flipped name, e.g. `flipped_names(FALSE)$x == "y"`

## Details

`has_flipped_aes()` is used to sniff out the orientation of the layer
from the data. It has a range of arguments that can be used to finetune
the sniffing based on what the data should look like. `flip_data()` will
switch the column names of the data so that it looks like x-oriented
data. `flipped_names()` provides a named list of aesthetic names that
corresponds to the orientation of the layer.

## Controlling the sniffing

How the layer data should be interpreted depends on its specific
features. `has_flipped_aes()` contains a range of flags for defining
what certain features in the data correspond to:

- `main_is_orthogonal`: This argument controls how the existence of only
  a `x` or `y` aesthetic is understood. If `TRUE` then the existing
  aesthetic would be then secondary axis. This behaviour is present in
  [`stat_ydensity()`](https://ggplot2.tidyverse.org/reference/geom_violin.md)
  and
  [`stat_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md).
  If `FALSE` then the existing aesthetic is the main axis as seen in
  e.g.
  [`stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.md),
  [`geom_count()`](https://ggplot2.tidyverse.org/reference/geom_count.md),
  and
  [`stat_density()`](https://ggplot2.tidyverse.org/reference/geom_density.md).

- `range_is_orthogonal`: This argument controls whether the existence of
  range-like aesthetics (e.g. `xmin` and `xmax`) represents the main or
  secondary axis. If `TRUE` then the range is given for the secondary
  axis as seen in e.g.
  [`geom_ribbon()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.md)
  and
  [`geom_linerange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.md).

- `group_has_equal`: This argument controls whether to test for equality
  of all `x` and `y` values inside each group and set the main axis to
  the one where all is equal. This test is only performed if `TRUE`, and
  only after less computationally heavy tests has come up empty handed.
  Examples are
  [`stat_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md)
  and
  [stat_ydensity](https://ggplot2.tidyverse.org/reference/geom_violin.md)

- `ambiguous`: This argument tells the function that the layer, while
  bidirectional, doesn't treat each axis differently. It will circumvent
  any data based guessing and only take hint from the `orientation`
  element in `params`. If this is not present it will fall back to
  `FALSE`. Examples are
  [`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.md)
  and
  [`geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.md)

- `main_is_continuous`: This argument controls how the test for
  discreteness in the scales should be interpreted. If `TRUE` then the
  main axis will be the one which is not discrete-like. Conversely, if
  `FALSE` the main axis will be the discrete-like one. Examples of
  `TRUE` is
  [`stat_density()`](https://ggplot2.tidyverse.org/reference/geom_density.md)
  and
  [`stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.md),
  while examples of `FALSE` is
  [`stat_ydensity()`](https://ggplot2.tidyverse.org/reference/geom_violin.md)
  and
  [`stat_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md)

- `main_is_optional`: This argument controls the rare case of layers
  were the main direction is an optional aesthetic. This is only seen in
  [`stat_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md)
  where `x` is set to `0` if not given. If `TRUE` there will be a check
  for whether all `x` or all `y` are equal to `0`
