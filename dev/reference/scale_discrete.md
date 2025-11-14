# Position scales for discrete data

`scale_x_discrete()` and `scale_y_discrete()` are used to set the values
for discrete x and y scale aesthetics. For simple manipulation of scale
labels and limits, you may wish to use
[`labs()`](https://ggplot2.tidyverse.org/dev/reference/labs.md) and
[`lims()`](https://ggplot2.tidyverse.org/dev/reference/lims.md) instead.

## Usage

``` r
scale_x_discrete(
  name = waiver(),
  ...,
  palette = seq_len,
  expand = waiver(),
  guide = waiver(),
  position = "bottom",
  sec.axis = waiver(),
  continuous.limits = NULL
)

scale_y_discrete(
  name = waiver(),
  ...,
  palette = seq_len,
  expand = waiver(),
  guide = waiver(),
  position = "left",
  sec.axis = waiver(),
  continuous.limits = NULL
)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md),
  the default, the name of the scale is taken from the first mapping
  used for that aesthetic. If `NULL`, the legend title will be omitted.

- ...:

  Arguments passed on to
  [`discrete_scale`](https://ggplot2.tidyverse.org/dev/reference/discrete_scale.md)

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
        for the default breaks (the scale limits)

      - A character vector of breaks

      - A function that takes the limits as input and returns breaks as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `limits`

  :   One of:

      - `NULL` to use the default scale values

      - A character vector that defines possible values of the scale and
        their order

      - A function that accepts the existing (automatic) values and
        returns new ones. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `drop`

  :   Should unused factor levels be omitted from the scale? The
      default, `TRUE`, uses the levels that appear in the data; `FALSE`
      includes the levels in the factor. Please note that to display
      every level in a legend, the layer should use
      `show.legend = TRUE`.

  `na.translate`

  :   Unlike continuous scales, discrete scales can easily show missing
      values, and do so by default. If you want to remove missing values
      from a discrete scale, specify `na.translate = FALSE`.

  `na.value`

  :   If `na.translate = TRUE`, what aesthetic value should the missing
      values be displayed as? Does not apply to position scales where
      `NA` is always placed at the far right.

  `aesthetics`

  :   The names of the aesthetics that this scale works with.

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
        for the default breaks (none for discrete, one minor break
        between each major break for continuous)

      - A numeric vector of positions

      - A function that given the limits returns a vector of minor
        breaks. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. When the function has two arguments, it will
        be given the limits and major break positions.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md)
        for the default labels computed by the transformation object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `super`

  :   The super class to use for the constructed scale

- palette:

  A palette function that when called with a single integer argument
  (the number of levels in the scale) returns the numerical values that
  they should take.

- expand:

  For position scales, a vector of range expansion constants used to add
  some padding around the data to ensure that they are placed some
  distance away from the axes. Use the convenience function
  [`expansion()`](https://ggplot2.tidyverse.org/dev/reference/expansion.md)
  to generate the values for the `expand` argument. The defaults are to
  expand the scale by 5% on each side for continuous variables, and by
  0.6 units on each side for discrete variables.

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/dev/reference/guides.md)
  for more information.

- position:

  For position scales, The position of the axis. `left` or `right` for y
  axes, `top` or `bottom` for x axes.

- sec.axis:

  [`dup_axis()`](https://ggplot2.tidyverse.org/dev/reference/sec_axis.md)
  is used to specify a secondary axis.

- continuous.limits:

  One of:

  - `NULL` to use the default scale range

  - A numeric vector of length two providing a display range for the
    scale. Use `NA` to refer to the existing minimum or maximum.

  - A function that accepts the limits and returns a numeric vector of
    length two.

## Details

You can use continuous positions even with a discrete position scale -
this allows you (e.g.) to place labels between bars in a bar chart.
Continuous positions are numeric values starting at one for the first
level, and increasing by one for each level (i.e. the labels are placed
at integer positions). This is what allows jittering to work.

## See also

The [position
documentation](https://ggplot2.tidyverse.org/dev/reference/aes_position.md).

The [discrete position scales
section](https://ggplot2-book.org/scales-position#sec-discrete-position)
of the online ggplot2 book.

Other position scales:
[`scale_x_binned()`](https://ggplot2.tidyverse.org/dev/reference/scale_binned.md),
[`scale_x_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_continuous.md),
[`scale_x_date()`](https://ggplot2.tidyverse.org/dev/reference/scale_date.md)

## Examples

``` r
ggplot(diamonds, aes(cut)) + geom_bar()


# \donttest{
# The discrete position scale is added automatically whenever you
# have a discrete position.

(d <- ggplot(subset(diamonds, carat > 1), aes(cut, clarity)) +
      geom_jitter())


d + scale_x_discrete("Cut")

d +
  scale_x_discrete(
    "Cut",
    labels = c(
      "Fair" = "F",
      "Good" = "G",
      "Very Good" = "VG",
      "Perfect" = "P",
      "Ideal" = "I"
    )
  )


# Use limits to adjust the which levels (and in what order)
# are displayed
d + scale_x_discrete(limits = c("Fair","Ideal"))
#> Warning: Removed 11189 rows containing missing values or values outside the
#> scale range (`geom_point()`).


# you can also use the short hand functions xlim and ylim
d + xlim("Fair","Ideal", "Good")
#> Warning: Removed 9610 rows containing missing values or values outside the
#> scale range (`geom_point()`).

d + ylim("I1", "IF")
#> Warning: Removed 16770 rows containing missing values or values outside the
#> scale range (`geom_point()`).


# See ?reorder to reorder based on the values of another variable
ggplot(mpg, aes(manufacturer, cty)) +
  geom_point()

ggplot(mpg, aes(reorder(manufacturer, cty), cty)) +
  geom_point()

ggplot(mpg, aes(reorder(manufacturer, displ), cty)) +
  geom_point()


# Use abbreviate as a formatter to reduce long names
ggplot(mpg, aes(reorder(manufacturer, displ), cty)) +
  geom_point() +
  scale_x_discrete(labels = abbreviate)

# }
```
