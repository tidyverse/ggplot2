# Cartesian coordinates with fixed "aspect ratio"

A fixed scale coordinate system forces a specified ratio between the
physical representation of data units on the axes. The ratio represents
the number of units on the y-axis equivalent to one unit on the x-axis.
The default, `ratio = 1`, ensures that one unit on the x-axis is the
same length as one unit on the y-axis. Ratios higher than one make units
on the y axis longer than units on the x-axis, and vice versa. This is
similar to
[`MASS::eqscplot()`](https://rdrr.io/pkg/MASS/man/eqscplot.html), but it
works for all types of graphics.

## Usage

``` r
coord_fixed(ratio = 1, ...)
```

## Arguments

- ratio:

  aspect ratio, expressed as `y / x`. Can be `NULL` (default) to not use
  an aspect ratio. Using `1` ensures that one unit on the x-axis is the
  same length as one unit on the y-axis. Ratios higher than one make
  units on the y-axis longer than units on the x-axis, and vice versa.

- ...:

  Arguments passed on to
  [`coord_cartesian`](https://ggplot2.tidyverse.org/reference/coord_cartesian.md)

  `xlim,ylim`

  :   Limits for the x and y axes.

  `expand`

  :   If `TRUE`, the default, adds a small expansion factor to the
      limits to ensure that data and axes don't overlap. If `FALSE`,
      limits are taken exactly from the data or `xlim`/`ylim`. Giving a
      logical vector will separately control the expansion for the four
      directions (top, left, bottom and right). The `expand` argument
      will be recycled to length 4 if necessary. Alternatively, can be a
      named logical vector to control a single direction, e.g.
      `expand = c(bottom = FALSE)`.

  `default`

  :   Is this the default coordinate system? If `FALSE` (the default),
      then replacing this coordinate system with another one creates a
      message alerting the user that the coordinate system is being
      replaced. If `TRUE`, that warning is suppressed.

  `clip`

  :   Should drawing be clipped to the extent of the plot panel? A
      setting of `"on"` (the default) means yes, and a setting of
      `"off"` means no. In most cases, the default of `"on"` should not
      be changed, as setting `clip = "off"` can cause unexpected
      results. It allows drawing of data points anywhere on the plot,
      including in the plot margins. If limits are set via `xlim` and
      `ylim` and some data points fall outside those limits, then those
      data points may show up in places such as the axes, the legend,
      the plot title, or the plot margins.

  `reverse`

  :   A string giving which directions to reverse. `"none"` (default)
      keeps directions as is. `"x"` and `"y"` can be used to reverse
      their respective directions. `"xy"` can be used to reverse both
      directions.

## Examples

``` r
# ensures that the ranges of axes are equal to the specified ratio by
# adjusting the plot aspect ratio

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p + coord_fixed(ratio = 1)

p + coord_fixed(ratio = 5)

p + coord_fixed(ratio = 1/5)

p + coord_fixed(xlim = c(15, 30))


# Resize the plot to see that the specified aspect ratio is maintained
```
