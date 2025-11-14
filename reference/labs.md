# Modify axis, legend, and plot labels

Good labels are critical for making your plots accessible to a wider
audience. Always ensure the axis and legend labels display the full
variable name. Use the plot `title` and `subtitle` to explain the main
findings. It's common to use the `caption` to provide information about
the data source. `tag` can be used for adding identification tags to
differentiate between multiple plots.

**\[superseded\]**: `xlab()`, `ylab()` and `ggtitle()` are superseded.
It is recommended to use the `labs(x, y, title, subtitle)` arguments
instead.

`get_labs()` retrieves completed labels from a plot.

## Usage

``` r
labs(
  ...,
  title = waiver(),
  subtitle = waiver(),
  caption = waiver(),
  tag = waiver(),
  dictionary = waiver(),
  alt = waiver(),
  alt_insight = waiver()
)

xlab(label)

ylab(label)

ggtitle(label, subtitle = waiver())

get_labs(plot = get_last_plot())
```

## Arguments

- ...:

  New name-value pairs. The name should be an aesthetic. The values can
  be one of the following:

  - A string or expression to set a label verbatim.

  - A function to use as formatter for the default label.

  - `NULL` to remove a label.

  - A [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.md) to
    use the default label.

- title:

  The text for the title.

- subtitle:

  The text for the subtitle for the plot which will be displayed below
  the title.

- caption:

  The text for the caption which will be displayed in the bottom-right
  of the plot by default.

- tag:

  The text for the tag label which will be displayed at the top-left of
  the plot by default.

- dictionary:

  A named character vector to serve as dictionary. Automatically derived
  labels, such as those based on variables will be matched with
  `names(dictionary)` and replaced by the matching entry in
  `dictionary`.

- alt, alt_insight:

  Text used for the generation of alt-text for the plot. See
  [get_alt_text](https://ggplot2.tidyverse.org/reference/get_alt_text.md)
  for examples. `alt` can also be a function that takes the plot as
  input and returns text as output. `alt` also accepts rlang
  [lambda](https://rlang.r-lib.org/reference/as_function.html) function
  notation.

- label:

  The title of the respective axis (for `xlab()` or `ylab()`) or of the
  plot (for `ggtitle()`).

- plot:

  A ggplot object

## Details

You can also set axis and legend labels in the individual scales (using
the first argument, the `name`). If you're changing other scale options,
this is recommended.

If a plot already has a title, subtitle, caption, etc., and you want to
remove it, you can do so by setting the respective argument to `NULL`.
For example, if plot `p` has a subtitle, then
`p + labs(subtitle = NULL)` will remove the subtitle from the plot.

## See also

The [plot and axis titles
section](https://ggplot2-book.org/annotations#sec-titles) of the online
ggplot2 book.

## Examples

``` r
p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p + labs(colour = "Cylinders")

p + labs(x = "New x label")


# Set labels by variable name instead of aesthetic
p + labs(dictionary = c(
  disp = "Displacment", # Not in use
  cyl  = "Number of cylinders",
  mpg  = "Miles per gallon",
  wt   = "Weight (1000 lbs)"
))


# The plot title appears at the top-left, with the subtitle
# display in smaller text underneath it
p + labs(title = "New plot title")

p + labs(title = "New plot title", subtitle = "A subtitle")


# The caption appears in the bottom-right, and is often used for
# sources, notes or copyright
p + labs(caption = "(based on data from ...)")


# The plot tag appears at the top-left, and is typically used
# for labelling a subplot with a letter.
p + labs(title = "title", tag = "A")


# If you want to remove a label, set it to NULL.
p +
 labs(title = "title") +
 labs(title = NULL)
```
