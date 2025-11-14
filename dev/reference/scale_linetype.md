# Scale for line patterns

Default line types based on a set supplied by Richard Pearson,
University of Manchester. Continuous values can not be mapped to line
types unless `scale_linetype_binned()` is used. Still, as linetypes has
no inherent order, this use is not advised.

## Usage

``` r
scale_linetype(name = waiver(), ..., aesthetics = "linetype")

scale_linetype_binned(name = waiver(), ..., aesthetics = "linetype")

scale_linetype_continuous(...)

scale_linetype_discrete(name = waiver(), ..., aesthetics = "linetype")
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

  `guide`

  :   A function used to create a guide or its name. See
      [`guides()`](https://ggplot2.tidyverse.org/dev/reference/guides.md)
      for more information.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `super`

  :   The super class to use for the constructed scale

- aesthetics:

  The names of the aesthetics that this scale works with.

## Details

Lines can be referred to by number, name or hex code. Contrary to base R
graphics, `NA`s are interpreted as blanks.

![Named linetypes by number and name](figures/linetype_table.svg)

## See also

The documentation for [differentiation related
aesthetics](https://ggplot2.tidyverse.org/dev/reference/aes_linetype_size_shape.md).

Other linetype scales:
[`scale_linetype_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md),
[`scale_linetype_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md).

The [line type
section](https://ggplot2-book.org/scales-other#sec-scale-linetype) of
the online ggplot2 book.

## Examples

``` r
base <- ggplot(economics_long, aes(date, value01))
base + geom_line(aes(group = variable))

base + geom_line(aes(linetype = variable))


# See scale_manual for more flexibility

# Common line types ----------------------------
df_lines <- data.frame(
  linetype = factor(
    1:4,
    labels = c("solid", "longdash", "dashed", "dotted")
  )
)
ggplot(df_lines) +
  geom_hline(aes(linetype = linetype, yintercept = 0), linewidth = 2) +
  scale_linetype_identity() +
  facet_grid(linetype ~ .) +
  theme_void(20)
```
