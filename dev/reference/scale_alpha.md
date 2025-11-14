# Alpha transparency scales

Alpha-transparency scales are not tremendously useful, but can be a
convenient way to visually down-weight less important observations.
`scale_alpha()` is an alias for `scale_alpha_continuous()` since that is
the most common use of alpha, and it saves a bit of typing.

## Usage

``` r
scale_alpha(name = waiver(), ..., range = NULL, aesthetics = "alpha")

scale_alpha_continuous(
  name = waiver(),
  ...,
  range = NULL,
  aesthetics = "alpha"
)

scale_alpha_binned(name = waiver(), ..., range = NULL, aesthetics = "alpha")

scale_alpha_discrete(...)

scale_alpha_ordinal(name = waiver(), ..., range = NULL, aesthetics = "alpha")
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/dev/reference/waiver.md),
  the default, the name of the scale is taken from the first mapping
  used for that aesthetic. If `NULL`, the legend title will be omitted.

- ...:

  Other arguments passed on to
  [`continuous_scale()`](https://ggplot2.tidyverse.org/dev/reference/continuous_scale.md),
  [`binned_scale()`](https://ggplot2.tidyverse.org/dev/reference/binned_scale.md),
  or
  [`discrete_scale()`](https://ggplot2.tidyverse.org/dev/reference/discrete_scale.md)
  as appropriate, to control name, limits, breaks, labels and so forth.

- range:

  Output range of alpha values. Must lie between 0 and 1.

- aesthetics:

  The names of the aesthetics that this scale works with.

## See also

The documentation on [colour
aesthetics](https://ggplot2.tidyverse.org/dev/reference/aes_colour_fill_alpha.md).

Other alpha scales:
[`scale_alpha_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md),
[`scale_alpha_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md).

The [alpha scales
section](https://ggplot2-book.org/scales-colour#sec-scales-alpha) of the
online ggplot2 book.

Other colour scales:
[`scale_colour_brewer()`](https://ggplot2.tidyverse.org/dev/reference/scale_brewer.md),
[`scale_colour_continuous()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_continuous.md),
[`scale_colour_discrete()`](https://ggplot2.tidyverse.org/dev/reference/scale_colour_discrete.md),
[`scale_colour_gradient()`](https://ggplot2.tidyverse.org/dev/reference/scale_gradient.md),
[`scale_colour_grey()`](https://ggplot2.tidyverse.org/dev/reference/scale_grey.md),
[`scale_colour_hue()`](https://ggplot2.tidyverse.org/dev/reference/scale_hue.md),
[`scale_colour_identity()`](https://ggplot2.tidyverse.org/dev/reference/scale_identity.md),
[`scale_colour_manual()`](https://ggplot2.tidyverse.org/dev/reference/scale_manual.md),
[`scale_colour_steps()`](https://ggplot2.tidyverse.org/dev/reference/scale_steps.md),
[`scale_colour_viridis_d()`](https://ggplot2.tidyverse.org/dev/reference/scale_viridis.md)

## Examples

``` r
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(alpha = year))

# The default range of 0.1-1.0 leaves all data visible
p


# Include 0 in the range to make data invisible
p + scale_alpha(range = c(0, 1))


# Changing the title
p + scale_alpha("cylinders")
```
