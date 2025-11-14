# Simultaneously dodge and jitter

This is primarily used for aligning points generated through
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.md)
with dodged boxplots (e.g., a
[`geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md)
with a fill aesthetic supplied).

## Usage

``` r
position_jitterdodge(
  jitter.width = NULL,
  jitter.height = 0,
  dodge.width = 0.75,
  reverse = FALSE,
  seed = NA
)
```

## Arguments

- jitter.width:

  degree of jitter in x direction. Defaults to 40% of the resolution of
  the data.

- jitter.height:

  degree of jitter in y direction. Defaults to 0.

- dodge.width:

  the amount to dodge in the x direction. Defaults to 0.75, the default
  [`position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.md)
  width.

- reverse:

  If `TRUE`, will reverse the default stacking order. This is useful if
  you're rotating both the plot and legend.

- seed:

  A random seed to make the jitter reproducible. Useful if you need to
  apply the same jitter twice, e.g., for a point and a corresponding
  label. The random seed is reset after jittering. If `NA` (the default
  value), the seed is initialised with a random value; this makes sure
  that two subsequent calls start with a different seed. Use `NULL` to
  use the current random seed and also avoid resetting (the behaviour of
  ggplot 2.2.1 and earlier).

## See also

Other position adjustments:
[`position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.md),
[`position_identity()`](https://ggplot2.tidyverse.org/reference/position_identity.md),
[`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.md),
[`position_nudge()`](https://ggplot2.tidyverse.org/reference/position_nudge.md),
[`position_stack()`](https://ggplot2.tidyverse.org/reference/position_stack.md)

## Examples

``` r
set.seed(596)
dsub <- diamonds[sample(nrow(diamonds), 1000), ]
ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(pch = 21, position = position_jitterdodge())
```
