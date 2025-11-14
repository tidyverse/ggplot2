# Layer statistical transformations

In ggplot2, a plot is constructed by adding layers to it. A layer
consists of two important parts: the geometry (geoms), and statistical
transformations (stats). The 'stat' part of a layer is important because
it performs a computation on the data before it is displayed. Stats
determine *what* is displayed, not *how* it is displayed.

For example, if you add
[`stat_density()`](https://ggplot2.tidyverse.org/reference/geom_density.md)
to a plot, a kernel density estimation is performed, which can be
displayed with the 'geom' part of a layer. For many `geom_*()`
functions,
[`stat_identity()`](https://ggplot2.tidyverse.org/reference/stat_identity.md)
is used, which performs no extra computation on the data.

## Specifying stats

There are five ways in which the 'stat' part of a layer can be
specified.

    # 1. The stat can have a layer constructor
    stat_density()

    # 2. A geom can default to a particular stat
    geom_density() # has `stat = "density"` as default

    # 3. It can be given to a geom as a string
    geom_line(stat = "density")

    # 4. The ggproto object of a stat can be given
    geom_area(stat = StatDensity)

    # 5. It can be given to `layer()` directly:
    layer(
      geom = "line",
      stat = "density",
      position = "identity"
    )

Many of these ways are absolutely equivalent. Using
`stat_density(geom = "line")` is identical to using
`geom_line(stat = "density")`. Note that for
[`layer()`](https://ggplot2.tidyverse.org/reference/layer.md), you need
to provide the `"position"` argument as well. To give stats as a string,
take the function name, and remove the `stat_` prefix, such that
`stat_bin` becomes `"bin"`.

Some of the more well known stats that can be used for the `stat`
argument are:
[`"density"`](https://ggplot2.tidyverse.org/reference/geom_density.md),
[`"bin"`](https://ggplot2.tidyverse.org/reference/geom_histogram.md),
[`"count"`](https://ggplot2.tidyverse.org/reference/geom_bar.md),
[`"function"`](https://ggplot2.tidyverse.org/reference/geom_function.md)
and
[`"smooth"`](https://ggplot2.tidyverse.org/reference/geom_smooth.md).

## Paired geoms and stats

Some geoms have paired stats. In some cases, like
[`geom_density()`](https://ggplot2.tidyverse.org/reference/geom_density.md),
it is just a variant of another geom,
[`geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.md),
with slightly different defaults.

In other cases, the relationship is more complex. In the case of
boxplots for example, the stat and the geom have distinct roles. The
role of the stat is to compute the five-number summary of the data. In
addition to just displaying the box of the five-number summary, the geom
also provides display options for the outliers and widths of boxplots.
In such cases, you cannot freely exchange geoms and stats: using
`stat_boxplot(geom = "line")` or `geom_area(stat = "boxplot")` give
errors.

Some stats and geoms that are paired are:

- [`geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.md)
  and
  [`stat_ydensity()`](https://ggplot2.tidyverse.org/reference/geom_violin.md)

- [`geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.md)
  and
  [`stat_bin()`](https://ggplot2.tidyverse.org/reference/geom_histogram.md)

- [`geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.md)
  and
  [`stat_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.md)

- [`geom_function()`](https://ggplot2.tidyverse.org/reference/geom_function.md)
  and
  [`stat_function()`](https://ggplot2.tidyverse.org/reference/geom_function.md)

- [`geom_bin_2d()`](https://ggplot2.tidyverse.org/reference/geom_bin_2d.md)
  and
  [`stat_bin_2d()`](https://ggplot2.tidyverse.org/reference/geom_bin_2d.md)

- [`geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md)
  and
  [`stat_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md)

- [`geom_count()`](https://ggplot2.tidyverse.org/reference/geom_count.md)
  and
  [`stat_sum()`](https://ggplot2.tidyverse.org/reference/geom_count.md)

- [`geom_density()`](https://ggplot2.tidyverse.org/reference/geom_density.md)
  and
  [`stat_density()`](https://ggplot2.tidyverse.org/reference/geom_density.md)

- [`geom_density_2d()`](https://ggplot2.tidyverse.org/reference/geom_density_2d.md)
  and
  [`stat_density_2d()`](https://ggplot2.tidyverse.org/reference/geom_density_2d.md)

- [`geom_hex()`](https://ggplot2.tidyverse.org/reference/geom_hex.md)
  and
  [`stat_binhex()`](https://ggplot2.tidyverse.org/reference/geom_hex.md)

- [`geom_quantile()`](https://ggplot2.tidyverse.org/reference/geom_quantile.md)
  and
  [`stat_quantile()`](https://ggplot2.tidyverse.org/reference/geom_quantile.md)

- [`geom_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.md)
  and
  [`stat_smooth()`](https://ggplot2.tidyverse.org/reference/geom_smooth.md)

## Using computed variables

As mentioned above, the role of stats is to perform computation on the
data. As a result, stats have 'computed variables' that determine
compatibility with geoms. These computed variables are documented in the
**Computed variables** sections of the documentation, for example in
[`?stat_bin`](https://ggplot2.tidyverse.org/reference/geom_histogram.md).
While more thoroughly documented in
[`after_stat()`](https://ggplot2.tidyverse.org/reference/aes_eval.md),
it should briefly be mentioned that these computed stats can be accessed
in [`aes()`](https://ggplot2.tidyverse.org/reference/aes.md).

For example, the
[`?stat_density`](https://ggplot2.tidyverse.org/reference/geom_density.md)
documentation states that, in addition to a variable called `density`,
the stat computes a variable named `count`. Instead of scaling such that
the area integrates to 1, the `count` variable scales the computed
density such that the values can be interpreted as counts. If
`stat_density(aes(y = after_stat(count)))` is used, we can display these
count-scaled densities instead of the regular densities.

The computed variables offer flexibility in that arbitrary geom-stat
pairings can be made. While not necessarily recommended,
[`geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.md)
*can* be paired with `stat = "boxplot"` if the line is instructed on how
to use the boxplot computed variables:

    ggplot(mpg, aes(factor(cyl))) +
      geom_line(
        # Stage gives 'displ' to the stat, and afterwards chooses 'middle' as
        # the y-variable to display
        aes(y = stage(displ, after_stat = middle),
            # Regroup after computing the stats to display a single line
            group = after_stat(1)),
        stat = "boxplot"
      )

## Under the hood

Internally, stats are represented as
[`ggproto`](https://ggplot2.tidyverse.org/reference/ggproto.md) classes
that occupy a slot in a layer. All these classes inherit from the
parental [`Stat`](https://ggplot2.tidyverse.org/reference/Stat.md)
ggproto object that orchestrates how stats work. Briefly, stats are
given the opportunity to perform computation either on the layer as a
whole, a facet panel, or on individual groups. For more information on
extending stats, see the **Creating a new stat** section after running
[`vignette("extending-ggplot2")`](https://ggplot2.tidyverse.org/articles/extending-ggplot2.md).
Additionally, see the **New stats** section of the [online
book](https://ggplot2-book.org/extensions.html#new-stats).

## See also

For an overview of all stat layers, see the [online
reference](https://ggplot2.tidyverse.org/reference/index.html#stats).

How [computed
aesthetics](https://ggplot2.tidyverse.org/reference/aes_eval.md) work.

Other layer documentation:
[`layer()`](https://ggplot2.tidyverse.org/reference/layer.md),
[`layer_geoms`](https://ggplot2.tidyverse.org/reference/layer_geoms.md),
[`layer_positions`](https://ggplot2.tidyverse.org/reference/layer_positions.md)
