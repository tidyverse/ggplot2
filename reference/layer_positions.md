# Layer position adjustments

In ggplot2, a plot is constructed by adding layers to it. In addition to
[geoms](https://ggplot2.tidyverse.org/reference/layer_geoms.md) and
[stats](https://ggplot2.tidyverse.org/reference/layer_stats.md),
position adjustments are the third required part of a layer. The
'position' part of a layer is responsible for dodging, jittering and
nudging groups of data to minimise their overlap, or otherwise tweaking
their positions.

For example if you add `position = position_nudge(x = 1)` to a layer,
you can offset every x-position by 1. For many layers, the default
position adjustment is
[`position_identity()`](https://ggplot2.tidyverse.org/reference/position_identity.md),
which performs no adjustment.

## Specifying positions

There are 4 ways in which the 'position' part of a layer can be
specified.

    1. A layer can have default position adjustments
    geom_jitter() # has `position = "jitter"`

    2. It can be given to a layer as a string
    geom_point(position = "jitter")

    3. The position function can be used to pass extra arguments
    geom_point(position = position_jitter(width = 1))

    4. It can be given to `layer()` directly
    layer(
      geom = "point",
      stat = "identity",
      position = "jitter"
    )

These ways are not always equivalent. Some layers may not understand
what to do with a position adjustment, and require additional parameters
passed through the `position_*()` function, or may not work correctly.
For example
[`position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.md)
requires non-overlapping x intervals, whereas
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.md)
doesn't have dimensions to calculate intervals for. To give positions as
a string, take the function name, and remove the `position_` prefix,
such that `position_fill` becomes `"fill"`.

## Pairing geoms with positions

Some geoms work better with some positions than others. Below follows a
brief overview of geoms and position adjustments that work well
together.

### Identity

[`position_identity()`](https://ggplot2.tidyverse.org/reference/position_identity.md)
can work with virtually any geom.

### Dodging

[`position_dodge()`](https://ggplot2.tidyverse.org/reference/position_dodge.md)
pushes overlapping objects away from one another and requires a `group`
variable.
[`position_dodge2()`](https://ggplot2.tidyverse.org/reference/position_dodge.md)
can work without group variables and can handle variable widths. As a
rule of thumb, layers where groups occupy a range on the x-axis pair
well with dodging. If layers have no width, you may be required to
specify it manually with `position_dodge(width = ...)`. Some geoms that
pair well with dodging are
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.md),
[`geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md),
[`geom_linerange()`](https://ggplot2.tidyverse.org/reference/geom_linerange.md),
[`geom_errorbar()`](https://ggplot2.tidyverse.org/reference/geom_linerange.md)
and
[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.md).

### Jittering

[`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.md)
adds a some random noise to every point, which can help with
overplotting.
[`position_jitterdodge()`](https://ggplot2.tidyverse.org/reference/position_jitterdodge.md)
does the same, but also dodges the points. As a rule of thumb, jittering
works best when points have discrete x-positions. Jittering is most
useful for
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.md),
but can also be used in
[`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.md)
for example.

### Nudging

[`position_nudge()`](https://ggplot2.tidyverse.org/reference/position_nudge.md)
can add offsets to x- and y-positions. This can be useful for discrete
positions where you don't want to put an object exactly in the middle.
While most useful for
[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.md),
it can be used with virtually all geoms.

### Stacking

[`position_stack()`](https://ggplot2.tidyverse.org/reference/position_stack.md)
is useful for displaying data on top of one another. It can be used for
geoms that are usually anchored to the x-axis, for example
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.md),
[`geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.md)
or
[`geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.md).

### Filling

[`position_fill()`](https://ggplot2.tidyverse.org/reference/position_stack.md)
can be used to give proportions at every x-position. Like stacking,
filling is most useful for geoms that are anchored to the x-axis, like
[`geom_bar()`](https://ggplot2.tidyverse.org/reference/geom_bar.md),
[`geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.md)
or
[`geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.md).

## Under the hood

Internally, positions are represented as
[`ggproto`](https://ggplot2.tidyverse.org/reference/ggproto.md) classes
that occupy a slot in a layer. All these classes inherit from the
parental
[`Position`](https://ggplot2.tidyverse.org/reference/Position.md)
ggproto object that orchestrates how positions work. Briefly, positions
are given the opportunity to adjust the data of each facet panel. For
more information about extending positions, see the **New positions**
section of the [online
book](https://ggplot2-book.org/extensions.html#new-positions).

## See also

For an overview of all position adjustments, see the [online
reference](https://ggplot2.tidyverse.org/reference/index.html#position-adjustment).

Other layer documentation:
[`layer()`](https://ggplot2.tidyverse.org/reference/layer.md),
[`layer_geoms`](https://ggplot2.tidyverse.org/reference/layer_geoms.md),
[`layer_stats`](https://ggplot2.tidyverse.org/reference/layer_stats.md)
