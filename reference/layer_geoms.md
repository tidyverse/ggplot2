# Layer geometry display

In ggplot2, a plot in constructed by adding layers to it. A layer
consists of two important parts: the geometry (geoms), and statistical
transformations (stats). The 'geom' part of a layer is important because
it determines the looks of the data. Geoms determine *how* something is
displayed, not *what* is displayed.

## Specifying geoms

There are five ways in which the 'geom' part of a layer can be
specified.

    # 1. The geom can have a layer constructor
    geom_area()

    # 2. A stat can default to a particular geom
    stat_density() # has `geom = "area"` as default

    # 3. It can be given to a stat as a string
    stat_function(geom = "area")

    # 4. The ggproto object of a geom can be given
    stat_bin(geom = GeomArea)

    # 5. It can be given to `layer()` directly
    layer(
      geom = "area",
      stat = "smooth",
      position = "identity"
    )

Many of these ways are absolutely equivalent. Using
`stat_density(geom = "line")` is identical to using
`geom_line(stat = "density")`. Note that for
[`layer()`](https://ggplot2.tidyverse.org/reference/layer.md), you need
to provide the `"position"` argument as well. To give geoms as a string,
take the function name, and remove the `geom_` prefix, such that
`geom_point` becomes `"point"`.

Some of the more well known geoms that can be used for the `geom`
argument are:
[`"point"`](https://ggplot2.tidyverse.org/reference/geom_point.md),
[`"line"`](https://ggplot2.tidyverse.org/reference/geom_path.md),
[`"area"`](https://ggplot2.tidyverse.org/reference/geom_ribbon.md),
[`"bar"`](https://ggplot2.tidyverse.org/reference/geom_bar.md) and
[`"polygon"`](https://ggplot2.tidyverse.org/reference/geom_polygon.md).

## Graphical display

A ggplot is build on top of the grid package. This package understands
various graphical primitives, such as points, lines, rectangles and
polygons and their
[positions](https://ggplot2.tidyverse.org/reference/aes_position.md), as
well as graphical attributes, also termed aesthetics, such as [colours,
fills](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.md),
[linewidths and
linetypes](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.md).
The job of the geom part of a layer, is to translate data to grid
graphics that can be plotted.

To see how aesthetics are specified, run
[`vignette("ggplot2-specs")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.md).
To see what geom uses what aesthetics, you can find the **Aesthetics**
section in their documentation, for example in
[`?geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.md).

While almost anything can be represented by polygons if you try hard
enough, it is not always convenient to do so manually. For this reason,
the geoms provide abstractions that take most of this hassle away.
[`geom_ribbon()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.md)
for example is a special case of
[`geom_polygon()`](https://ggplot2.tidyverse.org/reference/geom_polygon.md),
where two sets of y-positions have a shared x-position. In turn,
[`geom_area()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.md)
is a special case of a ribbon, where one of the two sets of y-positions
is set at 0.

    # A hassle to build a polygon
    my_polygon <- data.frame(
      x = c(economics$date,    rev(economics$date)),
      y = c(economics$uempmed, rev(economics$psavert))
    )
    ggplot(my_polygon, aes(x, y)) +
      geom_polygon()

    # More succinctly
    ggplot(economics, aes(date)) +
      geom_ribbon(aes(ymin = uempmed, ymax = psavert))

In addition to abstraction, geoms sometimes also perform composition. A
boxplot is a particular arrangement of lines, rectangles and points that
people have agreed upon is a summary of some data, which is performed by
[`geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.md).

    Boxplot data
    value <- fivenum(rnorm(100))
    df <- data.frame(
      min = value[1], lower = value[2], middle = value[3],
      upper = value[4], max = value[5]
    )

    # Drawing a boxplot manually
    ggplot(df, aes(x = 1, xend = 1)) +
      geom_rect(
        aes(
          xmin = 0.55, xmax = 1.45,
          ymin = lower, ymax = upper
        ),
        colour = "black", fill = "white"
      ) +
      geom_segment(
        aes(
          x = 0.55, xend = 1.45,
          y = middle, yend = middle
        ),
        size = 1
      ) +
      geom_segment(aes(y = lower, yend = min)) +
      geom_segment(aes(y = upper, yend = max))

    # More succinctly
    ggplot(df, aes(x = 1)) +
      geom_boxplot(
        aes(ymin = min, ymax = max,
            lower = lower, upper = upper,
            middle = middle),
        stat = "identity"
      )

## Under the hood

Internally, geoms are represented as
[`ggproto`](https://ggplot2.tidyverse.org/reference/ggproto.md) classes
that occupy a slot in a layer. All these classes inherit from the
parental [`Geom`](https://ggplot2.tidyverse.org/reference/Geom.md)
ggproto object that orchestrates how geoms work. Briefly, geoms are
given the opportunity to draw the data of the layer as a whole, a facet
panel, or of individual groups. For more information on extending geoms,
see the **Creating a new geom** section after running
[`vignette("extending-ggplot2")`](https://ggplot2.tidyverse.org/articles/extending-ggplot2.md).
Additionally, see the **New geoms** section of the [online
book](https://ggplot2-book.org/extensions.html#new-geoms).

## See also

For an overview of all geom layers, see the [online
reference](https://ggplot2.tidyverse.org/reference/index.html#geoms).

Other layer documentation:
[`layer()`](https://ggplot2.tidyverse.org/reference/layer.md),
[`layer_positions`](https://ggplot2.tidyverse.org/reference/layer_positions.md),
[`layer_stats`](https://ggplot2.tidyverse.org/reference/layer_stats.md)
