# Shared parameters -------------------------------------------------------

#' @name shared_layer_parameters
#' @title Shared layer parameters
#' @description
#' This is a central place for describing typical layer parameters.
#' It prevents cluttered definitions all over the place.
#'
#' @param mapping
#' Set of aesthetic mappings created by [aes()]. If specified and `inherit.aes =
#' TRUE` (the default), it is combined with the default mapping at the top level
#' of the plot. You must supply `mapping` if there is no plot mapping.
#'
#' @param data
#' The data to be displayed in this layer. There are three options:
#' * `NULL` (default): the data is inherited from the plot data as specified
#'   in the call to [ggplot()].
#' * A `data.frame`, or other object, will override the plot data. All objects
#'   will be fortified to produce a data frame. See [fortify()] for which
#'   variables will be created.
#' * A `function` will be called with a single argument, the plot data. The return
#'   value must be a `data.frame`, and will be used as the layer data. A
#'   `function` can be created from a `formula` (e.g. `~ head(.x, 10)`).
#'
#' @param geom
#' The geometric object to use to display the data for this layer. When using a
#' `stat_*()` function to construct a layer, the `geom` argument can be used to
#' override the default coupling between stats and geoms. The `geom` argument
#' accepts the following:
#' * A `Geom` ggproto subclass, for example `GeomPoint`.
#' * A string naming the geom. To give the geom as a string, strip the
#'   function name of the `geom_` prefix. For example, to use `geom_point()`,
#'   give the geom as `"point"`.
#' * For more information and other ways to specify the geom, see the
#'   [layer geom][layer_geoms] documentation.
#'
#' @param stat
#' The statistical transformation to use on the data for this layer. When using
#' a `geom_*()` function to construct a layer, the `stat` argument can be used
#' to override the default coupling between geoms and stats. The `stat` argument
#' accepts the following:
#' * A `Stat` ggproto subclass, for example `StatCount`.
#' * A string naming the stat. To give the stat as a string, strip the
#'   function name of the `stat_` prefix. For example, to use `stat_count()`,
#'   give the stat as `"count"`.
#' * For more information and other ways to specify the stat, see the
#'   [layer stat][layer_stats] documentation.
#'
#' @param position
#' A position adjustment to use on the data for this layer. This can be used in
#' various ways, including to prevent overplotting and improving the display.
#' The `position` argument accepts the following:
#' * The result of calling a position function, such as `position_jitter()`.
#'   This method allows for passing extra arguments to the position.
#' * A string naming the position adjustment. To give the position as a
#'   string, strip the function name of the `position_` prefix. For example, to
#'   use `position_jitter()`, give the position as `"jitter"`.
#' * For more information and other ways to specify the position, see the
#'   [layer position][layer_positions] documentation.
#'
#' @param na.rm
#' If `FALSE`, the default, missing values are removed with a warning. If
#' `TRUE`, missing values are silently removed.
#'
#' @param show.legend
#' Logical. Should this layer be included in the legends? `NA`, the default,
#' includes if any aesthetics are mapped. `FALSE` never includes, and `TRUE`
#' always includes. It can also be a named logical vector to finely select the
#' aesthetics to display. To include legend keys for all levels, even when no
#' data exists, use `TRUE`.  If `NA`, all levels are shown in legend, but
#' unobserved levels are omitted.
#'
#' @param inherit.aes
#' If `FALSE`, overrides the default aesthetics, rather than combining with
#' them. This is most useful for helper functions that define both data and
#' aesthetics and shouldn't inherit behaviour from the default plot
#' specification, e.g. [annotation_borders()].
#'
#' @param ...
#' Other arguments passed on to [layer()]'s `params` argument. These arguments
#' broadly fall into one of 4 categories below. Notably, further arguments to
#' the `position` argument, or aesthetics that are required can
#'   *not* be passed through `...`. Unknown arguments that are not part of the 4
#' categories below are ignored.
#' * Static aesthetics that are not mapped to a scale, but are at a fixed
#'   value and apply to the layer as a whole. For example, `colour = "red"` or
#'   `linewidth = 3`. The geom's documentation has an **Aesthetics** section
#'   that lists the available options. The 'required' aesthetics cannot be
#'   passed on to the `params`. Please note that while passing unmapped
#'   aesthetics as vectors is technically possible, the order and required
#'   length is not guaranteed to be parallel to the input data.
#' * When constructing a layer using
#'   a `stat_*()` function, the `...` argument can be used to pass on parameters
#'   to the `geom` part of the layer. An example of this is
#'   `stat_density(geom = "area", outline.type = "both")`. The geom's
#'   documentation lists which parameters it can accept.
#' * Inversely, when constructing a layer using a
#'   `geom_*()` function, the `...` argument can be used to pass on parameters
#'   to the `stat` part of the layer. An example of this is
#'   `geom_area(stat = "density", adjust = 0.5)`. The stat's documentation lists
#'   which parameters it can accept.
#' * The `key_glyph` argument of [`layer()`] may also be passed on through
#'   `...`. This can be one of the functions described as
#'   [key glyphs][draw_key], to change the display of the layer in the legend.
#'
#' @param lineend
#' Line end style, one of `"round"`, `"butt"` or `"square"`.
#'
#' @param linejoin
#' Line join style, one of `"round"`, `"mitre"` or `"bevel"`.
#'
#' @param linemitre
#' Line mitre limit, a number greater  than 1.
#'
#' @param arrow
#' Arrow specification. Can be created by [grid::arrow()] or `NULL` to not draw
#' an arrow.
#'
#' @param arrow.fill
#' Fill colour to use for closed arrowheads. `NULL` means use `colour`
#' aesthetic.
#'
#' @param orientation
#' The orientation of the layer. The default (`NA`) automatically determines the
#' orientation from the aesthetic mapping. In the rare event that this fails it
#' can be given explicitly by setting `orientation` to either `"x"` or `"y"`.
#' See the *Orientation* section for more detail.
#'
#' @section Orientation:
#' This geom treats each axis differently and, thus, can have two
#' orientations. Often the orientation is easy to deduce from a combination of
#' the given mappings and the types of positional scales in use. Thus, ggplot2
#' will by default try to guess which orientation the layer should have. Under
#' rare circumstances, the orientation is ambiguous and guessing may fail. In
#' that case the orientation can be specified directly using the `orientation`
#' parameter, which can be either `"x"` or `"y"`. The value gives the axis that
#' the geom should run along, `"x"` being the default orientation you would
#' expect for the geom.
#'
#' @keywords internal
#' @aliases NULL
NULL

# Geoms -------------------------------------------------------------------

#' @title
#' Layer geometry display
#'
#' @description
#' In ggplot2, a plot in constructed by adding layers to it. A layer consists
#' of two important parts: the geometry (geoms), and statistical transformations
#' (stats). The 'geom' part of a layer is important because it determines the
#' looks of the data. Geoms determine *how* something is displayed, not *what*
#' is displayed.
#'
#' @details
#' # Specifying geoms
#' There are five ways in which the 'geom' part of a layer can be specified.
#'
#' ```r
#' # 1. The geom can have a layer constructor
#' geom_area()
#'
#' # 2. A stat can default to a particular geom
#' stat_density() # has `geom = "area"` as default
#'
#' # 3. It can be given to a stat as a string
#' stat_function(geom = "area")
#'
#' # 4. The ggproto object of a geom can be given
#' stat_bin(geom = GeomArea)
#'
#' # 5. It can be given to `layer()` directly
#' layer(
#'   geom = "area",
#'   stat = "smooth",
#'   position = "identity"
#' )
#' ```
#'
#' Many of these ways are absolutely equivalent. Using
#' `stat_density(geom = "line")` is identical to using
#' `geom_line(stat = "density")`. Note that for [`layer()`], you need to
#' provide the `"position"` argument as well. To give geoms as a string, take
#' the function name, and remove the `geom_` prefix, such that `geom_point`
#' becomes `"point"`.
#'
#' Some of the more well known geoms that can be used for the `geom` argument
#' are: [`"point"`][geom_point()], [`"line"`][geom_line()],
#' [`"area"`][geom_area()], [`"bar"`][geom_bar()] and
#' [`"polygon"`][geom_polygon].
#'
#' # Graphical display
#' A ggplot is build on top of the [grid][grid-package] package. This package
#' understands various graphical primitives, such as points, lines, rectangles
#' and polygons and their [positions][aes_position], as well as graphical
#' attributes, also termed aesthetics, such as
#' [colours, fills][aes_colour_fill_alpha],
#' [linewidths and linetypes][aes_linetype_size_shape]. The job of the geom part
#' of a layer, is to translate data to grid graphics that can be plotted.
#'
#' To see how aesthetics are specified, run `vignette("ggplot2-specs")`. To see
#' what geom uses what aesthetics, you can find the **Aesthetics** section in
#' their documentation, for example in [`?geom_line`][geom_line()].
#'
#' While almost anything can be represented by polygons if you try hard enough,
#' it is not always convenient to do so manually. For this reason, the geoms
#' provide abstractions that take most of this hassle away. [`geom_ribbon()`]
#' for example is a special case of [`geom_polygon()`], where two sets of
#' y-positions have a shared x-position. In turn, [`geom_area()`] is a special
#' case of a ribbon, where one of the two sets of y-positions is set at 0.
#'
#' ```r
#' # A hassle to build a polygon
#' my_polygon <- data.frame(
#'   x = c(economics$date,    rev(economics$date)),
#'   y = c(economics$uempmed, rev(economics$psavert))
#' )
#' ggplot(my_polygon, aes(x, y)) +
#'   geom_polygon()
#'
#' # More succinctly
#' ggplot(economics, aes(date)) +
#'   geom_ribbon(aes(ymin = uempmed, ymax = psavert))
#' ```
#'
#' In addition to abstraction, geoms sometimes also perform composition.
#' A boxplot is a particular arrangement of lines, rectangles and points that
#' people have agreed upon is a summary of some data, which is performed by
#' [`geom_boxplot()`].
#'
#' ```r
#' Boxplot data
#' value <- fivenum(rnorm(100))
#' df <- data.frame(
#'   min = value[1], lower = value[2], middle = value[3],
#'   upper = value[4], max = value[5]
#' )
#'
#' # Drawing a boxplot manually
#' ggplot(df, aes(x = 1, xend = 1)) +
#'   geom_rect(
#'     aes(
#'       xmin = 0.55, xmax = 1.45,
#'       ymin = lower, ymax = upper
#'     ),
#'     colour = "black", fill = "white"
#'   ) +
#'   geom_segment(
#'     aes(
#'       x = 0.55, xend = 1.45,
#'       y = middle, yend = middle
#'     ),
#'     size = 1
#'   ) +
#'   geom_segment(aes(y = lower, yend = min)) +
#'   geom_segment(aes(y = upper, yend = max))
#'
#' # More succinctly
#' ggplot(df, aes(x = 1)) +
#'   geom_boxplot(
#'     aes(ymin = min, ymax = max,
#'         lower = lower, upper = upper,
#'         middle = middle),
#'     stat = "identity"
#'   )
#' ```
#'
#' # Under the hood
#' Internally, geoms are represented as [`ggproto`][ggproto()] classes that
#' occupy a slot in a layer. All these classes inherit from the parental
#' [`Geom`] ggproto object that orchestrates how geoms work. Briefly, geoms
#' are given the opportunity to draw the data of the layer as a whole,
#' a facet panel, or of individual groups. For more information on extending
#' geoms, see the **Creating a new geom** section after running
#' `vignette("extending-ggplot2")`. Additionally, see the **New geoms** section
#' of the [online book](https://ggplot2-book.org/extensions.html#new-geoms).
#'
#' @seealso
#' For an overview of all geom layers, see the
#' [online reference](https://ggplot2.tidyverse.org/reference/index.html#geoms).
#'
#' @family layer documentation
#'
#' @name layer_geoms
NULL

# Stats -------------------------------------------------------------------

#' @title
#' Layer statistical transformations
#'
#' @description
#' In ggplot2, a plot is constructed by adding layers to it. A layer consists
#' of two important parts: the geometry (geoms), and statistical transformations
#' (stats). The 'stat' part of a layer is important because it performs a
#' computation on the data before it is displayed. Stats determine *what* is
#' displayed, not *how* it is displayed.
#'
#' For example, if you add [`stat_density()`] to a plot, a kernel density
#' estimation is performed, which can be displayed with the 'geom' part of a
#' layer. For many `geom_*()` functions, [`stat_identity()`] is used,
#' which performs no extra computation on the data.
#'
#' @details
#' # Specifying stats
#' There are five ways in which the 'stat' part of a layer can be specified.
#'
#' ```r
#' # 1. The stat can have a layer constructor
#' stat_density()
#'
#' # 2. A geom can default to a particular stat
#' geom_density() # has `stat = "density"` as default
#'
#' # 3. It can be given to a geom as a string
#' geom_line(stat = "density")
#'
#' # 4. The ggproto object of a stat can be given
#' geom_area(stat = StatDensity)
#'
#' # 5. It can be given to `layer()` directly:
#' layer(
#'   geom = "line",
#'   stat = "density",
#'   position = "identity"
#' )
#' ```
#'
#' Many of these ways are absolutely equivalent. Using
#' `stat_density(geom = "line")` is identical to using
#' `geom_line(stat = "density")`. Note that for [`layer()`], you need to
#' provide the `"position"` argument as well. To give stats as a string, take
#' the function name, and remove the `stat_` prefix, such that `stat_bin`
#' becomes `"bin"`.
#'
#' Some of the more well known stats that can be used for the `stat` argument
#' are: [`"density"`][stat_density()], [`"bin"`][stat_bin()],
#' [`"count"`][stat_count()], [`"function"`][stat_function()] and
#' [`"smooth"`][stat_smooth()].
#'
#' # Paired geoms and stats
#' Some geoms have paired stats. In some cases, like [`geom_density()`], it is
#' just a variant of another geom, [`geom_area()`], with slightly different
#' defaults.
#'
#' In other cases, the relationship is more complex. In the case of boxplots for
#' example, the stat and the geom have distinct roles. The role of the stat is
#' to compute the five-number summary of the data. In addition to just
#' displaying the box of the five-number summary, the geom also provides display
#' options for the outliers and widths of boxplots. In such cases, you cannot
#' freely exchange geoms and stats: using `stat_boxplot(geom = "line")` or
#' `geom_area(stat = "boxplot")` give errors.
#'
#' Some stats and geoms that are paired are:
#' * [`geom_violin()`] and [`stat_ydensity()`]
#' * [`geom_histogram()`] and [`stat_bin()`]
#' * [`geom_contour()`] and [`stat_contour()`]
#' * [`geom_function()`] and [`stat_function()`]
#' * [`geom_bin_2d()`] and [`stat_bin_2d()`]
#' * [`geom_boxplot()`] and [`stat_boxplot()`]
#' * [`geom_count()`] and [`stat_sum()`]
#' * [`geom_density()`] and [`stat_density()`]
#' * [`geom_density_2d()`] and [`stat_density_2d()`]
#' * [`geom_hex()`] and [`stat_binhex()`]
#' * [`geom_quantile()`] and [`stat_quantile()`]
#' * [`geom_smooth()`] and [`stat_smooth()`]
#'
#' # Using computed variables
#' As mentioned above, the role of stats is to perform computation on the data.
#' As a result, stats have 'computed variables' that determine compatibility
#' with geoms. These computed variables are documented in the
#' **Computed variables** sections of the documentation, for example in
#' [`?stat_bin`][stat_bin()]. While more thoroughly documented
#' in [`after_stat()`], it should briefly be mentioned that these computed stats
#' can be accessed in [`aes()`].
#'
#' For example, the [`?stat_density`][stat_density()] documentation states that,
#' in addition to a variable called `density`, the stat computes a variable
#' named `count`. Instead of scaling such that the area integrates to 1, the
#' `count` variable scales the computed density such that the values
#' can be interpreted as counts. If `stat_density(aes(y = after_stat(count)))`
#' is used, we can display these count-scaled densities instead of the regular
#' densities.
#'
#' The computed variables offer flexibility in that arbitrary geom-stat pairings
#' can be made. While not necessarily recommended, [`geom_line()`] *can* be paired
#' with `stat = "boxplot"` if the line is instructed on how to use the boxplot
#' computed variables:
#'
#' ```r
#' ggplot(mpg, aes(factor(cyl))) +
#'   geom_line(
#'     # Stage gives 'displ' to the stat, and afterwards chooses 'middle' as
#'     # the y-variable to display
#'     aes(y = stage(displ, after_stat = middle),
#'         # Regroup after computing the stats to display a single line
#'         group = after_stat(1)),
#'     stat = "boxplot"
#'   )
#' ```
#'
#' # Under the hood
#' Internally, stats are represented as [`ggproto`][ggproto()] classes that
#' occupy a slot in a layer. All these classes inherit from the parental
#' [`Stat`] ggproto object that orchestrates how stats work. Briefly, stats
#' are given the opportunity to perform computation either on the layer as a
#' whole, a facet panel, or on individual groups. For more information on
#' extending stats, see the **Creating a new stat** section after
#' running `vignette("extending-ggplot2")`. Additionally, see the **New stats**
#' section of the
#' [online book](https://ggplot2-book.org/extensions.html#new-stats).
#'
#' @seealso
#' For an overview of all stat layers, see the
#' [online reference](https://ggplot2.tidyverse.org/reference/index.html#stats).
#'
#' How [computed aesthetics][after_stat()] work.
#' @family layer documentation
#'
#' @name layer_stats
NULL

# Position ----------------------------------------------------------------

#' @title
#' Layer position adjustments
#'
#' @description
#' In ggplot2, a plot is constructed by adding layers to it. In addition to
#' [geoms][layer_geoms] and [stats][layer_stats], position adjustments are the
#' third required part of a layer. The 'position' part of a layer is responsible
#' for dodging, jittering and nudging groups of data to minimise their overlap,
#' or otherwise tweaking their positions.
#'
#' For example if you add `position = position_nudge(x = 1)` to a layer, you
#' can offset every x-position by 1. For many layers, the default position
#' adjustment is [`position_identity()`], which performs no adjustment.
#'
#' @details
#' # Specifying positions
#' There are 4 ways in which the 'position' part of a layer can be specified.
#'
#' ```r
#' 1. A layer can have default position adjustments
#' geom_jitter() # has `position = "jitter"`
#'
#' 2. It can be given to a layer as a string
#' geom_point(position = "jitter")
#'
#' 3. The position function can be used to pass extra arguments
#' geom_point(position = position_jitter(width = 1))
#'
#' 4. It can be given to `layer()` directly
#' layer(
#'   geom = "point",
#'   stat = "identity",
#'   position = "jitter"
#' )
#' ```
#'
#' These ways are not always equivalent. Some layers may not understand what
#' to do with a position adjustment, and require additional parameters passed
#' through the `position_*()` function, or may not work correctly. For
#' example [`position_dodge()`] requires non-overlapping x intervals, whereas
#' [`geom_point()`] doesn't have dimensions to calculate intervals for. To give
#' positions as a string, take the function name, and remove the `position_`
#' prefix, such that `position_fill` becomes `"fill"`.
#'
#' # Pairing geoms with positions
#' Some geoms work better with some positions than others. Below follows a brief
#' overview of geoms and position adjustments that work well together.
#'
#' ## Identity
#' [`position_identity()`] can work with virtually any geom.
#'
#' ## Dodging
#' [`position_dodge()`] pushes overlapping objects away from one another and
#' requires a `group` variable. [`position_dodge2()`] can work without group
#' variables and can handle variable widths. As a rule of thumb, layers where
#' groups occupy a range on the x-axis pair well with dodging. If layers have
#' no width, you may be required to specify it manually with
#' `position_dodge(width = ...)`. Some geoms that pair well with dodging are
#' [`geom_bar()`], [`geom_boxplot()`], [`geom_linerange()`],
#' [`geom_errorbar()`] and [`geom_text()`].
#'
#' ## Jittering
#' [`position_jitter()`] adds a some random noise to every point,
#' which can help with overplotting. [`position_jitterdodge()`] does the same,
#' but also dodges the points. As a rule of thumb, jittering works best
#' when points have discrete x-positions. Jittering is most useful for
#' [`geom_point()`], but can also be used in [`geom_path()`] for example.
#'
#' ## Nudging
#' [`position_nudge()`] can add offsets to x- and y-positions. This can be
#' useful for discrete positions where you don't want to put an object
#' exactly in the middle. While most useful for [`geom_text()`], it can be
#' used with virtually all geoms.
#'
#' ## Stacking
#' [`position_stack()`] is useful for displaying data on top of one another. It
#' can be used for geoms that are usually anchored to the x-axis, for example
#' [`geom_bar()`], [`geom_area()`] or [`geom_histogram()`].
#'
#' ## Filling
#' [`position_fill()`] can be used to give proportions at every x-position. Like
#' stacking, filling is most useful for geoms that are anchored to the x-axis,
#' like [`geom_bar()`], [`geom_area()`] or [`geom_histogram()`].
#'
#' # Under the hood
#' Internally, positions are represented as [`ggproto`][ggproto()] classes that
#' occupy a slot in a layer. All these classes inherit from the parental
#' [`Position`] ggproto object that orchestrates how positions work. Briefly,
#' positions are given the opportunity to adjust the data of each facet panel.
#' For more information about extending positions, see the **New positions**
#' section of the
#' [online book](https://ggplot2-book.org/extensions.html#new-positions).
#'
#' @seealso
#' For an overview of all position adjustments, see the
#' [online reference](https://ggplot2.tidyverse.org/reference/index.html#position-adjustment).
#' @family layer documentation
#'
#' @name layer_positions
NULL
