#' Position related aesthetics: x, y, xmin, xmax, ymin, ymax, xend, yend
#'
#' The following aesthetics can be used to specify the position of elements:
#' `x`, `y`, `xmin`, `xmax`, `ymin`, `ymax`, `xend`, `yend`.
#'
#' `x` and `y` define the locations of points or of positions along a line
#' or path.
#'
#' `x`, `y` and `xend`, `yend` define the starting and ending points of
#' segment and curve geometries.
#'
#' `xmin`, `xmax`, `ymin`  and `ymax` can be used to specify the position of
#' annotations and to represent rectangular areas.
#'
#' In addition, there are position aesthetics that are contextual to the
#' geometry that they're used in. These are `xintercept`, `yintercept`,
#' `xmin_final`, `ymin_final`, `xmax_final`, `ymax_final`, `xlower`, `lower`,
#' `xmiddle`, `middle`, `xupper`, `upper`, `x0` and `y0`. Many of these are used
#' and automatically computed in [`geom_boxplot()`].
#'
#' ## Relation to `width` and `height`
#'
#' The position aesthetics mentioned above like `x` and `y` are all location
#' based. The `width` and `height` aesthetics are closely related length
#' based aesthetics, but are not position aesthetics. Consequently, `x` and `y`
#' aesthetics respond to scale transformations, whereas the length based
#' `width` and `height` aesthetics are not transformed by scales. For example,
#' if we have the pair `x = 10, width = 2`, that gets translated to the
#' locations `xmin = 9, xmax = 11` when using the default identity scales.
#' However, the same pair becomes `xmin = 1, xmax = 100` when using log10 scales,
#' as `width = 2` in log10-space spans a 100-fold change.
#'
#' @name aes_position
#' @aliases x y xmin xmax ymin ymax xend yend
#'
#' @seealso
#' * Geoms that commonly use these aesthetics: [geom_crossbar()],
#' [geom_curve()], [geom_errorbar()], [geom_line()], [geom_linerange()],
#' [geom_path()], [geom_point()], [geom_pointrange()], [geom_rect()],
#' [geom_segment()]
#' * Scales that can be used to modify positions:
#'   [`scale_continuous()`][scale_x_continuous()],
#'   [`scale_discrete()`][scale_x_discrete()],
#'   [`scale_binned()`][scale_x_binned()],
#'   [`scale_date()`][scale_x_date()].
#' * See also [annotate()] for placing annotations.
#' @family aesthetics documentation
#' @examples
#'
#' # Generate data: means and standard errors of means for prices
#' # for each type of cut
#' dmod <- lm(price ~ cut, data = diamonds)
#' cut <- unique(diamonds$cut)
#' cuts_df <- data.frame(
#'   cut,
#'   predict(dmod, data.frame(cut), se = TRUE)[c("fit", "se.fit")]
#' )
#' ggplot(cuts_df) +
#'   aes(
#'    x = cut,
#'    y = fit,
#'    ymin = fit - se.fit,
#'    ymax = fit + se.fit,
#'    colour = cut
#'   ) +
#'   geom_pointrange()
#'
#' # Using annotate
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' p
#' p + annotate(
#'   "rect", xmin = 2, xmax = 3.5, ymin = 2, ymax = 25,
#'   fill = "dark grey", alpha = .5
#' )
#'
#' # Geom_segment examples
#' p + geom_segment(
#'   aes(x = 2, y = 15, xend = 2, yend = 25),
#'   arrow = arrow(length = unit(0.5, "cm"))
#' )
#' p + geom_segment(
#'   aes(x = 2, y = 15, xend = 3, yend = 15),
#'   arrow = arrow(length = unit(0.5, "cm"))
#' )
#' p + geom_segment(
#'   aes(x = 5, y = 30, xend = 3.5, yend = 25),
#'   arrow = arrow(length = unit(0.5, "cm"))
#' )
#'
#' # You can also use geom_segment() to recreate plot(type = "h")
#' # from base R:
#' set.seed(1)
#' counts <- as.data.frame(table(x = rpois(100, 5)))
#' counts$x <- as.numeric(as.character(counts$x))
#' with(counts, plot(x, Freq, type = "h", lwd = 10))
#'
#' ggplot(counts, aes(x = x, y = Freq)) +
#'   geom_segment(aes(yend = 0, xend = x), size = 10)
NULL

#' Aesthetics: grouping
#'
#' @name aes_group_order
#' @aliases group
#'
#' @description
#' The `group` aesthetic is by default set to the interaction of all discrete variables
#' in the plot. This choice often partitions the data correctly, but when it does not,
#' or when no discrete variable is used in the plot, you will need to explicitly define the
#' grouping structure by mapping `group` to a variable that has a different value
#' for each group.
#'
#' @details
#' For most applications the grouping is set implicitly by mapping one or more
#' discrete variables to `x`, `y`, `colour`, `fill`, `alpha`, `shape`, `size`,
#' and/or `linetype`. This is demonstrated in the examples below.
#'
#' There are three common cases where the default does not display the data correctly.
#' 1. `geom_line()` where there are multiple individuals and the plot tries to
#'   connect every observation, even across individuals, with a line.
#' 1. `geom_line()` where a discrete x-position implies groups, whereas observations
#'   span the discrete x-positions.
#' 1. When the grouping needs to be different over different layers, for example
#'   when computing a statistic on all observations when another layer shows
#'   individuals.
#'
#' The examples below use a longitudinal dataset, `Oxboys`, from the nlme package to demonstrate
#' these cases. `Oxboys` records the heights (height) and centered ages (age) of 26 boys (Subject),
#' measured on nine occasions (Occasion).
#'
#' @seealso
#' * Geoms commonly used with groups: [geom_bar()], [geom_histogram()], [geom_line()]
#' * Run `vignette("ggplot2-specs")` to see an overview of other aesthetics that
#' can be modified.
#' @family aesthetics documentation
#'
#' @examples
#' \donttest{
#'
#' p <- ggplot(mtcars, aes(wt, mpg))
#' # A basic scatter plot
#' p + geom_point(size = 4)
#' # Using the colour aesthetic
#' p + geom_point(aes(colour = factor(cyl)), size = 4)
#' # Using the shape aesthetic
#' p + geom_point(aes(shape = factor(cyl)), size = 4)
#'
#' # Using fill
#' p <- ggplot(mtcars, aes(factor(cyl)))
#' p + geom_bar()
#' p + geom_bar(aes(fill = factor(cyl)))
#' p + geom_bar(aes(fill = factor(vs)))
#'
#' # Using linetypes
#' ggplot(economics_long, aes(date, value01)) +
#'   geom_line(aes(linetype = variable))
#'
#' # Multiple groups with one aesthetic
#' p <- ggplot(nlme::Oxboys, aes(age, height))
#' # The default is not sufficient here. A single line tries to connect all
#' # the observations.
#' p + geom_line()
#' # To fix this, use the group aesthetic to map a different line for each
#' # subject.
#' p + geom_line(aes(group = Subject))
#'
#' # Different groups on different layers
#' p <- p + geom_line(aes(group = Subject))
#' # Using the group aesthetic with both geom_line() and geom_smooth()
#' # groups the data the same way for both layers
#' p + geom_smooth(aes(group = Subject), method = "lm", se = FALSE)
#' # Changing the group aesthetic for the smoother layer
#' # fits a single line of best fit across all boys
#' p + geom_smooth(aes(group = 1), size = 2, method = "lm", se = FALSE)
#'
#' # Overriding the default grouping
#' # Sometimes the plot has a discrete scale but you want to draw lines
#' # that connect across groups. This is the strategy used in interaction
#' # plots, profile plots, and parallel coordinate plots, among others.
#' # For example, we draw boxplots of height at each measurement occasion.
#' p <- ggplot(nlme::Oxboys, aes(Occasion, height)) + geom_boxplot()
#' p
#' # There is no need to specify the group aesthetic here; the default grouping
#' # works because occasion is a discrete variable. To overlay individual
#' # trajectories, we again need to override the default grouping for that layer
#' # with aes(group = Subject)
#' p + geom_line(aes(group = Subject), colour = "blue")
#' }
NULL

#' Colour related aesthetics: colour, fill, and alpha
#'
#' These aesthetics parameters change the colour (`colour` and `fill`) and the
#' opacity (`alpha`) of geom elements on a plot. Almost every geom has either
#' colour or fill (or both), as well as can have their alpha modified.
#' Modifying colour on a plot is a useful way to enhance the presentation of data,
#' often especially when a plot graphs more than two variables.
#'
#' @section Colour and fill:
#'
#' The `colour` aesthetic is used to draw lines and strokes, such as in
#' [`geom_point()`] and [`geom_line()`], but also the line contours of
#' [`geom_rect()`] and [`geom_polygon()`]. The `fill` aesthetic is used to
#' colour the inside areas of geoms, such as [`geom_rect()`] and
#' [`geom_polygon()`], but also the insides of shapes 21-25 of [`geom_point()`].
#'
#' Colours and fills can be specified in the following ways:
#' * A name, e.g., `"red"`. R has 657 built-in named colours, which can be
#' listed with [grDevices::colors()].
#' * An rgb specification, with a string of the form `"#RRGGBB"` where each of the
#' pairs `RR`, `GG`, `BB` consists of two hexadecimal digits giving a value in the
#' range `00` to `FF`. You can optionally make the colour transparent by using the
#' form `"#RRGGBBAA"`.
#' * An `NA`, for a completely transparent colour.
#'
#' @section Alpha:
#'
#' Alpha refers to the opacity of a geom. Values of `alpha` range from 0 to 1,
#' with lower values corresponding to more transparent colors.
#'
#' Alpha can additionally be modified through the `colour` or `fill` aesthetic
#' if either aesthetic provides color values using an rgb specification
#' (`"#RRGGBBAA"`), where `AA` refers to transparency values.
#'
#'
#' @seealso
#' * Other options for modifying colour:
#' [scale_colour_brewer()],
#' [scale_colour_gradient()], [scale_colour_grey()],
#' [scale_colour_hue()], [scale_colour_identity()],
#' [scale_colour_manual()], [scale_colour_viridis_d()]
#' * Other options for modifying fill:
#' [scale_fill_brewer()],
#' [scale_fill_gradient()], [scale_fill_grey()],
#' [scale_fill_hue()], [scale_fill_identity()],
#' [scale_fill_manual()], [scale_fill_viridis_d()]
#' * Other options for modifying alpha:
#' [scale_alpha()], [scale_alpha_manual()], [scale_alpha_identity()]
#' * Run `vignette("ggplot2-specs")` to see an overview of other aesthetics that
#' can be modified.
#' @family aesthetics documentation
#'
#' @name aes_colour_fill_alpha
#' @aliases colour color fill
#' @examples
#' \donttest{
#'
#' # Bar chart example
#' p <- ggplot(mtcars, aes(factor(cyl)))
#' # Default plotting
#' p + geom_bar()
#' # To change the interior colouring use fill aesthetic
#' p + geom_bar(fill = "red")
#' # Compare with the colour aesthetic which changes just the bar outline
#' p + geom_bar(colour = "red")
#' # Combining both, you can see the changes more clearly
#' p + geom_bar(fill = "white", colour = "red")
#' # Both colour and fill can take an rgb specification.
#' p + geom_bar(fill = "#00abff")
#' # Use NA for a completely transparent colour.
#' p + geom_bar(fill = NA, colour = "#00abff")
#'
#' # Colouring scales differ depending on whether a discrete or
#' # continuous variable is being mapped. For example, when mapping
#' # fill to a factor variable, a discrete colour scale is used.
#' ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar()
#'
#' # When mapping fill to continuous variable a continuous colour
#' # scale is used.
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'   geom_raster(aes(fill = density))
#'
#' # Some geoms only use the colour aesthetic but not the fill
#' # aesthetic (e.g. geom_point() or geom_line()).
#' p <- ggplot(economics, aes(x = date, y = unemploy))
#' p + geom_line()
#' p + geom_line(colour = "green")
#' p + geom_point()
#' p + geom_point(colour = "red")
#'
#' # For large datasets with overplotting the alpha
#' # aesthetic will make the points more transparent.
#' set.seed(1)
#' df <- data.frame(x = rnorm(5000), y = rnorm(5000))
#' p  <- ggplot(df, aes(x,y))
#' p + geom_point()
#' p + geom_point(alpha = 0.5)
#' p + geom_point(alpha = 1/10)
#'
#' # Alpha can also be used to add shading.
#' p <- ggplot(economics, aes(x = date, y = unemploy)) + geom_line()
#' p
#' yrng <- range(economics$unemploy)
#' p <- p +
#'   geom_rect(
#'     aes(NULL, NULL, xmin = start, xmax = end, fill = party),
#'     ymin = yrng[1], ymax = yrng[2], data = presidential
#'   )
#' p
#' p + scale_fill_manual(values = alpha(c("blue", "red"), .3))
#' }
NULL

#' Differentiation related aesthetics: linetype, size, shape
#'
#' @description
#' The `linetype`, `linewidth`, `size`, and `shape` aesthetics modify the
#' appearance of lines and/or points. They also apply to the outlines of
#' polygons (`linetype` and `linewidth`) or to text (`size`).
#'
#' @section Linetype:
#' The `linetype` aesthetic can be specified with either an integer (0-6), a
#' name (0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash,
#' 6 = twodash), a mapping to a discrete variable, or a string of an even number
#' (up to eight) of hexadecimal digits which give the lengths in consecutive
#' positions in the string. See examples for a hex string demonstration.
#'
#' @section Linewidth and stroke:
#' The `linewidth` aesthetic sets the widths of lines, and can be specified
#' with a numeric value (for historical reasons, these units are about 0.75
#' millimetres). Alternatively, they can also be set via mapping to a continuous
#' variable. The `stroke` aesthetic serves the same role for points, but is
#' distinct for discriminating points from lines in geoms such as
#' [`geom_pointrange()`].
#'
#' @section Size:
#' The `size` aesthetic control the size of points and text, and can be
#' specified with a numerical value (in millimetres) or via a mapping to a
#' continuous variable.
#'
#' @section Shape:
#' The `shape` aesthetic controls the symbols of points, and can be specified
#' with an integer (between 0 and 25), a single character (which uses that
#' character as the plotting symbol), a `.` to draw the smallest rectangle that
#' is visible (i.e., about one pixel), an `NA` to draw nothing, or a mapping to
#' a discrete variable. Symbols and filled shapes are described in the examples
#' below.
#'
#' @seealso
#' * [geom_line()] and [geom_point()] for geoms commonly used
#'  with these aesthetics.
#' * [aes_group_order()] for using `linetype`, `size`, or
#' `shape` for grouping.
#' * Scales that can be used to modify these aesthetics: [`scale_linetype()`],
#'   [`scale_linewidth()`], [`scale_size()`], and [`scale_shape()`].
#' * Run `vignette("ggplot2-specs")` to see an overview of other aesthetics that
#' can be modified.
#' @family aesthetics documentation
#' @name aes_linetype_size_shape
#' @aliases linetype size shape
#' @examples
#'
#' df <- data.frame(x = 1:10 , y = 1:10)
#' p <- ggplot(df, aes(x, y))
#' p + geom_line(linetype = 2)
#' p + geom_line(linetype = "dotdash")
#'
#' # An example with hex strings; the string "33" specifies three units on followed
#' # by three off and "3313" specifies three units on followed by three off followed
#' # by one on and finally three off.
#' p + geom_line(linetype = "3313")
#'
#' # Mapping line type from a grouping variable
#' ggplot(economics_long, aes(date, value01)) +
#'   geom_line(aes(linetype = variable))
#'
#' # Linewidth examples
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_line(linewidth = 2, lineend = "round")
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_line(aes(linewidth = uempmed), lineend = "round")
#'
#' # Size examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point(size = 4)
#' p + geom_point(aes(size = qsec))
#' p + geom_point(size = 2.5) +
#'   geom_hline(yintercept = 25, size = 3.5)
#'
#' # Shape examples
#' p + geom_point()
#' p + geom_point(shape = 5)
#' p + geom_point(shape = "k", size = 3)
#' p + geom_point(shape = ".")
#' p + geom_point(shape = NA)
#' p + geom_point(aes(shape = factor(cyl)))
#'
#' # A look at all 25 symbols
#' df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
#' p <- ggplot(df2, aes(x, y))
#' p + geom_point(aes(shape = z), size = 4) +
#'   scale_shape_identity()
#' # While all symbols have a foreground colour, symbols 19-25 also take a
#' # background colour (fill)
#' p + geom_point(aes(shape = z), size = 4, colour = "Red") +
#'   scale_shape_identity()
#' p + geom_point(aes(shape = z), size = 4, colour = "Red", fill = "Black") +
#'   scale_shape_identity()
NULL
