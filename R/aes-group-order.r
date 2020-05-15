#' Aesthetics: grouping
#'
#' @name aes_group_order
#' @aliases group
#'
#' @description
#' The `group` aesthetic is by default set to the interaction of all discrete variables
#' in the plot. This choice often partitions the data correctly, but when it does not,
#' or when no discrete variable is used in the plot, you will need to explicitly define the
#' grouping structure, by mapping `group` to a variable that has a different value
#' for each group.
#'
#' @details
#' For most applications you can specify the grouping with implicitly via the default option,
#' by specifying various other aesthetics, such as `colour`, `shape`, `fill`, or `linetype`.
#' This is demonstrated in the examples below with the `mtcars`, `economics` and `diamonds`
#' datasets.
#'
#' There are three common cases where the default does not display the data correctly.
#' The examples below use a longitudinal dataset, `Oxboys`, from the nlme package to demonstrate
#' these cases. `Oxboys` records the heights (height) and centered ages (age) of 26 boys (Subject),
#' measured on nine occasions (Occasion).
#'
#' @seealso
#' * Geoms commonly used with groups: [geom_point()], [geom_bar()], [geom_line()],
#'   [geom_histogram()]
#' * Run `vignette("ggplot2-specs")` to see an overview of other aesthestics that
#' can be modified.
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
#' rescale01 <- function(x) (x - min(x)) / diff(range(x))
#' ec_scaled <- data.frame(
#'   date = economics$date,
#'   lapply(economics[, -(1:2)], rescale01))
#' ecm <- reshape2::melt(ec_scaled, id.vars = "date")
#' ggplot(ecm, aes(date, value)) + geom_line(aes(linetype = variable))
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
