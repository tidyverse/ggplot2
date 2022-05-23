#' Summarise y values at unique/binned x
#'
#' `stat_summary()` operates on unique `x` or `y`; `stat_summary_bin()`
#' operates on binned `x` or `y`. They are more flexible versions of
#' [stat_bin()]: instead of just counting, they can compute any
#' aggregate.
#'
#' @eval rd_orientation()
#'
#' @eval rd_aesthetics("stat", "summary")
#' @seealso [geom_errorbar()], [geom_pointrange()],
#'  [geom_linerange()], [geom_crossbar()] for geoms to
#'  display summarised data
#' @inheritParams stat_identity
#' @section Summary functions:
#' You can either supply summary functions individually (`fun`,
#' `fun.max`, `fun.min`), or as a single function (`fun.data`):
#'
#' \describe{
#'   \item{fun.data}{Complete summary function. Should take numeric vector as
#'      input and return data frame as output}
#'   \item{fun.min}{min summary function (should take numeric vector and
#'     return single number)}
#'   \item{fun}{main summary function (should take numeric vector and return
#'     single number)}
#'   \item{fun.max}{max summary function (should take numeric vector and
#'     return single number)}
#' }
#'
#' A simple vector function is easiest to work with as you can return a single
#' number, but is somewhat less flexible. If your summary function computes
#' multiple values at once (e.g. min and max), use `fun.data`.
#'
#' `fun.data` will receive data as if it was oriented along the x-axis and
#' should return a data.frame that corresponds to that orientation. The layer
#' will take care of flipping the input and output if it is oriented along the
#' y-axis.
#'
#' If no aggregation functions are supplied, will default to
#' [mean_se()].
#'
#' @param fun.data A function that is given the complete data and should
#'   return a data frame with variables `ymin`, `y`, and `ymax`.
#' @param fun.min,fun,fun.max Alternatively, supply three individual
#'   functions that are each passed a vector of values and should return a
#'   single number.
#' @param fun.ymin,fun.y,fun.ymax `r lifecycle::badge("deprecated")` Use the
#'   versions specified above instead.
#' @param fun.args Optional additional arguments passed on to the functions.
#' @export
#' @examples
#' d <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
#' d + stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 2)
#'
#' # Orientation follows the discrete axis
#' ggplot(mtcars, aes(mpg, factor(cyl))) +
#'   geom_point() +
#'   stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 2)
#'
#' # You can supply individual functions to summarise the value at
#' # each x:
#' d + stat_summary(fun = "median", colour = "red", size = 2, geom = "point")
#' d + stat_summary(fun = "mean", colour = "red", size = 2, geom = "point")
#' d + aes(colour = factor(vs)) + stat_summary(fun = mean, geom="line")
#'
#' d + stat_summary(fun = mean, fun.min = min, fun.max = max, colour = "red")
#'
#' d <- ggplot(diamonds, aes(cut))
#' d + geom_bar()
#' d + stat_summary(aes(y = price), fun = "mean", geom = "bar")
#'
#' # Orientation of stat_summary_bin is ambiguous and must be specified directly
#' ggplot(diamonds, aes(carat, price)) +
#'   stat_summary_bin(fun = "mean", geom = "bar", orientation = 'y')
#'
#' \donttest{
#' # Don't use ylim to zoom into a summary plot - this throws the
#' # data away
#' p <- ggplot(mtcars, aes(cyl, mpg)) +
#'   stat_summary(fun = "mean", geom = "point")
#' p
#' p + ylim(15, 30)
#' # Instead use coord_cartesian
#' p + coord_cartesian(ylim = c(15, 30))
#'
#' # A set of useful summary functions is provided from the Hmisc package:
#' stat_sum_df <- function(fun, geom="crossbar", ...) {
#'   stat_summary(fun.data = fun, colour = "red", geom = geom, width = 0.2, ...)
#' }
#' d <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
#' # The crossbar geom needs grouping to be specified when used with
#' # a continuous x axis.
#' d + stat_sum_df("mean_cl_boot", mapping = aes(group = cyl))
#' d + stat_sum_df("mean_sdl", mapping = aes(group = cyl))
#' d + stat_sum_df("mean_sdl", fun.args = list(mult = 1), mapping = aes(group = cyl))
#' d + stat_sum_df("median_hilow", mapping = aes(group = cyl))
#'
#' # An example with highly skewed distributions:
#' if (require("ggplot2movies")) {
#' set.seed(596)
#' mov <- movies[sample(nrow(movies), 1000), ]
#'  m2 <-
#'    ggplot(mov, aes(x = factor(round(rating)), y = votes)) +
#'    geom_point()
#'  m2 <-
#'    m2 +
#'    stat_summary(
#'      fun.data = "mean_cl_boot",
#'      geom = "crossbar",
#'      colour = "red", width = 0.3
#'    ) +
#'    xlab("rating")
#' m2
#' # Notice how the overplotting skews off visual perception of the mean
#' # supplementing the raw data with summary statistics is _very_ important
#'
#' # Next, we'll look at votes on a log scale.
#'
#' # Transforming the scale means the data are transformed
#' # first, after which statistics are computed:
#' m2 + scale_y_log10()
#' # Transforming the coordinate system occurs after the
#' # statistic has been computed. This means we're calculating the summary on the raw data
#' # and stretching the geoms onto the log scale.  Compare the widths of the
#' # standard errors.
#' m2 + coord_trans(y="log10")
#' }
#' }
stat_summary <- function(mapping = NULL, data = NULL,
                         geom = "pointrange", position = "identity",
                         ...,
                         fun.data = NULL,
                         fun = NULL,
                         fun.max = NULL,
                         fun.min = NULL,
                         fun.args = list(),
                         na.rm = FALSE,
                         orientation = NA,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         fun.y = deprecated(),
                         fun.ymin = deprecated(),
                         fun.ymax = deprecated()) {
  if (lifecycle::is_present(fun.y)) {
    lifecycle::deprecate_warn("3.3.0", "stat_summary(fun.y)", "stat_summary(fun)")
    fun = fun %||% fun.y
  }
  if (lifecycle::is_present(fun.ymin)) {
    lifecycle::deprecate_warn("3.3.0", "stat_summary(fun.ymin)", "stat_summary(fun.min)")
    fun.min = fun.min %||% fun.ymin
  }
  if (lifecycle::is_present(fun.ymax)) {
    lifecycle::deprecate_warn("3.3.0", "stat_summary(fun.ymax)", "stat_summary(fun.max)")
    fun.max = fun.max %||% fun.ymax
  }
  layer(
    data = data,
    mapping = mapping,
    stat = StatSummary,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      fun.data = fun.data,
      fun = fun,
      fun.max = fun.max,
      fun.min = fun.min,
      fun.args = fun.args,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSummary <- ggproto("StatSummary", Stat,
  required_aes = c("x", "y"),

  extra_params = c("na.rm", "orientation"),
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  compute_panel = function(data, scales, fun.data = NULL, fun = NULL,
                     fun.max = NULL, fun.min = NULL, fun.args = list(),
                     na.rm = FALSE, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    fun <- make_summary_fun(fun.data, fun, fun.max, fun.min, fun.args)
    summarised <- summarise_by_x(data, fun)
    summarised$flipped_aes <- flipped_aes
    flip_data(summarised, flipped_aes)
  }
)

# Summarise a data.frame by parts
# Summarise a data frame by unique value of x
#
# This function is used by [stat_summary()] to break a
# data.frame into pieces, summarise each piece, and join the pieces
# back together, retaining original columns unaffected by the summary.
#
# @param [data.frame()] to summarise
# @param vector to summarise by
# @param summary function (must take and return a data.frame)
# @param other arguments passed on to summary function
# @keyword internal
summarise_by_x <- function(data, summary, ...) {
  summary <- dapply(data, c("group", "x"), summary, ...)
  unique <- dapply(data, c("group", "x"), uniquecols)
  unique$y <- NULL

  merge(summary, unique, by = c("x", "group"), sort = FALSE)
}

#' A selection of summary functions from Hmisc
#'
#' @description
#' These are wrappers around functions from \pkg{Hmisc} designed to make them
#' easier to use with [stat_summary()]. See the Hmisc documentation
#' for more details:
#'
#'  - [Hmisc::smean.cl.boot()]
#'  - [Hmisc::smean.cl.normal()]
#'  - [Hmisc::smean.sdl()]
#'  - [Hmisc::smedian.hilow()]
#'
#' @param x a numeric vector
#' @param ... other arguments passed on to the respective Hmisc function.
#' @return A data frame with columns `y`, `ymin`, and `ymax`.
#' @name hmisc
#' @examples
#' if (requireNamespace("Hmisc", quietly = TRUE)) {
#' set.seed(1)
#' x <- rnorm(100)
#' mean_cl_boot(x)
#' mean_cl_normal(x)
#' mean_sdl(x)
#' median_hilow(x)
#' }
NULL

wrap_hmisc <- function(fun) {

  function(x, ...) {
    check_installed("Hmisc")

    fun <- getExportedValue("Hmisc", fun)
    result <- do.call(fun, list(x = quote(x), ...))

    rename(
      new_data_frame(as.list(result)),
      c(Median = "y", Mean = "y", Lower = "ymin", Upper = "ymax")
    )
  }
}
#' @export
#' @rdname hmisc
mean_cl_boot <- wrap_hmisc("smean.cl.boot")
#' @export
#' @rdname hmisc
mean_cl_normal <- wrap_hmisc("smean.cl.normal")
#' @export
#' @rdname hmisc
mean_sdl <- wrap_hmisc("smean.sdl")
#' @export
#' @rdname hmisc
median_hilow <- wrap_hmisc("smedian.hilow")

#' Calculate mean and standard error of the mean
#'
#' For use with [stat_summary()]
#'
#' @param x numeric vector.
#' @param mult number of multiples of standard error.
#' @return A data frame with three columns:
#' \describe{
#'     \item{`y`}{ The mean.}
#'     \item{`ymin`}{ The mean minus the multiples of the standard error.}
#'     \item{`ymax`}{ The mean plus the multiples of the standard error.}
#' }
#' @export
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' mean_se(x)
mean_se <- function(x, mult = 1) {
  x <- stats::na.omit(x)
  se <- mult * sqrt(stats::var(x) / length(x))
  mean <- mean(x)
  new_data_frame(list(y = mean, ymin = mean - se, ymax = mean + se), n = 1)
}
