#' Summarise y values at every unique x.
#'
#' \code{stat_summary} allows for tremendous flexibilty in the specification
#' of summary functions. The summary function can either supply individual
#' summary functions for each of y, ymin and ymax (with \code{fun.y},
#' \code{fun.ymax}, \code{fun.ymin}), or return a data frame containing any
#' number of aesthetiics with with \code{fun.data}. All summary functions
#' are called with a single vector of values, \code{x}.
#'
#' A simple vector function is easiest to work with as you can return a single
#' number, but is somewhat less flexible.  If your summary function operates
#' on a data.frame it should return a data frame with variables that the geom
#' can use.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "summary")}
#'
#' @seealso \code{\link{geom_errorbar}}, \code{\link{geom_pointrange}},
#'  \code{\link{geom_linerange}}, \code{\link{geom_crossbar}} for geoms to
#'  display summarised data
#' @inheritParams stat_identity
#' @return a data.frame with additional columns:
#'   \item{fun.data}{Complete summary function. Should take data frame as
#'      input and return data frame as output}
#'   \item{fun.ymin}{ymin summary function (should take numeric vector and
#'     return single number)}
#'   \item{fun.y}{y summary function (should take numeric vector and return
#'     single number)}
#'   \item{fun.ymax}{ymax summary function (should take numeric vector and
#'     return single number)}
#' @export
#' @examples
#' \donttest{
#' # Basic operation on a small dataset
#' d <- qplot(cyl, mpg, data=mtcars)
#' d + stat_summary(fun.data = "mean_cl_boot", colour = "red")
#'
#' p <- qplot(cyl, mpg, data = mtcars, stat="summary", fun.y = "mean")
#' p
#' # Don't use ylim to zoom into a summary plot - this throws the
#' # data away
#' p + ylim(15, 30)
#' # Instead use coord_cartesian
#' p + coord_cartesian(ylim = c(15, 30))
#'
#' # You can supply individual functions to summarise the value at
#' # each x:
#'
#' stat_sum_single <- function(fun, geom="point", ...) {
#'   stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
#' }
#'
#' d + stat_sum_single(mean)
#' d + stat_sum_single(mean, geom="line")
#' d + stat_sum_single(median)
#' d + stat_sum_single(sd)
#'
#' d + stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max,
#'   colour = "red")
#'
#' d + aes(colour = factor(vs)) + stat_summary(fun.y = mean, geom="line")
#'
#' # Alternatively, you can supply a function that operates on a data.frame.
#' # A set of useful summary functions is provided from the Hmisc package:
#'
#' stat_sum_df <- function(fun, geom="crossbar", ...) {
#'   stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...)
#' }
#'
#' # The crossbar geom needs grouping to be specified when used with
#' # a continuous x axis.
#' d + stat_sum_df("mean_cl_boot", mapping = aes(group = cyl))
#' d + stat_sum_df("mean_sdl", mapping = aes(group = cyl))
#' d + stat_sum_df("mean_sdl", mult = 1, mapping = aes(group = cyl))
#' d + stat_sum_df("median_hilow", mapping = aes(group = cyl))
#'
#' # There are lots of different geoms you can use to display the summaries
#'
#' d + stat_sum_df("mean_cl_normal", mapping = aes(group = cyl))
#' d + stat_sum_df("mean_cl_normal", geom = "errorbar")
#' d + stat_sum_df("mean_cl_normal", geom = "pointrange")
#' d + stat_sum_df("mean_cl_normal", geom = "smooth")
#'
#' # Summaries are more useful with a bigger data set:
#' mpg2 <- subset(mpg, cyl != 5L)
#' m <- ggplot(mpg2, aes(x=cyl, y=hwy)) +
#'         geom_point() +
#'         stat_summary(fun.data = "mean_sdl", geom = "linerange",
#'                      colour = "red", size = 2, mult = 1) +
#'        xlab("cyl")
#' m
#' # An example with highly skewed distributions:
#' set.seed(596)
#' mov <- movies[sample(nrow(movies), 1000), ]
#'  m2 <- ggplot(mov, aes(x= factor(round(rating)), y=votes)) + geom_point()
#'  m2 <- m2 + stat_summary(fun.data = "mean_cl_boot", geom = "crossbar",
#'                          colour = "red", width = 0.3) + xlab("rating")
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
stat_summary <- function (mapping = NULL, data = NULL, geom = "pointrange", position = "identity", ...) {
  StatSummary$new(mapping = mapping, data = data, geom = geom, position = position, ...)
}

StatSummary <- proto(Stat, {
  objname <- "summary"

  default_geom <- function(.) GeomPointrange
  required_aes <- c("x", "y")

  calculate_groups <- function(., data, scales, fun.data = NULL, fun.y = NULL, fun.ymax = NULL, fun.ymin = NULL, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm, c("x", "y"), name = "stat_summary")

    if (!missing(fun.data)) {
      # User supplied function that takes complete data frame as input
      fun.data <- match.fun(fun.data)
      fun <- function(df, ...) {
        fun.data(df$y, ...)
      }
    } else {
      # User supplied individual vector functions
      fs <- compact(list(ymin = fun.ymin, y = fun.y, ymax = fun.ymax))

      fun <- function(df, ...) {
        res <- llply(fs, function(f) do.call(f, list(df$y, ...)))
        names(res) <- names(fs)
        as.data.frame(res)
      }
    }

    summarise_by_x(data, fun, ...)
  }


})

# Summarise a data.frame by parts
# Summarise a data frame by unique value of x
#
# This function is used by \code{\link{stat_summary}} to break a
# data.frame into pieces, summarise each piece, and join the pieces
# back together, retaining original columns unaffected by the summary.
#
# @param \code{\link{data.frame}} to summarise
# @param vector to summarise by
# @param summary function (must take and return a data.frame)
# @param other arguments passed on to summary function
# @keyword internal
summarise_by_x <- function(data, summary, ...) {
  summary <- ddply(data, c("group", "x"), summary, ...)
  unique <- ddply(data, c("group", "x"), uniquecols)
  unique$y <- NULL

  merge(summary, unique, by = c("x", "group"))
}

#' Wrap up a selection of summary functions from Hmisc to make it easy to use
#' with \code{\link{stat_summary}}.
#'
#' See the Hmisc documentation for details of their options.
#'
#' @param x a numeric vector
#' @param ... other arguments passed on to the respective Hmisc function.
#' @seealso \code{\link[Hmisc]{smean.cl.boot}},
#'   \code{\link[Hmisc]{smean.cl.normal}}, \code{\link[Hmisc]{smean.sdl}},
#'    \code{\link[Hmisc]{smedian.hilow}}
#' @name hmisc
NULL

wrap_hmisc <- function(fun) {
  function(x, ...) {
    try_require("Hmisc")

    result <- safe.call(fun, list(x = x, ...))
    rename(
      data.frame(t(result)),
      c(Median = "y", Mean = "y", Lower = "ymin", Upper = "ymax"),
      warn_missing = FALSE
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

#' Calculate mean and standard errors on either side.
#'
#' @param x numeric vector
#' @param mult number of multiples of standard error
#' @seealso for use with \code{\link{stat_summary}}
#' @export
mean_se <- function(x, mult = 1) {
  x <- na.omit(x)
  se <- mult * sqrt(var(x) / length(x))
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - se, ymax = mean + se)
}
