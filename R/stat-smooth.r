#' @param method Smoothing method (function) to use, accepts either
#'   `NULL` or a character vector, e.g. `"lm"`, `"glm"`, `"gam"`, `"loess"`
#'   or a function, e.g. `MASS::rlm` or `mgcv::gam`, `stats::lm`, or `stats::loess`.
#'   `"auto"` is also accepted for backwards compatibility.  It is equivalent to
#'   `NULL`.
#'
#'   For `method = NULL` the smoothing method is chosen based on the
#'   size of the largest group (across all panels). [stats::loess()] is
#'   used for less than 1,000 observations; otherwise [mgcv::gam()] is
#'   used with `formula = y ~ s(x, bs = "cs")` with `method = "REML"`. Somewhat anecdotally,
#'   `loess` gives a better appearance, but is \eqn{O(N^{2})}{O(N^2)} in memory,
#'   so does not work for larger datasets.
#'
#'   If you have fewer than 1,000 observations but want to use the same `gam()`
#'   model that `method = NULL` would use, then set
#'   `method = "gam", formula = y ~ s(x, bs = "cs")`.
#' @param formula Formula to use in smoothing function, eg. `y ~ x`,
#'   `y ~ poly(x, 2)`, `y ~ log(x)`. `NULL` by default, in which case
#'   `method = NULL` implies `formula = y ~ x` when there are fewer than 1,000
#'   observations and `formula = y ~ s(x, bs = "cs")` otherwise.
#' @param se Display confidence interval around smooth? (`TRUE` by default, see
#'   `level` to control.)
#' @param fullrange Should the fit span the full range of the plot, or just
#'   the data?
#' @param level Level of confidence interval to use (0.95 by default).
#' @param span Controls the amount of smoothing for the default loess smoother.
#'   Smaller numbers produce wigglier lines, larger numbers produce smoother
#'   lines.
#' @param n Number of points at which to evaluate smoother.
#' @param method.args List of additional arguments passed on to the modelling
#'   function defined by `method`.
#' @section Computed variables:
#' \describe{
#'   \item{y}{predicted value}
#'   \item{ymin}{lower pointwise confidence interval around the mean}
#'   \item{ymax}{upper pointwise confidence interval around the mean}
#'   \item{se}{standard error}
#' }
#' @export
#' @rdname geom_smooth
stat_smooth <- function(mapping = NULL, data = NULL,
                        geom = "smooth", position = "identity",
                        ...,
                        method = NULL,
                        formula = NULL,
                        se = TRUE,
                        n = 80,
                        span = 0.75,
                        fullrange = FALSE,
                        level = 0.95,
                        method.args = list(),
                        na.rm = FALSE,
                        orientation = NA,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmooth,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      method.args = method.args,
      span = span,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSmooth <- ggproto("StatSmooth", Stat,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    msg <- character()
    if (is.null(params$method) || identical(params$method, "auto")) {
      # Use loess for small datasets, gam with a cubic regression basis for
      # larger. Based on size of the _largest_ group to avoid bad memory
      # behaviour of loess
      max_group <- max(table(interaction(data$group, data$PANEL, drop = TRUE)))

      if (max_group < 1000) {
        params$method <- "loess"
      } else {
        params$method <- "gam"
      }
      msg <- c(msg, paste0("method = '", params$method, "'"))
    }

    if (is.null(params$formula)) {
      if (identical(params$method, "gam")) {
        params$formula <- y ~ s(x, bs = "cs")
      } else {
        params$formula <- y ~ x
      }
      msg <- c(msg, paste0("formula '", deparse(params$formula), "'"))
    }
    if (identical(params$method, "gam")) {
      params$method <- mgcv::gam
    }

    if (length(msg) > 0) {
      message("`geom_smooth()` using ", paste0(msg, collapse = " and "))
    }

    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(data, scales, method = NULL, formula = NULL,
                           se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                           xseq = NULL, level = 0.95, method.args = list(),
                           na.rm = FALSE, flipped_aes = NA) {
    data <- flip_data(data, flipped_aes)
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(new_data_frame())
    }

    if (is.null(data$weight)) data$weight <- 1

    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scales$x$dimension()
        } else {
          xseq <- sort(unique(data$x))
        }
      } else {
        if (fullrange) {
          range <- scales$x$dimension()
        } else {
          range <- range(data$x, na.rm = TRUE)
        }
        xseq <- seq(range[1], range[2], length.out = n)
      }
    }

    # Special case span because it's the most commonly used model argument
    if (identical(method, "loess")) {
      method.args$span <- span
    }

    if (is.character(method)) {
      if (identical(method, "gam")) {
        method <- mgcv::gam
      } else {
        method <- match.fun(method)
      }
    }
    # If gam and gam's method is not specified by the user then use REML
    if (identical(method, mgcv::gam) && is.null(method.args$method)) {
      method.args$method <- "REML"
    }

    base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
    model <- do.call(method, c(base.args, method.args))

    prediction <- predictdf(model, xseq, se, level)
    prediction$flipped_aes <- flipped_aes
    flip_data(prediction, flipped_aes)
  },

  required_aes = c("x", "y")
)
