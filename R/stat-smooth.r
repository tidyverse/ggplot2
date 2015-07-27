#' @param method smoothing method (function) to use, eg. lm, glm, gam, loess,
#'   rlm. For datasets with n < 1000 default is \code{\link{loess}}. For datasets
#'   with 1000 or more observations defaults to gam, see \code{\link[mgcv]{gam}}
#'   for more details.
#' @param formula formula to use in smoothing function, eg. \code{y ~ x},
#'   \code{y ~ poly(x, 2)}, \code{y ~ log(x)}
#' @param se display confidence interval around smooth? (TRUE by default, see
#'   level to control
#' @param fullrange should the fit span the full range of the plot, or just
#'   the data
#' @param level level of confidence interval to use (0.95 by default)
#' @param n number of points to evaluate smoother at
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @return \code{stat_smooth} generates a data frame with columns:
#'   \item{y}{predicted value}
#'   \item{ymin}{lower pointwise confidence interval around the mean}
#'   \item{ymax}{upper pointwise confidence interval around the mean}
#'   \item{se}{standard error}
#' @export
#' @rdname geom_smooth
stat_smooth <- function(mapping = NULL, data = NULL, geom = "smooth",
  position = "identity", method = "auto", formula = y ~ x, se = TRUE, n = 80,
  fullrange = FALSE, level = 0.95, na.rm = FALSE, show_guide = NA,
  inherit.aes = TRUE, ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmooth,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    stat_params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm
    ),
    params = list(...)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSmooth <- ggproto("StatSmooth", Stat,
  calculate_groups = function(self, data, scales, method = "auto",
    formula = y~x, ...)
  {
    rows <- plyr::daply(data, "group", function(df) length(unique(df$x)))

    if (all(rows == 1) && length(rows) > 1) {
      message("geom_smooth: Only one unique x value each group.",
        "Maybe you want aes(group = 1)?")
      return(data.frame())
    }

    # Figure out what type of smoothing to do: loess for small datasets,
    # gam with a cubic regression basis for large data
    # This is based on the size of the _largest_ group.
    if (identical(method, "auto")) {
      groups <- plyr::count(data, "group")

      if (max(groups$freq) < 1000) {
        method <- "loess"
        message('geom_smooth: method="auto" and size of largest group is <1000,',
                ' so using loess.',
                ' Use \'method = x\' to change the smoothing method.')
      } else {
        method <- "gam"
        formula <- y ~ s(x, bs = "cs")
        message('geom_smooth: method="auto" and size of largest group is >=1000,',
                ' so using gam with formula: y ~ s(x, bs = "cs").',
                ' Use \'method = x\' to change the smoothing method.')
      }
    }
    if (identical(method, "gam")) {
      method <- mgcv::gam
    }

    ggproto_parent(Stat, self)$calculate_groups(data, scales, method = method,
      formula = formula, ...)
  },

  calculate = function(data, scales, method = "auto", formula = y~x,
    se = TRUE, n = 80, fullrange = FALSE, xseq = NULL, level = 0.95,
    na.rm = FALSE, ...)
  {
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_smooth")
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(data.frame())
    }

    if (is.null(data$weight)) data$weight <- 1

    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scale_dimension(scales$x, c(0, 0))
        } else {
          xseq <- sort(unique(data$x))
        }
      } else {
        if (fullrange) {
          range <- scale_dimension(scales$x, c(0, 0))
        } else {
          range <- range(data$x, na.rm=TRUE)
        }
        xseq <- seq(range[1], range[2], length.out = n)
      }
    }
    if (is.character(method)) method <- match.fun(method)

    method.special <- function(...)
      method(formula, data=data, weights=weight, ...)
    model <- safe.call(method.special, list(...), names(formals(method)))

    predictdf(model, xseq, se, level)
  },

  required_aes = c("x", "y")
)
