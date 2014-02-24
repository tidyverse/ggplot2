#' Continuous quantiles.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "quantile")}
#'
#' @param quantiles conditional quantiles of y to calculate and display
#' @param formula formula relating y variables to x variables
#' @param method Quantile regression method to use.  Currently only supports
#'    \code{\link[quantreg]{rq}}.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @inheritParams stat_identity
#' @return a data.frame with additional columns:
#'   \item{quantile}{quantile of distribution}
#' @export
#' @examples
#' \donttest{
#' msamp <- movies[sample(nrow(movies), 1000), ]
#' m <- ggplot(msamp, aes(year, rating)) + geom_point()
#' m + stat_quantile()
#' m + stat_quantile(quantiles = 0.5)
#' q10 <- seq(0.05, 0.95, by=0.05)
#' m + stat_quantile(quantiles = q10)
#'
#' # You can also use rqss to fit smooth quantiles
#' m + stat_quantile(method = "rqss")
#' # Note that rqss doesn't pick a smoothing constant automatically, so
#' # you'll need to tweak lambda yourself
#' m + stat_quantile(method = "rqss", lambda = 10)
#' m + stat_quantile(method = "rqss", lambda = 100)
#'
#' # Use 'votes' as weights for the quantile calculation
#' m + stat_quantile(aes(weight=votes))
#'
#' # Change scale
#' m + stat_quantile(aes(colour = ..quantile..), quantiles = q10)
#' m + stat_quantile(aes(colour = ..quantile..), quantiles = q10) +
#'   scale_colour_gradient2(midpoint = 0.5)
#'
#' # Set aesthetics to fixed value
#' m + stat_quantile(colour = "red", size = 2, linetype = 2)
#'
#' # Use qplot instead
#' qplot(year, rating, data=movies, geom="quantile")
#' }
stat_quantile <- function (mapping = NULL, data = NULL, geom = "quantile", position = "identity",
quantiles = c(0.25, 0.5, 0.75), formula = NULL, method = "rq",
na.rm = FALSE, ...) {
  StatQuantile$new(mapping = mapping, data = data, geom = geom,
  position = position, quantiles = quantiles, formula = formula,
  method = method, na.rm = na.rm, ...)
}

StatQuantile <- proto(Stat, {
  objname <- "quantile"

  default_geom <- function(.) GeomQuantile
  default_aes <- function(.) aes()
  required_aes <- c("x", "y")

  calculate <- function(., data, scales, quantiles = c(0.25, 0.5, 0.75),
    formula = NULL, xseq = NULL, method = "rq", lambda = 1, na.rm = FALSE,
    ...) {

    try_require("quantreg")

    if (is.null(formula)) {
      if (method == "rqss") {
        try_require("MatrixModels")
        formula <- eval(substitute(y ~ qss(x, lambda = lambda)),
          list(lambda = lambda))
      } else {
        formula <- y ~ x
      }
      message("Smoothing formula not specified. Using: ",
        deparse(formula))
    }

    if (is.null(data$weight)) data$weight <- 1

    if (is.null(xseq)) {
      xmin <- min(data$x, na.rm = TRUE)
      xmax <- max(data$x, na.rm = TRUE)
      xseq <- seq(xmin, xmax, length = 100)
    }
    grid <- data.frame(x = xseq)

    data <- as.data.frame(data)
    data <- remove_missing(data, na.rm, c("x", "y"), name = "stat_quantile")
    method <- match.fun(method)

    ldply(quantiles, quant_pred, data = data, method = method,
      formula = formula, weight = weight, grid = grid, ...)
  }
})

quant_pred <- function(quantile, data, method, formula, weight, grid, ...) {
  model <- method(formula, data = data, tau = quantile, weight = weight, ...)

  grid$y <- predict(model, newdata = grid)
  grid$quantile <- quantile
  grid$group <- paste(data$group[1], quantile, sep = "-")

  grid
}
