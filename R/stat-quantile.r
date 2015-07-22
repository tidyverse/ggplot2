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
#' @rdname geom_quantile
stat_quantile <- function (mapping = NULL, data = NULL, geom = "quantile",
  position = "identity", quantiles = c(0.25, 0.5, 0.75), formula = NULL,
  method = "rq", na.rm = FALSE, show_guide = NA, inherit.aes = TRUE, ...)
{
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatQuantile,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    stat_params = list(
      quantiles = quantiles,
      formula = formula,
      method = method,
      na.rm = na.rm
    ),
    params = list(...)
  )
}


StatQuantile <- proto2("StatQuantile", Stat,
  required_aes = c("x", "y"),

  calculate = function(self, data, scales, quantiles = c(0.25, 0.5, 0.75),
    formula = NULL, xseq = NULL, method = "rq", lambda = 1, na.rm = FALSE,
    ...)
  {
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
      xseq <- seq(xmin, xmax, length.out = 100)
    }
    grid <- data.frame(x = xseq)

    data <- as.data.frame(data)
    data <- remove_missing(data, na.rm, c("x", "y"), name = "stat_quantile")
    method <- match.fun(method)

    ldply(quantiles, quant_pred, data = data, method = method,
      formula = formula, weight = weight, grid = grid, ...)
  }
)

quant_pred <- function(quantile, data, method, formula, weight, grid, ...) {
  model <- method(formula, data = data, tau = quantile, weights = weight, ...)

  grid$y <- predict(model, newdata = grid)
  grid$quantile <- quantile
  grid$group <- paste(data$group[1], quantile, sep = "-")

  grid
}
