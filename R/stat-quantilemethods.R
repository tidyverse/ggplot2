#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuantile <- ggproto(
  "StatQuantile", Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, quantiles = c(0.25, 0.5, 0.75),
                           formula = NULL, xseq = NULL, method = "rq",
                           method.args = list(), lambda = 1, na.rm = FALSE) {
    check_installed("quantreg", reason = "for `stat_quantile()`.")

    if (is.null(formula)) {
      if (method == "rqss") {
        formula <- eval(
          substitute(y ~ qss(x, lambda = lambda)),
          list(lambda = lambda)
        )
        # make qss function available in case it is needed;
        # works around limitation in quantreg
        qss <- quantreg::qss
      } else {
        formula <- y ~ x
      }
      cli::cli_inform("Smoothing formula not specified. Using: {deparse(formula)}")
    }

    if (is.null(data$weight)) data$weight <- 1

    if (is.null(xseq)) {
      xmin <- min(data$x, na.rm = TRUE)
      xmax <- max(data$x, na.rm = TRUE)
      xseq <- seq(xmin, xmax, length.out = 100)
    }
    grid <- data_frame0(x = xseq, .size = length(xseq))

    # if method was specified as a character string, replace with
    # the corresponding function
    if (identical(method, "rq")) {
      method <- quantreg::rq
    } else if (identical(method, "rqss")) {
      method <- quantreg::rqss
    } else {
      method <- match.fun(method) # allow users to supply their own methods
    }

    result <- lapply(
      quantiles,
      quant_pred,
      data = data,
      method = method,
      formula = formula,
      weight = weight,
      grid = grid,
      method.args = method.args
    )
    vec_rbind0(!!!result)
  },

  # weight is no longer available after transformation
  dropped_aes = "weight"
)

#' @param quantiles conditional quantiles of y to calculate and display
#' @param formula formula relating y variables to x variables
#' @param method Quantile regression method to use. Available options are `"rq"` (for
#'    [`quantreg::rq()`]) and `"rqss"` (for [`quantreg::rqss()`]).
#' @inheritParams layer
#' @inheritParams geom_point
#' @eval rd_computed_vars(
#'   quantile = "Quantile of distribution."
#' )
#' @export
#' @rdname geom_quantile
stat_quantile <- make_constructor(
  StatQuantile, geom = "quantile",
  omit = c("xseq", "lambda")
)

quant_pred <- function(quantile, data, method, formula, weight, grid,
                       method.args = method.args) {
  model <- inject(method(
    formula,
    data = data,
    tau = quantile,
    weights = weight,
    !!!method.args
  ))

  grid$y <- stats::predict(model, newdata = grid)
  grid$quantile <- quantile
  grid$group <- paste(data$group[1], quantile, sep = "-")

  grid
}
