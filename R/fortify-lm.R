#' Supplement the data fitted to a linear model with model fit statistics.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This method is deprecated because using `broom::augment()` is a better
#' solution to supplement data from a linear model.
#' If you have missing values in your model data, you may need to refit
#' the model with `na.action = na.exclude`.
#'
#' @return The original data with extra columns:
#'   \item{.hat}{Diagonal of the hat matrix}
#'   \item{.sigma}{Estimate of residual standard deviation when
#'     corresponding observation is dropped from model}
#'   \item{.cooksd}{Cooks distance, [cooks.distance()]}
#'   \item{.fitted}{Fitted values of model}
#'   \item{.resid}{Residuals}
#'   \item{.stdresid}{Standardised residuals}
#' @param model linear model
#' @param data data set, defaults to data used to fit model
#' @param ... not used by this method
#' @keywords internal
#' @export
#' @examplesIf require("broom")
#' mod <- lm(mpg ~ wt, data = mtcars)
#'
#' # Show augmented model
#' head(augment(mod))
#' head(fortify(mod))
#'
#' # Using augment to convert model to ready-to-plot data
#' ggplot(augment(mod), aes(.fitted, .resid)) +
#'   geom_point() +
#'   geom_hline(yintercept = 0) +
#'   geom_smooth(se = FALSE)
#'
#' # Colouring by original data not included in the model
#' ggplot(augment(mod, mtcars), aes(.fitted, .std.resid, colour = factor(cyl))) +
#'   geom_point()
fortify.lm <- function(model, data = model$model, ...) {
  lifecycle::deprecate_warn(
    "3.6.0", I("`fortify(<lm>)`"), I("`broom::augment(<lm>)`")
  )
  infl <- stats::influence(model, do.coef = FALSE)
  data$.hat <- infl$hat
  data$.sigma <- infl$sigma
  data$.cooksd <- stats::cooks.distance(model, infl)

  data$.fitted <- stats::predict(model)
  data$.resid <- stats::resid(model)
  data$.stdresid <- stats::rstandard(model, infl)

  data
}
