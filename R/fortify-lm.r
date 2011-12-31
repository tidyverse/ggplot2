#' Supplement the data fitted to a linear model with model fit statistics.
#'
#' If you have missing values in your model data, you may need to refit 
#' the model with \code{na.action = na.exclude}.
#'  
#' 
#' @return The original data with extra columns:
#'   \item{.hat}{Diagonal of the hat matrix}
#'   \item{.sigma}{Estimate of residual standard deviation when
#'     corresponding observation is dropped from model}
#'   \item{.cooksd}{Cooks distance, \code{\link{cooks.distance}}}
#'   \item{.fitted}{Fitted values of model}
#'   \item{.resid}{Residuals}
#'   \item{.stdresid}{Standardised residuals}
#' @param model linear model
#' @param data data set, defaults to data used to fit model
#' @param ... not used by this method
#' @method fortify lm
#' @S3method fortify lm
#' @examples
#'
#' mod <- lm(mpg ~ wt, data = mtcars)
#' head(fortify(mod))
#' head(fortify(mod, mtcars))
#' 
#' plot(mod, which = 1)
#' qplot(.fitted, .resid, data = mod) + 
#'   geom_hline(yintercept = 0) + 
#'   geom_smooth(se = FALSE)
#' qplot(.fitted, .stdresid, data = mod) + 
#'   geom_hline(yintercept = 0) +
#'   geom_smooth(se = FALSE)
#' qplot(.fitted, .stdresid, data = fortify(mod, mtcars), 
#'   colour = factor(cyl))
#' qplot(mpg, .stdresid, data = fortify(mod, mtcars), colour = factor(cyl))
#' 
#' plot(mod, which = 2)
#' # qplot(sample =.stdresid, data = mod, stat = "qq") + geom_abline()
#' 
#' plot(mod, which = 3)
#' qplot(.fitted, sqrt(abs(.stdresid)), data = mod) + geom_smooth(se = FALSE)
#' 
#' plot(mod, which = 4)
#' qplot(seq_along(.cooksd), .cooksd, data = mod, geom = "bar",
#'  stat="identity")
#' 
#' plot(mod, which = 5)
#' qplot(.hat, .stdresid, data = mod) + geom_smooth(se = FALSE)
#' ggplot(mod, aes(.hat, .stdresid)) + 
#'   geom_vline(size = 2, colour = "white", xintercept = 0) +
#'   geom_hline(size = 2, colour = "white", yintercept = 0) +
#'   geom_point() + geom_smooth(se = FALSE)
#' 
#' qplot(.hat, .stdresid, data = mod, size = .cooksd) + 
#'   geom_smooth(se = FALSE, size = 0.5)
#' 
#' plot(mod, which = 6)
#' ggplot(mod, aes(.hat, .cooksd)) + 
#'   geom_vline(xintercept = 0, colour = NA) + 
#'   geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") +
#'   geom_smooth(se = FALSE) + 
#'   geom_point()
#' qplot(.hat, .cooksd, size = .cooksd / .hat, data = mod) + scale_area()
fortify.lm <- function(model, data = model$model, ...) {
  infl <- influence(model, do.coef = FALSE)
  data$.hat <- infl$hat
  data$.sigma <- infl$sigma 
  data$.cooksd <- cooks.distance(model, infl)

  data$.fitted <- predict(model)
  data$.resid <- resid(model)
  data$.stdresid <- rstandard(model, infl)

  data
}
