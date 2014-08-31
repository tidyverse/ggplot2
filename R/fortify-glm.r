#' Supplement the data fitted to a generalized linear model with
#'  model fit statistics.
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
#' @param type the type of prediction required for fitted values.
#' See \code{link{predict.glm}}
#' @param ... not used by this method
#' @export
#' @examples
#'
#' ## Dobson (1990) Page 93: Randomized Controlled Trial :
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' d.AD <- data.frame(treatment, outcome, counts)
#' glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' head(fortify(glm.D93))
#' ## .fitted defaults to type = "link"
#' head(fortify(glm.D93, type = "response"))
#'
#' qplot(.fitted, .resid, data = mod) +
#'   geom_hline(yintercept = 0) +
#'   geom_smooth(se = FALSE)
#'
#'## Defaults make no sense to compare fitted vs actual values
#' qplot(.fitted, counts, data = mod) +
#'   geom_smooth(se = FALSE)
#'
#' qplot(.fitted, counts, data = fortify(mod, type = "response")) +
#'   geom_smooth(se = FALSE)
#'
fortify.glm <- function (model, data = model$model, type = "link", ...)
{
  infl <- influence(model, do.coef = FALSE)
  data$.hat <- infl$hat
  data$.sigma <- infl$sigma
  data$.cooksd <- cooks.distance(model, infl)
  data$.fitted <- predict(model, type = type)
  data$.resid <- resid(model)
  data$.stdresid <- rstandard(model, infl)
  data
}
