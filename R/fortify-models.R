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

#' Fortify methods for objects produced by \pkg{multcomp}
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated because using `broom::tidy()` is a better
#' solution to convert model objects.
#'
#' @param model an object of class `glht`, `confint.glht`,
#'  `summary.glht` or [multcomp::cld()]
#' @param data,... other arguments to the generic ignored in this method.
#' @name fortify-multcomp
#' @keywords internal
#' @examplesIf require("multcomp") && require("broom")
#' amod <- aov(breaks ~ wool + tension, data = warpbreaks)
#' wht <- multcomp::glht(amod, linfct = multcomp::mcp(tension = "Tukey"))
#'
#' tidy(wht) # recommended
#' fortify(wht)
#'
#' ggplot(tidy(wht), aes(contrast, estimate)) + geom_point()
#'
#' ci <- confint(wht)
#' tidy(ci) # recommended
#' fortify(ci)
#'
#' ggplot(tidy(confint(wht)),
#'        aes(contrast, estimate, ymin = conf.low, ymax = conf.high)) +
#'    geom_pointrange()
#'
#' smry <- summary(wht)
#' tidy(smry) # recommended
#' fortify(smry)
#'
#' ggplot(mapping = aes(contrast, estimate)) +
#'    geom_linerange(aes(ymin = conf.low, ymax = conf.high), data = tidy(ci)) +
#'    geom_point(aes(size = adj.p.value), data = tidy(smry)) +
#'    scale_size(transform = "reverse")
#'
#' cld <- multcomp::cld(wht)
#' tidy(cld) # recommended
#' fortify(cld)
NULL

#' @method fortify glht
#' @rdname fortify-multcomp
#' @export
fortify.glht <- function(model, data, ...) {
  lifecycle::deprecate_warn(
    "3.6.0", I("`fortify(<glht>)`"), I("`broom::tidy(<glht>)`")
  )
  base::data.frame(
    lhs = rownames(model$linfct),
    rhs = model$rhs,
    estimate = stats::coef(model),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @rdname fortify-multcomp
#' @method fortify confint.glht
#' @export
fortify.confint.glht <- function(model, data, ...) {
  lifecycle::deprecate_warn(
    "3.6.0", I("`fortify(<confint.glht>)`"), I("`broom::tidy(<confint.glht>)`")
  )
  coef <- model$confint
  colnames(coef) <- to_lower_ascii(colnames(coef))

  base::data.frame(
    lhs = rownames(coef),
    rhs = model$rhs,
    coef,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @method fortify summary.glht
#' @rdname fortify-multcomp
#' @export
fortify.summary.glht <- function(model, data, ...) {
  lifecycle::deprecate_warn(
    "3.6.0", I("`fortify(<summary.glht>)`"), I("`broom::tidy(<summary.glht>)`")
  )
  coef <- as.data.frame(
    model$test[c("coefficients", "sigma", "tstat", "pvalues")])
  names(coef) <- c("estimate", "se", "t", "p")

  base::data.frame(
    lhs = rownames(coef),
    rhs = model$rhs,
    coef,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


#' @method fortify cld
#' @rdname fortify-multcomp
#' @export
fortify.cld <- function(model, data, ...) {
  lifecycle::deprecate_warn(
    "3.6.0", I("`fortify(<summary.glht>)`"), I("`broom::tidy(<summary.glht>)`")
  )
  base::data.frame(
    lhs = names(model$mcletters$Letters),
    letters = model$mcletters$Letters,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
