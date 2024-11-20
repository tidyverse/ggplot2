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
