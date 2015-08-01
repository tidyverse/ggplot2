#' Fortify methods for objects produced by \pkg{multcomp}
#'
#' @param model an object of class \code{glht}, \code{confint.glht},
#'  \code{summary.glht} or \code{\link[multcomp]{cld}}
#' @param data,... other arguments to the generic ignored in this method.
#' @name fortify-multcomp
#' @examples
#' if (require("multcomp")) {
#' amod <- aov(breaks ~ wool + tension, data = warpbreaks)
#' wht <- glht(amod, linfct = mcp(tension = "Tukey"))
#'
#' fortify(wht)
#' ggplot(wht, aes(lhs, estimate)) + geom_point()
#'
#' CI <- confint(wht)
#' fortify(CI)
#' ggplot(CI, aes(lhs, estimate, ymin = lwr, ymax = upr)) +
#'    geom_pointrange()
#'
#' fortify(summary(wht))
#' ggplot(mapping = aes(lhs, estimate)) +
#'    geom_linerange(aes(ymin = lwr, ymax = upr), data = CI) +
#'    geom_point(aes(size = p), data = summary(wht)) +
#'    scale_size(trans = "reverse")
#'
#' cld <- cld(wht)
#' fortify(cld)
#' }
NULL

#' @method fortify glht
#' @rdname fortify-multcomp
#' @export
fortify.glht <- function(model, data, ...) {
  plyr::unrowname(data.frame(
    lhs = rownames(model$linfct),
    rhs = model$rhs,
    estimate = stats::coef(model),
    check.names = FALSE,
    stringsAsFactors = FALSE))
}

#' @rdname fortify-multcomp
#' @method fortify confint.glht
#' @export
fortify.confint.glht <- function(model, data, ...) {
  coef <- model$confint
  colnames(coef) <- tolower(colnames(coef))

  plyr::unrowname(data.frame(
    lhs = rownames(coef),
    rhs = model$rhs,
    coef,
    check.names = FALSE,
    stringsAsFactors = FALSE))
}

#' @method fortify summary.glht
#' @rdname fortify-multcomp
#' @export
fortify.summary.glht <- function(model, data, ...) {
  coef <- as.data.frame(
    model$test[c("coefficients", "sigma", "tstat", "pvalues")])
  names(coef) <- c("estimate", "se", "t", "p")

  plyr::unrowname(data.frame(
    lhs = rownames(coef),
    rhs = model$rhs,
    coef,
    check.names = FALSE,
    stringsAsFactors = FALSE))
}


#' @method fortify cld
#' @rdname fortify-multcomp
#' @export
fortify.cld <- function(model, data, ...) {
  plyr::unrowname(data.frame(
    lhs = names(model$mcletters$Letters),
    letters = model$mcletters$Letters,
    check.names = FALSE,
    stringsAsFactors = FALSE))
}
