#' Scale for line patterns.
#'
#' Default line types based on a set supplied by Richard Pearson,
#' University of Manchester.  Line types can not be mapped to continuous
#' values.
#'
#' @inheritParams scale_x_discrete
#' @param na.value The linetype to use for \code{NA} values.
#' @rdname scale_linetype
#' @export
#' @examples
#' library(reshape2) # for melt
#' library(plyr) # for ddply
#' ecm <- melt(economics, id = "date")
#' rescale01 <- function(x) (x - min(x)) / diff(range(x))
#' ecm <- ddply(ecm, "variable", transform, value = rescale01(value))
#'
#' qplot(date, value, data=ecm, geom="line", group=variable)
#' qplot(date, value, data=ecm, geom="line", linetype=variable)
#' qplot(date, value, data=ecm, geom="line", colour=variable)
#'
#' # See scale_manual for more flexibility
scale_linetype <- function(..., na.value = "blank") {
  discrete_scale("linetype", "linetype_d", linetype_pal(),
    na.value = na.value, ...)
}

#' @rdname scale_linetype
#' @export
scale_linetype_continuous <- function(...) {
  stop("A continuous variable can not be mapped to linetype", call. = FALSE)
}
#' @rdname scale_linetype
#' @export
scale_linetype_discrete <- scale_linetype
