#' Scale for line patterns.
#' 
#' Default line types based on a set supplied by Richard Pearson, 
#' University of Manchester.  Line types can not be mapped to continuous
#' values.
#'
#' @inheritParams scale_x_discrete
#' @rdname scale_linetype
#' @export
#' @examples
#' library(reshape2)
#' library(plyr)
#' ecm <- melt(economics, id = "date")
#' rescale01 <- function(x) (x - min(x)) / diff(range(x))
#' ecm <- ddply(ecm, "variable", transform, value = rescale01(value))
#' 
#' qplot(date, value, data=ecm, geom="line", group=variable)
#' qplot(date, value, data=ecm, geom="line", linetype=variable)
#' qplot(date, value, data=ecm, geom="line", colour=variable)
#' 
#' # See scale_manual for more flexibility
scale_linetype <- function(...) {
  discrete_scale("linetype", "linetype_d", linetype_pal(), ...)
}

#' @rdname scale_linetype
#' @export
scale_linetype_continuous <- function(...) {
  stop("A continuous variable can not be mapped to linetype", call. = FALSE)
}
#' @rdname scale_linetype
#' @export
scale_linetype_discrete <- scale_linetype
 
icon.linetype <- function(.) {
  gTree(children=gList(
    segmentsGrob(0, 0.25, 1, 0.25, gp=gpar(lty=1)),
    segmentsGrob(0, 0.50, 1, 0.50, gp=gpar(lty=2)),
    segmentsGrob(0, 0.75, 1, 0.75, gp=gpar(lty=3))
  ))
}
