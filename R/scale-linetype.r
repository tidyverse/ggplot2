#' Scale for line patterns.
#' 
#' Default line types based on a set supplied by Richard Pearson, 
#' University of Manchester.
#'
#' @export
#' @examples
#' ec_scaled <- data.frame(
#'   date = economics$date, 
#'   rescaler(economics[, -(1:2)], "range")
#' )
#' ecm <- melt(ec_scaled, id = "date")
#' 
#' qplot(date, value, data=ecm, geom="line", group=variable)
#' qplot(date, value, data=ecm, geom="line", linetype=variable)
#' qplot(date, value, data=ecm, geom="line", colour=variable)
#' 
#' # See scale_manual for more flexibility
scale_linetype <- function(...) {
  discrete_scale("linetype", "linetype_d", linetype_pal())
}
scale_linetype_continuous <- function(...) {
  stop("A continuous variable can not be mapped to linetype", call. = FALSE)
}
scale_linetype_discrete <- scale_linetype
 
icon.linetype <- function(.) {
  gTree(children=gList(
    segmentsGrob(0, 0.25, 1, 0.25, gp=gpar(lty=1)),
    segmentsGrob(0, 0.50, 1, 0.50, gp=gpar(lty=2)),
    segmentsGrob(0, 0.75, 1, 0.75, gp=gpar(lty=3))
  ))
}
