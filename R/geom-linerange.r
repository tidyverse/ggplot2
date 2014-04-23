#' An interval represented by a vertical line.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "linerange")}
#'
#' @seealso \code{\link{geom_errorbar}}: error bars;
#'   \code{\link{geom_pointrange}}: range indicated by straight line, with
#'   point in the middle; \code{\link{geom_crossbar}}: hollow bar with middle
#'   indicated by horizontal line; \code{\link{stat_summary}}: examples of
#'   these guys in use; \code{\link{geom_smooth}}: for continuous analog
#' @export
#' @inheritParams geom_point
#' @examples
#' # Generate data: means and standard errors of means for prices
#' # for each type of cut
#' dmod <- lm(price ~ cut, data=diamonds)
#' cuts <- data.frame(cut = unique(diamonds$cut),
#'   predict(dmod, data.frame(cut = unique(diamonds$cut)), se=TRUE)[c("fit","se.fit")])
#'
#' qplot(cut, fit, data=cuts)
#' # With a bar chart, we are comparing lengths, so the y-axis is
#' # automatically extended to include 0
#' qplot(cut, fit, data=cuts, geom="bar")
#'
#' # Display estimates and standard errors in various ways
#' se <- ggplot(cuts, aes(cut, fit,
#'   ymin = fit - se.fit, ymax=fit + se.fit, colour = cut))
#' se + geom_linerange()
#' se + geom_pointrange()
#' se + geom_errorbar(width = 0.5)
#' se + geom_crossbar(width = 0.5)
#'
#' # Use coord_flip to flip the x and y axes
#' se + geom_linerange() + coord_flip()
geom_linerange <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) {
  GeomLinerange$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

GeomLinerange <- proto(Geom, {
  objname <- "linerange"

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
  required_aes <- c("x", "ymin", "ymax")

  draw <- function(., data, scales, coordinates, ...) {
    munched <- coord_transform(coordinates, data, scales)
    ggname(.$my_name(), GeomSegment$draw(transform(data, xend=x, y=ymin, yend=ymax), scales, coordinates, ...))
  }

})
