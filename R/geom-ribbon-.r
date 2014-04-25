#' Ribbons, y range with continuous x values.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "ribbon")}
#'
#' @seealso
#'   \code{\link{geom_bar}} for discrete intervals (bars),
#'   \code{\link{geom_linerange}} for discrete intervals (lines),
#'   \code{\link{geom_polygon}} for general polygons"
#' @inheritParams geom_point
#' @export
#' @examples
#' \donttest{
#' # Generate data
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#' library(plyr) # to access round_any
#' huron$decade <- round_any(huron$year, 10, floor)
#'
#' h <- ggplot(huron, aes(x=year))
#'
#' h + geom_ribbon(aes(ymin=0, ymax=level))
#' h + geom_area(aes(y = level))
#'
#' # Add aesthetic mappings
#' h + geom_ribbon(aes(ymin=level-1, ymax=level+1))
#' h + geom_ribbon(aes(ymin=level-1, ymax=level+1)) + geom_line(aes(y=level))
#'
#' # Take out some values in the middle for an example of NA handling
#' huron[huron$year > 1900 & huron$year < 1910, "level"] <- NA
#' h <- ggplot(huron, aes(x=year))
#' h + geom_ribbon(aes(ymin=level-1, ymax=level+1)) + geom_line(aes(y=level))
#'
#' # Another data set, with multiple y's for each x
#' m <- ggplot(movies, aes(y=votes, x=year))
#' (m <- m + geom_point())
#'
#' # The default summary isn't that useful
#' m + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max")
#' m + stat_summary(geom="ribbon", fun.data="median_hilow")
#'
#' # Use qplot instead
#' qplot(year, level, data=huron, geom=c("area", "line"))
#' }
geom_ribbon <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
na.rm = FALSE, ...) {
  GeomRibbon$new(mapping = mapping, data = data, stat = stat, position = position,
  na.rm = na.rm, ...)
}

GeomRibbon <- proto(Geom, {
  objname <- "ribbon"

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, alpha = NA)
  required_aes <- c("x", "ymin", "ymax")
  guide_geom <- function(.) "polygon"


  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {
    if (na.rm) data <- data[complete.cases(data[required_aes]), ]
    data <- data[order(data$group, data$x), ]

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
    if (nrow(aes) > 1) {
      stop("Aesthetics can not vary with a ribbon")
    }
    aes <- as.list(aes)

    # Instead of removing NA values from the data and plotting a single
    # polygon, we want to "stop" plotting the polygon whenever we're
    # missing values and "start" a new polygon as soon as we have new
    # values.  We do this by creating an id vector for polygonGrob that
    # has distinct polygon numbers for sequences of non-NA values and NA
    # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
    # 4, 4, 4, NA)
    missing_pos <- !complete.cases(data[required_aes])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA

    positions <- summarise(data,
      x = c(x, rev(x)), y = c(ymax, rev(ymin)), id = c(ids, rev(ids)))
    munched <- coord_munch(coordinates,positions, scales)

    ggname(.$my_name(), polygonGrob(
      munched$x, munched$y, id = munched$id,
      default.units = "native",
      gp = gpar(
        fill = alpha(aes$fill, aes$alpha),
        col = aes$colour,
        lwd = aes$size * .pt,
        lty = aes$linetype)
    ))
  }

})

#' Area plot.
#'
#' An area plot is the continuous analog of a stacked bar chart (see
#' \code{\link{geom_bar}}), and can be used to show how composition of the
#' whole varies over the range of x.  Choosing the order in which different
#' components is stacked is very important, as it becomes increasing hard to
#' see the individual pattern as you move up the stack.
#'
#' An area plot is a special case of \code{\link{geom_ribbon}}, where the
#' minimum of the range is fixed to 0, and the position adjustment defaults
#' to position_stacked.
#'
#' @inheritParams geom_point
#' @export
#' @examples
#' # see geom_ribbon
geom_area <- function (mapping = NULL, data = NULL, stat = "identity", position = "stack",
na.rm = FALSE, ...) {
  GeomArea$new(mapping = mapping, data = data, stat = stat, position = position,
  na.rm = na.rm, ...)
}

GeomArea <- proto(GeomRibbon,{
  objname <- "area"

  default_aes <- function(.) aes(colour=NA, fill="grey20", size=0.5, linetype=1, alpha = NA)
  default_pos <- function(.) PositionStack
  required_aes <- c("x", "y")

  reparameterise <- function(., df, params) {
    transform(df, ymin = 0, ymax = y)
  }

})
