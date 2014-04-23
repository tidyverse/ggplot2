#' Single line segments.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "segment")}
#'
#' @inheritParams geom_point
#' @param arrow specification for arrow heads, as created by arrow()
#' @param lineend Line end style (round, butt, square)
#' @seealso \code{\link{geom_path}} and \code{\link{geom_line}} for multi-
#'   segment lines and paths.
#' @export
#' @examples
#' library(grid) # needed for arrow function
#' p <- ggplot(seals, aes(x = long, y = lat))
#' (p <- p + geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat),
#'   arrow = arrow(length = unit(0.1,"cm"))))
#'
#' if (require("maps")) {
#'
#' xlim <- range(seals$long)
#' ylim <- range(seals$lat)
#' usamap <- data.frame(map("world", xlim = xlim, ylim = ylim, plot =
#' FALSE)[c("x","y")])
#' usamap <- rbind(usamap, NA, data.frame(map('state', xlim = xlim, ylim
#' = ylim, plot = FALSE)[c("x","y")]))
#' names(usamap) <- c("long", "lat")
#'
#' p + geom_path(data = usamap) + scale_x_continuous(limits = xlim)
#' }
#'
#' # You can also use geom_segment to recreate plot(type = "h") :
#' counts <- as.data.frame(table(x = rpois(100,5)))
#' counts$x <- as.numeric(as.character(counts$x))
#' with(counts, plot(x, Freq, type = "h", lwd = 10))
#'
#' qplot(x, Freq, data = counts, geom = "segment",
#'   yend = 0, xend = x, size = I(10))
#'
#' # Adding line segments
#' library(grid) # needed for arrow function
#' b <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' b + geom_segment(aes(x = 2, y = 15, xend = 2, yend = 25))
#' b + geom_segment(aes(x = 2, y = 15, xend = 3, yend = 15))
#' b + geom_segment(aes(x = 5, y = 30, xend = 3.5, yend = 25),
#'    arrow = arrow(length = unit(0.5, "cm")))
geom_segment <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", arrow = NULL, lineend = "butt", na.rm = FALSE, ...) {

  GeomSegment$new(mapping = mapping, data = data, stat = stat,
    position = position, arrow = arrow, lineend = lineend, na.rm = na.rm, ...)
}

GeomSegment <- proto(Geom, {
  objname <- "segment"

  draw <- function(., data, scales, coordinates, arrow = NULL,
    lineend = "butt", na.rm = FALSE, ...) {

    data <- remove_missing(data, na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "size", "shape"),
      name = "geom_segment")
    if (empty(data)) return(zeroGrob())

    if (is.linear(coordinates)) {
      return(with(coord_transform(coordinates, data, scales),
        segmentsGrob(x, y, xend, yend, default.units="native",
        gp = gpar(col=alpha(colour, alpha), fill = alpha(colour, alpha),
          lwd=size * .pt, lty=linetype, lineend = lineend),
        arrow = arrow)
      ))
    }

    data$group <- 1:nrow(data)
    starts <- subset(data, select = c(-xend, -yend))
    ends <- rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"),
      warn_missing = FALSE)

    pieces <- rbind(starts, ends)
    pieces <- pieces[order(pieces$group),]

    GeomPath$draw_groups(pieces, scales, coordinates, arrow = arrow, ...)
  }


  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "xend", "yend")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})

