#' Connect observations, ordered by x value.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "line")}
#'
#' @seealso \code{\link{geom_path}}: connect observations in data order,
#'  \code{\link{geom_segment}}: draw line segments,
#'  \code{\link{geom_ribbon}}: fill between line and x-axis
#' @inheritParams geom_point
#' @export
#' @examples
#' # Summarise number of movie ratings by year of movie
#' if (require("ggplot2movies")) {
#' mry <- do.call(rbind, by(movies, round(movies$rating), function(df) {
#'   nums <- tapply(df$length, df$year, length)
#'   data.frame(rating=round(df$rating[1]), year = as.numeric(names(nums)), number=as.vector(nums))
#' }))
#'
#' p <- ggplot(mry, aes(x=year, y=number, group=rating))
#' p + geom_line()
#'
#' # Add aesthetic mappings
#' p + geom_line(aes(size = rating))
#' p + geom_line(aes(colour = rating))
#'
#' # Change scale
#' p + geom_line(aes(colour = rating)) + scale_colour_gradient(low="red")
#' p + geom_line(aes(size = rating)) + scale_size(range = c(0.1, 3))
#'
#' # Set aesthetics to fixed value
#' p + geom_line(colour = "red", size = 1)
#' }
#'
#' # Using a time series
#' ggplot(economics, aes(date, pop)) + geom_line()
#' ggplot(economics, aes(date, pop)) +
#'   geom_line() +
#'   scale_y_log10()
#' ggplot(subset(economics, date > as.Date("2006-1-1")), aes(date, pop)) +
#'   geom_line()
#' ggplot(economics, aes(date, pop)) +
#'   geom_line(aes(size = unemploy/pop))
#'
#' # Use the arrow parameter to add an arrow to the line
#' # See ?grid::arrow for more details
#' c <- ggplot(economics, aes(x = date, y = pop))
#' # Arrow defaults to "last"
#' library(grid)
#' c + geom_line(arrow = arrow())
#' c + geom_line(arrow = arrow(angle = 15, ends = "both", type = "closed"))
#'
#' # See scale_date for examples of plotting multiple times series on
#' # a single graph
#'
#' # A simple pcp example
#'
#' y2005 <- runif(300, 20, 120)
#' y2010 <- y2005 * runif(300, -1.05, 1.5)
#' group <- rep(LETTERS[1:3], each = 100)
#'
#' df <- data.frame(id = seq_along(group), group, y2005, y2010)
#' library(reshape2) # for melt
#' dfm <- melt(df, id.var = c("id", "group"))
#' ggplot(dfm, aes(variable, value, group = id, colour = group)) +
#'   geom_path(alpha = 0.5)
geom_line <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", show_guide = NA, inherit.aes = TRUE, ...)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLine,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

GeomLine <- ggproto("GeomLine", GeomPath,
  draw = function(data, scales, coordinates, arrow = NULL, ...) {
    data <- data[order(data$group, data$x), ]
    GeomPath$draw(data, scales, coordinates, arrow, ...)
  }
)
