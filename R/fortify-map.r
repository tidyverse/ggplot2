#' Fortify method for map objects.
#' 
#' This function turns a map into a data frame than can more easily be
#' plotted with ggplot2.
#' 
#' @method fortify map
#' @S3method fortify map
#' @seealso \code{\link{map_data}} and \code{\link{border}}
#' @param model map object
#' @param data not used by this method
#' @param ... not used by this method
#' @examples
#' if (require("maps")) {
#' ca <- map("county", "ca", plot = FALSE, fill = TRUE)
#' head(fortify(ca))
#' qplot(long, lat, data = ca, geom = "polygon", group = group)
#'
#' tx <- map("county", "texas", plot = FALSE, fill = TRUE)
#' head(fortify(tx))
#' qplot(long, lat, data = tx, geom = "polygon", group = group, 
#'  colour = I("white"))
#' }
fortify.map <- function(model, data, ...) {
  df <- as.data.frame(model[c("x", "y")])
  names(df) <- c("long", "lat")
  df$group <- cumsum(is.na(df$long) & is.na(df$lat)) + 1
  df$order <- 1:nrow(df)
  
  names <- do.call("rbind", lapply(strsplit(model$names, "[:,]"), "[", 1:2))
  df$region <- names[df$group, 1]
  df$subregion <- names[df$group, 2]
  df[complete.cases(df$lat, df$long), ]
}

#' Create a data frame of map data.
#' 
#' @param map name of map provided by \code{\link{maps}} package.  These 
#'   include \code{\link{county}}, \code{\link{france}}, \code{\link{italy}},
#'   \code{\link{nz}}, \code{\link{state}}, \code{\link{usa}}, 
#'   \code{\link{world}}, \code{\link{world2}}.
#' @param region name of subregions to include.  Defaults to \code{.} which
#'   includes all subregion.  See documentation for \code{\link{map}} for
#'   more details.
#' @param exact should the \code{region} be treated as a regular expression
#'   (\code{FALSE}) or as a fixed string (\code{TRUE}).
#' @param ... all other arguments passed on to \code{\link[maps]{map}}
#' @export
#' @examples
#' if (require("maps")) {
#' states <- map_data("state")
#' arrests <- USArrests
#' names(arrests) <- tolower(names(arrests))
#' arrests$region <- tolower(rownames(USArrests))
#' 
#' choro <- merge(states, arrests, sort = FALSE, by = "region")
#' choro <- choro[order(choro$order), ]
#' qplot(long, lat, data = choro, group = group, fill = assault,
#'   geom = "polygon")
#' qplot(long, lat, data = choro, group = group, fill = assault / murder,
#'   geom = "polygon")
#' }
map_data <- function(map, region = ".", exact = FALSE, ...) {
  fortify(map(map, region, exact = exact, plot = FALSE, fill = TRUE, ...))
}
