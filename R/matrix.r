#' Code to create a scatterplot matrix (experimental)
#' 
#' @param data data frame
#' @param mapping any additional aesthetic mappings (do not use x and y)
#' @param colour default point colour
#' @keywords hplot
#' @export
#' @examples
#' plotmatrix(mtcars[, 1:3])
#' plotmatrix(mtcars[, 1:3]) + geom_smooth(method="lm")
plotmatrix <- function(data, mapping=aes(), colour="black") {
  # data <- rescaler(data, "range")
  grid <- expand.grid(x=1:ncol(data), y=1:ncol(data))
  grid <- subset(grid, x != y)

  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]

    data.frame(
      xvar = names(data)[ycol], 
      yvar = names(data)[xcol],
      x = data[, xcol], y = data[, ycol], data
    )
  }))
  all$xvar <- factor(all$xvar, levels=names(data))
  all$yvar <- factor(all$yvar, levels=names(data))

  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(
      xvar = names(data)[i], 
      yvar = names(data)[i],
      x = data[, i]
    )
  }))
  mapping <- defaults(mapping, aes_string(x="x", y="y"))
  class(mapping) <- "uneval"

  ggplot(all, mapping) + facet_grid(xvar ~ yvar, scales = "free") +
    geom_point(colour = colour, na.rm = TRUE) +
    stat_density(
      aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)),
      data = densities, position ="identity", colour = "grey20", geom = "line"
    )
}

