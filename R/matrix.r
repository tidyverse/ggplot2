# Code to create a scatterplot matrix (experimental)
# Crude experimental scatterplot matrix
# 
# @arguments data frame
# @arguments any additional aesthetic mappings (do not use x and y)
# @keyword hplot
#X plotmatrix(mtcars[, 1:3])
#X plotmatrix(mtcars[, 1:3]) + geom_smooth(method="lm")
plotmatrix <- function(data, mapping=aes(), colour="black") {
  data <- rescaler(data, "range")
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

  ggplot(all, mapping) + facet_grid(xvar ~ yvar) +
    geom_point(colour = colour) +
    scale_x_continuous("", limits=c(0, 1), breaks = seq(0,1, length=4), labels = "") +
    scale_y_continuous("", limits=c(0, 1), breaks = seq(0,1, length=4), labels = "") + 
    stat_density(aes_string(x="x", y = "..scaled.."), data=densities, position="identity", fill="grey60", colour=NA)
}

