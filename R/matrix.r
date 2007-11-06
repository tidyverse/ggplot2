# Code to create a scatterplot matrix (experimental)
# Crude experimental scatterplot matrix
# 
# @arguments data frame
# @arguments any additional aesthetic mappings (do not use x and y)
# @keyword hplot
#X plotmatrix(mtcars)
#X plotmatrix(mtcars, aes(colour=factor(cyl)))
#X plotmatrix(mtcars) + geom_smooth(method="lm")
#X plotmatrix(mtcars, aes(colour=factor(cyl))) 
plotmatrix <- function(data, mapping=aes()) {
  data <- rescaler(data, "range")
  grid <- expand.grid(x=1:ncol(data), y=1:ncol(data))
  grid <- subset(grid, x != y)

  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]

    data.frame(
      xvar = names(data)[xcol], 
      yvar = names(data)[ycol],
      x = data[, xcol], y = data[, ycol], data
    )
  }))
  all$xvar <- factor(all$xvar, levels=names(data))
  all$yvar <- factor(all$yvar, levels=names(data))

  # densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
  #   data.frame(
  #     xvar = names(data)[i], yvar=names(data)[i],
  #     x = data[, i]
  #   )
  # }))
  mapping <- defaults(mapping, aes(x=x, y=y))
  class(mapping) <- "uneval"

  ggplot(all, mapping) + facet_grid(xvar ~ yvar) +
    geom_point() +
    scale_x_continuous(NULL, limits=c(0, 1)) +
    scale_y_continuous(NULL, limits=c(0, 1))
    # +
    # geom_density(aes(x=x, max = ..scaled.., min = 0), data=densities, position="identity")
}

