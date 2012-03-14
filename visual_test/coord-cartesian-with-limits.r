## test codes of coord_cartesion with range specification.

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() 

pl <- list()
pl[[1]] <- p + coord_cartesian(xlim = c(0, 10), ylim = c(0, 50), wise = TRUE) + opts(title="wise=TRUE")
pl[[2]] <- p + coord_cartesian(xlim = c(0, 10), ylim = c(0, 50), wise = FALSE) + opts(title="wise=FALSE")
pl[[3]] <- p + coord_cartesian(xlim = c(2, 4), ylim = c(20, 40), wise = TRUE) + opts(title="wise=TRUE")
pl[[4]] <- p + coord_cartesian(xlim = c(2, 4), ylim = c(20, 40), wise = FALSE) + opts(title="wise=FALSE")
