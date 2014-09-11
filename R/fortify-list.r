#' Fortify method for x, y, z lists.
#'
#' This function turns a list with components x, y, z, such as those used
#' by \code{\link[graphics]{image}} and \code{\link[graphics]{image}} into
#' a data frame that can more easily be plotted with ggplot2.
#'
#' @export
#' @param model list with components x, y and z
#' @param data not used by this method
#' @param ... not used by this method
#' @examples
#' x <- seq(0, 1, by=0.1)
#' y <- seq(0, 2, by=0.1)
#' z <- outer(x, y, "+") 
#' d <- list(x=x, y=y, z=z)
#' image(d)
#' persp(d)
#' head(fortify(d))
#' ggplot(d) + geom_tile(aes(x=x, y=y, fill=z))
fortify.list <- function(model, data, ...) {
  if ( ! all(names(model) == c("x", "y", "z")) ) {
    stop("ggplot only knows how to deal lists with components x, y, and z, as is suitable for image() or persp()")
  }
  if ( ! is.matrix(model$z) ) {
    stop("element z of the list needs to be a matrix")
  }
  if ( length(model$x) != nrow(model$z) ) {
    stop("element x of the list needs to be the same length at the number of rows of element z")
  }
  if ( length(model$y) != ncol(model$z) ) {
    stop("element y of the list needs to be the same length at the number of columns of element z")
  }
  # convert to data.frame
  d <- melt(model$z)
  names(d) <- c("x", "y", "z")
  # get coordinates
  d$x <- model$x[d$x]
  d$y <- model$y[d$y]
  return(d)
}
