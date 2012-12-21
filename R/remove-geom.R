#' Remove a Geom From a ggplot2 Object
#'
#' remove_geom allows you to remove layers of from a ggplot2 object
#' by specifying the name of a geom to remove. This can be useful 
#' when dynamically building ggplot2 objects and want to remove
#' cluttered layers that have already been added to the object in
#' previous function calls.
#'
#' @param plot_object the ggplot2 object to work on
#' @param geom name of geom to remove
#' @examples
#' p <- ggplot(data.frame(a=1:10, b=1:10), aes(x=a, y=b)) + geom_point() + geom_line()
#' p_minus_point <- remove_geom(p, "point")
#' stopifnot(
#'   setdiff(
#'     sapply(p$layers, function(x) x$geom$objname), 
#'     sapply(p_minus_point$layers, function(x) x$geom$objname)
#'   ) == "point"
#' )
#' @export
remove_geom <- function(plot_object, geom) {
  
  layers <- lapply(plot_object$layers, function(x) if(x$geom$objname == geom) NULL else x)
  layers <- layers[!sapply(layers, is.null)]

  plot_object$layers <- layers
  plot_object
}
