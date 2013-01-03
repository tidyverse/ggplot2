#' Set a Geom's Parameters of an Existing ggplot2 Object
#'
#' Changes or sets a geom's parameters after a ggplot2 object has been created. This
#' function applies to all layers with a given geom type.
#'
#' @param plot_object the ggplot2 object to work on
#' @param geom name of the geom to set the parameter for (e.g. "ribbon", "point")
#' @param param_name name of parameter to set
#' @param param_value desired value of the parameter
#' @examples
#' p <- ggplot(data.frame(a=1:10, b=1:10), aes(x=a, y=b)) + geom_point() + geom_line()
#' p_set <- set_geom_params(p, "point", 'size', 10)
#' stopifnot(p_set$layers[[1]]$geom_params$size == 10)
#' @export
set_geom_params <- function(plot_object, geom, param_name, param_value) {
  
  plot_object$layers <- lapply(
    plot_object$layers, 
    function(x) {
      if(x$geom$objname == geom) 
        x$geom_params[param_name] <- param_value

      return(x)
    }
  )

  plot_object
}
