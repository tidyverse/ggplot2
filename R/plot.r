#' Create a new ggplot plot
#' 
#' @seealso \url{http://had.co.nz/ggplot2}
#' @export
#' @S3method ggplot default
#' @keywords hplot
#' @param data default data set
#' @param ... other arguments passed to specific methods
ggplot <- function(data = NULL, ...) UseMethod("ggplot")

ggplot.default <- function(data = NULL, mapping = aes(), ...) {
  ggplot.data.frame(fortify(data, ...), mapping)
}

#' Create a new ggplot plot from a data frame
#' 
#' @param data default data frame for plot
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param ... ignored
#' @param environment in which evaluation of aesthetics should occur
#' @seealso \url{http://had.co.nz/ggplot2}
#' @method ggplot data.frame
#' @S3method ggplot data.frame
ggplot.data.frame <- function(data, mapping=aes(), ..., environment = globalenv()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) stop("Mapping should be created with aes or aes_string")
  
  p <- structure(list(
    data = data, 
    layers = list(),
    scales = Scales$new(),
    mapping = mapping,
    options = list(),
    coordinates = coord_cartesian(),
    facet = facet_null(),
    plot_env = environment
  ), class="ggplot")
  
  p$options$labels <- make_labels(mapping)

  set_last_plot(p)
  p
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()
  p$layers <- lapply(plot$layers, function(x) x$clone())
  
  p
}
