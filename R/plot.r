# Create a new plot
# Create a new ggplot plot
# 
# @seealso \url{http://had.co.nz/ggplot2}
# @alias ggplot.default
# @keyword hplot
# @arguments default data set
# @arguments other arguments passed to specific methods
ggplot <- function(data = NULL, ...) UseMethod("ggplot")

ggplot.default <- function(data = NULL, mapping = aes(), ...) {
  ggplot.data.frame(fortify(data), mapping, ...)
}

# Create a new plot
# Create a new ggplot plot
# 
# @alias package-ggplot
# @arguments default data frame
# @arguments default list of aesthetic mappings (these can be colour, size, shape, line type -- see individual geom functions for more details)
# @arguments ignored
# @arguments environment in which evaluation of aesthetics should occur
# @seealso \url{http://had.co.nz/ggplot2}
# @alias package-ggplot
# @keyword hplot
ggplot.data.frame <- function(data, mapping=aes(), ..., environment = globalenv()) {
  if (!missing(mapping) && !inherits(mapping, "uneval")) stop("Mapping should be created with aes or aes_string")
  
  p <- structure(list(
    data = data, 
    layers = list(),
    scales = Scales$new(),
    mapping = mapping,
    options = list(),
    coordinates = CoordCartesian$new(),
    facet = FacetGrid$new(),
    plot_env = environment
  ), class="ggplot")

  p$options$labels <- as.list(as.character(mapping))

  set_last_plot(p)
  p
}


plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()
  p$layers <- lapply(plot$layers, function(x) x$clone())
  p$facet <- plot$facet$clone()
  
  p
}