ggplot <- function(data, ...) UseMethod("ggplot")

# Create a new plot
# Create a new ggplot plot
# 
# @alias package-ggplot
# @alias ggplot
# @arguments default data frame
# @arguments default list of aesthetic mappings (these can be colour, size, shape, line type -- see individual geom functions for more details)
# @seealso \url{http://had.co.nz/ggplot2}
# @keyword hplot
ggplot.default <- function(data = NULL, mapping=aes(), ..., environment = globalenv()) {
  if (!is.null(data) && !is.data.frame(data)) stop("Data needs to be a data.frame")
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

  p$scales$add_defaults(p$data, p$mapping, p$plot_env)

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