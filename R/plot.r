ggplot <- function(data, ...) UseMethod("ggplot")

# Create a new plot
# Create a new ggplot plot
# 
# @alias package-ggplot
# @alias ggplot
# @arguments default data frame
# @arguments default list of aesthetic mappings (these can be colour, size, shape, line type -- see individual geom functions for more details)
# @seealso \url{http://had.co.nz/ggplot/ggplot.html}
# @keyword hplot
ggplot.default <- function(data = NULL, mapping=aes(), ...) {
  if (!is.null(data) && !is.data.frame(data)) stop("Data needs to be a data.frame")
  if (!missing(mapping) && !inherits(mapping, "uneval")) stop("Mapping should be created with aes or aes_string")
  
  p <- structure(list(
    data = data, 
    layers = list(),
    scales = Scales$new(),
    defaults = mapping,
    title = NULL
  ), class="ggplot")
  p$coordinates <- CoordCartesian$new()
  p$facet <- FacetGrid$new()
  p$scales$add_defaults(p$data, p$defaults)

  set_last_plot(p)
  p
}


plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()
  p$layers <- lapply(plot$layers, function(x) x$clone())
  
  p
}