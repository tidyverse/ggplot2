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

  (.last_plot <<- p)
}


# Print ggplot
# Print generic for ggplot.  Plot on current graphics device.
#
# @arguments plot to display
# @arguments draw new (empty) page first?
# @arguments viewport to draw plot in
# @arguments other arguments passed on to \\code{\\link{ggplot_plot}}
# @keyword hplot
# @keyword internal 
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, save=ggopt()$save, ...) {
  if (save) {
    try_require("decumar")
    img(grid.draw(ggplot_plot(x, ...)), hash=digest.ggplot(x))
    return()
  }
  
  if (newpage) grid.newpage()
  if (is.null(vp)) {
    grid.draw(ggplot_plot(x, ...)) 
  } else {
    if (is.character(vp)) seekViewport(vp) else pushViewport(vp)
    grid.draw(ggplot_plot(x, ...)) 
    upViewport()
  }
}

