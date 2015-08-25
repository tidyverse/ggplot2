#' Build ggplot for rendering.
#'
#' \code{ggplot_build} takes the plot object, and performs all steps necessary
#' to produce an object that can be rendered.  This function outputs two pieces:
#' a list of data frames (one for each layer), and a panel object, which
#' contain all information about axis limits, breaks etc. \code{layer_data}
#' is a helper function which returns the data for a given layer.
#'
#' @param plot ggplot object
#' @seealso \code{\link{print.ggplot}} and \code{\link{benchplot}} for
#'  for functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @keywords internal
#' @export
ggplot_build <- function(plot) {
  plot <- plot_clone(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }

  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)

  scales <- plot$scales
  # Apply function to layer and matching data
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data

  panel <- new_panel()
  panel <- train_layout(panel, plot$facet, layer_data, plot$data)
  data <- map_layout(panel, plot$facet, layer_data, plot$data)

  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))
  data <- lapply(data, add_group)

  # Transform all scales
  data <- lapply(data, scales_transform_df, scales = scales)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  panel <- train_position(panel, data, scale_x(), scale_y())
  data <- map_position(panel, data, scale_x(), scale_y())

  # Apply and map statistics
  data <- by_layer(function(l, d) l$compute_statistic(d, panel))
  data <- by_layer(function(l, d) l$map_statistic(d, plot))
  data <- lapply(data, order_groups)

  # Make sure missing (but required) aesthetics are added
  scales_add_missing(plot, c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- by_layer(function(l, d) l$compute_geom(d))

  # Apply position adjustments
  data <- by_layer(function(l, d) l$compute_position(d, panel))

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's
  # displayed, or does it include the range of underlying data
  reset_scales(panel)
  panel <- train_position(panel, data, scale_x(), scale_y())
  data <- map_position(panel, data, scale_x(), scale_y())

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }

  # Train coordinate system
  panel <- train_ranges(panel, plot$coordinates)

  list(data = data, panel = panel, plot = plot)
}

#' @export
#' @rdname ggplot_build
layer_data <- function(plot, i = 1L) {
  ggplot_build(plot)$data[[i]]
}
