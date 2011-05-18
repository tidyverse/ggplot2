# Build ggplot for rendering
ggplot_build <- function(plot) {
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)
  
  scales <- plot$scales
  # Apply function to layer and matching data
  dlapply <- function(f) mlply(cbind(d = data, p = layers), f)

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  
  panels <- Panels$clone(plot$coordinates, plot$facet)
  panels$train_panels(layer_data, plot$data)
  data <- panels$map(layer_data, plot$data)

  # Compute aesthetics to produce data with generalised variable names
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
  
  # Transform all scales
  data <- lapply(data, scales_transform_df, scales = scales)
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  panels$train_scales(data, scales)
  data <- panels$map_scales(data)
  
  # Apply and map statistics
  data <- panels$calculate_stats(data, layers)
  data <- dlapply(function(d, p) p$map_statistic(d, plot)) 
  
  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Apply position adjustments
  data <- dlapply(function(d, p) p$adjust_position(d))
   
  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's 
  # displayed, or does it include the range of underlying data
  panels$reset_scales()
  panels$train_scales(data, scales)
  data <- panels$map_scales(data)
  
  # Train and map non-position scales
  npscales <- scales$non_position_scales()  
  if (length(npscales$scales) > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }
  
  list(data = data, panels = panels)
}

