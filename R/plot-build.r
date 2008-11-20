# Build ggplot for rendering
# This function is the powerhouse that converts the plot specification into something that's ready to be rendered on screen
# 
# @keywords internal
ggplot_build <- function(plot) {
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  
  plot <- plot_clone(plot)
  layers <- plot$layers
  scales <- plot$scales
  facet <- plot$facet
  cs <- plot$coordinates
  # Apply function to layer and matching data
  dlapply <- function(f) mlply(cbind(d = data, p = layers), f)

  # Evaluate aesthetics
  data <- lapply(layers, function(x) x$make_aesthetics(plot))
  
  # Facet
  facet$initialise(data)
  data <- facet$stamp_data(data)

  # Transform all scales
  data <- dlapply(function(d, p) p$scales_transform(d, scales))
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  facet$position_train(data, scales)
  data <- facet$position_map(data, scales)
  
  # Apply and map statistics, then reparameterise geoms that need it
  data <- facet$calc_statistics(data, layers)
  data <- dlapply(function(d, p) p$map_statistics(d, plot))  
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Adjust position
  data <- dlapply(function(d, p) p$adjust_position(d, scales))
  
  npscales <- scales$non_position_scales()
  
  # Train and map, for final time
  if (npscales$n() > 0) {
    dlapply(function(d, p) p$scales_train(d, npscales))
    data <- dlapply(function(d, p) p$scales_map(d, npscales))
  }
  facet$position_train(data, scales)
  data <- facet$position_map(data, scales)    

  # Produce grobs
  grobs <- facet$make_grobs(data, layers, cs)
  
  grobs3d <- array(unlist(grobs, recursive=FALSE), c(dim(data[[1]]), length(data)))
  panels <- aaply(grobs3d, 1:2, splat(grobTree), .drop = FALSE)
  
  list(
    plot = plot,
    scales = npscales,
    cs = cs,
    panels = panels,
    facet = facet
  )
}

