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
  data <- dlapply(function(d, p) facet$stamp_data(d))

  # Transform all scales
  data <- dlapply(function(d, p) p$scales_transform(d, scales))
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  facet$position_train(data, plot)
  data <- facet$position_map(data, plot)
  
  # Apply and map statistics, then reparameterise geoms that need it
  data <- dlapply(function(d, p) p$calc_statistics(d, scales))
  data <- dlapply(function(d, p) p$map_statistics(d, plot))  
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Adjust position, update scales, then map all values
  data <- dlapply(function(d, p) p$adjust_position(d, scales))
  dlapply(function(d, p) p$scales_train(d, scales))
  data <- dlapply(function(d, p) p$scales_map(d, scales))

  # Produce grobs
  cs$train(scales)
  grobs <- dlapply(function(d, p) p$make_grobs(d, scales, cs))
  grobs3d <- array(unlist(grobs, recursive=FALSE), c(dim(data[[1]]), length(data)))
  panels <- aaply(grobs3d, 1:2, splat(grobTree), drop. = FALSE)
  
  scales <- plot$scales$minus(plot$scales$get_scales(c("x", "y", "z")))
  
  list(
    plot = plot,
    scales = scales,
    cs = cs,
    panels = panels
  )
}

