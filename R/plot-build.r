# Build ggplot for rendering
ggplot_build <- function(plot) {
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  
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
  # and all positions are numeric (needed for current implementation of
  # many stats, and for later reparameterisation)
  facet$position_train(data, scales)
  data <- facet$position_map(data, scales)

  # Apply and map statistics, then reparameterise geoms that need it
  data <- facet$calc_statistics(data, layers)
  data <- dlapply(function(d, p) p$map_statistics(d, plot)) 
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Adjust position
  data <- dlapply(function(d, p) p$adjust_position(d, scales))
    
  # Train and map scales for legends
  npscales <- scales$non_position_scales()
  if (length(npscales$scales) > 0) {
    dlapply(function(d, p) p$scales_train(d, npscales))
    data <- dlapply(function(d, p) p$scales_map(d, npscales))
  }
  
  # Train and map position scales
  facet$position_train(data, scales)
  data <- facet$position_map(data, scales)
  
  data
}

