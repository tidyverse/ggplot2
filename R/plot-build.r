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
  dlapply <- function(f) mapply(f, data, layers, SIMPLIFY=FALSE)

  # Compute aesthetics from values at various levels
  data <- lapply(layers, function(x) x$make_aesthetics(plot))
  
  # Facet
  data <- mapply(function(d, p) facet$stamp_data(d), data, layers, SIMPLIFY=FALSE)

  # Transform scales where possible.  Also need to train so statisics
  # (e.g. stat_smooth) have access to info
  data <- dlapply(function(d, p) p$scales_transform(d, scales))
  facet$position_train(data, plot)

  # Ensure that position scales are of the correct type: 
  # continuous are numeric, and discrete are integers
  data <- facet$position_map(data, plot)

  # Apply and map statistics, then reparameterise geoms that need it
  data <- dlapply(function(d, p) p$calc_statistics(d, scales))
  data <- dlapply(function(d, p) p$map_statistics(d, plot))  
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Adjust position before scaling
  data <- dlapply(function(d, p) p$adjust_position(d, scales))

  # Transform, train and map new scales  
  dlapply(function(d, p) p$scales_train(d, scales))
  data <- dlapply(function(d, p) p$scales_map(d, scales))

  # missing_scales <- setdiff(c("x", "y"), scales$output())
  # if (length(missing_scales) > 0) {
  #   stop("ggplot: Some aesthetics (", paste(missing_scales, collapse =", "), ") are missing scales, you will need to add them by hand.", call.=FALSE)
  # }

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

