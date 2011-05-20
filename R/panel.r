# Panel object.
#
# A panel figures out how data is positioned within a panel of a plot,
# coordinates information from scales, facets and coords.  Eventually all
# state will move out of facets and coords, and live only in panels and 
# stats, simplifying these data structures to become strategies.
#
# Information about a panel is built up progressively over time, which
# is why the initial object is empty to start with.
new_panel <- function() {
  structure(list(), class = "panel")
}

# Learn the layout of panels within a plot.
#
# This is determined by the facet, which returns a data frame, than
# when joined to the data to be plotted tells us which panel it should
# appear in, where that panel appears in the grid, and what scales it
# uses.
#
# As well as the layout info, this function also adds empty lists in which
# to house the x and y scales.
#
# @param the panel object to train
# @param the facetting specification
# @param data a list of data frames (one for each layer), and one for the plot
# @return an updated panel object
train_layout <- function(panel, facet, data, plot_data) {
  layout <- facet_train_layout(facet, c(data, list(plot_data)))

  panel$layout <- layout
  panel$shrink <- facet$shrink
  
  panel
}

# Map data to find out where it belongs in the plot.
#
# Layout map ensures that all layer data has extra copies of data for margins 
# and missing facetting variables, and has a PANEL variable that tells that
# so it know what panel it belongs to. This is a change from the previous
# design which added facetting variables directly to the data frame and
# caused problems when they had names of aesthetics (like colour or group).
# 
# @param panel a trained panel object
# @param the facetting specification
# @param data list of data frames (one for each layer)  
# @param plot_data default plot data frame
map_layout <- function(panel, facet, data, plot_data) {
  lapply(data, function(data) {
    if (empty(data)) data <- plot_data
    facet_map_layout(facet, data, panel$layout)
  })    
}

# Train position scales with data
# 
# If panel-specific scales are not already present, will clone from
# the scales provided in the parameter
#
# @param the panel object to train
# @param data a list of data frames (one for each layer)  
# @param x_scale x scale for the plot
# @param y_scale y scale for the plot
train_position <- function(panel, data, x_scale, y_scale) { 
  # Initialise scales if needed, and possible.
  layout <- panel$layout
  if (is.null(panel$x_scales) && !is.null(x_scale)) {
    panel$x_scales <- rlply(max(layout$SCALE_X), scale_clone(x_scale))
  }
  if (is.null(panel$y_scales) && !is.null(y_scale)) {
    panel$y_scales <- rlply(max(layout$SCALE_Y), scale_clone(y_scale))
  }
  
  # loop over each layer, training x and y scales in turn
  for(layer_data in data) {
    pos <- join(layer_data, layout, by = "PANEL")
    
    # Loop through data for each scale, creating a new scale if needed
    d_ply(pos, "SCALE_X", function(df) {
      if (is.null(x_scale)) return()
      scale_train_df(panel$x_scales[[df$SCALE_X[1]]], df)
    })

    d_ply(pos, "SCALE_Y", function(df) {
      if (is.null(y_scale)) return()
      scale_train_df(panel$y_scales[[df$SCALE_Y[1]]], df)
    })
  }

  panel
}


reset_scales <- function(panel) {
  if (!panel$shrink) return()
  l_ply(panel$x_scales, scale_reset)
  l_ply(panel$y_scales, scale_reset)
}  

# Map data with scales.
#
# This operation must be idempotent because it is applied twice: both before
# and after statistical transformation.
# 
# TODO: see how slow this is, and whether it's more effective to do it
# column wise, and using the knowledge that all position scales do is
# convert to numeric/integer for x, xmin, xmax, y, ymin, ymax, xend and yend
#
# @param data a list of data frames (one for each layer)  
map_position <- function(panel, data) {
  lapply(data, function(layer_data) {
    panel_id <- match(layer_data$PANEL, panel$layout$PANEL)
    
    scale_x <- panel$layout$SCALE_X[panel_id]
    layer_data <- ldply(unique(scale_x), function(i) {
      old <- layer_data[scale_x == i, , drop = FALSE]
      new <- scale_map_df(panel$x_scales[[i]], old)
      cunion(new, old)
    })
    
    scale_y <- panel$layout$SCALE_Y[panel_id]
    layer_data <- ldply(unique(scale_y), function(i) {
      old <- layer_data[scale_y == i, , ]
      new <- scale_map_df(panel$y_scales[[i]], old)
      cunion(new, old)
    })
    
    layer_data
  })
}

panel_scales <- function(panel, i) {
  this_panel <- panel$layout[panel$layout$PANEL == i, ]

  list(
    x = panel$x_scales[[this_panel$SCALE_X]],
    y = panel$y_scales[[this_panel$SCALE_Y]]
  )    
}

# Compute ranges and dimensions of each panel, using the coord.
train_ranges <- function(panel, coord) {
  compute_range <- function(ix, iy) {
    # TODO: change coord_train method to take individual x and y scales
    coord_train(coord, list(x = panel$x_scales[[ix]], y = panel$y_scales[[iy]]))
  }

  panel$ranges <- Map(compute_range, 
    panel$layout$SCALE_X, panel$layout$SCALE_Y)
  panel
}

# Calculate statistics
# 
# @param layers list of layers
# @param data a list of data frames (one for each layer)  
calculate_stats <- function(panel, data, layers) {
  
  lapply(seq_along(data), function(i) {
    d <- data[[i]]
    l <- layers[[i]]
    
    ddply(d, "PANEL", function(panel_data) {
      scales <- panel_scales(panel, panel_data$PANEL[1])
      l$calc_statistic(panel_data, scales)
    })    
  }) 
}

