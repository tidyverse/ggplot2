# All data related information should be stored in the panels or scales, not
# in the facet or coord - these will become strategy objects.

Panels <- Object$clone()$do({
  
  self$clone <- function(scales, coord, facet) {
    self$facet <- facet
    self$coord <- coord
    self$scales <- list(x = scales$get_scales("x"), 
      y = scales$get_scales("y"))
      
    self
  }
  
  # Learn size and arrangement of panels from data.
  # 
  #  * facet: figure out how many panels are needed, and build up a table that
  #    matches data subsets to panel position and scales. 
  # 
  # @param data a list of data frames (one for each layer)  
  # @param plot_data default plot data frame
  self$train <- function(data, plot_data) {
    data <- c(list(plot_data), data)
    self$panel_info <- self$facet$panel_info(data)

    # Initialise as many scales as necessary
    nx <- max(self$panel_info$SCALE_X)
    self$x_scales <- rlply(nx, self$scales$x$clone())

    ny <- max(self$panel_info$SCALE_Y)
    self$y_scales <- rlply(ny, self$scales$y$clone())
    
    invisible(NULL)
  }
  
  # Add missing variables to data
  #
  # Data is mapped after training to ensure that all layers have extra
  # copies of data for margins and missing facetting variables, and 
  # has a PANEL variable that tells which panel it belongs to.
  # 
  # All other panel functions work with this data.
  # 
  # @param data a list of data frames (one for each layer)  
  # @param plot_data default plot data frame
  self$map <- function(data, plot_data) {
    lapply(layer_data, function(data) {
      if (empty(data)) data <- plot_data
      facet$map_layer(data, self$panel_info)
    })    
  }
  
  # Train scales with data.
  # 
  # @param data a list of data frames (one for each layer)  
  self$train_scales <- function(data) { 
    pos <- ldply(data, function(df) df[c("x", "y", "PANEL")])
    pos <- join(pos, self$panel_info, by = "PANEL")
    
    d_ply(pos, "SCALE_X", function(df) {
      x <- df$SCALE_X[1]
      self$x_scales[[x]]$train(df$x)
    })
    d_ply(pos, "SCALE_Y", function(df) {
      y <- df$SCALE_Y[1]
      self$y_scales[[y]]$train(df$y)
    })
    
    invisible(NULL)
  }
  
  # Map data with scales.
  #
  # This operation must be idempotent because it is applied twice: both before
  # and after statistical transformation.
  # 
  # @param data a list of data frames (one for each layer)  
  self$map_scales <- function(data) {
    lapply(data, function(layer_data) {
      panel <- match(layer_data$PANEL, .$panel_info$PANEL)
      
      scale_x <- .$panel_info$SCALE_X[panel]
      layer_data <- ldply(unique(scale_x), function(i) {
        old <- data[scale_x == i, , ]
        new <- .$scales$x[[i]]$map_df(old)
        cunion(new, old)
      })
      
      scale_y <- .$panel_info$SCALE_Y[panel]
      layer_data <- ldply(unique(scale_y), function(i) {
        old <- data[scale_y == i, , ]
        new <- .$scales$y[[i]]$map_df(old)
        cunion(new, old)
      })
      
      layer_data
    })
  }
  
  # Calculate statistics
  # 
  # @param layers list of layers
  # @param data a list of data frames (one for each layer)  
  self$calculate_stats <- function(data, layers) {
    mlply(cbind(d = data, l = layers), function(d, l) {
      ddply(d, "PANEL", function(panel_data) {
        panel <- self$make_panel(panel_data$PANEL[1])
        layer$calc_statistic(panel_data, panel)
      })
    })
  }
  
  # Render the plot, combining axes, contents, strips, legends and labels.
  #
  # @return a TableLayout
  render <- function(data, layers, scales, theme) {
    contents <- self$build_contents(data, layers, theme)
    axes <- self$build_axes(theme)
    strips <- self$build_strips(theme)
    labels <- self$build_labels(theme)
    legends <- self$build_legends(scales, theme)
    
    contents
  }
  
  # Build axes for each side.
  #
  #  * coord: which scale goes where, tick positions and labels.
  #  * theme: fonts, colours etc.
  #  * facet: which positions (trbl) to draw scales.
  # 
  # @return a TableLayout
  build_axes <- function(theme) {  
    
  }
  
  # Build contents = background + geoms + foreground
  #
  #  * coord: default aspect ratio, background, foreground, ranges (for geom)
  #  * facet: size
  #  * layer: data, geom
  #  * theme: colours etc.
  # 
  # @return a TableLayout
  build_contents <- function(data, layers, theme) {
    panels <- self$panel_info$PANEL    
    ncol <- max(self$panel_info$COL)
    nrow <- max(self$panel_info$ROW)
    
    grobs <- lapply(self$panel_info$PANEL, panel_grob, 
      layers = layers, data = data, theme = theme)
    
    panel_matrix <- matrix(list(nullGrob()), nrow = nrow, ncol = ncol)
    panel_matrix[panels] <- panel_grobs
    
    dimensions <- self$facet$panel_dimensions(self$panel_info, coord)
    
    layout_matrix("panel", panel_matrix, 
      dimensions$widths, dimensions$heights, dimension$respect)
  }
  
  # Build panel grob for a single panel.
  # 
  # @return a grob
  panel_grob <- function(i, layers, data, theme) {
    panel <- self$make_panel(i)
    
    fg <- self$coord$guide_foreground(panel, theme)
    contents <- mlply(cbind(l = layers, d = data), function(l, d) 
      l$make_geom(d, panel))
    bg <- self$coord$guide_background(panel, theme)

    grobTree(bg, do.call("gList", contents), fg)
  }
  
  # Build strip for each side.
  #
  #  * panel info: values of labels
  #  * theme: labelling function, fonts etc
  #  * facet: which labels go where, position of strips
  # 
  # @return a TableLayout
  build_strips <- function(theme) {
  }
  
  # Build legend for each side.
  # 
  #  * theme: position & orientation
  #  * non-pos scales: keys & labels
  # 
  # @return a TableLayout
  build_legends <- function(scales, theme) {
  }
  
  # Build labels and title.
  # 
  #  * theme: labels, title, fonts etc
  #  * pos scales: default labels
  #  * coord: which goes where
  # 
  # @return a TableLayout
  build_labels <- function(theme) {
    
  }
  
})