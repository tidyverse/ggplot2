# All data related information should be stored in the panels or scales, not
# in the facet or coord - these will become strategy objects.
require(mutatr)
Panels <- Object$clone()$do({
  
  self$clone <- function(coord, facet) {
    self$facet <- facet
    self$coord <- coord

    self
  }
  
  # Learn size and arrangement of panels from data.
  # 
  #  * facet: figure out how many panels are needed, and build up a table that
  #    matches data subsets to panel position and scales. 
  # 
  # @param data a list of data frames (one for each layer)  
  # @param plot_data default plot data frame
  self$train_panels <- function(data, plot_data) {
    data <- c(list(plot_data), data)
    self$panel_info <- self$facet$panel_info(data)
    
    # Make space for scales
    self$x_scales <- vector("list", max(self$panel_info$SCALE_X))
    self$y_scales <- vector("list", max(self$panel_info$SCALE_Y))

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
  self$map <- function(layer_data, plot_data) {
    lapply(layer_data, function(data) {
      if (empty(data)) data <- plot_data
      self$facet$map_layer(data)
    })    
  }
  
  # Train position scales with data.
  # 
  # @param data a list of data frames (one for each layer)  
  self$train_scales <- function(data, scales) { 
    pos <- ldply(data, function(df) df[c("x", "y", "PANEL")])
    pos <- join(pos, self$panel_info, by = "PANEL", match = "first")
    
    new_x_scale <- function() scale_clone(scales$get_scales("x"))
    new_y_scale <- function() scale_clone(scales$get_scales("y"))
    
    
    d_ply(pos, "SCALE_X", function(df) {
      if (is.null(df$x)) return()
      
      scale_pos <- df$SCALE_X[1]
      if (is.null(self$x_scales[[scale_pos]])) {
        self$x_scales[[scale_pos]] <- new_x_scale()
      }
      scale_train(self$x_scales[[scale_pos]], df$x)
    })
    
    d_ply(pos, "SCALE_Y", function(df) {
      if (is.null(df$y)) return()
      
      scale_pos <- df$SCALE_Y[1]
      if (is.null(self$y_scales[[scale_pos]])) {
        self$y_scales[[scale_pos]] <- new_y_scale()
      }
      scale_train(self$y_scales[[scale_pos]], df$y)
    })
    
    invisible(NULL)
  }
  
  self$reset_scales <- function() {
    l_ply(self$x_scales, scale_reset)
    l_ply(self$y_scales, scale_reset)
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
  self$map_scales <- function(data) {
    lapply(data, function(layer_data) {
      panel <- match(layer_data$PANEL, self$panel_info$PANEL)
      
      scale_x <- self$panel_info$SCALE_X[panel]
      layer_data <- ldply(unique(scale_x), function(i) {
        old <- layer_data[scale_x == i, , ]
        new <- scales_map_df(self$x_scales[[i]], old)
        cunion(new, old)
      })
      
      scale_y <- self$panel_info$SCALE_Y[panel]
      layer_data <- ldply(unique(scale_y), function(i) {
        old <- layer_data[scale_y == i, , ]
        new <- scales_map_df(self$y_scales[[i]], old)
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
        scales <- self$make_panel_scales(panel_data$PANEL[1])
        l$calc_statistic(panel_data, scales)
      })
    })
  }
  
  self$make_panel_scales <- function(i) {
    panel <- self$panel_info[self$panel_info$PANEL == i, ]
    list(
      x = self$x_scales[[panel$SCALE_X]],
      y = self$y_scales[[panel$SCALE_Y]]
    )
    
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