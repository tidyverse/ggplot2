# The job of `Layout` is to coordinate:
# * The coordinate system
# * The facetting specification
# * The individual position scales for each panel
#
# This includes managing the parameters for the facet and the coord
# so that we don't modify the ggproto object in place.

create_layout <- function(facet = FacetNull, coord = CoordCartesian) {
  ggproto(NULL, Layout, facet = facet, coord = coord)
}

Layout <- ggproto("Layout", NULL,
  coord = NULL,
  coord_params = list(),

  facet = NULL,
  facet_params = list(),

  panel_layout = NULL,
  panel_scales = NULL,
  panel_ranges = NULL,

  setup = function(self, data, plot_data = data.frame(), plot_env = emptyenv()) {
    data <- c(list(plot_data), data)

    # Setup facets
    self$facet_params <- self$facet$setup_params(data, self$facet$params)
    self$facet_params$plot_env <- plot_env
    data <- self$facet$setup_data(data, self$facet_params)

    # Setup coords
    self$coord_params <- self$coord$setup_params(data)
    data <- self$coord$setup_data(data, self$coord_params)

    # Generate panel layout
    self$panel_layout <- self$facet$compute_layout(data, self$facet_params)
    self$panel_layout <- self$coord$setup_layout(self$panel_layout, self$coord_params)
    check_layout(self$panel_layout)

    # Add panel coordinates to the data for each layer
    lapply(data[-1], self$facet$map_data,
      layout = self$panel_layout,
      params = self$facet_params
    )
  },

  # Assemble the facet fg & bg, the coord fg & bg, and the layers
  # Returns a gtable
  render = function(self, panels, data, theme, labels) {
    facet_bg <- self$facet$draw_back(data,
      self$panel_layout,
      self$panel_scales$x,
      self$panel_scales$y,
      theme,
      self$facet_params
    )
    facet_fg <- self$facet$draw_front(
      data,
      self$panel_layout,
      self$panel_scales$x,
      self$panel_scales$y,
      theme,
      self$facet_params
    )

    # Draw individual panels, then assemble into gtable
    panels <- lapply(seq_along(panels[[1]]), function(i) {
      panel <- lapply(panels, `[[`, i)
      panel <- c(facet_bg[i], panel, facet_fg[i])

      coord_fg <- self$coord$render_fg(self$panel_ranges[[i]], theme)
      coord_bg <- self$coord$render_bg(self$panel_ranges[[i]], theme)
      if (theme$panel.ontop) {
        panel <- c(panel, list(coord_bg), list(coord_fg))
      } else {
        panel <- c(list(coord_bg), panel, list(coord_fg))
      }

      ggname(
        paste("panel", i, sep = "-"),
        gTree(children = do.call("gList", panel))
      )
    })
    plot_table <- self$facet$draw_panels(
      panels,
      self$panel_layout,
      self$panel_scales$x,
      self$panel_scales$y,
      self$panel_ranges,
      self$coord,
      data,
      theme,
      self$facet_params
    )

    # Draw individual labels, then add to gtable
    labels <- self$coord$labels(list(
      x = self$xlabel(labels),
      y = self$ylabel(labels)
    ))
    labels <- self$render_labels(labels, theme)
    self$facet$draw_labels(
      plot_table,
      self$panel_layout,
      self$panel_scales$x,
      self$panel_scales$y,
      self$panel_ranges,
      self$coord,
      data,
      theme,
      labels,
      self$params
    )
  },

  train_position = function(self, data, x_scale, y_scale) {
    # Initialise scales if needed, and possible.
    layout <- self$panel_layout
    if (is.null(self$panel_scales$x)) {
      self$panel_scales$x <- self$facet$init_scales(layout, x_scale = x_scale,
        params = self$facet_params)$x
    }
    if (is.null(self$panel_scales$y)) {
      self$panel_scales$y <- self$facet$init_scales(layout, y_scale = y_scale,
        params = self$facet_params)$y
    }

    self$facet$train_scales(
      self$panel_scales$x,
      self$panel_scales$y,
      layout,
      data,
      self$facet_params
    )
  },

  reset_scales = function(self) {
    if (!self$facet$shrink) return()
    lapply(self$panel_scales$x, function(s) s$reset())
    lapply(self$panel_scales$y, function(s) s$reset())
    invisible()
  },

  map_position = function(self, data) {
    layout <- self$panel_layout

    lapply(data, function(layer_data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)

      # Loop through each variable, mapping across each scale, then joining
      # back together
      x_vars <- intersect(self$panel_scales$x[[1]]$aesthetics, names(layer_data))
      names(x_vars) <- x_vars
      SCALE_X <- layout$SCALE_X[match_id]
      new_x <- scale_apply(layer_data, x_vars, "map", SCALE_X, self$panel_scales$x)
      layer_data[, x_vars] <- new_x

      y_vars <- intersect(self$panel_scales$y[[1]]$aesthetics, names(layer_data))
      names(y_vars) <- y_vars
      SCALE_Y <- layout$SCALE_Y[match_id]
      new_y <- scale_apply(layer_data, y_vars, "map", SCALE_Y, self$panel_scales$y)
      layer_data[, y_vars] <- new_y

      layer_data
    })
  },

  finish_data = function(self, data) {
    lapply(data, self$facet$finish_data,
      layout = self$panel_layout,
      x_scales = self$panel_scales$x,
      y_scales = self$panel_scales$y,
      params = self$facet_params
    )
  },

  get_scales = function(self, i) {
    this_panel <- self$panel_layout[self$panel_layout$PANEL == i, ]

    list(
      x = self$panel_scales$x[[this_panel$SCALE_X]],
      y = self$panel_scales$y[[this_panel$SCALE_Y]]
    )
  },

  train_ranges = function(self) {
    # Fudge for CoordFlip and CoordPolar - in place modification of
    # scales is not elegant, but it is pragmatic
    self$coord$modify_scales(self$panel_scales$x, self$panel_scales$y)

    scales_x <- self$panel_scales$x[self$panel_layout$SCALE_X]
    scales_y <- self$panel_scales$y[self$panel_layout$SCALE_Y]

    self$panel_ranges <- Map(self$coord$train, scales_x, scales_y)
    invisible()
  },

  xlabel = function(self, labels) {
    primary <- self$panel_scales$x[[1]]$name %|W|% labels$x
    primary <- self$panel_scales$x[[1]]$make_title(primary)
    secondary <- if (is.null(self$panel_scales$x[[1]]$secondary.axis)) {
      waiver()
    } else {
      self$panel_scales$x[[1]]$sec_name()
    } %|W|% labels$sec.x
    if (is.derived(secondary)) secondary <- primary
    secondary <- self$panel_scales$x[[1]]$make_sec_title(secondary)
    list(primary = primary, secondary = secondary)[self$panel_scales$x[[1]]$axis_order()]
  },

  ylabel = function(self, labels) {
    primary <- self$panel_scales$y[[1]]$name %|W|% labels$y
    primary <- self$panel_scales$y[[1]]$make_title(primary)
    secondary <- if (is.null(self$panel_scales$y[[1]]$secondary.axis)) {
      waiver()
    } else {
      self$panel_scales$y[[1]]$sec_name()
    } %|W|% labels$sec.y
    if (is.derived(secondary)) secondary <- primary
    secondary <- self$panel_scales$y[[1]]$make_sec_title(secondary)
    list(primary = primary, secondary = secondary)[self$panel_scales$y[[1]]$axis_order()]
  },

  render_labels = function(self, labels, theme) {
    label_grobs <- lapply(names(labels), function(label) {
      lapply(c(1, 2), function(i) {
        modify <- if (i == 2 && label == "y") ".right" else if (i == 1 && label == "x") ".top" else ""
        if (is.null(labels[[label]][[i]]) || is.waive(labels[[label]][[i]]))
          return(zeroGrob())

        element_render(
          theme = theme,
          element = paste0("axis.title.", label, modify),
          label = labels[[label]][[i]],
          expand_x = label == "y",
          expand_y = label == "x"
        )
      })
    })
    names(label_grobs) <- names(labels)
    label_grobs
  }
)


# Helpers -----------------------------------------------------------------

# Function for applying scale method to multiple variables in a given
# data set.  Implement in such a way to minimize copying and hence maximise
# speed
scale_apply <- function(data, vars, method, scale_id, scales) {
  if (length(vars) == 0) return()
  if (nrow(data) == 0) return()

  n <- length(scales)
  if (any(is.na(scale_id))) stop()

  scale_index <- plyr::split_indices(scale_id, n)

  lapply(vars, function(var) {
    pieces <- lapply(seq_along(scales), function(i) {
      scales[[i]][[method]](data[[var]][scale_index[[i]]])
    })
    # Join pieces back together, if necessary
    if (!is.null(pieces)) {
      unlist(pieces)[order(unlist(scale_index))]
    }
  })
}
