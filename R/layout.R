create_layout <- function(facet) {
  ggproto(NULL, Layout, facet = facet)
}

Layout <- ggproto("Layout", NULL,
  facet = NULL,
  panel_layout = NULL,
  panel_scales = NULL,
  panel_ranges = NULL,

  train = function(self, data, plot_data) {
    self$panel_layout <- self$facet$train(c(list(plot_data), data))
    if (!all(c("PANEL", "SCALE_X", "SCALE_Y") %in% names(self$panel_layout))) {
      stop("Facet layout has bad format. It must contains the columns 'PANEL', 'SCALE_X', and 'SCALE_Y'", call. = FALSE)
    }
  },

  map = function(self, data) {
    lapply(data, function(data) {
      self$facet$map(data, self$panel_layout)
    })
  },

  render = function(self, panels, data, coord, theme, labels) {
    below <- self$facet$render_back(data, self$panel_layout, self$panel_scales$x, self$panel_scales$y)
    above <- self$facet$render_front(data, self$panel_layout, self$panel_scales$x, self$panel_scales$y)

    panels <- lapply(seq_along(panels), function(i) {
      fg <- coord$render_fg(self$panel_ranges[[i]], theme)
      bg <- coord$render_bg(self$panel_ranges[[i]], theme)

      panel <- lapply(panels, `[[`, i)
      panel <- c(below[i], panel, above[i])

      if (theme$panel.ontop) {
        panel <- c(panel, list(bg), list(fg))
      } else {
        panel <- c(list(bg), panel, list(fg))
      }

      ggname(paste("panel", i, sep = "-"),
             gTree(children = do.call("gList", panel)))
    })
    labels <- coord$labels(list(
      x = self$xlabel(labels),
      y = self$ylabel(labels)
    ))
    labels <- self$render_labels(labels, theme)
    self$facet$render_panels(panels, self$panel_layout, self$panel_scales$x,
      self$panel_scales$y, self$panel_ranges, coord, data, theme, labels)
  },

  train_position = function(self, data, x_scale, y_scale) {
    # Initialise scales if needed, and possible.
    layout <- self$panel_layout
    if (is.null(self$panel_scales$x) && !is.null(x_scale)) {
      self$panel_scales$x <- plyr::rlply(max(layout$SCALE_X), x_scale$clone())
    }
    if (is.null(self$panel_scales$y) && !is.null(y_scale)) {
      self$panel_scales$y <- plyr::rlply(max(layout$SCALE_Y), y_scale$clone())
    }

    self$facet$train_positions(self$panel_scales$x, self$panel_scales$y, layout, data)
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

  get_scales = function(self, i) {
    this_panel <- self$panel_layout[self$panel_layout$PANEL == i, ]

    list(
      x = self$panel_scales$x[[this_panel$SCALE_X]],
      y = self$panel_scales$y[[this_panel$SCALE_Y]]
    )
  },

  train_ranges = function(self, coord) {
    compute_range <- function(ix, iy) {
      # TODO: change coord_train method to take individual x and y scales
      coord$train(list(x = self$panel_scales$x[[ix]], y = self$panel_scales$y[[iy]]))
    }

    self$panel_ranges <- Map(compute_range, self$panel_layout$SCALE_X, self$panel_layout$SCALE_Y)
  },

  xlabel = function(self, labels) {
    primary <- self$panel_scales$x[[1]]$name %|W|% labels$x
    secondary <- if (is.null(self$panel_scales$x[[1]]$secondary.axis)) {
      waiver()
    } else {
      self$panel_scales$x[[1]]$sec_name()
    } %|W|% labels$sec.x
    list(primary = primary, secondary = secondary)[self$panel_scales$x[[1]]$axis_order()]
  },

  ylabel = function(self, labels) {
    primary <- self$panel_scales$y[[1]]$name %|W|% labels$y
    secondary <- if (is.null(self$panel_scales$y[[1]]$secondary.axis)) {
      waiver()
    } else {
      self$panel_scales$y[[1]]$sec_name()
    } %|W|% labels$sec.y
    list(primary = primary, secondary = secondary)[self$panel_scales$y[[1]]$axis_order()]
  },

  find_panel = function(self, tabel) {
    self$facet$find_panel(tabel)
  },

  render_labels = function(self, labels, theme) {
    label_grobs <- lapply(names(labels), function(label) {
      lapply(names(labels[[label]]), function(ary) {
        if (is.null(labels[[label]][[ary]]) || is.waive(labels[[label]][[ary]])) return(zeroGrob())
        args <- list(
          theme = theme,
          element = paste0("axis.title.", label),
          label = labels[[label]][[ary]],
          expand_x = label == "y",
          expand_y = label == "x"
        )
        do.call(element_render, args)
      })
    })
    names(label_grobs) <- names(labels)
    label_grobs
  }
)
