# The job of `Layout` is to coordinate:
# * The coordinate system
# * The facetting specification
# * The individual position scales for each panel
#
# This includes managing the parameters for the facet so that we
# don't modify the ggproto object in place.

create_layout <- function(facet = FacetNull, coord = CoordCartesian) {
  ggproto(NULL, Layout, facet = facet, coord = coord)
}

Layout <- ggproto("Layout", NULL,
  coord = NULL,

  facet = NULL,
  facet_params = NULL,

  panel_layout = NULL,
  panel_scales = NULL,
  panel_ranges = NULL,

  setup = function(self, data, plot_data = data.frame(), plot_env = emptyenv()) {
    data <- c(list(plot_data), data)

    self$facet_params <- self$facet$setup_params(data, self$facet$params)
    self$facet_params$plot_env <- plot_env
    data <- self$facet$setup_data(data, self$facet_params)

    self$panel_layout <- self$facet$compute_layout(data, self$facet_params)
    if (!all(c("PANEL", "SCALE_X", "SCALE_Y") %in% names(self$panel_layout))) {
      stop("Facet layout has bad format. It must contains the columns 'PANEL', 'SCALE_X', and 'SCALE_Y'", call. = FALSE)
    }
    # Special case of CoordFlip - switch the layout scales
    if (inherits(self$coord, "CoordFlip")) {
      self$panel_layout[, c("SCALE_X", "SCALE_Y")] <- self$panel_layout[, c("SCALE_Y", "SCALE_X"), drop = FALSE]
    }

    lapply(data[-1], function(data) {
      self$facet$map_data(data, self$panel_layout, self$facet_params)
    })
  },

  render = function(self, panels, data, theme, labels) {
    below <- self$facet$draw_back(data, self$panel_layout, self$panel_scales$x, self$panel_scales$y, theme, self$facet_params)
    above <- self$facet$draw_front(data, self$panel_layout, self$panel_scales$x, self$panel_scales$y, theme, self$facet_params)

    panels <- lapply(seq_along(panels[[1]]), function(i) {
      fg <- self$coord$render_fg(self$panel_ranges[[i]], theme)
      bg <- self$coord$render_bg(self$panel_ranges[[i]], theme)

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
    labels <- self$coord$labels(list(
      x = self$xlabel(labels),
      y = self$ylabel(labels)
    ))
    labels <- self$render_labels(labels, theme)

    panels <- self$facet$draw_panels(panels, self$panel_layout, self$panel_scales$x, self$panel_scales$y, self$panel_ranges, self$coord, data, theme, self$facet_params)
    self$facet$draw_labels(panels, self$panel_layout, self$panel_scales$x, self$panel_scales$y, self$panel_ranges, self$coord, data, theme, labels, self$params)
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

    self$facet$train_scales(self$panel_scales$x, self$panel_scales$y, layout, data, self$facet_params)
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
    lapply(data, function(layer_data) {
      self$facet$finish_data(layer_data, self$panel_layout, self$panel_scales$x,
        self$panel_scales$y, self$facet_params)
    })
  },

  get_scales = function(self, i) {
    this_panel <- self$panel_layout[self$panel_layout$PANEL == i, ]

    list(
      x = self$panel_scales$x[[this_panel$SCALE_X]],
      y = self$panel_scales$y[[this_panel$SCALE_Y]]
    )
  },

  train_ranges = function(self) {
    compute_range <- function(ix, iy) {
      # TODO: change coord_train method to take individual x and y scales
      self$coord$train(list(x = self$panel_scales$x[[ix]], y = self$panel_scales$y[[iy]]))
    }
    # Switch position of all scales if CoordFlip
    if (inherits(self$coord, "CoordFlip") || (inherits(self$coord, "CoordPolar") && self$coord$theta == "y")) {
      lapply(self$panel_scales$x, function(scale) {
        scale$position <- if (scale$position == "top") "bottom" else "top"
      })
      lapply(self$panel_scales$y, function(scale) {
        scale$position <- if (scale$position == "left") "right" else "left"
      })
    }
    self$panel_ranges <- Map(compute_range, self$panel_layout$SCALE_X, self$panel_layout$SCALE_Y)
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
