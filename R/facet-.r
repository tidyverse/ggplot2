#' @include ggproto.r
NULL

#' Is this object a facetting specification?
#'
#' @param x object to test
#' @keywords internal
#' @export
is.facet <- function(x) inherits(x, "Facet")

#' @export
Facet <- ggproto("Facet", NULL,
  shrink = FALSE,
  params = list(),


# Layout interface --------------------------------------------------------

  train = function(self, data) {
    self$compute_layout(data, self$params)
  },
  map = function(self, data, layout) {
    self$map_data(data, layout, self$params)
  },
  render_back = function(self, data, layout, x_scales, y_scales) {
    self$draw_back(data, layout, x_scales, y_scales, self$params)
  },
  render_front = function(self, data, layout, x_scales, y_scales) {
    self$draw_front(data, layout, x_scales, y_scales, self$params)
  },
  render_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, labels) {
    panels <- self$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, self$params)
    self$draw_labels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, labels, self$params)
  },
  train_positions = function(self, x_scales, y_scales, layout, data) {
    self$train_scales(x_scales, y_scales, layout, data, self$params)
  },


# Extension interface -----------------------------------------------------

  compute_layout = function(data, params) {
    stop("Not implemented", call. = FALSE)
  },
  map_data = function(data, layout, params) {
    stop("Not implemented", call. = FALSE)
  },
  train_scales = function(x_scales, y_scales, layout, data, params) {
    # loop over each layer, training x and y scales in turn
    for (layer_data in data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)

      if (!is.null(x_scales)) {
        x_vars <- intersect(x_scales[[1]]$aesthetics, names(layer_data))
        SCALE_X <- layout$SCALE_X[match_id]

        scale_apply(layer_data, x_vars, "train", SCALE_X, x_scales)
      }

      if (!is.null(y_scales)) {
        y_vars <- intersect(y_scales[[1]]$aesthetics, names(layer_data))
        SCALE_Y <- layout$SCALE_Y[match_id]

        scale_apply(layer_data, y_vars, "train", SCALE_Y, y_scales)
      }
    }
  },
  draw_back = function(data, layout, x_scales, y_scales, params) {
    rep(list(zeroGrob()), length(unique(layout$PANEL)))
  },
  draw_front = function(data, layout, x_scales, y_scales, params) {
    rep(list(zeroGrob()), length(unique(layout$PANEL)))
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    stop("Not implemented", call. = FALSE)
  },
  draw_labels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, labels, params) {
    panel_dim <-  Facet$find_panel(panels)

    xlab_height_top <- grobHeight(labels$x[[1]])
    panels <- gtable_add_rows(panels, xlab_height_top, pos = 0)
    panels <- gtable_add_grob(panels, labels$x[[1]], name = "xlab-t",
      l = panel_dim$l, r = panel_dim$r, t = 1, clip = "off")

    xlab_height_bottom <- grobHeight(labels$x[[2]])
    panels <- gtable_add_rows(panels, xlab_height_bottom, pos = -1)
    panels <- gtable_add_grob(panels, labels$x[[2]], name = "xlab-b",
      l = panel_dim$l, r = panel_dim$r, t = -1, clip = "off")

    panel_dim <-  Facet$find_panel(panels)

    ylab_width_left <- grobWidth(labels$y[[1]])
    panels <- gtable_add_cols(panels, ylab_width_left, pos = 0)
    panels <- gtable_add_grob(panels, labels$y[[1]], name = "ylab-l",
      l = 1, b = panel_dim$b, t = panel_dim$t, clip = "off")

    ylab_width_right <- grobWidth(labels$y[[2]])
    panels <- gtable_add_cols(panels, ylab_width_right, pos = -1)
    panels <- gtable_add_grob(panels, labels$y[[2]], name = "ylab-r",
      l = -1, b = panel_dim$b, t = panel_dim$t, clip = "off")

    panels
  },


  find_panel = function(table) {
    layout <- table$layout
    panels <- layout[grepl("^panel", layout$name), , drop = FALSE]

    data.frame(
      t = min(panels$t),
      r = max(panels$r),
      b = max(panels$b),
      l = min(panels$l)
    )
  },
  panel_cols = function(table) {
    panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
    unique(panels[, c('l', 'r')])
  },
  panel_rows = function(table) {
    panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
    unique(panels[, c('t', 'b')])
  },
  # Take input data and define a mapping between facetting variables and ROW,
  # COL and PANEL keys
  #
  # @param data A list of data.frames, the first being the plot data and the
  # subsequent individual layer data
  #
  # @return A data.frame with columns for PANEL, ROW, COL, and facetting vars
  combine_vars = function(data, vars = NULL, drop = TRUE) {
    if (length(vars) == 0) return(data.frame())

    # For each layer, compute the facet values
    values <- compact(plyr::llply(data, quoted_df, vars = vars))

    # Form the base data frame which contains all combinations of facetting
    # variables that appear in the data
    has_all <- unlist(plyr::llply(values, length)) == length(vars)
    if (!any(has_all)) {
      stop("At least one layer must contain all variables used for facetting")
    }

    base <- unique(plyr::ldply(values[has_all]))
    if (!drop) {
      base <- unique_combs(base)
    }

    # Systematically add on missing combinations
    for (value in values[!has_all]) {
      if (empty(value)) next;

      old <- base[setdiff(names(base), names(value))]
      new <- unique(value[intersect(names(base), names(value))])
      if (drop) {
        new <- unique_combs(new)
      }
      base <- rbind(base, df.grid(old, new))
    }

    if (empty(base)) {
      stop("Faceting variables must have at least one value", call. = FALSE)
    }

    base
  },
  render_axes = function(x = NULL, y = NULL, coord, theme, transpose = FALSE) {
    axes <- list()
    if (!is.null(x)) {
      axes$x <- lapply(x, coord$render_axis_h, theme)
    }
    if (!is.null(y)) {
      axes$y <- lapply(y, coord$render_axis_v, theme)
    }
    if (transpose) {
      axes <- list(
        x = list(
          top = lapply(axes$x, `[[`, "top"),
          bottom = lapply(axes$x, `[[`, "bottom")
        ),
        y = list(
          left = lapply(axes$y, `[[`, "left"),
          right = lapply(axes$y, `[[`, "right")
        )
      )
    }
    axes
  },
  render_strips = function(x = NULL, y = NULL, labeller, theme) {
    list(
      x = build_strip(x, labeller, theme, TRUE),
      y = build_strip(y, labeller, theme, FALSE)
    )
  }
)

# Helpers -----------------------------------------------------------------

# A "special" value, currently not used but could be used to determine
# if faceting is active
NO_PANEL <- -1L

unique_combs <- function(df) {
  if (length(df) == 0) return()

  unique_values <- plyr::llply(df, ulevels)
  rev(expand.grid(rev(unique_values), stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = TRUE))
}

df.grid <- function(a, b) {
  if (nrow(a) == 0) return(b)
  if (nrow(b) == 0) return(a)

  indexes <- expand.grid(
    i_a = seq_len(nrow(a)),
    i_b = seq_len(nrow(b))
  )
  plyr::unrowname(cbind(
    a[indexes$i_a, , drop = FALSE],
    b[indexes$i_b, , drop = FALSE]
  ))
}

quoted_df <- function(data, vars) {
  values <- plyr::eval.quoted(vars, data, emptyenv(), try = TRUE)
  as.data.frame(compact(values), optional = TRUE, stringsAsFactors = FALSE)
}

layout_null <- function() {
  data.frame(PANEL = 1, ROW = 1, COL = 1, SCALE_X = 1, SCALE_Y = 1)
}

max_height <- function(grobs) {
  unit(max(unlist(lapply(grobs, height_cm))), "cm")
}
max_width <- function(grobs) {
  unit(max(unlist(lapply(grobs, width_cm))), "cm")
}
