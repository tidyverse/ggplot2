#' Build ggplot for rendering.
#'
#' `ggplot_build()` takes the plot object, and performs all steps necessary
#' to produce an object that can be rendered.  This function outputs two pieces:
#' a list of data frames (one for each layer), and a panel object, which
#' contain all information about axis limits, breaks etc.
#'
#' `layer_data()`, `layer_grob()`, and `layer_scales()` are helper
#' functions that return the data, grob, or scales associated with a given
#' layer. These are useful for tests.
#'
#' @param plot ggplot object
#' @param i An integer. In `layer_data()`, the data to return (in the order added to the
#'   plot). In `layer_grob()`, the grob to return (in the order added to the
#'   plot). In `layer_scales()`, the row of a facet to return scales for.
#' @param j An integer. In `layer_scales()`, the column of a facet to return
#'   scales for.
#' @seealso [print.ggplot()] and [benchplot()] for
#'  functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @keywords internal
#' @export
ggplot_build <- function(plot) {
  # Attaching the plot env to be fetched by deprecations etc.
  attach_plot_env(plot$plot_env)

  UseMethod('ggplot_build')
}

#' @export
ggplot_build.ggplot <- function(plot) {
  plot <- plot_clone(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }

  layers <- plot$layers
  data <- rep(list(NULL), length(layers))

  scales <- plot$scales

  # Allow all layers to make any final adjustments based
  # on raw input data and plot info
  data <- by_layer(function(l, d) l$layer_data(plot$data), layers, data, "computing layer data")
  data <- by_layer(function(l, d) l$setup_layer(d, plot), layers, data, "setting up layer")

  # Initialise panels, add extra data for margins & missing faceting
  # variables, and add on a PANEL variable to data
  layout <- create_layout(plot$facet, plot$coordinates)
  data <- layout$setup(data, plot$data, plot$plot_env)

  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot), layers, data, "computing aesthetics")

  # Transform all scales
  data <- lapply(data, scales$transform_df)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)

  # Apply and map statistics
  data <- by_layer(function(l, d) l$compute_statistic(d, layout), layers, data, "computing stat")
  data <- by_layer(function(l, d) l$map_statistic(d, plot), layers, data, "mapping stat to aesthetics")

  # Make sure missing (but required) aesthetics are added
  plot$scales$add_missing(c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- by_layer(function(l, d) l$compute_geom_1(d), layers, data, "setting up geom")

  # Apply position adjustments
  data <- by_layer(function(l, d) l$compute_position(d, layout), layers, data, "computing position")

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what is
  # displayed, or does it include the range of underlying data
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  layout$setup_panel_params()
  data <- layout$map_position(data)

  # Hand off position guides to layout
  layout$setup_panel_guides(plot$guides, plot$layers)

  # Train and map non-position scales and guides
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, npscales$train_df)
    plot$guides <- plot$guides$build(npscales, plot$layers, plot$labels, data)
    data <- lapply(data, npscales$map_df)
  } else {
    # Only keep custom guides if there are no non-position scales
    plot$guides <- plot$guides$get_custom()
  }

  # Fill in defaults etc.
  data <- by_layer(function(l, d) l$compute_geom_2(d), layers, data, "setting up geom aesthetics")

  # Let layer stat have a final say before rendering
  data <- by_layer(function(l, d) l$finish_statistics(d), layers, data, "finishing layer stat")

  # Let Layout modify data before rendering
  data <- layout$finish_data(data)

  # Consolidate alt-text
  plot$labels$alt <- get_alt_text(plot)

  structure(
    list(data = data, layout = layout, plot = plot),
    class = "ggplot_built"
  )
}

#' @export
#' @rdname ggplot_build
layer_data <- function(plot = last_plot(), i = 1L) {
  ggplot_build(plot)$data[[i]]
}

#' @export
#' @rdname ggplot_build
layer_scales <- function(plot = last_plot(), i = 1L, j = 1L) {
  b <- ggplot_build(plot)

  layout <- b$layout$layout
  selected <- layout[layout$ROW == i & layout$COL == j, , drop = FALSE]

  list(
    x = b$layout$panel_scales_x[[selected$SCALE_X]],
    y = b$layout$panel_scales_y[[selected$SCALE_Y]]
  )
}

#' @export
#' @rdname ggplot_build
layer_grob <- function(plot = last_plot(), i = 1L) {
  b <- ggplot_build(plot)

  b$plot$layers[[i]]$draw_geom(b$data[[i]], b$layout)
}

#' Build a plot with all the usual bits and pieces.
#'
#' This function builds all grobs necessary for displaying the plot, and
#' stores them in a special data structure called a [gtable()].
#' This object is amenable to programmatic manipulation, should you want
#' to (e.g.) make the legend box 2 cm wide, or combine multiple plots into
#' a single display, preserving aspect ratios across the plots.
#'
#' @seealso [print.ggplot()] and [benchplot()] for
#'  for functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @return a [gtable()] object
#' @keywords internal
#' @param data plot data generated by [ggplot_build()]
#' @export
ggplot_gtable <- function(data) {
  # Attaching the plot env to be fetched by deprecations etc.
  attach_plot_env(data$plot$plot_env)

  UseMethod('ggplot_gtable')
}

#' @export
ggplot_gtable.ggplot_built <- function(data) {
  plot <- data$plot
  layout <- data$layout
  data <- data$data
  theme <- plot_theme(plot)

  geom_grobs <- by_layer(function(l, d) l$draw_geom(d, layout), plot$layers, data, "converting geom to grob")

  plot_table <- layout$render(geom_grobs, data, theme, plot$labels)

  # Legends
  position <- theme$legend.position %||% "right"
  if (length(position) == 2) {
    position <- "manual"
  }

  legend_box <- plot$guides$assemble(theme, position)

  if (is.zero(legend_box)) {
    position <- "none"
  } else {
    # these are a bad hack, since it modifies the contents of viewpoint directly...
    legend_width  <- gtable_width(legend_box)
    legend_height <- gtable_height(legend_box)

    # Set the justification of the legend box
    # First value is xjust, second value is yjust
    just <- valid.just(theme$legend.justification)
    xjust <- just[1]
    yjust <- just[2]

    if (position == "manual") {
      xpos <- theme$legend.position[1]
      ypos <- theme$legend.position[2]

      # x and y are specified via theme$legend.position (i.e., coords)
      legend_box <- editGrob(
        legend_box,
        vp = viewport(
          x = xpos,
          y = ypos,
          just = c(xjust, yjust),
          height = legend_height,
          width = legend_width
        )
      )
    } else {
      # x and y are adjusted using justification of legend box (i.e., theme$legend.justification)
      legend_box <- editGrob(
        legend_box,
        vp = viewport(
          x = xjust,
          y = yjust,
          just = c(xjust, yjust),
          height = legend_height,
          width = legend_width
        )
      )
      legend_box <- gtable_add_rows(legend_box, unit(yjust, 'null'))
      legend_box <- gtable_add_rows(legend_box, unit(1 - yjust, 'null'), 0)
      legend_box <- gtable_add_cols(legend_box, unit(xjust, 'null'), 0)
      legend_box <- gtable_add_cols(legend_box, unit(1 - xjust, 'null'))
    }
  }

  panel_dim <-  find_panel(plot_table)
  # for align-to-device, use this:
  # panel_dim <-  summarise(plot_table$layout, t = min(t), r = max(r), b = max(b), l = min(l))

  theme$legend.box.spacing <- theme$legend.box.spacing %||% unit(0.2, 'cm')
  if (position == "left") {
    plot_table <- gtable_add_cols(plot_table, theme$legend.box.spacing, pos = 0)
    plot_table <- gtable_add_cols(plot_table, legend_width, pos = 0)
    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
      t = panel_dim$t, b = panel_dim$b, l = 1, r = 1, name = "guide-box")
  } else if (position == "right") {
    plot_table <- gtable_add_cols(plot_table, theme$legend.box.spacing, pos = -1)
    plot_table <- gtable_add_cols(plot_table, legend_width, pos = -1)
    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
      t = panel_dim$t, b = panel_dim$b, l = -1, r = -1, name = "guide-box")
  } else if (position == "bottom") {
    plot_table <- gtable_add_rows(plot_table, theme$legend.box.spacing, pos = -1)
    plot_table <- gtable_add_rows(plot_table, legend_height, pos = -1)
    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
      t = -1, b = -1, l = panel_dim$l, r = panel_dim$r, name = "guide-box")
  } else if (position == "top") {
    plot_table <- gtable_add_rows(plot_table, theme$legend.box.spacing, pos = 0)
    plot_table <- gtable_add_rows(plot_table, legend_height, pos = 0)
    plot_table <- gtable_add_grob(plot_table, legend_box, clip = "off",
      t = 1, b = 1, l = panel_dim$l, r = panel_dim$r, name = "guide-box")
  } else if (position == "manual") {
    # should guide box expand whole region or region without margin?
    plot_table <- gtable_add_grob(plot_table, legend_box,
      t = panel_dim$t, b = panel_dim$b, l = panel_dim$l, r = panel_dim$r,
      clip = "off", name = "guide-box")
  }

  # Title
  title <- element_render(theme, "plot.title", plot$labels$title, margin_y = TRUE)
  title_height <- grobHeight(title)

  # Subtitle
  subtitle <- element_render(theme, "plot.subtitle", plot$labels$subtitle, margin_y = TRUE)
  subtitle_height <- grobHeight(subtitle)

  # whole plot annotation
  caption <- element_render(theme, "plot.caption", plot$labels$caption, margin_y = TRUE)
  caption_height <- grobHeight(caption)

  # positioning of title and subtitle is governed by plot.title.position
  # positioning of caption is governed by plot.caption.position
  #   "panel" means align to the panel(s)
  #   "plot" means align to the entire plot (except margins and tag)
  title_pos <- arg_match0(
    theme$plot.title.position %||% "panel",
    c("panel", "plot"),
    arg_nm = "plot.title.position",
    error_call = expr(theme())
  )

  caption_pos <- arg_match0(
    theme$plot.caption.position %||% "panel",
    values = c("panel", "plot"),
    arg_nm = "plot.caption.position",
    error_call = expr(theme())
  )

  pans <- plot_table$layout[grepl("^panel", plot_table$layout$name), , drop = FALSE]
  if (title_pos == "panel") {
    title_l = min(pans$l)
    title_r = max(pans$r)
  } else {
    title_l = 1
    title_r = ncol(plot_table)
  }
  if (caption_pos == "panel") {
    caption_l = min(pans$l)
    caption_r = max(pans$r)
  } else {
    caption_l = 1
    caption_r = ncol(plot_table)
  }

  plot_table <- gtable_add_rows(plot_table, subtitle_height, pos = 0)
  plot_table <- gtable_add_grob(plot_table, subtitle, name = "subtitle",
    t = 1, b = 1, l = title_l, r = title_r, clip = "off")

  plot_table <- gtable_add_rows(plot_table, title_height, pos = 0)
  plot_table <- gtable_add_grob(plot_table, title, name = "title",
    t = 1, b = 1, l = title_l, r = title_r, clip = "off")

  plot_table <- gtable_add_rows(plot_table, caption_height, pos = -1)
  plot_table <- gtable_add_grob(plot_table, caption, name = "caption",
    t = -1, b = -1, l = caption_l, r = caption_r, clip = "off")

  plot_table <- table_add_tag(plot_table, plot$labels$tag, theme)

  # Margins
  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[1], pos = 0)
  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[2])
  plot_table <- gtable_add_rows(plot_table, theme$plot.margin[3])
  plot_table <- gtable_add_cols(plot_table, theme$plot.margin[4], pos = 0)

  if (inherits(theme$plot.background, "element")) {
    plot_table <- gtable_add_grob(plot_table,
      element_render(theme, "plot.background"),
      t = 1, l = 1, b = -1, r = -1, name = "background", z = -Inf)
    plot_table$layout <- plot_table$layout[c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1)),]
    plot_table$grobs <- plot_table$grobs[c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1))]
  }

  # add alt-text as attribute
  attr(plot_table, "alt-label") <- plot$labels$alt

  plot_table
}

#' Generate a ggplot2 plot grob.
#'
#' @param x ggplot2 object
#' @keywords internal
#' @export
ggplotGrob <- function(x) {
  ggplot_gtable(ggplot_build(x))
}

# Apply function to layer and matching data
by_layer <- function(f, layers, data, step = NULL) {
  ordinal <- label_ordinal()
  out <- vector("list", length(data))
  try_fetch(
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    },
    error = function(cnd) {
      cli::cli_abort(c(
        "Problem while {step}.",
        "i" = "Error occurred in the {ordinal(i)} layer."),
        call = layers[[i]]$constructor,
        parent = cnd
      )
    }
  )
  out
}

# Add the tag element to the gtable
table_add_tag <- function(table, label, theme) {
  # Initialise the tag margins
  table <- gtable_add_padding(table, unit(0, "pt"))

  # Early exit when label is absent or element is blank
  if (length(label) < 1) {
    return(table)
  }
  element <- calc_element("plot.tag", theme)
  if (inherits(element, "element_blank")) {
    return(table)
  }

  # Resolve position
  position <- calc_element("plot.tag.position", theme) %||% "topleft"
  location <- calc_element("plot.tag.location", theme) %||%
    (if (is.numeric(position)) "plot" else "margin")

  if (is.numeric(position)) {
    if (location == "margin") {
      cli::cli_abort(paste0(
        "A {.cls numeric} {.arg plot.tag.position} cannot be used with ",
        "`{.val margin}` as {.arg plot.tag.location}."
      ),
      call = expr(theme()))
    }
    if (length(position) != 2) {
      cli::cli_abort(paste0(
        "A {.cls numeric} {.arg plot.tag.position} ",
        "theme setting must have length 2."
      ),
      call = expr(theme()))
    }
    top <- left <- right <- bottom <- FALSE
  } else {
    # Break position into top/left/right/bottom
    position <- arg_match0(
      position[1],
      c("topleft", "top", "topright", "left",
        "right", "bottomleft", "bottom", "bottomright"),
      arg_nm = "plot.tag.position",
      error_call = expr(theme())
    )
    top    <- position %in% c("topleft",    "top",    "topright")
    left   <- position %in% c("topleft",    "left",   "bottomleft")
    right  <- position %in% c("topright",   "right",  "bottomright")
    bottom <- position %in% c("bottomleft", "bottom", "bottomright")
  }

  # Resolve tag and sizes
  tag <- element_grob(element, label = label, margin_y = TRUE, margin_x = TRUE)
  height <- grobHeight(tag)
  width  <- grobWidth(tag)

  if (location %in% c("plot", "panel")) {
    if (!is.numeric(position)) {
      if (right || left) {
        x <- (1 - element$hjust) * width
        if (right) {
          x <- unit(1, "npc") - x
        }
      } else {
        x <- unit(element$hjust, "npc")
      }
      if (top || bottom) {
        y <- (1 - element$vjust) * height
        if (top) {
          y <- unit(1, "npc") - y
        }
      } else {
        y <- unit(element$vjust, "npc")
      }
    } else {
      x <- unit(position[1], "npc")
      y <- unit(position[2], "npc")
    }
    # Do manual placement of tag
    tag <- justify_grobs(
      tag, x = x, y = y,
      hjust = element$hjust, vjust = element$vjust,
      int_angle = element$angle, debug = element$debug
    )
    if (location == "plot") {
      table <- gtable_add_grob(
        table, tag, name = "tag", clip = "off",
        t = 1, b = nrow(table), l = 1, r = ncol(table)
      )
      return(table)
    }
  }

  if (location == "panel") {
    place <- find_panel(table)
  } else {
    n_col <- ncol(table)
    n_row <- nrow(table)
    # Actually fill margin with relevant units
    if (top)    table$heights <- unit.c(height, table$heights[-1])
    if (left)   table$widths  <- unit.c(width,  table$widths[-1])
    if (right)  table$widths  <- unit.c(table$widths[-n_col],  width)
    if (bottom) table$heights <- unit.c(table$heights[-n_row], height)
    place <- data_frame0(t = 1L, r = n_col, b = n_row, l = 1L)
  }

  # Shrink placement to position
  if (top)    place$b <- place$t
  if (left)   place$r <- place$l
  if (right)  place$l <- place$r
  if (bottom) place$t <- place$b

  gtable_add_grob(
    table, tag, name = "tag", clip = "off",
    t = place$t, l = place$l, b = place$b, r = place$r
  )
}
