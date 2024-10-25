#' Build ggplot for rendering.
#'
#' `ggplot_build()` takes the plot object, and performs all steps necessary
#' to produce an object that can be rendered.  This function outputs two pieces:
#' a list of data frames (one for each layer), and a panel object, which
#' contain all information about axis limits, breaks etc.
#'
#' `get_layer_data()`, `get_layer_grob()`, and `get_panel_scales()` are helper
#' functions that return the data, grob, or scales associated with a given
#' layer. These are useful for tests.
#'
#' @param plot ggplot object
#' @param i An integer. In `get_layer_data()`, the data to return (in the order added to the
#'   plot). In `get_layer_grob()`, the grob to return (in the order added to the
#'   plot). In `get_panel_scales()`, the row of a facet to return scales for.
#' @param j An integer. In `get_panel_scales()`, the column of a facet to return
#'   scales for.
#' @seealso
#' [print.ggplot()] and [benchplot()] for
#' functions that contain the complete set of steps for generating
#' a ggplot2 plot.
#'
#' The `r link_book("build step section", "internals#sec-ggplotbuild")`
#' @keywords internal
#' @export
ggplot_build <- function(plot) {
  # Attaching the plot env to be fetched by deprecations etc.
  attach_plot_env(plot$plot_env)

  UseMethod('ggplot_build')
}

#' @export
ggplot_build.ggplot_built <- function(plot) {
  # This is a no-op
  plot
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
  layout <- create_layout(plot$facet, plot$coordinates, plot$layout)
  data <- layout$setup(data, plot$data, plot$plot_env)

  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot), layers, data, "computing aesthetics")
  plot$labels <- setup_plot_labels(plot, layers, data)
  data <- .ignore_data(data)

  # Transform all scales
  data <- lapply(data, scales$transform_df)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)
  data <- .expose_data(data)

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
  data <- .ignore_data(data)
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  layout$setup_panel_params()
  data <- layout$map_position(data)

  # Hand off position guides to layout
  layout$setup_panel_guides(plot$guides, plot$layers)

  # Complete the plot's theme
  plot$theme <- plot_theme(plot)

  # Train and map non-position scales and guides
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, npscales$train_df)
    plot$guides <- plot$guides$build(npscales, plot$layers, plot$labels, data, plot$theme)
    data <- lapply(data, npscales$map_df)
  } else {
    # Only keep custom guides if there are no non-position scales
    plot$guides <- plot$guides$get_custom()
  }
  data <- .expose_data(data)

  # Fill in defaults etc.
  data <- by_layer(
    function(l, d) l$compute_geom_2(d, theme = plot$theme),
    layers, data, "setting up geom aesthetics"
  )

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
get_layer_data <- function(plot = get_last_plot(), i = 1L) {
  ggplot_build(plot)$data[[i]]
}
#' @export
#' @rdname ggplot_build
layer_data <- get_layer_data

#' @export
#' @rdname ggplot_build
get_panel_scales <- function(plot = get_last_plot(), i = 1L, j = 1L) {
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
layer_scales <- get_panel_scales

#' @export
#' @rdname ggplot_build
get_layer_grob <- function(plot = get_last_plot(), i = 1L) {
  b <- ggplot_build(plot)

  b$plot$layers[[i]]$draw_geom(b$data[[i]], b$layout)
}

#' @export
#' @rdname ggplot_build
layer_grob <- get_layer_grob

#' Build a plot with all the usual bits and pieces.
#'
#' This function builds all grobs necessary for displaying the plot, and
#' stores them in a special data structure called a [gtable()].
#' This object is amenable to programmatic manipulation, should you want
#' to (e.g.) make the legend box 2 cm wide, or combine multiple plots into
#' a single display, preserving aspect ratios across the plots.
#'
#' @seealso
#' [print.ggplot()] and [benchplot()] for
#' for functions that contain the complete set of steps for generating
#' a ggplot2 plot.
#'
#' The `r link_book("gtable step section", "internals#sec-ggplotgtable")`
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
  theme <- plot$theme

  geom_grobs <- by_layer(function(l, d) l$draw_geom(d, layout), plot$layers, data, "converting geom to grob")

  plot_table <- layout$render(geom_grobs, data, theme, plot$labels)

  # Legends
  legend_box <- plot$guides$assemble(theme)
  plot_table <- table_add_legends(plot_table, legend_box, theme)

  # Title
  title <- element_render(
    theme, "plot.title", plot$labels$title,
    margin_y = TRUE, margin_x = TRUE
  )
  title_height <- grobHeight(title)

  # Subtitle
  subtitle <- element_render(
    theme, "plot.subtitle", plot$labels$subtitle,
    margin_y = TRUE, margin_x = TRUE
  )
  subtitle_height <- grobHeight(subtitle)

  # whole plot annotation
  caption <- element_render(
    theme, "plot.caption", plot$labels$caption,
    margin_y = TRUE, margin_x = TRUE
  )
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
    title_l <- min(pans$l)
    title_r <- max(pans$r)
  } else {
    title_l <- 1
    title_r <- ncol(plot_table)
  }
  if (caption_pos == "panel") {
    caption_l <- min(pans$l)
    caption_r <- max(pans$r)
  } else {
    caption_l <- 1
    caption_r <- ncol(plot_table)
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
  plot_margin <- calc_element("plot.margin", theme)
  plot_table  <- gtable_add_padding(plot_table, plot_margin)

  if (is.theme_element(theme$plot.background)) {
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
    # Re-render with manual positions
    tag <- element_grob(
      element, x = x, y = y, label = label,
      margin_y = TRUE, margin_x = TRUE
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

# Add the legends to the gtable
table_add_legends <- function(table, legends, theme) {

  if (is.zero(legends)) {
    legends <- rep(list(zeroGrob()), 5)
    names(legends) <- c(.trbl, "inside")
  }

  # Extract sizes
  widths <- heights <- set_names(
    rep(list(unit(0, "cm")), length(legends)),
    names(legends)
  )

  empty <- vapply(legends, is.zero, logical(1))
  widths[!empty]  <- lapply(legends[!empty], gtable_width)
  heights[!empty] <- lapply(legends[!empty], gtable_height)
  spacing <- calc_element("legend.box.spacing", theme) %||% unit(0.2, "cm")

  # If legend is missing, set spacing to zero for that legend
  zero    <- unit(0, "pt")
  spacing <- lapply(empty, function(is_empty) if (is_empty) zero else spacing)

  location <- switch(
    theme$legend.location %||% "panel",
    "plot" = plot_extent,
    find_panel
  )

  place <- location(table)

  # Add right legend
  table <- gtable_add_cols(table, spacing$right, pos = -1)
  table <- gtable_add_cols(table, widths$right,  pos = -1)
  table <- gtable_add_grob(
    table, legends$right, clip = "off",
    t = place$t, b = place$b, l = -1, r = -1,
    name = "guide-box-right"
  )

  # Add left legend
  table <- gtable_add_cols(table, spacing$left, pos = 0)
  table <- gtable_add_cols(table, widths$left,  pos = 0)
  table <- gtable_add_grob(
    table, legends$left, clip = "off",
    t = place$t, b = place$b, l = 1, r = 1,
    name = "guide-box-left"
  )

  place <- location(table)

  # Add bottom legend
  table <- gtable_add_rows(table, spacing$bottom, pos = -1)
  table <- gtable_add_rows(table, heights$bottom, pos = -1)
  table <- gtable_add_grob(
    table, legends$bottom, clip = "off",
    t = -1, b = -1, l = place$l, r = place$r,
    name = "guide-box-bottom"
  )

  # Add top legend
  table <- gtable_add_rows(table, spacing$top, pos = 0)
  table <- gtable_add_rows(table, heights$top, pos = 0)
  table <- gtable_add_grob(
    table, legends$top, clip = "off",
    t = 1, b = 1, l = place$l, r = place$r,
    name = "guide-box-top"
  )

  # Add manual legend
  place <- find_panel(table)
  table <- gtable_add_grob(
    table, legends$inside, clip = "off",
    t = place$t, b = place$b, l = place$l, r = place$r,
    name = "guide-box-inside"
  )

  table
}

plot_extent <- function(table) {
  layout <- table$layout
  data_frame0(
    t = min(layout[["t"]]),
    r = max(layout[["r"]]),
    b = max(layout[["b"]]),
    l = min(layout[["l"]]),
    .size = 1L
  )
}
