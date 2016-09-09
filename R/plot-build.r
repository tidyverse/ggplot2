#' Build ggplot for rendering.
#'
#' \code{ggplot_build} takes the plot object, and performs all steps necessary
#' to produce an object that can be rendered.  This function outputs two pieces:
#' a list of data frames (one for each layer), and a panel object, which
#' contain all information about axis limits, breaks etc.
#'
#' \code{layer_data}, \code{layer_grob}, and \code{layer_scales} are helper
#' functions that returns the data, grob, or scales associated with a given
#' layer. These are useful for tests.
#'
#' @param plot ggplot object
#' @seealso \code{\link{print.ggplot}} and \code{\link{benchplot}} for
#'  functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @keywords internal
#' @export
ggplot_build <- function(plot) {
  plot <- plot_clone(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }

  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$layer_data(plot$data))

  scales <- plot$scales
  # Apply function to layer and matching data
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data

  layout <- create_layout(plot$facet)
  layout$train(layer_data, plot$data)
  data <- layout$map(layer_data)

  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))

  # Transform all scales
  data <- lapply(data, scales_transform_df, scales = scales)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)

  # Apply and map statistics
  data <- by_layer(function(l, d) l$compute_statistic(d, layout))
  data <- by_layer(function(l, d) l$map_statistic(d, plot))

  # Make sure missing (but required) aesthetics are added
  scales_add_missing(plot, c("x", "y"), plot$plot_env)

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- by_layer(function(l, d) l$compute_geom_1(d))

  # Apply position adjustments
  data <- by_layer(function(l, d) l$compute_position(d, layout))

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's
  # displayed, or does it include the range of underlying data
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }

  # Train coordinate system
  layout$train_ranges(plot$coordinates)

  # Fill in defaults etc.
  data <- by_layer(function(l, d) l$compute_geom_2(d))

  # Let Layout modify data before rendering
  data <- layout$finish_data(data)

  list(data = data, layout = layout, plot = plot)
}

#' @export
#' @rdname ggplot_build
layer_data <- function(plot, i = 1L) {
  ggplot_build(plot)$data[[i]]
}

#' @export
#' @rdname ggplot_build
layer_scales <- function(plot, i = 1L, j = 1L) {
  b <- ggplot_build(plot)

  layout <- b$panel$layout
  selected <- layout[layout$ROW == i & layout$COL == j, , drop = FALSE]

  list(
    x = b$panel$x_scales[[selected$SCALE_X]],
    y = b$panel$y_scales[[selected$SCALE_Y]]
  )
}

#' @export
#' @rdname ggplot_build
layer_grob <- function(plot, i = 1L) {
  b <- ggplot_build(plot)

  b$plot$layers[[i]]$draw_geom(b$data[[i]], b$panel, b$plot$coordinates)
}

#' Build a plot with all the usual bits and pieces.
#'
#' This function builds all grobs necessary for displaying the plot, and
#' stores them in a special data structure called a \code{\link{gtable}}.
#' This object is amenable to programmatic manipulation, should you want
#' to (e.g.) make the legend box 2 cm wide, or combine multiple plots into
#' a single display, preserving aspect ratios across the plots.
#'
#' @seealso \code{\link{print.ggplot}} and \code{link{benchplot}} for
#'  for functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @return a \code{\link{gtable}} object
#' @keywords internal
#' @param plot plot object
#' @param data plot data generated by \code{\link{ggplot_build}}
#' @export
ggplot_gtable <- function(data) {
  plot <- data$plot
  layout <- data$layout
  data <- data$data
  theme <- plot_theme(plot)

  geom_grobs <- Map(function(l, d) l$draw_geom(d, layout, plot$coordinates),
    plot$layers, data)

  plot_table <- layout$render(geom_grobs, data, plot$coordinates, theme, plot$labels)

  # Legends
  position <- theme$legend.position
  if (length(position) == 2) {
    position <- "manual"
  }

  legend_box <- if (position != "none") {
    build_guides(plot$scales, plot$layers, plot$mapping, position, theme, plot$guides, plot$labels)
  } else {
    zeroGrob()
  }

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
      legend_box <- editGrob(legend_box,
        vp = viewport(x = xpos, y = ypos, just = c(xjust, yjust),
          height = legend_height, width = legend_width))
    } else {
      # x and y are adjusted using justification of legend box (i.e., theme$legend.justification)
      legend_box <- editGrob(legend_box,
        vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust)))
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
  title <- element_render(theme, "plot.title", plot$labels$title, expand_y = TRUE)
  title_height <- grobHeight(title)

  # Subtitle
  subtitle <- element_render(theme, "plot.subtitle", plot$labels$subtitle, expand_y = TRUE)
  subtitle_height <- grobHeight(subtitle)

  # whole plot annotation
  caption <- element_render(theme, "plot.caption", plot$labels$caption, expand_y = TRUE)
  caption_height <- grobHeight(caption)

  pans <- plot_table$layout[grepl("^panel", plot_table$layout$name), ,
    drop = FALSE]

  plot_table <- gtable_add_rows(plot_table, subtitle_height, pos = 0)
  plot_table <- gtable_add_grob(plot_table, subtitle, name = "subtitle",
    t = 1, b = 1, l = min(pans$l), r = max(pans$r), clip = "off")

  plot_table <- gtable_add_rows(plot_table, title_height, pos = 0)
  plot_table <- gtable_add_grob(plot_table, title, name = "title",
    t = 1, b = 1, l = min(pans$l), r = max(pans$r), clip = "off")

  plot_table <- gtable_add_rows(plot_table, caption_height, pos = -1)
  plot_table <- gtable_add_grob(plot_table, caption, name = "caption",
    t = -1, b = -1, l = min(pans$l), r = max(pans$r), clip = "off")

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
