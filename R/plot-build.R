#' Build ggplot for rendering.
#'
#' `build_ggplot()` takes the plot object, and performs all steps necessary
#' to produce an object that can be rendered.  This function outputs two pieces:
#' a list of data frames (one for each layer), and a panel object, which
#' contain all information about axis limits, breaks etc. The `ggplot_build()`
#' function is vestigial and `build_ggplot()` should be used instead.
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
#' @param ... Not currently in use.
#' @seealso
#' [print.ggplot()] and [benchplot()] for
#' functions that contain the complete set of steps for generating
#' a ggplot2 plot.
#'
#' The `r link_book("build step section", "internals#sec-ggplotbuild")`
#' @keywords internal
#' @export
ggplot_build <- function(plot, ...) {
  # TODO: Swap to S7 generic once S7/#543 is resolved
  env <- try_prop(plot, "plot_env")
  if (!is.null(env)) {
    attach_plot_env(env)
  }
  UseMethod("ggplot_build")
}

S7::method(ggplot_build, class_ggplot_built) <- function(plot, ...) {
  plot # This is a no-op
}

# The build_ggplot is a temporary concession to {thematic} after we put in
# a compatibility PR that uses this function
build_ggplot <- S7::method(ggplot_build, class_ggplot) <- function(plot, ...) {
  plot <- plot_clone(plot)
  if (length(plot@layers) == 0) {
    plot <- plot + geom_blank()
  }

  layers <- plot@layers
  data <- rep(list(NULL), length(layers))

  scales <- plot@scales

  # Allow all layers to make any final adjustments based
  # on raw input data and plot info
  data <- by_layer(function(l, d) l$layer_data(plot@data), layers, data, "computing layer data")
  data <- by_layer(function(l, d) l$setup_layer(d, plot), layers, data, "setting up layer")

  # Initialise panels, add extra data for margins & missing faceting
  # variables, and add on a PANEL variable to data
  layout <- create_layout(plot@facet, plot@coordinates, plot@layout)
  data <- layout$setup(data, plot@data, plot@plot_env)

  # Compute aesthetics to produce data with generalised variable names
  data <- by_layer(function(l, d) l$compute_aesthetics(d, plot), layers, data, "computing aesthetics")
  plot@labels <- setup_plot_labels(plot, layers, data)
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
  plot@scales$add_missing(c("x", "y"), plot@plot_env)

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
  layout$setup_panel_guides(plot@guides, plot@layers)

  # Complete the plot's theme
  plot@theme <- plot_theme(plot)

  # Train and map non-position scales and guides
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    npscales$set_palettes(plot@theme)
    lapply(data, npscales$train_df)
    plot@guides <- plot@guides$build(npscales, plot@layers, plot@labels, data, plot@theme)
    data <- lapply(data, npscales$map_df)
  } else {
    # Only keep custom guides if there are no non-position scales
    plot@guides <- plot@guides$get_custom()
  }
  data <- .expose_data(data)

  # Fill in defaults etc.
  data <- by_layer(
    function(l, d) l$compute_geom_2(d, theme = plot@theme),
    layers, data, "setting up geom aesthetics"
  )

  # Let layer stat have a final say before rendering
  data <- by_layer(function(l, d) l$finish_statistics(d), layers, data, "finishing layer stat")

  # Let Layout modify data before rendering
  data <- layout$finish_data(data)

  # Consolidate alt-text
  plot@labels$alt <- get_alt_text(plot)

  build <- class_ggplot_built(data = data, layout = layout, plot = plot)
  class(build) <- union(c("ggplot2::ggplot_built", "ggplot_built"), class(build))
  build
}

#' @export
#' @rdname ggplot_build
get_layer_data <- function(plot = get_last_plot(), i = 1L) {
  ggplot_build(plot)@data[[i]]
}
#' @export
#' @rdname ggplot_build
layer_data <- get_layer_data

#' @export
#' @rdname ggplot_build
get_panel_scales <- function(plot = get_last_plot(), i = 1L, j = 1L) {
  b <- ggplot_build(plot)

  layout <- b@layout$layout
  selected <- layout[layout$ROW == i & layout$COL == j, , drop = FALSE]

  list(
    x = b@layout$panel_scales_x[[selected$SCALE_X]],
    y = b@layout$panel_scales_y[[selected$SCALE_Y]]
  )
}

#' @export
#' @rdname ggplot_build
layer_scales <- get_panel_scales

#' @export
#' @rdname ggplot_build
get_layer_grob <- function(plot = get_last_plot(), i = 1L) {
  b <- ggplot_build(plot)

  b@plot@layers[[i]]$draw_geom(b@data[[i]], b@layout)
}

#' @export
#' @rdname ggplot_build
layer_grob <- get_layer_grob

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
