# The job of `Layout` is to coordinate:
# * The coordinate system
# * The faceting specification
# * The individual position scales for each panel
#
# This includes managing the parameters for the facet and the coord
# so that we don't modify the ggproto object in place.

create_layout <- function(facet, coord, layout = NULL) {
  layout <- layout %||% Layout
  check_inherits(layout, "Layout")
  ggproto(NULL, layout, facet = facet, coord = coord)
}

#' Layout
#'
#' @description
#' The Layout class is a chaperone class discouraged for extension. The class
#' fulfils the following tasks. The class houses the Coord and Facet classes
#' and tracks their stateful parameters. In addition, it manages the position
#' scales for each panel. It is responsible for keeping track of panel
#' specifications and matching pieces of the data to scales and parameters in
#' panel-wise manners.
#'
#' @details
#' The Layout class is only exported for extensions that re-implement a
#' `ggplot_build()` method for their specific class of plots. It is discouraged
#' to subclass the Layout class and for all purposes be considered an internal
#' structure. It has no user-facing constructor to put an small barrier in the
#' way.
#'
#' The class is used throughout `ggplot_build()`, with the notable exception of
#' the `render()` method, which is used in `ggplot_gtable()` instead.
#'
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @family Layout components
#' @family chaperone classes
#' @export
#' @examples
#' # Some dummy layout components
#' facet <- facet_null()
#' coord <- coord_cartesian()
#'
#' # Use in custom `ggplot_build()` methods
#' layout <- ggproto(NULL, Layout, facet = facet, coord = coord)
Layout <- ggproto(
  "Layout", NULL,

  # Fields ----------------------------------------------------------------

  #' @field coord,coord_params A [`<Coord>`][Coord] ggproto object and a list
  #' of the coordinate system's parameters. Parameters get populated by the
  #' `Coord$setup_params()` method.
  coord = NULL,
  coord_params = list(),

  # The faceting specification and its parameters
  #' @field facet,facet_params A [`<Facet>`][Facet] ggproto object and a list
  #' of the faceting specification's parameters. Parameters get populated by
  #' the `Facet$setup_params()` method.
  facet = NULL,
  facet_params = list(),

  #' @field layout A data frame with a row for each panel. The data frame
  #' contains integer columns `PANEL`, `SCALE_X`, `SCALE_Y`, `ROW` and `COL`
  #' representing a panel ID, scale indices and placement locations. In addition,
  #' the layout may contain faceting variables or other additional information.
  #' This field gets populated by the `Facet$compute_layout()` method.
  layout = NULL,

  #' @field panel_scales_x,panel_scales_y A list of `x` and `y` position scales
  #' parallel to the layout field's `SCALE_X` and `SCALE_Y` levels respectively.
  #' This fields gets populated by the `Facet$init_scales()` method.
  panel_scales_x = NULL,
  panel_scales_y = NULL,

  #' @field panel_params A named list of parameters per panel populated by the
  #' `Coord$setup_panel_params()` method. Contains `<ViewScale>` entries for
  #' the `x` and `y` variables in addition to ranges and other information the
  #' coordinate system might need to transform or render guides and grids.
  panel_params = NULL,

  # Methods -----------------------------------------------------------------

  ## ggplot_build -----------------------------------------------------------

  #' @field setup
  #' **Description**
  #'
  #' A function method for setting up the relevant information for the layout
  #' of the plot. It populates the `facet_params`, `coord_params` and `layout`
  #' fields and appends a `PANEL` variable to the layer data.
  #'
  #' **Usage**
  #' ```r
  #' Layout$setup(data, plot_data, plot_env)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A list of data frames with layer data.}
  #'   \item{`plot_data`}{The data frame in the `data` field of the ggplot
  #'   object.}
  #'   \item{`plot_env`}{The environment in the `plot_env` field of the
  #'   ggplot object.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of data frames from the `data` argument with a `PANEL` variable
  #' corresponding to rows in the `layout` field.
  #' Also called for the side effects of populating fields.
  setup = function(self, data, plot_data = data_frame0(), plot_env = emptyenv()) {
    data <- c(list(plot_data), data)

    # Setup facets
    self$facet_params <- self$facet$setup_params(data, self$facet$params)
    self$facet_params$plot_env <- plot_env
    data <- self$facet$setup_data(data, self$facet_params)

    # Setup coords
    self$coord_params <- self$coord$setup_params(data)
    data <- self$coord$setup_data(data, self$coord_params)

    # Generate panel layout
    self$layout <- self$facet$compute_layout(data, self$facet_params)
    self$layout <- self$coord$setup_layout(self$layout, self$coord_params)
    check_layout(self$layout)

    # Add panel coordinates to the data for each layer
    lapply(data[-1], self$facet$map_data,
      layout = self$layout,
      params = self$facet_params
    )
  },

  #' @field train_position
  #' **Description**
  #'
  #' A function method for training position scales and optionally initiating
  #' them. Implementation is via the `Facet$train_scales()` and
  #' `Facet$init_scales()` methods.
  #'
  #' **Usage**
  #' ```r
  #' Layout$train_position(data, x_scale, y_scale)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A list of data frames with layer data.}
  #'   \item{`x_scale`,`y_scale`}{A single prototype position scale for the `x`
  #'   and `y` aesthetics respectively.}
  #' }
  #'
  #' **Value**
  #'
  #' Nothing, this method is called for the side effect of training scales and
  #' optionally populating the `panel_scales_x` and `panel_scales_y` fields.
  train_position = function(self, data, x_scale, y_scale) {
    # Initialise scales if needed, and possible.
    layout <- self$layout
    if (is.null(self$panel_scales_x)) {
      self$panel_scales_x <- self$facet$init_scales(layout, x_scale = x_scale,
        params = self$facet_params)$x
    }
    if (is.null(self$panel_scales_y)) {
      self$panel_scales_y <- self$facet$init_scales(layout, y_scale = y_scale,
        params = self$facet_params)$y
    }

    self$facet$train_scales(
      self$panel_scales_x,
      self$panel_scales_y,
      layout,
      data,
      self$facet_params
    )
  },

  #' @field map_position
  #' **Description**
  #'
  #' A function method for mapping position aesthetics. For discrete scales this
  #' converts discrete levels to a numeric representation, usually integers. For
  #' continuous scales, this applies out-of-bounds handling.
  #'
  #' **Usage**
  #' ```r
  #' Layout$map_position(data)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A list of data frames with layer data.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of data frames per the `data` argument with mapped position
  #' aesthetics.
  map_position = function(self, data) {
    layout <- self$layout

    lapply(data, function(layer_data) {
      match_id <- NULL

      # Loop through each variable, mapping across each scale, then joining
      # back together
      x_vars <- intersect(self$panel_scales_x[[1]]$aesthetics, names(layer_data))
      if (length(x_vars) > 0) {
        match_id <- match(layer_data$PANEL, layout$PANEL)
        names(x_vars) <- x_vars
        SCALE_X <- layout$SCALE_X[match_id]
        new_x <- scale_apply(layer_data, x_vars, "map", SCALE_X, self$panel_scales_x)
        layer_data[, x_vars] <- new_x
      }

      y_vars <- intersect(self$panel_scales_y[[1]]$aesthetics, names(layer_data))
      if (length(y_vars) > 0) {
        if (is.null(match_id)) {
          match_id <- match(layer_data$PANEL, layout$PANEL)
        }
        names(y_vars) <- y_vars
        SCALE_Y <- layout$SCALE_Y[match_id]
        new_y <- scale_apply(layer_data, y_vars, "map", SCALE_Y, self$panel_scales_y)
        layer_data[, y_vars] <- new_y
      }

      layer_data
    })
  },

  #' @field reset_scales
  #' **Description**
  #'
  #' A function method for resetting scale ranges. After computing stats and
  #' position adjustments, scales need to be reset and re-trained to have an
  #' accurate measure of the data limits. This goes through the
  #' `panel_scales_x` and `panel_scales_y` fields and invokes the
  #' `Scale$reset()` method.
  #'
  #' **Usage**
  #' ```r
  #' Layout$reset_scales()
  #' ```
  #'
  #' **Value**
  #'
  #' Nothing, it is called for the side-effect of resetting scale ranges.
  reset_scales = function(self) {
    if (!self$facet$shrink) return()
    lapply(self$panel_scales_x, function(s) s$reset())
    lapply(self$panel_scales_y, function(s) s$reset())
    invisible()
  },

  #' @field setup_panel_params
  #' **Description**
  #'
  #' A function method for executing `Coord$setup_panel_params()` once per panel
  #' with the appropriate scales. For efficiency reasons, the setup is invoked
  #' once per unique combination of `x` and `y` scale.
  #'
  #' **Usage**
  #' ```r
  #' Layout$setup_panel_params()
  #' ```
  #'
  #' **Value**
  #'
  #' Nothing, it is called for the side effect of populating the `panel_params`
  #' field.
  setup_panel_params = function(self) {
    # Fudge for CoordFlip and CoordPolar - in place modification of
    # scales is not elegant, but it is pragmatic
    self$coord$modify_scales(self$panel_scales_x, self$panel_scales_y)

    # We only need to setup panel params once for unique combinations of x/y
    # scales. These will be repeated for duplicated combinations.
    index <- vec_unique_loc(self$layout$COORD)
    order <- vec_match(self$layout$COORD, self$layout$COORD[index])

    scales_x <- self$panel_scales_x[self$layout$SCALE_X[index]]
    scales_y <- self$panel_scales_y[self$layout$SCALE_Y[index]]

    panel_params <- Map(
      self$coord$setup_panel_params,
      scales_x, scales_y,
      MoreArgs = list(params = self$coord_params)
    )[order] # `[order]` does the repeating

    # Let Facet modify `panel_params` for each panel
    self$panel_params <- self$facet$setup_panel_params(panel_params, self$coord)

    invisible()
  },

  #' @field setup_panel_guides
  #' **Description**
  #'
  #' A function method for setting up and training the position guides (axes)
  #' once per panel with the appropriate scales. For efficiency reasons,
  #' the guides are setup once per unique  combination of `x` and `y` scale.
  #' It calls the `Coord$setup_panel_guides()` and `Coord$train_panel_guides()`
  #' methods.
  #'
  #' **Usage**
  #' ```r
  #' Layout$setup_panel_guides(guides, layers)
  #' ```
  #'
  #' **Arguments**
  #' \describe{
  #'   \item{`guides`}{A `<Guides>` ggproto object from the `guides` field of
  #'   the ggplot object.}
  #'   \item{`layers`}{A list of layers from the `layers` field of the ggplot
  #'   object.}
  #' }
  #'
  #' **Value**
  #'
  #' Nothing, it is called for the side effect of augmenting each entry of the
  #' `panel_params` field with position guides.
  setup_panel_guides = function(self, guides, layers) {

    # Like in `setup_panel_params`, we only need to setup guides for unique
    # combinations of x/y scales.
    index <- vec_unique_loc(self$layout$COORD)
    order <- vec_match(self$layout$COORD, self$layout$COORD[index])

    self$panel_params <- lapply(
      self$panel_params[index],
      self$coord$setup_panel_guides,
      guides,
      self$coord_params
    )

    self$panel_params <- lapply(
      self$panel_params,
      self$coord$train_panel_guides,
      layers,
      self$coord_params
    )[order]

    invisible()
  },

  #' @field setup_panel_guides
  #' **Description**
  #'
  #' A function method for setting up the `Facet$finish_data()` hook.
  #'
  #' **Usage**
  #' ```r
  #' Layout$finish_data(data)
  #' ```
  #'
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A list of data frames with layer data.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of data frames with layer data.
  finish_data = function(self, data) {
    lapply(data, self$facet$finish_data,
      layout = self$layout,
      x_scales = self$panel_scales_x,
      y_scales = self$panel_scales_y,
      params = self$facet_params
    )
  },

  ## ggplot_gtable ----------------------------------------------------------

  #' @field render
  #' **Description**
  #'
  #' A function method for drawing and assembling the core plot. Mostly it
  #' delegates tasks to the specific Facet methods for drawing components.
  #'
  #' **Usage**
  #' ```r
  #' Layout$render(panels, data, theme, labels)
  #' ```
  #'
  #' **Arguments**
  #' \describe{
  #'   \item{`panels`}{A list parallel to layers. Each element is another list
  #'   with grobs for each panel, generated by `Layer$draw_geom()`.}
  #'   \item{`data`}{A list of data frames with layer data.}
  #'   \item{`theme`}{A [complete theme][complete_theme()].}
  #'   \item{`labels`}{A list of labels from the `labels` field of the ggplot
  #'   object.}
  #' }
  #'
  #' **Value**
  #'
  #' A gtable containing a plot with panels, axes, axis titles and strips.
  render = function(self, panels, data, theme, labels) {
    facet_bg <- self$facet$draw_back(data,
                                     self$layout,
                                     self$panel_scales_x,
                                     self$panel_scales_y,
                                     theme,
                                     self$facet_params
    )
    facet_fg <- self$facet$draw_front(
      data,
      self$layout,
      self$panel_scales_x,
      self$panel_scales_y,
      theme,
      self$facet_params
    )

    # Draw individual panels, then assemble into gtable
    panels <- lapply(seq_along(panels[[1]]), function(i) {
      panel <- lapply(panels, `[[`, i)
      panel <- c(facet_bg[i], panel, facet_fg[i])
      panel <- self$coord$draw_panel(panel, self$panel_params[[i]], theme)
      ggname(paste("panel", i, sep = "-"), panel)
    })
    plot_table <- self$facet$draw_panels(
      panels,
      self$layout,
      self$panel_scales_x,
      self$panel_scales_y,
      self$panel_params,
      self$coord,
      data,
      theme,
      self$facet_params
    )
    plot_table <- self$facet$set_panel_size(plot_table, theme)

    # Draw individual labels, then add to gtable
    labels <- self$coord$labels(
      list(
        x = self$resolve_label(self$panel_scales_x[[1]], labels),
        y = self$resolve_label(self$panel_scales_y[[1]], labels)
      ),
      self$panel_params[[1]]
    )
    labels <- self$render_labels(labels, theme)
    self$facet$draw_labels(
      plot_table,
      self$layout,
      self$panel_scales_x,
      self$panel_scales_y,
      self$panel_params,
      self$coord,
      data,
      theme,
      labels,
      self$params
    )
  },

  #' @field resolve_label
  #' **Description**
  #'
  #' A function method for prying the axis titles from guides, scales or plot
  #' labels.
  #'
  #' **Usage**
  #' ```r
  #' Layout$resolve_label(scale, labels)
  #' ```
  #'
  #' **Arguments**
  #' \describe{
  #'   \item{`scale`}{A single scale from the `panel_scales_x` or
  #'   `panel_scales_y` fields.}
  #'   \item{`labels`}{A list of labels from the `labels` field of the ggplot
  #'   object.}
  #' }
  #'
  #' **Value**
  #'
  #' A named list containing a two titles named `"primary"` and `"secondary"`.
  resolve_label = function(self, scale, labels) {
    aes <- scale$aesthetics[[1]]

    prim_scale <- scale$name
    seco_scale <- (scale$sec_name %||% waiver)()

    prim_label <- labels[[aes]]
    seco_label <- labels[[paste0("sec. aes")]]

    prim_guide <- seco_guide <- waiver()

    order <- scale$axis_order()

    panel <- self$panel_params[[1]]$guides
    if (!is.null(panel)) {
      position <- scale$position
      aes <- switch(position, left = , right = "y", "x")
      params <- panel$get_params(paste0(aes, c("", ".sec")))
      if (!is.null(params)) {
        prim_guide <- params[[1]]$title
        seco_guide <- params[[2]]$title
        position   <- scale$position
        if ((params[[1]]$position %||% position) != position) {
          order <- rev(order)
        }
      }
    }

    primary   <- scale$make_title(prim_guide, prim_scale, prim_label)
    secondary <- scale$make_sec_title(seco_guide, seco_scale, seco_label)
    if (is.derived(secondary)) {
      secondary <- primary
    }

    list(primary = primary, secondary = secondary)[order]
  },

  #' @field render_labels
  #' **Description**
  #'
  #' A function method for drawing axis title grobs. The position guides
  #' themselves do not typically render the axis title grobs as they are
  #' orchestrated by the layout to draw one title even for multiple axes.
  #'
  #' **Usage**
  #' ```r
  #' Layout$render_labels(labels, theme)
  #' ```
  #'
  #' **Arguments**
  #' \describe{
  #'   \item{`labels`}{A named list containing an `x` list and a `y` list. The
  #'   `x` and `y` lists have `primary` and `secondary` labels. It originates
  #'   from the `Coord$labels()` method.}
  #'   \item{`theme`}{A [complete theme][complete_theme()].}
  #' }
  #'
  #' **Value**
  #'
  #' A list with the same structure and names as the `labels` argument, but with
  #' grobs instead of text.
  render_labels = function(self, labels, theme) {
    label_grobs <- lapply(names(labels), function(label) {
      lapply(c(1, 2), function(i) {
        modify <- if (i == 1) {
          switch(label, x = ".top", y = ".left")
        } else {
          switch(label, x = ".bottom", y = ".right")
        }
        if (is.null(labels[[label]][[i]]) || is.waiver(labels[[label]][[i]]))
          return(zeroGrob())

        element_render(
          theme = theme,
          element = paste0("axis.title.", label, modify),
          label = labels[[label]][[i]],
          margin_x = label == "y",
          margin_y = label == "x"
        )
      })
    })
    names(label_grobs) <- names(labels)
    label_grobs
  },

  ## Utilities ------------------------------------------------------------

  #' @field get_scales
  #' **Description**
  #'
  #' A function method for retrieving panel specific scales. It is called in
  #' the `Stat$compute_layer()` and `Position$compute_layer()` methods. The
  #' `Geom` uses the `panel_params` field instead of the raw scales.
  #'
  #' **Usage**
  #' ```r
  #' Layout$get_scales(i)
  #' ```
  #'
  #' **Arguments**
  #' \describe{
  #'   \item{`i`}{A scalar integer panel index giving the panel for which to
  #'   retrieve scales}
  #' }
  #'
  #' **Value**
  #'
  #' A named list of scales giving the `x` and `y` scale for the panel.
  get_scales = function(self, i) {
    this_panel <- self$layout[self$layout$PANEL == i, ]

    list(
      x = self$panel_scales_x[[this_panel$SCALE_X]],
      y = self$panel_scales_y[[this_panel$SCALE_Y]]
    )
  }
)

# Helpers -----------------------------------------------------------------

# Function for applying scale method to multiple variables in a given
# data set.  Implement in such a way to minimize copying and hence maximise
# speed
scale_apply <- function(data, vars, method, scale_id, scales) {
  if (length(vars) == 0) return()
  if (nrow(data) == 0) return()

  if (anyNA(scale_id)) {
    cli::cli_abort("{.arg scale_id} must not contain any {.val NA}.")
  }

  scale_index <- split_with_index(seq_along(scale_id), scale_id, length(scales))

  lapply(vars, function(var) {
    pieces <- lapply(seq_along(scales), function(i) {
      scales[[i]][[method]](vec_slice(data[[var]], scale_index[[i]]))
    })
    # Remove empty vectors to avoid coercion issues with vctrs
    pieces[lengths(pieces) == 0] <- NULL
    o <- order(unlist(scale_index))[seq_len(sum(lengths(pieces)))]
    vec_c(!!!pieces)[o]
  })
}
