#' @name shared_layer_parameters
#' @title Shared layer parameters
#' @description
#' This is a central place for describing typical layer parameters.
#' It prevents cluttered definitions all over the place.
#'
#' @param mapping
#' Set of aesthetic mappings created by [aes()]. If specified and `inherit.aes =
#' TRUE` (the default), it is combined with the default mapping at the top level
#' of the plot. You must supply `mapping` if there is no plot mapping.
#'
#' @param data
#' The data to be displayed in this layer. There are three options:
#' * `NULL` (default): the data is inherited from the plot data as specified
#'   in the call to [ggplot()].
#' * A `data.frame`, or other object, will override the plot data. All objects
#'   will be fortified to produce a data frame. See [fortify()] for which
#'   variables will be created.
#' * A `function` will be called with a single argument, the plot data. The return
#'   value must be a `data.frame`, and will be used as the layer data. A
#'   `function` can be created from a `formula` (e.g. `~ head(.x, 10)`).
#'
#' @param geom
#' The geometric object to use to display the data for this layer. When using a
#' `stat_*()` function to construct a layer, the `geom` argument can be used to
#' override the default coupling between stats and geoms. The `geom` argument
#' accepts the following:
#' * A `Geom` ggproto subclass, for example `GeomPoint`.
#' * A string naming the geom. To give the geom as a string, strip the
#'   function name of the `geom_` prefix. For example, to use `geom_point()`,
#'   give the geom as `"point"`.
#' * For more information and other ways to specify the geom, see the
#'   [layer geom][layer_geoms] documentation.
#'
#' @param stat
#' The statistical transformation to use on the data for this layer. When using
#' a `geom_*()` function to construct a layer, the `stat` argument can be used
#' to override the default coupling between geoms and stats. The `stat` argument
#' accepts the following:
#' * A `Stat` ggproto subclass, for example `StatCount`.
#' * A string naming the stat. To give the stat as a string, strip the
#'   function name of the `stat_` prefix. For example, to use `stat_count()`,
#'   give the stat as `"count"`.
#' * For more information and other ways to specify the stat, see the
#'   [layer stat][layer_stats] documentation.
#'
#' @param position
#' A position adjustment to use on the data for this layer. This can be used in
#' various ways, including to prevent overplotting and improving the display.
#' The `position` argument accepts the following:
#' * The result of calling a position function, such as `position_jitter()`.
#'   This method allows for passing extra arguments to the position.
#' * A string naming the position adjustment. To give the position as a
#'   string, strip the function name of the `position_` prefix. For example, to
#'   use `position_jitter()`, give the position as `"jitter"`.
#' * For more information and other ways to specify the position, see the
#'   [layer position][layer_positions] documentation.
#'
#' @param na.rm
#' If `FALSE`, the default, missing values are removed with a warning. If
#' `TRUE`, missing values are silently removed.
#'
#' @param show.legend
#' Logical. Should this layer be included in the legends? `NA`, the default,
#' includes if any aesthetics are mapped. `FALSE` never includes, and `TRUE`
#' always includes. It can also be a named logical vector to finely select the
#' aesthetics to display. To include legend keys for all levels, even when no
#' data exists, use `TRUE`.  If `NA`, all levels are shown in legend, but
#' unobserved levels are omitted.
#'
#' @param inherit.aes
#' If `FALSE`, overrides the default aesthetics, rather than combining with
#' them. This is most useful for helper functions that define both data and
#' aesthetics and shouldn't inherit behaviour from the default plot
#' specification, e.g. [annotation_borders()].
#'
#' @param ...
#' Other arguments passed on to [layer()]'s `params` argument. These arguments
#' broadly fall into one of 4 categories below. Notably, further arguments to
#' the `position` argument, or aesthetics that are required can
#'   *not* be passed through `...`. Unknown arguments that are not part of the 4
#' categories below are ignored.
#' * Static aesthetics that are not mapped to a scale, but are at a fixed
#'   value and apply to the layer as a whole. For example, `colour = "red"` or
#'   `linewidth = 3`. The geom's documentation has an **Aesthetics** section
#'   that lists the available options. The 'required' aesthetics cannot be
#'   passed on to the `params`. Please note that while passing unmapped
#'   aesthetics as vectors is technically possible, the order and required
#'   length is not guaranteed to be parallel to the input data.
#' * When constructing a layer using
#'   a `stat_*()` function, the `...` argument can be used to pass on parameters
#'   to the `geom` part of the layer. An example of this is
#'   `stat_density(geom = "area", outline.type = "both")`. The geom's
#'   documentation lists which parameters it can accept.
#' * Inversely, when constructing a layer using a
#'   `geom_*()` function, the `...` argument can be used to pass on parameters
#'   to the `stat` part of the layer. An example of this is
#'   `geom_area(stat = "density", adjust = 0.5)`. The stat's documentation lists
#'   which parameters it can accept.
#' * The `key_glyph` argument of [`layer()`] may also be passed on through
#'   `...`. This can be one of the functions described as
#'   [key glyphs][draw_key], to change the display of the layer in the legend.

#' @param orientation
#' The orientation of the layer. The default (`NA`) automatically determines the
#' orientation from the aesthetic mapping. In the rare event that this fails it
#' can be given explicitly by setting `orientation` to either `"x"` or `"y"`.
#' See the *Orientation* section for more detail.
#'
#' @keywords internal
#' @aliases NULL
NULL

#' Create a new layer
#'
#' A layer is a combination of data, stat and geom with a potential position
#' adjustment. Usually layers are created using `geom_*` or `stat_*`
#' calls but it can also be created directly using this function.
#'
#' @export
#' @inheritParams shared_layer_parameters
#' @param check.aes,check.param If `TRUE`, the default, will check that
#'   supplied parameters and aesthetics are understood by the `geom` or
#'   `stat`. Use `FALSE` to suppress the checks.
#' @param params Additional parameters to the `geom` and `stat`.
#' @param key_glyph A legend key drawing function or a string providing the
#'   function name minus the `draw_key_` prefix. See [draw_key] for details.
#' @param layout Argument to control layout at the layer level. Consult the
#'   faceting documentation to view appropriate values.
#' @param layer_class The type of layer object to be constructed. This is
#'   intended for ggplot2 internal use only.
#' @keywords internal
#' @seealso
#' The `r link_book(c("plot building chapter", "geoms chapter"), c("layers", "individual-geoms"))`
#' @family layer documentation
#' @examples
#' # geom calls are just a short cut for layer
#' ggplot(mpg, aes(displ, hwy)) + geom_point()
#' # shortcut for
#' ggplot(mpg, aes(displ, hwy)) +
#'   layer(
#'     geom = "point", stat = "identity", position = "identity",
#'     params = list(na.rm = FALSE)
#'   )
#'
#' # use a function as data to plot a subset of global data
#' ggplot(mpg, aes(displ, hwy)) +
#'   layer(
#'     geom = "point", stat = "identity", position = "identity",
#'     data = head, params = list(na.rm = FALSE)
#'   )
#'
layer <- function(geom = NULL, stat = NULL,
                  data = NULL, mapping = NULL,
                  position = NULL, params = list(),
                  inherit.aes = TRUE, check.aes = TRUE, check.param = TRUE,
                  show.legend = NA, key_glyph = NULL, layout = NULL, layer_class = Layer) {
  call_env <- caller_env()
  user_env <- caller_env(2)

  # Handle show_guide/show.legend
  if (!is.null(params$show_guide)) {
    lifecycle::deprecate_stop(
      "2.0.0", "layer(show_guide)", "layer(show.legend)"
    )
  }

  # we validate mapping before data because in geoms and stats
  # the mapping is listed before the data argument; this causes
  # less confusing error messages when layers are accidentally
  # piped into each other
  if (!is.null(mapping)) {
    mapping <- validate_mapping(mapping, call_env)
  }

  data <- fortify(data)

  geom <- validate_subclass(geom, "Geom", env = parent.frame(), call = call_env)
  stat <- validate_subclass(stat, "Stat", env = parent.frame(), call = call_env)
  position <- validate_subclass(position, "Position", env = parent.frame(), call = call_env)

  # Special case for na.rm parameter needed by all layers
  params$na.rm <- params$na.rm %||% FALSE

  # Split up params between aesthetics, geom, and stat
  params <- rename_aes(params)
  aes_params  <- params[intersect(names(params), union(geom$aesthetics(), position$aesthetics()))]
  geom_params <- params[intersect(names(params), geom$parameters(TRUE))]
  stat_params <- params[intersect(names(params), stat$parameters(TRUE))]

  ignore <- c("key_glyph", "name", "layout")
  all <- c(geom$parameters(TRUE), stat$parameters(TRUE), geom$aesthetics(), position$aesthetics(), ignore)

  # Take care of plain patterns provided as aesthetic
  pattern <- vapply(aes_params, is_pattern, logical(1))
  if (any(pattern)) {
    aes_params[pattern] <- lapply(aes_params[pattern], list)
  }

  # Warn about extra params and aesthetics
  extra_param <- setdiff(names(params), all)
  # Take care of size->linewidth renaming in layer params
  if (geom$rename_size && "size" %in% extra_param && !"linewidth" %in% mapped_aesthetics(mapping)) {
    aes_params <- c(aes_params, params["size"])
    extra_param <- setdiff(extra_param, "size")
    deprecate_warn0("3.4.0", I("Using `size` aesthetic for lines"), I("`linewidth`"), user_env = user_env)
  }
  if (check.param && length(extra_param) > 0) {
    cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}", call = call_env)
  }

  extra_aes <- setdiff(
    mapped_aesthetics(mapping),
    c(geom$aesthetics(), stat$aesthetics(), position$aesthetics())
  )
  # Take care of size->linewidth aes renaming
  if (geom$rename_size && "size" %in% extra_aes && !"linewidth" %in% mapped_aesthetics(mapping)) {
    extra_aes <- setdiff(extra_aes, "size")
    deprecate_warn0("3.4.0", I("Using `size` aesthetic for lines"), I("`linewidth`"), user_env = user_env)
  }
  if (check.aes && length(extra_aes) > 0) {
    cli::cli_warn("Ignoring unknown aesthetics: {.field {extra_aes}}", call = call_env)
  }
  aes_params[["label"]] <- normalise_label(aes_params[["label"]])

  # adjust the legend draw key if requested
  geom <- set_draw_key(geom, key_glyph %||% params$key_glyph)

  fr_call <- layer_class$constructor %||% frame_call(call_env) %||% current_call()
  attr(fr_call, "srcref") <- NULL

  ggproto("LayerInstance", layer_class,
    constructor = fr_call,
    geom = geom,
    geom_params = geom_params,
    stat = stat,
    stat_params = stat_params,
    data = data,
    mapping = mapping,
    aes_params = aes_params,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    name = params$name,
    layout = layout %||% params$layout
  )
}

validate_mapping <- function(mapping, call = caller_env()) {
  # Upgrade any old S3 input to new S7 input
  # TODO: deprecate this after a while
  is_old_mapping <- !S7::S7_inherits(mapping) && inherits(mapping, "uneval")
  if (is_old_mapping && is.list(mapping)) {
    mapping <- aes(!!!mapping)
  }

  if (!is_mapping(mapping)) {
    msg <- c(
      "{.arg mapping} must be created by {.fn aes}.",
      "x" = "You've supplied {.obj_type_friendly {mapping}}."
    )
    # Native pipe have higher precedence than + so any type of gg object can be
    # expected here, not just ggplot
    if (S7::S7_inherits(mapping, class_gg)) {
      msg <- c(msg, "i" = "Did you use {.code %>%} or {.code |>} instead of {.code +}?")
    }

    cli::cli_abort(msg, call = call)
  } else {
    return(mapping)
  }

  # For backward compatibility with pre-tidy-eval layers
  class_mapping(mapping)
}

#' Layers
#' @name Layer-class
#'
#' @description
#' The Layer class is a chaperone class not available for extension. The class
#' fulfils the following tasks. The class houses the Geom, Stat and Position
#' trinity and tracks their stateful parameters. Furthermore, its methods are
#' responsible for managing the layer data and exposing it to other components
#' of the plot at the right time.
#'
#' @details
#' The Layer class is an internal class that is not exported because the class
#' is not intended for extension. The `layer()` function instantiates the
#' LayerInstance class, which inherits from Layer, but has relevant fields
#' populated.
#'
#' The class is mostly used in `ggplot_build()`, with the notable exception
#' of the `draw_geom()` method, which is used in `ggplot_gtable()` instead.
#'
#' @section Layer data diagram:
#'
#' As the Layer class is a chaparone for the data, it makes sense to give a
#' small overview of how layer data flows through a plot. In the diagram below
#' we following the `layer(data)` argument over the course of plot building
#' through Layer class methods. When an outside class acts on the data without
#' the Layer class, this is indicated with the left arrow `<-`. Subcomponents
#' of a method that touch data are indicated with the right arrow `->`.
#'
#' ```r
#' # Inside `ggplot_build()`
#'  |
#' layer(data)
#'  |
#'  |
#'  | # Inherit plot data
#'  |
#' Layer$layer_data()
#'  |
#'  |
#'  | # Finalise mapping
#'  |
#' Layer$setup_layer()
#'  |
#'  |
#'  | # Append PANEL variable for facets
#'  |
#'  |<- Layout$setup()
#'  |    |
#'  |    +-> Facet$setup_data()
#'  |    |
#'  |    +-> Coord$setup_data()
#'  |
#'  |
#'  | # Evaluate mappings to new data and infer group
#'  |
#' Layer$compute_aesthetics()
#'  |
#'  |
#'  | # Scale-transform all aesthetics
#'  |
#'  |<- ScalesList$transform_df()
#'  |    |
#'  |    +-> Scale$transform_df()
#'  |
#'  |
#'  | # Map x/y aesthetics with initial scale
#'  |
#'  |<- Layout$map_position()
#'  |    |
#'  |    +-> Scale$map()
#'  |
#'  |
#'  | # Compute stat part of layer
#'  |
#' Layer$compute_statistic()
#'  | |
#'  | +-> Stat$setup_data()
#'  | |
#'  | +-> Stat$compute_layer()
#'  |
#'  |
#'  | # Add `after_stat()` stage
#'  | # Scale transform computed variables
#'  |
#' Layer$map_statistic()
#'  |
#'  |
#'  | # Setup geom part of layer
#'  |
#' Layer$compute_geom_1()
#'  | |
#'  | +-> Geom$setup_data()
#'  |
#'  |
#'  | # Apply position adjustments
#'  |
#' Layer$compute_position()
#'  | |
#'  | +-> Position$use_defaults()
#'  | |
#'  | +-> Position$setup_data()
#'  | |
#'  | +-> Position$compute_layer()
#'  |
#'  |
#'  | # Map x/y aesthetics with final scales
#'  |
#'  |<- Layout$map_position()
#'  |    |
#'  |    +-> Scale$map()
#'  |
#'  | # Map non-position aesthetics
#'  |
#'  |<- ScalesList$map_df()
#'  |    |
#'  |    +-> Scale$map()
#'  |
#'  |
#'  | # Fill in defaults and fixed aesthetics
#'  |
#' Layer$compute_geom_2()
#'  | |
#'  | +-> Geom$use_defaults()
#'  |
#'  |
#'  | # Apply final Stat hook
#'  |
#' Layer$finish_statistics()
#'  | |
#'  | +-> Stat$finish_layer()
#'  |
#'  |
#'  | # Apply final Facet hook
#'  |
#'  |<- Layout$finish_data()
#'  |    |
#'  |    +-> Facet$finish_data()
#'  |
#'  V
#' # `ggplot_build()` is finished
#' # Hand off to `ggplot_gtable()`
#'  |
#'  |
#'  | # Draw the geom part
#'  |
#' Layer$draw_geom()
#'  |
#'  +-> Geom$handle_na()
#'  |
#'  +-> Geom$draw_layer()
#' ```
#' @usage NULL
#' @format NULL
#' @family Layer components
#' @family chaperone classes
#' @keywords internal
#' @examples
#' # None: Layer is not intended to be extended
Layer <- ggproto("Layer", NULL,

  # Fields ------------------------------------------------------------------

  #' @field constructor A [call][call()] object with the user-facing
  #' constructor function, for use in error messaging. This field is populated
  #' by `layer()`.
  constructor = NULL,

  #' @field geom,stat,position These fields house the Geom, Stat and Position
  #' trifecta in ggproto form and is populated by `layer()`.
  geom = NULL,
  stat = NULL,
  position = NULL,

  #' @field stat_params,computed_stat_params These fields hold parameters
  #' assigned to the Stat. The `stat_params` field is directly derived from
  #' user input and is populated by `layer()`. The `computed_stat_params`
  #' carries state and is constructed by the `Stat$setup_params()` method.
  stat_params = NULL,
  computed_stat_params = NULL,

  #' @field geom_params,computed_geom_params These fields hold parameters
  #' assigned to the Geom. The `geom_params` field is directly derived from
  #' user input and is populated by `layer()`. The `computed_geom_params`
  #' carries state and is constructed by the `Geom$setup_params()` method.
  geom_params = NULL,
  computed_geom_params = NULL,

  #' @field mapping,computed_mapping These fields hold [mapping][aes()]s.
  #' The `mapping` field holds the `layer(mapping)` argument. The
  #' `computed_mapping` field carries state and is constructed in the
  #' `setup_layer()` method.
  mapping = NULL,
  computed_mapping = NULL,

  #' @field data The fortified `layer(data)` argument.
  data = NULL,

  #' @field aes_params Holds the fixed, unmapped aesthetics passed to
  #' `layer(params)` as determined by `Geom$aesthetics()`.
  aes_params = NULL,

  #' @field inherit.aes A scalar boolean used in the `setup_layer()` method to
  #' indicate whether the `computed_mapping` should include the global mapping
  #' (`TRUE`) or only the layer mapping (`FALSE`). This is populated by the
  #' `layer(inherit.aes)` parameter.
  inherit.aes = FALSE,

  # Methods -----------------------------------------------------------------

  #' @field layer_data
  #' **Description**
  #'
  #' A function method for initially resolving layer data. If layer data is
  #' missing or is a function, it will derive layer data from the global plot
  #' data.
  #'
  #' **Usage**
  #' ```r
  #' Layer$layer_data(plot_data)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`plot_data`}{The `data` field of the ggplot object.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data or `NULL`
  layer_data = function(self, plot_data) {
    if (is_waiver(self$data)) {
      data <- plot_data
    } else if (is.function(self$data)) {
      data <- self$data(plot_data)
      if (!is.data.frame(data)) {
        cli::cli_abort("{.fn layer_data} must return a {.cls data.frame}.")
      }
    } else {
      data <- self$data
    }
    if (is.null(data) || is_waiver(data)) data else unrowname(data)
  },

  #' @field setup_layer
  #' **Description**
  #'
  #' A function method is a hook to allow a final access to layer data in
  #' input form. In addition, it allows a layer access to global plot
  #' information. The latter is used to enforce the `inherit.aes` parameter by
  #' supplementing the layer mapping with the global mapping when requested.
  #'
  #' **Usage**
  #' ```r
  #' Layer$setup_data(data, plot)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`plot`}{A ggplot object}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data. As a side effect, the `computed_mapping`
  #' field is populated.
  setup_layer = function(self, data, plot) {
    # For annotation geoms, it is useful to be able to ignore the default aes
    if (isTRUE(self$inherit.aes)) {
      self$computed_mapping <- class_mapping(defaults(self$mapping, plot@mapping))

      # Inherit size as linewidth from global mapping
      if (self$geom$rename_size &&
          "size" %in% names(plot@mapping) &&
          !"linewidth" %in% names(self$computed_mapping) &&
          "linewidth" %in% self$geom$aesthetics()) {
        self$computed_mapping$size <- plot@mapping$size
        deprecate_warn0("3.4.0", I("Using `size` aesthetic for lines"), I("`linewidth`"))
      }
    } else {
      self$computed_mapping <- self$mapping
    }
    attr(data, "layout") <- self$layout

    data
  },

  #' @field compute_aesthetics
  #' **Description**
  #'
  #' A function method that evaluates aesthetics and warns about any problems.
  #' It also infers a `group` aesthetic if not provided. This method is also
  #' the step where layer data becomes standardised to base data frames without
  #' row names or additional attributes.
  #'
  #' **Usage**
  #' ```r
  #' Layer$compute_aesthetics(data, plot)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`plot`}{A ggplot object}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data
  compute_aesthetics = function(self, data, plot) {
    aesthetics <- self$computed_mapping

    # Drop aesthetics that are set or calculated
    set <- names(aesthetics) %in% names(self$aes_params)
    calculated <- is_calculated_aes(aesthetics, warn = TRUE)
    modifiers <- is_scaled_aes(aesthetics)
    themed <- is_themed_aes(aesthetics)

    aesthetics <- aesthetics[!set & !calculated & !modifiers & !themed]

    # Override grouping if set in layer
    if (!is.null(self$geom_params$group)) {
      aesthetics[["group"]] <- self$aes_params$group
    }

    # Evaluate aesthetics
    evaled <- eval_aesthetics(aesthetics, data)
    evaled$label <- normalise_label(evaled$label)
    plot@scales$add_defaults(evaled, plot@plot_env)

    # Check for discouraged usage in mapping
    warn_for_aes_extract_usage(aesthetics, data[setdiff(names(data), "PANEL")])

    # Check aesthetic values
    check_nondata_cols(
      evaled, aesthetics,
      problem = "Aesthetics are not valid data columns.",
      hint    = "Did you mistype the name of a data column or forget to add {.fn after_stat}?"
    )

    n <- nrow(data)
    aes_n <- list_sizes(evaled)
    if (n == 0) {
      # No data, so look at longest evaluated aesthetic
      if (length(evaled) == 0) {
        n <- 0
      } else {
        n <- if (min(aes_n) == 0) 0L else max(aes_n)
      }
    }
    if ((self$geom$check_constant_aes %||% TRUE)
        && length(aes_n) > 0 && all(aes_n == 1) && n > 1) {
      cli::cli_warn(c(
        "All aesthetics have length 1, but the data has {n} rows.",
        i = "Please consider using {.fn annotate} or provide this layer \\
        with data containing a single row."
      ), call = self$constructor)
    }
    check_aesthetics(evaled, n)

    # Set special group and panel vars
    if (empty(data) && n > 0) {
      evaled$PANEL <- 1
    } else {
      evaled$PANEL <- data$PANEL
    }
    evaled <- data_frame0(!!!lapply(evaled, vec_set_names, names = NULL))
    # evaled <- as_gg_data_frame(evaled)
    evaled <- add_group(evaled)
    evaled
  },

  #' @field compute_aesthetics
  #' **Description**
  #'
  #' A function method that orchestrates computing statistics. It executes
  #' methods from the Stat class to form new computed variables.
  #'
  #' **Usage**
  #' ```r
  #' Layer$compute_statistic(data, layout)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`layout`}{A `<Layout>` ggproto object.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data. As a side effect the `computed_stat_params`
  #' field is populated.
  compute_statistic = function(self, data, layout) {
    if (empty(data)) return(data_frame0())

    ptype <- vec_ptype(data)
    self$computed_stat_params <- self$stat$setup_params(data, self$stat_params)
    data <- self$stat$setup_data(data, self$computed_stat_params)
    data <- self$stat$compute_layer(data, self$computed_stat_params, layout)
    merge_attrs(data, ptype)
  },

  # TODO: should this be merged with compute_aesthetics?
  #' @field map_statistic
  #' **Description**
  #'
  #' A function method that finishes the result of computed statistics. It has
  #' several tasks:
  #' * It evaluates the `after_stat()` stage of the mapping from both the
  #'   `computed_mapping` but also the `Stat$default_aes` fields.
  #' * It ensures relevant scales are instantiated for computed aesthetics.
  #' * It takes care that scale transformation is applied to computed aesthetics.
  #'
  #' **Usage**
  #' ```r
  #' Layer$map_statistic(data, plot)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`plot`}{A ggplot object.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data
  map_statistic = function(self, data, plot) {
    if (empty(data)) return(data_frame0())

    # Make sure data columns are converted to correct names. If not done, a
    # column with e.g. a color name will not be found in an after_stat()
    # evaluation (since the evaluation symbols gets renamed)
    data <- rename_aes(data)

    # Assemble aesthetics from layer, plot and stat mappings
    aesthetics <- self$computed_mapping
    aesthetics <- defaults(aesthetics, self$stat$default_aes)
    aesthetics <- compact(aesthetics)

    new <- strip_dots(aesthetics[is_calculated_aes(aesthetics) | is_staged_aes(aesthetics)])
    if (length(new) == 0) return(data)

    # data needs to be non-scaled
    data_orig <- plot@scales$backtransform_df(data)

    # Add map stat output to aesthetics
    stat_data <- eval_aesthetics(
      substitute_aes(new), data_orig,
      mask = list(stage = stage_calculated)
    )
    # Check that all columns in aesthetic stats are valid data
    check_nondata_cols(
      stat_data, aesthetics,
      problem = "Aesthetics must be valid computed stats.",
      hint    = "Did you map your stat in the wrong layer?"
    )

    stat_data <- data_frame0(!!!stat_data)

    # Add any new scales, if needed
    plot@scales$add_defaults(stat_data, plot@plot_env)
    # Transform the values, if the scale say it's ok
    # (see stat_spoke for one exception)
    if (self$stat$retransform) {
      stat_data <- plot@scales$transform_df(stat_data)
    }
    stat_data <- cleanup_mismatched_data(stat_data, nrow(data), "after_stat")
    data[names(stat_data)] <- stat_data
    data
  },

  #' @field compute_geom_1
  #' **Description**
  #'
  #' A function method that prepares data for drawing. It checks that all
  #' required aesthetics are present and sets up parameters and data using the
  #' Geom class.
  #'
  #' **Usage**
  #' ```r
  #' Layer$compute_geom_1(data)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data. As a side effect the `computed_geom_params`
  #' field is populated.
  compute_geom_1 = function(self, data) {
    if (empty(data)) return(data_frame0())
    ptype <- vec_ptype(data)

    check_required_aesthetics(
      self$geom$required_aes,
      c(names(data), names(self$aes_params)),
      snake_class(self$geom)
    )
    self$computed_geom_params <- self$geom$setup_params(data, c(self$geom_params, self$aes_params))
    data <- self$geom$setup_data(data, self$computed_geom_params)
    merge_attrs(data, ptype)
  },

  #' @field compute_position
  #' **Description**
  #'
  #' A function method that orchestrates the position adjustment. It executes
  #' methods from the Position class.
  #'
  #' **Usage**
  #' ```r
  #' Layer$compute_position(data, layout)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`layout`}{A `<Layout>` ggproto object.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data.
  compute_position = function(self, data, layout) {
    if (empty(data)) return(data_frame0())
    ptype <- vec_ptype(data)
    data <- self$position$use_defaults(data, self$aes_params)
    params <- self$position$setup_params(data)
    data <- self$position$setup_data(data, params)
    data <- self$position$compute_layer(data, params, layout)
    merge_attrs(data, ptype)
  },

  #' @field compute_geom_2
  #' **Description**
  #'
  #' A function method that add defaults and fixed parameters. It wraps the
  #' `Geom$use_defaults()` method.
  #'
  #' **Usage**
  #' ```r
  #' Layer$compute_geom_2(data, params, theme, ...)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`params`}{A list with fixed aesthetic parameters, typically the
  #'     `aes_params` field.}
  #'   \item{`theme`}{A [theme][theme()] object}
  #'   \item{`...`}{Passed on to `Geom$use_defaults()`, not in use.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data.
  compute_geom_2 = function(self, data, params = self$aes_params, theme = NULL, ...) {
    # Combine aesthetics, defaults, & params
    if (empty(data)) return(data)

    # Drop empty aesthetics
    empty_aes <- names(params)[lengths(params) == 0]
    if (length(empty_aes) > 0) {
      cli::cli_warn(
        "Ignoring empty aesthetic{?s}: {.arg {empty_aes}}.",
        call = self$constructor
      )
      params <- params[setdiff(names(params), empty_aes)]
    }

    aesthetics <- self$computed_mapping
    modifiers <- aesthetics[is_scaled_aes(aesthetics) | is_staged_aes(aesthetics) | is_themed_aes(aesthetics)]

    self$geom$use_defaults(data, params, modifiers, theme = theme, ...)
  },

  #' @field finish_statistics
  #' **Description**
  #'
  #' A function method that wraps `Stat$finish_layer()`.
  #'
  #' **Usage**
  #' ```r
  #' Layer$finish_statistics(data)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #' }
  #'
  #' **Value**
  #'
  #' A data frame with layer data.
  finish_statistics = function(self, data) {
    self$stat$finish_layer(data, self$computed_stat_params)
  },

  #' @field draw_geom
  #' **Description**
  #'
  #' A function method that produces graphics for every panel. It uses Geom
  #' class methods to handle missing data and produce grobs. In contrast to
  #' other methods, this is called during the `ggplot_gtable()` stage, not the
  #' `ggplot_build()` stage.
  #'
  #' **Usage**
  #' ```r
  #' Layer$draw_geom(data, layout)
  #' ```
  #' **Arguments**
  #' \describe{
  #'   \item{`data`}{A data frame with the layer's data.}
  #'   \item{`layout`}{A `<Layout>` ggproto object.}
  #' }
  #'
  #' **Value**
  #'
  #' A list of grobs, one per panel.
  draw_geom = function(self, data, layout) {
    if (empty(data)) {
      n <- nrow(layout$layout)
      return(rep(list(zeroGrob()), n))
    }

    data <- self$geom$handle_na(data, self$computed_geom_params)
    self$geom$draw_layer(data, self$computed_geom_params, layout, layout$coord)
  },

  # Utilities ---------------------------------------------------------------

  #' @field print
  #' **Description**
  #'
  #' A function method that prints information about the layer.
  #'
  #' **Usage**
  #' ```r
  #' Layer$print()
  #' ```
  #'
  #' **Value**
  #'
  #' Nothing (`NULL`), invisibly
  print = function(self) {
    if (!is.null(self$mapping)) {
      cat("mapping:", clist(self$mapping), "\n")
    }
    cat(snakeize(class(self$geom)[[1]]), ": ", clist(self$geom_params), "\n",
        sep = "")
    cat(snakeize(class(self$stat)[[1]]), ": ", clist(self$stat_params), "\n",
        sep = "")
    cat(snakeize(class(self$position)[[1]]), "\n")
  }
)

# Helpers -----------------------------------------------------------------

#' @export
#' @rdname is_tests
is_layer <- function(x) inherits(x, "Layer")
is.layer <- function(x) lifecycle::deprecate_stop("3.5.2", "is.layer()", "is_layer()")

validate_subclass <- function(x, subclass,
                              argname = to_lower_ascii(subclass),
                              x_arg = caller_arg(x),
                              env = parent.frame(),
                              call = caller_env()) {

  if (inherits(x, subclass)) {
    return(x)
  }
  if (!is_scalar_character(x)) {
    stop_input_type(x, as_cli("either a string or a {.cls {subclass}} object"), arg = x_arg)
  }

  # Try getting class object directly
  name <- paste0(subclass, camelize(x, first = TRUE))
  obj <- find_global(name, env = env)
  if (inherits(obj, subclass)) {
    return(obj)
  }

  # Try retrieving class via constructors
  name <- snakeize(name)
  obj <- find_global(name, env = env, mode = "function")
  if (is.function(obj)) {
    obj <- try_fetch(
      obj(),
      error = function(cnd) {
        # replace `obj()` call with name of actual constructor
        cnd$call <- call(name)
        cli::cli_abort(
          "Failed to retrieve a {.cls {subclass}} object from {.fn {name}}.",
          parent = cnd, call = call
        )
      })
  }
  # Position constructors return classes directly
  if (inherits(obj, subclass)) {
    return(obj)
  }
  # Try prying the class from a layer
  if (inherits(obj, "Layer")) {
    obj <- switch(
      subclass,
      Geom = obj$geom,
      Stat = obj$stat,
      NULL
    )
  }
  if (inherits(obj, subclass)) {
    return(obj)
  }
  cli::cli_abort("Can't find {argname} called {.val {x}}.", call = call)
}

# helper function to adjust the draw_key slot of a geom
# if a custom key glyph is requested
set_draw_key <- function(geom, draw_key = NULL) {
  if (is.null(draw_key)) {
    return(geom)
  }
  if (is.character(draw_key)) {
    draw_key <- paste0("draw_key_", draw_key)
  }
  draw_key <- match.fun(draw_key)

  ggproto(NULL, geom, draw_key = draw_key)
}

cleanup_mismatched_data <- function(data, n, fun) {
  if (vec_duplicate_any(names(data))) {
    data <- data[unique0(names(data))]
  }

  failed <- !lengths(data) %in% c(0, 1, n)
  if (!any(failed)) {
    return(data)
  }

  failed <- names(data)[failed]
  cli::cli_warn(
    "Failed to apply {.fn {fun}} for the following \\
    aesthetic{?s}: {.field {failed}}."
  )

  data[failed] <- NULL
  data
}

normalise_label <- function(label) {
  if (is.null(label)) {
    return(NULL)
  }
  if (obj_is_list(label)) {
    # Ensure that each element in the list has length 1
    label[lengths(label) == 0] <- ""
    truncate <- !vapply(label, is.call, logical(1)) # Don't mess with call/formula
    label[truncate] <- lapply(label[truncate], `[`, 1)
  }
  if (is.expression(label)) {
    # Classed expressions, when converted to lists, retain their class.
    # The unclass is needed to properly treat it as a vctrs-compatible list.
    label <- unclass(as.list(label))
  }
  label
}
