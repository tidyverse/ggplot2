#' Create a new layer
#'
#' A layer is a combination of data, stat and geom with a potential position
#' adjustment. Usually layers are created using `geom_*` or `stat_*`
#' calls but it can also be created directly using this function.
#'
#' @export
#' @param mapping Set of aesthetic mappings created by [aes()]. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply `mapping` if there is no plot
#'   mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#'
#' @param geom The geometric object to use to display the data for this layer.
#'   When using a `stat_*()` function to construct a layer, the `geom` argument
#'   can be used to override the default coupling between stats and geoms. The
#'   `geom` argument accepts the following:
#'   * A `Geom` ggproto subclass, for example `GeomPoint`.
#'   * A string naming the geom. To give the geom as a string, strip the
#'     function name of the `geom_` prefix. For example, to use `geom_point()`,
#'     give the geom as `"point"`.
#'   * For more information and other ways to specify the geom, see the
#'     [layer geom][layer_geoms] documentation.
#' @param stat The statistical transformation to use on the data for this layer.
#'   When using a `geom_*()` function to construct a layer, the `stat`
#'   argument can be used to override the default coupling between geoms and
#'   stats. The `stat` argument accepts the following:
#'   * A `Stat` ggproto subclass, for example `StatCount`.
#'   * A string naming the stat. To give the stat as a string, strip the
#'     function name of the `stat_` prefix. For example, to use `stat_count()`,
#'     give the stat as `"count"`.
#'   * For more information and other ways to specify the stat, see the
#'     [layer stat][layer_stats] documentation.
#' @param position A position adjustment to use on the data for this layer. This
#'   can be used in various ways, including to prevent overplotting and
#'   improving the display. The `position` argument accepts the following:
#'   * The result of calling a position function, such as `position_jitter()`.
#'     This method allows for passing extra arguments to the position.
#'   * A string naming the position adjustment. To give the position as a
#'     string, strip the function name of the `position_` prefix. For example,
#'     to use `position_jitter()`, give the position as `"jitter"`.
#'   * For more information and other ways to specify the position, see the
#'     [layer position][layer_positions] documentation.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display. To include legend keys for all levels, even
#'   when no data exists, use `TRUE`.  If `NA`, all levels are shown in legend,
#'   but unobserved levels are omitted.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param check.aes,check.param If `TRUE`, the default, will check that
#'   supplied parameters and aesthetics are understood by the `geom` or
#'   `stat`. Use `FALSE` to suppress the checks.
#' @param params Additional parameters to the `geom` and `stat`.
#' @param key_glyph A legend key drawing function or a string providing the
#'   function name minus the `draw_key_` prefix. See [draw_key] for details.
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
                  show.legend = NA, key_glyph = NULL, layer_class = Layer) {
  call_env <- caller_env()
  user_env <- caller_env(2)
  if (is.null(geom))
    cli::cli_abort("Can't create layer without a geom.", call = call_env)
  if (is.null(stat))
    cli::cli_abort("Can't create layer without a stat.", call = call_env)
  if (is.null(position))
    cli::cli_abort("Can't create layer without a position.", call = call_env)

  # Handle show_guide/show.legend
  if (!is.null(params$show_guide)) {
    deprecate_warn0("2.0.0", "layer(show_guide)", "layer(show.legend)", user_env = user_env)
    show.legend <- params$show_guide
    params$show_guide <- NULL
  }

  # we validate mapping before data because in geoms and stats
  # the mapping is listed before the data argument; this causes
  # less confusing error messages when layers are accidentally
  # piped into each other
  if (!is.null(mapping)) {
    mapping <- validate_mapping(mapping, call_env)
  }

  data <- fortify(data)

  geom <- check_subclass(geom, "Geom", env = parent.frame(), call = call_env)
  stat <- check_subclass(stat, "Stat", env = parent.frame(), call = call_env)
  position <- check_subclass(position, "Position", env = parent.frame(), call = call_env)

  # Special case for na.rm parameter needed by all layers
  params$na.rm <- params$na.rm %||% FALSE

  # Split up params between aesthetics, geom, and stat
  params <- rename_aes(params)
  aes_params  <- params[intersect(names(params), geom$aesthetics())]
  geom_params <- params[intersect(names(params), geom$parameters(TRUE))]
  stat_params <- params[intersect(names(params), stat$parameters(TRUE))]

  ignore <- c("key_glyph", "name")
  all <- c(geom$parameters(TRUE), stat$parameters(TRUE), geom$aesthetics(), ignore)

  # Take care of plain patterns provided as aesthetic
  pattern <- vapply(aes_params, is_pattern, logical(1))
  if (any(pattern)) {
    aes_params[pattern] <- lapply(aes_params[pattern], list)
  }
  # Drop empty aesthetics
  empty_aes <- names(aes_params)[lengths(aes_params) == 0]
  if (length(empty_aes) > 0) {
    cli::cli_warn(
      "Ignoring empty aesthetic{?s}: {.arg {empty_aes}}.",
      call = call_env
    )
    aes_params <- aes_params[setdiff(names(aes_params), empty_aes)]
  }

  # Warn about extra params and aesthetics
  extra_param <- setdiff(names(params), all)
  # Take care of size->linewidth renaming in layer params
  if (geom$rename_size && "size" %in% extra_param && !"linewidth" %in% mapped_aesthetics(mapping)) {
    aes_params <- c(aes_params, params["size"])
    extra_param <- setdiff(extra_param, "size")
    deprecate_soft0("3.4.0", I("Using `size` aesthetic for lines"), I("`linewidth`"), user_env = user_env)
  }
  if (check.param && length(extra_param) > 0) {
    cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}", call = call_env)
  }

  extra_aes <- setdiff(
    mapped_aesthetics(mapping),
    c(geom$aesthetics(), stat$aesthetics())
  )
  # Take care of size->linewidth aes renaming
  if (geom$rename_size && "size" %in% extra_aes && !"linewidth" %in% mapped_aesthetics(mapping)) {
    extra_aes <- setdiff(extra_aes, "size")
    deprecate_soft0("3.4.0", I("Using `size` aesthetic for lines"), I("`linewidth`"), user_env = user_env)
  }
  if (check.aes && length(extra_aes) > 0) {
    cli::cli_warn("Ignoring unknown aesthetics: {.field {extra_aes}}", call = call_env)
  }

  # adjust the legend draw key if requested
  geom <- set_draw_key(geom, key_glyph %||% params$key_glyph)

  fr_call <- layer_class$constructor %||% frame_call(call_env) %||% current_call()

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
    name = params$name
  )
}

#' @export
#' @rdname is_tests
is.layer <- function(x) inherits(x, "Layer")

validate_mapping <- function(mapping, call = caller_env()) {
  if (!is.mapping(mapping)) {
    msg <- "{.arg mapping} must be created by {.fn aes}."
    # Native pipe have higher precedence than + so any type of gg object can be
    # expected here, not just ggplot
    if (inherits(mapping, "gg")) {
      msg <- c(msg, "i" = "Did you use {.code %>%} or {.code |>} instead of {.code +}?")
    }

    cli::cli_abort(msg, call = call)
  }

  # For backward compatibility with pre-tidy-eval layers
  new_aes(mapping)
}

Layer <- ggproto("Layer", NULL,
  constructor = NULL,
  geom = NULL,
  geom_params = NULL,
  stat = NULL,
  stat_params = NULL,

  # These two fields carry state throughout rendering but will always be
  # calculated before use
  computed_geom_params = NULL,
  computed_stat_params = NULL,
  computed_mapping = NULL,

  data = NULL,
  aes_params = NULL,
  mapping = NULL,
  position = NULL,
  inherit.aes = FALSE,

  print = function(self) {
    if (!is.null(self$mapping)) {
      cat("mapping:", clist(self$mapping), "\n")
    }
    cat(snakeize(class(self$geom)[[1]]), ": ", clist(self$geom_params), "\n",
      sep = "")
    cat(snakeize(class(self$stat)[[1]]), ": ", clist(self$stat_params), "\n",
      sep = "")
    cat(snakeize(class(self$position)[[1]]), "\n")
  },

  layer_data = function(self, plot_data) {
    if (is.waive(self$data)) {
      data <- plot_data
    } else if (is.function(self$data)) {
      data <- self$data(plot_data)
      if (!is.data.frame(data)) {
        cli::cli_abort("{.fn layer_data} must return a {.cls data.frame}.")
      }
    } else {
      data <- self$data
    }
    if (is.null(data) || is.waive(data)) data else unrowname(data)
  },

  # hook to allow a layer access to the final layer data
  # in input form and to global plot info
  setup_layer = function(self, data, plot) {
    # For annotation geoms, it is useful to be able to ignore the default aes
    if (isTRUE(self$inherit.aes)) {
      self$computed_mapping <- defaults(self$mapping, plot$mapping)

      # Inherit size as linewidth from global mapping
      if (self$geom$rename_size &&
          "size" %in% names(plot$mapping) &&
          !"linewidth" %in% names(self$computed_mapping) &&
          "linewidth" %in% self$geom$aesthetics()) {
        self$computed_mapping$size <- plot$mapping$size
        deprecate_soft0("3.4.0", I("Using `size` aesthetic for lines"), I("`linewidth`"))
      }
      # defaults() strips class, but it needs to be preserved for now
      class(self$computed_mapping) <- "uneval"
    } else {
      self$computed_mapping <- self$mapping
    }

    data
  },

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
    plot$scales$add_defaults(evaled, plot$plot_env)

    # Check for discouraged usage in mapping
    warn_for_aes_extract_usage(aesthetics, data[setdiff(names(data), "PANEL")])

    # Check aesthetic values
    nondata_cols <- check_nondata_cols(evaled)
    if (length(nondata_cols) > 0) {
      issues <- paste0("{.code ", nondata_cols, " = ", as_label(aesthetics[[nondata_cols]]), "}")
      names(issues) <- rep("x", length(issues))
      cli::cli_abort(c(
        "Aesthetics are not valid data columns.",
        "x" = "The following aesthetics are invalid:",
        issues,
        "i" = "Did you mistype the name of a data column or forget to add {.fn after_stat}?"
      ))
    }

    n <- nrow(data)
    aes_n <- lengths(evaled)
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
    evaled <- lapply(evaled, unname)
    evaled <- as_gg_data_frame(evaled)
    evaled <- add_group(evaled)
    evaled
  },

  compute_statistic = function(self, data, layout) {
    if (empty(data))
      return(data_frame0())

    self$computed_stat_params <- self$stat$setup_params(data, self$stat_params)
    data <- self$stat$setup_data(data, self$computed_stat_params)
    self$stat$compute_layer(data, self$computed_stat_params, layout)
  },

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
    data_orig <- plot$scales$backtransform_df(data)

    # Add map stat output to aesthetics
    stat_data <- eval_aesthetics(
      substitute_aes(new), data_orig,
      mask = list(stage = stage_calculated)
    )
    # Check that all columns in aesthetic stats are valid data
    nondata_stat_cols <- check_nondata_cols(stat_data)
    if (length(nondata_stat_cols) > 0) {
      issues <- paste0("{.code ", nondata_stat_cols, " = ", as_label(aesthetics[[nondata_stat_cols]]), "}")
      names(issues) <- rep("x", length(issues))
      cli::cli_abort(c(
        "Aesthetics must be valid computed stats.",
        "x" = "The following aesthetics are invalid:",
        issues,
        "i" = "Did you map your stat in the wrong layer?"
      ))
    }

    stat_data <- data_frame0(!!!stat_data)

    # Add any new scales, if needed
    plot$scales$add_defaults(stat_data, plot$plot_env)
    # Transform the values, if the scale say it's ok
    # (see stat_spoke for one exception)
    if (self$stat$retransform) {
      stat_data <- plot$scales$transform_df(stat_data)
    }
    stat_data <- cleanup_mismatched_data(stat_data, nrow(data), "after_stat")

    data_frame0(!!!defaults(stat_data, data))
  },

  compute_geom_1 = function(self, data) {
    if (empty(data)) return(data_frame0())

    check_required_aesthetics(
      self$geom$required_aes,
      c(names(data), names(self$aes_params)),
      snake_class(self$geom)
    )
    self$computed_geom_params <- self$geom$setup_params(data, c(self$geom_params, self$aes_params))
    self$geom$setup_data(data, self$computed_geom_params)
  },

  compute_position = function(self, data, layout) {
    if (empty(data)) return(data_frame0())

    params <- self$position$setup_params(data)
    data <- self$position$setup_data(data, params)

    self$position$compute_layer(data, params, layout)
  },

  compute_geom_2 = function(self, data, params = self$aes_params, theme = NULL, ...) {
    # Combine aesthetics, defaults, & params
    if (empty(data)) return(data)

    aesthetics <- self$computed_mapping
    modifiers <- aesthetics[is_scaled_aes(aesthetics) | is_staged_aes(aesthetics) | is_themed_aes(aesthetics)]

    self$geom$use_defaults(data, params, modifiers, theme = theme, ...)
  },

  finish_statistics = function(self, data) {
    self$stat$finish_layer(data, self$computed_stat_params)
  },

  draw_geom = function(self, data, layout) {
    if (empty(data)) {
      n <- nrow(layout$layout)
      return(rep(list(zeroGrob()), n))
    }

    data <- self$geom$handle_na(data, self$computed_geom_params)
    self$geom$draw_layer(data, self$computed_geom_params, layout, layout$coord)
  }
)

check_subclass <- function(x, subclass,
                           argname = to_lower_ascii(subclass),
                           env = parent.frame(),
                           call = caller_env()) {
  if (inherits(x, subclass)) {
    x
  } else if (is_scalar_character(x)) {
    name <- paste0(subclass, camelize(x, first = TRUE))
    obj <- find_global(name, env = env)

    if (is.null(obj) || !inherits(obj, subclass)) {
      cli::cli_abort("Can't find {argname} called {.val {x}}.", call = call)
    } else {
      obj
    }
  } else {
    stop_input_type(x, as_cli("either a string or a {.cls {subclass}} object"))
  }
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

  ggproto("", geom, draw_key = draw_key)
}

cleanup_mismatched_data <- function(data, n, fun) {
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
