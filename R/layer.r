#' Create a new layer
#'
#' A layer is a combination of data, stat and geom with a potential position
#' adjustment. Usually layers are created using \code{geom_*} or \code{stat_*}
#' calls but it can also be created directly using this function.
#'
#' @export
#' @inheritParams geom_point
#' @param mapping Set of aesthetic mappings created by \code{\link{aes}} or
#'   \code{\link{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If \code{NULL}, the default, the data is inherited from the plot
#'    data as specified in the call to \code{\link{ggplot}}.
#'
#'    A \code{data.frame}, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    \code{\link{fortify}} for which variables will be created.
#'
#'    A \code{function} will be called with a single argument,
#'    the plot data. The return value must be a \code{data.frame.}, and
#'    will be used as the layer data.
#' @param geom The geometric object to use display the data
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param params Additional parameters to the \code{geom} and \code{stat}.
#' @param subset DEPRECATED. An older way of subsetting the dataset used in a
#'   layer.
#' @examples
#' # geom calls are just a short cut for layer
#' ggplot(mpg, aes(displ, hwy)) + geom_point()
#' # shortcut for
#' ggplot(mpg, aes(displ, hwy)) +
#'   layer(geom = "point", stat = "identity", position = "identity",
#'     params = list(na.rm = FALSE)
#'   )
#'
#' # use a function as data to plot a subset of global data
#' ggplot(mpg, aes(displ, hwy)) +
#'   layer(geom = "point", stat = "identity", position = "identity",
#'     data = head, params = list(na.rm = FALSE)
#'   )
#'
layer <- function(geom = NULL, stat = NULL,
                  data = NULL, mapping = NULL,
                  position = NULL, params = list(),
                  inherit.aes = TRUE, subset = NULL, show.legend = NA) {
  if (is.null(geom))
    stop("Attempted to create layer with no geom.", call. = FALSE)
  if (is.null(stat))
    stop("Attempted to create layer with no stat.", call. = FALSE)
  if (is.null(position))
    stop("Attempted to create layer with no position.", call. = FALSE)

  # Handle show_guide/show.legend
  if (!is.null(params$show_guide)) {
    warning("`show_guide` has been deprecated. Please use `show.legend` instead.",
      call. = FALSE)
    show.legend <- params$show_guide
    params$show_guide <- NULL
  }
  if (!is.logical(show.legend) || length(show.legend) != 1) {
    warning("`show.legend` must be a logical vector of length 1.", call. = FALSE)
    show.legend <- FALSE
  }

  data <- fortify(data)
  if (!is.null(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping must be created by `aes()` or `aes_()`", call. = FALSE)
  }

  if (is.character(geom))
    geom <- find_subclass("Geom", geom)
  if (is.character(stat))
    stat <- find_subclass("Stat", stat)
  if (is.character(position))
    position <- find_subclass("Position", position)

  # Special case for na.rm parameter needed by all layers
  if (is.null(params$na.rm)) {
    params$na.rm <- FALSE
  }

  # Split up params between aesthetics, geom, and stat
  params <- rename_aes(params)
  aes_params  <- params[intersect(names(params), geom$aesthetics())]
  geom_params <- params[intersect(names(params), geom$parameters(TRUE))]
  stat_params <- params[intersect(names(params), stat$parameters(TRUE))]

  all <- c(geom$parameters(TRUE), stat$parameters(TRUE), geom$aesthetics())
  extra <- setdiff(names(params), all)
  if (length(extra) > 0) {
    stop("Unknown parameters: ", paste(extra, collapse = ", "), call. = FALSE)
  }

  ggproto("LayerInstance", Layer,
    geom = geom,
    geom_params = geom_params,
    stat = stat,
    stat_params = stat_params,
    data = data,
    mapping = mapping,
    aes_params = aes_params,
    subset = subset,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

Layer <- ggproto("Layer", NULL,
  geom = NULL,
  geom_params = NULL,
  stat = NULL,
  stat_params = NULL,
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
      plot_data
    } else if (is.function(self$data)) {
      data <- self$data(plot_data)
      if (!is.data.frame(data)) {
        stop("Data function must return a data.frame", call. = FALSE)
      }
      data
    } else {
      self$data
    }
  },

  compute_aesthetics = function(self, data, plot) {
    # For annotation geoms, it is useful to be able to ignore the default aes
    if (self$inherit.aes) {
      aesthetics <- defaults(self$mapping, plot$mapping)
    } else {
      aesthetics <- self$mapping
    }

    # Drop aesthetics that are set or calculated
    set <- names(aesthetics) %in% names(self$aes_params)
    calculated <- is_calculated_aes(aesthetics)
    aesthetics <- aesthetics[!set & !calculated]

    # Override grouping if set in layer
    if (!is.null(self$geom_params$group)) {
      aesthetics[["group"]] <- self$aes_params$group
    }

    # Old subsetting method
    if (!is.null(self$subset)) {
      include <- data.frame(plyr::eval.quoted(self$subset, data, plot$env))
      data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
    }

    scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)

    # Evaluate and check aesthetics
    aesthetics <- compact(aesthetics)
    evaled <- lapply(aesthetics, eval, envir = data, enclos = plot$plot_env)

    n <- nrow(data)
    if (n == 0) {
      # No data, so look at longest evaluated aesthetic
      if (length(evaled) == 0) {
        n <- 0
      } else {
        n <- max(vapply(evaled, length, integer(1)))
      }
    }
    check_aesthetics(evaled, n)

    # Set special group and panel vars
    if (empty(data) && n > 0) {
      evaled$PANEL <- 1
    } else {
      evaled$PANEL <- data$PANEL
    }
    evaled <- lapply(evaled, unname)
    evaled <- data.frame(evaled, stringsAsFactors = FALSE)
    evaled <- add_group(evaled)
    evaled
  },

  compute_statistic = function(self, data, panel) {
    if (empty(data))
      return(data.frame())

    params <- self$stat$setup_params(data, self$stat_params)
    data <- self$stat$setup_data(data, params)
    self$stat$compute_layer(data, params, panel)
  },

  map_statistic = function(self, data, plot) {
    if (empty(data)) return(data.frame())

    # Assemble aesthetics from layer, plot and stat mappings
    aesthetics <- self$mapping
    if (self$inherit.aes) {
      aesthetics <- defaults(aesthetics, plot$mapping)
    }
    aesthetics <- defaults(aesthetics, self$stat$default_aes)
    aesthetics <- compact(aesthetics)

    new <- strip_dots(aesthetics[is_calculated_aes(aesthetics)])
    if (length(new) == 0) return(data)

    # Add map stat output to aesthetics
    stat_data <- plyr::quickdf(lapply(new, eval, data, baseenv()))
    names(stat_data) <- names(new)

    # Add any new scales, if needed
    scales_add_defaults(plot$scales, data, new, plot$plot_env)
    # Transform the values, if the scale say it's ok
    # (see stat_spoke for one exception)
    if (self$stat$retransform) {
      stat_data <- scales_transform_df(plot$scales, stat_data)
    }

    cunion(stat_data, data)
  },

  compute_geom_1 = function(self, data) {
    if (empty(data)) return(data.frame())
    data <- self$geom$setup_data(data, c(self$geom_params, self$aes_params))

    check_required_aesthetics(
      self$geom$required_aes,
      c(names(data), names(self$aes_params)),
      snake_class(self$geom)
    )

    data
  },

  compute_position = function(self, data, panel) {
    if (empty(data)) return(data.frame())

    params <- self$position$setup_params(data)
    data <- self$position$setup_data(data, params)

    self$position$compute_layer(data, params, panel)
  },

  compute_geom_2 = function(self, data) {
    # Combine aesthetics, defaults, & params
    if (empty(data)) return(data)

    self$geom$use_defaults(data, self$aes_params)
  },

  draw_geom = function(self, data, panel, coord) {
    if (empty(data)) {
      n <- nrow(panel$layout)
      return(rep(list(zeroGrob()), n))
    }

    data <- self$geom$handle_na(data, self$geom_params)
    self$geom$draw_layer(data, self$geom_params, panel, coord)
  }
)

is.layer <- function(x) inherits(x, "Layer")


find_subclass <- function(super, class) {
  name <- paste0(super, camelize(class, first = TRUE))
  if (!exists(name)) {
    stop("No ", tolower(super), " called ", name, ".", call. = FALSE)
  }

  obj <- get(name)
  if (!inherits(obj, super)) {
    stop("Found object is not a ", tolower(super), ".", call. = FALSE)
  }

  obj
}
