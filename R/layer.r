# Create a new layer
# Layer objects store the layer of an object.
#
# They have the following attributes:
#
#  * data
#  * geom + parameters
#  * statistic + parameters
#  * position + parameters
#  * aesthetic mapping
#  * flag for display guide: TRUE/FALSE/NA. in the case of NA, decision depends on a guide itself.
#
# Can think about grob creation as a series of data frame transformations.
Layer <- ggproto("Layer", NULL,
  geom = NULL,
  geom_params = NULL,
  stat = NULL,
  stat_params = NULL,
  data = NULL,
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

  compute_aesthetics = function(self, data, plot) {
    # For annotation geoms, it is useful to be able to ignore the default aes
    if (self$inherit.aes) {
      aesthetics <- defaults(self$mapping, plot$mapping)
    } else {
      aesthetics <- self$mapping
    }

    # Drop aesthetics that are set or calculated
    set <- names(aesthetics) %in% names(self$geom_params)
    calculated <- is_calculated_aes(aesthetics)
    aesthetics <- aesthetics[!set & !calculated]

    # Override grouping if set in layer
    if (!is.null(self$geom_params$group)) {
      aesthetics[["group"]] <- self$geom_params$group
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
      n <- max(vapply(evaled, length, integer(1)))
    }
    check_aesthetics(evaled, n)

    if (empty(data) && n > 0) {
      evaled$PANEL <- 1
    } else {
      evaled$PANEL <- data$PANEL
    }
    data.frame(evaled)
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
    data <- self$geom$setup_data(data, self$geom_params)

    check_required_aesthetics(
      self$geom$required_aes,
      c(names(data), names(self$geom_params)),
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
    self$geom$use_defaults(data, self$geom_params)
  },

  draw_geom = function(self, data, panel, coord) {
    if (empty(data)) return(list(zeroGrob()))

    self$geom$draw_layer(data, self$geom_params, panel, coord)
  }
)


#' Create a new layer
#'
#' @export
#' @inheritParams geom_point
#' @param geom,stat,position Geom, stat and position adjustment to use in
#'   this layer. Can either be the name of a ggproto object, or the object
#'   itself.
#' @param geom_params,stat_params,params Additional parameters to the
#'   \code{geom} and \code{stat}. If supplied individual in \code{...} or as a
#'   list in \code{params}, \code{layer} does it's best to figure out which
#'   arguments belong to which. To be explicit, supply as individual lists to
#'   \code{geom_param} and \code{stat_param}.
#' @param mapping Set of aesthetic mappings created by \code{\link{aes}} or
#'   \code{\link{aes_string}}. If specified and \code{inherit.aes = TRUE},
#'   is combined with the default mapping at the top level of the plot.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param subset DEPRECATED. An older way of subsetting the dataset used in a
#'   layer.
#' @examples
#' # geom calls are just a short cut for layer
#' ggplot(mpg, aes(displ, hwy)) + geom_point()
#' # shortcut for
#' ggplot(mpg, aes(displ, hwy)) +
#'   layer(geom = "point", stat = "identity", position = "identity")
layer <- function(geom = NULL, geom_params = list(), stat = NULL,
  stat_params = list(), data = NULL, mapping = NULL, position = NULL,
  params = list(), inherit.aes = TRUE, subset = NULL, show.legend = NA)
{
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
    stop("Mapping should be a list of unevaluated mappings created by aes or aes_string", call. = FALSE)
  }

  if (is.character(geom)) geom <- make_geom(geom)
  if (is.character(stat)) stat <- make_stat(stat)
  if (is.character(position)) position <- make_position(position)

  # Categorize items from params into geom_params and stat_params
  if (length(params) > 0) {
    geom_params <- utils::modifyList(params, geom_params)
    stat_params <- utils::modifyList(params, stat_params)
  }
  geom_params <- rename_aes(geom_params)

  ggproto("LayerInstance", Layer,
    geom = geom,
    geom_params = geom_params,
    stat = stat,
    stat_params = stat_params,
    data = data,
    mapping = mapping,
    subset = subset,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

is.layer <- function(x) inherits(x, "Layer")
