#' Create a new sf layer that auto-maps geometry data
#'
#' The `layer_sf()` function is a variant of [`layer()`] meant to be used by
#' extension developers who are writing new sf-based geoms or stats.
#' The sf layer checks whether the data contains a geometry column, and
#' if one is found it is automatically mapped to the `geometry` aesthetic.
#' @include layer.R
#' @inheritParams layer
#' @keywords internal
#' @export
layer_sf <- function(geom = NULL, stat = NULL,
                     data = NULL, mapping = NULL,
                     position = NULL, params = list(),
                     inherit.aes = TRUE, check.aes = TRUE, check.param = TRUE,
                     show.legend = NA) {
  call_env <- caller_env()
  if (is.character(show.legend)) {
    legend_key_type <- show.legend
    show.legend <- TRUE
  } else {
    legend_key_type <- NULL
  }

  # inherit from LayerSf class to add `legend_key_type` slot
  layer_class <- ggproto(NULL, LayerSf,
    constructor = frame_call(call_env),
    legend_key_type = legend_key_type
  )

  layer(
    geom = geom, stat = stat, data = data, mapping = mapping,
    position = position, params = params, inherit.aes = inherit.aes,
    check.aes = check.aes, check.param = check.param,
    show.legend = show.legend, layer_class = layer_class
  )
}

LayerSf <- ggproto("LayerSf", Layer,
  legend_key_type = NULL,

  setup_layer = function(self, data, plot) {
    # process generic layer setup first
    data <- ggproto_parent(Layer, self)$setup_layer(data, plot)

    # automatically determine the name of the geometry column
    # and add the mapping if it doesn't exist
    if (is.null(self$computed_mapping$geometry) && is_sf(data)) {
      geometry_col <- attr(data, "sf_column")
      self$computed_mapping$geometry <- sym(geometry_col)
    }
    data
  },
  compute_geom_1 = function(self, data) {
    data <- ggproto_parent(Layer, self)$compute_geom_1(data)

    # Determine the legend type
    legend_type <- self$legend_key_type
    if (is.null(legend_type)) {
      legend_type <- switch(
        detect_sf_type(data$geometry),
        point = "point", line = "line", "other"
      )
    }

    # Add legend type after computed_geom_params has been calculated
    self$computed_geom_params$legend <- legend_type
    data
  },

  compute_geom_2 = function(self, data, params = self$aes_params, ...) {
    if (empty(data)) return(data)
    data$geometry <- data$geometry %||% self$computed_geom_params$legend
    ggproto_parent(Layer, self)$compute_geom_2(data, params, ...)
  }
)

# helper function to find the geometry column
geom_column <- function(data) {
  w <- which(vapply(data, inherits, TRUE, what = "sfc"))
  if (length(w) == 0) {
    if (!is.character(data[["geometry"]])) {
      "geometry" # avoids breaks when objects without geometry list-column are examined
    } else {
      # Avoids a rare case where computed_geom_params$legend is present but there is no actual geometry column
      ""
    }
  } else {
    # this may not be best in case more than one geometry list-column is present:
    if (length(w) > 1)
      cli::cli_warn("More than one geometry column present: taking the first")
    w[[1]]
  }
}

# helper function to determine whether data contains sf column
is_sf <- function(data) {
  inherits(data, "sf")
}

# needed so that sf columns can be mapped without scale

#' @export
scale_type.sfc <- function(x) "identity"

# helper function to determine the geometry type of sf object
detect_sf_type <- function(sf) {
  if (is.null(sf)) {
    return("other")
  }
  geometry_type <- unique0(as.character(sf::st_geometry_type(sf)))
  if (length(geometry_type) != 1)  geometry_type <- "GEOMETRY"
  sf_types[geometry_type]
}
