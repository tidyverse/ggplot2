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

  # This field carry state throughout rendering but will always be
  # calculated before use
  computed_legend_key_type = NULL,

  setup_layer = function(self, data, plot) {
    # process generic layer setup first
    data <- ggproto_parent(Layer, self)$setup_layer(data, plot)

    # automatically determine the name of the geometry column
    # and add the mapping if it doesn't exist
    if ((isTRUE(self$inherit.aes) && is.null(self$computed_mapping$geometry) &&
         is.null(plot$computed_mapping$geometry)) ||
        (!isTRUE(self$inherit.aes) && is.null(self$computed_mapping$geometry))) {
      if (is_sf(data)) {
        geometry_col <- attr(data, "sf_column")
        self$computed_mapping$geometry <- sym(geometry_col)
      }
    }

    # automatically determine the legend type
    if (is.null(self$legend_key_type)) {
      # first, set default value in case downstream tests fail
      self$computed_legend_key_type <- "polygon"

      # now check if the type should not be polygon
      if (!is.null(self$computed_mapping$geometry) && quo_is_symbol(self$computed_mapping$geometry)) {
        geometry_column <- as_name(self$computed_mapping$geometry)
        if (inherits(data[[geometry_column]], "sfc")) {
          sf_type <- detect_sf_type(data[[geometry_column]])
          if (sf_type == "point") {
            self$computed_legend_key_type <- "point"
          } else if (sf_type == "line") {
            self$computed_legend_key_type <- "line"
          }
        }
      }
    } else {
      self$computed_legend_key_type <- self$legend_key_type
    }
    data
  },
  compute_geom_1 = function(self, data) {
    data <- ggproto_parent(Layer, self)$compute_geom_1(data)

    # Add legend type after computed_geom_params has been calculated
    self$computed_geom_params$legend <- self$computed_legend_key_type
    data
  }
)

# helper function to find the geometry column
geom_column <- function(data) {
  w <- which(vapply(data, inherits, TRUE, what = "sfc"))
  if (length(w) == 0) {
    "geometry" # avoids breaks when objects without geometry list-column are examined
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
  geometry_type <- unique0(as.character(sf::st_geometry_type(sf)))
  if (length(geometry_type) != 1)  geometry_type <- "GEOMETRY"
  sf_types[geometry_type]
}
