# A special sf layer that auto-maps geometry data to the `geometry` aesthetic

#' @export
#' @include layer.r
#' @rdname ggsf
#' @usage NULL
#' @format NULL
LayerSf <- ggproto("LayerSf", Layer,
  setup_layer = function(self, data, plot) {
    # process generic layer setup first
    data <- ggproto_parent(Layer, self)$setup_layer(data, plot)

    # automatically determine the name of the geometry column
    # and add the mapping if it doesn't exist
    if ((isTRUE(self$inherit.aes) && is.null(self$mapping$geometry) && is.null(plot$mapping$geometry)) ||
        (!isTRUE(self$inherit.aes) && is.null(self$mapping$geometry))) {
      if (is_sf(data)) {
        geometry_col <- attr(data, "sf_column")
        self$mapping$geometry <- as.name(geometry_col)
      }
    }
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
      warning("more than one geometry column present: taking the first")
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

