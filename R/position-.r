# Position adjustment occurs over all groups within a geom
# They work only with discrete x scales and may affect x and y position.
# Should occur after statistics and scales have been applied.
Position <- proto2("Position", TopLevel,
  type = "position",
  adjust = function(self, data, scales, ...) data
)

# Convenience function to ensure that all position variables
# (x, xmin, xmax, xend) are transformed in the same way
transform_position <- function(df, trans_x = NULL, trans_y = NULL, ...) {
  scales <- aes_to_scale(names(df))

  if (!is.null(trans_x)) {
    df[scales == "x"] <- lapply(df[scales == "x"], trans_x, ...)
  }
  if (!is.null(trans_y)) {
    df[scales == "y"] <- lapply(df[scales == "y"], trans_y, ...)
  }

  df
}

# make_position("dodge") returns PositionDodge
make_position <- function(class) {
  name <- paste0("Position", camelize(class, first = TRUE))
  if (!exists(name)) {
    stop("No position called ", name, ".", call. = FALSE)
  }

  obj <- get(name)
  if (!inherits(obj, "Position")) {
    stop("Found object is not a position", call. = FALSE)
  }

  obj
}
