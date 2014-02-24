# Position adjustment occurs over all groups within a geom
# They work only with discrete x scales and may affect x and y position.
# Should occur after statistics and scales have been applied.
Position <- proto(TopLevel, expr = {
  adjust <- function(., data, scales, ...) data

  class <- function(.) "position"

  width <- NULL
  height <- NULL
  new <- function(., width = NULL, height = NULL) {
    .$proto(width = width, height = height)
  }

  parameters <- function(.) {
    pnames <- setdiff(names(formals(get("new", .))), ".")
    values <- lapply(pnames, get, envir = .)
    names(values) <- pnames

    values
  }

  pprint <- function(., newline=TRUE) {
    cat("position_", .$objname, ": (", clist(.$parameters()), ")", sep="")
    if (newline) cat("\n")
  }

})


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
