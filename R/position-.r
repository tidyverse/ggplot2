# Position adjustment occurs over all groups within a geom
# They work only with discrete x scales and may affect x and y position.
# Should occur after statistics and scales have been applied.
Position <- R6::R6Class("Position", inherit = TopLevelR6,
  public = list(
    adjust = function(data, scales, ...) data,

    class = function() "position",

    width = NULL,
    height = NULL,
    initialize = function(width = NULL, height = NULL) {
      self$width <- width
      self$height <- height
    },

    parameters = function() {
      pnames <- names(formals(self$initialize))
      values <- mget(pnames, envir = self)
      names(values) <- pnames

      values
    },

    pprint = function(newline=TRUE) {
      cat("position_", self$objname, ": (", clist(self$parameters()), ")", sep="")
      if (newline) cat("\n")
    }
  )
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
