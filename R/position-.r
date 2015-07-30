#' @section Positions:
#'
#' All \code{position_*} functions (like \code{position_dodge}) return a
#' \code{Position*} object (like \code{PositionDodge}). The \code{Position*}
#' object is responsible for adjusting the position of overlapping geoms.
#'
#' The way that the \code{position_*} functions work is slightly different from
#' the \code{geom_*} and \code{stat_*} functions, because a \code{position_*}
#' function actually "instantiates" the \code{Position*} object by creating a
#' descendant, and returns that.
#'
#' Each of the \code{Position*} objects is a \code{\link{ggproto}} object,
#' descended from the top-level \code{Position}, and each implements the
#' following method:
#'
#' \itemize{
#'   \item \code{adjust}: Adjusts the position of overlapping geoms.
#' }
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Position <- ggproto("Position",
  adjust = function(data, scales, ...) data
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
