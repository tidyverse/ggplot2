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
#' following methods:
#'
#' \itemize{
#'   \item \code{compute_layer(self, data, params, panel)} is called once
#'     per layer. \code{panel} is currently an internal data structure, so
#'     this method should not be overriden.
#'
#'   \item \code{compute_panel(self, data, params, panel)} is called once per
#'     panel and should return a modified data frame.
#'
#'     \code{data} is a data frame containing the variables named according
#'     to the aesthetics that they're mapped to. \code{scales} is a list
#'     containing the \code{x} and \code{y} scales. There functions are called
#'     before the facets are trained, so they are global scales, not local
#'     to the individual panels. \code{params} contains the parameters returned by
#'     \code{setup_params()}.
#'   \item \code{setup_params(data, params)}: called once for each layer.
#'      Used to setup defaults that need to complete dataset, and to inform
#'      the user of important choices. Should return list of parameters.
#'   \item \code{setup_data(data, params)}: called once for each layer,
#'      after \code{setp_params()}. Should return modified \code{data}.
#'      Default methods removes all rows containing a missing value in
#'      required aesthetics (with a warning if \code{!na.rm}).
#' }
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Position <- ggproto("Position",
  adjust = function(self, data, params) data,
  setup_params = function(self, data) {
    list()
  },
  setup_data = function(self, data, params) {
    data
  },
  compute_layer = function(self, data, params, panel) {
    plyr::ddply(data, "PANEL", function(data) {
      if (empty(data)) return(data.frame())

      scales <- panel_scales(panel, data$PANEL[1])
      self$compute_panel(data = data, params = params, scales = scales)
    })
  },
  compute_panel = function(self, data, scales) {
    stop("Not implemented", call. = FALSE)
  }
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
