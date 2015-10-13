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
#'      Default checks that required aesthetics are present.
#' }
#'
#' And the following fields
#' \itemize{
#'   \item \code{required_aes}: a character vector giving the aesthetics
#'      that must be present for this position adjustment to work.
#' }
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Position <- ggproto("Position",
  required_aes = character(),

  setup_params = function(self, data) {
    list()
  },

  setup_data = function(self, data, params) {
    check_required_aesthetics(self$required_aes, names(data), snake_class(self))
    data
  },

  compute_layer = function(self, data, params, panel) {
    plyr::ddply(data, "PANEL", function(data) {
      if (empty(data)) return(data.frame())

      scales <- panel_scales(panel, data$PANEL[1])
      self$compute_panel(data = data, params = params, scales = scales)
    })
  },

  compute_panel = function(self, data, params, scales) {
    stop("Not implemented", call. = FALSE)
  }
)

#' Convenience function to transform all position variables.
#'
#' @param trans_x,trans_y Transformation functions for x and y aesthetics.
#'   (will transform x, xmin, xmax, xend etc)
#' @param ... Additional arguments passed to \code{trans_x} and \code{trans_y}.
#' @keywords internal
#' @export
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
