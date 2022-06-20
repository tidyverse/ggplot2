#' @section Positions:
#'
#' All `position_*()` functions (like `position_dodge()`) return a
#' `Position*` object (like `PositionDodge`). The `Position*`
#' object is responsible for adjusting the position of overlapping geoms.
#'
#' The way that the `position_*` functions work is slightly different from
#' the `geom_*` and `stat_*` functions, because a `position_*`
#' function actually "instantiates" the `Position*` object by creating a
#' descendant, and returns that.
#'
#' Each of the `Position*` objects is a [ggproto()] object,
#' descended from the top-level `Position`, and each implements the
#' following methods:
#'
#'   - `compute_layer(self, data, params, panel)` is called once
#'     per layer. `panel` is currently an internal data structure, so
#'     this method should not be overridden.
#'
#'   - `compute_panel(self, data, params, scales)` is called once per
#'     panel and should return a modified data frame.
#'
#'     `data` is a data frame containing the variables named according
#'     to the aesthetics that they're mapped to. `scales` is a list
#'     containing the `x` and `y` scales. There functions are called
#'     before the facets are trained, so they are global scales, not local
#'     to the individual panels. `params` contains the parameters returned by
#'     `setup_params()`.
#'   - `setup_params(data, params)`: called once for each layer.
#'      Used to setup defaults that need to complete dataset, and to inform
#'      the user of important choices. Should return list of parameters.
#'   - `setup_data(data, params)`: called once for each layer,
#'      after `setup_params()`. Should return modified `data`.
#'      Default checks that required aesthetics are present.
#'
#' And the following fields
#'   - `required_aes`: a character vector giving the aesthetics
#'      that must be present for this position adjustment to work.
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

  compute_layer = function(self, data, params, layout) {
    dapply(data, "PANEL", function(data) {
      if (empty(data)) return(data_frame0())

      scales <- layout$get_scales(data$PANEL[1])
      self$compute_panel(data = data, params = params, scales = scales)
    })
  },

  compute_panel = function(self, data, params, scales) {
    cli::cli_abort("Not implemented")
  }
)

#' Convenience function to transform all position variables.
#'
#' @param trans_x,trans_y Transformation functions for x and y aesthetics.
#'   (will transform x, xmin, xmax, xend etc)
#' @param ... Additional arguments passed to `trans_x` and `trans_y`.
#' @keywords internal
#' @export
transform_position <- function(df, trans_x = NULL, trans_y = NULL, ...) {
  # Treat df as list during transformation for faster set/get
  oldclass <- class(df)
  df <- unclass(df)
  scales <- aes_to_scale(names(df))

  if (!is.null(trans_x)) {
    df[scales == "x"] <- lapply(df[scales == "x"], trans_x, ...)
  }
  if (!is.null(trans_y)) {
    df[scales == "y"] <- lapply(df[scales == "y"], trans_y, ...)
  }

  class(df) <- oldclass

  df
}
