#' Identity statistic.
#'
#' The identity statistic leaves the data unchanged.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points
#'    on this layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link{borders}}.
#' @param ... other arguments passed on to \code{\link{layer}}. This can
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + stat_identity()
stat_identity <- function(mapping = NULL, data = NULL,
                          geom = "point", position = "identity",
                          ...,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatIdentity <- ggproto("StatIdentity", Stat,
  compute_layer = function(data, scales, params) {
    data
  }
)
