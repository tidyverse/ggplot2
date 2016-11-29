#' @include order-.r
NULL

#' Order specification: no ordering.
#'
#'
#' @export
#' @examples
#' # order_null is the default ordering specification if you
#' # don't override it with order_as
order_null <- function() {
  ggproto(NULL, OrderNull)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
OrderNull <- ggproto("OrderNull", Order,

  order_data = function(self, data, position) {
    data
  },

  order_scales = function(self, panel_scales) {
    panel_scales
  }
)
