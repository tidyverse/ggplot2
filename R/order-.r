#' @include ggproto.r
NULL

#' @section Ordering:
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Order <- ggproto("Order", NULL,

  order_map = NULL,

  train_order = function(self, order_aes, data) {

  },

  order_data = function(self, data) {

  },

  order_scales = function(self, scales) {

  }

)
