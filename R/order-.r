#' @include ggproto.r
NULL

#' @section Ordering:
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Order <- ggproto("Order", NULL,

  order_map = list(),
  params = list(),

  train_order = function(self, data) {
    to_order <- self$params$to_order
    order_by <- self$params$order_by

    # Checks on ordering variables -----

    if (!all(c(to_order, order_by) %in% .all_aesthetics)) {
      stop("Valid aesthetics are required for ordering such as x or y.")
    }

    convert_to <- to_order %in% names(.base_to_ggplot)
    to_order[convert_to] <- unname(.base_to_ggplot[to_order[convert_to]])

    convert_by <- order_by %in% names(.base_to_ggplot)
    order_by[convert_by] <- unname(.base_to_ggplot[order_by[convert_by]])

    if (!all(c(to_order, order_by) %in% names(data)))
      stop("One or more of the ordering variables cannot be found.
           Please specify aesthetics used by the geom layers such as x or y.")

    # Ordering -----

    # This will only work for ONE order_by
    # Need to pass ... to tapply
    # Will also need something to handle different facet options (via reference to PANEL)
    order_map <- lapply(data[to_order], function(o) {
      order_as <- order(tapply(data[[order_by]], o, self$params$order_f, self$params$na.rm))

      new <- as.factor(o)
      new <- as.numeric(factor(new, levels = levels(new)[order_as]))

      data.frame(old = o, new = new)
    })

    self$order_map <- order_map
    order_map
  },

  order_data = function(self, data) {
    order_map <- self$order_map
    if (length(order_map) == 0) order_map <- self$train_order(data)

    # 1-to-1 reordering
    data[names(order_map)] <- lapply(order_map, function(i) i$new)

    return(data)
  },

  order_scales = function(self, scales) {
    if (length(self$order_map) == 0) stop("order_scales cannot be executed before train_data or order_data")
    order_map <- lapply(self$order_map, unique_order)


    ## NOT READY
    return(order_map)
  }

)



# Helpers -----------------------------------------------------------------

#' Is this object an ordering specification?
#'
#' @param x object to test
#' @keywords internal
#' @export
is.order <- function(x) inherits(x, "Order")

unique_order <- function(order_map) {
  order_map[!duplicated.data.frame(order_map), ]
}
