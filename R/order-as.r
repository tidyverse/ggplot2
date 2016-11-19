#' @include order-.r
NULL

#' Order as specified by formula
#'
#' @param ordering a formula with the axes (of the tabular display) to order on
#'   the LHS and the aesthetics (of the tabular display) to use for determining
#'   the order on the RHS. Example x ~ y to order the x axis by values on y
#' @param facets How to handle ordering across facets. "within" will produce
#'   different axis ordering within each facet panel. "betwee" will order the
#'   panels themselves. "all" will do both.
#' @param order_f Function used to create ordering.
#' @param na.rm Should missing values be removed? Passed to order_f
#' @param ... Additional arguments to pass to order_f
#' @param na.rm
#' @export
order_as <- function(ordering, facets = "all", order_f = max, na.rm = TRUE, ...) {

  # Ordering can either be a formula, a string, or a list of things to be
  # convert to quoted
  if (is.character(ordering)) {
    ordering <- stats::as.formula(ordering)
  }
  if (is.formula(ordering)) {
    lhs <- function(x) if (length(x) == 2) NULL else x[-3]
    rhs <- function(x) if (length(x) == 2) x else x[-2]

    to_order <- as.quoted(lhs(ordering))
    #to_order <- to_order[!sapply(to_order, identical, as.name("."))]
    order_by <- as.quoted(rhs(ordering))
    #order_by <- order_by[!sapply(order_by, identical, as.name("."))]
  }
  if (is.list(ordering)) {
    to_order <- as.quoted(ordering[[1]])
    order_by <- as.quoted(ordering[[2]])
  }
  if (length(to_order) == 0) {
    stop("Must specify at least one axis to order", call. = FALSE)
  }
  if (length(order_by) == 0) {
    stop("Must specify at least one aesthetic to order by", call. = FALSE)
  }
  to_order <- as.character(to_order)
  order_by <- as.character(order_by)

  facets <- match.arg(facets, c("all", "within", "between"))

  ggproto(NULL, Order,
          params = list(to_order = to_order, order_by = order_by, facets = facets)
  )
}
