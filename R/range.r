#' Mutable range objects
#'
#' Mutable ranges have a two methods (\code{train} and \code{reset}), and make
#' it possible to build up complete ranges with multiple passes.
#'
#' These range objects should be instantiated with
#' \code{\link{continuous_range}} and \code{\link{discrete_range}}.
#'
#' @aliases RangeDiscrete RangeContinuous
#' @keywords internal
#' @format NULL
#' @usage NULL
#' @export RangeDiscrete RangeContinuous
Range <- ggproto("Range", NULL,
  range = NULL,
  reset = function(self) {
    self$range <- NULL
  }
)

RangeDiscrete <- ggproto("RangeDiscrete", Range,
  train = function(self, x, drop = FALSE) {
    self$range <- scales::train_discrete(x, self$range, drop)
  }
)

RangeContinuous <- ggproto("RangeContinuous", Range,
  train = function(self, x) {
    self$range <- scales::train_continuous(x, self$range)
  }
)

#' @rdname Range
#' @format NULL
#' @export
continuous_range <- function() {
  ggproto(NULL, RangeContinuous)
}

#' @rdname Range
#' @format NULL
#' @export
discrete_range <- function() {
  ggproto(NULL, RangeDiscrete)
}
