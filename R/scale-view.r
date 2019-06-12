
#' View scale constructor
#'
#' View scales the immutable result of the trained and expanded scales.
#' Calls to its methods should return identical results to the scale
#' methods.
#'
#' @param scale The scale from which to construct a view scale.
#' @param limits The final scale limits
#' @param continuous_range The final dimensions of the scale
#'
#' @noRd
view_scale <- function(scale, limits = scale$get_limits(),
                       continuous_range = scale$dimension(limits = limits)) {

  if(!scale$is_discrete()) {
    breaks <- scale$get_breaks(continuous_range)
    breaks <- breaks[is.finite(breaks)]
    minor_breaks <- scale$get_breaks_minor(b = breaks, limits = continuous_range)
  } else {
    breaks <- scale$get_breaks(limits)
    minor_breaks <- scale$get_breaks_minor(b = breaks, limits = limits)
  }

  ggproto(NULL, ViewScale,
    scale = scale,
    aesthetics = scale$aesthetics,
    name = scale$name,
    scale_is_discrete = scale$is_discrete(),
    limits = limits,
    continuous_range = continuous_range,
    breaks = breaks,
    labels = scale$get_labels(breaks),
    minor_breaks = minor_breaks
  )
}

view_scale_empty <- function() {
  ggproto(NULL, ViewScale,
    is_empty = function() TRUE,
    is_discrete = function() NA,
    dimension = function() c(0, 1),
    get_limits = function() c(0, 1),
    get_breaks = function() NULL,
    get_breaks_minor = function() NULL,
    get_labels = function() NULL,
    rescale = function(x) stop("Not implemented", call. = FALSE),
    map = function(x) stop("Not implemented", call. = FALSE),
    make_title = function(title) title,
    break_positions = function() NULL,
    break_positions_minor = function() NULL
  )
}

ViewScale <- ggproto("ViewScale", NULL,
  aesthetics = NULL,
  name = waiver(),
  scale_is_discrete = FALSE,
  limits = NULL,
  continuous_range = NULL,
  breaks = NULL,
  labels = NULL,
  minor_breaks = NULL,

  # map, rescale, and make_title need a reference
  # to the original scale
  scale = ggproto(NULL, Scale),

  is_empty = function(self) {
    is.null(self$get_breaks()) && is.null(self$get_breaks_minor())
  },
  is_discrete = function(self) self$scale_is_discrete,
  dimension = function(self) self$continuous_range,
  get_limits = function(self) self$limits,
  get_breaks = function(self) self$breaks,
  get_breaks_minor = function(self) self$minor_breaks,
  get_labels = function(self) self$labels,
  rescale = function(self, x) {
    self$scale$rescale(x, self$limits, self$continuous_range)
  },
  map = function(self, x) {
    self$scale$map(x, self$limits)
  },
  make_title = function(self, title) {
    self$scale$make_title(title)
  },
  break_positions = function(self) {
    self$rescale(self$get_breaks())
  },
  break_positions_minor = function(self) {
    b <- self$get_breaks_minor()
    if (is.null(b)) {
      return(NULL)
    }

    self$rescale(b)
  }
)
