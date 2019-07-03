
#' View scale constructor
#'
#' View scales are an implementation of `Scale` objects that have fixed
#' limits, dimension, breaks, labels, and minor breaks. They are used as
#' the immutable result of the trained scales that have been assigned
#' `limits` and a `continuous_range` from the coordinate system's
#' implementation of scale expantion.
#'
#' @param scale The scale from which to construct a view scale.
#' @param limits The final scale limits
#' @param continuous_range The final dimensions of the scale
#'
#' @noRd
view_scale_primary <- function(scale, limits = scale$get_limits(),
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
    guide = scale$guide,
    position = scale$position,
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

# this function is a hack that is difficult to avoid given the complex implementation of second axes
view_scale_secondary <- function(scale, limits = scale$get_limits(),
                                 continuous_range = scale$dimension(limits = limits)) {
  if (is.null(scale$secondary.axis) || is.waive(scale$secondary.axis) || scale$secondary.axis$empty()) {
    view_scale_empty()
  } else {
    scale$secondary.axis$init(scale)
    break_info <- scale$secondary.axis$break_info(continuous_range, scale)
    names(break_info) <- gsub("sec\\.", "", names(break_info))

    # flip position from the original scale by default
    # this can (should) be overridden in the guide
    position <- switch(scale$position,
      top = "bottom",
      bottom = "top",
      left = "right",
      right = "left",
      scale$position
    )

    ggproto(NULL, ViewScale,
      scale = scale,
      guide = scale$secondary_axis$guide,
      position = position,
      break_info = break_info,
      # as far as scales are concerned, this is a regular scale with
      # different breaks and labels in a different data space
      aesthetics = scale$aesthetics,
      name = scale$sec_name(),
      make_title = function(self, title) self$scale$make_sec_title(title),

      dimension = function(self) self$break_info$range,
      get_limits = function(self) self$break_info$range,
      get_breaks = function(self) self$break_info$major_source,
      get_breaks_minor = function(self) self$break_info$minor_source,
      break_positions = function(self) self$break_info$major,
      break_positions_minor = function(self) self$break_info$minor,
      get_labels = function(self) self$break_info$labels,
      rescale = function(x) rescale(x, from = break_info$range, to = c(0, 1))
    )
  }
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
  # map, rescale, and make_title need a reference
  # to the original scale
  scale = ggproto(NULL, Scale),
  guide = guide_none(),
  position = NULL,
  aesthetics = NULL,
  name = waiver(),
  scale_is_discrete = FALSE,
  limits = NULL,
  continuous_range = NULL,
  breaks = NULL,
  labels = NULL,
  minor_breaks = NULL,

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
    if (self$is_discrete()) {
      self$scale$map(x, self$limits)
    } else {
      self$scale$map(x, self$continuous_range)
    }
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
