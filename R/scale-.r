
#' Continuous scale constructor
#'
#' @export
#' @param aesthetics The names of the aesthetics that this scale works with.
#' @param scale_name The name of the scale that should be used for error messages
#'   associated with this scale.
#' @param palette A palette function that when called with a numeric vector with
#'   values between 0 and 1 returns the corresponding output values
#'   (e.g., [scales::area_pal()]).
#' @param name The name of the scale. Used as the axis or legend title. If
#'   `waiver()`, the default, the name of the scale is taken from the first
#'   mapping used for that aesthetic. If `NULL`, the legend title will be
#'   omitted.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     [transformation object][scales::trans_new()]
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks
#'     as output (e.g., a function returned by [scales::extended_breaks()])
#' @param minor_breaks One of:
#'   - `NULL` for no minor breaks
#'   - `waiver()` for the default breaks (one minor break between
#'     each major break)
#'   - A numeric vector of positions
#'   - A function that given the limits returns a vector of minor breaks.
#' @param labels One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels computed by the
#'     transformation object
#'   - A character vector giving labels (must be same length as `breaks`)
#'   - A function that takes the breaks as input and returns labels
#'     as output
#' @param limits One of:
#'   - `NULL` to use the default scale range
#'   - A numeric vector of length two providing limits of the scale.
#'     Use `NA` to refer to the existing minimum or maximum
#'   - A function that accepts the existing (automatic) limits and returns
#'     new limits
#'   Note that setting limits on positional scales will **remove** data outside of the limits.
#'   If the purpose is to zoom, use the limit argument in the coordinate system
#'   (see [coord_cartesian()]).
#' @param rescaler A function used to scale the input values to the
#'   range \[0, 1]. This is always [scales::rescale()], except for
#'   diverging and n colour gradients (i.e., [scale_colour_gradient2()],
#'   [scale_colour_gradientn()]). The `rescaler` is ignored by position
#'   scales, which ways use [scales::rescale()].
#' @param oob One of:
#'   - Function that handles limits outside of the scale limits
#'   (out of bounds).
#'   - The default ([scales::censor()]) replaces out of
#'   bounds values with `NA`.
#'   - [scales::squish()] for squishing out of bounds values into range.
#'   - [scales::squish_infinite()] for squishing infitite values into range.
#' @param na.value Missing values will be replaced with this value.
#' @param trans For continuous scales, the name of a transformation object
#'   or the object itself. Built-in transformations include "asn", "atanh",
#'   "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2",
#'   "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal",
#'   "reverse", "sqrt" and "time".
#'
#'   A transformation object bundles together a transform, its inverse,
#'   and methods for generating breaks and labels. Transformation objects
#'   are defined in the scales package, and are called `<name>_trans` (e.g.,
#'   [scales::boxcox_trans()]). You can create your own
#'   transformation with [scales::trans_new()].
#' @param guide A function used to create a guide or its name. See
#'   [guides()] for more information.
#' @param expand For position scales, a vector of range expansion constants used to add some
#'   padding around the data to ensure that they are placed some distance
#'   away from the axes. Use the convenience function [expansion()]
#'   to generate the values for the `expand` argument. The defaults are to
#'   expand the scale by 5\% on each side for continuous variables, and by
#'   0.6 units on each side for discrete variables.
#' @param position For position scales, The position of the axis.
#' `left` or `right` for y axes, `top` or `bottom` for x axes.
#' @param super The super class to use for the constructed scale
#' @keywords internal
continuous_scale <- function(aesthetics, scale_name, palette, name = waiver(),
                             breaks = waiver(), minor_breaks = waiver(), labels = waiver(), limits = NULL,
                             rescaler = rescale, oob = censor, expand = waiver(), na.value = NA_real_,
                             trans = "identity", guide = "legend", position = "left", super = ScaleContinuous) {

  aesthetics <- standardise_aes_names(aesthetics)

  check_breaks_labels(breaks, labels)

  position <- match.arg(position, c("left", "right", "top", "bottom"))

  # If the scale is non-positional, break = NULL means removing the guide
  if (is.null(breaks) && all(!is_position_aes(aesthetics))) {
    guide <- "none"
  }

  trans <- as.trans(trans)
  if (!is.null(limits) && !is.function(limits)) {
    limits <- trans$transform(limits)
  }

  ggproto(NULL, super,
    call = match.call(),

    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,

    range = continuous_range(),
    limits = limits,
    trans = trans,
    na.value = na.value,
    expand = expand,
    rescaler = rescaler,
    oob = oob,

    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,

    labels = labels,
    guide = guide,
    position = position
  )
}

#' Discrete scale constructor
#'
#' @export
#' @inheritParams continuous_scale
#' @param palette A palette function that when called with a single integer
#'   argument (the number of levels in the scale) returns the values that
#'   they should take (e.g., [scales::hue_pal()]).
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks (the scale limits)
#'   - A character vector of breaks
#'   - A function that takes the limits as input and returns breaks
#'     as output
#' @param limits A character vector that defines possible values of the scale
#'   and their order.
#' @param drop Should unused factor levels be omitted from the scale?
#'    The default, `TRUE`, uses the levels that appear in the data;
#'    `FALSE` uses all the levels in the factor.
#' @param na.translate Unlike continuous scales, discrete scales can easily show
#'   missing values, and do so by default. If you want to remove missing values
#'   from a discrete scale, specify `na.translate = FALSE`.
#' @param na.value If `na.translate = TRUE`, what value aesthetic
#'   value should missing be displayed as? Does not apply to position scales
#'   where `NA` is always placed at the far right.
#' @keywords internal
discrete_scale <- function(aesthetics, scale_name, palette, name = waiver(),
                           breaks = waiver(), labels = waiver(), limits = NULL, expand = waiver(),
                           na.translate = TRUE, na.value = NA, drop = TRUE,
                           guide = "legend", position = "left", super = ScaleDiscrete) {

  aesthetics <- standardise_aes_names(aesthetics)

  check_breaks_labels(breaks, labels)

  position <- match.arg(position, c("left", "right", "top", "bottom"))

  # If the scale is non-positional, break = NULL means removing the guide
  if (is.null(breaks) && all(!is_position_aes(aesthetics))) {
    guide <- "none"
  }

  ggproto(NULL, super,
    call = match.call(),

    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,

    range = discrete_range(),
    limits = limits,
    na.value = na.value,
    na.translate = na.translate,
    expand = expand,

    name = name,
    breaks = breaks,
    labels = labels,
    drop = drop,
    guide = guide,
    position = position
  )
}

#' @section Scales:
#'
#' All `scale_*` functions like [scale_x_continuous()] return a `Scale*`
#' object like `ScaleContinuous`. Each of the `Scale*` objects is a [ggproto()]
#' object, descended from the top-level `Scale`.
#'
#' Properties not documented in [continuous_scale()] or [discrete_scale()]:
#'
#' - `call` The call to [continuous_scale()] or [discrete_scale()] that constructed
#'   the scale.
#'
#' - `range` One of `continuous_range()` or `discrete_range()`.
#'
#'
#' Methods:
#'
#' - `is_discrete()` Returns `TRUE` if the scale is a discrete scale
#'
#' - `is_empty()` Returns `TRUE` if the scale contains no information (i.e.,
#'   it has no information with which to calculate its `limits`).
#'
#' - `clone()` Returns a copy of the scale that can be trained
#'   independently without affecting the original scale.
#'
#' - `transform()` Transforms a vector of values using `self$trans`.
#'   This occurs before the `Stat` is calculated.
#'
#' - `train()` Update the `self$range` of observed (transformed) data values with
#'   a vector of (possibly) new values.
#'
#' - `reset()` Reset the `self$range` of observed data values. For discrete
#'   position scales, only the continuous range is reset.
#'
#' - `map()` Map transformed data values to some output value as
#'   determined by `self$rescale()` and `self$pallete` (except for position scales,
#'   which do not use the default implementation of this method). The output corresponds
#'   to the transformed data value in aesthetic space (e.g., a color, line width, or size).
#'
#' - `rescale()` Rescale transformed data to the the range 0, 1. This is most useful for
#'   position scales. For continuous scales, `rescale()` uses the `rescaler` that
#'   was provided to the constructor. `rescale()` does not apply `self$oob()` to
#'   its input, which means that discrete values outside `limits` will be `NA`, and
#'   values that are outside `range` will have values less than 0 or greater than 1.
#'   This allows guides more control over how out-of-bounds values are displayed.
#'
#' - `transform_df()`, `train_df()`, `map_df()` These `_df` variants
#'   accept a data frame, and apply the `transform`, `train`, and `map` methods
#'   (respectively) to the columns whose names are in `self$aesthetics`.
#'
#' - `get_limits()` Calculates the final scale limits in transformed data space
#'   based on the combination of `self$limits` and/or the range of observed values
#'   (`self$range`).
#'
#' - `get_breaks()` Calculates the final scale breaks in transformed data space
#'   based on on the combination of `self$breaks`, `self$trans$breaks()` (for
#'   continuous scales), and `limits`. Breaks outside of `limits` are assigned
#'   a value of `NA` (continuous scales) or dropped (discrete scales).
#'
#' - `get_labels()` Calculates labels for a given set of (transformed) `breaks`
#'   based on the combination of `self$labels` and `breaks`.
#'
#' - `get_breaks_minor()` For continuous scales, calculates the final scale minor breaks
#'   in transformed data space based on the rescaled `breaks`, the value of `self$minor_breaks`,
#'   and the value of `self$trans$minor_breaks()`. Discrete scales always return `NULL`.
#'
#' - `make_title()` Hook to modify the title that is calculated during guide construction
#'   (for non-position scales) or when the `Layout` calculates the x and y labels
#'   (position scales).
#'
#' These methods are only valid for position (x and y) scales:
#'
#' - `dimension()` For continuous scales, the dimension is the same concept as the limits.
#'   For discrete scales, `dimension()` returns a continuous range, where the limits
#'   would be placed at integer positions. `dimension()` optionally expands
#'   this range given an expantion of length 4 (see [expansion()]).
#'
#' - `break_info()` Returns a `list()` with calculated values needed for the `Coord`
#'   to transform values in transformed data space. Axis and grid guides also use
#'   these values to draw guides. This is called with
#'   a (usually expanded) continuous range, such as that returned by `self$dimension()`
#'   (even for discrete scales). The list has components `major_source`
#'   (`self$get_breaks()` for continuous scales, or `seq_along(self$get_breaks())`
#'   for discrete scales), `major` (the rescaled value of `major_source`, ignoring
#'   `self$rescaler`), `minor` (the rescaled value of `minor_source`, ignoring
#'   `self$rescaler`), `range` (the range that was passed in to `break_info()`),
#'   `labels` (the label values, one for each element in `breaks`).
#'
#' - `axis_order()` One of `c("primary", "secondary")` or `c("secondary", "primary")`
#'
#' - `make_sec_title()` Hook to modify the title for the second axis that is calculated
#'   when the `Layout` calculates the x and y labels.
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Scale <- ggproto("Scale", NULL,

  call = NULL,
  aesthetics = aes(),
  scale_name = NULL,
  palette = function() {
    stop("Not implemented", call. = FALSE)
  },

  range = ggproto(NULL, Range),
  limits = NULL,
  na.value = NA,
  expand = waiver(),

  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  guide = "legend",
  position = "left",


  is_discrete = function() {
    stop("Not implemented", call. = FALSE)
  },

  train_df = function(self, df) {
    if (empty(df)) return()

    aesthetics <- intersect(self$aesthetics, names(df))
    for (aesthetic in aesthetics) {
      self$train(df[[aesthetic]])
    }
    invisible()
  },

  train = function(self, x) {
    stop("Not implemented", call. = FALSE)
  },

  reset = function(self) {
    self$range$reset()
  },

  is_empty = function(self) {
    is.null(self$range$range) && is.null(self$limits)
  },

  transform_df = function(self, df) {
    if (empty(df)) {
      return()
    }

    aesthetics <- intersect(self$aesthetics, names(df))
    if (length(aesthetics) == 0) {
      return()
    }

    lapply(df[aesthetics], self$transform)
  },

  transform = function(self, x) {
    stop("Not implemented", call. = FALSE)
  },

  map_df = function(self, df, i = NULL) {
    if (empty(df)) {
      return()
    }

    aesthetics <- intersect(self$aesthetics, names(df))
    names(aesthetics) <- aesthetics
    if (length(aesthetics) == 0) {
      return()
    }

    if (is.null(i)) {
      lapply(aesthetics, function(j) self$map(df[[j]]))
    } else {
      lapply(aesthetics, function(j) self$map(df[[j]][i]))
    }
  },

  map = function(self, x, limits = self$get_limits()) {
    stop("Not implemented", call. = FALSE)
  },

  rescale = function(self, x, limits = self$get_limits(), range = self$dimension()) {
    stop("Not implemented", call. = FALSE)
  },

  get_limits = function(self) {
    if (self$is_empty()) {
      return(c(0, 1))
    }

    if (is.null(self$limits)) {
      self$range$range
    } else if (is.function(self$limits)) {
      # if limits is a function, it expects to work in data space
      self$trans$transform(self$limits(self$trans$inverse(self$range$range)))
    } else {
      ifelse(is.na(self$limits), self$range$range, self$limits)
    }
  },

  dimension = function(self, expand = expansion(0, 0), limits = self$get_limits()) {
    stop("Not implemented", call. = FALSE)
  },

  get_breaks = function(self, limits = self$get_limits()) {
    stop("Not implemented", call. = FALSE)
  },

  break_positions = function(self, range = self$get_limits()) {
    self$map(self$get_breaks(range))
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(), limits = self$get_limits()) {
    stop("Not implemented", call. = FALSE)
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    stop("Not implemented", call. = FALSE)
  },

  clone = function(self) {
    stop("Not implemented", call. = FALSE)
  },

  break_info = function(self, range = NULL) {
    stop("Not implemented", call. = FALSE)
  },

  axis_order = function(self) {
    ord <- c("primary", "secondary")
    if (self$position %in% c("right", "bottom")) {
      ord <- rev(ord)
    }
    ord
  },

  make_title = function(title) {
    title
  },

  make_sec_title = function(title) {
    title
  }
)

check_breaks_labels <- function(breaks, labels) {
  if (is.null(breaks)) {
    return(TRUE)
  }
  if (is.null(labels)) {
    return(TRUE)
  }

  bad_labels <- is.atomic(breaks) && is.atomic(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    stop("`breaks` and `labels` must have the same length", call. = FALSE)
  }

  TRUE
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuous <- ggproto("ScaleContinuous", Scale,
  range = continuous_range(),
  na.value = NA_real_,
  rescaler = rescale,
  oob = censor,
  minor_breaks = waiver(),

  is_discrete = function() FALSE,

  train = function(self, x) {
    if (length(x) == 0) {
      return()
    }
    self$range$train(x)
  },

  transform = function(self, x) {
     new_x <- self$trans$transform(x)
     if (any(is.finite(x) != is.finite(new_x))) {
       type <- if (self$scale_name == "position_c") "continuous" else "discrete"
       axis <- if ("x" %in% self$aesthetics) "x" else "y"
       warning("Transformation introduced infinite values in ", type, " ", axis, "-axis", call. = FALSE)
     }
     new_x
  },

  map = function(self, x, limits = self$get_limits()) {
    x <- self$rescale(self$oob(x, range = limits), limits)

    uniq <- unique(x)
    pal <- self$palette(uniq)
    scaled <- pal[match(x, uniq)]

    ifelse(!is.na(scaled), scaled, self$na.value)
  },

  rescale = function(self, x, limits = self$get_limits(), range = limits) {
    self$rescaler(x, from = range)
  },

  dimension = function(self, expand = expansion(0, 0), limits = self$get_limits()) {
    expand_limits_scale(self, expand, limits)
  },

  get_breaks = function(self, limits = self$get_limits()) {
    if (self$is_empty()) {
      return(numeric())
    }

    # Limits in transformed space need to be converted back to data space
    limits <- self$trans$inverse(limits)

    if (is.null(self$breaks)) {
      return(NULL)
    }

    if (identical(self$breaks, NA)) {
      stop("Invalid breaks specification. Use NULL, not NA", call. = FALSE)
    }

    if (zero_range(as.numeric(limits))) {
      breaks <- limits[1]
    } else if (is.waive(self$breaks)) {
      breaks <- self$trans$breaks(limits)
    } else if (is.function(self$breaks)) {
      breaks <- self$breaks(limits)
    } else {
      breaks <- self$breaks
    }

    # Breaks in data space need to be converted back to transformed space
    breaks <- self$trans$transform(breaks)
    # Any breaks outside the dimensions are flagged as missing
    breaks <- censor(breaks, self$trans$transform(limits), only.finite = FALSE)

    breaks
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(), limits = self$get_limits()) {
    if (zero_range(as.numeric(limits))) {
      return()
    }

    if (is.null(self$minor_breaks)) {
      return(NULL)
    }

    if (identical(self$minor_breaks, NA)) {
      stop("Invalid minor_breaks specification. Use NULL, not NA", call. = FALSE)
    }

    if (is.waive(self$minor_breaks)) {
      if (is.null(b)) {
        breaks <- NULL
      } else {
        breaks <- self$trans$minor_breaks(b, limits, n)
      }
    } else if (is.function(self$minor_breaks)) {
      # Find breaks in data space, and convert to numeric
      breaks <- self$minor_breaks(self$trans$inverse(limits))
      breaks <- self$trans$transform(breaks)
    } else {
      breaks <- self$trans$transform(self$minor_breaks)
    }

    # Any minor breaks outside the dimensions need to be thrown away
    discard(breaks, limits)
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    if (is.null(breaks)) {
      return(NULL)
    }

    breaks <- self$trans$inverse(breaks)

    if (is.null(self$labels)) {
      return(NULL)
    }

    if (identical(self$labels, NA)) {
      stop("Invalid labels specification. Use NULL, not NA", call. = FALSE)
    }

    if (is.waive(self$labels)) {
      labels <- self$trans$format(breaks)
    } else if (is.function(self$labels)) {
      labels <- self$labels(breaks)
    } else {
      labels <- self$labels
    }

    if (length(labels) != length(breaks)) {
      stop("Breaks and labels are different lengths", call. = FALSE)
    }

    labels
  },

  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- continuous_range()
    new
  },

  break_info = function(self, range = NULL) {
    # range
    if (is.null(range)) range <- self$dimension()

    # major breaks
    major <- self$get_breaks(range)

    # labels
    labels <- self$get_labels(major)

    # drop oob breaks/labels by testing major == NA
    if (!is.null(labels)) labels <- labels[!is.na(major)]
    if (!is.null(major)) major <- major[!is.na(major)]

    # minor breaks
    minor <- self$get_breaks_minor(b = major, limits = range)
    if (!is.null(minor)) minor <- minor[!is.na(minor)]

    # rescale breaks [0, 1], which are used by coord/guide
    major_n <- rescale(major, from = range)
    minor_n <- rescale(minor, from = range)

    list(
      range = range,
      labels = labels,
      major = major_n,
      minor = minor_n,
      major_source = major,
      minor_source = minor
    )
  },

  print = function(self, ...) {
    show_range <- function(x) paste0(formatC(x, digits = 3), collapse = " -- ")

    cat("<", class(self)[[1]], ">\n", sep = "")
    cat(" Range:  ", show_range(self$range$range), "\n", sep = "")
    cat(" Limits: ", show_range(self$dimension()), "\n", sep = "")
  }
)


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleDiscrete <- ggproto("ScaleDiscrete", Scale,
  drop = TRUE,
  na.value = NA,
  n.breaks.cache = NULL,
  palette.cache = NULL,

  is_discrete = function() TRUE,

  train = function(self, x) {
    if (length(x) == 0) {
      return()
    }
    self$range$train(x, drop = self$drop, na.rm = !self$na.translate)
  },

  transform = function(x) {
    x
  },

  map = function(self, x, limits = self$get_limits()) {
    n <- sum(!is.na(limits))
    if (!is.null(self$n.breaks.cache) && self$n.breaks.cache == n) {
      pal <- self$palette.cache
    } else {
      if (!is.null(self$n.breaks.cache)) {
        warning("Cached palette does not match requested", call. = FALSE)
      }
      pal <- self$palette(n)
      self$palette.cache <- pal
      self$n.breaks.cache <- n
    }

    if (is.null(names(pal))) {
      pal_match <- pal[match(as.character(x), limits)]
    } else {
      pal_match <- pal[match(as.character(x), names(pal))]
      pal_match <- unname(pal_match)
    }

    if (self$na.translate) {
      ifelse(is.na(x) | is.na(pal_match), self$na.value, pal_match)
    } else {
      pal_match
    }
  },

  rescale = function(self, x, limits = self$get_limits(), range = c(1, length(limits))) {
    rescale(x, match(as.character(x), limits), from = range)
  },

  dimension = function(self, expand = expansion(0, 0), limits = self$get_limits()) {
    expand_limits_discrete(limits, expand = expand)
  },

  get_breaks = function(self, limits = self$get_limits()) {
    if (self$is_empty()) {
      return(numeric())
    }

    if (is.null(self$breaks)) {
      return(NULL)
    }

    if (identical(self$breaks, NA)) {
      stop("Invalid breaks specification. Use NULL, not NA", call. = FALSE)
    }

    if (is.waive(self$breaks)) {
      breaks <- limits
    } else if (is.function(self$breaks)) {
      breaks <- self$breaks(limits)
    } else {
      breaks <- self$breaks
    }

    # Breaks only occur only on values in domain
    in_domain <- intersect(breaks, limits)
    structure(in_domain, pos = match(in_domain, breaks))
  },

  get_breaks_minor = function(...) NULL,

  get_labels = function(self, breaks = self$get_breaks()) {
    if (self$is_empty()) {
      return(character())
    }

    if (is.null(breaks)) {
      return(NULL)
    }

    if (is.null(self$labels)) {
      return(NULL)
    }

    if (identical(self$labels, NA)) {
      stop("Invalid labels specification. Use NULL, not NA", call. = FALSE)
    }

    if (is.waive(self$labels)) {
      breaks <- self$get_breaks()
      if (is.numeric(breaks)) {
        # Only format numbers, because on Windows, format messes up encoding
        format(breaks, justify = "none")
      } else {
        as.character(breaks)
      }
    } else if (is.function(self$labels)) {
      self$labels(breaks)
    } else {
      if (!is.null(names(self$labels))) {
        # If labels have names, use them to match with breaks
        labels <- breaks

        map <- match(names(self$labels), labels, nomatch = 0)
        labels[map] <- self$labels[map != 0]
        labels
      } else {
        labels <- self$labels

        # Need to ensure that if breaks were dropped, corresponding labels are too
        pos <- attr(breaks, "pos")
        if (!is.null(pos)) {
          labels <- labels[pos]
        }
        labels
      }
    }
  },

  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- discrete_range()
    new
  },

  break_info = function(self, range = NULL) {
    # for discrete, limits != range
    limits <- self$get_limits()

    major <- self$get_breaks(limits)
    if (is.null(major)) {
      labels <- major_n <- NULL
    } else {

      labels <- self$get_labels(major)

      major <- self$map(major)
      major <- major[!is.na(major)]

      # rescale breaks [0, 1], which are used by coord/guide
      major_n <- rescale(major, from = range)
    }

    list(
      range = range,
      labels = labels,
      major = major_n,
      minor = NULL,
      major_source = major,
      minor_source = NULL
    )
  }
)

# In place modification of a scale to change the primary axis
scale_flip_position <- function(scale) {
  scale$position <- switch(scale$position,
    top = "bottom",
    bottom = "top",
    left = "right",
    right = "left",
    scale$position
  )
  invisible()
}
