#' Components of a scale:
#'
#' Guide related:
#'
#' \itemize{
#'   \item name
#'   \item breaks
#'   \item labels
#'   \item expand
#' }
#'
#' Mapping related:
#' \itemize{
#'   \item aesthetic
#'   \item limits
#'   \item palette
#'   \item trans
#' }
#'
#' Scales are an S3 class with a single mutable component implemented with
#' a reference class - the range of the data.  This mutability makes working
#' with scales much easier, because it makes it possible to distribute the
#' training, without having to worry about collecting all the pieces back
#' together again.
#'
#' @name ggscale
#' @keywords internal
NULL

#' Continuous scale constructor.
#'
#' @export
#' @inheritParams discrete_scale
#' @param name The name of the scale. Used as axis or legend title. If
#'   \code{NULL}, the default, the name of the scale is taken from the first
#'   mapping used for that aesthetic.
#' @param breaks One of: \itemize{
#'   \item \code{NULL} for no breaks
#'   \item \code{waiver()} for the default breaks computed by the
#'     transformation object
#'   \item A numeric vector of positions
#'   \item A function that takes the limits as input and returns breaks
#'     as output
#' }
#' @param minor_breaks One of: \itemize{
#'   \item \code{NULL} for no minor breaks
#'   \item \code{waiver()} for the default breaks (one minor break between
#'     each major break)
#'   \item A numeric vector of positions
#'   \item A function that given the limits returns a vector of minor breaks.
#' }
#' @param labels One of: \itemize{
#'   \item \code{NULL} for no labels
#'   \item \code{waiver()} for the default labels computed by the
#'     transformation object
#'   \item A character vector giving labels (must be same length as \code{breaks})
#'   \item A function that takes the breaks as input and returns labels
#'     as output
#' }
#' @param limits A numeric vector of length two providing limits of the scale.
#'   Use \code{NA} to refer to the existing minimum or maximum.
#' @param rescaler  Used by diverging and n colour gradients
#'   (i.e. \code{\link{scale_colour_gradient2}}, \code{\link{scale_colour_gradientn}}).
#'   A function used to scale the input values to the range [0, 1].
#' @param oob Function that handles limits outside of the scale limits
#'   (out of bounds). The default replaces out of bounds values with NA.
#' @param na.value Missing values will be replaced with this value.
#' @param trans Either the name of a transformation object, or the
#'   object itself. Built-in transformations include "asn", "atanh",
#'   "boxcox", "exp", "identity", "log", "log10", "log1p", "log2",
#'   "logit", "probability", "probit", "reciprocal", "reverse" and "sqrt".
#'
#'   A transformation object bundles together a transform, it's inverse,
#'   and methods for generating breaks and labels. Transformation objects
#'   are defined in the scales package, and are called \code{name_trans}, e.g.
#'   \code{\link[scales]{boxcox_trans}}. You can create your own
#'   transformation with \code{\link[scales]{trans_new}}.
#' @param expand A numeric vector of length two giving multiplicative and
#'   additive expansion constants. These constants ensure that the data is
#'   placed some distance away from the axes. The defaults are
#'   \code{c(0.05, 0)} for continuous variables, and \code{c(0, 0.6)} for
#'   discrete variables.
#' @param guide Name of guide object, or object itself.
#' @keywords internal
continuous_scale <- function(aesthetics, scale_name, palette, name = waiver(), breaks = waiver(), minor_breaks = waiver(), labels = waiver(), limits = NULL, rescaler = rescale, oob = censor, expand = waiver(), na.value = NA_real_, trans = "identity", guide="legend") {

  bad_labels <- is.atomic(breaks) && is.atomic(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    stop("`breaks` and `labels` must have the same length", call. = FALSE)
  }

  if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
    guide <- "none"
  }

  trans <- as.trans(trans)
  if (!is.null(limits)) {
    limits <- trans$transform(limits)
  }

  structure(list(
    call = match.call(),

    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,

    range = ContinuousRange$new(),
    limits = limits,
    trans = trans,
    na.value = na.value,
    expand = expand,
    rescaler = rescaler,  # Used by diverging and n colour gradients
    oob = oob,

    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,

    labels = labels,
    legend = legend,
    guide = guide
  ), class = c(scale_name, "continuous", "scale"))
}

#' Discrete scale constructor.
#'
#' @export
#' @param aesthetics the names of the aesthetics that this scale works with
#' @param scale_name the name of the scale
#' @param palette a palette function that when called with a single integer
#'   argument (the number of levels in the scale) returns the values that
#'   they should take
#' @param name the name of the scale - used as the axis label or the legend
#'  title
#' @param drop drop unused factor levels from the scale (\code{TRUE} or
#'   \code{FALSE})
#' @param breaks control the breaks in the guide.  There are four possible
#'   types of input:
#'   \itemize{
#'     \item \code{NULL}: don't display any breaks
#'     \item a character vector giving the breaks as they should appear on the
#'      axis or in the legend.
#'     \item \code{waiver()} to use the default break computation.
#'     \item a function, that when called with a single argument, a character
#'       vector giving the limits of the scale, returns a character vector
#'       specifying which breaks to display.
#'   }
#'   This parameter does not affect in any way how the data is scaled - it
#'   only affects the appearance of the legend.
#' @param limits A character vector specifying the data range for the scale.
#   The limits control what levels are displayed in the plot, their order,
#'  and the default order of their display in guides.
#' @param labels \code{NULL} for no labels, \code{waiver()} for default
#'   labels (labels the same as breaks), a character vector the same length
#'   as breaks, or a named character vector whose names are used to match
#'   replacement the labels for matching breaks.
#' @param expand a numeric vector of length two, giving a multiplicative and
#'   additive constant used to expand the range of the scales so that there
#'   is a small gap between the data and the axes. The defaults are (0,0.6)
#'   for discrete scales and (0.05,0) for continuous scales.
#' @param na.value how should missing values be displayed?
#' @param guide the name of, or actual function, used to create the
#'   guide. See \code{\link{guides}} for more info.
#' @keywords internal
discrete_scale <- function(aesthetics, scale_name, palette, name = waiver(), breaks = waiver(), labels = waiver(), limits = NULL, expand = waiver(), na.value = NA, drop = TRUE, guide="legend") {

  bad_labels <- is.vector(breaks) && is.vector(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    stop("`breaks` and `labels` must have the same length", call. = FALSE)
  }

  if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
    guide <- "none"
  }

  structure(list(
    call = match.call(),

    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,

    range = DiscreteRange$new(),
    limits = limits,
    na.value = na.value,
    expand = expand,

    name = name,
    breaks = breaks,
    labels = labels,
    legend = legend,
    drop = drop,
    guide = guide
  ), class = c(scale_name, "discrete", "scale"))
}

# Train scale from a data frame.
#
# @return updated range (invisibly)
# @seealso \code{\link{scale_train}} for scale specific generic method
scale_train_df <- function(scale, df) {
  if (empty(df)) return()

  aesthetics <- intersect(scale$aesthetics, names(df))
  for(aesthetic in aesthetics) {
    scale_train(scale, df[[aesthetic]])
  }
  invisible()
}

# Train an individual scale from a vector of data.
#
scale_train <- function(scale, x) {
  if (length(x) == 0) return()
  UseMethod("scale_train")
}

#' @export
scale_train.continuous <- function(scale, x) {
  scale$range$train(x)
}
#' @export
scale_train.discrete <- function(scale, x) {
  scale$range$train(x, drop = scale$drop)
}

# Reset scale, untraining ranges
scale_reset <- function(scale, x) UseMethod("scale_reset")
#' @export
scale_reset.default <- function(scale, x) {
  scale$range$reset()
}

scale_is_empty <- function(scale) UseMethod("scale_is_empty")

#' @export
scale_is_empty.default <- function(scale) {
  is.null(scale$range$range) && is.null(scale$limits)
}

# @return list of transformed variables
scale_transform_df <- function(scale, df) {
  if (empty(df)) return()

  aesthetics <- intersect(scale$aesthetics, names(df))
  if (length(aesthetics) == 0) return()

  lapply(df[aesthetics], scale_transform, scale = scale)
}

scale_transform <- function(scale, x) UseMethod("scale_transform")

#' @export
scale_transform.continuous <- function(scale, x) {
  scale$trans$transform(x)
}
#' @export
scale_transform.discrete <- function(scale, x) {
  x
}

# @return list of mapped variables
scale_map_df <- function(scale, df, i = NULL) {
  if (empty(df)) return()

  aesthetics <- intersect(scale$aesthetics, names(df))
  names(aesthetics) <- aesthetics
  if (length(aesthetics) == 0) return()

  if (is.null(i)) {
    lapply(aesthetics, function(j) scale_map(scale, df[[j]]))
  } else {
    lapply(aesthetics, function(j) scale_map(scale, df[[j]][i]))
  }
}

# @kohske
# scale_map tentatively accept limits argument.
# scale_map replaces oob (i.e., outside limits) values with NA.
#
# Previously limits are always scale_limits(scale).
# But if this function is called to get breaks,
# and breaks spans oob, the oob breaks is replaces by NA.
# This makes impossible to display oob breaks.
# Now coord_train calls this function with limits determined by coord (with expansion).
scale_map <- function(scale, x, limits) UseMethod("scale_map")

#' @export
scale_map.continuous <- function(scale, x, limits = scale_limits(scale)) {
  x <- scale$oob(scale$rescaler(x, from = limits))

  uniq <- unique(x)
  pal <- scale$palette(uniq)
  scaled <- pal[match(x, uniq)]

  ifelse(!is.na(scaled), scaled, scale$na.value)
}

#' @export
scale_map.discrete <- function(scale, x, limits = scale_limits(scale)) {
  n <- sum(!is.na(limits))
  pal <- scale$palette(n)

  if (is.null(names(pal))) {
    pal_match <- pal[match(as.character(x), limits)]
  } else {
    pal_match <- pal[match(as.character(x), names(pal))]
    pal_match <- unname(pal_match)
  }

  ifelse(is.na(x) | is.na(pal_match), scale$na.value, pal_match)
}

scale_limits <- function(scale) {
  if (scale_is_empty(scale)) return(c(0, 1))

  UseMethod("scale_limits")
}


#  if scale contains a NULL, use the default scale range
#  if scale contains a NA, use the default range for that axis, otherwise
#  use the user defined limit for that axis
#' @export
scale_limits.default <- function(scale) {
  if(!is.null(scale$limits)) {
    ifelse(!is.na(scale$limits), scale$limits, scale$range$range)
  } else {
    scale$range$range
  }
}

scale_expand <- function(scale) UseMethod("scale_expand")
#' @export
scale_expand.default <- function(scale) {
  scale$expand %|W|% c(0, 0)
}

# The phyical size of the scale, if a position scale
# Unlike limits, this always returns a numeric vector of length 2
# @kohske
# scale_dimension uses scale_expand(scale) for expansion by default.
scale_dimension <- function(scale, expand = scale_expand(scale)) UseMethod("scale_dimension")

#' @export
scale_dimension.continuous  <- function(scale, expand = scale_expand(scale)) {
  expand_range(scale_limits(scale), expand[1], expand[2])
}
#' @export
scale_dimension.discrete <- function(scale, expand = scale_expand(scale)) {
  expand_range(length(scale_limits(scale)), expand[1], expand[2])
}

scale_breaks <- function(scale, limits = scale_limits(scale)) {
  if (scale_is_empty(scale)) return(numeric())

  UseMethod("scale_breaks")
}

#' @export
scale_breaks.continuous <- function(scale, limits = scale_limits(scale)) {
  # Limits in transformed space need to be converted back to data space
  limits <- scale$trans$inverse(limits)

  if (is.null(scale$breaks)) {
    return(NULL)
  } else if (identical(scale$breaks, NA)) {
    stop("Invalid breaks specification. Use NULL, not NA")
  } else if (zero_range(as.numeric(limits))) {
    breaks <- limits[1]
  } else if (is.waive(scale$breaks)) {
    breaks <- scale$trans$breaks(limits)
  } else if (is.function(scale$breaks)) {
    breaks <- scale$breaks(limits)
  } else {
    breaks <- scale$breaks
  }

  # Breaks in data space need to be converted back to transformed space
  # And any breaks outside the dimensions need to be flagged as missing
  #
  # @kohske
  # TODO: replace NA with something else for flag.
  #       guides cannot discriminate oob from missing value.
  breaks <- censor(scale$trans$transform(breaks), scale$trans$transform(limits))
  if (length(breaks) == 0) {
    stop("Zero breaks in scale for ", paste(scale$aesthetics, collapse = "/"),
      call. = FALSE)
  }
  breaks
}

#' @export
scale_breaks.discrete <- function(scale, limits = scale_limits(scale)) {
  if (is.null(scale$breaks)) {
    return(NULL)
  } else if (identical(scale$breaks, NA)) {
    stop("Invalid breaks specification. Use NULL, not NA", call. = FALSE)
  } else if (is.waive(scale$breaks)) {
    breaks <- limits
  } else if (is.function(scale$breaks)) {
    breaks <- scale$breaks(limits)
  } else {
    breaks <- scale$breaks
  }

  # Breaks can only occur only on values in domain
  in_domain <- intersect(breaks, scale_limits(scale))
  structure(in_domain, pos = match(in_domain, breaks))
}

# The numeric position of scale breaks, used by coord/guide
scale_break_positions <- function(scale, range = scale_limits(scale)) {
  scale_map(scale, scale_breaks(scale, range))
}

scale_breaks_minor<- function(scale, n = 2, b = scale_break_positions(scale), limits = scale_limits(scale)) {
  UseMethod("scale_breaks_minor")
}

#' @export
scale_breaks_minor.continuous <- function(scale, n = 2, b = scale_break_positions(scale), limits = scale_limits(scale)) {
  if (zero_range(as.numeric(limits))) {
    return()
  }

  if (is.null(scale$minor_breaks)) {
    return(NULL)
  } else if (identical(scale$minor_breaks, NA)) {
    stop("Invalid minor_breaks specification. Use NULL, not NA", call. = FALSE)
  } else if (is.waive(scale$minor_breaks)) {
    if (is.null(b)) {
      breaks <- NULL
    } else {
      b <- b[!is.na(b)]
      if (length(b) < 2) return()

      bd <- diff(b)[1]
      if (min(limits) < min(b)) b <- c(b[1] - bd, b)
      if (max(limits) > max(b)) b <- c(b, b[length(b)] + bd)
      breaks <- unique(unlist(mapply(seq, b[-length(b)], b[-1], length.out = n+1,
        SIMPLIFY = FALSE)))
    }
  } else if (is.function(scale$minor_breaks)) {
    # Find breaks in data space, and convert to numeric
    breaks <- scale$minor_breaks(scale$trans$inverse(limits))
    breaks <- scale$trans$transform(breaks)
  } else {
    breaks <- scale$minor_breaks
  }

  # Any minor breaks outside the dimensions need to be thrown away
  discard(breaks, limits)
}

#' @export
scale_breaks_minor.discrete <- function(...) NULL

scale_breaks_minor_positions <- function(scale) {
  scale_map(scale, scale_breaks_minor(scale))
}

scale_labels <- function(scale, breaks = scale_breaks(scale)) {
  if (scale_is_empty(scale)) return(character())

  UseMethod("scale_labels")
}

#' @export
scale_labels.continuous <- function(scale, breaks = scale_breaks(scale)) {
  if (is.null(breaks)) return(NULL)

  breaks <- scale$trans$inverse(breaks)

  if (is.null(scale$labels)) {
    return(NULL)
  } else if (identical(scale$labels, NA)) {
    stop("Invalid labels specification. Use NULL, not NA", call. = FALSE)
  } else if (is.waive(scale$labels)) {
    labels <- scale$trans$format(breaks)
  } else if (is.function(scale$labels)) {
    labels <- scale$labels(breaks)
  } else {
    labels <- scale$labels
  }
  if (length(labels) != length(breaks)) {
    stop("Breaks and labels are different lengths")
  }
  labels
}

#' @export
scale_labels.discrete <- function(scale, breaks = scale_breaks(scale)) {
  if (is.null(breaks)) return(NULL)

  if (is.null(scale$labels)) {
    return(NULL)
  } else if (identical(scale$labels, NA)) {
    stop("Invalid labels specification. Use NULL, not NA", call. = FALSE)
  }else if (is.waive(scale$labels)) {
    format(scale_breaks(scale), justify = "none", trim = TRUE)
  } else if (is.function(scale$labels)) {
    scale$labels(breaks)
  } else {
    if (!is.null(names(scale$labels))) {
      # If labels have names, use them to match with breaks
      labels <- breaks

      map <- match(names(scale$labels), labels, nomatch = 0)
      labels[map] <- scale$labels[map != 0]
      labels
    } else {
      labels <- scale$labels

      # Need to ensure that if breaks were dropped, corresponding labels are too
      pos <- attr(breaks, "pos")
      if (!is.null(pos)) {
        labels <- labels[pos]
      }
      labels
    }

  }
}

named_labels <- function(breaks, labels) {
  breaks[match(names(labels), breaks, nomatch = 0)] <- labels
  breaks
}

#' @export
print.scale <- function(x, ...) {
  print(x$call)
}

scale_clone <- function(scale) UseMethod("scale_clone")

#' @export
scale_clone.continuous <- function(scale) {
  new <- scale
  new$range <- ContinuousRange$new()
  new
}

#' @export
scale_clone.discrete <- function(scale) {
  new <- scale
  new$range <- DiscreteRange$new()
  new
}


scale_break_info <- function(scale, range = NULL)  UseMethod("scale_break_info")
#' @export
scale_break_info.discrete <- function(scale, range = NULL) {

  # for discrete, limits != range
  limits <- scale_limits(scale)

  major <- scale_breaks(scale, limits)
  if (is.null(major)) {
    labels <- major_n <- NULL
  } else {

    labels <- scale_labels(scale, major)
    labels <- labels[!is.na(labels)]

    major <- scale_map(scale, major)
    major <- major[!is.na(major)]

    # rescale breaks [0, 1], which are used by coord/guide
    major_n <- rescale(major, from = range)
  }

  list(range = range, labels = labels,
       major = major_n, minor = NULL,
       major_source = major, minor_source = NULL)
}
#' @export
scale_break_info.continuous <- function(scale, range = NULL) {
  # range
  if (is.null(range)) range <- scale_dimension(scale)

  # major breaks
  major <- scale_breaks(scale, range)

  # labels
  labels <- scale_labels(scale, major)

  # drop oob breaks/labels by testing major == NA
  if (!is.null(labels)) labels <- labels[!is.na(major)]
  if (!is.null(major)) major <- major[!is.na(major)]

  # minor breaks
  minor <- scale_breaks_minor(scale, b = major, limits = range)
  if (!is.null(minor)) minor <- minor[!is.na(minor)]

  # rescale breaks [0, 1], which are used by coord/guide
  major_n <- rescale(major, from = range)
  minor_n <- rescale(minor, from = range)

  list(range = range, labels = labels,
       major = major_n, minor = minor_n,
       major_source = major, minor_source = minor)
}
