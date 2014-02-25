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
#' @param minor_breaks Used with date or datetime scales. Either \code{NULL} for
#'   no minor breaks, \code{waiver()} for the default breaks (one minor break
#'   between each major break), a numeric vector of positions, or a function
#'   that given the limits returns a vector of minor breaks.
#' @param limits A numeric vector of length two describing the scale limits.
#' @param rescaler  Used by diverging and n colour gradients
#'   (i.e. \code{\link{scale_colour_gradient2}}, \code{\link{scale_colour_gradientn}}).
#' @param oob What to do with values outside scale limits (out of bounds)?
#' @keywords internal
continuous_scale <- function(aesthetics, scale_name, palette, name = NULL, breaks = waiver(), minor_breaks = waiver(), labels = waiver(), legend = NULL, limits = NULL, rescaler = rescale, oob = censor, expand = waiver(), na.value = NA_real_, trans = "identity", guide="legend") {

  if (!is.null(legend)) {
    gg_dep("0.8.9", "\"legend\" argument in scale_XXX is deprecated. Use guide=\"none\" for suppress the guide display.")
    if (legend == FALSE) guide = "none"
    else if (legend == TRUE) guide = "legend"
  }

  bad_labels <- is.vector(breaks) && is.vector(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    stop("Breaks and labels have unequal lengths", call. = FALSE)
  }

  if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
    guide <- "none"
  }

  trans <- as.trans(trans)
  if (!is.null(limits)) {
    limits <- trans$trans(limits)
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
#' @param legend deprecated.  Use \code{guide} instead.
#' @param expand a numeric vector of length two, giving a multiplicative and
#'   additive constant used to expand the range of the scales so that there
#'   is a small gap between the data and the axes.
#' @param na.value how should missing values be displayed?
#' @param guide the name of, or actual function, used to create the
#'   guide.
#' @keywords internal
discrete_scale <- function(aesthetics, scale_name, palette, name = NULL, breaks = waiver(), labels = waiver(), legend = NULL, limits = NULL, expand = waiver(), na.value = NA, drop = TRUE, guide="legend") {

  if (!is.null(legend)) {
    gg_dep("0.8.9", "\"legend\" argument in scale_XXX is deprecated. Use guide=\"none\" for suppress the guide display.")
    if (legend == FALSE) guide = "none"
    else if (legend == TRUE) guide = "legend"
  }

  bad_labels <- is.vector(breaks) && is.vector(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    stop("Breaks and labels have unequal lengths", call. = FALSE)
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
  scale$trans$trans(x)
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

  # Points are rounded to the nearest 500th, to reduce the amount of
  # work that the scale palette must do - this is particularly important
  # for colour scales which are rather slow.  This shouldn't have any
  # perceptual impacts.
  x <- round_any(x, 1 / 500)
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

# @kohske
# this (internal) function always returns a vector of length 2 of giving
# multiplicative and additive expansion constants.
# if scale' expand is specified, return it.
# if is.waive, return c(0, 0)
scale_expand <- function(scale) UseMethod("scale_expand")
#' @export
scale_expand.default <- function(scale) {
  if (is.waive(scale$expand)) c(0, 0)
  else scale$expand
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
  limits <- scale$trans$inv(limits)

  if (is.null(scale$breaks)) {
    return(NULL)
  } else if (length(scale$breaks) == 1 && !is.function(scale$breaks) && is.na(scale$breaks)) {
    gg_dep("0.8.9", "breaks = NA is deprecated. Please use breaks = NULL to remove breaks in the scale.")
    return(NULL)
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
  breaks <- censor(scale$trans$trans(breaks), scale$trans$trans(limits))
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
  } else if (length(scale$breaks) == 1 && !is.function(scale$breaks) && is.na(scale$breaks)) {
    gg_dep("0.8.9", "breaks = NA is deprecated. Please use breaks = NULL to remove breaks in the scale.")
    return(NULL)
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
  } else if (length(scale$minor_breaks) == 1 && !is.function(scale$minor_breaks) && is.na(scale$minor_breaks)) {
    gg_dep("0.8.9", "minor_breaks = NA is deprecated. Please use minor_breaks = NULL to remove minor breaks in the scale.")
    return(NULL)
  } else if (is.waive(scale$minor_breaks)) {
    if (is.null(b)) {
      breaks <- NULL
    } else {
      b <- b[!is.na(b)]
      if (length(b) < 2) return()

      bd <- diff(b)[1]
      if (min(limits) < min(b)) b <- c(b[1] - bd, b)
      if (max(limits) > max(b)) b <- c(b, b[length(b)] + bd)
      breaks <- unique(unlist(mapply(seq, b[-length(b)], b[-1], length=n+1,
        SIMPLIFY = FALSE)))
    }
  } else if (is.function(scale$minor_breaks)) {
    # Find breaks in data space, and convert to numeric
    breaks <- scale$minor_breaks(scale$trans$inv(limits))
    breaks <- scale$trans$trans(breaks)
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

  breaks <- scale$trans$inv(breaks)

  if (is.null(scale$labels)) {
    return(NULL)
  } else if (length(scale$labels) == 1 && !is.function(scale$labels) && is.na(scale$labels)) {
    gg_dep("0.8.9", "labels = NA is deprecated. Please use labels = NULL to remove labels in the scale.")
    return(NULL)
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
  } else if (length(scale$labels) == 1 && !is.function(scale$labels) && is.na(scale$labels)) {
    gg_dep("0.8.9", "labels = NA is deprecated. Please use labels = NULL to remove labels in the scale.")
    return(NULL)
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
