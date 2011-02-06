#' Components of a scale:
#'
#' Guide related:
#'   * name 
#'   * breaks
#'   * labels
#'   * expand
#'
#' Mapping related:
#'   * aesthetic
#'   * limits
#'   * palette
#'   * trans
#'
#' Scales are an S3 class with a single mutable component implemented with
#' a reference class - the range of the data.  This mutability makes working
#' with scales much easier, because it makes it possible to distribute the
#' training, without having to worry about collecting all the pieces back 
#' together again.
#'
#' @name ggscale
#' @S3method print scale
NULL

#' @paramCopy breaks scales::cbreaks
#' @paramCopy labels scales::cbreaks
#' @paramCopy palette scales::cscale
#' @paramCopy na.value scales::cscale
#' @paramCopy trans scales::cscale
continuous_scale <- function(aesthetics, scale_name, palette, name = NULL, breaks = NULL, labels = NULL, legend = TRUE, limits = NULL, rescaler = rescale, oob = censor, expand = c(0, 0), na.value = NA, trans = "identity") {
  
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
    labels = labels, 
    legend = legend
  ), class = c(scale_name, "continuous", "scale"))
}


discrete_scale <- function(aesthetics, scale_name, palette, name = NULL, breaks = NULL, labels = NULL, legend = TRUE, limits = NULL, expand = c(0, 0), na.value = NA, drop = TRUE) {
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
    drop = drop
  ), class = c(scale_name, "discrete", "scale"))
}

# Train scale from a data frame.
#
# @return updated range (invisibly)
# @seealso \code{\link{scale_train}} for scale specific generic method
scale_train_df <- function(scale, df) {
  if (empty(df)) return() 

  aesthetics <- intersect(scale$aesthetics, names(df))
  lapply(df[aesthetics], scale_train, scale = scale)
}

# Train an individual scale from a vector of data.
scale_train <- function(scale, x) UseMethod("scale_train")
scale_train.continuous <- function(scale, x) {
  scale$range$train(x)
}
scale_train.discrete <- function(scale, x) {
  scale$range$train(x, drop = scale$drop)
}

# @return list of transformed variables
scale_transform_df <- function(scale, df) {
  if (empty(df)) return()

  aesthetics <- intersect(scale$aesthetics, names(df))
  if (length(aesthetics) == 0) return()

  lapply(df[aesthetics], scale_transform, scale = scale)
}

scale_transform <- function(scale, x) UseMethod("scale_transform")

scale_transform.continuous <- function(scale, x) {
  scale$trans$trans(x)
}
scale_transform.discrete <- function(scale, x) {
  x
}

# @return list of mapped variables
scale_map_df <- function(scale, df) {    
  if (empty(df)) return()

  aesthetics <- intersect(scale$aesthetics, names(df))
  if (length(aesthetics) == 0) return()
  
  lapply(df[aesthetics], scale_map, scale = scale)
}

scale_map <- function(scale, x) UseMethod("scale_map")

scale_map.continuous <- function(scale, x) {
  limits <- scale_limits(scale)
  x <- scale$oob(scale$rescaler(x, from = limits))
  pal <- scale$palette(x)
  ifelse(!is.na(x), pal, scale$na.value)
}

scale_map.discrete <- function(scale, x) {
  limits <- scale_limits(scale)

  n <- length(limits)
  pal <- scale$palette(n)[match(as.character(x), limits)]
  ifelse(!is.na(x), pal, scale$na.value)
}

scale_limits <- function(scale) {
  scale$limits %||% scale$range$range
}

# The phyical size of the scale, if a position scale
# Unlike limits, this always returns a numeric vector of length 2
scale_dimension <- function(scale, expand = scale$expand) UseMethod("scale_dimension")

scale_dimension.continuous  <- function(scale, expand = scale$expand) {
  expand_range(scale_limits(scale), expand[1], expand[2])  
}
scale_dimension.discrete <- function(scale, expand = scale$expand) {
  expand_range(length(scale_limits(scale)), expand[1], expand[2])  
}

scale_breaks <- function(scale, limits = scale_limits(scale)) {
  UseMethod("scale_breaks")
}

scale_breaks.continuous <- function(scale, limits = scale_limits(scale)) {
  # Limits in transformed space need to be converted back to data space
  limits <- scale$trans$inv(limits)
  
  if (zero_range(limits)) {
    breaks <- range[1]
  } else if (is.null(scale$breaks)) {
    breaks <- scale$trans$breaks(limits)
  } else if (is.function(scale$breaks)) {
    breaks <- scale$breaks(limits)
  } else {
    breaks <- scale$breaks
  }
  
  # Breaks in data space need to be converted back to transformed space
  # And any breaks outside the dimensions need to be thrown away
  discard(scale$trans$trans(breaks), scale_dimension(scale))
}

scale_breaks.discrete <- function(scale, limits = scale_limits(scale)) {
  if (is.null(scale$breaks)) {
    limits
  } else if (is.function(scale$breaks)) {
    scale$breaks(limits)
  } else {
    scale$breaks
  }
}

# The numeric position of scale breaks, when used for a position guide.
scale_break_positions <- function(scale) {
  scale_map(scale, scale_breaks(scale))
}

scale_breaks_minor <- function(scale, n = 2, b = scale_break_positions(scale), r = scale_limits(scale)) {
  if (length(b) == 1) return(b)

  bd <- diff(b)[1]
  if (min(r) < min(b)) b <- c(b[1] - bd, b)
  if (max(r) > max(b)) b <- c(b, b[length(b)] + bd)
  unique(unlist(mapply(seq, b[-length(b)], b[-1], length=n+1, SIMPLIFY=F)))
}

scale_labels <- function(scale, breaks = scale_breaks(scale)) {
  UseMethod("scale_labels")
}

scale_labels.continuous <- function(scale, breaks = scale_breaks(scale)) {
  breaks <- scale$trans$inv(breaks)
  
  if (is.null(scale$labels)) {
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

scale_labels.discrete <- function(scale, breaks = scale_breaks(scale)) {
  if (is.null(scale$labels)) {
    format(scale_limits(scale), justify = "none")
  } else if (is.function(scale$labels)) {
    scale$labels(breaks)
  } else {
    scale$labels
  }
}

print.scale <- function(x, ...) {
  print(x$call)
}

scale_clone <- function(scale) UseMethod("scale_clone")
