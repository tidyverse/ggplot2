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

#' Continuous scale constructor.
#'
#' @export
#' @param aesthetics character 
#' @keywords internal
continuous_scale <- function(aesthetics, scale_name, palette, name = NULL, breaks = NULL, minor_breaks = NULL, labels = NULL, legend = NULL, limits = NULL, rescaler = rescale, oob = censor, expand = c(0, 0), na.value = NA, trans = "identity", guide="legend") {

  if (!is.null(legend)) {
    warning("\"legend\" argument in scale_XXX is deprecated. Use guide=\"none\" for suppress the guide display.")
    if (legend == FALSE) guide = "none"
    else if (legend == TRUE) guide = "legend"
  }
  
  bad_labels <- is.vector(breaks) && is.vector(labels) && 
    length(breaks) != length(labels)
  if (bad_labels) {
    stop("Breaks and labels have unequal lengths", call. = FALSE)
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
#' @keywords internal
discrete_scale <- function(aesthetics, scale_name, palette, name = NULL, breaks = NULL, labels = NULL, legend = NULL, limits = NULL, expand = c(0, 0), na.value = NA, drop = TRUE, guide="legend") {

  if (!is.null(legend)) {
    warning("\"legend\" argument in scale_XXX is deprecated. Use guide=\"none\" for suppress the guide display.")
    if (legend == FALSE) guide = "none"
    else if (legend == TRUE) guide = "legend"
  }
  
  bad_labels <- is.vector(breaks) && is.vector(labels) && 
    length(breaks) != length(labels)
  if (bad_labels) {
    stop("Breaks and labels have unequal lengths", call. = FALSE)
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

#' Train an individual scale from a vector of data.
#'
#' @S3method scale_train continuous
#' @S3method scale_train discrete
scale_train <- function(scale, x) UseMethod("scale_train")
scale_train.continuous <- function(scale, x) {
  scale$range$train(x)
}
scale_train.discrete <- function(scale, x) {
  scale$range$train(x, drop = scale$drop)
}

# Reset scale, untraining ranges
scale_reset <- function(scale, x) UseMethod("scale_reset")
#' @S3method scale_reset default
scale_reset.default <- function(scale, x) {
  scale$range$reset()
}

# @return list of transformed variables
scale_transform_df <- function(scale, df) {
  if (empty(df)) return()

  aesthetics <- intersect(scale$aesthetics, names(df))
  if (length(aesthetics) == 0) return()

  lapply(df[aesthetics], scale_transform, scale = scale)
}

#' @S3method scale_transform continuous
#' @S3method scale_transform discrete
scale_transform <- function(scale, x) UseMethod("scale_transform")

scale_transform.continuous <- function(scale, x) {
  scale$trans$trans(x)
}
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

#' @S3method scale_map continuous
#' @S3method scale_map discrete
scale_map <- function(scale, x) UseMethod("scale_map")

scale_map.continuous <- function(scale, x) {
  limits <- scale_limits(scale)
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

scale_map.discrete <- function(scale, x) {
  limits <- scale_limits(scale)

  n <- length(limits)
  pal <- scale$palette(n)
  
  if (is.null(names(pal))) {
    pal_match <- pal[match(as.character(x), limits)]
  } else {
    pal_match <- pal[match(as.character(x), names(pal))]    
  } 
  
  ifelse(!is.na(x), pal_match, scale$na.value)
}

scale_limits <- function(scale)
  UseMethod("scale_limits")

scale_limits.default <- function(scale) {
  scale$limits %||% scale$range$range
}

# The phyical size of the scale, if a position scale
# Unlike limits, this always returns a numeric vector of length 2
#' @S3method scale_dimension continuous
#' @S3method scale_dimension discrete
scale_dimension <- function(scale, expand = scale$expand) UseMethod("scale_dimension")

scale_dimension.continuous  <- function(scale, expand = scale$expand) {
  expand_range(scale_limits(scale), expand[1], expand[2])  
}
scale_dimension.discrete <- function(scale, expand = scale$expand) {
  expand_range(length(scale_limits(scale)), expand[1], expand[2])  
}

#' @S3method scale_breaks continuous
#' @S3method scale_breaks discrete
scale_breaks <- function(scale, limits = scale_limits(scale)) {
  UseMethod("scale_breaks")
}

scale_breaks.continuous <- function(scale, limits = scale_limits(scale)) {
  # Limits in transformed space need to be converted back to data space
  limits <- scale$trans$inv(limits)
  
  if (zero_range(as.numeric(limits))) {
    breaks <- limits[1]
  } else if (is.null(scale$breaks)) {
    breaks <- scale$trans$breaks(limits)
  } else if (is.function(scale$breaks)) {
    breaks <- scale$breaks(limits)
  } else {
    breaks <- scale$breaks
  }
  
  # Breaks in data space need to be converted back to transformed space
  # And any breaks outside the dimensions need to be flagged as missing
  breaks <- censor(scale$trans$trans(breaks), scale_dimension(scale))
  if (length(breaks) == 0) {
    stop("Zero breaks in scale for ", paste(scale$aesthetics, collapse = "/"),
      call. = FALSE)
  }
  breaks
}

scale_breaks.discrete <- function(scale, limits = scale_limits(scale)) {
  if (is.null(scale$breaks)) {
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

# The numeric position of scale breaks, when used for a position guide.
scale_break_positions <- function(scale) {
  scale_map(scale, scale_breaks(scale))
}

#' @S3method scale_breaks_minor continuous
#' @S3method scale_breaks_minor discrete
scale_breaks_minor<- function(scale, ...) {
  UseMethod("scale_breaks_minor")
}

scale_breaks_minor.continuous <- function(scale, n = 2, b = scale_break_positions(scale), limits = scale_limits(scale)) {
  if (zero_range(as.numeric(limits))) {
    return()
  }
  
  if (is.null(scale$minor_breaks)) {
    b <- b[!is.na(b)]
    if (length(b) < 2) return()

    bd <- diff(b)[1]
    if (min(limits) < min(b)) b <- c(b[1] - bd, b)
    if (max(limits) > max(b)) b <- c(b, b[length(b)] + bd)
    breaks <- unique(unlist(mapply(seq, b[-length(b)], b[-1], length=n+1,
      SIMPLIFY = FALSE)))
  } else if (is.function(scale$minor_breaks)) {
    breaks <- scale$minor_breaks(scale$trans$inv(limits))
  } else {
    breaks <- scale$minor_breaks
  }
  
  # Any minor breaks outside the dimensions need to be thrown away
  discard(breaks, scale_dimension(scale))
}

scale_breaks_minor.discrete <- function(...) NULL

scale_breaks_minor_positions <- function(scale) {
  scale_map(scale, scale_breaks_minor(scale))
}

#' @S3method scale_labels continuous
#' @S3method scale_labels discrete
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
    format(scale_breaks(scale), justify = "none", trim = TRUE)
  } else if (is.function(scale$labels)) {
    scale$labels(breaks)
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

print.scale <- function(x, ...) {
  print(x$call)
}

scale_clone <- function(scale) UseMethod("scale_clone")

#' @S3method scale_clone continuous
scale_clone.continuous <- function(scale) {
  new <- scale
  new$range <- ContinuousRange$new()  
  new
}

#' @S3method scale_clone discrete
scale_clone.discrete <- function(scale) {
  new <- scale
  new$range <- DiscreteRange$new()
  new
}
