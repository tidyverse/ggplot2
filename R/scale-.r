#' @section Scales:
#'
#' All `scale_*` functions (like `scale_x_continuous`) return a
#' `Scale*` object (like `ScaleContinuous`). The `Scale*`
#' object represents a single scale.
#'
#' Each of the `Scale*` objects is a [ggproto()] object,
#' descended from the top-level `Scale`.
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

  # Train scale from a data frame.
  #
  # @return updated range (invisibly)
  # @seealso [scale_train()] for scale specific generic method
  train_df = function(self, df) {
    if (empty(df)) return()

    aesthetics <- intersect(self$aesthetics, names(df))
    for (aesthetic in aesthetics) {
      self$train(df[[aesthetic]])
    }
    invisible()
  },

  # Train an individual scale from a vector of data.
  train = function(self, x) {
    stop("Not implemented", call. = FALSE)
  },

  # Reset scale, untraining ranges
  reset = function(self) {
    self$range$reset()
  },

  is_empty = function(self) {
    is.null(self$range$range) && is.null(self$limits)
  },

  # @return list of transformed variables
  transform_df = function(self, df) {
    if (empty(df)) return()

    aesthetics <- intersect(self$aesthetics, names(df))
    if (length(aesthetics) == 0) return()

    lapply(df[aesthetics], self$transform)
  },

  transform = function(self, x) {
    stop("Not implemented", call. = FALSE)
  },

  # @return list of mapped variables
  map_df = function(self, df, i = NULL) {
    if (empty(df)) return()

    aesthetics <- intersect(self$aesthetics, names(df))
    names(aesthetics) <- aesthetics
    if (length(aesthetics) == 0) return()

    if (is.null(i)) {
      lapply(aesthetics, function(j) self$map(df[[j]]))
    } else {
      lapply(aesthetics, function(j) self$map(df[[j]][i]))
    }
  },

  # @kohske
  # map tentatively accept limits argument.
  # map replaces oob (i.e., outside limits) values with NA.
  #
  # Previously limits are always scale_limits(scale).
  # But if this function is called to get breaks,
  # and breaks spans oob, the oob breaks is replaces by NA.
  # This makes impossible to display oob breaks.
  # Now coord_train calls this function with limits determined by coord (with expansion).
  map = function(self, x, limits = self$get_limits()) {
    stop("Not implemented", call. = FALSE)
  },

  #  if scale contains a NULL, use the default scale range
  #  if scale contains a NA, use the default range for that axis, otherwise
  #  use the user defined limit for that axis
  get_limits = function(self) {
    if (self$is_empty()) return(c(0, 1))

    if (!is.null(self$limits)) {
      ifelse(!is.na(self$limits), self$limits, self$range$range)
    } else {
      self$range$range
    }
  },

  # The physical size of the scale.
  # This always returns a numeric vector of length 4, giving the physical
  # dimensions of a scale.
  dimension = function(self, expand = c(0, 0, 0, 0)) {
    stop("Not implemented", call. = FALSE)
  },

  get_breaks = function(self, limits = self$get_limits()) {
    stop("Not implemented", call. = FALSE)
  },

  # The numeric position of scale breaks, used by coord/guide
  break_positions = function(self, range = self$get_limits()) {
    self$map(self$get_breaks(range))
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(), limits = self$get_limits()) {
    stop("Not implemented", call. = FALSE)
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    stop("Not implemented", call. = FALSE)
  },

  # Each implementation of a Scale must implement a clone method that makes
  # copies of reference objecsts.
  clone = function(self) {
    stop("Not implemented", call. = FALSE)
  },

  break_info = function(self, range = NULL) {
    stop("Not implemented", call. = FALSE)
  },

  # Only relevant for positional scales
  axis_order = function(self) {
    ord <- c("primary", "secondary")
    if (self$position %in% c("right", "bottom")) {
      ord <- rev(ord)
    }
    ord
  },

  # Here to make it possible for scales to modify the default titles
  make_title = function(title) {
    title
  },
  make_sec_title = function(title) {
    title
  }
)

check_breaks_labels <- function(breaks, labels) {
  if (is.null(breaks)) return(TRUE)
  if (is.null(labels)) return(TRUE)

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
  rescaler = rescale, # Used by diverging and n colour gradients x
  oob = censor,
  minor_breaks = waiver(),

  is_discrete = function() FALSE,

  train = function(self, x) {
    if (length(x) == 0) return()
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
    x <- self$rescaler(self$oob(x, range = limits), from = limits)

    uniq <- unique(x)
    pal <- self$palette(uniq)
    scaled <- pal[match(x, uniq)]

    ifelse(!is.na(scaled), scaled, self$na.value)
  },

  dimension = function(self, expand = c(0, 0, 0, 0)) {
    expand_range4(self$get_limits(), expand)
  },

  get_breaks = function(self, limits = self$get_limits()) {
    if (self$is_empty()) return(numeric())

    # Limits in transformed space need to be converted back to data space
    limits <- self$trans$inverse(limits)

    if (is.null(self$breaks)) {
      return(NULL)
    } else if (identical(self$breaks, NA)) {
      stop("Invalid breaks specification. Use NULL, not NA")
    } else if (zero_range(as.numeric(limits))) {
      breaks <- limits[1]
    } else if (is.waive(self$breaks)) {
      breaks <- self$trans$breaks(limits)
    } else if (is.function(self$breaks)) {
      breaks <- self$breaks(limits)
    } else {
      breaks <- self$breaks
    }

    # Breaks in data space need to be converted back to transformed space
    # And any breaks outside the dimensions need to be flagged as missing
    #
    # @kohske
    # TODO: replace NA with something else for flag.
    #       guides cannot discriminate oob from missing value.
    breaks <- censor(self$trans$transform(breaks), self$trans$transform(limits),
                     only.finite = FALSE)
    breaks
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(), limits = self$get_limits()) {
    if (zero_range(as.numeric(limits))) {
      return()
    }

    if (is.null(self$minor_breaks)) {
      return(NULL)
    } else if (identical(self$minor_breaks, NA)) {
      stop("Invalid minor_breaks specification. Use NULL, not NA", call. = FALSE)
    } else if (is.waive(self$minor_breaks)) {
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
    if (is.null(breaks)) return(NULL)

    breaks <- self$trans$inverse(breaks)

    if (is.null(self$labels)) {
      return(NULL)
    } else if (identical(self$labels, NA)) {
      stop("Invalid labels specification. Use NULL, not NA", call. = FALSE)
    } else if (is.waive(self$labels)) {
      labels <- self$trans$format(breaks)
    } else if (is.function(self$labels)) {
      labels <- self$labels(breaks)
    } else {
      labels <- self$labels
    }
    if (length(labels) != length(breaks)) {
      stop("Breaks and labels are different lengths")
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

    list(range = range, labels = labels,
         major = major_n, minor = minor_n,
         major_source = major, minor_source = minor)
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
    if (length(x) == 0) return()
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
      if (!is.null(self$n.breaks.cache)) warning("Cached palette does not match requested", call. = FALSE)
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

  dimension = function(self, expand = c(0, 0, 0, 0)) {
    expand_range4(length(self$get_limits()), expand)
  },

  get_breaks = function(self, limits = self$get_limits()) {
    if (self$is_empty()) return(numeric())

    if (is.null(self$breaks)) {
      return(NULL)
    } else if (identical(self$breaks, NA)) {
      stop("Invalid breaks specification. Use NULL, not NA", call. = FALSE)
    } else if (is.waive(self$breaks)) {
      breaks <- limits
    } else if (is.function(self$breaks)) {
      breaks <- self$breaks(limits)
    } else {
      breaks <- self$breaks
    }

    # Breaks can only occur only on values in domain
    in_domain <- intersect(breaks, self$get_limits())
    structure(in_domain, pos = match(in_domain, breaks))
  },

  get_breaks_minor = function(...) NULL,

  get_labels = function(self, breaks = self$get_breaks()) {
    if (self$is_empty()) return(character())

    if (is.null(breaks)) return(NULL)

    if (is.null(self$labels)) {
      return(NULL)
    } else if (identical(self$labels, NA)) {
      stop("Invalid labels specification. Use NULL, not NA", call. = FALSE)
    } else if (is.waive(self$labels)) {
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

    list(range = range, labels = labels,
         major = major_n, minor = NULL,
         major_source = major, minor_source = NULL)
  }
)


#' Continuous scale constructor.
#'
#' @export
#' @param aesthetics The names of the aesthetics that this scale works with
#' @param scale_name The name of the scale
#' @param palette A palette function that when called with a numeric vector with
#'   values between 0 and 1 returns the corresponding values in the range the
#'   scale maps to.
#' @param name The name of the scale. Used as the axis or legend title. If
#'   `waiver()`, the default, the name of the scale is taken from the first
#'   mapping used for that aesthetic. If `NULL`, the legend title will be
#'   omitted.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     transformation object
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks
#'     as output
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
#' @param limits A numeric vector of length two providing limits of the scale.
#'   Use `NA` to refer to the existing minimum or maximum.
#' @param rescaler  Used by diverging and n colour gradients
#'   (i.e. [scale_colour_gradient2()], [scale_colour_gradientn()]).
#'   A function used to scale the input values to the range \[0, 1].
#' @param oob Function that handles limits outside of the scale limits
#'   (out of bounds). The default replaces out of bounds values with `NA`.
#' @inheritParams scale_x_discrete
#' @param na.value Missing values will be replaced with this value.
#' @param trans Either the name of a transformation object, or the
#'   object itself. Built-in transformations include "asn", "atanh",
#'   "boxcox", "exp", "identity", "log", "log10", "log1p", "log2",
#'   "logit", "probability", "probit", "reciprocal", "reverse" and "sqrt".
#'
#'   A transformation object bundles together a transform, its inverse,
#'   and methods for generating breaks and labels. Transformation objects
#'   are defined in the scales package, and are called `name_trans`, e.g.
#'   [scales::boxcox_trans()]. You can create your own
#'   transformation with [scales::trans_new()].
#' @param guide A function used to create a guide or its name. See
#'   [guides()] for more info.
#' @param position The position of the axis. "left" or "right" for vertical
#' scales, "top" or "bottom" for horizontal scales
#' @param super The super class to use for the constructed scale
#' @keywords internal
continuous_scale <- function(aesthetics, scale_name, palette, name = waiver(),
  breaks = waiver(), minor_breaks = waiver(), labels = waiver(), limits = NULL,
  rescaler = rescale, oob = censor, expand = waiver(), na.value = NA_real_,
  trans = "identity", guide = "legend", position = "left", super = ScaleContinuous) {

  aesthetics <- standardise_aes_names(aesthetics)

  check_breaks_labels(breaks, labels)

  position <- match.arg(position, c("left", "right", "top", "bottom"))

  if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
    guide <- "none"
  }

  trans <- as.trans(trans)
  if (!is.null(limits)) {
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
    rescaler = rescaler,  # Used by diverging and n colour gradients
    oob = oob,

    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,

    labels = labels,
    guide = guide,
    position = position
  )
}

#' Discrete scale constructor.
#'
#' @export
#' @inheritParams continuous_scale
#' @param palette A palette function that when called with a single integer
#'   argument (the number of levels in the scale) returns the values that
#'   they should take.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     transformation object
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

  if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
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
