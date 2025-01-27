
#' Continuous scale constructor
#'
#' @export
#' @param aesthetics The names of the aesthetics that this scale works with.
#' @param scale_name `r lifecycle::badge("deprecated")` The name of the scale
#'   that should be used for error messages associated with this scale.
#' @param palette A palette function that when called with a numeric vector with
#'   values between 0 and 1 returns the corresponding output values
#'   (e.g., [scales::pal_area()]).
#' @param name The name of the scale. Used as the axis or legend title. If
#'   `waiver()`, the default, the name of the scale is taken from the first
#'   mapping used for that aesthetic. If `NULL`, the legend title will be
#'   omitted.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     [transformation object][scales::new_transform()]
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks
#'     as output (e.g., a function returned by [scales::extended_breaks()]).
#'     Note that for position scales, limits are provided after scale expansion.
#'     Also accepts rlang [lambda][rlang::as_function()] function notation.
#' @param minor_breaks One of:
#'   - `NULL` for no minor breaks
#'   - `waiver()` for the default breaks (none for discrete, one minor break
#'     between each major break for continuous)
#'   - A numeric vector of positions
#'   - A function that given the limits returns a vector of minor breaks. Also
#'     accepts rlang [lambda][rlang::as_function()] function notation. When
#'     the function has two arguments, it will be given the limits and major
#'     break positions.
#' @param n.breaks An integer guiding the number of major breaks. The algorithm
#'   may choose a slightly different number to ensure nice break labels. Will
#'   only have an effect if `breaks = waiver()`. Use `NULL` to use the default
#'   number of breaks given by the transformation.
#' @param labels One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels computed by the
#'     transformation object
#'   - A character vector giving labels (must be same length as `breaks`)
#'   - An expression vector (must be the same length as breaks). See ?plotmath for details.
#'   - A function that takes the breaks as input and returns labels
#'     as output. Also accepts rlang [lambda][rlang::as_function()] function
#'     notation.
#' @param limits One of:
#'   - `NULL` to use the default scale range
#'   - A numeric vector of length two providing limits of the scale.
#'     Use `NA` to refer to the existing minimum or maximum
#'   - A function that accepts the existing (automatic) limits and returns
#'     new limits. Also accepts rlang [lambda][rlang::as_function()] function
#'     notation.
#'   Note that setting limits on positional scales will **remove** data outside of the limits.
#'   If the purpose is to zoom, use the limit argument in the coordinate system
#'   (see [coord_cartesian()]).
#' @param rescaler A function used to scale the input values to the
#'   range \[0, 1]. This is always [scales::rescale()], except for
#'   diverging and n colour gradients (i.e., [scale_colour_gradient2()],
#'   [scale_colour_gradientn()]). The `rescaler` is ignored by position
#'   scales, which always use [scales::rescale()]. Also accepts rlang
#'   [lambda][rlang::as_function()] function notation.
#' @param oob One of:
#'   - Function that handles limits outside of the scale limits
#'   (out of bounds). Also accepts rlang [lambda][rlang::as_function()]
#'   function notation.
#'   - The default ([scales::censor()]) replaces out of
#'   bounds values with `NA`.
#'   - [scales::squish()] for squishing out of bounds values into range.
#'   - [scales::squish_infinite()] for squishing infinite values into range.
#' @param na.value Missing values will be replaced with this value.
#' @param transform For continuous scales, the name of a transformation object
#'   or the object itself. Built-in transformations include "asn", "atanh",
#'   "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2",
#'   "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal",
#'   "reverse", "sqrt" and "time".
#'
#'   A transformation object bundles together a transform, its inverse,
#'   and methods for generating breaks and labels. Transformation objects
#'   are defined in the scales package, and are called `transform_<name>`. If
#'   transformations require arguments, you can call them from the scales
#'   package, e.g. [`scales::transform_boxcox(p = 2)`][scales::transform_boxcox].
#'   You can create your own transformation with [scales::new_transform()].
#' @param trans `r lifecycle::badge("deprecated")` Deprecated in favour of
#'   `transform`.
#' @param guide A function used to create a guide or its name. See
#'   [guides()] for more information.
#' @param expand For position scales, a vector of range expansion constants used to add some
#'   padding around the data to ensure that they are placed some distance
#'   away from the axes. Use the convenience function [expansion()]
#'   to generate the values for the `expand` argument. The defaults are to
#'   expand the scale by 5% on each side for continuous variables, and by
#'   0.6 units on each side for discrete variables.
#' @param position For position scales, The position of the axis.
#' `left` or `right` for y axes, `top` or `bottom` for x axes.
#' @param call The `call` used to construct the scale for reporting messages.
#' @param super The super class to use for the constructed scale
#'
#' @seealso
#' The `r link_book("new scales section", "extensions#sec-new-scales")`
#'
#' @keywords internal
continuous_scale <- function(aesthetics, scale_name = deprecated(), palette, name = waiver(),
                             breaks = waiver(), minor_breaks = waiver(), n.breaks = NULL,
                             labels = waiver(), limits = NULL, rescaler = rescale,
                             oob = censor, expand = waiver(), na.value = NA_real_,
                             transform = "identity", trans = deprecated(),
                             guide = "legend", position = "left",
                             call = caller_call(),
                             super = ScaleContinuous) {
  call <- call %||% current_call()
  if (lifecycle::is_present(scale_name)) {
    deprecate_soft0("3.5.0", "continuous_scale(scale_name)")
  }
  if (lifecycle::is_present(trans)) {
    deprecate_soft0("3.5.0", "continuous_scale(trans)", "continuous_scale(transform)")
    transform <- trans
  }

  aesthetics <- standardise_aes_names(aesthetics)

  check_breaks_labels(breaks, labels, call = call)

  position <- arg_match0(position, c("left", "right", "top", "bottom"))

  # If the scale is non-positional, break = NULL means removing the guide
  if (is.null(breaks) && !any(is_position_aes(aesthetics))) {
    guide <- "none"
  }

  transform <- as.transform(transform)

  # Convert formula to function if appropriate
  limits   <- allow_lambda(limits)
  breaks   <- allow_lambda(breaks)
  labels   <- allow_lambda(labels)
  rescaler <- allow_lambda(rescaler)
  oob      <- allow_lambda(oob)
  minor_breaks <- allow_lambda(minor_breaks)

  if (!is.null(limits) && !is.function(limits)) {
    limits <- transform$transform(limits)
    if (!anyNA(limits)) {
      limits <- sort(limits)
    }
  }
  check_continuous_limits(limits, call = call)

  ggproto(NULL, super,
    call = call,

    aesthetics = aesthetics,
    palette = palette,

    range = ContinuousRange$new(),
    limits = limits,
    trans = transform,
    na.value = na.value,
    expand = expand,
    rescaler = rescaler,
    oob = oob,

    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,
    n.breaks = n.breaks,

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
#'   they should take (e.g., [scales::pal_hue()]).
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks (the scale limits)
#'   - A character vector of breaks
#'   - A function that takes the limits as input and returns breaks
#'     as output. Also accepts rlang [lambda][rlang::as_function()] function
#'     notation.
#' @param limits One of:
#'   - `NULL` to use the default scale values
#'   - A character vector that defines possible values of the scale and their
#'     order
#'   - A function that accepts the existing (automatic) values and returns
#'     new ones. Also accepts rlang [lambda][rlang::as_function()] function
#'     notation.
#' @param drop Should unused factor levels be omitted from the scale?
#'    The default, `TRUE`, uses the levels that appear in the data;
#'    `FALSE` includes the levels in the factor. Please note that to display
#'    every level in a legend, the layer should use `show.legend = TRUE`.
#' @param na.translate Unlike continuous scales, discrete scales can easily show
#'   missing values, and do so by default. If you want to remove missing values
#'   from a discrete scale, specify `na.translate = FALSE`.
#' @param na.value If `na.translate = TRUE`, what aesthetic value should the
#'   missing values be displayed as? Does not apply to position scales
#'   where `NA` is always placed at the far right.
#' @seealso
#' The `r link_book("new scales section", "extensions#sec-new-scales")`
#' @keywords internal
discrete_scale <- function(aesthetics, scale_name = deprecated(), palette, name = waiver(),
                           breaks = waiver(), minor_breaks = waiver(),
                           labels = waiver(), limits = NULL, expand = waiver(),
                           na.translate = TRUE, na.value = NA, drop = TRUE,
                           guide = "legend", position = "left",
                           call = caller_call(),
                           super = ScaleDiscrete) {
  call <- call %||% current_call()
  if (lifecycle::is_present(scale_name)) {
    deprecate_soft0("3.5.0", "discrete_scale(scale_name)")
  }

  aesthetics <- standardise_aes_names(aesthetics)

  check_breaks_labels(breaks, labels, call = call)

  # Convert formula input to function if appropriate
  limits <- allow_lambda(limits)
  breaks <- allow_lambda(breaks)
  labels <- allow_lambda(labels)
  minor_breaks <- allow_lambda(minor_breaks)

  if (!is.function(limits) && (length(limits) > 0) && !is.discrete(limits)) {
    cli::cli_warn(c(
      "Continuous limits supplied to discrete scale.",
      "i" = "Did you mean {.code limits = factor(...)} or {.fn scale_*_continuous}?"
    ), call = call)
  }

  position <- arg_match0(position, c("left", "right", "top", "bottom"))

  # If the scale is non-positional, break = NULL means removing the guide
  is_position <- any(is_position_aes(aesthetics))
  if (is.null(breaks) && !is_position) {
    guide <- "none"
  }
  if (is_position && identical(palette, identity)) {
    palette <- seq_len
  }

  ggproto(NULL, super,
    call = call,

    aesthetics = aesthetics,
    palette = palette,

    range = DiscreteRange$new(),
    limits = limits,
    na.value = na.value,
    na.translate = na.translate,
    expand = expand,

    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    drop = drop,
    guide = guide,
    position = position
  )
}

#' Binning scale constructor
#'
#' @export
#' @inheritParams continuous_scale
#' @param n.breaks The number of break points to create if breaks are not given
#'   directly.
#' @param nice.breaks Logical. Should breaks be attempted placed at nice values
#'   instead of exactly evenly spaced between the limits. If `TRUE` (default)
#'   the scale will ask the transformation object to create breaks, and this
#'   may result in a different number of breaks than requested. Ignored if
#'   breaks are given explicitly.
#' @param oob One of:
#'   - Function that handles limits outside of the scale limits
#'   (out of bounds). Also accepts rlang [lambda][rlang::as_function()]
#'   function notation.
#'   - The default ([scales::squish()]) squishes out of
#'   bounds values into range.
#'   - [scales::censor] for replacing out of bounds values with `NA`.
#'   - [scales::squish_infinite()] for squishing infinite values into range.
#' @param right Should the intervals be closed on the right (`TRUE`, default) or
#'   should the intervals be closed on the left (`FALSE`)? 'Closed on the right'
#'   means that values at break positions are part of the lower bin (open on the
#'   left), whereas they are part of the upper bin when intervals are closed on
#'   the left (open on the right).
#' @param show.limits should the limits of the scale appear as ticks
#' @seealso
#' The `r link_book("new scales section", "extensions#sec-new-scales")`
#' @keywords internal
binned_scale <- function(aesthetics, scale_name = deprecated(), palette, name = waiver(),
                         breaks = waiver(), labels = waiver(), limits = NULL,
                         rescaler = rescale, oob = squish, expand = waiver(),
                         na.value = NA_real_, n.breaks = NULL, nice.breaks = TRUE,
                         right = TRUE, transform = "identity",
                         trans = deprecated(), show.limits = FALSE,
                         guide = "bins", position = "left",
                         call = caller_call(),
                         super = ScaleBinned) {
  if (lifecycle::is_present(scale_name)) {
    deprecate_soft0("3.5.0", "binned_scale(scale_name)")
  }
  if (lifecycle::is_present(trans)) {
    deprecate_soft0("3.5.0", "binned_scale(trans)", "binned_scale(transform)")
    transform <- trans
  }

  call <- call %||% current_call()

  aesthetics <- standardise_aes_names(aesthetics)

  check_breaks_labels(breaks, labels, call = call)

  position <- arg_match0(position, c("left", "right", "top", "bottom"))

  if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
    guide <- "none"
  }

  transform <- as.transform(transform)

  # Convert formula input to function if appropriate
  limits   <- allow_lambda(limits)
  breaks   <- allow_lambda(breaks)
  labels   <- allow_lambda(labels)
  rescaler <- allow_lambda(rescaler)
  oob      <- allow_lambda(oob)

  if (!is.null(limits) && !is.function(limits)) {
    limits <- transform$transform(limits)
    if (!anyNA(limits)) {
      limits <- sort(limits)
    }
  }

  ggproto(NULL, super,
    call = call,

    aesthetics = aesthetics,
    palette = palette,

    range = ContinuousRange$new(),
    limits = limits,
    trans = transform,
    na.value = na.value,
    expand = expand,
    rescaler = rescaler,
    oob = oob,
    n.breaks = n.breaks,
    nice.breaks = nice.breaks,
    right = right,
    show.limits = show.limits,

    name = name,
    breaks = breaks,

    labels = labels,
    guide = guide,
    position = position
  )
}

#' @export
#' @rdname is_tests
is.scale <- function(x) inherits(x, "Scale")

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
#'   determined by `self$rescale()` and `self$palette` (except for position scales,
#'   which do not use the default implementation of this method). The output corresponds
#'   to the transformed data value in aesthetic space (e.g., a color, line width, or size).
#'
#' - `rescale()` Rescale transformed data to the range 0, 1. This is most useful for
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
#' - `get_transformation()` Returns the scale's transformation object.
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
#'   this range given an expansion of length 4 (see [expansion()]).
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
  palette = function() {
    cli::cli_abort("Not implemented.")
  },

  range = Range$new(),
  limits = NULL,
  na.value = NA,
  expand = waiver(),

  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  guide = "legend",
  position = "left",


  is_discrete = function() {
    cli::cli_abort("Not implemented.")
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
    cli::cli_abort("Not implemented.", call = self$call)
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
    cli::cli_abort("Not implemented.", call = self$call)
  },

  map_df = function(self, df, i = NULL) {
    if (empty(df)) {
      return()
    }
    self$palette <- self$palette %||% fallback_palette(self)

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
    cli::cli_abort("Not implemented.", call = self$call)
  },

  rescale = function(self, x, limits = self$get_limits(), range = self$dimension()) {
    cli::cli_abort("Not implemented.", call = self$call)
  },

  get_limits = function(self) {
    if (self$is_empty()) {
      return(c(0, 1))
    }

    if (is.null(self$limits)) {
      self$range$range
    } else if (is.function(self$limits)) {
      self$limits(self$range$range)
    } else {
      self$limits
    }
  },

  dimension = function(self, expand = expansion(0, 0), limits = self$get_limits()) {
    cli::cli_abort("Not implemented.", call = self$call)
  },

  get_breaks = function(self, limits = self$get_limits()) {
    cli::cli_abort("Not implemented.", call = self$call)
  },

  break_positions = function(self, range = self$get_limits()) {
    self$map(self$get_breaks(range))
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(), limits = self$get_limits()) {
    cli::cli_abort("Not implemented.", call = self$call)
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    cli::cli_abort("Not implemented.", call = self$call)
  },

  get_transformation = function(self) {
    self$trans
  },

  clone = function(self) {
    cli::cli_abort("Not implemented.", call = self$call)
  },

  break_info = function(self, range = NULL) {
    cli::cli_abort("Not implemented.", call = self$call)
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

check_breaks_labels <- function(breaks, labels, call = NULL) {
  if (is.null(breaks) || is.null(labels)) {
    return(invisible())
  }

  bad_labels <- is.atomic(breaks) && is.atomic(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    cli::cli_abort(
      "{.arg breaks} and {.arg labels} must have the same length.",
      call = call
    )
  }

  invisible()
}

default_transform <- function(self, x) {
  transformation <- self$get_transformation()
  new_x <- transformation$transform(x)
  check_transformation(x, new_x, transformation$name, call = self$call)
  new_x
}

has_default_transform <- function(scale) {
  transform_method <- environment(scale$transform)$f
  identical(default_transform, transform_method) || identical(identity, transform_method)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuous <- ggproto("ScaleContinuous", Scale,
  range = ContinuousRange$new(),
  na.value = NA_real_,
  rescaler = rescale,
  oob = censor,
  minor_breaks = waiver(),
  n.breaks = NULL,
  trans = transform_identity(),

  is_discrete = function() FALSE,

  train = function(self, x) {
    if (length(x) == 0) {
      return()
    }
    # Intercept error here to give examples and mention scale in call
    if (is.factor(x) || !typeof(x) %in% c("integer", "double")) {
      # These assumptions only hold for standard ContinuousRange class, so
      # we skip the error if another range class is used
      if (inherits(self$range, "ContinuousRange")) {
        cli::cli_abort(
          c("Discrete values supplied to continuous scale.",
            i = "Example values: {.and {.val {head(x, 5)}}}"),
          call = self$call
        )
      }
    }
    self$range$train(x)
  },

  is_empty = function(self) {
    has_data <- !is.null(self$range$range)
    has_limits <- is.function(self$limits) || (!is.null(self$limits) && all(is.finite(self$limits)))
    !has_data && !has_limits
  },

  transform = default_transform,

  map = function(self, x, limits = self$get_limits()) {
    x <- self$rescale(self$oob(x, range = limits), limits)

    uniq <- unique0(x)
    pal <- self$palette(uniq)
    scaled <- pal[match(x, uniq)]

    ifelse(!is.na(scaled), scaled, self$na.value)
  },

  rescale = function(self, x, limits = self$get_limits(), range = limits) {
    self$rescaler(x, from = range)
  },

  get_limits = function(self) {
    if (self$is_empty()) {
      return(c(0, 1))
    }

    if (is.null(self$limits)) {
      self$range$range
    } else if (is.function(self$limits)) {
      transformation <- self$get_transformation()
      # if limits is a function, it expects to work in data space
      transformation$transform(self$limits(transformation$inverse(self$range$range)))
    } else {
      # NA limits for a continuous scale mean replace with the min/max of data
      ifelse(is.na(self$limits), self$range$range, self$limits)
    }
  },

  dimension = function(self, expand = expansion(0, 0), limits = self$get_limits()) {
    expand_limits_scale(self, expand, limits)
  },

  get_breaks = function(self, limits = self$get_limits()) {
    if (self$is_empty()) {
      return(numeric())
    }
    transformation <- self$get_transformation()
    # Ensure limits don't exceed domain (#980)
    domain <- suppressWarnings(transformation$transform(transformation$domain))
    domain <- sort(domain)
    # To avoid NaN causing issues. NaN are dropped by the sort()
    if (length(domain) == 2 && !zero_range(domain)) {
      limits <- oob_squish(limits, domain)
    }

    # Limits in transformed space need to be converted back to data space
    limits <- transformation$inverse(limits)

    if (is.null(self$breaks)) {
      return(NULL)
    }

    if (identical(self$breaks, NA)) {
      cli::cli_abort(
        "Invalid {.arg breaks} specification. Use {.code NULL}, not {.code NA}.",
        call = self$call
      )
    }

    # Compute `zero_range()` in transformed space in case `limits` in data space
    # don't support conversion to numeric (#5304)
    if (zero_range(as.numeric(transformation$transform(limits)))) {
      breaks <- limits[1]
    } else if (is.waiver(self$breaks)) {
      if (!is.null(self$n.breaks) && trans_support_nbreaks(transformation)) {
        breaks <- transformation$breaks(limits, self$n.breaks)
      } else {
        if (!is.null(self$n.breaks)) {
          cli::cli_warn(
            "Ignoring {.arg n.breaks}. Use a {.cls transform} object that supports setting number of breaks.",
            call = self$call
          )
        }
        breaks <- transformation$breaks(limits)
      }
    } else if (is.function(self$breaks)) {
      breaks <- self$breaks(limits)
    } else {
      breaks <- self$breaks
    }

    # Breaks in data space need to be converted back to transformed space
    transformation$transform(breaks)
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(), limits = self$get_limits()) {
    if (zero_range(as.numeric(limits))) {
      return()
    }

    if (is.null(self$minor_breaks)) {
      return(NULL)
    }

    if (identical(self$minor_breaks, NA)) {
      cli::cli_abort(
        "Invalid {.arg minor_breaks} specification. Use {.code NULL}, not {.code NA}.",
        call = self$call
      )
    }
    # major breaks are not censored, however;
    # some transforms assume finite major breaks
    b <- b[is.finite(b)]

    transformation <- self$get_transformation()
    if (is.waiver(self$minor_breaks)) {
      if (is.null(b)) {
        breaks <- NULL
      } else {
        breaks <- transformation$minor_breaks(b, limits, n)
      }
    } else if (is.function(self$minor_breaks)) {
      # Using `fetch_ggproto` here to avoid auto-wrapping the user-supplied
      # breaks function as a ggproto method.
      break_fun <- fetch_ggproto(self, "minor_breaks")
      arg_names <- fn_fmls_names(break_fun)

      # Find breaks in data space
      if (length(arg_names) == 1L) {
        breaks <- break_fun(transformation$inverse(limits))
      } else {
        breaks <- break_fun(transformation$inverse(limits), transformation$inverse(b))
      }
      # Convert breaks to numeric
      breaks <- transformation$transform(breaks)
    } else {
      breaks <- transformation$transform(self$minor_breaks)
    }

    # Any minor breaks outside the dimensions need to be thrown away
    discard(breaks, limits)
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    if (is.null(breaks)) {
      return(NULL)
    }

    transformation <- self$get_transformation()
    breaks <- transformation$inverse(breaks)

    if (is.null(self$labels)) {
      return(NULL)
    }

    if (identical(self$labels, NA)) {
      cli::cli_abort(
        "Invalid {.arg labels} specification. Use {.code NULL}, not {.code NA}.",
        call = self$call
      )
    }

    if (is.waiver(self$labels)) {
      labels <- transformation$format(breaks)
    } else if (is.function(self$labels)) {
      labels <- self$labels(breaks)
    } else {
      labels <- self$labels
    }

    if (!identical(size0(labels), size0(breaks))) {
      cli::cli_abort(
        "{.arg breaks} and {.arg labels} have different lengths.",
        call = self$call
      )
    }

    if (obj_is_list(labels)) {
      # Guard against list with empty elements
      labels[lengths(labels) == 0] <- ""
      # Make sure each element is scalar
      labels <- lapply(labels, `[`, 1)
    }
    if (is.expression(labels)) {
      labels <- as.list(labels)
    }

    labels
  },

  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- ContinuousRange$new()
    new
  },

  break_info = function(self, range = NULL) {
    # range
    if (is.null(range)) range <- self$dimension()

    # major breaks
    major <- self$get_breaks(range)

    # labels
    labels <- self$get_labels(major)

    # minor breaks
    minor <- self$get_breaks_minor(b = major, limits = range)
    if (!is.null(minor)) minor <- minor[!is.na(minor)]

    major <- oob_censor_any(major, range)

    # drop oob breaks/labels by testing major == NA
    if (!is.null(labels)) labels <- labels[!is.na(major)]
    if (!is.null(major)) major <- major[!is.na(major)]

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
    if (is.function(self$limits)) {
      cat(" Limits: function()\n")
    } else {
      cat(" Limits: ", show_range(self$dimension()), "\n", sep = "")
    }
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
    # Intercept error here to give examples and mention scale in call
    if (!is.discrete(x)) {
      # These assumptions only hold for standard DiscreteRange class, so
      # we skip the error if another range class is used
      if (inherits(self$range, "DiscreteRange")) {
        cli::cli_abort(
          c("Continuous values supplied to discrete scale.",
            i = "Example values: {.and {.val {head(x, 5)}}}"),
          call = self$call
        )
      }
    }
    self$range$train(x, drop = self$drop, na.rm = !self$na.translate)
  },

  transform = identity,

  map = function(self, x, limits = self$get_limits()) {
    limits <- vec_slice(limits, !is.na(limits))
    n <- vec_size(limits)
    if (n < 1) {
      return(vec_rep(self$na.value, vec_size(x)))
    }
    if (!is.null(self$n.breaks.cache) && self$n.breaks.cache == n) {
      pal <- self$palette.cache
    } else {
      if (!is.null(self$n.breaks.cache)) {
        cli::cli_warn(
          "Cached palette does not match requested.",
          call = self$call
        )
      }
      pal <- self$palette(n)
      self$palette.cache <- pal
      self$n.breaks.cache <- n
    }

    na_value <- NA
    if (self$na.translate) {
      na_value <- self$na.value
      if (obj_is_list(pal) && !obj_is_list(na_value)) {
        # We prevent a casting error that occurs when mapping grid patterns
        na_value <- list(na_value)
      }
    }

    pal_names <- vec_names(pal)

    if (!is_null(pal_names)) {
      # if pal is named, limit the pal by the names first,
      # then limit the values by the pal
      vec_slice(pal, is.na(match(pal_names, limits))) <- na_value
      pal <- vec_set_names(pal, NULL)
      limits <- pal_names
    }
    pal <- vec_c(pal, na_value)
    pal_match <-
      vec_slice(pal, match(as.character(x), limits, nomatch = vec_size(pal)))

    if (!is.na(na_value)) {
      vec_slice(pal_match, is.na(x)) <- na_value
    }
    pal_match
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
      cli::cli_abort(
        "Invalid {.arg breaks} specification. Use {.code NULL}, not {.code NA}.",
        call = self$call
      )
    }

    if (is.waiver(self$breaks)) {
      breaks <- limits
    } else if (is.function(self$breaks)) {
      breaks <- self$breaks(limits)
    } else {
      breaks <- self$breaks
    }

    # Breaks only occur only on values in domain
    breaks <- setNames(as.character(breaks), names(breaks))
    in_domain <- vec_set_intersect(breaks, as.character(limits))
    structure(in_domain, pos = match(in_domain, breaks))
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(),
                              limits = self$get_limits()) {
    breaks <- self$minor_breaks
    # The default is to draw no minor ticks
    if (is.null(breaks %|W|% NULL)) {
      return(NULL)
    }
    if (is.function(breaks)) {
      # Ensure function gets supplied numeric limits and breaks
      if (!is.numeric(b)) {
        b <- self$map(b)
      }
      if (!is.numeric(limits)) {
        limits <- self$map(limits)
        limits <- self$dimension(self$expand, limits)
      }

      # Allow for two types of minor breaks specifications
      break_fun <- fetch_ggproto(self, "minor_breaks")
      arg_names <- fn_fmls_names(break_fun)
      if (length(arg_names) == 1L) {
        breaks <- break_fun(limits)
      } else {
        breaks <- break_fun(limits, b)
      }
    }
    breaks
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    if (self$is_empty()) {
      return(character())
    }

    if (is.null(breaks)) {
      return(NULL)
    }

    labels <- self$labels
    if (is.null(labels)) {
      return(NULL)
    }

    if (identical(labels, NA)) {
      cli::cli_abort(
        "Invalid {.arg labels} specification. Use {.code NULL}, not {.code NA}.",
        call = self$call
      )
    }

    if (is.waiver(labels)) {
      if (!is.null(names(breaks))) {
        labels <- names(breaks)
      } else if (is.numeric(breaks)) {
        # Only format numbers, because on Windows, format messes up encoding
        labels <- format(breaks, justify = "none")
      } else {
        labels <- as.character(breaks)
      }
    } else if (is.function(labels)) {
      labels <- labels(breaks)
    } else if (!is.null(names(labels))) {
      # If labels have names, use them to match with breaks
      map <- match(names(self$labels), breaks, nomatch = 0)
      labels <- replace(breaks, map, labels[map != 0])
    } else if (!is.null(attr(breaks, "pos"))) {
      # Need to ensure that if breaks were dropped, corresponding labels are too
      labels <- labels[attr(breaks, "pos")]
    }

    if (is.expression(labels)) {
      labels <- as.list(labels)
    }
    labels
  },

  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- DiscreteRange$new()
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

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleBinned <- ggproto("ScaleBinned", Scale,
  range = ContinuousRange$new(),
  na.value = NA_real_,
  rescaler = rescale,
  oob = squish,
  n.breaks = NULL,
  nice.breaks = TRUE,
  right = TRUE,
  after.stat = FALSE,
  show.limits = FALSE,

  is_discrete = function() FALSE,

  train = function(self, x) {
    if (!is.numeric(x)) {
      cli::cli_abort(
        "Binned scales only support continuous data.",
        call = self$call
      )
    }

    if (length(x) == 0) {
      return()
    }
    self$range$train(x)
  },

  transform = default_transform,

  map = function(self, x, limits = self$get_limits()) {
    if (self$after.stat) {
      x
    } else {
      breaks <- self$get_breaks(limits)
      breaks <- sort(unique0(c(limits[1], breaks, limits[2])))

      x <- self$rescale(self$oob(x, range = limits), limits)
      breaks <- self$rescale(breaks, limits)

      if (length(breaks) > 1) {
        x_binned <- cut(x, breaks,
          labels = FALSE,
          include.lowest = TRUE,
          right = self$right
        )
        midpoints <- breaks[-1] - diff(breaks) / 2
      } else {
        x_binned  <- 1L
        midpoints <- 0.5
      }

      if (!is.null(self$palette.cache)) {
        pal <- self$palette.cache
      } else {
        pal <- self$palette(midpoints)
        self$palette.cache <- pal
      }

      scaled <- pal[x_binned]
      ifelse(!is.na(scaled), scaled, self$na.value)
    }
  },

  rescale = function(self, x, limits = self$get_limits(), range = limits) {
    self$rescaler(x, from = range)
  },

  dimension = function(self, expand = c(0, 0, 0, 0)) {
    expand_range4(self$get_limits(), expand)
  },

  get_limits = function(self) {
    ggproto_parent(ScaleContinuous, self)$get_limits()
  },

  get_breaks = function(self, limits = self$get_limits()) {
    if (self$is_empty()) return(numeric())

    transformation <- self$get_transformation()

    limits <- transformation$inverse(limits)
    is_rev <- limits[2] < limits[1]
    limits <- sort(limits)

    if (is.null(self$breaks)) {
      return(NULL)
    } else if (identical(self$breaks, NA)) {
      cli::cli_abort(
        "Invalid {.arg breaks} specification. Use {.code NULL}, not {.code NA}.",
        call = self$call
      )
    } else if (is.waiver(self$breaks)) {
      if (self$nice.breaks) {
        if (!is.null(self$n.breaks) && trans_support_nbreaks(transformation)) {
          breaks <- transformation$breaks(limits, n = self$n.breaks)
        } else {
          if (!is.null(self$n.breaks)) {
            cli::cli_warn(
              "Ignoring {.arg n.breaks}. Use a {.cls trans} object that supports setting number of breaks.",
              call = self$call
            )
          }
          breaks <- transformation$breaks(limits)
        }
      } else {
        n.breaks <- self$n.breaks %||% 5 # same default as trans objects
        breaks <- seq(limits[1], limits[2], length.out = n.breaks + 2)
        breaks <- breaks[-c(1, length(breaks))]
      }
      breaks <- oob_discard(breaks, limits)

      # Ensure terminal bins are same width if limits not set
      if (is.null(self$limits)) {
        # Remove calculated breaks if they coincide with limits
        breaks <- breaks[!breaks %in% limits]
        nbreaks <- length(breaks)
        if (nbreaks >= 2) {
          new_limits <- c(
            breaks[1] + (breaks[1] - breaks[2]),
            breaks[nbreaks] + (breaks[nbreaks] - breaks[nbreaks - 1])
          )
          if (breaks[nbreaks] > limits[2]) {
            new_limits[2] <- breaks[nbreaks]
            breaks <- breaks[-nbreaks]
          }
          if (breaks[1] < limits[1]) {
            new_limits[1] <- breaks[1]
            breaks <- breaks[-1]
          }
        } else if (nbreaks == 1) {
          bin_size <- max(breaks[1] - limits[1], limits[2] - breaks[1])
          new_limits <- c(breaks[1] - bin_size, breaks[1] + bin_size)
        } else {
          new_limits <- limits
          if (zero_range(new_limits)) {
            # 0.1 is the same width as the expansion `default_expansion()`
            # gives for 0-width data
            new_limits <- new_limits + c(-0.05, 0.05)
          }
          breaks <- new_limits
        }
        new_limits_trans <- suppressWarnings(transformation$transform(new_limits))
        limits[is.finite(new_limits_trans)] <- new_limits[is.finite(new_limits_trans)]
        if (is_rev) {
          self$limits <- rev(transformation$transform(limits))
        } else {
          self$limits <- transformation$transform(limits)
        }
      }
    } else if (is.function(self$breaks)) {
      if ("n.breaks" %in% names(formals(environment(self$breaks)$f))) {
        n.breaks <- self$n.breaks %||% 5 # same default as trans objects
        breaks <- self$breaks(limits, n.breaks = n.breaks)
      } else {
        if (!is.null(self$n.breaks)) {
          cli::cli_warn(
            "Ignoring {.arg n.breaks}. Use a breaks function that supports setting number of breaks.",
            call = self$call
          )
        }
        breaks <- self$breaks(limits)
      }
    } else {
      breaks <- self$breaks
    }

    self$breaks <- breaks

    transformation$transform(breaks)
  },

  get_breaks_minor = function(...) NULL,

  get_labels = function(self, breaks = self$get_breaks()) {
    if (is.null(breaks)) return(NULL)

    transformation <- self$get_transformation()
    breaks <- transformation$inverse(breaks)

    if (is.null(self$labels)) {
      return(NULL)
    } else if (identical(self$labels, NA)) {
      cli::cli_abort(
        "Invalid {.arg labels} specification. Use {.code NULL}, not {.code NA}.",
        call = self$call
      )
    } else if (is.waiver(self$labels)) {
      labels <- transformation$format(breaks)
    } else if (is.function(self$labels)) {
      labels <- self$labels(breaks)
    } else {
      labels <- self$labels
    }
    if (length(labels) != length(breaks)) {
      cli::cli_abort(
        "{.arg breaks} and {.arg labels} have different lengths.",
        call = self$call
      )
    }
    if (is.expression(labels)) {
      labels <- as.list(labels)
    }
    labels
  },

  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- ContinuousRange$new()
    new
  },

  break_info = function(self, range = NULL) {
    # range
    if (is.null(range)) range <- self$dimension()

    # major breaks
    major <- self$get_breaks(range)

    if (!is.null(self$palette.cache)) {
      pal <- self$palette.cache
    } else {
      pal <- self$palette(length(major) + 1)
    }

    if (self$show.limits) {
      limits <- self$get_limits()
      major <- sort(unique0(c(limits, major)))
    }

    # labels
    labels <- self$get_labels(major)

    list(range = range, labels = labels,
         major = pal, minor = NULL,
         major_source = major, minor_source = NULL)
  }
)

# In place modification of a scale to change the primary axis
scale_flip_position <- function(scale) {
  scale$position <- opposite_position(scale$position)
  invisible()
}

check_transformation <- function(x, transformed, name, arg = NULL, call = NULL) {
  if (!any(is_finite(x) != is_finite(transformed))) {
    return(invisible())
  }
  if (is.null(arg)) {
    end <- "."
  } else {
    end <- paste0(" in {.arg {arg}}.")
  }
  msg <- paste0("{.field {name}} transformation introduced infinite values", end)
  cli::cli_warn(msg, call = call)
}

check_continuous_limits <- function(limits, ...,
                                    arg = caller_arg(limits),
                                    call = caller_env()) {
  if (is.null(limits) || is.function(limits)) {
    return(invisible())
  }
  check_numeric(limits, arg = arg, call = call, allow_na = TRUE)
  check_length(limits, 2L, arg = arg, call = call)
}

trans_support_nbreaks <- function(trans) {
  "n" %in% names(formals(trans$breaks))
}

allow_lambda <- function(x) {
  if (is_formula(x)) as_function(x) else x
}
