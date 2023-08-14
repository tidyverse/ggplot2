#' Continuous and binned colour scales
#'
#' The scales `scale_colour_continuous()` and `scale_fill_continuous()` are
#' the default colour scales ggplot2 uses when continuous data values are
#' mapped onto the `colour` or `fill` aesthetics, respectively. The scales
#' `scale_colour_binned()` and `scale_fill_binned()` are equivalent scale
#' functions that assign discrete color bins to the continuous values
#' instead of using a continuous color spectrum.
#'
#' All these colour scales use the [options()] mechanism to determine
#' default settings. Continuous colour scales default to the values of the
#' `ggplot2.continuous.colour` and `ggplot2.continuous.fill` options, and
#' binned colour scales default to the values of the `ggplot2.binned.colour`
#' and `ggplot2.binned.fill` options. These option values default to
#' `"gradient"`, which means that the scale functions actually used are
#' [scale_colour_gradient()]/[scale_fill_gradient()] for continuous scales and
#' [scale_colour_steps()]/[scale_fill_steps()] for binned scales.
#' Alternative option values are `"viridis"` or a different scale function.
#' See description of the `type` argument for details.
#'
#' Note that the binned colour scales will use the settings of
#' `ggplot2.continuous.colour` and `ggplot2.continuous.fill` as fallback,
#' respectively, if `ggplot2.binned.colour` or `ggplot2.binned.fill` are
#' not set.
#'
#' These scale functions are meant to provide simple defaults. If
#' you want to manually set the colors of a scale, consider using
#' [scale_colour_gradient()] or [scale_colour_steps()].
#'
#' @param ... Additional parameters passed on to the scale type
#' @param type One of the following:
#'   * "gradient" (the default)
#'   * "viridis"
#'   * A function that returns a continuous colour scale.
#' @seealso [scale_colour_gradient()], [scale_colour_viridis_c()],
#'   [scale_colour_steps()], [scale_colour_viridis_b()], [scale_fill_gradient()],
#'   [scale_fill_viridis_c()], [scale_fill_steps()], and [scale_fill_viridis_b()]
#'
#'   The documentation on [colour aesthetics][aes_colour_fill_alpha].
#' @family colour scales
#' @rdname scale_colour_continuous
#' @section Color Blindness:
#' Many color palettes derived from RGB combinations (like the "rainbow" color
#' palette) are not suitable to support all viewers, especially those with
#' color vision deficiencies. Using `viridis` type, which is perceptually
#' uniform in both colour and black-and-white display is an easy option to
#' ensure good perceptive properties of your visualizations.
#' The colorspace package offers functionalities
#' - to generate color palettes with good perceptive properties,
#' - to analyse a given color palette, like emulating color blindness,
#' - and to modify a given color palette for better perceptivity.
#'
#' For more information on color vision deficiencies and suitable color choices
#' see the [paper on the colorspace package](https://arxiv.org/abs/1903.06490)
#' and references therein.
#' @examples
#' v <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#' geom_tile()
#' v
#'
#' v + scale_fill_continuous(type = "gradient")
#' v + scale_fill_continuous(type = "viridis")
#'
#' # The above are equivalent to
#' v + scale_fill_gradient()
#' v + scale_fill_viridis_c()
#'
#' # To make a binned version of this plot
#' v + scale_fill_binned(type = "viridis")
#'
#' # Set a different default scale using the options
#' # mechanism
#' tmp <- getOption("ggplot2.continuous.fill") # store current setting
#' options(ggplot2.continuous.fill = scale_fill_distiller)
#' v
#' options(ggplot2.continuous.fill = tmp) # restore previous setting
#' @export
scale_colour_continuous <- function(...,
                                    type = getOption("ggplot2.continuous.colour")) {
  type <- type %||% "gradient"

  if (is.function(type)) {
    check_scale_type(type(...), "scale_colour_continuous", "colour")
  } else if (identical(type, "gradient")) {
    scale_colour_gradient(...)
  } else if (identical(type, "viridis")) {
    scale_colour_viridis_c(...)
  } else {
    cli::cli_abort(c(
      "Unknown scale type: {.val {type}}",
      "i" = "Use either {.val gradient} or {.val viridis}"
    ))
  }
}

#' @rdname scale_colour_continuous
#' @export
scale_fill_continuous <- function(...,
                                  type = getOption("ggplot2.continuous.fill")) {
  type <- type %||% "gradient"

  if (is.function(type)) {
    check_scale_type(type(...), "scale_fill_continuous", "fill")
  } else if (identical(type, "gradient")) {
    scale_fill_gradient(...)
  } else if (identical(type, "viridis")) {
    scale_fill_viridis_c(...)
  } else {
    cli::cli_abort(c(
      "Unknown scale type: {.val {type}}",
      "i" = "Use either {.val gradient} or {.val viridis}"
    ))
  }
}

#' @export
#' @rdname scale_colour_continuous
scale_colour_binned <- function(...,
                                type = getOption("ggplot2.binned.colour")) {
  if (is.function(type)) {
    check_scale_type(type(...), "scale_colour_binned", "colour")
  } else {
    type_fallback <- getOption("ggplot2.continuous.colour", default = "gradient")
    # don't use fallback from scale_colour_continuous() if it is
    # a function, since that would change the type of the color
    # scale from binned to continuous
    if (is.function(type_fallback)) {
      type_fallback <- "gradient"
    }
    type <- type %||% type_fallback

    if (identical(type, "gradient")) {
      scale_colour_steps(...)
    } else if (identical(type, "viridis")) {
      scale_colour_viridis_b(...)
    } else {
      cli::cli_abort(c(
        "Unknown scale type: {.val {type}}",
        "i" = "Use either {.val gradient} or {.val viridis}"
      ))
    }
  }
}

#' @export
#' @rdname scale_colour_continuous
scale_fill_binned <- function(...,
                              type = getOption("ggplot2.binned.fill")) {
  if (is.function(type)) {
    check_scale_type(type(...), "scale_fill_binned", "fill")
  } else {
    type_fallback <- getOption("ggplot2.continuous.fill", default = "gradient")
    # don't use fallback from scale_colour_continuous() if it is
    # a function, since that would change the type of the color
    # scale from binned to continuous
    if (is.function(type_fallback)) {
      type_fallback <- "gradient"
    }
    type <- type %||% type_fallback

    if (identical(type, "gradient")) {
      scale_fill_steps(...)
    } else if (identical(type, "viridis")) {
      scale_fill_viridis_b(...)
    } else {
      cli::cli_abort(c(
        "Unknown scale type: {.val {type}}",
        "i" = "Use either {.val gradient} or {.val viridis}"
      ))
    }
  }
}


# helper function to make sure that the provided scale is of the correct
# type (i.e., is continuous and works with the provided aesthetic)
check_scale_type <- function(scale, name, aesthetic, scale_is_discrete = FALSE, call = caller_env()) {
  if (!is.ggproto(scale) || !inherits(scale, "Scale")) {
    cli::cli_abort(c(
      "The {.arg type} argument must return a continuous scale for the {.field {aesthetic}} aesthetic.",
      "x" = "The provided object is not a scale function."
    ), call = call)
  }
  if (!isTRUE(aesthetic %in% scale$aesthetics)) {
    cli::cli_abort(c(
      "The {.arg type} argument must return a continuous scale for the {.field {aesthetic}} aesthetic.",
      "x" = "The provided scale works with the following aesthetics: {.field {scale$aesthetics}}"
    ), call = call)
  }
  if (isTRUE(scale$is_discrete()) != scale_is_discrete) {
    scale_types <- c("continuous", "discrete")
    if (scale_is_discrete) {
      scale_types <- rev(scale_types)
    }
    cli::cli_abort(c(
      "The {.arg type} argument must return a {scale_types[1]} scale for the {.field {aesthetic}} aesthetic.",
      "x" = "The provided scale is {scale_types[2]}."
    ), call = call)
  }

  scale
}
