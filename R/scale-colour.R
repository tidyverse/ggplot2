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
#' @inheritParams continuous_scale
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
#' @seealso
#' The `r link_book("continuous colour scales section", "scales-colour#sec-colour-continuous")`
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
scale_colour_continuous <- function(..., aesthetics = "colour",
                                    guide = "colourbar", na.value = "grey50",
                                    type = getOption("ggplot2.continuous.colour")) {

  if (!is.null(type)) {
    scale <- scale_backward_compatibility(
      ..., guide = guide, na.value = na.value, scale = type,
      aesthetic = "colour", type = "continuous"
    )
    return(scale)
  }

  continuous_scale(
    aesthetics, palette = NULL, guide = guide, na.value = na.value,
    ...
  )
}

#' @rdname scale_colour_continuous
#' @export
scale_fill_continuous <- function(..., aesthetics = "fill", guide = "colourbar",
                                  na.value = "grey50",
                                  type = getOption("ggplot2.continuous.fill")) {

  if (!is.null(type)) {
    scale <- scale_backward_compatibility(
      ..., guide = guide, na.value = na.value, scale = type,
      aesthetic = "fill", type = "continuous"
    )
    return(scale)
  }

  continuous_scale(
    aesthetics, palette = NULL, guide = guide, na.value = na.value,
    ...
  )
}

#' @export
#' @rdname scale_colour_continuous
scale_colour_binned <- function(..., aesthetics = "colour", guide = "coloursteps",
                                na.value = "grey50",
                                type = getOption("ggplot2.binned.colour")) {
  if (!is.null(type)) {
    scale <- scale_backward_compatibility(
      ..., guide = guide, na.value = na.value, scale = type,
      aesthetic = "colour", type = "binned"
    )
    return(scale)
  }

  binned_scale(
    aesthetics, palette = NULL, guide = guide, na.value = na.value,
    ...
  )
}

#' @export
#' @rdname scale_colour_continuous
scale_fill_binned <- function(..., aesthetics = "fill", guide = "coloursteps",
                              na.value = "grey50",
                              type = getOption("ggplot2.binned.fill")) {
  if (!is.null(type)) {
    scale <- scale_backward_compatibility(
      ..., guide = guide, na.value = na.value, scale = type,
      aesthetic = "fill", type = "binned"
    )
    return(scale)
  }

  binned_scale(
    aesthetics, palette = NULL, guide = guide, na.value = na.value,
    ...
  )
}

# helper function to make sure that the provided scale is of the correct
# type (i.e., is continuous and works with the provided aesthetic)
check_scale_type <- function(scale, name, aesthetic, scale_is_discrete = FALSE, call = caller_env()) {
  if (!is.ggproto(scale) || !is.scale(scale)) {
    cli::cli_abort(c(
      "The {.arg type} argument must return a continuous scale for the {.field {aesthetic}} aesthetic.",
      "x" = "The provided object is not a scale function."
    ), call = call)
  }
  if (!isTRUE(aesthetic %in% scale$aesthetics)) {
    cli::cli_abort(c(
      "The {.arg type} argument must return a continuous scale for the {.field {aesthetic}} aesthetic.",
      "x" = "The provided scale works with the following aesthetics: {.field {scale$aesthetics}}."
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

# helper function for backwards compatibility through setting defaults
# scales through `options()` instead of `theme()`.
scale_backward_compatibility <- function(..., scale, aesthetic, type) {
  aesthetic <- standardise_aes_names(aesthetic[1])

  args <- list2(...)
  args$call <- args$call %||% caller_call() %||% current_call()

  if (type == "binned") {
    fallback <- getOption(
      paste("ggplot2", type, aesthetic, sep = "."),
      default = "gradient"
    )
    if (is.function(fallback)) {
      fallback <- "gradient"
    }
    scale <- scale %||% fallback
  }

  if (is_bare_string(scale)) {
    if (scale == "continuous") {
      scale <- "gradient"
    }
    if (scale == "discrete") {
      scale <- "hue"
    }
    if (scale == "viridis") {
      scale <- switch(
        type, discrete = "viridis_d", binned = "viridis_b", "viridis_c"
      )
    }

    candidates <- paste("scale", aesthetic, scale, sep = "_")
    for (candi in candidates) {
      f <- find_global(candi, env = caller_env(), mode = "function")
      if (!is.null(f)) {
        scale <- f
        break
      }
    }
  }

  if (!is.function(scale) && type == "discrete") {
    args$type <- scale
    scale <- switch(
      aesthetic,
      colour = scale_colour_qualitative,
      fill   = scale_fill_qualitative
    )
  }

  if (is.function(scale)) {
    if (!any(c("...", "call") %in% fn_fmls_names(scale))) {
      args$call <- NULL
    }
    if (!"..." %in% fn_fmls_names(scale)) {
      args <- args[intersect(names(args), fn_fmls_names(scale))]
    }
    scale <- check_scale_type(
      exec(scale, !!!args),
      paste("scale", aesthetic, type, sep = "_"),
      aesthetic,
      scale_is_discrete = type == "discrete"
    )
    return(scale)
  }

  cli::cli_abort("Unknown scale type: {.val {scale}}")
}
