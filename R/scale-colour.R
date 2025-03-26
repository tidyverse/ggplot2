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
#' @param palette One of the following:
#'   * `NULL` for the default palette stored in the theme.
#'   * a character vector of colours.
#'   * a single string naming a palette.
#'   * a palette function that when called with a numeric vector with values
#'     between 0 and 1 returns the corresponding output values.
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
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
#'   geom_point()
#'
#' # You can use the scale to give a palette directly
#' p + scale_colour_continuous(palette = c("#FEE0D2", "#FC9272", "#DE2D26"))
#'
#' # The default colours are encoded into the theme
#' p + theme(palette.colour.continuous = c("#DEEBF7", "#9ECAE1", "#3182BD"))
#'
#' # You can globally set default colour palette via the theme
#' old <- update_theme(palette.colour.continuous = c("#E5F5E0", "#A1D99B", "#31A354"))
#'
#' # Plot now shows new global default
#' p
#'
#' # The default binned colour scale uses the continuous palette
#' p + scale_colour_binned() +
#'   theme(palette.colour.continuous = c("#EFEDF5", "#BCBDDC", "#756BB1"))
#'
#' # Restoring the previous theme
#' theme_set(old)
#' @export
scale_colour_continuous <- function(..., palette = NULL, aesthetics = "colour",
                                    guide = "colourbar", na.value = "grey50",
                                    type = getOption("ggplot2.continuous.colour")) {

  has_old_args <- any(names(enexprs(...)) %in% c("low", "high"))

  if (has_old_args || (!is.null(type) && is.null(palette))) {
    scale <- scale_backward_compatibility(
      ..., guide = guide, na.value = na.value, scale = type,
      aesthetic = "colour", type = "continuous"
    )
    return(scale)
  }
  palette <- if (!is.null(palette)) as_continuous_pal(palette)
  continuous_scale(
    aesthetics, palette = palette, guide = guide, na.value = na.value,
    ...
  )
}

#' @rdname scale_colour_continuous
#' @export
scale_fill_continuous <- function(..., palette = NULL, aesthetics = "fill", guide = "colourbar",
                                  na.value = "grey50",
                                  type = getOption("ggplot2.continuous.fill")) {

  has_old_args <- any(names(enexprs(...)) %in% c("low", "high"))

  if (has_old_args || (!is.null(type) && is.null(palette))) {
    scale <- scale_backward_compatibility(
      ..., guide = guide, na.value = na.value, scale = type,
      aesthetic = "fill", type = "continuous"
    )
    return(scale)
  }
  palette <- if (!is.null(palette)) as_continuous_pal(palette)
  continuous_scale(
    aesthetics, palette = palette, guide = guide, na.value = na.value,
    ...
  )
}

#' @export
#' @rdname scale_colour_continuous
scale_colour_binned <- function(..., palette = NULL, aesthetics = "colour", guide = "coloursteps",
                                na.value = "grey50",
                                type = getOption("ggplot2.binned.colour")) {

  has_old_args <- any(names(enexprs(...)) %in% c("low", "high"))

  if (has_old_args || (!is.null(type) && is.null(palette))) {
    scale <- scale_backward_compatibility(
      ..., guide = guide, na.value = na.value, scale = type,
      aesthetic = "colour", type = "binned"
    )
    return(scale)
  }
  palette <- if (!is.null(palette)) pal_binned(as_discrete_pal(palette))
  binned_scale(
    aesthetics, palette = palette, guide = guide, na.value = na.value,
    ...
  )
}

#' @export
#' @rdname scale_colour_continuous
scale_fill_binned <- function(..., palette = NULL, aesthetics = "fill", guide = "coloursteps",
                              na.value = "grey50",
                              type = getOption("ggplot2.binned.fill")) {
  has_old_args <- any(names(enexprs(...)) %in% c("low", "high"))

  if (has_old_args || (!is.null(type) && is.null(palette))) {
    scale <- scale_backward_compatibility(
      ..., guide = guide, na.value = na.value, scale = type,
      aesthetic = "fill", type = "binned"
    )
    return(scale)
  }
  palette <- if (!is.null(palette)) pal_binned(as_discrete_pal(palette))
  binned_scale(
    aesthetics, palette = palette, guide = guide, na.value = na.value,
    ...
  )
}

#' Discrete colour scales
#'
#' The default discrete colour scale. Defaults to [scale_fill_hue()]/[scale_fill_brewer()]
#' unless `type` (which defaults to the `ggplot2.discrete.fill`/`ggplot2.discrete.colour` options)
#' is specified.
#'
#' @param palette One of the following:
#'   * `NULL` for the default palette stored in the theme.
#'   * a character vector of colours.
#'   * a single string naming a palette.
#'   * a palette function that when called with a single integer argument (the
#'     number of levels in the scale) returns the values that they should take.
#' @param ... Additional parameters passed on to the scale type,
#' @inheritParams discrete_scale
#' @param type One of the following:
#'   * A character vector of color codes. The codes are used for a 'manual' color
#'   scale as long as the number of codes exceeds the number of data levels
#'   (if there are more levels than codes, [scale_colour_hue()]/[scale_fill_hue()]
#'   are used to construct the default scale). If this is a named vector, then the color values
#'   will be matched to levels based on the names of the vectors. Data values that
#'   don't match will be set as `na.value`.
#'   * A list of character vectors of color codes. The minimum length vector that exceeds the
#'   number of data levels is chosen for the color scaling. This is useful if you
#'   want to change the color palette based on the number of levels.
#'   * A function that returns a discrete colour/fill scale (e.g., [scale_fill_hue()],
#'   [scale_fill_brewer()], etc).
#' @export
#' @seealso
#' The `r link_book("discrete colour scales section", "scales-colour#sec-colour-discrete")`
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point()
#'
#' # You can use the scale to give a palette directly
#' p + scale_colour_discrete(palette = scales::pal_brewer(palette = "Dark2"))
#'
#' # The default colours are encoded into the theme
#' p + theme(palette.colour.discrete = scales::pal_grey())
#'
#' # You can globally set default colour palette via the theme
#' old <- update_theme(palette.colour.discrete = scales::pal_viridis())
#'
#' # Plot now shows new global default
#' p
#'
#' # Restoring the previous theme
#' theme_set(old)
scale_colour_discrete <- function(..., palette = NULL, aesthetics = "colour", na.value = "grey50",
                                  type = getOption("ggplot2.discrete.colour")) {

  has_old_args <- any(names(enexprs(...)) %in% c("h", "c", "l", "h.start", "direction"))

  if (has_old_args || (!is.null(type) && is.null(palette))) {
    scale <- scale_backward_compatibility(
      ..., na.value = na.value, scale = type,
      aesthetic = "colour", type = "discrete"
    )
    return(scale)
  }
  palette <- if (!is.null(palette)) as_discrete_pal(palette)
  discrete_scale(
    aesthetics, palette = palette, na.value = na.value,
    ...
  )
}

#' @rdname scale_colour_discrete
#' @export
scale_fill_discrete <- function(..., palette = NULL, aesthetics = "fill", na.value = "grey50",
                                type = getOption("ggplot2.discrete.fill")) {

  has_old_args <- any(names(enexprs(...)) %in% c("h", "c", "l", "h.start", "direction"))

  if (has_old_args || (!is.null(type) && is.null(palette))) {
    scale <- scale_backward_compatibility(
      ..., na.value = na.value, scale = type,
      aesthetic = "fill", type = "discrete"
    )
    return(scale)
  }
  palette <- if (!is.null(palette)) as_discrete_pal(palette)
  discrete_scale(
    aesthetics, palette = palette, na.value = na.value,
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
  invisible()
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
    scale <- exec(scale, !!!args)
    check_scale_type(
      scale,
      paste("scale", aesthetic, type, sep = "_"),
      aesthetic,
      scale_is_discrete = type == "discrete"
    )
    return(scale)
  }

  cli::cli_abort("Unknown scale type: {.val {scale}}")
}
