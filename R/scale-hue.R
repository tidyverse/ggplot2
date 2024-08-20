#' Evenly spaced colours for discrete data
#'
#' Maps each level to an evenly spaced hue on the colour wheel.
#' It does not generate colour-blind safe palettes.
#'
#' @param na.value Colour to use for missing values
#' @inheritDotParams discrete_scale -aesthetics -expand -position -scale_name -palette
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @inheritParams scales::pal_hue
#' @inheritParams discrete_scale
#' @rdname scale_hue
#' @export
#' @family colour scales
#' @seealso
#' The documentation on [colour aesthetics][aes_colour_fill_alpha].
#'
#' The `r link_book("hue and grey scales section", "scales-colour#hue-and-grey-scales")`
#' @examples
#' \donttest{
#' set.seed(596)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) + geom_point(aes(colour = clarity)))
#'
#' # Change scale label
#' d + scale_colour_hue()
#' d + scale_colour_hue("clarity")
#' d + scale_colour_hue(expression(clarity[beta]))
#'
#' # Adjust luminosity and chroma
#' d + scale_colour_hue(l = 40, c = 30)
#' d + scale_colour_hue(l = 70, c = 30)
#' d + scale_colour_hue(l = 70, c = 150)
#' d + scale_colour_hue(l = 80, c = 150)
#'
#' # Change range of hues used
#' d + scale_colour_hue(h = c(0, 90))
#' d + scale_colour_hue(h = c(90, 180))
#' d + scale_colour_hue(h = c(180, 270))
#' d + scale_colour_hue(h = c(270, 360))
#'
#' # Vary opacity
#' # (only works with pdf, quartz and cairo devices)
#' d <- ggplot(dsamp, aes(carat, price, colour = clarity))
#' d + geom_point(alpha = 0.9)
#' d + geom_point(alpha = 0.5)
#' d + geom_point(alpha = 0.2)
#'
#' # Colour of missing values is controlled with na.value:
#' miss <- factor(sample(c(NA, 1:5), nrow(mtcars), replace = TRUE))
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = miss))
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = miss)) +
#'   scale_colour_hue(na.value = "black")
#' }
scale_colour_hue <- function(name = waiver(), ..., h = c(0, 360) + 15, c = 100,
                             l = 65, h.start = 0, direction = 1,
                             na.value = "grey50", aesthetics = "colour") {
  discrete_scale(
    aesthetics, name = name,
    palette = pal_hue(h, c, l, h.start, direction),
    na.value = na.value, ...
  )
}

#' @rdname scale_hue
#' @export
scale_fill_hue <- function(name = waiver(), ..., h = c(0, 360) + 15, c = 100,
                           l = 65, h.start = 0, direction = 1,
                           na.value = "grey50", aesthetics = "fill") {
  discrete_scale(
    aesthetics, name = name,
    palette = pal_hue(h, c, l, h.start, direction),
    na.value = na.value, ...
  )
}


#' Discrete colour scales
#'
#' The default discrete colour scale. Defaults to [scale_fill_hue()]/[scale_fill_brewer()]
#' unless `type` (which defaults to the `ggplot2.discrete.fill`/`ggplot2.discrete.colour` options)
#' is specified.
#'
#' @param ... Additional parameters passed on to the scale type,
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
#' # Template function for creating densities grouped by a variable
#' cty_by_var <- function(var) {
#'   ggplot(mpg, aes(cty, colour = factor({{var}}), fill = factor({{var}}))) +
#'     geom_density(alpha = 0.2)
#' }
#'
#' # The default, scale_fill_hue(), is not colour-blind safe
#' cty_by_var(class)
#'
#' # (Temporarily) set the default to Okabe-Ito (which is colour-blind safe)
#' okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#' withr::with_options(
#'   list(ggplot2.discrete.fill = okabe),
#'   print(cty_by_var(class))
#' )
#'
#' # Define a collection of palettes to alter the default based on number of levels to encode
#' discrete_palettes <- list(
#'   c("skyblue", "orange"),
#'   RColorBrewer::brewer.pal(3, "Set2"),
#'   RColorBrewer::brewer.pal(6, "Accent")
#' )
#' withr::with_options(
#'   list(ggplot2.discrete.fill = discrete_palettes), {
#'   # 1st palette is used when there 1-2 levels (e.g., year)
#'   print(cty_by_var(year))
#'   # 2nd palette is used when there are 3 levels
#'   print(cty_by_var(drv))
#'   # 3rd palette is used when there are 4-6 levels
#'   print(cty_by_var(fl))
#' })
#'
scale_colour_discrete <- function(..., type = getOption("ggplot2.discrete.colour")) {
  # TODO: eventually `type` should default to a set of colour-blind safe color codes (e.g. Okabe-Ito)
  type <- type %||% scale_colour_hue
  args <- list2(...)
  args$call <- args$call %||% current_call()

  if (is.function(type)) {
    if (!any(c("...", "call") %in% fn_fmls_names(type))) {
      args$call <- NULL
    }
    check_scale_type(
      exec(type, !!!args),
      "scale_colour_discrete",
      "colour",
      scale_is_discrete = TRUE
    )
  } else {
    exec(scale_colour_qualitative, !!!args, type = type)
  }
}

#' @rdname scale_colour_discrete
#' @export
scale_fill_discrete <- function(..., type = getOption("ggplot2.discrete.fill")) {
  # TODO: eventually `type` should default to a set of colour-blind safe color codes (e.g. Okabe-Ito)
  type <- type %||% scale_fill_hue
  args <- list2(...)
  args$call <- args$call %||% current_call()

  if (is.function(type)) {
    if (!any(c("...", "call") %in% fn_fmls_names(type))) {
      args$call <- NULL
    }
    check_scale_type(
      exec(type, !!!args),
      "scale_fill_discrete",
      "fill",
      scale_is_discrete = TRUE
    )
  } else {
    exec(scale_fill_qualitative, !!!args, type = type)
  }
}

scale_colour_qualitative <- function(name = waiver(), ..., type = NULL,
                                     h = c(0, 360) + 15, c = 100, l = 65,
                                     h.start = 0, direction = 1,
                                     na.value = "grey50",
                                     aesthetics = "colour") {
  discrete_scale(
    aesthetics, name = name,
    palette = pal_qualitative(type, h, c, l, h.start, direction),
    na.value = na.value, ...
  )
}

scale_fill_qualitative <- function(name = waiver(), ..., type = NULL,
                                   h = c(0, 360) + 15, c = 100, l = 65,
                                   h.start = 0, direction = 1,
                                   na.value = "grey50", aesthetics = "fill") {
  discrete_scale(
    aesthetics, name = name,
    palette = pal_qualitative(type, h, c, l, h.start, direction),
    na.value = na.value, ...
  )
}

#' Given set(s) of colour codes (i.e., type), find the smallest set that can support n levels
#' @param type a character vector or a list of character vectors
#' @noRd
pal_qualitative <- function(type, h, c, l, h.start, direction) {
  function(n) {
    type_list <- if (!is.list(type)) list(type) else type
    if (!all(vapply(type_list, is.character, logical(1)))) {
      cli::cli_abort("{.arg type} must be a character vector or a list of character vectors.")
    }
    type_lengths <- lengths(type_list)
    # If there are more levels than color codes default to pal_hue()
    if (max(type_lengths) < n) {
      return(scales::pal_hue(h, c, l, h.start, direction)(n))
    }
    # Use the minimum length vector that exceeds the number of levels (n)
    type_list <- type_list[order(type_lengths)]
    i <- 1
    while (length(type_list[[i]]) < n) {
      i <- i + 1
    }
    type_list[[i]][seq_len(n)]
  }
}
