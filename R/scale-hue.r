#' Evenly spaced colours for discrete data
#'
#' Maps each level to an evenly spaced hue on the colour wheel.
#' It does not generate colour-blind safe palettes.
#'
#' @param na.value Colour to use for missing values
#' @inheritDotParams discrete_scale -aesthetics
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @inheritParams scales::hue_pal
#' @rdname scale_hue
#' @export
#' @family colour scales
#' @examples
#' \donttest{
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
#' ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = miss))
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = miss)) +
#'   scale_colour_hue(na.value = "black")
#' }
scale_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                             direction = 1, na.value = "grey50", aesthetics = "colour") {
  discrete_scale(aesthetics, "hue", hue_pal(h, c, l, h.start, direction),
    na.value = na.value, ...)
}

#' @rdname scale_hue
#' @export
scale_fill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                           direction = 1, na.value = "grey50", aesthetics = "fill") {
  discrete_scale(aesthetics, "hue", hue_pal(h, c, l, h.start, direction),
    na.value = na.value, ...)
}


#' Discrete colour scales
#'
#' Colour scales for discrete data default to the values of the `ggplot2.discrete.fill`
#' and `ggplot2.discrete.colour` options. By default these scales attempt to use
#' a colour-blind safe (or a custom) palette, but if the number of levels is
#' large, they fallback to [scale_fill_hue()]/[scale_colour_hue()].
#'
#' @param ... Additional parameters passed on to the scale type,
#' @param type One of the following:
#'   * A character vector of color codes. The codes are used for a 'manual' color
#'   scale as long as the number of codes exceeds the number of data levels
#'   (if there are more levels than codes, [scale_colour_hue()]/[scale_fill_hue()]
#'   are used to construct the default scale).
#'   * A list of character vectors of color codes. The minimum length vector that exceeds the
#'   number of data levels is chosen for the color scaling. This is useful if you
#'   want to change the color palette based on the number of levels.
#'   * A function that returns a discrete colour/fill scale (e.g., [scale_fill_hue()],
#'   [scale_fill_brewer()], etc).
#' @export
#' @rdname
#' @examples
#' # Template function for creating densities grouped by a variable
#' cty_by_var <- function(var) {
#'   ggplot(mpg, aes(cty, colour = factor({{var}}), fill = factor({{var}}))) +
#'     geom_density(alpha = 0.2)
#' }
#' # The default color scale for three levels
#' cty_by_var(class)
#'
#' # Define custom palettes for when there are 1-2, 3, or 4-6 levels
#' opts <- options(
#'   ggplot2.discrete.fill = list(
#'     c("skyblue", "orange"),
#'     RColorBrewer::brewer.pal(3, "Set2"),
#'     RColorBrewer::brewer.pal(6, "Accent")
#'   )
#' )
#' cty_by_var(year)
#' cty_by_var(drv)
#' cty_by_var(fl)
#' cty_by_var(class)
#' options(opts)
#'
scale_colour_discrete <- function(..., type = getOption("ggplot2.discrete.colour", getOption("ggplot2.discrete.fill"))) {
  if (is.function(type)) {
    type(...)
  } else {
    scale_colour_qualitative(..., codes = type)
  }
}

#' @rdname scale_colour_discrete
#' @export
scale_fill_discrete <- function(..., type = getOption("ggplot2.discrete.fill", getOption("ggplot2.discrete.colour"))) {
  if (is.function(type)) {
    type(...)
  } else {
    scale_fill_qualitative(..., codes = type)
  }
}

scale_colour_qualitative <- function(..., codes = NULL, h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                     direction = 1, na.value = "grey50", aesthetics = "colour") {
  discrete_scale(
    aesthetics, "qualitative", qualitative_pal(codes, h, c, l, h.start, direction),
    na.value = na.value, ...
  )
}

scale_fill_qualitative <- function(..., codes = NULL, h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                   direction = 1, na.value = "grey50", aesthetics = "fill") {
  discrete_scale(
    aesthetics, "qualitative", qualitative_pal(codes, h, c, l, h.start, direction),
    na.value = na.value, ...
  )
}

qualitative_pal <- function(codes, h, c, l, h.start, direction) {
  function(n) {
    if (!length(codes))  {
      return(scales::hue_pal(h, c, l, h.start, direction)(n))
    }
    codes_list <- if (!is.list(codes)) list(codes) else codes
    if (!all(vapply(codes_list, is.character, logical(1)))) {
      stop("codes must be a character vector or a list of character vectors", call. = FALSE)
    }
    codes_lengths <- vapply(codes_list, length, integer(1))
    # If there are more levels than color codes default to hue_pal()
    if (max(codes_lengths) < n) {
      return(scales::hue_pal(h, c, l, h.start, direction)(n))
    }
    # Use the minimum length vector that exceeds the number of levels (n)
    codes_list <- codes_list[order(codes_lengths)]
    i <- 1
    while (length(codes_list[[i]]) < n) {
      i <- i + 1
    }
    codes_list[[i]][seq_len(n)]
  }
}
