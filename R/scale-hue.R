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
  type_list <- type
  if (!is.list(type_list)) {
    type_list <- list(type_list)
  }
  if (!all(vapply(type_list, is.character, logical(1)))) {
    stop_input_type(type, "a character vector or list of character vectors")
  }
  type_lengths <- lengths(type_list)
  function(n) {
    # If there are more levels than color codes default to pal_hue()
    if (max(type_lengths) < n) {
      return(scales::pal_hue(h, c, l, h.start, direction)(n))
    }
    # Use the minimum length vector that exceeds the number of levels (n)
    i <- which(type_lengths >= n)
    i <- i[which.min(type_lengths[i])]
    type_list[[i]]
  }
}
