#' @title build_my_palette
#' 
#' @description build_my_palette is a function that allows to create a palette with a specific number of colors
#' @param name the name of the palette or a vector of colors 
#' @param n the number of colors of the palette
#' @param type the type of palette: 'discrete' or 'continuous'
#' @param direction the direction of the palette: 1 or -1
#'
#' @return a palette with n colors
#'
#' @examples
#' build_my_palette(name = "pal1", n = 4)
#' build_my_palette(name = c("red", "blue", "green"))
build_my_palette = function(name, n, type = c("discrete", "continuous"), direction = 1) {
  if(is.character(name) && length(name) == 1)
  {
    # on this way we can continue to use the palettes from the package RColorBrewer and old code don't need to be changed
    if(name %in% row.names(RColorBrewer::brewer.pal.info))
    {
      palette = RColorBrewer::brewer.pal(n = n, name)
    }else
    {
      stop("name has to be a pallete's name from the package RColorBrewer")
    }
  }else if(is.character(name) && length(name) > 1)
  {
    palette = name
  }else if(is.numeric(name))
  {
    # select the number oƒ the palette
    palette = RColorBrewer::brewer.pal(n = n, row.names(RColorBrewer::brewer.pal.info[name,]))
  }else
  {
    stop("name has to be a character for the name of palette or a vector of colors")
  }
  if (missing(n)) {
    n = length(palette)
  }else if(n > length(palette) & type == "discrete")
  {
    warning(paste("n too large, allowed maximum for palette", 
                  name, "is", length(palette)), 
            "\nReturning the palette you asked for with some colors repeated\n")
  }
  type = match.arg(type)
  if (direction == -1) {
    palette <- rev(palette)
  }
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = rep(palette,n/length(palette) +1)[1:n] # repeat the palette until the length is n 
  )
  structure(out, name = name, class = "palette")
}

#' @title brewer_my_pal
#' 
#' @description brewer_my_pal is a function that allows to create a palette with a specific number of colors
#'
#' @description build_my_palette is a function that allows to create a palette with a specific number of colors
#' @param n the number of colors of the palette
#' @param type the type of palette: 'discrete' or 'continuous'
#' @param direction the direction of the palette: 1 or -1
#'
#' @return a palette with n colors
#' @export
#'
#' @examples
brewer_my_pal = function (palette = "Blue", type = "seq", direction = 1, ...)
{
  force(direction)
  # force(palette)
  # force(type)
  function(n) {
    if (n < 3) {
      pal <- suppressWarnings(build_my_palette(name = palette,  n = n, type = "continuous"))
    }
    else {
      pal <- build_my_palette(name = palette,  n = n, type = "continuous")
    }
    pal <- pal[seq_len(n)]
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}

#' Sequential, diverging and qualitative colour scales from ColorBrewer
#'
#' @description
#' The `brewer` scales provide sequential, diverging and qualitative
#' colour schemes from ColorBrewer. These are particularly well suited to
#' display discrete values on a map. See \url{https://colorbrewer2.org} for
#' more information.
#'
#' @note
#' The `distiller` scales extend `brewer` scales by smoothly
#' interpolating colours from any palette to a continuous scale. You can choose
#' the number of colours that you want in the palette, we recommend you to use 6-7 colours.
#' The `distiller` scales have a default direction = -1. To reverse, use direction = 1.
#' The `fermenter` scales provide binned versions of the `brewer` scales.
#'
#' @details
#' The `brewer` scales were carefully designed and tested on discrete data.
#' They were not designed to be extended to continuous data, but results often
#' look good. Your mileage may vary.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{Diverging}{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'   \item{Qualitative}{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#'   \item{Sequential}{Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges,
#'      OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd}
#' }
#' Modify the palette through the `palette` argument.
#' You can pass a vector of colours in alternative to the name of the palette.
#'
#' @inheritParams scale_colour_hue
#' @inheritParams scale_colour_gradient
#' @inheritParams scales::pal_gradient_n
#' @param palette If a string, will use that named palette. If a vector, will use 
#'   the colours inside to the vector. If a number, will index into
#'   the list of palettes of appropriate `type`. The list of available palettes can found
#'   in the Palettes section.
#' @param ... Other arguments passed on to [discrete_scale()], [continuous_scale()],
#'   or [binned_scale()], for `brewer`, `distiller`, and `fermenter` variants
#'   respectively, to control name, limits, breaks, labels and so forth.
#' @family colour scales
#' @seealso
#' The documentation on [colour aesthetics][aes_colour_fill_alpha].
#' @rdname scale_brewer
#' @export
#' @examples
#' set.seed(596)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) +
#'   geom_point(aes(colour = clarity)))
#' d + scale_colour_brewer()
#'
#' # Change scale label
#' d + scale_colour_brewer("Diamond\nclarity")
#'
#' # Select brewer palette to use, see ?scales::pal_brewer for more details
#' d + scale_colour_brewer(palette = "Greens")
#' d + scale_colour_brewer(palette = "Set1")
#' 
#' # Use your own palette
#' d + scale_colour_brewer(palette = c("#E41A1C", "#377EB8", "#4DAF4A"))
#'
#' \donttest{
#' # scale_fill_brewer works just the same as
#' # scale_colour_brewer but for fill colours
#' p <- ggplot(diamonds, aes(x = price, fill = cut)) +
#'   geom_histogram(position = "dodge", binwidth = 1000)
#' p + scale_fill_brewer()
#' # the order of colour can be reversed
#' p + scale_fill_brewer(direction = -1)
#' # the brewer scales look better on a darker background
#' p +
#'   scale_fill_brewer(direction = -1) +
#'   theme_dark()
#' }
#'
#' # Use distiller variant with continuous data
#' v <- ggplot(faithfuld) +
#'   geom_tile(aes(waiting, eruptions, fill = density))
#' v
#' v + scale_fill_distiller()
#' v + scale_fill_distiller(palette = "Spectral")
#' # the order of colour can be reversed, but with scale_*_distiller(),
#' # the default direction = -1, so to reverse, use direction = 1.
#' v + scale_fill_distiller(palette = "Spectral", direction = 1)
#'
#' # or use blender variants to discretise continuous data
#' v + scale_fill_fermenter()
#'
scale_colour_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "colour") {
  discrete_scale(aesthetics, "brewer", palette = brewer_my_pal(type = type, palette =  palette, direction =  direction), ...)
}

#' @export
#' @rdname scale_brewer
scale_fill_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "fill") {
  discrete_scale(aesthetics, "brewer", palette = brewer_my_pal(type = type, palette =  palette, direction = direction), ...)
}

#' @export
#' @rdname scale_brewer
scale_colour_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "colour", n_colors = 7) {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- rlang::arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a continuous scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  continuous_scale(
    aesthetics,
    "distiller",
    scales::gradient_n_pal(build_my_palette(name = palette,
                                              n = n_colors,
                                              type = "continuous"),
                             values,
                             space),
    na.value = na.value, guide = guide, ...
  )
  # NB: 6-7 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
  # For diverging scales, you need an odd number to make sure the mid-point is in the center
}

#' @export
#' @rdname scale_brewer
scale_fill_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill", n_colors = 7) {
  type <- rlang::arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a continuous scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  continuous_scale(
    aesthetics,
    "distiller",
    scales::gradient_n_pal(build_my_palette(name = palette,
                                              n = n_colors,
                                              type = "continuous"),
                             values,
                             space),
    na.value = na.value, guide = guide, ...
  )
}

#' @export
#' @rdname scale_brewer
scale_colour_fermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "colour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- rlang::arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a binned scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  binned_scale(aesthetics, "fermenter", ggplot2:::binned_pal(brewer_my_pal(type = type, palette = palette, direction = direction)), na.value = na.value, guide = guide, ...)
}

#' @export
#' @rdname scale_brewer
scale_fill_fermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "fill") {
  type <- rlang::arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a binned scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  binned_scale(aesthetics, "fermenter", ggplot2:::binned_pal(brewer_my_pal(type = type, palette = palette, direction = direction)), na.value = na.value, guide = guide, ...)
}
