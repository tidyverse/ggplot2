#' Binned gradient colour scales
#'
#' `scale_*_steps` creates a two colour binned gradient (low-high),
#' `scale_*_steps2` creates a diverging binned colour gradient (low-mid-high),
#' and `scale_*_stepsn` creates a n-colour binned gradient. These scales are
#' binned variants of the [gradient scale][scale_colour_gradient] family and
#' works in the same way.
#'
#' Default colours are generated with \pkg{munsell} and
#' `mnsl(c("2.5PB 2/4", "2.5PB 7/10"))`. Generally, for continuous
#' colour scales you want to keep hue constant, but vary chroma and
#' luminance. The \pkg{munsell} package makes this easy to do using the
#' Munsell colour system.
#'
#' @inheritParams scale_colour_gradient
#' @inheritDotParams binned_scale -aesthetics -scale_name -palette -na.value -guide -rescaler
#'
#' @seealso
#' [scales::pal_seq_gradient()] for details on underlying palette,
#' [scale_colour_gradient()] for continuous scales without binning.
#'
#' The documentation on [colour aesthetics][aes_colour_fill_alpha].
#'
#' The `r link_book("binned colour scales section", "scales-colour#sec-binned-colour")`
#' @family colour scales
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   x = runif(100),
#'   y = runif(100),
#'   z1 = rnorm(100)
#' )
#'
#' # Use scale_colour_steps for a standard binned gradient
#' ggplot(df, aes(x, y)) +
#'   geom_point(aes(colour = z1)) +
#'   scale_colour_steps()
#'
#' # Get a divergent binned scale with the *2 variant
#' ggplot(df, aes(x, y)) +
#'   geom_point(aes(colour = z1)) +
#'   scale_colour_steps2()
#'
#' # Define your own colour ramp to extract binned colours from
#' ggplot(df, aes(x, y)) +
#'   geom_point(aes(colour = z1)) +
#'   scale_colour_stepsn(colours = terrain.colors(10))
#' @rdname scale_steps
scale_colour_steps <- function(name = waiver(), ..., low = "#132B43",
                               high = "#56B1F7", space = "Lab",
                               na.value = "grey50", guide = "coloursteps",
                               aesthetics = "colour") {
  binned_scale(
    aesthetics, name = name,
    palette = pal_seq_gradient(low, high, space),
    na.value = na.value, guide = guide, ...
  )
}
#' @rdname scale_steps
#' @export
scale_colour_steps2 <- function(name = waiver(), ..., low = muted("red"),
                                mid = "white", high = muted("blue"),
                                midpoint = 0, space = "Lab", na.value = "grey50",
                                transform = "identity", guide = "coloursteps",
                                aesthetics = "colour") {
  binned_scale(
    aesthetics, name = name,
    palette = div_gradient_pal(low, mid, high, space),
    na.value = na.value, transform = transform, guide = guide,
    rescaler = mid_rescaler(mid = midpoint, transform = transform),
    ...
  )
}
#' @rdname scale_steps
#' @export
scale_colour_stepsn <- function(name = waiver(), ..., colours, values = NULL,
                                space = "Lab", na.value = "grey50",
                                guide = "coloursteps", aesthetics = "colour",
                                colors) {
  colours <- if (missing(colours)) colors else colours
  binned_scale(
    aesthetics, name = name,
    palette = pal_gradient_n(colours, values, space),
    na.value = na.value, guide = guide,
    ...
  )
}
#' @rdname scale_steps
#' @export
scale_fill_steps <- function(name = waiver(), ..., low = "#132B43",
                             high = "#56B1F7", space = "Lab",
                             na.value = "grey50", guide = "coloursteps",
                             aesthetics = "fill") {
  binned_scale(
    aesthetics, name = name,
    palette = pal_seq_gradient(low, high, space),
    na.value = na.value, guide = guide,
    ...
  )
}
#' @rdname scale_steps
#' @export
scale_fill_steps2 <- function(name = waiver(), ..., low = muted("red"),
                              mid = "white", high = muted("blue"),
                              midpoint = 0, space = "Lab", na.value = "grey50",
                              transform = "identity", guide = "coloursteps",
                              aesthetics = "fill") {
  binned_scale(
    aesthetics, name = name,
    palette = div_gradient_pal(low, mid, high, space),
    na.value = na.value, transform = transform, guide = guide,
    rescaler = mid_rescaler(mid = midpoint, transform = transform),
    ...
  )
}
#' @rdname scale_steps
#' @export
scale_fill_stepsn <- function(name = waiver(), ..., colours, values = NULL,
                              space = "Lab", na.value = "grey50",
                              guide = "coloursteps", aesthetics = "fill",
                              colors) {
  colours <- if (missing(colours)) colors else colours
  binned_scale(
    aesthetics, name = name,
    palette = pal_gradient_n(colours, values, space),
    na.value = na.value, guide = guide, ...
  )
}
