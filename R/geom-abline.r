#' @include stat-.r
NULL

#' Reference lines: horizontal, vertical, and diagonal
#'
#' These geoms add reference lines (sometimes called rules) to a plot, either
#' horizontal, vertical, or diagonal (specified by slope and intercept).
#' These are useful for annotating plots.
#'
#' These geoms act slightly differently from other geoms. You can supply the
#' parameters in two ways: either as arguments to the layer function,
#' or via aesthetics. If you use arguments, e.g.
#' `geom_abline(intercept = 0, slope = 1)`, then behind the scenes
#' the geom makes a new data frame containing just the data you've supplied.
#' That means that the lines will be the same in all facets; if you want them
#' to vary across facets, construct the data frame yourself and use aesthetics.
#'
#' Unlike most other geoms, these geoms do not inherit aesthetics from the plot
#' default, because they do not understand x and y aesthetics which are
#' commonly set in the plot. They also do not affect the x and y scales.
#'
#' @section Aesthetics:
#' These geoms are drawn using with [geom_line()] so support the
#' same aesthetics: `alpha`, `colour`, `linetype` and
#' `linewidth`. They also each have aesthetics that control the position of
#' the line:
#'
#'   - `geom_vline()`: `xintercept`
#'   - `geom_hline()`: `yintercept`
#'   - `geom_abline()`: `slope` and `intercept`
#'
#' @seealso See [geom_segment()] for a more general approach to
#'   adding straight line segments to a plot.
#' @inheritParams layer
#' @inheritParams geom_point
#' @param mapping Set of aesthetic mappings created by [aes()] or [aes_()].
#' @param xintercept,yintercept,slope,intercept Parameters that control the
#'   position of the line. If these are set, `data`, `mapping` and
#'   `show.legend` are overridden.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Fixed values
#' p + geom_vline(xintercept = 5)
#' p + geom_vline(xintercept = 1:5)
#' p + geom_hline(yintercept = 20)
#'
#' p + geom_abline() # Can't see it - outside the range of the data
#' p + geom_abline(intercept = 20)
#'
#' # Calculate slope and intercept of line of best fit
#' coef(lm(mpg ~ wt, data = mtcars))
#' p + geom_abline(intercept = 37, slope = -5)
#' # But this is easier to do with geom_smooth:
#' p + geom_smooth(method = "lm", se = FALSE)
#'
#' # To show different lines in different facets, use aesthetics
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   facet_wrap(~ cyl)
#'
#' mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
#' p + geom_hline(aes(yintercept = wt), mean_wt)
#'
#' # You can also control other aesthetics
#' ggplot(mtcars, aes(mpg, wt, colour = wt)) +
#'   geom_point() +
#'   geom_hline(aes(yintercept = wt, colour = wt), mean_wt) +
#'   facet_wrap(~ cyl)
geom_abline <- function(mapping = NULL, data = NULL,
                        ...,
                        slope,
                        intercept,
                        na.rm = FALSE,
                        show.legend = NA) {

  # If nothing set, default to y = x
  if (is.null(mapping) && missing(slope) && missing(intercept)) {
    slope <- 1
    intercept <- 0
  }

  # Act like an annotation
  if (!missing(slope) || !missing(intercept)) {

    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_abline}: Ignoring {.arg mapping} because {.arg slope} and/or {.arg intercept} were provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_abline}: Ignoring {.arg data} because {.arg slope} and/or {.arg intercept} were provided.")
    }

    if (missing(slope)) slope <- 1
    if (missing(intercept)) intercept <- 0
    n_slopes <- max(length(slope), length(intercept))

    data <- data_frame0(
      intercept = intercept,
      slope = slope,
      .size = n_slopes
    )
    mapping <- aes(intercept = intercept, slope = slope)
    show.legend <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomAbline,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAbline <- ggproto("GeomAbline", Geom,
  draw_panel = function(data, panel_params, coord, lineend = "butt") {
    ranges <- coord$backtransform_range(panel_params)

    if (coord$clip == "on" && coord$is_linear()) {
      # Ensure the line extends well outside the panel to avoid visible line
      # ending for thick lines
      ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
    }

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- ranges$x[1] * data$slope + data$intercept
    data$yend <- ranges$x[2] * data$slope + data$intercept

    GeomSegment$draw_panel(unique0(data), panel_params, coord, lineend = lineend)
  },

  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
  required_aes = c("slope", "intercept"),

  draw_key = draw_key_abline,

  rename_size = TRUE
)
