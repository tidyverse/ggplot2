#' Annotation: log tick marks
#'
#' This annotation adds log tick marks with diminishing spacing.
#' These tick marks probably make sense only for base 10.
#'
#' @param base the base of the log (default 10)
#' @param sides a string that controls which sides of the plot the log ticks appear on.
#'   It can be set to a string containing any of \code{"trbl"}, for top, right,
#'   bottom, and left.
#' @param short a \code{\link[grid]{unit}} object specifying the length of the
#'   short tick marks
#' @param mid a \code{\link[grid]{unit}} object specifying the length of the
#'   middle tick marks. In base 10, these are the "5" ticks.
#' @param long a \code{\link[grid]{unit}} object specifying the length of the
#'   long tick marks. In base 10, these are the "1" (or "10") ticks.
#' @param scaled is the data already log-scaled? This should be \code{TRUE}
#'   (default) when the data is already transformed with \code{log10()} or when
#'   using \code{scale_y_log10}. It should be \code{FALSE} when using
#'   \code{coord_trans(y = "log10")}.
#' @param colour Colour of the tick marks.
#' @param size Thickness of tick marks, in mm.
#' @param linetype Linetype of tick marks (\code{solid}, \code{dashed}, etc.)
#' @param alpha The transparency of the tick marks.
#' @param color An alias for \code{colour}.
#' @param ... Other parameters passed on to the layer
#'
#' @export
#' @seealso \code{\link{scale_y_continuous}}, \code{\link{scale_y_log10}} for log scale
#'   transformations.
#' @seealso \code{\link{coord_trans}} for log coordinate transformations.
#'
#' @examples
#' # Make a log-log plot (without log ticks)
#' a <- ggplot(msleep, aes(bodywt, brainwt)) +
#'  geom_point(na.rm = TRUE) +
#'  scale_x_log10(
#'    breaks = scales::trans_breaks("log10", function(x) 10^x),
#'    labels = scales::trans_format("log10", scales::math_format(10^.x))
#'  ) +
#'  scale_y_log10(
#'    breaks = scales::trans_breaks("log10", function(x) 10^x),
#'    labels = scales::trans_format("log10", scales::math_format(10^.x))
#'  ) +
#'  theme_bw()
#'
#' a + annotation_logticks()                # Default: log ticks on bottom and left
#' a + annotation_logticks(sides = "lr")    # Log ticks for y, on left and right
#' a + annotation_logticks(sides = "trbl")  # All four sides
#'
#' # Hide the minor grid lines because they don't align with the ticks
#' a + annotation_logticks(sides = "trbl") + theme(panel.grid.minor = element_blank())
#'
#' # Another way to get the same results as 'a' above: log-transform the data before
#' # plotting it. Also hide the minor grid lines.
#' b <- ggplot(msleep, aes(log10(bodywt), log10(brainwt))) +
#'  geom_point(na.rm = TRUE) +
#'  scale_x_continuous(name = "body", labels = scales::math_format(10^.x)) +
#'  scale_y_continuous(name = "brain", labels = scales::math_format(10^.x)) +
#'  theme_bw() + theme(panel.grid.minor = element_blank())
#'
#' b + annotation_logticks()
#'
#' # Using a coordinate transform requires scaled = FALSE
#' t <- ggplot(msleep, aes(bodywt, brainwt)) +
#'   geom_point() +
#'   coord_trans(x = "log10", y = "log10") +
#'   theme_bw()
#' t + annotation_logticks(scaled = FALSE)
#'
#' # Change the length of the ticks
#' a + annotation_logticks(
#'   short = unit(.5,"mm"),
#'   mid = unit(3,"mm"),
#'   long = unit(4,"mm")
#' )
annotation_logticks <- function(base = 10, sides = "bl", scaled = TRUE,
      short = unit(0.1, "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"),
      colour = "black", size = 0.5, linetype = 1, alpha = 1, color = NULL, ...)
{
  if (!is.null(color))
    colour <- color

  layer(
    data = data.frame(x = NA),
    mapping = NULL,
    stat = StatIdentity,
    geom = GeomLogticks,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      base = base,
      sides = sides,
      scaled = scaled,
      short = short,
      mid = mid,
      long = long,
      colour = colour,
      size = size,
      linetype = linetype,
      alpha = alpha,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLogticks <- ggproto("GeomLogticks", Geom,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_scales, coord, base = 10, sides = "bl",
    scaled = TRUE, short = unit(0.1, "cm"), mid = unit(0.2, "cm"),
    long = unit(0.3, "cm"))
  {
    ticks <- list()

    # Convert these units to numbers so that they can be put in data frames
    short <- convertUnit(short, "cm", valueOnly = TRUE)
    mid   <- convertUnit(mid,   "cm", valueOnly = TRUE)
    long  <- convertUnit(long,  "cm", valueOnly = TRUE)


    if (grepl("[b|t]", sides)) {

      # Get positions of x tick marks
      xticks <- calc_logticks(
        base = base,
        minpow = floor(panel_scales$x.range[1]),
        maxpow = ceiling(panel_scales$x.range[2]),
        start = 0,
        shortend = short,
        midend = mid,
        longend = long
      )

      if (scaled)
        xticks$value <- log(xticks$value, base)

      names(xticks)[names(xticks) == "value"] <- "x"   # Rename to 'x' for coordinates$transform
      xticks <- coord$transform(xticks, panel_scales)

      # Make the grobs
      if (grepl("b", sides)) {
        ticks$x_b <- with(data, segmentsGrob(
          x0 = unit(xticks$x, "native"), x1 = unit(xticks$x, "native"),
          y0 = unit(xticks$start, "cm"), y1 = unit(xticks$end, "cm"),
          gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
        ))
      }
      if (grepl("t", sides)) {
        ticks$x_t <- with(data, segmentsGrob(
          x0 = unit(xticks$x, "native"), x1 = unit(xticks$x, "native"),
          y0 = unit(1, "npc") - unit(xticks$start, "cm"), y1 = unit(1, "npc") - unit(xticks$end, "cm"),
          gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
        ))
      }
    }


    if (grepl("[l|r]", sides)) {
      yticks <- calc_logticks(
        base = base,
        minpow = floor(panel_scales$y.range[1]),
        maxpow = ceiling(panel_scales$y.range[2]),
        start = 0,
        shortend = short,
        midend = mid,
        longend = long
      )

      if (scaled)
        yticks$value <- log(yticks$value, base)

      names(yticks)[names(yticks) == "value"] <- "y"   # Rename to 'y' for coordinates$transform
      yticks <- coord$transform(yticks, panel_scales)

      # Make the grobs
      if (grepl("l", sides)) {
        ticks$y_l <- with(data, segmentsGrob(
          y0 = unit(yticks$y, "native"), y1 = unit(yticks$y, "native"),
          x0 = unit(yticks$start, "cm"), x1 = unit(yticks$end, "cm"),
          gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
        ))
      }
      if (grepl("r", sides)) {
        ticks$y_r <- with(data, segmentsGrob(
          y0 = unit(yticks$y, "native"), y1 = unit(yticks$y, "native"),
          x0 = unit(1, "npc") - unit(yticks$start, "cm"), x1 = unit(1, "npc") - unit(yticks$end, "cm"),
          gp = gpar(col = alpha(colour, alpha), lty = linetype, lwd = size * .pt)
        ))
      }
    }

    gTree(children = do.call("gList", ticks))
  },

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = 1)
)


# Calculate the position of log tick marks
# Returns data frame with:
# - value: the position of the log tick on the data axis, for example 1, 2, ..., 9, 10, 20, ...
# - start: on the other axis, start position of the line (usually 0)
# - end: on the other axis, end position of the line (for example, .1, .2, or .3)
calc_logticks <- function(base = 10, ticks_per_base = base - 1,
    minpow = 0, maxpow = minpow + 1, start = 0, shortend = .1, midend = .2, longend = .3) {

  # Number of blocks of tick marks
  reps <- maxpow - minpow

  # For base 10: 1, 2, 3, ..., 7, 8, 9, 1, 2, ...
  ticknums  <- rep(seq(1, base - 1, length.out = ticks_per_base), reps)

  # For base 10: 1, 1, 1, ..., 1, 1, 1, 2, 2, ... (for example)
  powers <- rep(seq(minpow, maxpow - 1), each = ticks_per_base)

  ticks  <- ticknums * base ^ powers
  ticks  <- c(ticks, base ^ maxpow)  # Add the last tick mark

  # Set all of the ticks short
  tickend <- rep(shortend, length(ticks))

  # Get the position within each cycle, 0, 1, 2, ..., 8, 0, 1, 2. ...
  cycleIdx <- ticknums - 1

  # Set the "major" ticks long
  tickend[cycleIdx == 0] <- longend

  # Where to place the longer tick marks that are between each base
  # For base 10, this will be at each 5
  longtick_after_base <- floor(ticks_per_base/2)
  tickend[ cycleIdx == longtick_after_base ] <- midend

  tickdf <- data.frame(value = ticks, start = start, end = tickend)

  return(tickdf)
}
