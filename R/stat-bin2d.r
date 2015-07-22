#' @param bins numeric vector giving number of bins in both vertical and
#'   horizontal directions. Set to 30 by default.
#' @param drop if \code{TRUE} removes all cells with 0 counts.
#' @export
#' @rdname geom_bin2d
stat_bin2d <- function (mapping = NULL, data = NULL, geom = "rect",
  position = "identity", bins = 30, drop = TRUE, show_guide = NA,
  inherit.aes = TRUE, ...) {
  Layer$new(
    data = data,
    mapping = mapping,
    stat = StatBin2d,
    geom = geom,
    position = position,
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    stat_params = list(
      bins = bins,
      drop = drop
    ),
    params = list(...)
  )
}

StatBin2d <- proto2(
  class = "StatBin2d",
  inherit = Stat,
  members = list(
    default_aes = aes(fill = ..count..),
    required_aes = c("x", "y"),

    calculate = function(self, data, scales, binwidth = NULL, bins = 30,
                          breaks = NULL, origin = NULL, drop = TRUE, ...) {
      range <- list(
        x = scale_dimension(scales$x, c(0, 0)),
        y = scale_dimension(scales$y, c(0, 0))
      )

      # Determine origin, if omitted
      if (is.null(origin)) {
        origin <- c(NA, NA)
      } else {
        stopifnot(is.numeric(origin))
        stopifnot(length(origin) == 2)
      }
      originf <- function(x) if (is.integer(x)) -0.5 else min(x, na.rm = TRUE)
      if (is.na(origin[1])) origin[1] <- originf(data$x)
      if (is.na(origin[2])) origin[2] <- originf(data$y)

      # Determine binwidth, if omitted
      if (is.null(binwidth)) {
        binwidth <- c(NA, NA)
        if (is.integer(data$x)) {
          binwidth[1] <- 1
        } else {
          binwidth[1] <- diff(range$x) / bins
        }
        if (is.integer(data$y)) {
          binwidth[2] <- 1
        } else {
          binwidth[2] <- diff(range$y) / bins
        }
      }
      stopifnot(is.numeric(binwidth))
      stopifnot(length(binwidth) == 2)

      # Determine breaks, if omitted
      if (is.null(breaks)) {
        breaks <- list(
          seq(origin[1], max(range$x) + binwidth[1], binwidth[1]),
          seq(origin[2], max(range$y) + binwidth[2], binwidth[2])
        )
      } else {
        stopifnot(is.list(breaks))
        stopifnot(length(breaks) == 2)
        stopifnot(all(sapply(breaks, is.numeric)))
      }
      names(breaks) <- c("x", "y")

      xbin <- cut(data$x, sort(breaks$x), include.lowest = TRUE)
      ybin <- cut(data$y, sort(breaks$y), include.lowest = TRUE)

      if (is.null(data$weight)) data$weight <- 1

      counts <- as.data.frame(
        xtabs(weight ~ xbin + ybin, data), responseName = "count")
      if (drop) counts <- subset(counts, count > 0)

      within(counts,{
        xint <- as.numeric(xbin)
        xmin <- breaks$x[xint]
        xmax <- breaks$x[xint + 1]

        yint <- as.numeric(ybin)
        ymin <- breaks$y[yint]
        ymax <- breaks$y[yint + 1]

        density <- count / sum(count, na.rm = TRUE)
      })
    }
  )
)
