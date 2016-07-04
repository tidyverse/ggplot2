#' @inheritParams layer
#' @inheritParams geom_point
#' @inheritParams stat_density
#' @param scale if "area" (default), all violins have the same area (before trimming
#'   the tails). If "count", areas are scaled proportionally to the number of
#'   observations. If "width", all violins have the same maximum width.
#' @section Computed variables:
#' \describe{
#'   \item{density}{density estimate}
#'   \item{scaled}{density estimate, scaled to maximum of 1}
#'   \item{count}{density * number of points - probably useless for violin plots}
#'   \item{violinwidth}{density scaled for the violin plot, according to area, counts
#'                      or to a constant maximum width}
#'   \item{n}{number of points}
#'   \item{width}{width of violin bounding box}
#' }
#' @seealso \code{\link{geom_violin}} for examples, and \code{\link{stat_density}}
#'   for examples with data along the x axis.
#' @export
#' @rdname geom_violin
stat_ydensity <- function(mapping = NULL, data = NULL,
                          geom = "violin", position = "dodge",
                          ...,
                          bw = "nrd0",
                          adjust = 1,
                          kernel = "gaussian",
                          trim = TRUE,
                          scale = "area",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  scale <- match.arg(scale, c("area", "count", "width"))

  layer(
    data = data,
    mapping = mapping,
    stat = StatYdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      trim = trim,
      scale = scale,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatYdensity <- ggproto("StatYdensity", Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",

  compute_group = function(data, scales, width = NULL, bw = "nrd0", adjust = 1,
                       kernel = "gaussian", trim = TRUE, na.rm = FALSE) {
    if (nrow(data) < 3) return(data.frame())

    if (trim) {
      range <- range(data$y, na.rm = TRUE)
    } else {
      range <- scales$y$dimension()
    }
    dens <- compute_density(data$y, data$w, from = range[1], to = range[2],
      bw = bw, adjust = adjust, kernel = kernel)

    dens$y <- dens$x
    dens$x <- mean(range(data$x))

    # Compute width if x has multiple values
    if (length(unique(data$x)) > 1) {
      width <- diff(range(data$x)) * 0.9
    }
    dens$width <- width

    dens
  },

  compute_panel = function(self, data, scales, width = NULL, bw = "nrd0", adjust = 1,
                           kernel = "gaussian", trim = TRUE, na.rm = FALSE,
                           scale = "area") {
    data <- ggproto_parent(Stat, self)$compute_panel(
      data, scales, width = width, bw = bw, adjust = adjust, kernel = kernel,
      trim = trim, na.rm = na.rm
    )

    # choose how violins are scaled relative to each other
    data$violinwidth <- switch(scale,
      # area : keep the original densities but scale them to a max width of 1
      #        for plotting purposes only
      area = data$density / max(data$density),
      # count: use the original densities scaled to a maximum of 1 (as above)
      #        and then scale them according to the number of observations
      count = data$density / max(data$density) * data$n / max(data$n),
      # width: constant width (density scaled to a maximum of 1)
      width = data$scaled
    )
    data
  }

)
