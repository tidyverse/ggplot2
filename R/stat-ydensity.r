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
                          outliers = NULL,
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
      outliers= outliers,
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
                       kernel = "gaussian", trim = TRUE, outliers = NULL, na.rm = FALSE) {
    if (nrow(data) < 3) return(data.frame())

    # Determine any outliers. They will not be used to compute violin
    if (!is.null(outliers)) {
        stats <- as.numeric(stats::quantile(data$y, c(0.25, 0.75)))
        iqr <- diff(stats)
        outliers <- data$y < (stats[1] - outliers * iqr) | data$y > (stats[2] + outliers * iqr)
    } else {
      outliers <- rep(FALSE, nrow(data)) # no outliers
    }

    data.no.outliers <- data[!outliers,]

    if (trim) {
      range <- range(data.no.outliers$y, na.rm = TRUE)
    } else {
      range <- scales$y$dimension() # TODO - what about this case when there are outliers?
    }

    dens <- compute_density(data.no.outliers$y, data.no.outliers$w,
      from = range[1], to = range[2],
      bw = bw, adjust = adjust, kernel = kernel)

    dens$y <- dens$x
    dens$x <- mean(range(data$x))

    # add outliers
    dens$is.outlier <- FALSE
    if (any(outliers)) {
      dens <- plyr::rbind.fill(dens, data.frame(x = dens$x[1], y = data$y[outliers], is.outlier = TRUE))
    }
    
    # Compute width if x has multiple values
    if (length(unique(data$x)) > 1) {
      width <- diff(range(data$x)) * 0.9
    }
    dens$width <- width

    dens
  },

  compute_panel = function(self, data, scales, width = NULL, bw = "nrd0", adjust = 1,
                           kernel = "gaussian", trim = TRUE, outliers = NULL, na.rm = FALSE,
                           scale = "area") {
    data <- ggproto_parent(Stat, self)$compute_panel(
      data, scales, width = width, bw = bw, adjust = adjust, kernel = kernel,
      trim = trim, outliers = outliers, na.rm = na.rm
    )

    data.inliers <- subset(data, !is.outlier)
    
    # choose how violins are scaled relative to each other
    data.inliers$violinwidth <- switch(scale,
      # area : keep the original densities but scale them to a max width of 1
      #        for plotting purposes only
      area = data.inliers$density / max(data.inliers$density),
      # count: use the original densities scaled to a maximum of 1 (as above)
      #        and then scale them according to the number of observations
      count = data.inliers$density / max(data.inliers$density) * data.inliers$n / max(data.inliers$n),
      # width: constant width (density scaled to a maximum of 1)
      width = data.inliers$scaled
    )
    plyr::rbind.fill(data.inliers, subset(data, is.outlier))
  }

)
