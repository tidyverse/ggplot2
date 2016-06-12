#' @rdname geom_sina
#' @inheritParams stat_identity
#' @inheritParams stat_bin
#' @param bins	Number of bins. Overridden by binwidth. Defaults to 50.
#' @param scale Logical. When set to \code{TRUE} x-coordinate widths across all
#' groups are scaled based on the densiest area in the plot.
#' Default: \code{TRUE}
#' @param method Choose the method to spread the samples within the same
#' bin along the x-axis. Available methods: "density", "counts" (can be
#' abbreviated, e.g. "d"). See \code{Details}.
#' @param maxwidth Control the maximum width the points can spread into. Values
#' between 0 and 1.
#' @param adjust Adjusts the bandwidth of the density kernel when
#' \code{method == "density"} (see \code{\link[stats]{density}}).
#' @param bin_limit If the samples within the same y-axis bin are more
#' than \code{bin_limit}, the samples's X coordinates will be adjusted.
#' @section Computed variables:
#' \describe{
#'   \item{bin_counts}{sample counts per bin per group}
#'   \item{scaled}{adjusted x-coordinates}
#' }
#' @export
stat_sina <-function(mapping = NULL, data = NULL,
                     geom = "sina", position = "identity",
                     ...,
                     binwidth = NULL,
                     bins = NULL,
                     scale = TRUE,
                     method = "density",
                     maxwidth = NULL,
                     adjust = 1,
                     bin_limit = 1,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  method <- match.arg(method, c("density", "counts"))

  layer(
    data = data,
    mapping = mapping,
    stat = StatSina,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      scale = scale,
      method = method,
      maxwidth = maxwidth,
      adjust = adjust,
      bin_limit = bin_limit,
      na.rm = na.rm,
      ...
      )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSina <- ggproto("StatSina", Stat,

  required_aes = c("x", "y"),

  default_aes = aes(xend = ..scaled..),

  setup_data = function(data, params) {

    if (is.double(data$x) && !has_groups(data) && any(data$x != data$x[1L])) {
      stop(
        "Continuous x aesthetic -- did you forget aes(group=...)?",
        call. = FALSE)
    }

    data
  },

  setup_params = function(data, params) {

    #Limit maxwidth to 0.96 to leave some space between groups
    if (!is.null(params$maxwidth))
      params$maxwidth <- (min(abs(params$maxwidth), .96))
    else
      params$maxwidth <- 0.96

    if (is.null(params$binwidth) && is.null(params$bins)) {
      params$bins <- 50
    }

    params
  },

  compute_panel = function(self, data, scales, binwidth = NULL, bins = NULL,
                           scale = TRUE, method = "density", maxwidth = NULL,
                           adjust = 1, bin_limit = 1, na.rm = FALSE) {

    if (!is.null(binwidth))
      bins <- bin_breaks_width(scales$y$dimension(), binwidth)
    else
      bins <- bin_breaks_bins(scales$y$dimension(), bins)

    data <- ggproto_parent(Stat, self)$compute_panel(data, scales,
      scale = scale, method = method, maxwidth = maxwidth, adjust = adjust,
      bin_limit = bin_limit, bins = bins$breaks, na.rm = na.rm)

    #scale all bins based on their density relative to the densiest bin
    if (scale) {
      group_scaling_factor <-
        ddply(data, .(group), mutate,
              group_max = max(bin_counts))$group_max / max(data$bin_counts)
    } else {
      group_scaling_factor <- 1
    }

    data$scaled <- data$x + data$x_translation * group_scaling_factor
    data$x_translation <- NULL
    data
  },

  compute_group = function(data, scales, scale = TRUE, method = "density",
                           maxwidth = NULL, adjust = 1, bin_limit = 1,
                           bins = NULL, na.rm = FALSE) {

    #initialize x_translation and bin_counts to 0
    data$x_translation <- data$bin_counts <- rep(0, nrow(data))

    #if group has less than 2 points return as is
    if (nrow(data) < 2) {
      data$max_bin_counts <- 1
      return(data)
    }

    #per bin sample count
    bin_counts <- table(findInterval(data$y, bins))

    #per bin sample density
    if (method == "density") {
      densities <- stats::density(data$y, adjust = adjust)

      #confine the samples in a (-maxwidth/2, -maxwidth/2) area around the
      #group's center
      if (max(densities$y) > 0.5 * maxwidth)
        intra_scaling_factor <- 0.5 * maxwidth / max(densities$y)
      else
        intra_scaling_factor <- 1

    } else {
      #allow up to 50 samples in a bin without scaling
      if (max(bin_counts) > 50 * maxwidth) {
        intra_scaling_factor <- 50 * maxwidth / max(bin_counts)
      } else
        intra_scaling_factor <- 1
    }

    for (i in names(bin_counts)) {

      #examine bins with more than 'bin_limit' samples
      if (bin_counts[i] > bin_limit){
        cur_bin <- bins[ as.integer(i) : (as.integer(i) + 1)]

        #find samples in the current bin and translate their X coord.
        points <- findInterval(data$y, cur_bin) == 1

        #compute the border margin for the current bin.
        if (method == "density")
          xmax <- mean(densities$y[findInterval(densities$x, cur_bin) == 1])
        else
          xmax <- bin_counts[i] / 100

        #assign the samples uniformely within the specified range
        x_translation <- stats::runif(bin_counts[i], - xmax, xmax)

        #scale and store new x coordinates
        data$x_translation[points] <- x_translation * intra_scaling_factor
        #store bin counts. Used for group-wise scaling.
        data$bin_counts[points] <- bin_counts[i]
      }
    }

    data
  }
)
