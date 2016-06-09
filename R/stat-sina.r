#' @rdname geom_sina
#' @inheritParams stat_identity
#' @inheritParams stat_bin
#' @param bins	Number of bins. Overridden by binwidth. Defaults to 50.
#' @param scale Logical. When set to \code{TRUE} x-coordinate widths across all
#' groups are scaled based on the densiest are in the plot. Default: \code{TRUE}
#' between 0 and 1.
#' @param method Choose the method to spread the samples within the same
#' neighbourhood along the x-axis. Available methods: "density",
#' "neighbourhood" (can be abbreciated, e.g. "d"). See \code{Details}.
#' @param maxwidth Control the maximum width the points can spread into. Values
#' @param adjust Adjusts the bandwidth of the density kernel when
#' @param neighbour_limit If the samples within the same y-axis bin are more
#' than neighbour_limit, the samples's X coordinates will be adjusted.
#' \code{method == "density"} (see \code{\link[stats]{density}}) or the spread
#' of the samples within the same group along the x-axis.
#' @export
stat_sina <-function(mapping = NULL, data = NULL,
                     geom = "point", position = "identity",
                     ...,
                     binwidth = NULL,
                     bins = NULL,
                     scale = TRUE,
                     method = "density",
                     maxwidth = NULL,
                     adjust = 1,
                     neighbour_limit = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  method <- match.arg(method, c("density", "neighbourhood"))

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
      neighbour_limit = neighbour_limit,
      adjust = adjust,
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

  setup_data = function(data, params) {

    if (is.double(data$x) && !has_groups(data) && any(data$x != data$x[1L])) {
      stop(
        "Continuous x aesthetic -- did you forget aes(group=...)?",
        call. = FALSE)
    }

    data
  },

  setup_params = function(data, params) {

    #Limit maxwidth to 0.96 to leave some space between classes
    if (!is.null(params$maxwidth))
      params$maxwidth <- (min(abs(params$maxwidth), .96))
    else
      params$maxwidth <- 0.96

    #scale adjust value
    if (params$method == "neighbourhood"){
      if (!is.null(params$adjust))
        params$adjust <- params$adjust / 100
      else
        params$adjust <- 0.02
    }

    if (is.null(params$binwidth) && is.null(params$bins)) {
      params$bins <- 50
    }

    params
  },


  compute_panel = function(self, data, scales, binwidth = NULL, bins = NULL,
                           scale = TRUE, method = "d", maxwidth = NULL,
                            adjust = 1, neighbour_limit = 1, na.rm = FALSE) {

    if (!is.null(binwidth))
      bins <- bin_breaks_width(scales$y$dimension(), binwidth)
    else
      bins <- bin_breaks_bins(scales$y$dimension(), bins)

    data <- ggproto_parent(Stat, self)$compute_panel(data, scales,
      scale = scale, method = method, maxwidth = maxwidth, adjust = adjust,
      neighbour_limit = neighbour_limit, bins = bins$breaks, na.rm = na.rm)

    #scale all neighbourhoods based on their density relative to the
    #densiest neighbourhood
    if (scale)
      group_scaling_factor <- data$max_neighbours / max(data$max_neighbours)
    else
      group_scaling_factor <- 1

    #translate x coordinates
    data$x <- data$x + data$x_translation * group_scaling_factor

    data
  },

  compute_group = function(data, scales, scale = TRUE, method = "density",
                           maxwidth = maxwidth, adjust = 1, neighbour_limit = 1,
                           bins = NULL, na.rm = FALSE) {

    #initialize x_translation to 0
    data$x_translation <- rep(0, nrow(data))

    #if group has less than 2 points return as is
    if (nrow(data) < 2) {
      data$max_neighbours <- 1
      return(data)
    }

    #per bin sample count
    neighbours <- table(findInterval(data$y, bins))

    #per bin sample density
    if (method == "density") {
      densities <- stats::density(data$y, adjust = adjust)

      #confine the samples in a (-maxwidth/2, -maxwidth/2) area around the class
      #center
      if (max(densities$y) > 0.5 * maxwidth)
        intra_scaling_factor <- 0.5 * maxwidth / max(densities$y)
      else
        intra_scaling_factor <- 1

    } else {
      #if the space required to spread the samples in a neighbourhood exceeds
      #1, create  compress the points
      if (max(neighbours) > 1 / adjust) {
        intra_scaling_factor <- (1 / adjust) / max(neighbours)
      } else
        intra_scaling_factor <- 1
    }

    for (i in names(neighbours)) {

      #examine neighbourhoods with more than 'neighbour_limit' samples
      if (neighbours[i] > neighbour_limit){
        cur_bin <- bins[ as.integer(i) : (as.integer(i) + 1)]

        #find samples in the current bin and translate their X coord.
        points <- findInterval(data$y, cur_bin) == 1

        #compute the border margin for the current bin
        if (method == "density")
          xmax <- mean(densities$y[findInterval(densities$x, cur_bin) == 1])
        else
          xmax <- adjust * neighbours[i] / 2

        #assign the samples uniformely within the specified range
        x_translation <- stats::runif(neighbours[i], - xmax, xmax )

        #scale and store new x coordinates
        data$x_translation[points] <- x_translation * intra_scaling_factor
      }
    }

    #return the max neighbour count per group. Used for group-wise scaling.
    data$max_neighbours <- max(neighbours)
    data
  }
)
