#' @rdname geom_sina
#' @inheritParams stat_identity
#' @param binwidth Binning factor. The range of the values in \code{aes(y)}
#' are binned in windows of length \code{(max(y) - min(y)) * y_fraction}.
#' Samples within the same bin belong to the same "neighbourhood".
#' @param scale logical. When set to \code{TRUE} x-coordinate widths across all
#' groups are scaled based on the densiest are in the plot. Default: \code{TRUE}
#' @param neighbour_limit if the samples within the same y-axis bin are more
#' than neighbour_limit, the samples's X coordinates will be adjusted.
#' @param method choose the method to spread the samples within the same
#' neighbourhood along the x-axis. Available methods: "density",
#' "neighbourhood" (can be abbreciated, e.g. "d"). See \code{Details}.
#' @param adjust adjusts the bandwidth of the density kernel when
#' \code{method == "density"} (see \code{\link[stats]{density}}) or the spread
#' of the samples within the same group along the x-axis.
#' @export
stat_sina <-function(mapping = NULL, data = NULL,
                     geom = "point", position = "identity",
                     ...,
                     binwidth = 0.02,
                     scale = TRUE,
                     neighbour_limit = 1,
                     method = "density",
                     adjust = 1,
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
      scale = scale,
      neighbour_limit = neighbour_limit,
      method = method,
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

    print(params)
    #scale adjust value
    if (params$method == "neighbourhood")
      params$adjust <- params$adjust / 100

    params
  },

  #compute_group = function()
  compute_panel = function(data, scales, binwidth = 0.02, scale = TRUE,
                           neighbour_limit = 1, method = "d", adjust = 1) {

    ###Initialise variables

    #neighbour counts
    neighbours <- list()
    max_neighbours <- 0

    #method == "density"
    if (method == "density") {
      max_density <- 0
      densities <- list()
    }

    #bin the y-axis
    bins <- bin_y(data$y, binwidth)

    # ------------------------------------------------------------------------ #

    #Compute per class density and per bin sample count
    for (j in levels(factor(data$group))) {

      #extract samples per group and store them in a data.frame
      keep <- data$group == j

      if (sum(keep) < 2)
        next

      #per bin sample count
      neighbours[[j]] <- table(findInterval(data$y[keep], bins))

      #find the densiest neighbourhood in the current group and compare it
      #with the global max
      tmp_max_neighbours <- max(neighbours[[j]])

      if (tmp_max_neighbours > max_neighbours)
        max_neighbours <- tmp_max_neighbours

      #per bin sample density
      if (method == "density")
        densities[[j]] <- stats::density(data$y[keep], adjust = adjust)

    }

    # ------------------------------------------------------------------------ #

    for (j in levels(factor(data$group))) {

      keep <- data$group == j

      if (sum(keep) < 2 ) next

      #confine the samples in a (-0.5, 0.5) area around the class center
      if (method == "density") {
        if (max(densities[[j]]$y) > 0.48)
          global_scaling_factor <- 0.48 / max(densities[[j]]$y)
        else
          global_scaling_factor <- 1

      } else {
        #if the space required to spread the samples in a neighbourhood exceeds
        #1, create  compress the points
        if (max(neighbours[[j]]) > 1 / adjust) {
          global_scaling_factor <- (1 / adjust) / max(neighbours[[j]])
        } else
          global_scaling_factor <- 1
      }

      #scale all neighbourhoods based on their density relative to the
      #densiest neighbourhood
      if (scale == TRUE)
        group_scaling_factor <- max(neighbours[[j]]) / max_neighbours
      else
        group_scaling_factor <- 1

      for (i in names(neighbours[[j]])) {

        #examine neighbourhoods with more than 'neighbour_limit' samples
        if (neighbours[[j]][i] > neighbour_limit){
          cur_bin <- bins[ as.integer(i) : (as.integer(i) + 1)]

          #find samples in the current bin and translate their X coord.
          points <- findInterval(data$y[keep], cur_bin) == 1

          #compute the border margin for the current bin
          if (method == "density")
            xmax <- mean(densities[[j]]$y[findInterval(densities[[j]]$x,
                                                       cur_bin) == 1])
          else
            xmax <- adjust * neighbours[[j]][i] / 2

          #assign the samples uniformely within the specified range
          x_translation <- stats::runif(neighbours[[j]][i], - xmax, xmax )

          #scale and store new x coordinates
          data$x[keep][points] <- data$x[keep][points] +
            (x_translation * global_scaling_factor * group_scaling_factor)
        }
      }
    }

    data
  }
)

bin_y <- function(data, bw) {
  #get y value range
  ymin <- min(data)
  ymax <- max(data)

  #window width
  window_size <- (ymax - ymin) * (bw + 1e-8)

  bins <- c()
  for (i in 0:ceiling(1 / bw)) {
    bins <- c(bins, ymin + i * window_size)
  }

  bins
}
