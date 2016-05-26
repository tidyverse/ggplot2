#' Point density based jitter.
#'
#' @family position adjustments
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
#' @examples
#' # See geom_sina for examples.
position_sina <- function(binwidth = 0.2,
                          scale = TRUE,
                          neighbour_limit = 1,
                          method = c("density", "neighbourhood"),
                          adjust = 1) {
  ggproto(NULL, PositionSina,
          binwidth = binwidth,
          scale = scale,
          neighbour_limit = neighbour_limit,
          method = match.arg(method),
          adjust = adjust
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionSina <- ggproto("PositionSina", Position,

  required_aes = c("x", "y"),

  setup_params = function(self, data) {

    if (is.double(data$x) && any(data$x != data$x[1L])) {
      stop(
        "Continuous x aesthetic -- maybe try aes(factor(x))?",
        call. = FALSE)
    }

    list(
      binwidth = self$binwidth,
      scale = self$scale,
      neighbour_limit = self$neighbour_limit,
      method = self$method,
      adjust = self$adjust
    )
  },

  setup_data = function(self, data, params) {
    data = remove_missing(data, FALSE,
      c("x", "y", "ymin", "ymax", "xmin", "xmax"))
    data
  },

  compute_layer = function(data, params, panel) {
    trans_x <- function(x) sina(x, data, binwidth = params$binwidth,
                                 scale = params$scale,
                                 neighbour_limit = params$neighbour_limit,
                                 method = params$method,
                                 adjust = params$adjust)
    trans_y <- NULL

    transform_position(data, trans_x, trans_y)
  }
)

sina <- function(x, data, binwidth, scale, neighbour_limit, method, adjust) {


  ### Initialise variables
  #x-axis transpose vector
  trans_x <- list()

  #assing groups as factors of x
  groups <- factor(x)

  #neighbour counts
  neighbours <- list()
  max_neighbours <- 0

  #method == "density"
  if (method == "density") {
    max_density <- 0
    densities <- list()
  } else {
    #scale adjust value
    adjust <- adjust / 5
  }

  #keep an index of the original data order
  idx <- 1:length(x)

  #bin the y-axis
  bins <- bin_y(data$y, binwidth)

  # -------------------------------------------------------------------------- #

  #Compute per class density, per bin sample count and initialize the trans_x
  #transpose data.frame
  for (j in levels(groups)) {

    #extract samples per group and store them in a data.frame
    keep <- groups == j
    trans_x[[j]] <- data.frame( "x" = x[keep],
                                "y" = data$y[keep],
                                "idx" = idx[keep])

    if (sum(keep) < 2)
      next

    #per bin sample count
    neighbours[[j]] <- table(findInterval(trans_x[[j]]$y, bins))

    #find the densiest neighbourhood in the current group and compare it
    #with the global max
    tmp_max_neighbours <- max(neighbours[[j]])

    if (tmp_max_neighbours > max_neighbours)
      max_neighbours <- tmp_max_neighbours

    if (method == "density") {

      #per bin sample density
      densities[[j]] <- stats::density(trans_x[[j]]$y, adjust = adjust)

      #find the highest density value
      tmp_max_density <- max(densities[[j]]$y)

      if (tmp_max_density > max_density)
        max_density <- tmp_max_density

    }
  }

  # -------------------------------------------------------------------------- #

  for (j in levels(groups)) {

    if (nrow(trans_x[[j]]) < 2 ) next

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
        points <- findInterval(trans_x[[j]]$y, cur_bin) == 1

        #compute the border margin for the current bin
        if (method == "density")
          xmax <- mean(densities[[j]]$y[findInterval(densities[[j]]$x,
                                                     cur_bin) == 1])
        else
          xmax <- adjust * neighbours[[j]][i] / 2

        #assign the samples uniformely within the specified range
        x_translation <- stats::runif(neighbours[[j]][i], - xmax, xmax )

        #scale and store new x coordinates
        trans_x[[j]]$x[points] <- trans_x[[j]]$x[points] +
          (x_translation * global_scaling_factor * group_scaling_factor)
      }
    }
  }

  #collapse list to data frame
  trans_x <- do.call(rbind, trans_x)
  #return only the transposed x values in input order
  trans_x$x[order(trans_x$idx)]
}


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
