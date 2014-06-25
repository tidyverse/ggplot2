
#' @rdname stat_ydensity
#' @export
stat_xdensity <- function(mapping = NULL, data = NULL, geom = "violinh",
                          position = "dodgev", adjust = 1,
                          kernel = "gaussian", trim = TRUE, scale = "area",
                          na.rm = FALSE, ...) {
  StatXdensity$new(mapping = mapping, data = data, geom = geom,
    position = position, adjust = adjust, kernel = kernel, trim = trim,
    scale = scale, na.rm = na.rm, ...)
}

StatXdensity <- proto(Stat, {
  objname <- "xdensity"

  calculate_groups <- function(., data, na.rm = FALSE, width = NULL,
                               scale = "area", ...) {
    data <- remove_missing(data, na.rm, "x", name = "stat_xdensity", finite = TRUE)
    data <- .super$calculate_groups(., data, na.rm = na.rm, height = height, ...)

    # choose how violins are scaled relative to each other
    scale <- match.arg(scale, c("area", "count", "height"))

    data$violinheight <- switch(scale,
      # area : keep the original densities but scale them to a max width of 1
      #        for plotting purposes only
      area = data$density / max(data$density),
      # count: use the original densities scaled to a maximum of 1 (as above)
      #        and then scale them according to the number of observations
      count = (data$density / max(data$density)) * data$n / max(data$n),
      # width: constant width (density scaled to a maximum of 1)
      width = data$scaled
    )

    data
  }

  calculate <- function(., data, scales, height = NULL, adjust = 1,
                        kernel = "gaussian", trim = TRUE,
                        na.rm = FALSE, ...) {

    n <- nrow(data)

    # if less than 3 points, return a density of 1 everywhere
    if (n < 3) {
      return(data.frame(data, density = 1, scaled = 1, count = 1))
    }

    # initialize weights if they are not supplied by the user
    if (is.null(data$weight)) { data$weight <- rep(1, n) / n }

    # compute the density
    dens <- density(data$x, adjust = adjust, kernel = kernel,
      weight = data$weight, n = 200)

    # NB: stat_density restricts to the scale range, here we leave that
    # free so violins can extend the y scale
    densdf <- data.frame(x = dens$x, density = dens$y)

    # scale density to a maximum of 1
    densdf$scaled <- densdf$density / max(densdf$density, na.rm = TRUE)

    # trim density outside of the data range
    if (trim) {
      densdf <- subset(densdf, x > min(data$x, na.rm = TRUE) &
                                 x < max(data$x, na.rm = TRUE))
    }
    # NB: equivalently, we could have used these bounds in the from and
    # to arguments of density()

    # scale density by the number of observations
    densdf$count <- densdf$density * n
    # record the number of observations to be able to scale the density later
    densdf$n <- n

    # coordinate on the y axis
    densdf$y <- if (is.factor(data$y)) data$y[1] else mean(range(data$y))


    # Circumvent a weird bug: height argument is not passed along
    height <- NULL

    # height of the bounding box of the violin plot on the y axis for continuous y
    if (length(unique(data$y)) > 1) { height <- diff(range(data$y)) * 0.9 }
    densdf$height <- height

    densdf
  }

  default_geom <- function(.) GeomViolinh
  required_aes <- c("x", "y")

})
