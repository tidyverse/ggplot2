#' Polar coordinates.
#'
#' The polar coordinate system is most commonly used for pie charts, which
#' are a stacked bar chart in polar coordinates.
#'
#' @param theta variable to map angle to (\code{x} or \code{y})
#' @param start offset of starting point from 12 o'clock in radians
#' @param direction 1, clockwise; -1, anticlockwise
#' @export
#' @examples
#' # NOTE: Use these plots with caution - polar coordinates has
#' # major perceptual problems.  The main point of these examples is
#' # to demonstrate how these common plots can be described in the
#' # grammar.  Use with EXTREME caution.
#'
#' #' # A pie chart = stacked bar chart + polar coordinates
#' pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
#'  geom_bar(width = 1)
#' pie + coord_polar(theta = "y")
#'
#' \donttest{
#'
#' # A coxcomb plot = bar chart + polar coordinates
#' cxc <- ggplot(mtcars, aes(x = factor(cyl))) +
#'   geom_bar(width = 1, colour = "black")
#' cxc + coord_polar()
#' # A new type of plot?
#' cxc + coord_polar(theta = "y")
#'
#' # The bullseye chart
#' pie + coord_polar()
#'
#' # Hadley's favourite pie chart
#' df <- data.frame(
#'   variable = c("does not resemble", "resembles"),
#'   value = c(20, 80)
#' )
#' ggplot(df, aes(x = "", y = value, fill = variable)) +
#'   geom_bar(width = 1, stat = "identity") +
#'   scale_fill_manual(values = c("red", "yellow")) +
#'   coord_polar("y", start = pi / 3) +
#'   labs(title = "Pac man")
#'
#' # Windrose + doughnut plot
#' if (require("ggplot2movies")) {
#' movies$rrating <- cut_interval(movies$rating, length = 1)
#' movies$budgetq <- cut_number(movies$budget, 4)
#'
#' doh <- ggplot(movies, aes(x = rrating, fill = budgetq))
#'
#' # Wind rose
#' doh + geom_bar(width = 1) + coord_polar()
#' # Race track plot
#' doh + geom_bar(width = 0.9, position = "fill") + coord_polar(theta = "y")
#' }
#' }
coord_polar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"

  ggproto(NULL, CoordPolar,
    theta = theta,
    r = r,
    start = start,
    direction = sign(direction)
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordPolar <- ggproto("CoordPolar", Coord,

  aspect = function(details) 1,

  distance = function(self, x, y, details) {
    if (self$theta == "x") {
      r <- rescale(y, from = details$r.range)
      theta <- theta_rescale_no_clip(self, x, details)
    } else {
      r <- rescale(x, from = details$r.range)
      theta <- theta_rescale_no_clip(self, y, details)
    }

    dist_polar(r, theta)
  },

  range = function(self, scale_details) {
    setNames(
      list(scale_details$theta.range, scale_details$r.range),
      c(self$theta, self$r)
    )
  },

  train = function(self, scale_details) {

    ret <- list(x = list(), y = list())
    for (n in c("x", "y")) {

      scale <- scale_details[[n]]
      limits <- self$limits[[n]]

      if (is.null(limits)) {
        if (self$theta == n) {
          expand <- expand_default(scale, c(0, 0.5), c(0, 0))
        } else {
          expand <- expand_default(scale, c(0, 0),   c(0, 0))
        }
        range <- scale$dimension(expand)
      } else {
        range <- range(scale_transform(scale, limits))
      }

      out <- scale$break_info(range)
      ret[[n]]$range <- out$range
      ret[[n]]$major <- out$major_source
      ret[[n]]$minor <- out$minor_source
      ret[[n]]$labels <- out$labels
    }

    details = list(
      x.range = ret$x$range, y.range = ret$y$range,
      x.major = ret$x$major, x.minor = ret$x$minor, x.labels = ret$x$labels,
      y.major = ret$y$major, y.minor = ret$y$minor, y.labels = ret$y$labels
    )

    if (self$theta == "y") {
      names(details) <- gsub("x\\.", "r.", names(details))
      names(details) <- gsub("y\\.", "theta.", names(details))
    } else {
      names(details) <- gsub("x\\.", "theta.", names(details))
      names(details) <- gsub("y\\.", "r.", names(details))
    }

    details
  },

  transform = function(self, data, scale_details) {
    data <- rename_data(self, data)

    data$r  <- r_rescale(self, data$r, scale_details)
    data$theta <- theta_rescale(self, data$theta, scale_details)
    data$x <- data$r * sin(data$theta) + 0.5
    data$y <- data$r * cos(data$theta) + 0.5

    data
  },

  render_axis_v = function(self, scale_details, theme) {
    x <- r_rescale(self, scale_details$r.major, scale_details) + 0.5
    guide_axis(x, scale_details$r.labels, "left", theme)
  },

  render_axis_h = function(scale_details, theme) {
    guide_axis(NA, "", "bottom", theme)
  },

  render_bg = function(self, scale_details, theme) {
    scale_details <- rename_data(self, scale_details)

    theta <- if (length(scale_details$theta.major) > 0)
      theta_rescale(self, scale_details$theta.major, scale_details)
    thetamin <- if (length(scale_details$theta.minor) > 0)
      theta_rescale(self, scale_details$theta.minor, scale_details)
    thetafine <- seq(0, 2 * pi, length.out = 100)

    rfine <- c(r_rescale(self, scale_details$r.major, scale_details), 0.45)

    # This gets the proper theme element for theta and r grid lines:
    #   panel.grid.major.x or .y
    majortheta <- paste("panel.grid.major.", self$theta, sep = "")
    minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
    majorr     <- paste("panel.grid.major.", self$r,     sep = "")

    ggname("grill", grobTree(
      element_render(theme, "panel.background"),
      if (length(theta) > 0) element_render(
        theme, majortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
        y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
        id.lengths = rep(2, length(theta)),
        default.units = "native"
      ),
      if (length(thetamin) > 0) element_render(
        theme, minortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
        y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
        id.lengths = rep(2, length(thetamin)),
        default.units = "native"
      ),

      element_render(
        theme, majorr, name = "radius",
        x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
        y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
        id.lengths = rep(length(thetafine), length(rfine)),
        default.units = "native"
      )
    ))
  },

  render_fg = function(self, scale_details, theme) {
    if (is.null(scale_details$theta.major)) {
      return(element_render(theme, "panel.border"))
    }

    theta <- theta_rescale(self, scale_details$theta.major, scale_details)
    labels <- scale_details$theta.labels

    # Combine the two ends of the scale if they are close
    theta <- theta[!is.na(theta)]
    ends_apart <- (theta[length(theta)] - theta[1]) %% (2 * pi)
    if (length(theta) > 0 && ends_apart < 0.05) {
      n <- length(labels)
      if (is.expression(labels)) {
        combined <- substitute(paste(a, "/", b),
          list(a = labels[[1]], b = labels[[n]]))
      } else {
        combined <- paste(labels[1], labels[n], sep = "/")
      }
      labels[[n]] <- combined
      labels <- labels[-1]
      theta <- theta[-1]
    }

    grobTree(
      if (length(labels) > 0) element_render(
        theme, "axis.text.x",
        labels, 0.45 * sin(theta) + 0.5, 0.45 * cos(theta) + 0.5,
        hjust = 0.5, vjust = 0.5,
        default.units = "native"
      ),
      element_render(theme, "panel.border")
    )
  },

  render_fg = function(self, scale_details, theme) {
    if (is.null(scale_details$theta.major)) {
      return(element_render(theme, "panel.border"))
    }

    theta <- theta_rescale(self, scale_details$theta.major, scale_details)
    labels <- scale_details$theta.labels

    # Combine the two ends of the scale if they are close
    theta <- theta[!is.na(theta)]
    ends_apart <- (theta[length(theta)] - theta[1]) %% (2*pi)
    if (length(theta) > 0 && ends_apart < 0.05) {
      n <- length(labels)
      if (is.expression(labels)) {
        combined <- substitute(paste(a, "/", b),
          list(a = labels[[1]], b = labels[[n]]))
      } else {
        combined <- paste(labels[1], labels[n], sep = "/")
      }
      labels[[n]] <- combined
      labels <- labels[-1]
      theta <- theta[-1]
    }

    grobTree(
      if (length(labels) > 0) element_render(
        theme, "axis.text.x",
        labels,
        unit(0.45 * sin(theta) + 0.5, "native"),
        unit(0.45 * cos(theta) + 0.5, "native"),
        hjust = 0.5, vjust = 0.5
      ),
      element_render(theme, "panel.border")
    )
  },

  labels = function(self, scale_details) {
    if (self$theta == "y") {
      list(x = scale_details$y, y = scale_details$x)
    } else {
      scale_details
    }
  }
)


rename_data <- function(coord, data) {
  if (coord$theta == "y") {
    plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
  } else {
    plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
  }
}

theta_rescale_no_clip <- function(coord, x, scale_details) {
  rotate <- function(x) (x + coord$start) * coord$direction
  rotate(rescale(x, c(0, 2 * pi), scale_details$theta.range))
}

theta_rescale <- function(coord, x, scale_details) {
  rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
  rotate(rescale(x, c(0, 2 * pi), scale_details$theta.range))
}

r_rescale <- function(coord, x, scale_details) {
  rescale(x, c(0, 0.4), scale_details$r.range)
}
