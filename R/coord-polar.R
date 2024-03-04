#' Polar coordinates
#'
#' The polar coordinate system is most commonly used for pie charts, which
#' are a stacked bar chart in polar coordinates. `coord_radial()` has extended
#' options.
#'
#' @param theta variable to map angle to (`x` or `y`)
#' @param start Offset of starting point from 12 o'clock in radians. Offset
#'   is applied clockwise or anticlockwise depending on value of `direction`.
#' @param direction 1, clockwise; -1, anticlockwise
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of `"on"` (the default) means yes, and a setting of `"off"`
#'   means no. For details, please see [`coord_cartesian()`].
#' @export
#' @seealso
#' The `r link_book("polar coordinates section", "coord#polar-coordinates-with-coord_polar")`
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
#'   geom_col(width = 1) +
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
coord_polar <- function(theta = "x", start = 0, direction = 1, clip = "on") {
  theta <- arg_match0(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"

  ggproto(NULL, CoordPolar,
    theta = theta,
    r = r,
    start = start,
    direction = sign(direction),
    clip = clip
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordPolar <- ggproto("CoordPolar", Coord,

  aspect = function(details) 1,

  is_free = function() TRUE,

  distance = function(self, x, y, details) {
    arc <- self$start + c(0, 2 * pi)
    dir <- self$direction
    if (self$theta == "x") {
      r <- rescale(y, from = details$r.range)
      theta <- theta_rescale_no_clip(x, details$theta.range, arc, dir)
    } else {
      r <- rescale(x, from = details$r.range)
      theta <- theta_rescale_no_clip(y, details$theta.range, arc, dir)
    }

    dist_polar(r, theta)
  },

  backtransform_range = function(self, panel_params) {
    self$range(panel_params)
  },

  range = function(self, panel_params) {
    # summarise_layout() expects that the x and y ranges here
    # match the setting from self$theta and self$r
    setNames(
      list(panel_params$theta.range, panel_params$r.range),
      c(self$theta, self$r)
    )
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {

    ret <- list(x = list(), y = list())
    for (n in c("x", "y")) {

      scale <- get(paste0("scale_", n))
      limits <- self$limits[[n]]

      if (self$theta == n) {
        expansion <- default_expansion(scale, c(0, 0.5), c(0, 0))
      } else {
        expansion <- default_expansion(scale, c(0, 0),   c(0, 0))
      }
      range <- expand_limits_scale(scale, expansion, coord_limits = limits)

      out <- scale$break_info(range)
      ret[[n]]$range <- out$range
      ret[[n]]$major <- out$major_source
      ret[[n]]$minor <- out$minor_source
      ret[[n]]$labels <- out$labels
      ret[[n]]$sec.range <- out$sec.range
      ret[[n]]$sec.major <- out$sec.major_source_user
      ret[[n]]$sec.minor <- out$sec.minor_source_user
      ret[[n]]$sec.labels <- out$sec.labels
    }

    details = list(
      x.range = ret$x$range, y.range = ret$y$range,
      x.major = ret$x$major, y.major = ret$y$major,
      x.minor = ret$x$minor, y.minor = ret$y$minor,
      x.labels = ret$x$labels, y.labels = ret$y$labels,
      x.sec.range = ret$x$sec.range, y.sec.range = ret$y$sec.range,
      x.sec.major = ret$x$sec.major, y.sec.major = ret$y$sec.major,
      x.sec.minor = ret$x$sec.minor, y.sec.minor = ret$y$sec.minor,
      x.sec.labels = ret$x$sec.labels, y.sec.labels = ret$y$sec.labels
    )

    if (self$theta == "y") {
      names(details) <- gsub("x\\.", "r.", names(details))
      names(details) <- gsub("y\\.", "theta.", names(details))
      details$r.arrange <- scale_x$axis_order()
    } else {
      names(details) <- gsub("x\\.", "theta.", names(details))
      names(details) <- gsub("y\\.", "r.", names(details))
      details$r.arrange <- scale_y$axis_order()
    }

    details
  },

  setup_panel_guides = function(self, panel_params, guides, params = list()) {
    panel_params
  },

  train_panel_guides = function(self, panel_params, layers, default_mapping, params = list()) {
    panel_params
  },

  transform = function(self, data, panel_params) {
    arc  <- self$start + c(0, 2 * pi)
    dir  <- self$direction
    data <- rename_data(self, data)

    data$r  <- r_rescale(data$r, panel_params$r.range)
    data$theta <- theta_rescale(data$theta, panel_params$theta.range, arc, dir)
    data$x <- data$r * sin(data$theta) + 0.5
    data$y <- data$r * cos(data$theta) + 0.5

    data
  },

  render_axis_v = function(self, panel_params, theme) {
    arrange <- panel_params$r.arrange %||% c("primary", "secondary")

    x <- r_rescale(panel_params$r.major, panel_params$r.range) + 0.5
    panel_params$r.major <- x
    if (!is.null(panel_params$r.sec.major)) {
      panel_params$r.sec.major <- r_rescale(
        panel_params$r.sec.major,
        panel_params$r.sec.range
      ) + 0.5
    }

    list(
      left = render_axis(panel_params, arrange[1], "r", "left", theme),
      right = render_axis(panel_params, arrange[2], "r", "right", theme)
    )
  },

  render_axis_h = function(panel_params, theme) {
    list(
      top = zeroGrob(),
      bottom = draw_axis(NA, "", "bottom", theme)
    )
  },

  render_bg = function(self, panel_params, theme) {
    panel_params <- rename_data(self, panel_params)
    arc <- self$start + c(0, 2 * pi)
    dir <- self$direction

    theta <- if (length(panel_params$theta.major) > 0)
      theta_rescale(panel_params$theta.major, panel_params$theta.range, arc, dir)
    thetamin <- if (length(panel_params$theta.minor) > 0)
      theta_rescale(panel_params$theta.minor, panel_params$theta.range, arc, dir)
    thetafine <- seq(0, 2 * pi, length.out = 100)

    rfine <- c(r_rescale(panel_params$r.major, panel_params$r.range), 0.45)

    # This gets the proper theme element for theta and r grid lines:
    #   panel.grid.major.x or .y
    majortheta <- paste("panel.grid.major.", self$theta, sep = "")
    minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
    majorr     <- paste("panel.grid.major.", self$r,     sep = "")

    ggname("grill", grobTree(
      element_render(theme, "panel.background"),
      if (length(theta) > 0) element_render(
        theme, majortheta, name = "angle",
        x = vec_interleave(0, 0.45 * sin(theta)) + 0.5,
        y = vec_interleave(0, 0.45 * cos(theta)) + 0.5,
        id.lengths = rep(2, length(theta)),
        default.units = "native"
      ),
      if (length(thetamin) > 0) element_render(
        theme, minortheta, name = "angle",
        x = vec_interleave(0, 0.45 * sin(thetamin)) + 0.5,
        y = vec_interleave(0, 0.45 * cos(thetamin)) + 0.5,
        id.lengths = rep(2, length(thetamin)),
        default.units = "native"
      ),

      element_render(
        theme, majorr, name = "radius",
        x = rep(rfine, each = length(thetafine)) * rep(sin(thetafine), length(rfine)) + 0.5,
        y = rep(rfine, each = length(thetafine)) * rep(cos(thetafine), length(rfine)) + 0.5,
        id.lengths = rep(length(thetafine), length(rfine)),
        default.units = "native"
      )
    ))
  },

  render_fg = function(self, panel_params, theme) {
    if (is.null(panel_params$theta.major)) {
      return(element_render(theme, "panel.border"))
    }
    arc <- self$start + c(0, 2 * pi)
    dir <- self$direction

    theta <- theta_rescale(panel_params$theta.major, panel_params$theta.range, arc, dir)
    labels <- panel_params$theta.labels

    # Combine the two ends of the scale if they are close
    theta <- theta[!is.na(theta)]
    ends_apart <- (theta[length(theta)] - theta[1]) %% (2*pi)
    if (length(theta) > 0 && ends_apart < 0.05 && !is.null(labels)) {
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

  labels = function(self, labels, panel_params) {
    if (self$theta == "y") {
      list(x = labels$y, y = labels$x)
    } else {
      labels
    }
  },

  modify_scales = function(self, scales_x, scales_y) {
    if (self$theta != "y")
      return()

    lapply(scales_x, scale_flip_position)
    lapply(scales_y, scale_flip_position)
  }
)


rename_data <- function(coord, data) {
  if (coord$theta == "y") {
    rename(data, c("y" = "theta", "x" = "r"))
  } else {
    rename(data, c("y" = "r", "x" = "theta"))
  }
}

theta_rescale_no_clip <- function(x, range, arc = c(0, 2 * pi), direction = 1) {
  rescale(x, to = arc, from = range) * direction
}

theta_rescale <- function(x, range, arc = c(0, 2 * pi), direction = 1) {
  x <- squish_infinite(x, range)
  rescale(x, to = arc, from = range) %% (2 * pi) * direction
}

r_rescale <- function(x, range, donut = c(0, 0.4)) {
  x <- squish_infinite(x, range)
  rescale(x, donut, range)
}
