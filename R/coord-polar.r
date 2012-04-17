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
#' \donttest{
#' # NOTE: Use these plots with caution - polar coordinates has
#' # major perceptual problems.  The main point of these examples is 
#' # to demonstrate how these common plots can be described in the
#' # grammar.  Use with EXTREME caution.
#'
#' # A coxcomb plot = bar chart + polar coordinates
#' cxc <- ggplot(mtcars, aes(x = factor(cyl))) + 
#'   geom_bar(width = 1, colour = "black")
#' cxc + coord_polar()
#' # A new type of plot?
#' cxc + coord_polar(theta = "y")
#' 
#' # A pie chart = stacked bar chart + polar coordinates
#' pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
#'  geom_bar(width = 1)
#' pie + coord_polar(theta = "y")
#'
#' # The bullseye chart
#' pie + coord_polar()
#' 
#' # Hadley's favourite pie chart
#' df <- data.frame(
#'   variable = c("resembles", "does not resemble"),
#'   value = c(80, 20)
#' )
#' ggplot(df, aes(x = "", y = value, fill = variable)) + 
#'   geom_bar(width = 1) + 
#'   scale_fill_manual(values = c("red", "yellow")) + 
#'   coord_polar("y", start=pi / 3) + 
#'   opts(title = "Pac man")
#' 
#' # Windrose + doughnut plot
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
coord_polar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  
  coord(
    theta = theta, r = r, 
    start = start, direction = sign(direction),
    subclass = "polar"
  )
}

#' @S3method coord_aspect polar
coord_aspect.polar <- function(coord, details) 1

#' @S3method coord_distance polar
coord_distance.polar <- function(coord, x, y, details) {
  max_dist <- 2 * pi
  
  if (coord$theta == "x") {
    r <- rescale(y, from = details$r.range)
    theta <- theta_rescale_no_clip(coord, x, details)
  } else {
    r <- rescale(x, from = details$r.range)
    theta <- theta_rescale_no_clip(coord, y, details)
  }
  px <- r * cos(theta)
  py <- r * sin(theta)
  pz <- theta * r

  sqrt(diff(px) ^ 2 + diff(py) ^ 2 + diff(pz) ^ 2) / max_dist
}

#' @S3method coord_range polar
coord_range.polar <- function(coord, scales) {
  setNames(list(scales$theta.range, scales$r.range), c(coord$theta, coord$r))
}

#' @S3method coord_train polar
coord_train.polar <- function(coord, scales) {

  ret <- list(x = list(), y = list())
  for (n in c("x", "y")) {

    scale <- scales[[n]]
    limits <- coord$limits[[n]]
    
    if (is.null(limits)) {
      expand <- coord_expand_defaults(coord, scale, n)
      range <- scale_dimension(scale, expand)
    } else {
      range <- range(scale_transform(scale, limits))
    }

    # @kohske
    # TODO
    # these codes is reusable
    # probably scale_breaks(scale, range) that returns
    # list(major, minor, labels, range)
    if (inherits(scale, "continuous")) {
      major_v <- c(na.omit(scale_map(scale, scale_breaks(scale, range), range)))
      labels <- scale_labels(scale, major_v)
      minor_v <- c(na.omit(scale_map(scale, scale_breaks_minor(scale, b = major_v, limits = range), range)))
    } else {
      b <- scale_breaks(scale, scale_limits(scale))
      major_v <- c(na.omit(scale_map(scale, b)))
      labels <- scale_labels(scale, b)
      minor_v <- NULL
    }

    # major and minor values in plot space
    major <- rescale(major_v, from = range)
    minor <- rescale(minor_v, from = range)

    ret[[n]]$range <- range
    ret[[n]]$major <- major_v
    ret[[n]]$minor <- minor_v
    ret[[n]]$labels <- labels
  }

  details <- list(
    x.range = ret$x$range, y.range = ret$y$range, 
    x.major = ret$x$major, x.minor = ret$x$minor, x.labels = ret$x$labels,
    y.major = ret$y$major, y.minor = ret$y$minor, y.labels = ret$y$labels
  )

  if (coord$theta == "y") {
    names(details) <- gsub("x\\.", "r.", names(details))
    names(details) <- gsub("y\\.", "theta.", names(details))
  } else {
    names(details) <- gsub("x\\.", "theta.", names(details))      
    names(details) <- gsub("y\\.", "r.", names(details))
  }

  details
}

rename_data <- function(coord, data) {
  if (coord$theta == "y") {
    rename(data, c("y" = "theta", "x" = "r"))
  } else {
    rename(data, c("y" = "r", "x" = "theta"))
  }
}

theta_rescale_no_clip <- function(coord, x, details) {
  rotate <- function(x) (x + coord$start) * coord$direction
  rotate(rescale(x, c(0, 2 * pi), details$theta.range))
}

theta_rescale <- function(coord, x, details) {
  rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
  rotate(rescale(x, c(0, 2 * pi), details$theta.range))
}
  
r_rescale <- function(coord, x, details) {
  rescale(x, c(0, 0.4), details$r.range)
}

#' @S3method coord_expand_defaults polar
coord_expand_defaults.polar <- function(coord, scale, aesthetic) {
  if (coord$theta == aesthetic)
    expand_default(scale, c(0, 0.5), c(0, 0))
  else
    expand_default(scale, c(0, 0),   c(0, 0))
}

#' @S3method coord_transform polar
coord_transform.polar <- function(coord, data, details) {
  data <- rename_data(coord, data)
  
  data$r  <- r_rescale(coord, data$r, details)
  data$theta <- theta_rescale(coord, data$theta, details)
  data$x <- data$r * sin(data$theta) + 0.5
  data$y <- data$r * cos(data$theta) + 0.5
  
  data
}

#' @S3method coord_render_axis_v polar
coord_render_axis_v.polar <- function(coord, details, theme) {
  x <- r_rescale(coord,details$r.major, details) + 0.5
  guide_axis(x, details$r.labels, "left", theme)
}
#' @S3method coord_render_axis_h polar
coord_render_axis_h.polar <- function(coord, details, theme) {
  guide_axis(NA, "", "bottom", theme)
}

#' @S3method coord_render_bg polar
coord_render_bg.polar <- function(coord, details, theme) {
  details <- rename_data(coord, details)
  
  theta <- if (length(details$theta.major) > 0) theta_rescale(coord, details$theta.major, details)
  thetamin <- if (length(details$theta.minor) > 0) theta_rescale(coord, details$theta.minor, details)
  thetafine <- seq(0, 2 * pi, length=100)    

  r <- 0.4
  rfine <- c(r_rescale(coord, details$r.major, details), 0.45)

  ggname("grill", grobTree(
    theme_render(theme, "panel.background"),
    if (length(theta) > 0) theme_render(
      theme, "panel.grid.major", name = "angle", 
      x = c(rbind(0, 0.45 * sin(theta))) + 0.5, 
      y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
      id.lengths = rep(2, length(theta)), 
      default.units="native"
    ),
    if (length(thetamin) > 0) theme_render(
      theme, "panel.grid.minor", name = "angle", 
      x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5, 
      y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
      id.lengths = rep(2, length(thetamin)),  
      default.units="native"
    ),
    
    theme_render(
      theme, "panel.grid.major", name = "radius",
      x = rep(rfine, each=length(thetafine)) * sin(thetafine) + 0.5, 
      y = rep(rfine, each=length(thetafine)) * cos(thetafine) + 0.5,
      id.lengths = rep(length(thetafine), length(rfine)),
      default.units="native"
    )
  ))
}

#' @S3method coord_render_fg polar
coord_render_fg.polar <- function(coord, details, theme) {
  theta <- theta_rescale(coord, details$theta.major, details)
  labels <- details$theta.labels
  
  # Combine the two ends of the scale if they are close
  theta <- theta[!is.na(theta)]
  ends_apart <- (theta[length(theta)] - theta[1]) %% (2*pi)
  if (ends_apart < 0.05) {
    n <- length(labels)
    if (is.expression(labels)) {
      combined <- substitute(paste(a, "/", b), 
        list(a = labels[[1]], b = labels[[n]]))
    } else {
      combined <- paste(labels[1], labels[n], sep="/")
    }
    labels[[n]] <- combined
    labels <- labels[-1]
    theta <- theta[-1]
  }
    
  grobTree(
    if (length(labels) > 0) theme_render(
      theme, "axis.text.x", 
      labels, 0.45 * sin(theta) + 0.5, 0.45 * cos(theta) + 0.5,
      hjust = 0.5, vjust = 0.5,
      default.units="native"
    ),      
    theme_render(theme, "panel.border")
  )
}  

#' @S3method coord_labels polar
coord_labels.polar <- function(coord, scales) {
  if (coord$theta == "y") {
    list(x = scales$y, y = scales$x)
  } else {
    scales
  }
}

  
icon <- function(.) circleGrob(r = c(0.1, 0.25, 0.45), gp=gpar(fill=NA))
