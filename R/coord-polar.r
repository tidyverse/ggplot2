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
#'   labs(title = "Pac man")
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
  if (coord$theta == "x") {
    r <- rescale(y, from = details$r.range)
    theta <- theta_rescale_no_clip(coord, x, details)
  } else {
    r <- rescale(x, from = details$r.range)
    theta <- theta_rescale_no_clip(coord, y, details)
  }

  # Pretending that theta is x and r is y, find the slope and intercepts
  # for each line segment.
  # This is just like finding the x-intercept of a line in cartesian coordinates.
  lf <- find_line_formula(theta, r)

  # Rename x and y columns to r and t, since we're working in polar
  # Note that 'slope' actually means the spiral slope, 'a' in the spiral
  #   formula r = a * theta
  lf <- rename(lf, c(x1 = "t1", x2 = "t2", y1 = "r1", y2 = "r2",
    yintercept = "r_int",  xintercept = "t_int"))

  # Re-normalize the theta values so that intercept for each is 0
  # This is necessary for calculating spiral arc length.
  # If the formula is r=a*theta, there's a big difference between
  # calculating the arc length from theta = 0 to pi/2, vs.
  # theta = 2*pi to pi/2
  lf$tn1 <- lf$t1 - lf$t_int
  lf$tn2 <- lf$t2 - lf$t_int

  # Add empty distance column
  lf$dist <- NA_real_

  # There are three types of lines, which we handle in turn:
  # - Spiral arcs (r and theta change)
  # - Circular arcs (r is constant)
  # - Rays (theta is constant)

  # Get spiral arc length for segments that have non-zero, non-infinite slope
  # (spiral_arc_length only works for actual spirals, not circle arcs or rays)
  # Use the _normalized_ theta values for arc length calculation
  idx <- lf$slope != 0 & !is.infinite(lf$slope)
  lf$dist[idx] <-
    spiral_arc_length(lf$slope[idx], lf$tn1[idx], lf$tn2[idx])

  # Get cicular arc length for segments that have zero slope (r1 == r2)
  idx <- lf$slope == 0
  lf$dist[idx] <- lf$r1[idx] * (lf$t2[idx] - lf$t1[idx])

  # Get radial length for segments that have infinite slope (t1 == t2)
  idx <- is.infinite(lf$slope)
  lf$dist[idx] <- lf$r1[idx] - lf$r2[idx]

  # Find the maximum possible length, a spiral line from
  # (r=0, theta=0) to (r=1, theta=2*pi)
  max_dist <- spiral_arc_length(1 / (2 * pi), 0, 2 * pi)

  # Final distance values, normalized
  abs(lf$dist / max_dist)
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

    out <- scale_break_info(scale, range)
    ret[[n]]$range <- out$range
    ret[[n]]$major <- out$major_source
    ret[[n]]$minor <- out$minor_source
    ret[[n]]$labels <- out$labels
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
  if (coord$theta == aesthetic) {
    expand_default(scale, c(0, 0.5), c(0, 0))
  } else {
    expand_default(scale, c(0, 0),   c(0, 0))
  }
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

  # This gets the proper theme element for theta and r grid lines:
  #   panel.grid.major.x or .y
  majortheta <- paste("panel.grid.major.", coord$theta, sep = "")
  minortheta <- paste("panel.grid.minor.", coord$theta, sep = "")
  majorr     <- paste("panel.grid.major.", coord$r,     sep = "")

  ggname("grill", grobTree(
    element_render(theme, "panel.background"),
    if (length(theta) > 0) element_render(
      theme, majortheta, name = "angle",
      x = c(rbind(0, 0.45 * sin(theta))) + 0.5, 
      y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
      id.lengths = rep(2, length(theta)), 
      default.units="native"
    ),
    if (length(thetamin) > 0) element_render(
      theme, minortheta, name = "angle",
      x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5, 
      y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
      id.lengths = rep(2, length(thetamin)),  
      default.units="native"
    ),
    
    element_render(
      theme, majorr, name = "radius",
      x = rep(rfine, each=length(thetafine)) * sin(thetafine) + 0.5, 
      y = rep(rfine, each=length(thetafine)) * cos(thetafine) + 0.5,
      id.lengths = rep(length(thetafine), length(rfine)),
      default.units="native"
    )
  ))
}

#' @S3method coord_render_fg polar
coord_render_fg.polar <- function(coord, details, theme) {
  if (is.null(details$theta.major)) {
    return(element_render(theme, "panel.border"))
  }
  
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
    if (length(labels) > 0) element_render(
      theme, "axis.text.x", 
      labels, 0.45 * sin(theta) + 0.5, 0.45 * cos(theta) + 0.5,
      hjust = 0.5, vjust = 0.5,
      default.units="native"
    ),      
    element_render(theme, "panel.border")
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
