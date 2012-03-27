#' Polar coordinates.
#' 
#' The polar coordinate system is most commonly used for pie charts, which 
#' are a stacked bar chart in polar coordinates.
#'
#' @param theta variable to map angle to (\code{x} or \code{y})
#' @param start offset of starting point from 12 o'clock in radians
#' @param direction 1, clockwise; -1, anticlockwise
#' @param expand should axes be expanded to slightly outside the range of the
#'   data? (default: \code{FALSE})
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
coord_polar <- function(theta = "x", start = 0, direction = 1, expand = FALSE) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  
  coord(
    theta = theta, r = r, 
    start = start, direction = sign(direction),
    expand = expand, 
    subclass = "polar"
  )
}

#' @S3method coord_aspect polar
coord_aspect.polar <- function(coord, details) 1

#' @S3method coord_distance polar
coord_distance.polar <- function(coord, x, y, details) {
  max_dist <- 2 * pi * abs(diff(details$r.range))
  
  if (coord$theta == "x") {
    r <- y
    theta <- theta_rescale_no_clip(coord, x, details)
  } else {
    r <- x
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
  if (coord$expand) {
    x.range <- scale_dimension(scales$x)
    y.range <- scale_dimension(scales$y)
  } else {
    x.range <- scale_dimension(scales$x, c(0, 0))
    y.range <- scale_dimension(scales$y, c(0, 0))
  }

  x.major <- scale_break_positions(scales$x)
  x.minor <- scale_breaks_minor_positions(scales$x)
  x.labels <- scale_labels(scales$x, x.major)

  y.major <- scale_break_positions(scales$y)
  y.minor <- scale_breaks_minor_positions(scales$y)
  y.labels <- scale_labels(scales$y, y.major)
  
  details <- list(
    x.range = x.range, y.range = y.range, 
    x.major = x.major, x.minor = x.minor, x.labels = x.labels,
    y.major = y.major, y.minor = y.minor, y.labels = y.labels
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

#' @S3method coord_transform polar
coord_transform.polar <- function(coord, data, details) {
  data <- rename_data(coord, data)
  data <- within(data, {
    r <- r_rescale(coord, r, details)
    theta <- theta_rescale(coord, theta, details)

    x <- r * sin(theta) + 0.5
    y <- r * cos(theta) + 0.5
  })
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
