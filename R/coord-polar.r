CoordPolar <- proto(Coord, {

  new <- function(., theta="x", start = 0, direction = 1, expand = FALSE) {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") "y" else "x"

    c(
      .$proto(
        theta = theta, r = r, 
        start = start, direction = sign(direction),
        expand = expand
      ), 
      list(opts(aspect.ratio = 1))
    )
  }

  compute_ranges <- function(., scales) {
    if (.$expand) {
      x.range <- scales$x$output_expand() 
      y.range <- scales$y$output_expand() 
    } else {
      x.range <- scales$x$output_set() 
      y.range <- scales$y$output_set() 
    }

    x.major <- scales$x$input_breaks_n()
    x.minor <- scales$x$output_breaks()
    x.labels <- scales$x$labels()

    y.major <- scales$y$input_breaks_n()
    y.minor <- scales$y$output_breaks()
    y.labels <- scales$y$labels()
    
    details <- list(
      x.range = x.range, y.range = y.range, 
      x.major = x.major, x.minor = x.minor, x.labels = x.labels,
      y.major = y.major, y.minor = y.minor, y.labels = y.labels
    )
    
    if (.$theta == "y") {
      names(details) <- gsub("x\\.", "r.", names(details))
      names(details) <- gsub("y\\.", "theta.", names(details))
    } else {
      names(details) <- gsub("x\\.", "theta.", names(details))      
      names(details) <- gsub("y\\.", "r.", names(details))
    }
    details
  }

  rename_data <- function(., data) {
    if (.$theta == "y") {
      rename(data, c("y" = "theta", "x" = "r"))
    } else {
      rename(data, c("y" = "r", "x" = "theta"))
    }
  }
  

  theta_rescale <- function(., x, details) {
    rotate <- function(x) (x + .$start) %% (2 * pi) * .$direction
    rotate(rescale(x, c(0, 2 * pi), details$theta.range))
  }
    
  r_rescale <- function(., x, details) {
    rescale(x, c(0, 0.4), details$r.range)
  }

  muncher <- function(.) TRUE
  transform <- function(., data, details) {
    data <- .$rename_data(data)
    
    data <- within(data, {
      r <- .$r_rescale(r, details)
      theta <- .$theta_rescale(theta, details)

      x <- r * sin(theta) + 0.5
      y <- r * cos(theta) + 0.5
    })
  }
  
  guide_axis_v <- function(., details, theme) {
    guide_axis(.$r_rescale(details$r.major, details) + 0.5, details$r.labels, "left", theme)
  }
  guide_axis_h <- function(., details, theme) {
    guide_axis(NA, "", "bottom", theme)
  }
  
  guide_background <- function(., details, theme) {
    details <- .$rename_data(details)
    
    theta <- .$theta_rescale(details$theta.major, details)
    thetamin <- .$theta_rescale(details$theta.minor, details)
    thetafine <- seq(0, 2 * pi, length=100)    
    
    r <- 0.4
    rfine <- c(.$r_rescale(details$r.major, details), 0.45)

    ggname("grill", grobTree(
      theme_render(theme, "panel.background"),
      if (length(labels) > 0) theme_render(
        theme, "panel.grid.major", name = "angle", 
        x = c(rbind(0, 0.45 * sin(theta))) + 0.5, 
        y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
        id.lengths = rep(2, length(theta)), 
        default.units="native"
      ),
      theme_render(
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

  guide_foreground <- function(., details, theme) {
    theta <- .$theta_rescale(details$theta.major, details)
    labels <- details$theta.labels
    
    # Combine the two ends of the scale if they are close
    theta <- theta[!is.na(theta)]
    ends_apart <- (theta[length(theta)] - theta[1]) %% (2*pi)
    if (ends_apart < 0.05) {
      labels[length(labels)] <- paste(labels[1], labels[length(labels)], sep="/")
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

    

  # Documentation -----------------------------------------------

  objname <- "polar"
  desc <- "Polar coordinates"
  icon <- function(.) circleGrob(r = c(0.1, 0.25, 0.45), gp=gpar(fill=NA))
  
  details <- "<p>The polar coordinate system is most commonly used for pie charts, which are a stacked bar chart in polar coordinates.</p>\n\n<p>This coordinate system has one argument, <code>theta</code>, which determines which variable is mapped to angle and which to radius.  Valid values are \"x\" and \"y\".</p>\n"
  
  desc_params <- list(
    theta = "variable to map angle to ('x' or 'y')",
    start = "offset from 12 o'clock in radians",
    direction = "1, clockwise; -1, anticlockwise",
    expand = "should axes be expanded to slightly outside the range of the data? (default: FALSE)"
  )
  
  examples <- function(.) {
    # NOTE: Use these plots with caution - polar coordinates has
    # major perceptual problems.  The main point of these examples is 
    # to demonstrate how these common plots can be described in the
    # grammar.  Use with EXTREME caution.

    # A coxcomb plot = bar chart + polar coordinates
    cxc <- ggplot(mtcars, aes(x = factor(cyl))) + 
      geom_bar(width = 1, colour = "black")
    cxc + coord_polar()
    # A new type of plot?
    cxc + coord_polar(theta = "y")
    
    # A pie chart = stacked bar chart + polar coordinates
    pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
     geom_bar(width = 1)
    pie + coord_polar(theta = "y")

    # The bullseye chart
    pie + coord_polar()
    
    # Hadley's favourite pie chart
    df <- data.frame(
      variable = c("resembles", "does not resemble"),
      value = c(80, 20)
    )
    ggplot(df, aes(x = "", y = value, fill = variable)) + 
      geom_bar(width = 1) + 
      scale_fill_manual(values = c("red", "yellow")) + 
      coord_polar("y", start=pi / 3) + 
      opts(title = "Pac man")
    
    # Windrose + doughnut plot
    movies$rrating <- cut_interval(movies$rating, length = 1)
    movies$budgetq <- cut_number(movies$budget, 4)
    
    doh <- ggplot(movies, aes(x = rrating, fill = budgetq))
    
    # Wind rose
    doh + geom_bar(width = 1) + coord_polar()
    # Race track plot
    doh + geom_bar(width = 0.9, position = "fill") + coord_polar(theta = "y")
  }

})