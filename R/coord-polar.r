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

  theta_scale <- function(.) .$.scales$get_scales(.$theta)
  theta_range <- function(.) {
    if (.$expand) {
      .$theta_scale()$output_expand()
    } else {
      .$theta_scale()$output_set()
    }
  }
  theta_rescale <- function(., x) {
    rotate <- function(x) (x + .$start) %% (2 * pi) * .$direction
    rotate(rescale(x, c(0, 2 * pi), .$theta_range()))
  }
  theta_discrete <- function(., x) .$.scales$get_scales(.$theta)$objname == "discrete"
  
  xlabel <- function(., gp) textGrob(NULL)
  ylabel <- function(., gp) textGrob(NULL)
  
  r_scale <- function(.) .$.scales$get_scales(.$r)
  r_range <- function(.) {
    if (.$expand) {
      .$r_scale()$output_expand()
    } else {
      .$r_scale()$output_set()
    }
  }
  
  r_rescale <- function(., x) rescale(x, c(0, 0.4), .$r_range())
  r_discrete <- function(., x) .$.scales$get_scales(.$r)$objname == "discrete"

  muncher <- function(.) TRUE
  transform <- function(., data) {
    r <- .$r_rescale(data[[.$r]])
    theta <- .$theta_rescale(data[[.$theta]])
    
    data$x <- r * sin(theta) + 0.5
    data$y <- r * cos(theta) + 0.5

    data
  }
  
  guide_background <- function(., theme) {
    
    theta <- .$theta_rescale(.$theta_scale()$input_breaks_n())
    thetamin <- .$theta_rescale(.$theta_scale()$output_breaks())
    thetafine <- seq(0, 2 * pi, length=100)
    
    
    r <- 0.4
    rfine <- c(.$r_rescale(.$r_scale()$input_breaks_n()), 0.45)

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

  guide_foreground <- function(., theme) {
    theta <- .$theta_rescale(.$theta_scale()$input_breaks_n())
    labels <- .$theta_scale()$labels()
    
    # Combine the two ends of the scale if they are close
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

  
  output_set <- function(.) {
    list(
      x = expand_range(c(-1, 1), 0.1, 0),
      y = expand_range(c(-1, 1), 0.1, 0)
    )
  }
    
  guide_axes <- function(., theme) {
    list(
      x = guide_axis(c(-1, 1), "", "bottom", theme),
      y = guide_axis(.$r_rescale(.$r_scale()$input_breaks_n()) + 0.5, .$r_scale()$labels(), "left", theme)
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
    movies$rrating <- factor(round_any(movies$rating, 1))
    movies$budgetq <- factor(chop(movies$budget, 4), labels = 1:4)
    
    doh <- ggplot(movies, aes(x = rrating, fill = budgetq))
    
    # Wind rose
    doh + geom_bar(width = 1) + coord_polar()
    # Race track plot
    doh + geom_bar(width = 0.9, position = "fill") + coord_polar(theta = "y")
  }

})