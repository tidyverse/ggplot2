CoordPolar <- proto(Coord, {

  new <- function(., theta="x", start = 0, direction = 1) {
    theta <- if (theta == "x") "x" else "y"
    r <- if (theta == "x") "y" else "x"

    c(.$proto(theta = theta, r = r, start=start, direction=sign(direction)), list(opts(aspect.ratio = 1)))
  }

  expand <- function(.) {
    list(
      theta = .$theta_scale()$.expand, 
      r = .$r_scale()$.expand
    )
  }

  theta_scale <- function(.) .$.scales$get_scales(.$theta)
  theta_range <- function(.) {
    expand <- .$expand()
    expand_range(.$theta_scale()$output_set(), expand$theta[1], expand$theta[2])
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
    expand <- .$expand()
    expand_range(.$r_scale()$output_set(), 0, expand$r[2])
  }
  
  r_rescale <- function(., x) rescale(x, c(0, 0.9), .$r_range())
  r_discrete <- function(., x) .$.scales$get_scales(.$r)$objname == "discrete"

  muncher <- function(.) TRUE
  transform <- function(., data) {
    r <- .$r_rescale(data[[.$r]])
    theta <- .$theta_rescale(data[[.$theta]])
    
    data$x <- r * sin(theta)
    data$y <- r * cos(theta)
  
    data
  }
  
  guide_background <- function(., theme) {
    
    theta <- .$theta_rescale(.$theta_scale()$input_breaks())
    thetamin <- .$theta_rescale(.$theta_scale()$output_breaks())
    thetafine <- seq(0, 2*pi, length=100)
    
    labels <- .$theta_scale()$labels()
    ends_apart <- 1 - (theta[length(theta)] - theta[1]) / (2*pi)
    if (ends_apart < 0.05) {
      labels[length(labels)] <- paste(labels[1], labels[length(labels)], sep="/")
      labels <- labels[-1]
      theta <- theta[-1]
    }
    
    r <- 1
    rfine <- .$r_rescale(.$r_scale()$input_breaks())

    ggname("grill", grobTree(
      theme_render(theme, "panel.background"),
      if (length(labels) > 0) theme_render(
        theme, "panel.grid.major", name = "angle", 
        x = c(rbind(0, 5 * sin(theta))), 
        y = c(rbind(0, 5 * cos(theta))),
        id.lengths = rep(2, length(theta)), 
        default.units="native"
      ),
      theme_render(
        theme, "panel.grid.minor", name = "angle", 
        x = c(rbind(0, 5 * sin(thetamin))), 
        y = c(rbind(0, 5 * cos(thetamin))),
        id.lengths = rep(2, length(thetamin)),  
        default.units="native"
      ),
      
      if (length(labels) > 0) theme_render(
        theme, "axis.text.x", 
        labels, r * 1.1 * sin(theta), r * 1.1 * cos(theta),
        default.units="native"
      ),
      theme_render(
        theme, "panel.grid.major", name = "radius",
        x = rep(rfine, each=length(thetafine)) * sin(thetafine), 
        y = rep(rfine, each=length(thetafine)) * cos(thetafine),
        id.lengths = rep(length(thetafine), length(rfine)),
        default.units="native"
      )
    ))
  }

  
  output_set <- function(.) {
    list(
      x = expand_range(c(-1, 1), 0.1, 0),
      y = expand_range(c(-1, 1), 0.1, 0)
    )
  }
    
  guide_axes <- function(.) {
    list(
      x = guide_axis(c(-1, 1), "", "bottom", c(-1,1)),
      y = guide_axis(.$r_rescale(.$r_scale()$input_breaks()) / 2 + 0.6, .$r_scale()$labels(), "left", c(0, 1.2))
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
    direction = "1, clockwise; -1, anticlockwise"
  )
  
  examples <- function(.) {
    # Still very experimental, so a bit on the buggy side, but I'm
    # working on it.  Also need to deal properly with cyclical
    # variables
    ggopt(aspect.ratio = 1)    
    
    # NOTE: Use these plots with caution - polar coordinates has
    # major perceptual problems.  The main point of these examples is 
    # to demonstrate how these common plots can be described in the
    # grammar.  Use with EXTREME caution.

    # A coxcomb plot = bar chart + polar coordinates
    cxc <- ggplot(mtcars, aes(x=factor(cyl))) + geom_bar(width=1, colour="black")
    cxc + coord_polar()
    # A new type of plot?
    cxc + coord_polar(theta = "y")
    
    # A pie chart = stacked bar chart + polar coordinates
    pie <- ggplot(mtcars, aes(x=factor(1), fill=factor(cyl))) + geom_bar(width=1)
    pie + coord_polar(theta="y")

    # A new type of plot?
    pie + coord_polar()
    
    # Hadley's favourite pie chart
    df <- data.frame(
      variable = c("resembles", "does not resemble"),
      value = c(80, 20)
    )
    ggplot(df, aes(x = "", y = value, fill = variable)) + geom_bar(width=1) + scale_fill_manual(values = c("red","yellow")) + coord_polar("y", start=pi/3) + opts(title = "Pac man")
    
    
    
    # Windrose + doughnut plot
    movies$rrating <- factor(round_any(movies$rating, 1))
    movies$budgetq <- factor(chop(movies$budget, 4), labels=1:4)
    
    doh <- ggplot(movies, aes(x=rrating, fill=budgetq))
    
    # Wind rose
    doh + geom_bar(width=1) + coord_polar()
    #Doughnut plot
    doh + geom_bar(width=0.9, position="fill") + coord_polar(theta="y")
    ggopt(aspect.ratio = NULL)
  }

})