CoordFlip <- proto(CoordCartesian, expr={
  x <- function(.) .$.scales$get_scales("y")
  y <- function(.) .$.scales$get_scales("x")
  
  muncher <- function(.) FALSE
  transform <- function(., data) {
    data <- transform_position(data, .$transform_y, .$transform_x)
    rename(data, c(
      x = "y",       y = "x", 
      xend = "yend", yend = "xend", 
      xmin = "ymin", ymin = "xmin",
      xmax = "ymax", ymax = "xmax")
    )
  }
  munch <- function(., data) .$transform(data)

  # Documentation -----------------------------------------------

  objname <- "flip"
  desc <- "Flipped cartesian coordinates"
  details <- "<p>Flipped cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal.  This is primarily useful for converting geoms and statistics which display y conditional on x, to x conditional on y</p>"
  icon <- function(.) {
    angles <- seq(0, pi/2, length=20)[-c(1, 20)]
    gTree(children=gList(
      segmentsGrob(0, 0, 0, 1),
      segmentsGrob(0, 0, 1, 0),
      linesGrob(0.9 * sin(angles), 0.9 * cos(angles), arrow=arrow(length=unit(0.05, "npc"))),
      linesGrob(0.5 * sin(angles), 0.5 * cos(angles), arrow=arrow(end="first", length= unit(0.05, "npc")))
    ))
  }
  
  examples <- function(.) {
    # Very useful for creating boxplots, and other interval
    # geoms in the horizontal instead of vertical position.
    qplot(cut, price, data=diamonds, geom="boxplot")
    last_plot() + coord_flip()

    qplot(cut, data=diamonds, geom="bar")
    last_plot() + coord_flip()
    
    qplot(carat, data=diamonds, geom="histogram")
    last_plot() + coord_flip()

    # You can also use it to flip lines and area plots:
    qplot(1:5, (1:5)^2, geom="line")
    last_plot() + coord_flip()
  }
})
