GeomVline <- proto(Geom, {
  new <- function(., ...) {
    .super$new(., ..., ignore.extra = TRUE)
  }

  draw <- function(., data, scales, coordinates, intercept = NULL, ...) {
    if (is.character(intercept)) intercept <- (match.fun(intercept))(data$x)
    
    data <- aesdefaults(data, .$default_aes(), list(...))
    if (is.null(intercept)) {
      if (is.null(data$intercept)) data$intercept <- 0
    } else {
      data <- data[rep(1, length(intercept)), ]
      data$intercept <- intercept
    }
    
    yrange <- coordinates$output_set()$y

    data <- coordinates$transform(transform(data,
      y = yrange[1],
      yend = yrange[2],
      x = intercept,
      xend = intercept
    ))
    
    GeomSegment$draw(unique(data), scales, coordinates)
  }

  objname <- "vline"
  desc <- "Line, vertical"
  icon <- function(.) linesGrob(c(0.5, 0.5), c(0, 1))
  details <- "<p>This geom allows you to annotate the plot with vertical lines (see geom_hline and geom_abline for other types of lines)</p>\n\n<p>There are two ways to use it.  You can either specify the intercept of the line in the call to the geom, in which case the line will be in the same position in every panel.  Alternatively, you can supply a different intercept for each panel using a data.frame.  See the examples for the differences</p>"
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) c(GeomPath$default_aes(), aes(intercept=0))
  guide_geom <- function(.) "vline"

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, 
      ggname(.$my_name(), segmentsGrob(0.5, 0, 0.5, 1, default.units="npc",
      gp=gpar(col=colour, lwd=size * .pt, lty=linetype, lineend="butt")))
    )
  }

  seealso <- list(
    geom_hline = "for horizontal lines",
    geom_abline = "for lines defined by a slope and intercept",
    geom_segment = "for a more general approach"
  )
  
  examples <- function(.) {
    # Fixed lines
    p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()
    p + geom_vline(intercept=5)
    p + geom_vline(intercept=2:5)
    p + geom_vline(intercept=2:5, colour="green")
    p + geom_vline(intercept=mean(mtcars$wt), size=2)
    
    # Lines from data
    p <- ggplot(mtcars, aes(x = wt, y=mpg)) + facet_grid(. ~ cyl) + geom_point()
    p + geom_vline(intercept="mean")
  }  
})
