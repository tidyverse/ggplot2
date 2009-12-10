GeomVline <- proto(Geom, {
  new <- function(., data = NULL, mapping = NULL, xintercept = NULL, ...) {
    if (is.numeric(xintercept)) {
      data <- data.frame(xintercept = xintercept)
      mapping <- aes_all(names(data))
    }
    .super$new(., data = data, mapping = mapping, inherit.aes = FALSE, 
      xintercept = xintercept, ...)
  }
  
  draw <- function(., data, scales, coordinates, ...) {
    data$y    <- -Inf
    data$yend <- Inf
    
    GeomSegment$draw(unique(data), scales, coordinates)
  }

  objname <- "vline"
  desc <- "Line, vertical"
  icon <- function(.) linesGrob(c(0.5, 0.5), c(0, 1))
  details <- "<p>This geom allows you to annotate the plot with vertical lines (see geom_hline and geom_abline for other types of lines)</p>\n\n<p>There are two ways to use it.  You can either specify the intercept of the line in the call to the geom, in which case the line will be in the same position in every panel.  Alternatively, you can supply a different intercept for each panel using a data.frame.  See the examples for the differences</p>"
  
  default_stat <- function(.) StatVline
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = 1)
  guide_geom <- function(.) "vline"

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, 
      ggname(.$my_name(), segmentsGrob(0.5, 0, 0.5, 1, default.units="npc",
      gp=gpar(col=alpha(colour, alpha), lwd=size * .pt, lty=linetype, lineend="butt")))
    )
  }

  seealso <- list(
    geom_hline = "for horizontal lines",
    geom_abline = "for lines defined by a slope and intercept",
    geom_segment = "for a more general approach"
  )
  
  examples <- function(.) {
    # Fixed lines
    p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
    p + geom_vline(xintercept = 5)
    p + geom_vline(xintercept = 1:5)
    p + geom_vline(xintercept = 1:5, colour="green")
    
    last_plot() + coord_equal()
    last_plot() + coord_flip()
    
    p2 <- p + aes(colour = factor(cyl))
    p2 + geom_vline(xintercept = 15)
  }  
})
