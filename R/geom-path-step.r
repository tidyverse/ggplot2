GeomStep <- proto(GeomPath, {
  objname <- "step"
  desc <- "Connect observations by stairs"
  icon <- function(.) {
    n <- 15
    xs <- rep(0:n, each = 2)[-2*(n + 1)] / 15
    ys <- c(0, rep(1:n, each=2)) / 15
    
    linesGrob(xs, ys, gp=gpar(col="grey20"))
  }
  details <- "Equivalent to plot(type='s')."
  
  default_stat <- function(.) StatStep
  
  examples <- function(.) {
    # Simple quantiles/ECDF from examples(plot)
    x <- sort(rnorm(47))
    qplot(x, 1:47, geom="step")
    plot(x, type="s")
    
  }
})
