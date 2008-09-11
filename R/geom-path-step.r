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
  
  draw <- function(., data, scales, coordinates, direction = "hv", ...) {
    data <- stairstep(data, direction)
    GeomPath$draw(data, scales, coordinates, ...)
  }

  desc_params <- list(
    direction = "direction of stairs: 'vh' for vertical then horizontal, or 'hv' for horizontal then vertical"
  )
  default_stat <- function(.) StatIdentity
  
  examples <- function(.) {
    # Simple quantiles/ECDF from examples(plot)
    x <- sort(rnorm(47))
    qplot(seq_along(x), x, geom="step")
    
    # Steps go vertically, then horizontally
    qplot(seq_along(x), x, geom="step", direction = "hv")
    plot(x, type = "s")
    # Steps go horizontally, then vertically (default)
    qplot(seq_along(x), x, geom="step", direction = "vh")
    plot(x, type = "S")
  }
})


stairstep <- function(data, direction="hv") {
  direction <- match.arg(direction, c("hv", "vh"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)
  
  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each=2))
  } else {
      ys <- rep(1:n, each = 2)[-2*n]
      xs <- c(1, rep(2:n, each=2))
  }
  
  data.frame(
    x = data$x[xs],
    y = data$y[ys],
    data[xs, setdiff(names(data), c("x", "y"))]
  ) 
}
