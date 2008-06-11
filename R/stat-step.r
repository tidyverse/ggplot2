StatStep <- proto(Stat, {
  objname <- "step" 
  desc <- "Create stair steps"
  default_geom <- function(.) GeomPath
  icon <- function(.) GeomStep$icon()
  required_aes <- c("x", "y")
  
  calculate <- function(., data, scales, ...) {
    data <- as.data.frame(data)[order(data$x), ]
    n <- nrow(data)
    
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each=2))
    
    data.frame(
      x = data$x[xs],
      y = data$y[ys],
      data[xs, setdiff(names(data), c("x", "y"))]
    )
  }
  
  desc_outputs <- list()
  
  examples <- function(.) {
    # See geom_step for examples
  }

})
