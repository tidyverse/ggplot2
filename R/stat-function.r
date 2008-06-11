StatFunction <- proto(Stat, {
  
  calculate <- function(., data, scales, fun, n=101, args = list(), ...) {
    range <- scales$get_scales("x")$output_set()
    xseq <- seq(range[1], range[2], length=n)
    
    data.frame(
      x = xseq,
      y = do.call(fun, c(list(xseq), args))
    )
  }

  objname <- "function" 
  desc <- "Superimpose a function "

  desc_params <- list(
    fun = "function to use",
    n = "number of points to interpolate along",
    args = "list of additional arguments to pass to fun"
  )
  
  desc_outputs <- list(
    x = "x's along a grid",
    y = "value of function evaluated at corresponding x"
  )

  default_geom <- function(.) GeomPath
  
  examples <- function(.) {
    x <- rnorm(100)
    qplot(x, geom="density") + stat_function(fun = dnorm, colour="red")
  }
  
})
