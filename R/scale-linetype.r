ScaleLinetype <- proto(ScaleDiscrete, expr={
  common <- NULL
  .input <- .output <- "linetype"

  frange <- function(.) (1:4)[1:length(.$domain())]
  max_levels <- function(.) 4
  guide_legend_geom <- function(.) GeomPath
  
  # Documetation -----------------------------------------------

  objname <- "linetype"
  desc <- "Scale for line patterns"
  
  icon <- function(.) {
    gTree(children=gList(
      segmentsGrob(0, 0.25, 1, 0.25, gp=gpar(lty=1)),
      segmentsGrob(0, 0.50, 1, 0.50, gp=gpar(lty=2)),
      segmentsGrob(0, 0.75, 1, 0.75, gp=gpar(lty=3))
    ))
  }
  
  examples <- function() {
    # Fairly nonsensical example, but looking for better
    # sample data set
    qplot(wt, mpg, data=mtcars, geom="line", linetype=factor(cyl))
    
    # Force all points to be connected together
    qplot(wt, mpg, data=mtcars, geom="line", linetype=factor(cyl), group=1)
    
    # The linetype scale currently has no options, so there's
    # no point in adding it manually
  }
  
})
