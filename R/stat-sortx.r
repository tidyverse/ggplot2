StatSort <- proto(Stat, {
  objname <- "sort" 
  desc <- "Sort in order of ascending x"
  default_geom <- function(.) GeomPath
  
  calculate_groups <- function(., data, scales, variable="x", ...) {
    as.data.frame(data)[order(data$group, data[[variable]]), ]
  }
  
  examples <- function(.) {
    # See geom_line for the main use of this
  }

})
