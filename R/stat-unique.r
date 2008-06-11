StatUnique <- proto(Stat, {
  objname <- "unique" 
  desc <- "Remove duplicates"
  
  default_geom <- function(.) GeomPoint
  
  calculate_groups <- function(., data, scales, ...) unique(data)
  
  desc_outputs <- list()
  
  examples <- function(.) {
    ggplot(mtcars, aes(x=vs, y=am)) + geom_point(colour="#00000010")
    ggplot(mtcars, aes(x=vs, y=am)) + geom_point(colour="#00000010", stat="unique")
  }
})
