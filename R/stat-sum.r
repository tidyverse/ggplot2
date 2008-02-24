StatSum <- proto(Stat, {
  default_aes <- function(.) aes(size = ..prop..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomPoint
  icon <- function(.) textGrob(expression(Sigma), gp=gpar(cex=4))
  
  calculate <- function(., data, scales, ...) {
    if (is.null(data$weight)) data$weight <- 1
    
    counts <- as.data.frame(xtabs(weight ~ x + y, data), responseName="sum")
    counts$prop <- counts$sum / sum(counts$sum)
    counts$group <- 1
    
    counts
  }
  
  objname <- "sum" 
  desc <- "Sum unique values.  Useful for overplotting on scatterplots"
  seealso <- list(
    "ggfluctuation" = "Fluctuation diagram, which is very similar"
  )
  desc_outputs <- list(
    "sum" = "number of observations at position",
    "prop" = "percent of points in that panel at that position",
    "round_any" = "for rounding continuous observations to desired level of accuracy"
  )
  
  examples <- function(.) {
    d <- ggplot(diamonds, aes(x=cut, y=clarity))
    # Need to control which group proportion calculated over
    # Overall proportion
    d + stat_sum(aes(group=1))
    # by cut
    d + stat_sum(aes(group=cut))
    # by clarity
    d + stat_sum(aes(group=clarity))

    # Can also weight by another variable
    d + stat_sum(aes(group=1, weight = price))
    d + stat_sum(aes(group=1, weight = price, size = ..sum..))
    
    
    # Or using qplot
    qplot(cut, clarity, data=diamonds)
    qplot(cut, clarity, data=diamonds, stat="sum", group=1)
    
  }
  
})
