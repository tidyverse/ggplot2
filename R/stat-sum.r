StatSum <- proto(Stat, {
  default_aes <- function(.) aes(size = ..prop..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomPoint
  icon <- function(.) textGrob(expression(Sigma), gp=gpar(cex=4))
  
  calculate_groups <- function(., data, scales, ...) {
    if (is.null(data$weight)) data$weight <- 1
    
    counts <- ddply(data, .(x, y, group), function(df) {
      cols <- names(df)[sapply(df, function(x) length(unique(x)) == 1)]
      data.frame(n = sum(df$weight), df[1, cols, drop = FALSE])
    })
    counts <- ddply(counts, .(group), transform, prop = n / sum(n))
    counts$group <- 1

    counts
  }
  
  objname <- "sum" 
  desc <- "Sum unique values.  Useful for overplotting on scatterplots"
  seealso <- list(
    "ggfluctuation" = "Fluctuation diagram, which is very similar"
    # "round_any" = "for rounding continuous observations to desired level of accuracy"
  )
  desc_outputs <- list(
    "n" = "number of observations at position",
    "prop" = "percent of points in that panel at that position"
  )
  
  examples <- function(.) {
    d <- ggplot(diamonds, aes(x = cut, y = clarity))
    # Need to control which group proportion calculated over
    # Overall proportion
    d + stat_sum(aes(group = 1))
    d + stat_sum(aes(group = 1)) + scale_size(to = c(3, 10))
    d + stat_sum(aes(group = 1)) + scale_area(to = c(3, 10))
    # by cut
    d + stat_sum(aes(group = cut))
    d + stat_sum(aes(group = cut, colour = cut))
    # by clarity
    d + stat_sum(aes(group = clarity))
    d + stat_sum(aes(group = clarity, colour = cut))
    
    # Instead of proportions, can also use sums
    d + stat_sum(aes(size = ..n..))

    # Can also weight by another variable
    d + stat_sum(aes(group = 1, weight = price))
    d + stat_sum(aes(group = 1, weight = price, size = ..n..))
    
    # Or using qplot
    qplot(cut, clarity, data = diamonds)
    qplot(cut, clarity, data = diamonds, stat = "sum", group = 1)    
  }
  
})
