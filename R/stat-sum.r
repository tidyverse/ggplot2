StatSum <- proto(Stat, {
  default_aes <- function(.) aes(size = ..prop..)
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomPoint
  icon <- function(.) textGrob(expression(Sigma), gp=gpar(cex=4))
  
  calculate <- function(., data, scales, ...) {
    counts <- rename(dftable(data[, c("x", "y")]), c("Freq"="n"))
    counts$prop <- counts$n / sum(counts$n)
    counts$group <- 1
    
    counts
  }
  
  objname <- "sum" 
  desc <- "Sum unique values.  Useful for overplotting on scatterplots"
  seealso <- list(
    "ggfluctuation" = "Fluctuation diagram, which is very similar"
  )
  desc_outputs <- list(
    "n" = "number of observations at position",
    "prop" = "percent of points in that panel at that position"
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
    
    # Or using qplot
    qplot(cut, clarity, data=diamonds)
    qplot(cut, clarity, data=diamonds, stat="sum", group=1)
    
  }
  
})

dftable <- function(...) {
  df <- do.call("cbind", list(...))
  tab <- as.data.frame(table(df))
  
  cont <- c(sapply(df, is.numeric), FALSE)
  tab[cont] <- lapply(tab[cont], function(x) as.numeric(as.character(x)))
  
  tab
}

