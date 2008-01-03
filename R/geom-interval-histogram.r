GeomHistogram <- proto(GeomBar, {
  objname <- "histogram"
  desc <- "Histogram"
  
  details <- ""
  advice <- ""
  
  default_stat <- function(.) StatBin
  default_pos <- function(.) PositionStack

  icon <- function(.) {
    y <- c(0.2, 0.3, 0.5, 0.6,0.2, 0.8, 0.5, 0.3)
    rectGrob(seq(0.1, 0.9, by=0.1), y, height=y, width=0.1, vjust=1, gp=gpar(fill="grey60", col=NA))
  }

  examples <- function(.) {
    m <- ggplot(movies, aes(x=rating))
    m + geom_histogram()
    m + geom_histogram(aes(y = ..density..)) + geom_density()

    m + geom_histogram(binwidth=1)
    m + geom_histogram(binwidth=0.5)
    m + geom_histogram(binwidth=0.1)
    
    # Add aesthetic mappings
    m + geom_histogram(aes(weight = votes))
    m + geom_histogram(aes(y = ..count..))
    m + geom_histogram(aes(fill = ..count..))

    # Change scales
    m + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient(low="green", high="red")
    
    m <- m + aes(x=votes)
    m + geom_histogram() + scale_x_log()
    m + geom_histogram() + scale_x_sqrt()
    
    # Change coordinate systems
    m + geom_histogram() + coord_trans(y = "sqrt")
      
    # Set aesthetics to fixed value
    m + geom_histogram(colour="darkgreen", fill="white") + aes(x=rating)
    
    # Use facets
    m <- m + facet_grid(Action ~ Comedy)
    m + geom_histogram()

    # Multiple histograms on the same graph
    # see ?position, ?position_fill, etc for more details
    ggplot(diamonds, aes(x=price)) + geom_bar()
    hist_cut <- ggplot(diamonds, aes(x=price, fill=cut))
    hist_cut + geom_bar() # defaults to stacking
    hist_cut + geom_bar(position="fill")
    hist_cut + geom_bar(position="dodge")
    
    # Use qplot instead
    qplot(rating, data=movies, geom="histogram")
    qplot(rating, data=movies, weight=votes, geom="histogram")

  }
})
