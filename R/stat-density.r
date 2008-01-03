StatDensity <- proto(Stat, {
  calculate <- function(., data, scales, adjust=1, kernel="gaussian", trim=FALSE, ...) {
    data <- data[!is.na(data$x), ]
    
    n <- nrow(data)
    if (is.null(data$weight)) data$weight <- rep(1, n) / n

    range <- scales$get_scales("x")$frange()
    xgrid <- seq(range[1], range[2], length=200)
    
    dens <- density(data$x, adjust=adjust, kernel=kernel, weight=data$weight, from=range[1], to=range[2])
    densdf <- as.data.frame(dens[c("x","y")])

    densdf$scaled <- densdf$y / max(densdf$y, na.rm = TRUE)
    if (trim) densdf <- subset(densdf, x > min(data$x, na.rm = TRUE) & x < max(data$x, na.rm = TRUE))
  
    densdf$count <- densdf$y * n
    rename(densdf, c(y = "density"))
  }

  objname <- "density" 
  desc <- "Density estimation, 1D"
  icon <- function(.) GeomDensity$icon()

  desc_params <- list(
    adjust = "see ?density for details",
    kernel = "kernel used for density estimation, see \\code{\\link{density}} for details"
  )
  desc_outputs <- list(
    density = "density estimate",
    count = "density * number of points - useful for stacked density plots",
    scaled = "density estimate, scaled to maximum of 1"
  )

  seealso <- list(
    stat_bin = "for the histogram",
    density = "for details of the algorithm used"
  )
  
  default_geom <- function(.) GeomArea
  default_aes <- function(.) aes(y = ..density.., fill=NA)
  required_aes <- c("x")
  

  examples <- function(.) {
    m <- ggplot(movies, aes(x=rating))
    m + geom_density()
    
    # Adjust parameters
    m + geom_density(kernel = "rectangular")
    m + geom_density(kernel = "biweight") 
    m + geom_density(kernel = "epanechnikov")
    m + geom_density(adjust=1/5) # Very rough
    m + geom_density(adjust=5) # Very smooth
    
    # Adjust aesthetics
    m + geom_density(aes(fill=factor(Drama)), size=2)
    # Scale so peaks have same height:
    m + geom_density(aes(fill=factor(Drama), y = ..scaled..), size=2)

    m + geom_density(colour="darkgreen", size=2)
    m + geom_density(colour="darkgreen", size=2, fill=NA)
    m + geom_density(colour="darkgreen", size=2, fill="green")
    
    # Change scales
    (m <- ggplot(movies, aes(x=votes)) + geom_density(trim = TRUE))
    m + scale_x_log10()
    m + coord_trans(x="log10")
    m + scale_x_log10() + coord_trans(x="log10")
    
    # Also useful with
    m + stat_bin()

    # Stacked density plots
    # If you want to create a stacked density plot, you need to use
    # the 'count' (density * n) variable instead of the default density
    
    # Loses marginal densities
    qplot(rating, ..density.., data=movies, geom="density", fill=mpaa, position="stack")
    # Preserves marginal densities
    qplot(rating, ..count.., data=movies, geom="density", fill=mpaa, position="stack")
    
    # You can use position="fill" to produce a conditional density estimate
    qplot(rating, ..count.., data=movies, geom="density", fill=mpaa, position="fill")

    # Need to be careful with weighted data
    m <- ggplot(movies, aes(x=rating, weight=votes))
    m + geom_histogram(aes(y = ..count..)) + geom_density(fill=NA)

    m <- ggplot(movies, aes(x=rating, weight=votes/sum(votes)))
    m + geom_histogram(aes(y=..density..)) + geom_density(fill=NA, colour="black")

    movies$decade <- round_any(movies$year, 10)
    m <- ggplot(movies, aes(x=rating, colour=decade, group=decade)) 
    m + geom_density(fill=NA)
    m + geom_density(fill=NA) + aes(y = ..count..)
    
    # Use qplot instead
    qplot(length, data=movies, geom="density", weight=rating)
    qplot(length, data=movies, geom="density", weight=rating/sum(rating))
  }  
})
