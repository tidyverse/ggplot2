\name{StatDensity}
\alias{stat_density}
\alias{StatDensity}
\title{stat_density}
\description{Density estimation, 1D}
\details{
This page describes \code{\link{stat_density}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with stat_density.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{stat_density(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{y}: y position 
  \item \code{fill}: internal colour 
}
}
\usage{stat_density(...)}
\arguments{
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \code{\link{stat_bin}}: for the histogram
  \item density: for details of the algorithm used
  \item \url{http://had.co.nz/ggplot/stat_density.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
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
    
    # Make a volcano plot
    ggplot(diamonds, aes(x = price)) + geom_density(aes(min = -..density.., adjust= 0.5),fill="grey50", colour=NA) + facet_grid(. ~ cut) + coord_flip() 

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
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
