\name{StatContour}
\alias{stat_contour}
\alias{StatContour}
\title{stat_contour}
\description{Contours of 3d data}
\details{
This page describes \code{\link{stat_contour}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with stat_contour.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{stat_contour(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{y}: y position (\strong{required}) 
  \item \code{z}: z position (\strong{required}) 
  \item \code{group}: how observations are divided into different groups 
}
}
\usage{stat_contour(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/stat_contour.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # Generate data
    volcano3d <- rename(melt(volcano), c(X1="x", X2="y", value="z"))
    v <- ggplot(volcano3d, aes(x=x,y=y,z=z))
    v + stat_contour()

    # Add aesthetic mappings
    v + stat_contour(aes(size = ..level..))
    v + stat_contour(aes(colour = ..level..))

    # Change scale
    v + stat_contour(aes(colour = ..level..), size=2) + 
      scale_colour_gradient(low="brown", high="white")

    v + stat_contour() + scale_z_continuous(breaks=c(100, 150))
    v + stat_contour(size=0.5) + scale_z_continuous(breaks=seq(95, 195, by=2))
    v + stat_contour() + scale_z_log10()

    # Set aesthetics to fixed value
    v + stat_contour(colour="red")
    v + stat_contour(size=2, linetype=4)

    # Try different geoms
    v + stat_contour(geom="polygon", aes(fill=..level..))
    v + geom_tile(aes(fill=z)) + stat_contour()
    
    # Use qplot instead
    qplot(x, y, z, data=volcano3d, geom="contour")
    qplot(x, y, z, data=volcano3d, stat="contour", geom="path")
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
