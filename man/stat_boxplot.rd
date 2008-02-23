\name{StatBoxplot}
\alias{stat_boxplot}
\alias{StatBoxplot}
\title{stat_boxplot}
\description{Calculate components of box and whisker plot}
\details{
This page describes \code{\link{stat_boxplot}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with stat_boxplot.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{stat_boxplot(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{y}: y position (\strong{required}) 
}
}
\usage{stat_boxplot(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/stat_boxplot.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # See geom_boxplot for examples
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
