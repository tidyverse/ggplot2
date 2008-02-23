\name{StatSpoke}
\alias{stat_spoke}
\alias{StatSpoke}
\title{stat_spoke}
\description{Convert angle and radius to xend and yend}
\details{
This page describes \code{\link{stat_spoke}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with stat_spoke.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{stat_spoke(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{y}: y position (\strong{required}) 
  \item \code{angle}: angle (\strong{required}) 
  \item \code{radius}: NULL (\strong{required}) 
  \item \code{xend}: NULL 
  \item \code{yend}: NULL 
}
}
\usage{stat_spoke(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/stat_spoke.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    df <- expand.grid(x = 1:10, y=1:10)
    df$angle <- runif(100, 0, 2*pi)
    df$speed <- runif(100, 0, 0.5)
    
    qplot(x, y, data=df) + stat_spoke(aes(angle=angle), radius = 0.5)
    qplot(x, y, data=df) + stat_spoke(aes(angle=angle, radius=speed))
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
