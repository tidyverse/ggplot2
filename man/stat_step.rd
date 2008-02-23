\name{StatStep}
\alias{stat_step}
\alias{StatStep}
\title{stat_step}
\description{Create stair steps}
\details{
This page describes \code{\link{stat_step}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with stat_step.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{stat_step(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{y}: y position (\strong{required}) 
}
}
\usage{stat_step(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/stat_step.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # See geom_step for examples
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
