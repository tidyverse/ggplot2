\name{StatSum}
\alias{stat_sum}
\alias{StatSum}
\title{stat_sum}
\description{Sum unique values.  Useful for overplotting on scatterplots}
\details{
This page describes \code{\link{stat_sum}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with stat_sum.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{stat_sum(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{y}: y position (\strong{required}) 
  \item \code{size}: size 
}
}
\usage{stat_sum(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item ggfluctuation: Fluctuation diagram, which is very similar
  \item \url{http://had.co.nz/ggplot/stat_sum.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
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
    
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
