\name{ScaleContinuous}
\alias{scale_continuous}
\alias{ScaleContinuous}
\alias{scale_x_continuous}
\alias{scale_y_continuous}
\alias{scale_z_continuous}
\alias{scale_xend_continuous}
\alias{scale_yend_continuous}
\title{scale_continuous}
\description{Continuous position scale}
\details{
This page describes \code{\link{scale_continuous}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_continuous(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \code{\link{scale_discrete}}: Discrete position scales
  \item \url{http://had.co.nz/ggplot/scale_continuous.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    (m <- qplot(rating, votes, data=movies))
    
    # Manipulating the default position scales lets you:

    #  * change the axis labels
    m + scale_y_continuous("number of votes")
    m + scale_y_continuous(expression(votes^alpha))
    
    #  * modify the axis limits
    m + scale_y_continuous(limits=c(NA, 5000))
    m + scale_y_continuous(limits=c(1000, NA))
    m + scale_x_continuous(limits=c(7, 8))

    #  * choose where the ticks appear
    m + scale_x_continuous(breaks=1:10)
    m + scale_x_continuous(breaks=c(1,3,7,9))

    #  * manually label the ticks
    m + scale_x_continuous(breaks=c(1,5,10), labels=c("one", "five", "ten"))
    m + scale_x_continuous(breaks=c(1,5,10), labels=c("horrible", "ok", "awesome"))
    m + scale_x_continuous(breaks=c(1,5,10), labels=expression(Alpha, Beta, Omega))
    
    # There are also a wide range of transformations you can use:
    m + scale_y_log10()
    m + scale_y_log()
    m + scale_y_log2()
    m + scale_y_sqrt()
    # see ?transformer for a full list
    
    # qplot allows you to do some of this with a little less typing:
    #   * axis limits
    qplot(rating, votes, data=movies, xlim=c(5,10), ylim=c(50000, NA))
    #   * axis labels
    qplot(rating, votes, data=movies, xlab="My x axis", ylab="My y axis")
    #   * log scaling
    qplot(rating, votes, data=movies, log="xy")
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
