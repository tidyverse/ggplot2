\name{ScaleLinetype}
\alias{scale_linetype}
\alias{ScaleLinetype}
\title{scale_linetype}
\description{Scale for line patterns}
\details{
This page describes \code{\link{scale_linetype}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_linetype(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/scale_linetype.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    ec_scaled <- data.frame(
      date = economics$date, 
      rescaler(economics[, -(1:2)], "range")
    )
    ecm <- melt(ec_scaled, id = "date")
    
    qplot(date, value, data=ecm, geom="line", group=variable)
    qplot(date, value, data=ecm, geom="line", linetype=variable)
    qplot(date, value, data=ecm, geom="line", colour=variable)
    
    # The linetype scale currently has no options, so there's
    # no point in adding it manually
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
