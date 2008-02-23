\name{ScaleManual}
\alias{scale_manual}
\alias{ScaleManual}
\alias{scale_colour_manual}
\alias{scale_fill_manual}
\alias{scale_size_manual}
\alias{scale_shape_manual}
\alias{scale_linetype_manual}
\title{scale_manual}
\description{Simple way of manually controlling scale}
\details{
This page describes \code{\link{scale_manual}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_manual(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/scale_manual.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl))

    p + scale_colour_manual(values = c("red","blue", "green"))
    p + scale_colour_manual(values = c("8" = "red","4" = "blue","6" = "green"))
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
