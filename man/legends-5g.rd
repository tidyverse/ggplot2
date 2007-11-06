\name{legends}
\alias{legends}
\title{Legends}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create and arrange legends for all scales.
}
\usage{legends(scales, horizontal = FALSE)}
\arguments{
\item{scales}{scales object}
\item{horizontal}{direction of scales, vertical by default}
}
\value{frameGrob, or NULL if no legends}
\details{This function gathers together all of the legends produced by
the scales that make up the plot and organises them into a
\\code{\\link[grid]{frameGrob}}.

If there are no legends to create, this function will return \\code{NULL}}

\examples{}
\keyword{hplot}
\keyword{internal}
