\name{legends}
\alias{legends}
\title{Legends}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create and arrange legends for all scales.
}
\usage{legends(scales, scale_usage, horizontal = FALSE)}
\arguments{
\item{scales}{scales object}
\item{scale_usage}{direction of scales, vertical by default}
\item{horizontal}{}
}
\value{frameGrob, or NULL if no legends}
\details{This function gathers together all of the legends produced by
the scales that make up the plot and organises them into a
\\code{\\link[grid]{frameGrob}}.

If there are no legends to create, this function will return \\code{NULL}}

\examples{}
\keyword{hplot}
\keyword{internal}
