\name{guide_legends_box}
\alias{guide_legends_box}
\title{Legends}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create and arrange legends for all scales.
}
\usage{guide_legends_box(scales, layers, default_mapping, horizontal = FALSE, theme)}
\arguments{
\item{scales}{scales object}
\item{layers}{direction of scales, vertical by default}
\item{default_mapping}{}
\item{horizontal}{}
\item{theme}{}
}
\value{frameGrob, or NULL if no legends}
\details{This function gathers together all of the legends produced by
the scales that make up the plot and organises them into a
\\code{\\link[grid]{frameGrob}}.

If there are no legends to create, this function will return \\code{NULL}}

\examples{}
\keyword{hplot}
\keyword{internal}
