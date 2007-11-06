\name{guides_basic}
\alias{guides_basic}
\title{Default guides}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Generate default guides (axes, and labels).
}
\usage{guides_basic(plot, scales, coordinates)}
\arguments{
\item{plot}{plot object}
\item{scales}{plot scales}
\item{coordinates}{}
}
\value{
 \item{background list of grobs to appear in background}
 \item{grid grobs that form background grob}
 \item{axes\_v vertical axes}
 \item{axes\_h horizontal axes}
 \item{labels row and column labels}
}
\details{The default guides built for a plot are:

\item the background colour over the whole plotting area (white)
\item within each a panel a gray background with white gridlines
(see \\code{\\link{ggopt}}) to change)
\item vertical and horizontal axes (appearance control by options
to the position scales)
\item facetting labels (see \\code{\\link{ggopt}}) to change default
colours etc)

To decouple plot construction from the objects that are placed within it,
each of the grobs produced by this function uses a \\code{\\link[grid]{vpPath}}.}

\examples{}
\keyword{hplot}
\keyword{internal}
