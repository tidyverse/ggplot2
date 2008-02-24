\name{ggstructure}
\alias{ggstructure}
\title{Structure plot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
A plot which aims to reveal gross structural anomalies in the data
}
\usage{ggstructure(data, scale = "rank")}
\arguments{
\item{data}{data set to plot}
\item{scale}{type of scaling to use.  See \code{\link[reshape]{rescaler}} for options}
\item{}{data set to plot}
\item{}{type of scaling to use.  See \code{\link[reshape]{rescaler}} for options}
}

\details{@arguments data set to plot
@arguments type of scaling to use.  See \code{\link[reshape]{rescaler}} for options
@keyword hplot
X ggstructure(mtcars)
Order plot
A plot to investigate the order in which observations were recorded.

@arguments data set to plot
@arguments type of scaling to use.  See \code{\link[reshape]{rescaler}} for options
@keyword hplot
Distribution plot
Experimental template}

\examples{ggstructure(mtcars)}
\keyword{hplot}
\keyword{hplot}
\keyword{internal}
