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
}



\examples{ggstructure(mtcars)}
\keyword{hplot}
