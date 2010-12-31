\name{ggstructure}
\alias{ggstructure}
\title{A plot which aims to reveal gross structural anomalies in the data.}
\usage{ggstructure(data, scale="rank")}

\description{
  A plot which aims to reveal gross structural anomalies in
  the data.
}
\arguments{
  \item{data}{data set to plot}
  \item{scale}{type of scaling to use.  See \code{\link[reshape]{rescaler}}
for options}
}
\examples{ggstructure(mtcars)}
