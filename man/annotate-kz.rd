\name{annotate}
\alias{annotate}
\title{Annotate a plot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Add annotations to a plot in a convenient manner
}
\usage{annotate(geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, ...)}
\arguments{
\item{geom}{name of geom to use for annotation}
\item{x}{x position}
\item{y}{y position}
\item{xmin}{xmin position}
\item{xmax}{ymin position}
\item{ymin}{xmax position}
\item{ymax}{ymax position}
\item{...}{... other arguments passed to geom as parameters}
}



\examples{annotate("text", x = 0, y = 0, label = "title")}
\keyword{internal}
