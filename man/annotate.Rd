\name{annotate}
\alias{annotate}
\title{Annotate a plot...}
\usage{annotate(geom, x, y, xmin, xmax, ymin, ymax, ...)}

\description{
  Annotate a plot
}
\arguments{
  \item{name}{of geom to use for annotation}
  \item{x}{x position}
  \item{y}{y position}
  \item{xmin}{xmin position}
  \item{ymin}{ymin position}
  \item{xmax}{xmax position}
  \item{ymax}{ymax position}
  \item{...}{other arguments passed to geom as parameters}
}
\examples{annotate("text", x = 0, y = 0, label = "title")}
