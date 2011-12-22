\name{annotate}
\alias{annotate}
\title{Annotate a plot}
\usage{
  annotate(geom, x = NULL, y = NULL, xmin = NULL,
    xmax = NULL, ymin = NULL, ymax = NULL, ...)
}
\arguments{
  \item{geom}{name of geom to use for annotation}

  \item{x}{x position}

  \item{y}{y position}

  \item{xmin}{xmin position}

  \item{ymin}{ymin position}

  \item{xmax}{xmax position}

  \item{ymax}{ymax position}

  \item{...}{other arguments passed to geom as parameters}
}
\description{
  Annotate a plot
}
\examples{
annotate("text", x = 0, y = 0, label = "title")
}

