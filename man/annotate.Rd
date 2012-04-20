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
  This function adds geoms to a plot. Unlike typical a geom
  function, the properties of the geoms are not mapped from
  variables of a data frame. The aesthetic properties, such
  as x-position, are passed in as vectors.
}
\details{
  This is useful for adding individual labels to a plot,
  and it can also be used for drawing geoms from data in
  vectors.

  Note that all position aesthetics are scaled (i.e. they
  will expand the limits of the plot so they are visible),
  but all other aesthetics are set.
}
\examples{
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + annotate("text", x = 4, y = 25, label = "Some text")
p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21, alpha = .2)
p + annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25, colour = "blue")
p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,
  colour = "red", size = 1.5)
}

