\name{gtable}
\alias{gtable}
\title{Create a new table grid.}
\usage{
  gtable(grobs = list(), layout = NULL, widths = list(),
  heights = list(), respect = FALSE, name = "layout")
}
\arguments{
  \item{grobs}{a list of grobs}

  \item{layout}{a data frame with one row for each grob,
  and columns \code{t}, \code{r}, \code{b}, \code{l} giving
  top, right, bottom and left positions respectively,
  \code{clip} a string, either \code{"on"}, \code{"off"} or
  \code{"inherit"}, and \code{name}, a character vector
  used to name each grob as it is plotted.}

  \item{widths}{a unit vector giving the width of each
  column}

  \item{height}{a unit vector giving the height of each
  row}

  \item{respect}{a logical vector of length 1: should the
  aspect ratio of height and width specified in null units
  be respected.  See \code{\link{grid.layout}} for more
  details}

  \item{name}{a string giving the name of the table. This
  is used to name the layout viewport}
}
\description{
  A table grid captures all the information needed to
  layout grobs in a table structure. It supports row and
  column spanning, and offers some tools to automatically
  figure out correct dimensions.
}
\details{
  Each grob is put in its own viewport - grobs in the same
  location are not combined into one cell. Each grob takes
  up the entire cell viewport so justification control is
  not available.

  It constructs both the viewports and the gTree needed to
  display the table.
}
\keyword{internal}

