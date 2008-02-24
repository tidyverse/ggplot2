\name{geom_polylist}
\alias{geom_polylist}
\title{Convert map files into useful data.frames}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
* point level data
}
\usage{geom_polylist(coords, ...)}
\arguments{
\item{coords}{}
\item{...}{}
}

\details{Utilities to merge together, along with data
try_require("maps","mapproj","maptools")


geom_polylist <- function(coords, ...) {
def <- function(x, default=1) if (is.null(x)) default else x

order <- as.integer(def(attr(coords, "plotOrder"), 1:length(coords)))
grobs <- lapply(order, function(i) geom_mappoly(coords[[i]], ...))

do.call(gList, grobs)
}}

\examples{}

