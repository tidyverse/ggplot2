\name{fortify.SpatialPolygonsDataFrame}
\alias{fortify.SpatialPolygonsDataFrame}
\alias{fortify.SpatialPolygons}
\alias{fortify.Polygons}
\alias{fortify.Polygon}
\alias{fortify.SpatialLinesDataFrame}
\alias{fortify.Lines}
\alias{fortify.Line}
\title{Fortify spatial polygons and lines}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Fortify method for a number of the class from the sp package.
}
\usage{fortify.SpatialPolygonsDataFrame(model, data, region = NULL, ...)}
\arguments{
\item{model}{SpatialPolygonsDataFrame}
\item{data}{not used}
\item{region}{name of variable to split up regions by}
\item{...}{not used}
}

\details{To figure out the correct variable name for region, inspect
\code{as.data.frame(model)}.}



