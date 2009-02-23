\name{fortify.map}
\alias{fortify.map}
\title{Fortify a map}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Fortify method for map objects
}
\usage{fortify.map(model, data, ...)}
\arguments{
\item{model}{map object}
\item{data}{ignored}
\item{...}{ignored}
}

\details{This function turns a map into a data frame than can more easily be
plotted with ggplot2.}

\examples{if (require(maps)) {
ca <- map_data("county", "ca")
qplot(long, lat, data = ca, geom="polygon", group = group)
tx <- map_data("county", "texas")
qplot(long, lat, data = tx, geom="polygon", group = group, 
colour = I("white"))
}}

