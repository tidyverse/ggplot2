\name{map_data}
\alias{map_data}
\title{Map data}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Convert map to data frame
}
\usage{map_data(map, region = ".")}
\arguments{
\item{map}{map name}
\item{region}{region name}
}

\details{}

\examples{if (require(maps)) {
states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group, fill = assault,
geom="polygon")
qplot(long, lat, data = choro, group = group, fill = assault / murder,
geom="polygon")
}}

