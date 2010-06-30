\name{borders}
\alias{borders}
\title{Map borders.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create a layer of map borders
}
\usage{borders(database = "world", regions = ".", fill = NA, colour = "grey50", ...)}
\arguments{
\item{database}{map data, see \code{\link[maps]{map}} for details}
\item{regions}{map region}
\item{fill}{fill colour}
\item{colour}{border colour}
\item{...}{other arguments passed on to \code{\link{geom_polygon}}}
}



\examples{if (require(maps)) {
ia <- map_data("county", "iowa")
mid_range <- function(x) mean(range(x))
seats <- ddply(ia, .(subregion), plyr::colwise(mid_range, .(lat, long)))
ggplot(ia, aes(long, lat)) + 
geom_polygon(aes(group = group), fill = NA, colour = "grey60") +
geom_text(aes(label = subregion), data = seats, size = 2, angle = 45)

data(us.cities)
capitals <- subset(us.cities, capital == 2)
ggplot(capitals, aes(long, lat)) +
borders("state") + 
geom_point(aes(size = pop)) + 
scale_area()
}}
\keyword{hplot}
