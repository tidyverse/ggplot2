\name{geom_segment}
\alias{geom_segment}
\title{Single line segments.}
\usage{
  geom_segment(mapping = NULL, data = NULL, stat =
  "identity", position = "identity", ...)
}
\arguments{
  \item{arrow}{specification for arrow heads, as created by
  arrow()}
}
\description{
  Single line segments.
}
\examples{
library(grid) # needed for arrow function
p <- ggplot(seals, aes(x = long, y = lat))
(p <- p + geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat), arrow=arrow(length=unit(0.1,"cm"))))

if (require("maps")) {

xlim <- range(seals$long)
ylim <- range(seals$lat)
usamap <- data.frame(map("world", xlim = xlim, ylim = ylim, plot =
FALSE)[c("x","y")])
usamap <- rbind(usamap, NA, data.frame(map('state', xlim = xlim, ylim
= ylim, plot = FALSE)[c("x","y")]))
names(usamap) <- c("long", "lat")

p + geom_path(data = usamap) + scale_x_continuous(limits=xlim)
}

# You can also use geom_segment to recreate plot(type = "h") :
counts <- as.data.frame(table(x = rpois(100,5)))
counts$x <- as.numeric(as.character(counts$x))
with(counts, plot(x, Freq, type = "h", lwd = 10))

qplot(x, Freq, data = counts, geom="segment",
yend = 0, xend = x, size = I(10))
}
\seealso{
  \code{\link{geom_path}} and \code{\link{geom_line}} for
  multi- segment lines and paths.
}

