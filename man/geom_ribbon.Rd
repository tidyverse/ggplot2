\name{geom_ribbon}
\alias{geom_ribbon}
\title{Ribbons, y range with continuous x values.}
\usage{geom_ribbon(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", na.rm = FALSE, ...)}

\description{
  Ribbons, y range with continuous x values.
}
\seealso{\code{\link{geom_bar}} for discrete intervals (bars),
\code{\link{geom_linerange}} for discrete intervals (lines),
\code{\link{geom_polygon}} for general polygons"}
\examples{# Generate data
huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
huron$decade <- round_any(huron$year, 10, floor)

h <- ggplot(huron, aes(x=year))

h + geom_ribbon(aes(ymin=0, ymax=level))
h + geom_area(aes(y = level))

# Add aesthetic mappings
h + geom_ribbon(aes(ymin=level-1, ymax=level+1))
h + geom_ribbon(aes(ymin=level-1, ymax=level+1)) + geom_line(aes(y=level))

# Take out some values in the middle for an example of NA handling
huron[huron$year > 1900 & huron$year < 1910, "level"] <- NA
h <- ggplot(huron, aes(x=year))
h + geom_ribbon(aes(ymin=level-1, ymax=level+1)) + geom_line(aes(y=level))

# Another data set, with multiple y's for each x
m <- ggplot(movies, aes(y=votes, x=year)) 
(m <- m + geom_point())

# The default summary isn't that useful
m + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max")
m + stat_summary(geom="ribbon", fun.data="median_hilow")

# Use qplot instead
qplot(year, level, data=huron, geom=c("area", "line"))}

