\name{stat_sum}
\alias{stat_sum}
\title{Sum unique values.}

\description{
  Sum unique values.  Useful for overplotting on
  scatterplots.
}
\seealso{\code{\link{ggfluctuation}} for a fluctuation diagram,}
\value{a data.frame with additional columns
\item{n}{number of observations at position}
\item{prop}{percent of points in that panel at that position}}
\examples{d <- ggplot(diamonds, aes(x = cut, y = clarity))
# Need to control which group proportion calculated over
# Overall proportion
d + stat_sum(aes(group = 1))
d + stat_sum(aes(group = 1)) + scale_size(range = c(3, 10))
d + stat_sum(aes(group = 1)) + scale_area(range = c(3, 10))
# by cut
d + stat_sum(aes(group = cut))
d + stat_sum(aes(group = cut, colour = cut))
# by clarity
d + stat_sum(aes(group = clarity))
d + stat_sum(aes(group = clarity, colour = cut))

# Instead of proportions, can also use sums
d + stat_sum(aes(size = ..n..))

# Can also weight by another variable
d + stat_sum(aes(group = 1, weight = price))
d + stat_sum(aes(group = 1, weight = price, size = ..n..))

# Or using qplot
qplot(cut, clarity, data = diamonds)
qplot(cut, clarity, data = diamonds, stat = "sum", group = 1)}
