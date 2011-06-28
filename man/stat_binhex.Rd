\name{stat_binhex}
\alias{stat_binhex}
\title{Bin 2d plane into hexagons.}
\usage{stat_binhex(mapping, data, geom="hex", position="identity",
    bins=30, na.rm=FALSE, ...)}

\description{
  Bin 2d plane into hexagons.
}
\seealso{\code{\link{stat_bin2d}} for rectangular binning}
\examples{d <- ggplot(diamonds, aes(carat, price))
d + stat_binhex()
d + geom_hex()

# You can control the size of the bins by specifying the number of
# bins in each direction:
d + stat_binhex(bins = 10)
d + stat_binhex(bins = 30)

# Or by specifying the width of the bins
d + stat_binhex(binwidth = c(1, 1000))
d + stat_binhex(binwidth = c(.1, 500))

# With qplot
qplot(x, y, data = diamonds, geom="hex", xlim = c(4, 10), ylim = c(4, 10))
qplot(x, y, data = diamonds, geom="hex", xlim = c(4, 10), ylim = c(4, 10),
binwidth = c(0.1, 0.1))}
