\name{coord_cartesian}
\alias{coord_cartesian}
\title{Cartesian coordinates.}

\description{
  Cartesian coordinates.
}

\details{
  The Cartesian coordinate system is the most familiar, and
  common, type of coordinate system. Setting limits on the
  coordinate system will zoom the plot (like you're looking
  at it with a magnifying class), and will not change the
  underlying data like setting limits on a scale will.
}
\arguments{
  \item{xlim}{limits for the x axis}
  \item{ylim}{limits for the y axis}
  \item{wise}{If \code{TRUE} will wisely expand the actual range of the plot
a little, in the way that setting the limits on the scales does}
}
\examples{# There are two ways of zooming the plot display: with scales or 
# with coordinate systems.  They work in two rather different ways.

(p <- qplot(disp, wt, data=mtcars) + geom_smooth())

# Setting the limits on a scale will throw away all data that's not
# inside these limits.  This is equivalent to plotting a subset of
# the original data
p + scale_x_continuous(limits = c(325, 500))

# Setting the limits on the coordinate system performs a visual zoom
# the data is unchanged, and we just view a small portion of the original
# plot.  See how the axis labels are the same as the original data, and 
# the smooth continue past the points visible on this plot.
p + coord_cartesian(xlim = c(325, 500))

# You can see the same thing with this 2d histogram
(d <- ggplot(diamonds, aes(carat, price)) + 
stat_bin2d(bins = 25, colour="grey50"))

# When zooming the scale, the we get 25 new bins that are the same
# size on the plot, but represent smaller regions of the data space
d + scale_x_continuous(limits = c(0, 2))

# When zooming the coordinate system, we see a subset of original 50 bins, 
# displayed bigger
d + coord_cartesian(xlim = c(0, 2))}
