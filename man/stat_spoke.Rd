\name{stat_spoke}
\alias{stat_spoke}
\title{Convert angle and radius to xend and yend.}
\usage{stat_spoke(mapping, data, geom="segment",
    position="identity", ...)}

\description{
  Convert angle and radius to xend and yend.
}
\value{a data.frame with additional columns
\item{xend}{x position of end of line segment}
\item{yend}{x position of end of line segment}}
\examples{df <- expand.grid(x = 1:10, y=1:10)
df$angle <- runif(100, 0, 2*pi)
df$speed <- runif(100, 0, 0.5)

qplot(x, y, data=df) + stat_spoke(aes(angle=angle), radius = 0.5)
last_plot() + scale_y_reverse()

qplot(x, y, data=df) + stat_spoke(aes(angle=angle, radius=speed))}
