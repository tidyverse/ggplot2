\name{stat_summary}
\alias{stat_summary}
\title{Summarise y values at every unique x.}
\usage{
  stat_summary(mapping = NULL, data = NULL,
    geom = "pointrange", position = "identity", ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{geom}{The geometric object to use display the data}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\value{
  a data.frame with additional columns:
  \item{fun.data}{Complete summary function. Should take
  data frame as input and return data frame as output}
  \item{fun.ymin}{ymin summary function (should take
  numeric vector and return single number)} \item{fun.y}{y
  summary function (should take numeric vector and return
  single number)} \item{fun.ymax}{ymax summary function
  (should take numeric vector and return single number)}
}
\description{
  \code{stat_summary} allows for tremendous flexibilty in
  the specification of summary functions.  The summary
  function can either operate on a data frame (with
  argument name \code{fun.data}) or on a vector
  (\code{fun.y}, \code{fun.ymax}, \code{fun.ymin}).
}
\details{
  A simple vector function is easiest to work with as you
  can return a single number, but is somewhat less
  flexible.  If your summary function operates on a
  data.frame it should return a data frame with variables
  that the geom can use.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat",
  "summary")}
}
\examples{
\donttest{
# Basic operation on a small dataset
d <- qplot(cyl, mpg, data=mtcars)
d + stat_summary(fun.data = "mean_cl_boot", colour = "red")

p <- qplot(cyl, mpg, data = mtcars, stat="summary", fun.y = "mean")
p
# Don't use ylim to zoom into a summary plot - this throws the
# data away
p + ylim(15, 30)
# Instead use coord_cartesian
p + coord_cartesian(ylim = c(15, 30))

# You can supply individual functions to summarise the value at
# each x:

stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 3, ...)
}

d + stat_sum_single(mean)
d + stat_sum_single(mean, geom="line")
d + stat_sum_single(median)
d + stat_sum_single(sd)

d + stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max,
  colour = "red")

d + aes(colour = factor(vs)) + stat_summary(fun.y = mean, geom="line")

# Alternatively, you can supply a function that operates on a data.frame.
# A set of useful summary functions is provided from the Hmisc package:

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data=fun, colour="red", geom=geom, width=0.2, ...)
}

d + stat_sum_df("mean_cl_boot")
d + stat_sum_df("mean_sdl")
d + stat_sum_df("mean_sdl", mult=1)
d + stat_sum_df("median_hilow")

# There are lots of different geoms you can use to display the summaries

d + stat_sum_df("mean_cl_normal")
d + stat_sum_df("mean_cl_normal", geom = "errorbar")
d + stat_sum_df("mean_cl_normal", geom = "pointrange")
d + stat_sum_df("mean_cl_normal", geom = "smooth")

# Summaries are more useful with a bigger data set:
mpg2 <- subset(mpg, cyl != 5L)
m <- ggplot(mpg2, aes(x=cyl, y=hwy)) +
        geom_point() +
        stat_summary(fun.data = "mean_sdl", geom = "linerange",
                     colour = "red", size = 2, mult = 1) +
       xlab("cyl")
m
# An example with highly skewed distributions:
set.seed(596)
mov <- movies[sample(nrow(movies), 1000), ]
 m2 <- ggplot(mov, aes(x= factor(round(rating)), y=votes)) + geom_point()
 m2 <- m2 + stat_summary(fun.data = "mean_cl_boot", geom = "crossbar",
                         colour = "red", width = 0.3) + xlab("rating")
m2
# Notice how the overplotting skews off visual perception of the mean
# supplementing the raw data with summary statistics is _very_ important

# Next, we'll look at votes on a log scale.

# Transforming the scale means the data are transformed
# first, after which statistics are computed:
m2 + scale_y_log10()
# Transforming the coordinate system occurs after the
# statistic has been computed. This means we're calculating the summary on the raw data
# and stretching the geoms onto the log scale.  Compare the widths of the
# standard errors.
m2 + coord_trans(y="log10")
}
}
\seealso{
  \code{\link{geom_errorbar}},
  \code{\link{geom_pointrange}},
  \code{\link{geom_linerange}}, \code{\link{geom_crossbar}}
  for geoms to display summarised data
}

