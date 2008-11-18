\name{scale_continuous}
\alias{scale_continuous}
\alias{scale_x_continuous}
\alias{scale_y_continuous}
\alias{ScaleContinuous}
\title{scale\_continuous}
\description{Continuous position scale}
\details{
This page describes scale\_continuous, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_x_continuous(name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans=NULL, expand=c(0.05, 0), minor_breaks=NULL, formatter=scientific, ...)
scale_y_continuous(name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans=NULL, expand=c(0.05, 0), minor_breaks=NULL, formatter=scientific, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{trans}{a transformer to use}
 \item{expand}{numeric vector of length 2, giving multiplicative and additive expansion factors}
 \item{minor_breaks}{NULL}
 \item{formatter}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \code{\link{scale_discrete}}: Discrete position scales
  \item \url{http://had.co.nz/ggplot2/scale_continuous.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
(m <- qplot(rating, votes, data=subset(movies, votes > 1000)))

# Manipulating the default position scales lets you:

#  * change the axis labels
m + scale_y_continuous("number of votes")
m + scale_y_continuous(expression(votes^alpha))

#  * modify the axis limits
m + scale_y_continuous(limits=c(0, 5000))
m + scale_y_continuous(limits=c(1000, 10000))
m + scale_x_continuous(limits=c(7, 8))

# you can also use the short hand functions xlim and ylim
m + ylim(0, 5000)
m + ylim(1000, 10000)
m + xlim(7, 8)

#  * choose where the ticks appear
m + scale_x_continuous(breaks=1:10)
m + scale_x_continuous(breaks=c(1,3,7,9))

#  * manually label the ticks
m + scale_x_continuous(breaks=c(2,5,8), labels=c("two", "five", "eight"))
m + scale_x_continuous(breaks=c(2,5,8), labels=c("horrible", "ok", "awesome"))
m + scale_x_continuous(breaks=c(2,5,8), labels=expression(Alpha, Beta, Omega))

# There are also a wide range of transformations you can use:
m + scale_y_log10()
m + scale_y_log()
m + scale_y_log2()
m + scale_y_sqrt()
m + scale_y_reverse()
# see ?transformer for a full list

# You can control the formatting of the labels with the formatter
# argument.  Some common formats are built in:
x <- rnorm(10) * 100000
y <- seq(0, 1, length = 10)
p <- qplot(x, y)
p + scale_y_continuous(formatter = "percent")
p + scale_y_continuous(formatter = "dollar")
p + scale_x_continuous(formatter = "comma")

# qplot allows you to do some of this with a little less typing:
#   * axis limits
qplot(rating, votes, data=movies, ylim=c(1e4, 5e4))
#   * axis labels
qplot(rating, votes, data=movies, xlab="My x axis", ylab="My y axis")
#   * log scaling
qplot(rating, votes, data=movies, log="xy")
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
