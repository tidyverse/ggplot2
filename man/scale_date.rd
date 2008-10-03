\name{scale_date}
\alias{scale_date}
\alias{scale_x_date}
\alias{scale_y_date}
\alias{ScaleDate}
\title{scale\_date}
\description{Position scale, date}
\details{
This page describes scale\_date, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_x_date(name=NULL, limits=NULL, major=NULL, minor=NULL, format=NULL, ...)
scale_y_date(name=NULL, limits=NULL, major=NULL, minor=NULL, format=NULL, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{major}{NULL}
 \item{minor}{NULL}
 \item{format}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \code{\link{scale_discrete}}: Discrete position scales
  \item \url{http://had.co.nz/ggplot2/scale_date.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
# We'll start by creating some nonsense data with dates
df <- data.frame(
  date = seq(Sys.Date(), len=100, by="1 day")[sample(100, 50)],
  price = runif(50)
)
df <- df[order(df$date), ]
dt <- qplot(date, price, data=df, geom="line") + opts(aspect.ratio = 1/4)

# We can control the format of the labels, and the frequency of 
# the major and minor tickmarks.  See ?format.Date and ?seq.Date 
# for more details.
dt + scale_x_date()
dt + scale_x_date(format="%m/%d")
dt + scale_x_date(format="%W")
dt + scale_x_date(major="months", minor="weeks", format="%b")
dt + scale_x_date(major="months", minor="3 days", format="%b")
dt + scale_x_date(major="years", format="%b-%Y")

# The date scale will attempt to pick sensible defaults for 
# major and minor tick marks
qplot(date, price, data=df[1:10,], geom="line")
qplot(date, price, data=df[1:4,], geom="line")

df <- data.frame(
  date = seq(Sys.Date(), len=1000, by="1 day"),
  price = runif(500)
)
qplot(date, price, data=df, geom="line")

# A real example using economic time series data
qplot(date, psavert, data=economics) 
qplot(date, psavert, data=economics, geom="path") 

end <- max(economics$date)
last_plot() + scale_x_date(lim = c(as.Date("2000-1-1"), end))
last_plot() + scale_x_date(lim = c(as.Date("2005-1-1"), end))
last_plot() + scale_x_date(lim = c(as.Date("2006-1-1"), end))
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
