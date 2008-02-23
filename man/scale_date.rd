\name{ScaleDate}
\alias{scale_date}
\alias{ScaleDate}
\alias{scale_x_date}
\alias{scale_y_date}
\title{scale_date}
\description{Continuous scale for date variables}
\details{
Currently somewhat broken due to lack of support for dates in R.

This page describes \code{\link{scale_date}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_date(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \code{\link{scale_discrete}}: Discrete position scales
  \item \url{http://had.co.nz/ggplot/scale_date.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # We'll start by creating some nonsense data with dates
    df <- data.frame(
      date = seq(Sys.Date(), len=100, by="1 day")[sample(100, 50)],
      price = runif(50)
    )
    df <- df[order(df$date), ]
    dt <- qplot(date, price, data=df, geom="line")
    dt$aspect.ratio <- 1/4
    
    # We can control the format of the labels, and the frequency of 
    # the major and minor tickmarks.  See ?format.Date and ?seq.Date 
    # for more details.
    dt + scale_x_date()
    dt + scale_x_date(format="%m/%d")
    dt + scale_x_date(format="%W")
    dt + scale_x_date(major="months", minor="weeks", format="%b")
    dt + scale_x_date(major="months", minor="2 days", format="%b")
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
    
    qplot(date, psavert, data=economics, geom="path", xlim=c(as.Date("2000-1-1"),NA) )
    qplot(date, psavert, data=economics, geom="path", xlim=c(as.Date("2005-1-1"),NA) )
    qplot(date, psavert, data=economics, geom="path", xlim=c(as.Date("2007-1-1"),NA) )
    # cf
    qplot(date, psavert, data=subset(economics, date > "2000-1-1"), geom="path")
    qplot(date, psavert, data=subset(economics, date > "2005-1-1"), geom="path")
    qplot(date, psavert, data=subset(economics, date > "2006-1-1"), geom="path")
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
