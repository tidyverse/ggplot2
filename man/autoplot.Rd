% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot.R
\name{autoplot}
\alias{autoplot}
\title{Create a complete ggplot appropriate to a particular data type}
\usage{
autoplot(object, ...)
}
\arguments{
\item{object}{an object, whose class will determine the behaviour of autoplot}

\item{...}{other arguments passed to specific methods}
}
\value{
a ggplot object
}
\description{
\code{autoplot()} uses ggplot2 to draw a particular plot for an object of a
particular class in a single command. This defines the S3 generic that
other classes and packages can extend.
}
\seealso{
Other plotting automation topics: 
\code{\link{autolayer}()},
\code{\link{automatic_plotting}},
\code{\link{fortify}()}
}
\concept{plotting automation topics}
