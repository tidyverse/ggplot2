\name{ggmissing}
\alias{ggmissing}
\title{Missing values plot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create a plot to illustrate patterns of missing values
}
\usage{ggmissing(data, avoid="stack", order=TRUE, missing.only = TRUE)}
\arguments{
\item{data}{data.frame}
\item{avoid}{whether missings should be stacked or dodged, see \code{\link{geom_bar}} for more details}
\item{order}{whether variable should be ordered by number of missings}
\item{missing.only}{whether only variables containing some missing values should be shown}
}

\details{The missing values plot is a useful tool to get a rapid
overview of the number of missings in a dataset.  It's strength
is much more apparent when used with interactive graphics, as you can
see in Mondrian (\url{http://rosuda.org/mondrian}) where this plot was
copied from.}
\seealso{\code{\link{ggstructure}}, \code{\link{ggorder}}}
\examples{mmissing <- movies
mmissing[sample(nrow(movies), 1000), sample(ncol(movies), 5)] <- NA
ggmissing(mmissing)
ggmissing(mmissing, order=FALSE, missing.only = FALSE)
ggmissing(mmissing, avoid="dodge") + scale_y_sqrt()
ggmissing(mmissing) + scale_y_log10(limits=c(1, NA))}
\keyword{hplot}
