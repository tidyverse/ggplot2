\name{stat_boxplot}
\alias{stat_boxplot}
\title{Calculate components of box and whisker plot.}

\description{
  Calculate components of box and whisker plot.
}
\value{A data frame with additional columns:
\item{width}{width of boxplot}
\item{ymin}{lower whisker = lower hinge - 1.5 * IQR}
\item{lower}{lower hinge, 25\% quantile} 
\item{middle}{median, 50\% quantile}
\item{upper}{upper hinge, 75\% quantile}
\item{ymax}{upper whisker = upper hinge + 1.5 * IQR}}
\examples{# See geom_boxplot for examples}
