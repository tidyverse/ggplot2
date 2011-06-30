\name{update_labels}
\alias{update_labels}
\title{Update axis/legend labels...}
\usage{update_labels(p, labels)}

\description{
  Update axis/legend labels
}
\arguments{
  \item{p}{plot to modify}
  \item{labels}{named list of new labels}
}
\examples{p <- qplot(mpg, wt, data = mtcars)
update_labels(p, list(x = "New x"))
update_labels(p, list(x = expression(x / y ^ 2)))
update_labels(p, list(x = "New x", y = "New Y"))
update_labels(p, list(colour = "Fail silently"))}

