\name{update_labels}
\alias{update_labels}
\title{Update axis/legend labels}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Change the scale names of an existing plot
}
\usage{update_labels(p, labels)}
\arguments{
\item{p}{plot}
\item{labels}{}
}



\examples{p <- qplot(mpg, wt, data = mtcars)
update_labels(p, list(x = "New x"))
update_labels(p, list(x = expression(x / y ^ 2)))
update_labels(p, list(x = "New x", y = "New Y"))
update_labels(p, list(colour = "Fail silently"))}

