\name{check_breaks_and_labels}
\alias{check_breaks_and_labels}
\title{Check breaks and labels.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Ensure that breaks and labels are the correct format.
}
\usage{check_breaks_and_labels(breaks = NULL, labels = NULL)}
\arguments{
\item{breaks}{}
\item{labels}{}
}



\examples{check_breaks_and_labels(NULL, NULL)
check_breaks_and_labels(1:5, NULL)
should_stop(check_breaks_and_labels(labels = 1:5))
check_breaks_and_labels(labels = c("a" = 1, "b" = 2))
check_breaks_and_labels(breaks = c("a" = 1, "b" = 2))
check_breaks_and_labels(1:2, c("a", "b"))}
\keyword{internal}
