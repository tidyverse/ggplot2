\name{ggplot_plot}
\alias{ggplot_plot}
\title{ggplot plot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Creates a complete ggplot grob.
}
\usage{ggplot_plot(plot, pretty=TRUE)}
\arguments{
\item{plot}{plot object}
\item{pretty}{should the plot be wrapped up inside the pretty accoutrements (labels, legends, etc)}
}

\details{Delegates almost everything to its arguments.  Responsible for the
transformation chain and for collecting everything into one grob with the
appropriate viewports}

\examples{}
\keyword{hplot}
\keyword{internal}
