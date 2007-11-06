\name{plot_grob_matrix}
\alias{plot_grob_matrix}
\title{Plot grob matrix}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Take a matrix of grobs and edit them so that their viewport name
}
\usage{plot_grob_matrix(gm, type=deparse(substitute(gm)))}
\arguments{
\item{gm}{matrix of grobs to position}
\item{type}{viewport type to position them in}
}

\details{This provides a convenient way of converting a matrix of grobs
(as produced by \\code{\\link[reshape]{stamp}}) into the equivalent
visual representation.  Assumes that there are viewports named
\\code{type_1_1}, \\code{type_1_2}, ..., \\code{type_nrow_ncol}.}

\examples{}
\keyword{hplot}
\keyword{internal}
