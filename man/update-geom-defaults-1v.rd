\name{update_geom_defaults}
\alias{update_geom_defaults}
\title{Update geom defaults}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Modify geom aesthetic defaults for future plots
}
\usage{update_geom_defaults(geom, new)}
\arguments{
\item{geom}{}
\item{new}{}
}



\examples{update_geom_defaults("point", aes(colour = "darkblue"))
qplot(mpg, wt, data = mtcars)
update_geom_defaults("point", aes(colour = "black"))}
\keyword{name of geom to modify}
\keyword{named list of aesthetics}
\keyword{hplot}
