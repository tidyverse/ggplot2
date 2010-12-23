\name{update_geom_defaults}
\alias{update_geom_defaults}
\title{Update geom defaults}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Modify geom aesthetic defaults for future plots
}
\usage{update_geom_defaults(geom, new)}
\arguments{
\item{geom}{name of geom to modify}
\item{new}{named list of aesthetics}
}



\examples{update_geom_defaults("point", list(colour = "darkblue"))
qplot(mpg, wt, data = mtcars)
update_geom_defaults("point", list(colour = "black"))}
\keyword{hplot}
