\name{update_geom_defaults}
\alias{update_geom_defaults}
\alias{update_stat_defaults}
\title{Modify geom/stat aesthetic defaults for future plots}
\usage{
  update_geom_defaults(geom, new)

  update_stat_defaults(stat, new)
}
\arguments{
  \item{stat,geom}{name of geom/stat to modify}

  \item{new}{named list of aesthetics}
}
\description{
  Modify geom/stat aesthetic defaults for future plots
}
\examples{
update_geom_defaults("point", list(colour = "darkblue"))
qplot(mpg, wt, data = mtcars)
update_geom_defaults("point", list(colour = "black"))
}

