\name{fortify.sp}
\alias{fortify.sp}
\title{Fortify method for classes from the sp package.}
\arguments{
  \item{model}{\code{SpatialPolygonsDataFrame} to convert
  into a dataframe.}

  \item{data}{not used by this method}

  \item{region}{name of variable used to split up regions}

  \item{...}{not used by this method}
}
\description{
  Fortify method for classes from the sp package.
}
\details{
  To figure out the correct variable name for region,
  inspect \code{as.data.frame(model)}.
}

