\name{coord_map}
\alias{coord_map}
\title{Map projections.}
\usage{
  coord_map(projection = "mercator", ...,
    orientation = NULL, xlim = NULL, ylim = NULL)
}
\arguments{
  \item{projection}{projection to use, see
  \code{\link[mapproj]{mapproject}} for list}

  \item{...}{other arguments passed on to
  \code{\link[mapproj]{mapproject}}}

  \item{orientation}{projection orientation, which defaults
  to \code{c(90, 0, mean(range(x)))}.  This is not optimal
  for many projections, so you will have to supply your
  own.}

  \item{xlim}{manually specific x limits (in degrees of
  lontitude)}

  \item{ylim}{manually specific y limits (in degrees of
  latitude)}
}
\description{
  This coordinate system provides the full range of map
  projections available in the mapproj package.
}
\details{
  This is still experimental, and if you have any advice to
  offer regarding a better (or more correct) way to do
  this, please let me know
}
\examples{
if (require("maps")) {
# Create a lat-long dataframe from the maps package
nz <- data.frame(map("nz", plot=FALSE)[c("x","y")])
(nzmap <- qplot(x, y, data=nz, geom="path"))

nzmap + coord_map()
nzmap + coord_map(project="cylindrical")
nzmap + coord_map(project='azequalarea',orientation=c(-36.92,174.6,0))

states <- data.frame(map("state", plot=FALSE)[c("x","y")])
(usamap <- qplot(x, y, data=states, geom="path"))
usamap + coord_map()
# See ?mapproject for coordinate systems and their parameters
usamap + coord_map(project="gilbert")
usamap + coord_map(project="lagrange")

# For most projections, you'll need to set the orientation yourself
# as the automatic selection done by mapproject is not available to
# ggplot
usamap + coord_map(project="orthographic")
usamap + coord_map(project="stereographic")
usamap + coord_map(project="conic", lat0 = 30)
usamap + coord_map(project="bonne", lat0 = 50)
}
}

