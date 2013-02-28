\name{aesthetic_linetype}
\alias{aesthetic-linetype}
\alias{aesthetic_linetype}
\alias{linetype}
\title{Aesthetic: linetype}
\description{
  This page demonstrates the usage of the linetype
  aesthetic.
}
\section{Geoms}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_geom_with_aesthetic("linetype")}
}
\examples{
# Line types should be specified with either an integer, a name, or with a string of
# an even number (up to eight) of hexidecimal digits which give the lengths in
# consecutive positions in the string.
# 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash

# Data
df <- data.frame(x = 1:10 , y = 1:10)
f <- ggplot(df, aes(x = x, y = y))
f + geom_line(linetype = 2)
f + geom_line(linetype = "dotdash")
# An example with hex strings, the string "33" specifies three units on followed
# by three off and "3313" specifies three units on followed by three off followed
# by one on and finally three off.
f + geom_line(linetype = "3313")

# Mapping line type from a variable
library(plyr)
library(reshape2)
rescale01 <- function(x) (x - min(x)) / diff(range(x))
ec_scaled <- data.frame(
  date = economics$date,
  colwise(rescale01)(economics[, -(1:2)]))
ecm <- melt(ec_scaled, id = "date")
qplot(date, value, data = ecm, geom = "line", linetype = variable)
}
\seealso{
  Other aesthetics: \code{\link{aesthetic_alpha}},
  \code{\link{aesthetic_colour_fill}},
  \code{\link{aesthetic_group_order}},
  \code{\link{aesthetic_shape}},
  \code{\link{aesthetic_size}}
}

