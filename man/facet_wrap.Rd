\name{facet_wrap}
\alias{facet_wrap}
\title{Wrap a 1d ribbon of panels into 2d.}
\usage{
  facet_wrap(facets, nrow = NULL, ncol = NULL,
    scales = "fixed", shrink = TRUE, as.table = TRUE,
    drop = TRUE)
}
\arguments{
  \item{nrow}{number of rows}

  \item{ncol}{number of columns}

  \item{facets}{formula specifying variables to facet by}

  \item{scales}{should scales be fixed (\code{"fixed"}, the
  default), free (\code{"free"}), or free in one dimension
  (\code{"free_x"}, \code{"free_y"})}

  \item{shrink}{If \code{TRUE}, will shrink scales to fit
  output of statistics, not raw data. If \code{FALSE}, will
  be range of raw data before statistical summary.}

  \item{as.table}{If \code{TRUE}, the default, the facets
  are laid out like a table with highest values at the
  bottom-right. If \code{FALSE}, the facets are laid out
  like a plot with the highest value at the top-right.}

  \item{drop}{If \code{TRUE}, the default, all factor
  levels not used in the data will automatically be
  dropped. If \code{FALSE}, all factor levels will be
  shown, regardless of whether or not they appear in the
  data.}
}
\description{
  Wrap a 1d ribbon of panels into 2d.
}
\examples{
\donttest{
d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) +
  xlim(0, 2) + stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ color)
d + facet_wrap(~ color, ncol = 1)
d + facet_wrap(~ color, ncol = 4)
d + facet_wrap(~ color, nrow = 1)
d + facet_wrap(~ color, nrow = 3)

# Using multiple variables continues to wrap the long ribbon of
# plots into 2d - the ribbon just gets longer
# d + facet_wrap(~ color + cut)

# To change plot order of facet wrap,
# change the order of varible levels with factor()
diamonds$color <- factor(diamonds$color, levels = c("G", "J", "D", "E", "I", "F", "H"))
# Repeat first example with new order
d <- ggplot(diamonds, aes(carat, price, fill = ..density..)) +
xlim(0, 2) + stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ color)

# You can choose to keep the scales constant across all panels
# or vary the x scale, the y scale or both:
p <- qplot(price, data = diamonds, geom = "histogram", binwidth = 1000)
p + facet_wrap(~ color)
p + facet_wrap(~ color, scales = "free_y")

p <- qplot(displ, hwy, data = mpg)
p + facet_wrap(~ cyl)
p + facet_wrap(~ cyl, scales = "free")

# Use as.table to to control direction of horizontal facets, TRUE by default
p + facet_wrap(~ cyl, as.table = FALSE)

# Add data that does not contain all levels of the faceting variables
cyl6 <- subset(mpg, cyl == 6)
p + geom_point(data = cyl6, colour = "red", size = 1) +
  facet_wrap(~ cyl)
p + geom_point(data = transform(cyl6, cyl = 7), colour = "red") +
  facet_wrap(~ cyl)
p + geom_point(data = transform(cyl6, cyl = NULL), colour = "red") +
  facet_wrap(~ cyl)
}
}

