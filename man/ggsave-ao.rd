\name{ggsave}
\alias{ggsave}
\title{ggsave}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Save a ggplot with sensible defaults
}
\usage{ggsave(plot = last_plot(), filename=default_name(plot), device=default_device(filename), path = "", scale=1, width=par("din")[1], height=par("din")[2], dpi=300, keep = plot$options$keep, drop = plot$options$drop, ...)}
\arguments{
\item{plot}{plot to save, defaults to last plot displayed}
\item{filename}{file name/filename of plot}
\item{device}{device to use, automatically extract from file name extension}
\item{path}{path to save plot to (if you just want to set path and not filename)}
\item{scale}{scaling factor}
\item{width}{width (in inches)}
\item{height}{height (in inches)}
\item{dpi}{dpi to use for raster graphics}
\item{keep}{other arguments passed to graphics device}
\item{drop}{}
\item{...}{}
}

\details{ggsave is a convenient function for saving a plot.  It defaults to
saving the last plot that you displayed, and for a default size uses
the size of the current graphics device.  It also guess the type of
graphics device from the extension.  This means the only argument you
need to supply is the filename.

\code{ggsave} currently recognises the extensions ps, tex (pictex), pdf,
tiff, png, bmp and wmf (windows only).}

\examples{\dontrun{
ratings <- qplot(rating, data=movies, geom="histogram")
qplot(length, data=movies, geom="histogram")
ggsave(file="length-hist.pdf")
ggsave(file="length-hist.png")
ggsave(ratings, file="ratings.pdf")
ggsave(ratings, file="ratings.pdf", width=4, height=4)
# make twice as big as on screen
ggsave(ratings, file="ratings.pdf", scale=2)
}}
\keyword{file}
