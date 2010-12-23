\name{ggsave}
\alias{ggsave}
\title{ggsave}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Save a ggplot with sensible defaults
}
\usage{ggsave(filename=default_name(plot), plot = last_plot(), device=default_device(filename), path = NULL, scale=1, width=par("din")[1], height=par("din")[2], dpi=300, keep = plot$options$keep, drop = plot$options$drop, ...)}
\arguments{
\item{filename}{file name/filename of plot}
\item{plot}{plot to save, defaults to last plot displayed}
\item{device}{device to use, automatically extract from file name extension}
\item{path}{path to save plot to (if you just want to set path and not filename)}
\item{scale}{scaling factor}
\item{width}{width (in inches)}
\item{height}{height (in inches)}
\item{dpi}{dpi to use for raster graphics}
\item{keep}{plot components to keep}
\item{drop}{plot components to drop}
\item{...}{other arguments passed to graphics device}
}

\details{ggsave is a convenient function for saving a plot.  It defaults to
saving the last plot that you displayed, and for a default size uses
the size of the current graphics device.  It also guesses the type of
graphics device from the extension.  This means the only argument you
need to supply is the filename.

\code{ggsave} currently recognises the extensions eps/ps, tex (pictex), pdf,
jpeg, tiff, png, bmp, svg and wmf (windows only).}

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
