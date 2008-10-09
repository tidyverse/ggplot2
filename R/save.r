# ggsave
# Save a ggplot with sensible defaults
# 
# ggsave is a convenient function for saving a plot.  It defaults to
# saving the last plot that you displayed, and for a default size uses 
# the size of the current graphics device.  It also guess the type of 
# graphics device from the extension.  This means the only argument you 
# need to supply is the path.
# 
# \code{ggsave} currently recognises the extensions ps, tex (pictex), pdf,
# tiff, png, bmp and wmf (windows only).
# 
# @arguments plot to save, defaults to last plot displayed
# @arguments file name/path of plot
# @arguments device to use, automatically extract from file name extension
# @arguments scaling factor
# @arguments width (in inches)
# @arguments height (in inches)
# @arguments dpi to use for raster graphics
# @arguments path where file should be saved (if filename unspecified)
# @arguments other arguments passed to graphics device
# @keyword file 
#X \dontrun{
#X ratings <- qplot(rating, data=movies, geom="histogram")
#X qplot(length, data=movies, geom="histogram")
#X ggsave(file="length-hist.pdf")
#X ggsave(file="length-hist.png")
#X ggsave(ratings, file="ratings.pdf")
#X ggsave(ratings, file="ratings.pdf", width=4, height=4)
#X # make twice as big as on screen
#X ggsave(ratings, file="ratings.pdf", scale=2)
#X }
ggsave <- function(plot = last_plot(), filename=default_name(plot), device=default_device(filename), scale=1, width=par("din")[1], height=par("din")[2], dpi=72, path="", keep = plot$options$keep, drop = plot$options$drop, ...) {

  ps <- function(..., width, height)  
    grDevices::postscript(..., width=width, height=height)
  tex <- function(..., width, height) 
    grDevices::pictex(..., width=width, height=height)
  pdf <- function(..., version="1.4") 
    grDevices::pdf(..., version=version)
  wmf <- function(..., width, height) 
    grDevices::win.metafile(..., width=width, height=height)

  png <- function(..., width, height) 
    grDevices::png(...,  width=width, height=height, res = dpi, units = "in")
  jpeg <- function(..., width, height) 
    grDevices::jpeg(..., width=width, height=height, res = dpi, units = "in")
  bmp <- function(..., width, height) 
    grDevices::bmp(...,  width=width, height=height, res = dpi, units = "in")
  tiff <- function(..., width, height) 
    grDevices::tiff(..., width=width, height=height, res = dpi, units = "in")
  
  default_name <- function(plot) { 
    paste(path, digest.ggplot(plot), ".png", sep="")
  }
  
  default_device <- function(filename) {
    pieces <- strsplit(filename, "\\.")[[1]]
    ext <- tolower(pieces[length(pieces)])
    match.fun(ext)
  }

  if (missing(width) || missing(height)) {
    message("Saving ", prettyNum(width * scale, digits=3), "\" x ", prettyNum(height * scale, digits=3), "\" image")
  }
  
  width <- width * scale
  height <- height * scale
  
  device(file=filename, width=width, height=height, ...)
  on.exit(capture.output(dev.off()))
  print(plot, keep = keep, drop = drop)
  
  invisible()
}