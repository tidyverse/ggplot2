#' Save a ggplot with sensible defaults
#' 
#' ggsave is a convenient function for saving a plot.  It defaults to
#' saving the last plot that you displayed, and for a default size uses 
#' the size of the current graphics device.  It also guesses the type of 
#' graphics device from the extension.  This means the only argument you 
#' need to supply is the filename.
#' 
#' \code{ggsave} currently recognises the extensions eps/ps, tex (pictex),
#' pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).
#' 
#' @param filename file name/filename of plot
#' @param plot plot to save, defaults to last plot displayed
#' @param device device to use, automatically extract from file name extension
#' @param path path to save plot to (if you just want to set path and not
#'    filename)
#' @param scale scaling factor
#' @param width width (in inches)
#' @param height height (in inches)
#' @param dpi dpi to use for raster graphics
#' @param ... other arguments passed to graphics device
#' @export
#' @examples
#' \dontrun{
#' ratings <- qplot(rating, data=movies, geom="histogram")
#' qplot(length, data=movies, geom="histogram")
#' ggsave(file="length-hist.pdf")
#' ggsave(file="length-hist.png")
#' ggsave(ratings, file="ratings.pdf")
#' ggsave(ratings, file="ratings.pdf", width=4, height=4)
#' # make twice as big as on screen
#' ggsave(ratings, file="ratings.pdf", scale=2)
#' }
ggsave <- function(filename=default_name(plot), plot = last_plot(), device=default_device(filename), path = NULL, scale=1, width=par("din")[1], height=par("din")[2], dpi=300, ...) {
  if (!inherits(plot, "ggplot")) stop("plot should be a ggplot2 plot")

  eps <- ps <- function(..., width, height)  
    grDevices::postscript(..., width=width, height=height, onefile=FALSE,
      horizontal = FALSE, paper = "special")
  tex <- function(..., width, height) 
    grDevices::pictex(..., width=width, height=height)
  pdf <- function(..., version="1.4") 
    grDevices::pdf(..., version=version)
  svg <- function(...) 
    grDevices::svg(...)
  wmf <- function(..., width, height) 
    grDevices::win.metafile(..., width=width, height=height)

  png <- function(..., width, height) 
    grDevices::png(...,  width=width, height=height, res = dpi, units = "in")
  jpg <- jpeg <- function(..., width, height) 
    grDevices::jpeg(..., width=width, height=height, res = dpi, units = "in")
  bmp <- function(..., width, height) 
    grDevices::bmp(...,  width=width, height=height, res = dpi, units = "in")
  tiff <- function(..., width, height) 
    grDevices::tiff(..., width=width, height=height, res = dpi, units = "in")
  
  default_name <- function(plot) { 
    paste(digest.ggplot(plot), ".pdf", sep="")
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
  
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  device(file=filename, width=width, height=height, ...)
  on.exit(capture.output(dev.off()))
  print(plot)
  
  invisible()
}