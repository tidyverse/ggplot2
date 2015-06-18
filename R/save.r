#' Save a ggplot (or other grid object) with sensible defaults
#'
#' \code{ggsave()} is a convenient function for saving a plot. It defaults to
#' saving the last plot that you displayed, using the size of the current
#' graphics device. It also guesses the type of graphics device from the
#' extension.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param device Device to use. By default, extracted from extension.
#'   \code{ggsave} currently recognises eps/ps, tex (pictex), pdf, jpeg, tiff,
#'   png, bmp, svg and wmf (windows only).
#' @param path Path to save plot to (combined with filename).
#' @param scale Multiplicative scaling factor.
#' @param width,height Plot dimensions, defaults to size of current graphics
#'   device.
#' @param units Units for width and height when specified explicitly (in, cm,
#'   or mm)
#' @param dpi Resolution used for raster outputs.
#' @param limitsize When \code{TRUE} (the default), \code{ggsave} will not
#'   save images larger than 50x50 inches, to prevent the common error of
#'   specifying dimensions in pixels.
#' @param ... Other arguments passed on to graphics device
#' @export
#' @examples
#' \dontrun{
#' ggplot(movies, aes(rating)) + geom_histogram(binwidth = 0.1)
#'
#' ggsave("ratings.pdf")
#' ggsave("ratings.pdf")
#'
#' ggsave("ratings.pdf", width = 4, height = 4)
#' ggsave("ratings.pdf", width = 20, height = 20, units = "cm")
#'
#' unlink("ratings.pdf")
#' unlink("ratings.png")
#' }
ggsave <- function(filename, plot = last_plot(),
                   device = default_device(filename), path = NULL, scale = 1,
                   width = NA, height = NA, units = c("in", "cm", "mm"),
                   dpi = 300, limitsize = TRUE, ...) {

  eps <- ps <- function(..., width, height)
    grDevices::postscript(..., width = width, height = height, onefile = FALSE,
      horizontal = FALSE, paper = "special")
  tex <- function(..., width, height)
    grDevices::pictex(..., width = width, height = height)
  pdf <- function(..., version = "1.4")
    grDevices::pdf(..., version = version)
  svg <- function(...)
    grDevices::svg(...)
  wmf <- function(..., width, height)
    grDevices::win.metafile(..., width = width, height = height)
  emf <- function(..., width, height)
    grDevices::win.metafile(..., width = width, height = height)
  png <- function(..., width, height)
    grDevices::png(...,  width = width, height = height, res = dpi, units = "in")
  jpg <- jpeg <- function(..., width, height)
    grDevices::jpeg(..., width = width, height = height, res = dpi, units = "in")
  bmp <- function(..., width, height)
    grDevices::bmp(...,  width = width, height = height, res = dpi, units = "in")
  tiff <- function(..., width, height)
    grDevices::tiff(..., width = width, height = height, res = dpi, units = "in")

  default_device <- function(filename) {
    pieces <- strsplit(filename, "\\.")[[1]]
    ext <- tolower(pieces[length(pieces)])
    match.fun(ext)
  }

  dim <- plot_dim(c(width, height), scale = scale, units = units,
    limitsize = limitsize)

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  device(file = filename, width = dim[1], height = dim[2], ...)
  on.exit(capture.output(dev.off()))
  grid.draw(plot)

  invisible()
}

plot_dim <- function(dim = c(NA, NA), scale = 1, units = c("in", "cm", "mm"),
                      limitsize = TRUE, dim_guess = FALSE) {


  units <- match.arg(units)
  to_inches <- function(x) x / c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units]
  from_inches <- function(x) x * c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units]

  dim <- to_inches(dim) * scale

  if (any(is.na(dim))) {
    dim[is.na(dim)] <- par("din") * scale
    dim_f <- prettyNum(from_inches(dim), digits = 3)
    message("Saving ", dim_f[1], " x ", dim_f[2], " ", units, " image")
  }

  if (limitsize && any(dim >= 50)) {
    stop("Dimensions exceed 50 inches (height and width are specified in '",
      units, "' not pixels). If you're sure you a plot that big, use ",
      "`limitsize = FALSE`.", call. = FALSE)
  }

  dim
}

#' @export
grid.draw.ggplot <- function(x, recording = TRUE) {
  print(x)
}
