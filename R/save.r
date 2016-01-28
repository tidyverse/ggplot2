#' Save a ggplot (or other grid object) with sensible defaults
#'
#' \code{ggsave()} is a convenient function for saving a plot. It defaults to
#' saving the last plot that you displayed, using the size of the current
#' graphics device. It also guesses the type of graphics device from the
#' extension.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param device Device to use (function or any of the recognized extensions,
#'   e.g. \code{"pdf"}). By default, extracted from filename extension.
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
#' ggplot(mtcars, aes(mpg, wt)) + geom_point()
#'
#' ggsave("mtcars.pdf")
#' ggsave("mtcars.png")
#'
#' ggsave("mtcars.pdf", width = 4, height = 4)
#' ggsave("mtcars.pdf", width = 20, height = 20, units = "cm")
#'
#' unlink("mtcars.pdf")
#' unlink("mtcars.png")
#'
#' # specify device when saving to a file with unknown extension
#' # (for example a server supplied temporary file)
#' file <- tempfile()
#' ggsave(file, device = "pdf")
#' unlink(file)
#' }
ggsave <- function(filename, plot = last_plot(),
                   device = NULL, path = NULL, scale = 1,
                   width = NA, height = NA, units = c("in", "cm", "mm"),
                   dpi = 300, limitsize = TRUE, ...) {

  dev <- plot_dev(device, filename, dpi = dpi)
  dim <- plot_dim(c(width, height), scale = scale, units = units,
    limitsize = limitsize)

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  dev(file = filename, width = dim[1], height = dim[2], ...)
  on.exit(utils::capture.output(grDevices::dev.off()))
  grid.draw(plot)

  invisible()
}

plot_dim <- function(dim = c(NA, NA), scale = 1, units = c("in", "cm", "mm"),
                     limitsize = TRUE) {

  units <- match.arg(units)
  to_inches <- function(x) x / c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units]
  from_inches <- function(x) x * c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units]

  dim <- to_inches(dim) * scale

  if (any(is.na(dim))) {
    if (length(grDevices::dev.list()) == 0) {
      default_dim <- c(7, 7)
    } else {
      default_dim <- grDevices::dev.size() * scale
    }
    dim[is.na(dim)] <- default_dim[is.na(dim)]
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

plot_dev <- function(device, filename, dpi = 300) {
  if (is.function(device))
    return(device)

  eps <- function(...) {
    grDevices::postscript(..., onefile = FALSE, horizontal = FALSE,
      paper = "special")
  }
  devices <- list(
    eps =  eps,
    ps =   eps,
    tex =  function(...) grDevices::pictex(...),
    pdf =  function(..., version = "1.4") grDevices::pdf(..., version = version),
    svg =  function(...) svglite::svglite(...),
    emf =  function(...) grDevices::win.metafile(...),
    wmf =  function(...) grDevices::win.metafile(...),
    png =  function(...) grDevices::png(..., res = dpi, units = "in"),
    jpg =  function(...) grDevices::jpeg(..., res = dpi, units = "in"),
    jpeg = function(...) grDevices::jpeg(..., res = dpi, units = "in"),
    bmp =  function(...) grDevices::bmp(..., res = dpi, units = "in"),
    tiff = function(...) grDevices::tiff(..., res = dpi, units = "in")
  )

  if (is.null(device)) {
    device <- tolower(tools::file_ext(filename))
  }

  if (!is.character(device) || length(device) != 1) {
    stop("`device` must be NULL, a string or a function.", call. = FALSE)
  }

  dev <- devices[[device]]
  if (is.null(dev)) {
    stop("Unknown graphics device '", device, "'", call. = FALSE)
  }
  dev
}

#' @export
grid.draw.ggplot <- function(x, recording = TRUE) {
  print(x)
}
