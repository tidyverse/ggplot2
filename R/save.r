#' Save a ggplot (or other grid object) with sensible defaults
#'
#' `ggsave()` is a convenient function for saving a plot. It defaults to
#' saving the last plot that you displayed, using the size of the current
#' graphics device. It also guesses the type of graphics device from the
#' extension.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param device Device to use. Can be either be a device function
#'   (e.g. [png()]), or one of "eps", "ps", "tex" (pictex),
#'   "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' @param path Path to save plot to (combined with filename).
#' @param scale Multiplicative scaling factor.
#' @param width,height,units Plot size in `units` ("in", "cm", or "mm").
#'   If not supplied, uses the size of current graphics device.
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320),
#'   "print" (300), or "screen" (72). Applies only to raster output types.
#' @param limitsize When `TRUE` (the default), `ggsave` will not
#'   save images larger than 50x50 inches, to prevent the common error of
#'   specifying dimensions in pixels.
#' @param ... Other arguments passed on to the graphics device function,
#'   as specified by `device`.
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
#' # delete files with base::unlink()
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

  dpi <- parse_dpi(dpi)
  dev <- plot_dev(device, filename, dpi = dpi)
  dim <- plot_dim(c(width, height), scale = scale, units = units,
    limitsize = limitsize)

  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  old_dev <- grDevices::dev.cur()
  dev(filename = filename, width = dim[1], height = dim[2], ...)
  on.exit(utils::capture.output({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev) # restore old device unless null device
  }))
  grid.draw(plot)

  invisible()
}

#' Parse a DPI input from the user
#'
#' Allows handling of special strings when user specifies a DPI like "print".
#'
#' @param dpi Input value from user
#' @return Parsed DPI input value
#' @noRd
parse_dpi <- function(dpi) {
  if (is.character(dpi) && length(dpi) == 1) {
    switch(dpi,
      screen = 72,
      print = 300,
      retina = 320,
      stop("Unknown DPI string", call. = FALSE)
    )
  } else if (is.numeric(dpi) && length(dpi) == 1) {
    dpi
  } else {
    stop("DPI must be a single number or string", call. = FALSE)
  }
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
      units, "' not pixels). If you're sure you want a plot that big, use ",
      "`limitsize = FALSE`.", call. = FALSE)
  }

  dim
}

plot_dev <- function(device, filename = NULL, dpi = 300) {
  force(filename)
  force(dpi)

  if (is.function(device))
    return(device)

  eps <- function(filename, ...) {
    grDevices::postscript(file = filename, ..., onefile = FALSE, horizontal = FALSE,
      paper = "special")
  }
  devices <- list(
    eps =  eps,
    ps =   eps,
    tex =  function(filename, ...) grDevices::pictex(file = filename, ...),
    pdf =  function(filename, ..., version = "1.4") grDevices::pdf(file = filename, ..., version = version),
    svg =  function(filename, ...) svglite::svglite(file = filename, ...),
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
