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
#' @param preset A character string to specify `width` and `height` (and
#'   `units`) at once. Recognizes common paper sizes (such as "a4" and
#'   "letter"), common screen resolutions ("4k"), or a resolution specification
#'   such as "1920x1080". All presets default to landscape orientation, and
#'   portrait can be specified by appending an "r" (for rotated) to the end of
#'   the string. This is similar to the `paper` option for
#'   \code{\link[grDevices]{pdf}()}, but with landscape being the default as
#'   it's more common. Ignored if `width`/`height` was specified.
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
                   dpi = 300, limitsize = TRUE, preset = NULL, ...) {

  dpi <- parse_dpi(dpi)
  if (!is.null(preset)) {
    if (is.na(width) && is.na(height)) {
      preset_values <- parse_preset(preset, dpi)
      width <- preset_values$width
      height <- preset_values$height
      units <- preset_values$units
    } else {
      warning(
        "Ignoring 'preset = \"", preset, "\"', as width/height was specified."
      )
    }
  }
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
    grDevices::dev.set(old_dev)
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

# Selected size presets obtained from:
# https://en.wikipedia.org/wiki/Paper_size
# https://en.wikipedia.org/wiki/Computer_display_standard
# These are defined here (instead of inside the function below), so that they
# can be accessed directly from within tests.
ggsave_presets <- tibble::tribble(
               ~names, ~width, ~height, ~units,
                 "a3",    420,     297,   "mm",
                 "a4",    297,     210,   "mm",
                 "a5",    210,     148,   "mm",
             "letter",     11,     8.5,   "in",
     c("legal", "us"),     14,     8.5,   "in",
          "executive",   10.5,    7.25,   "in",
                 "hd",   1280,     720,   "px",
  c("fhd", "full hd"),   1920,    1080,   "px",
    c("4k", "4h uhd"),   3840,    1920,   "px",
    c("5k", "5h uhd"),   5120,    2880,   "px",
    c("8k", "8h uhd"),   5120,    2880,   "px",
                "vga",    640,     480,   "px",
               "svga",    800,     600,   "px",
                "xga",   1024,     768,   "px",
               "wxga",   1280,     800,   "px",
              "wxga+",   1440,     900,   "px",
               "uxga",   1600,    1200,   "px",
             "wsxga+",   1680,    1050,   "px",
              "wqxga",   2560,    1600,   "px"
)

#' Parse a size preset from the user
#'
#' A preset allows `width` and `height` (and `units`) to be specified at once.
#' It recognizes common paper sizes (such as "a4" and "letter"), common screen
#' resolutions ("4k"), or a resolution specification such as "1920x1080".
#'
#' @param preset Size preset from user
#' @return Named list of width, height, and units
#' @noRd
parse_preset <- function(preset, dpi) {
  if (!is.character(preset) || length(preset) != 1) {
    stop("preset must be a character string.", call. = FALSE)
  }
  preset <- tolower(preset)

  if (preset %in% unlist(ggsave_presets$names)) {
    index <-
      sapply(ggsave_presets$names, function(x) preset %in% x)
    width <- ggsave_presets$width[index]
    height <- ggsave_presets$height[index]
    units <- ggsave_presets$units[index]
  } else if (sub("r$", "", preset) %in% unlist(ggsave_presets$names)) {
    index <-
      sapply(ggsave_presets$names, function(x) sub("r$", "", preset) %in% x)
    width <- ggsave_presets$height[index]
    height <- ggsave_presets$width[index]
    units <- ggsave_presets$units[index]
  } else if (length(grep("^(\\d+)\\s?x\\s?(\\d+)$", preset)) == 1) {
    width <- as.numeric(sub("^(\\d+)\\s?x\\s?(\\d+)$", "\\1", preset))
    height <- as.numeric(sub("^(\\d+)\\s?x\\s?(\\d+)$", "\\2", preset))
    units <- "px"
  } else if (length(grep("^(\\d+)\\s?x\\s?(\\d+)r$", preset)) == 1) {
    width <- as.numeric(sub("^(\\d+)\\s?x\\s?(\\d+)r$", "\\2", preset))
    height <- as.numeric(sub("^(\\d+)\\s?x\\s?(\\d+)r$", "\\1", preset))
    units <- "px"
  } else {
    stop("Unknown preset: ", preset, call. = FALSE)
  }
  if (units == "px") {
    width <- width / dpi
    height <- height / dpi
    units = "in"
  }
  list(width = width, height = height, units = units)
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

plot_dev <- function(device, filename, dpi = 300) {
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
