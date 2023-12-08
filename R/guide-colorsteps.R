#' Discretized colourbar guide
#'
#' This guide is version of [guide_colourbar()] for binned colour and fill
#' scales. It shows areas between breaks as a single constant colour instead of
#' the gradient known from the colourbar counterpart.
#'
#' @param even.steps Should the rendered size of the bins be equal, or should
#'   they be proportional to their length in the data space? Defaults to `TRUE`
#' @param show.limits Logical. Should the limits of the scale be shown with
#'   labels and ticks. Default is `NULL` meaning it will take the value from the
#'   scale. This argument is ignored if `labels` is given as a vector of
#'   values. If one or both of the limits is also given in `breaks` it will be
#'   shown irrespective of the value of `show.limits`.
#' @param ticks A theme object for rendering tick marks at the colourbar.
#'   Usually, the object of `element_line()` is expected. If `element_blank()`
#'   (default), no tick marks are drawn. For backward compatibility, can also
#'   be a logical which translates `TRUE` to `element_line()` and `FALSE` to
#'   `element_blank()`.
#' @inheritDotParams guide_colourbar -nbin -raster -ticks -available_aes
#'
#' @inheritSection guide_bins Use with discrete scale
#'
#' @return A guide object
#' @export
#'
#' @family guides
#' @examples
#' df <- expand.grid(X1 = 1:10, X2 = 1:10)
#' df$value <- df$X1 * df$X2
#'
#' p <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
#'
#' # Coloursteps guide is the default for binned colour scales
#' p + scale_fill_binned()
#'
#' # By default each bin in the guide is the same size irrespectively of how
#' # their sizes relate in data space
#' p + scale_fill_binned(breaks = c(10, 25, 50))
#'
#' # This can be changed with the `even.steps` argument
#' p + scale_fill_binned(
#'   breaks = c(10, 25, 50),
#'   guide = guide_coloursteps(even.steps = FALSE)
#' )
#'
#' # By default the limits is not shown, but this can be changed
#' p + scale_fill_binned(guide = guide_coloursteps(show.limits = TRUE))
#'
#' # (can also be set in the scale)
#' p + scale_fill_binned(show.limits = TRUE)
guide_coloursteps <- function(
  even.steps  = TRUE,
  show.limits = NULL,
  ticks       = element_blank(),
  ...
) {
  guide_colourbar(
    even.steps  = even.steps,
    show.limits = show.limits,
    ticks       = ticks,
    ...,
    super       = GuideColoursteps
  )
}

#' @export
#' @rdname guide_coloursteps
guide_colorsteps <- guide_coloursteps

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideColoursteps <- ggproto(
  "GuideColoursteps", GuideColourbar,

  params = c(
    list(even.steps = TRUE, show.limits = NULL),
    GuideColourbar$params
  ),

  extract_key = function(scale, aesthetic, even.steps, ...) {

    breaks <- scale$get_breaks()

    if (!(even.steps || !is.numeric(breaks))) {
      return(Guide$extract_key(scale, aesthetic))
    }

    parsed <- parse_binned_breaks(scale, breaks, even.steps)
    if (is.null(parsed)) {
      return(parsed)
    }
    limits <- parsed$limits
    breaks <- parsed$breaks

    key <- data_frame(scale$map(breaks), .name_repair = ~ aesthetic)
    key$.value <- seq_along(breaks)
    key$.label <- scale$get_labels(breaks)

    if (breaks[1] %in% limits) {
      key$.value  <- key$.value - 1L
      key[[1]][1] <- NA
    }
    if (breaks[length(breaks)] %in% limits) {
      key[[1]][nrow(key)] <- NA
    }
    # To avoid having to recalculate these variables in other methods, we
    # attach these as attributes. It might not be very elegant, but it works.
    attr(key, "limits") <- parsed$limits
    attr(key, "bin_at") <- parsed$bin_at
    return(key)
  },

  extract_decor = function(scale, aesthetic, key,
                           reverse = FALSE, even.steps = TRUE,
                           nbin = 100, ...) {
    if (even.steps) {
      bin_at <- attr(key, "bin_at", TRUE)
      bar <- data_frame0(
        colour = scale$map(bin_at),
        min    = seq_along(bin_at) - 1,
        max    = seq_along(bin_at),
        .size  = length(bin_at)
      )
    } else {
      breaks <- unique(sort(c(scale$get_limits(), scale$get_breaks())))
      n <- length(breaks)
      bin_at <- (breaks[-1] + breaks[-n]) / 2
      bar <- data_frame0(
        colour = scale$map(bin_at),
        min    = head(breaks, -1),
        max    = tail(breaks, -1),
        .size  = length(bin_at)
      )
    }
    return(bar)
  },

  extract_params = function(scale, params, direction = "vertical", title = waiver(), ...) {

    show.limits <- params$show.limits %||% scale$show.limits %||% FALSE

    if (show.limits &&
        (is.character(scale$labels) || is.numeric(scale$labels))) {
      cli::cli_warn(c(paste0(
        "{.arg show.limits} is ignored when {.arg labels} are given as a ",
        "character vector."
      ), "i" = paste0(
        "Either add the limits to {.arg breaks} or provide a function for ",
        "{.arg labels}."
      )))
      show.limits <- FALSE
    }

    if (show.limits) {
      key <- params$key
      limits <- attr(key, "limits", TRUE) %||% scale$get_limits()
      key <- key[c(NA, seq_len(nrow(key)), NA), , drop = FALSE]
      n <- nrow(key)
      key$.value[c(1, n)] <- range(params$decor$min, params$decor$max)
      key$.label[c(1, n)] <- scale$get_labels(limits)
      if (key$.value[1] == key$.value[2]) {
        key <- vec_slice(key, -1)
        n <- n - 1
      }
      if (key$.value[n - 1] == key$.value[n]) {
        key <- vec_slice(key, -n)
      }
      params$key <- key
    }

    params$title <- scale$make_title(
      params$title %|W|% scale$name %|W|% title
    )

    limits <- c(params$decor$min[1], params$decor$max[nrow(params$decor)])
    if (params$reverse) {
      limits <- rev(limits)
    }
    params$key$.value <- rescale(params$key$.value, from = limits)
    params$decor$min  <- rescale(params$decor$min,  from = limits)
    params$decor$max  <- rescale(params$decor$max,  from = limits)
    params$key <-
      vec_slice(params$key, !is.na(oob_censor_any(params$key$.value)))
    params
  },

  build_decor = function(decor, grobs, elements, params) {

    size <- abs(decor$max - decor$min)
    just <- as.numeric(decor$min > decor$max)
    gp   <- gpar(col = NA, fill = decor$colour)
    if (params$direction == "vertical") {
      grob <- rectGrob(
        x = 0, y = decor$min,
        width = 1, height = size,
        vjust = just, hjust = 0, gp = gp
      )
    } else {
      grob <- rectGrob(
        x = decor$min, y = 0,
        height = 1, width = size,
        hjust = just, vjust = 0, gp = gp
      )
    }

    frame <- element_grob(elements$frame, fill = NA)
    list(bar = grob, frame = frame, ticks = grobs$ticks)
  }
)
