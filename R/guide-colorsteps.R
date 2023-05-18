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
#'   (default), no tick marks are drawn. For backward compatability, can also
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
    raster      = FALSE,
    ticks       = ticks,
    nbin        = 100,
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
  NULL, GuideColourbar,

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
    key$.value <- seq_along(breaks) - 0.5
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
    if (!(even.steps || !is.numeric(scale$get_breaks()))) {
      return(GuideColourbar$extract_decor(scale, aesthetic, reverse = reverse,
                                          nbin = nbin))
    }

    bin_at <- attr(key, "bin_at", TRUE)

    bar <- data_frame0(
      colour = scale$map(bin_at),
      value  = seq_along(bin_at) - 1,
      .size  = length(bin_at)
    )
    if (reverse) {
      bar <- bar[nrow(bar):1, , drop = FALSE]
    }
    return(bar)
  },

  extract_params = function(scale, params, hashables, ...) {

    if (params$even.steps) {
      params$nbin <- nbin <- sum(!is.na(params$key[[1]])) + 1
    } else {
      nbin <- params$nbin
    }

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
      edges <- rescale(
        c(0, 1),
        to   = params$decor$value[c(1, nrow(params$decor))],
        from = c(0.5, nbin - 0.5) / nbin
      )
      key <- params$key
      limits <- attr(key, "limits", TRUE)
      key <- key[c(NA, seq_len(nrow(key)), NA), , drop = FALSE]
      key$.value[c(1, nrow(key))] <- edges
      key$.label[c(1, nrow(key))] <- scale$get_labels(limits)
      if (key$.value[1] == key$.value[2]) {
        key <- key[-1, , drop = FALSE]
      }
      if (key$.value[nrow(key) - 1] == key$.value[nrow(key)]) {
        key <- key[-nrow(key), , drop = FALSE]
      }
      params$key <- key
    }

    GuideColourbar$extract_params(scale, params, hashables, ...)
  }
)
