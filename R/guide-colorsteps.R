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
#' @inheritParams guide_colourbar
#'
#' @inheritSection guide_bins Use with discrete scale
#'
#' @return A guide object
#' @export
#'
#' @family guides
#' @seealso
#' The `r link_book("binned legend section", "scales-colour#sec-guide-coloursteps")`
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
  title = waiver(),
  theme = NULL,
  alpha = NA,
  even.steps  = TRUE,
  show.limits = NULL,
  override.aes = list(),
  direction = NULL,
  position = NULL,
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill"),
  ...
) {

  theme <- deprecated_guide_args(theme, ...)
  check_number_decimal(alpha, min = 0, max = 1, allow_na = TRUE)

  new_guide(
    title = title,
    theme = theme,
    alpha = alpha,
    even.steps  = even.steps,
    show.limits = show.limits,
    override.aes = override.aes,
    position = position,
    direction = direction,
    reverse = reverse,
    order = order,
    available_aes = available_aes,
    super = GuideColoursteps
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
    vec_assign(GuideColourbar$params, "default_ticks", list(element_blank()))
  ),

  extract_key = function(scale, aesthetic, even.steps, ...) {

    orig_breaks <- scale$get_breaks()
    is_missing  <- which(is.na(orig_breaks))

    if (!(even.steps || !is.numeric(orig_breaks))) {
      return(Guide$extract_key(scale, aesthetic))
    }

    parsed <- parse_binned_breaks(scale, orig_breaks)
    if (is.null(parsed)) {
      return(parsed)
    }
    limits <- parsed$limits
    breaks <- parsed$breaks

    key <- data_frame0(!!aesthetic := scale$map(breaks))
    if (even.steps) {
      key$.value <- seq_along(breaks)
    } else {
      key$.value <- breaks
    }

    orig_labels <- NULL
    if (length(is_missing) > 0) {
      is_missing  <- is_missing[1]
      orig_labels <- scale$get_labels(orig_breaks)[match(breaks, orig_breaks)]
    }
    key$.label <- orig_labels %||% scale$get_labels(breaks)

    if (breaks[1] %in% limits) {
      key$.value  <- key$.value - 1L
      key[[1]][1] <- NA
    }
    if (breaks[length(breaks)] %in% limits) {
      key[[1]][nrow(key)] <- NA
    }

    if (length(is_missing) > 0) {
      missing <- data_frame0(
        !!aesthetic := scale$map(orig_breaks[is_missing]),
        .value = orig_breaks[is_missing],
        .label = scale$get_labels(orig_breaks)[is_missing]
      )
      if (is_missing == 1) {
        key <- vec_c(missing, key)
      } else {
        key <- vec_c(key, missing)
      }
    }

    # To avoid having to recalculate these variables in other methods, we
    # attach the parsed values as attributes. It might not be very elegant,
    # but it works.
    attr(key, "parsed") <- parsed
    key
  },

  extract_decor = function(scale, aesthetic, key,
                           reverse = FALSE, even.steps = TRUE,
                           nbin = 100, alpha = NA,...) {

    parsed <- attr(key, "parsed")
    breaks <- parsed$breaks %||% scale$get_breaks()
    limits <- parsed$limits %||% scale$get_limits()

    breaks <- sort(unique0(c(limits, breaks)))
    n      <- length(breaks)
    bin_at <- parsed$bin_at %||% ((breaks[-1] + breaks[-n]) / 2)

    if (even.steps) {
      breaks <- seq_len(n) - 1L
    }

    data_frame0(
      colour = alpha(scale$map(bin_at), alpha),
      min    = breaks[-n],
      max    = breaks[-1],
      .size  = length(bin_at)
    )
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

    key <- params$key
    if (show.limits) {
      # Separate NA-breaks from proper breaks
      missing <- vec_slice(key,  is.na(key$.value))
      key     <- vec_slice(key, !is.na(key$.value))

      # Add extra top and bottom rows for limits
      limits <- attr(key, "parsed")$limits %||% scale$get_limits()
      key <- key[c(NA, seq_len(nrow(key)), NA), , drop = FALSE]
      n <- nrow(key)
      key$.value[c(1, n)] <- range(params$decor$min, params$decor$max)
      key$.label[c(1, n)] <- scale$get_labels(limits)

      # Remove duplicates when e.g. outer breaks are included in limits
      if (key$.value[1] == key$.value[2]) {
        key <- vec_slice(key, -1)
        n <- n - 1
      }
      if (key$.value[n - 1] == key$.value[n]) {
        key <- vec_slice(key, -n)
      }

      # Reintroduce NA-breaks
      if (is.na(params$key$.value[1])) {
        key <- vec_c(missing, key)
      } else {
        key <- vec_c(key, missing)
      }
    }

    params$title <- scale$make_title(
      params$title %|W|% scale$name %|W|% title
    )

    limits <- c(params$decor$min[1], params$decor$max[nrow(params$decor)])
    if (params$reverse) {
      limits <- rev(limits)
    }
    key$.value <- rescale(key$.value, from = limits)
    params$decor$min <- rescale(params$decor$min, from = limits)
    params$decor$max <- rescale(params$decor$max, from = limits)

    keep <- !is.na(oob_censor_any(key$.value))
    if (!is.na(scale$na.value %||% NA)) {
      keep <- keep | is.na(key$.value) & !is.na(key[[params$aesthetic]])
    }
    params$key <- vec_slice(key, keep)
    params
  },

  build_decor = function(decor, grobs, elements, params) {

    bar_data <- decor[[1]]

    size <- abs(bar_data$max - bar_data$min)
    just <- as.numeric(bar_data$min > bar_data$max)
    gp   <- gg_par(col = NA, fill = bar_data$colour)
    if (params$direction == "vertical") {
      grob <- rectGrob(
        x = 0, y = bar_data$min,
        width = 1, height = size,
        vjust = just, hjust = 0, gp = gp
      )
    } else {
      grob <- rectGrob(
        x = bar_data$min, y = 0,
        height = 1, width = size,
        hjust = just, vjust = 0, gp = gp
      )
    }

    frame <- element_grob(elements$frame, fill = NA)
    bar <- grobTree(bar = grob, frame = frame, ticks = grobs$ticks)
    list(bar = bar)
  }
)
