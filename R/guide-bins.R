#' @include guide-legend.R
NULL

#' A binned version of guide_legend
#'
#' This guide is a version of the [guide_legend()] guide for binned scales. It
#' differs in that it places ticks correctly between the keys, and sports a
#' small axis to better show the binning. Like [guide_legend()] it can be used
#' for all non-position aesthetics though colour and fill defaults to
#' [guide_coloursteps()], and it will merge aesthetics together into the same
#' guide if they are mapped in the same way.
#'
#' @inheritParams guide_legend
#' @param axis A theme object for rendering a small axis along the guide.
#'   Usually, the object of `element_line()` is expected (default). If
#'   `element_blank()`, no axis is drawn. For backward compatibility, can also
#'   be a logical which translates `TRUE` to `element_line()` and `FALSE` to
#'   `element_blank()`.
#' @param axis.colour,axis.linewidth Graphic specifications for the look of the
#'   axis.
#' @param axis.arrow A call to `arrow()` to specify arrows at the end of the
#'   axis line, thus showing an open interval.
#' @param show.limits Logical. Should the limits of the scale be shown with
#'   labels and ticks. Default is `NULL` meaning it will take the value from the
#'   scale. This argument is ignored if `labels` is given as a vector of
#'   values. If one or both of the limits is also given in `breaks` it will be
#'   shown irrespective of the value of `show.limits`.
#' @param ticks A theme object for rendering tick marks at the colourbar.
#'   Usually, the object of `element_line()` is expected. If `element_blank()`,
#'   no tick marks are drawn. If `NULL` (default), the `axis` argument is
#'   re-used as `ticks` argument (without arrow).
#' @param ticks.length A numeric or a [grid::unit()] object specifying the
#'   length of tick marks between the keys.
#'
#' @section Use with discrete scale:
#' This guide is intended to show binned data and work together with ggplot2's
#' binning scales. However, it is sometimes desirable to perform the binning in
#' a separate step, either as part of a stat (e.g. [stat_contour_filled()]) or
#' prior to the visualisation. If you want to use this guide for discrete data
#' the levels must follow the naming scheme implemented by [base::cut()]. This
#' means that a bin must be encoded as `"(<lower>, <upper>]"` with `<lower>`
#' giving the lower bound of the bin and `<upper>` giving the upper bound
#' (`"[<lower>, <upper>)"` is also accepted). If you use [base::cut()] to
#' perform the binning everything should work as expected, if not, some recoding
#' may be needed.
#'
#' @return A guide object
#' @family guides
#' @export
#'
#' @examples
#' p <- ggplot(mtcars) +
#'   geom_point(aes(disp, mpg, size = hp)) +
#'   scale_size_binned()
#'
#' # Standard look
#' p
#'
#' # Remove the axis or style it
#' p + guides(size = guide_bins(axis = FALSE))
#'
#' p + guides(size = guide_bins(show.limits = TRUE))
#'
#' p + guides(size = guide_bins(
#'   axis.arrow = arrow(length = unit(1.5, 'mm'), ends = 'both')
#' ))
#'
#' # Guides are merged together if possible
#' ggplot(mtcars) +
#'   geom_point(aes(disp, mpg, size = hp, colour = hp)) +
#'   scale_size_binned() +
#'   scale_colour_binned(guide = "bins")
#'
guide_bins <- function(
  # title
  title = waiver(),
  title.position = NULL,
  title.theme    = NULL,
  title.hjust    = NULL,
  title.vjust    = NULL,

  # label
  label          = TRUE,
  label.position = NULL,
  label.theme    = NULL,
  label.hjust    = NULL,
  label.vjust    = NULL,

  # key
  keywidth  = NULL,
  keyheight = NULL,

  # ticks
  axis           = TRUE,
  axis.colour    = "black",
  axis.linewidth = NULL,
  axis.arrow     = NULL,

  ticks        = NULL,
  ticks.length = unit(0.2, "npc"),

  # general
  direction    = NULL,
  default.unit = "line",
  override.aes = list(),
  reverse      = FALSE,
  order        = 0,
  show.limits  = NULL,
  ...
) {

  if (!(is.null(keywidth) || is.unit(keywidth))) {
    keywidth <- unit(keywidth, default.unit)
  }
  if (!(is.null(keyheight) || is.unit(keyheight))) {
    keyheight <- unit(keyheight, default.unit)
  }
  if (!is.unit(ticks.length)) {
    ticks.length <- unit(ticks.length, default.unit)
  }
  if (!is.null(title.position)) {
    title.position <- arg_match0(title.position, .trbl)
  }
  if (!is.null(direction)) {
    direction <- arg_match0(direction, c("horizontal", "vertical"))
  }
  if (!is.null(label.position)) {
    label.position <- arg_match0(label.position, .trbl)
  }

  if (is.logical(axis)) {
    axis <- if (axis) element_line() else element_rect()
  }
  if (inherits(axis, "element_line")) {
    axis$colour    <- axis.colour    %||% axis$colour      %||% "black"
    axis$linewidth <- axis.linewidth %||% axis$linewidth   %||% (0.5 / .pt)
    axis$arrow     <- axis.arrow     %||% axis$arrow
  } else {
    axis <- element_blank()
  }

  if (is.null(ticks)) {
    ticks <- axis
    ticks$arrow <- NULL
  }

  new_guide(
    # title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    # key
    keywidth  = keywidth,
    keyheight = keyheight,

    # ticks
    line  = axis,
    ticks = ticks,
    ticks_length = ticks.length,

    # general
    direction = direction,
    override.aes = rename_aes(override.aes),
    reverse = reverse,
    order = order,
    show.limits = show.limits,

    # parameter
    available_aes = c("any"),
    ...,
    name = "bins",
    super = GuideBins
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideBins <- ggproto(
  "GuideBins", GuideLegend,

  params = list(
    title = waiver(),
    title.position = NULL,
    title.theme = NULL,
    title.hjust = NULL,
    title.vjust = NULL,

    label = TRUE,
    label.position = NULL,
    label.theme = NULL,
    label.hjust = NULL,
    label.vjust = NULL,

    keywidth  = NULL,
    keyheight = NULL,

    direction = NULL,
    override.aes = list(),
    reverse = FALSE,
    order = 0,
    show.limits = FALSE,

    name = "bins",
    hash = character(),
    position = NULL,
    direction = NULL
  ),

  elements = c(
    GuideLegend$elements,
    list(
      line  = "line",
      ticks = "line",
      ticks_length = unit(0.2, "npc")
    )
  ),

  extract_key = function(scale, aesthetic, show.limits = FALSE,
                         reverse = FALSE, ...) {

    breaks <- scale$get_breaks()

    parsed <- parse_binned_breaks(scale, breaks)
    if (is.null(parsed)) {
      return(parsed)
    }
    limits <- parsed$limits
    breaks <- parsed$breaks

    key <- data_frame(c(scale$map(parsed$bin_at), NA),
                      .name_repair = ~ aesthetic)
    key$.value <- (seq_along(key[[1]]) - 1) / (nrow(key) - 1)
    key$.show  <- NA

    labels <- scale$get_labels(breaks)
    if (is.character(scale$labels) || is.numeric(scale$labels)) {
      limit_lab <- c(NA, NA)
    } else {
      limit_lab <- scale$get_labels(limits)
    }
    if (!breaks[1] %in% limits) {
      labels <- c(limit_lab[1], labels)
    } else {
      key$.show[1] <- TRUE
    }
    if (!breaks[length(breaks)] %in% limits) {
      labels <- c(labels, limit_lab[2])
    } else {
      key$.show[nrow(key)] <- TRUE
    }

    key$.label <- labels

    return(key)
  },

  extract_params = function(scale, params,
                            title = waiver(), direction = NULL, ...) {

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
    show.limits <- rep(show.limits, length.out = 2)

    key <- params$key
    show <- key$.show[c(1, nrow(key))]
    show.limits <- ifelse(is.na(show), show.limits, show)
    key$.show <- NULL
    params$show.limits <- show.limits

    if (params$reverse) {
      key <- key[rev(seq_len(nrow(key))), , drop = FALSE]
      key$.value <- 1 - key$.value
    }

    params$title <- scale$make_title(
      params$title %|W|% scale$name %|W|% title
    )
    params$key <- key
    params
  },

  setup_params = function(params) {
    params$direction <- arg_match0(
      params$direction,
      c("horizontal", "vertical"), arg_nm = "direction"
    )
    valid_label_pos <- switch(
      params$direction,
      "horizontal" = c("bottom", "top"),
      "vertical"   = c("right",  "left")
    )
    params$label.position <- params$label.position %||% valid_label_pos[1]
    if (!params$label.position %in% valid_label_pos) {
      cli::cli_abort(paste0(
        "When {.arg direction} is {.val {params$direction}}, ",
        "{.arg label.position} must be one of {.or {.val {valid_label_pos}}}, ",
        "not {.val {params$label.position}}."
      ))
    }
    params <- GuideLegend$setup_params(params)
    params$byrow <- FALSE
    params$rejust_labels <- FALSE
    params$nrow <- params$ncol <- params$n_breaks <- params$n_key_layers <- 1
    params$multikey_decor <- FALSE
    params
  },

  override_elements = function(params, elements, theme) {
    elements$ticks <- combine_elements(elements$ticks, theme$line)
    elements$line  <- combine_elements(elements$line,  theme$line)
    GuideLegend$override_elements(params, elements, theme)
  },

  build_labels = function(key, elements, params) {
    n_labels <- length(key$.label)
    if (n_labels < 1) {
      return(list(labels = zeroGrob()))
    }
    key$.label[c(1, n_labels)[!params$show.limits]] <- ""

    if (params$direction == "vertical") {
      key$.value <- 1 - key$.value
    }

    list(labels = flip_element_grob(
      elements$text,
      label = key$.label,
      x = unit(key$.value, "npc"),
      margin_x = FALSE,
      margin_y = TRUE,
      flip = params$direction == "vertical"
    ))
  },

  build_ticks = function(key, elements, params, position = params$position) {
    if (params$direction == "vertical") {
      key$.value <- 1 - key$.value
    }
    key$.value[c(1, nrow(key))[!params$show.limits]] <- NA
    Guide$build_ticks(key$.value, elements, params, params$label.position)
  },

  build_decor = function(decor, grobs, elements, params) {
    params$n_breaks <- nkeys <- nrow(params$key) - 1

    dim <- if (params$direction == "vertical") c(nkeys, 1) else c(1, nkeys)

    decor <- GuideLegend$build_decor(decor, grobs, elements, params)

    sizes <- measure_legend_keys(
      decor, nkeys, dim, byrow = FALSE,
      default_width  = elements$key.width,
      default_height = elements$key.height
    )
    sizes <- lapply(sizes, function(x) rep_len(max(x), length(x)))

    n_layers <- length(decor) / nkeys
    key_id <- rep(seq_len(nkeys), each = n_layers)
    key_nm <- paste("key", key_id, c("bg", seq_len(n_layers - 1)))
    if (params$direction == "vertical") {
      top  <- key_id
      left <- 1
    } else {
      top  <- 1
      left <- key_id
    }
    gt <- gtable(
      widths  = unit(sizes$widths,  "cm"),
      heights = unit(sizes$heights, "cm")
    )
    gt <- gtable_add_grob(gt, decor, t = top, l = left,
                          name = key_nm, clip = "off")

    axis <- switch(
      params$label.position,
      "top"    = list(x = c(0, 1), y = c(1, 1)),
      "bottom" = list(x = c(0, 1), y = c(0, 0)),
      "left"   = list(x = c(0, 0), y = c(0, 1)),
      "right"  = list(x = c(1, 1), y = c(0, 1))
    )
    axis <- element_grob(elements$line, x = axis$x, y = axis$y)

    list(keys = gt, axis_line = axis, ticks = grobs$ticks)
  },

  measure_grobs = function(grobs, params, elements) {
    params$sizes <- list(
      widths  = sum( width_cm(grobs$decor$keys)),
      heights = sum(height_cm(grobs$decor$keys))
    )
    GuideLegend$measure_grobs(grobs, params, elements)
  }
)

parse_binned_breaks = function(scale, breaks = scale$get_breaks(),
                               even.steps = TRUE) {

  breaks <- breaks[!is.na(breaks)]
  if (length(breaks) == 0) {
    return(NULL)
  }
  breaks <- sort(breaks)
  if (is.numeric(breaks)) {
    limits <- scale$get_limits()
    if (!is.numeric(scale$breaks)) {
      breaks <- breaks[!breaks %in% limits]
    }
    all_breaks <- unique0(c(limits[1], breaks, limits[2]))
    bin_at <- all_breaks[-1] - diff(all_breaks) / 2
  } else {
    if (isFALSE(even.steps)) {
      cli::cli_warn(paste0(
        "{.code even.steps = FALSE} is not supported when used with a ",
        "discrete scale."
      ))
    }
    bin_at <- breaks
    nums   <- as.character(breaks)
    nums   <- strsplit(gsub("\\(|\\)|\\[|\\]", "", nums), ",\\s?")
    nums   <- as.numeric(unlist(nums, FALSE, FALSE))

    if (anyNA(nums)) {
      cli::cli_abort(c(
        "Breaks are not formatted correctly for a bin legend.",
        "i" = "Use {.code (<lower>, <upper>]} format to indicate bins."
      ))
    }
    all_breaks <- nums[c(1, seq_along(breaks) * 2)]
    limits     <- all_breaks[ c(1, length(all_breaks))]
    breaks     <- all_breaks[-c(1, length(all_breaks))]
  }
  list(
    breaks = breaks,
    limits = limits,
    bin_at = bin_at
  )
}
