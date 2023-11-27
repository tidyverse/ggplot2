#' Connect observations
#'
#' `geom_path()` connects the observations in the order in which they appear
#' in the data. `geom_line()` connects them in order of the variable on the
#' x axis. `geom_step()` creates a stairstep plot, highlighting exactly
#' when changes occur. The `group` aesthetic determines which cases are
#' connected together.
#'
#' An alternative parameterisation is [geom_segment()], where each line
#' corresponds to a single case which provides the start and end coordinates.
#'
#' @eval rd_orientation()
#'
#' @eval rd_aesthetics("geom", "path")
#' @inheritParams layer
#' @inheritParams geom_bar
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param arrow Arrow specification, as created by [grid::arrow()].
#' @seealso
#'  [geom_polygon()]: Filled paths (polygons);
#'  [geom_segment()]: Line segments
#' @section Missing value handling:
#' `geom_path()`, `geom_line()`, and `geom_step()` handle `NA` as follows:
#'
#' * If an `NA` occurs in the middle of a line, it breaks the line. No warning
#'   is shown, regardless of whether `na.rm` is `TRUE` or `FALSE`.
#' * If an `NA` occurs at the start or the end of the line and `na.rm` is `FALSE`
#'   (default), the `NA` is removed with a warning.
#' * If an `NA` occurs at the start or the end of the line and `na.rm` is `TRUE`,
#'   the `NA` is removed silently, without warning.
#' @export
#' @examples
#' # geom_line() is suitable for time series
#' ggplot(economics, aes(date, unemploy)) + geom_line()
#' ggplot(economics_long, aes(date, value01, colour = variable)) +
#'   geom_line()
#'
#' # You can get a timeseries that run vertically by setting the orientation
#' ggplot(economics, aes(unemploy, date)) + geom_line(orientation = "y")
#'
#' # geom_step() is useful when you want to highlight exactly when
#' # the y value changes
#' recent <- economics[economics$date > as.Date("2013-01-01"), ]
#' ggplot(recent, aes(date, unemploy)) + geom_line()
#' ggplot(recent, aes(date, unemploy)) + geom_step()
#'
#' # geom_path lets you explore how two variables are related over time,
#' # e.g. unemployment and personal savings rate
#' m <- ggplot(economics, aes(unemploy/pop, psavert))
#' m + geom_path()
#' m + geom_path(aes(colour = as.numeric(date)))
#'
#' # Changing parameters ----------------------------------------------
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_line(colour = "red")
#'
#' # Use the arrow parameter to add an arrow to the line
#' # See ?arrow for more details
#' c <- ggplot(economics, aes(x = date, y = pop))
#' c + geom_line(arrow = arrow())
#' c + geom_line(
#'   arrow = arrow(angle = 15, ends = "both", type = "closed")
#' )
#'
#' # Control line join parameters
#' df <- data.frame(x = 1:3, y = c(4, 1, 9))
#' base <- ggplot(df, aes(x, y))
#' base + geom_path(linewidth = 10)
#' base + geom_path(linewidth = 10, lineend = "round")
#' base + geom_path(linewidth = 10, linejoin = "mitre", lineend = "butt")
#'
#' # You can use NAs to break the line.
#' df <- data.frame(x = 1:5, y = c(1, 2, NA, 4, 5))
#' ggplot(df, aes(x, y)) + geom_point() + geom_line()
#'
#' \donttest{
#' # Setting line type vs colour/size
#' # Line type needs to be applied to a line as a whole, so it can
#' # not be used with colour or size that vary across a line
#' x <- seq(0.01, .99, length.out = 100)
#' df <- data.frame(
#'   x = rep(x, 2),
#'   y = c(qlogis(x), 2 * qlogis(x)),
#'   group = rep(c("a","b"),
#'   each = 100)
#' )
#' p <- ggplot(df, aes(x=x, y=y, group=group))
#' # These work
#' p + geom_line(linetype = 2)
#' p + geom_line(aes(colour = group), linetype = 2)
#' p + geom_line(aes(colour = x))
#' # But this doesn't
#' should_stop(p + geom_line(aes(colour = x), linetype=2))
#' }
geom_path <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      lineend = "butt",
                      linejoin = "round",
                      linemitre = 10,
                      arrow = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPath <- ggproto("GeomPath", Geom,
  required_aes = c("x", "y"),

  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),

  non_missing_aes = c("linewidth", "colour", "linetype"),

  handle_na = function(self, data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    aesthetics <- c(self$required_aes, self$non_missing_aes)
    complete <- stats::complete.cases(data[names(data) %in% aesthetics])
    kept <- stats::ave(complete, data$group, FUN = keep_mid_true)
    data <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      cli::cli_warn(paste0(
        "Removed {sum(!kept)} row{?s} containing missing values or values ",
        "outside the scale range ({.fn {snake_class(self)}})."
      ))
    }

    data
  },

  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {
    data <- check_linewidth(data, snake_class(self))
    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(
        "{.fn {snake_class(self)}}: Each group consists of only one observation.",
        i = "Do you need to adjust the {.field group} aesthetic?"
      ))
    }

    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())

    # Work out whether we should use lines or segments
    attr <- dapply(munched, "group", function(df) {
      linetype <- unique0(df$linetype)
      data_frame0(
        solid = identical(linetype, 1) || identical(linetype, "solid"),
        constant = nrow(unique0(df[, names(df) %in% c("alpha", "colour", "linewidth", "linetype")])) == 1,
        .size = 1
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      cli::cli_abort("{.fn {snake_class(self)}} can't have varying {.field colour}, {.field linewidth}, and/or {.field alpha} along the line when {.field linetype} isn't solid")
    }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)

    if (!constant) {

      arrow <- repair_segment_arrow(arrow, munched$group)

      segmentsGrob(
        munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
        default.units = "native", arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[!end],
          fill = alpha(munched$colour, munched$alpha)[!end],
          lwd = munched$linewidth[!end] * .pt,
          lty = munched$linetype[!end],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
    } else {
      id <- match(munched$group, unique0(munched$group))
      polylineGrob(
        munched$x, munched$y, id = id,
        default.units = "native", arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[start],
          fill = alpha(munched$colour, munched$alpha)[start],
          lwd = munched$linewidth[start] * .pt,
          lty = munched$linetype[start],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
    }
  },

  draw_key = draw_key_path,

  rename_size = TRUE
)

# Trim false values from left and right: keep all values from
# first TRUE to last TRUE
keep_mid_true <- function(x) {
  first <- match(TRUE, x) - 1
  if (is.na(first)) {
    return(rep(FALSE, length(x)))
  }

  last <- length(x) - match(TRUE, rev(x)) + 1
  c(
    rep(FALSE, first),
    rep(TRUE, last - first),
    rep(FALSE, length(x) - last)
  )
}


#' @export
#' @rdname geom_path
geom_line <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, orientation = NA,
                      show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.R
GeomLine <- ggproto("GeomLine", GeomPath,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    flip_data(data, params$flipped_aes)
  }
)

#' @param direction direction of stairs: 'vh' for vertical then horizontal,
#'   'hv' for horizontal then vertical, or 'mid' for step half-way between
#'   adjacent x-values.
#' @export
#' @rdname geom_path
geom_step <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", direction = "hv",
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStep,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      direction = direction,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.R
GeomStep <- ggproto("GeomStep", GeomPath,
  draw_panel = function(data, panel_params, coord, direction = "hv") {
    data <- dapply(data, "group", stairstep, direction = direction)
    GeomPath$draw_panel(data, panel_params, coord)
  }
)

#' Calculate stairsteps for `geom_step()`
#' Used by `GeomStep()`
#'
#' @noRd
stairstep <- function(data, direction = "hv") {
  direction <- arg_match0(direction, c("hv", "vh", "mid"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)

  if (n <= 1) {
    # Need at least one observation
    return(data[0, , drop = FALSE])
  }

  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each = 2))
  } else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2*n]
    xs <- c(1, rep(2:n, each = 2))
  } else if (direction == "mid") {
    xs <- rep(1:(n-1), each = 2)
    ys <- rep(1:n, each = 2)
  } else {
    cli::cli_abort(c(
      "{.arg direction} is invalid.",
      "i" = "Use either {.val vh}, {.val hv}, or {.va mid}"
    ))
  }

  if (direction == "mid") {
    gaps <- data$x[-1] - data$x[-n]
    mid_x <- data$x[-n] + gaps/2 # map the mid-point between adjacent x-values
    x <- c(data$x[1], mid_x[xs], data$x[n])
    y <- c(data$y[ys])
    data_attr <- data[c(1,xs,n), setdiff(names(data), c("x", "y"))]
  } else {
    x <- data$x[xs]
    y <- data$y[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "y"))]
  }

  data_frame0(x = x, y = y, data_attr)
}

repair_segment_arrow <- function(arrow, group) {
  # Early exit if there is no arrow
  if (is.null(arrow)) {
    return(arrow)
  }

  # Get group parameters
  rle       <- vec_group_rle(group) # handles NAs better than base::rle()
  n_groups  <- length(rle)
  rle_len   <- field(rle, "length") - 1 # segments have 1 member less than lines
  rle_end   <- cumsum(rle_len)
  rle_start <- rle_end - rle_len + 1

  # Recycle ends and lengths
  ends <- rep(rep(arrow$ends,   length.out = n_groups), rle_len)
  len  <- rep(rep(arrow$length, length.out = n_groups), rle_len)

  # Repair ends
  # Convert 'both' ends to first/last in multi-member groups
  is_both <- which(ends == 3)
  ends[setdiff(intersect(rle_start, is_both), rle_end)] <- 1L
  ends[setdiff(intersect(rle_end, is_both), rle_start)] <- 2L
  arrow$ends <- ends

  # Repair lengths
  zero <- unit(0, "mm")
  # Set length of first segment to zero when ends is 'last'
  len[intersect(setdiff(rle_start, rle_end), which(ends == 2))] <- zero
  # Set length of last segment to zero when ends is 'first'
  len[intersect(setdiff(rle_end, rle_start), which(ends == 1))] <- zero
  # Set length of middle pieces to zero
  len[setdiff(seq_along(len), c(rle_start, rle_end))] <- zero
  arrow$length <- len

  return(arrow)
}
