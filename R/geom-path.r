#' Connect observations.
#'
#' \code{geom_path()} connects the observations in the order in which they appear
#' in the data. \code{geom_line()} connects them in order of the variable on the
#' x axis. \code{geom_step()} creates a stairstep plot, highlighting exactly
#' when changes occur.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "path")}
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param lineend Line end style (round, butt, square)
#' @param linejoin Line join style (round, mitre, bevel)
#' @param linemitre Line mitre limit (number greater than 1)
#' @param arrow Arrow specification, as created by \code{\link[grid]{arrow}}
#' @seealso
#'  \code{\link{geom_polygon}}: Filled paths (polygons);
#'  \code{\link{geom_segment}}: Line segments
#' @export
#' @examples
#' # geom_line() is suitable for time series
#' ggplot(economics, aes(date, unemploy)) + geom_line()
#' ggplot(economics_long, aes(date, value01, colour = variable)) +
#'   geom_line()
#'
#' # geom_step() is useful when you want to highlight exactly when
#' # the y value chanes
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
#' base + geom_path(size = 10)
#' base + geom_path(size = 10, lineend = "round")
#' base + geom_path(size = 10, linejoin = "mitre", lineend = "butt")
#'
#' # NAs break the line. Use na.rm = T to suppress the warning message
#' df <- data.frame(
#'   x = 1:5,
#'   y1 = c(1, 2, 3, 4, NA),
#'   y2 = c(NA, 2, 3, 4, 5),
#'   y3 = c(1, 2, NA, 4, 5)
#' )
#' ggplot(df, aes(x, y1)) + geom_point() + geom_line()
#' ggplot(df, aes(x, y2)) + geom_point() + geom_line()
#' ggplot(df, aes(x, y3)) + geom_point() + geom_line()
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
                      linemitre = 1,
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
    params = list(
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

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  handle_na = function(data, params) {
    keep <- function(x) {
      # from first non-missing to last non-missing
      first <- match(FALSE, x, nomatch = 1) - 1
      last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
      c(
        rep(FALSE, first),
        rep(TRUE, last - first),
        rep(FALSE, length(x) - last)
      )
    }
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    missing <- !stats::complete.cases(data[c("x", "y", "size", "colour",
      "linetype")])
    kept <- stats::ave(missing, data$group, FUN = keep)
    data <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      warning("Removed ", sum(!kept), " rows containing missing values",
        " (geom_path).", call. = FALSE)
    }

    data
  },

  draw_panel = function(data, panel_scales, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 1,
                        na.rm = FALSE) {
    if (!anyDuplicated(data$group)) {
      message_wrap("geom_path: Each group consists of only one observation. ",
        "Do you need to adjust the group aesthetic?")
    }

    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_scales)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())

    # Work out whether we should use lines or segments
    attr <- plyr::ddply(munched, "group", function(df) {
      linetype <- unique(df$linetype)
      data.frame(
        solid = identical(linetype, 1) || identical(linetype, "solid"),
        constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      stop("geom_path: If you are using dotted or dashed lines",
        ", colour, size and linetype must be constant over the line",
        call. = FALSE)
    }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)

    if (!constant) {
      segmentsGrob(
        munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
        default.units = "native", arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[!end],
          fill = alpha(munched$colour, munched$alpha)[!end],
          lwd = munched$size[!end] * .pt,
          lty = munched$linetype[!end],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
    } else {
      id <- match(munched$group, unique(munched$group))
      polylineGrob(
        munched$x, munched$y, id = id,
        default.units = "native", arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[start],
          fill = alpha(munched$colour, munched$alpha)[start],
          lwd = munched$size[start] * .pt,
          lty = munched$linetype[start],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
    }
  },

  draw_key = draw_key_path
)

#' @export
#' @rdname geom_path
geom_line <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.r
GeomLine <- ggproto("GeomLine", GeomPath,
  setup_data = function(data, params) {
    data[order(data$PANEL, data$group, data$x), ]
  }
)

#' @param direction direction of stairs: 'vh' for vertical then horizontal, or
#'   'hv' for horizontal then vertical
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
    params = list(
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
#' @include geom-path.r
GeomStep <- ggproto("GeomStep", GeomPath,
  draw_panel = function(data, panel_scales, coord, direction = "hv") {
    data <- plyr::ddply(data, "group", stairstep, direction = direction)
    GeomPath$draw_panel(data, panel_scales, coord)
  }
)

# Calculate stairsteps
# Used by \code{\link{geom_step}}
#
# @keyword internal
stairstep <- function(data, direction="hv") {
  direction <- match.arg(direction, c("hv", "vh"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)

  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each = 2))
  } else {
    ys <- rep(1:n, each = 2)[-2*n]
    xs <- c(1, rep(2:n, each = 2))
  }

  data.frame(
    x = data$x[xs],
    y = data$y[ys],
    data[xs, setdiff(names(data), c("x", "y"))]
  )
}
