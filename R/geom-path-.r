#' Connect observations in original order
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "path")}
#'
#' @inheritParams geom_point
#' @param lineend Line end style (round, butt, square)
#' @param linejoin Line join style (round, mitre, bevel)
#' @param linemitre Line mitre limit (number greater than 1)
#' @param arrow Arrow specification, as created by ?grid::arrow
#' @seealso \code{\link{geom_line}}: Functional (ordered) lines;
#'  \code{\link{geom_polygon}}: Filled paths (polygons);
#'  \code{\link{geom_segment}}: Line segments
#' @export
#' @examples
#' \donttest{
#' # Generate data
#' library(plyr)
#' myear <- ddply(movies, .(year), colwise(mean, .(length, rating)))
#' p <- ggplot(myear, aes(length, rating))
#' p + geom_path()
#'
#' # Add aesthetic mappings
#' p + geom_path(aes(size = year))
#' p + geom_path(aes(colour = year))
#'
#' # Change scale
#' p + geom_path(aes(size = year)) + scale_size(range = c(1, 3))
#'
#' # Set aesthetics to fixed value
#' p + geom_path(colour = "green")
#'
#' # Control line join parameters
#' df <- data.frame(x = 1:3, y = c(4, 1, 9))
#' base <- ggplot(df, aes(x, y))
#' base + geom_path(size = 10)
#' base + geom_path(size = 10, lineend = "round")
#' base + geom_path(size = 10, linejoin = "mitre", lineend = "butt")
#'
#' # Use qplot instead
#' qplot(length, rating, data=myear, geom="path")
#'
#' # Using economic data:
#' # How is unemployment and personal savings rate related?
#' qplot(unemploy/pop, psavert, data=economics)
#' qplot(unemploy/pop, psavert, data=economics, geom="path")
#' qplot(unemploy/pop, psavert, data=economics, geom="path", size=as.numeric(date))
#'
#' # How is rate of unemployment and length of unemployment?
#' qplot(unemploy/pop, uempmed, data=economics)
#' qplot(unemploy/pop, uempmed, data=economics, geom="path")
#' qplot(unemploy/pop, uempmed, data=economics, geom="path") +
#'   geom_point(data=head(economics, 1), colour="red") +
#'   geom_point(data=tail(economics, 1), colour="blue")
#' qplot(unemploy/pop, uempmed, data=economics, geom="path") +
#'   geom_text(data=head(economics, 1), label="1967", colour="blue") +
#'   geom_text(data=tail(economics, 1), label="2007", colour="blue")
#'
#' # geom_path removes missing values on the ends of a line.
#' # use na.rm = T to suppress the warning message
#' df <- data.frame(
#'   x = 1:5,
#'   y1 = c(1, 2, 3, 4, NA),
#'   y2 = c(NA, 2, 3, 4, 5),
#'   y3 = c(1, 2, NA, 4, 5),
#'   y4 = c(1, 2, 3, 4, 5))
#' qplot(x, y1, data = df, geom = c("point","line"))
#' qplot(x, y2, data = df, geom = c("point","line"))
#' qplot(x, y3, data = df, geom = c("point","line"))
#' qplot(x, y4, data = df, geom = c("point","line"))
#'
#' # Setting line type vs colour/size
#' # Line type needs to be applied to a line as a whole, so it can
#' # not be used with colour or size that vary across a line
#'
#' x <- seq(0.01, .99, length=100)
#' df <- data.frame(x = rep(x, 2), y = c(qlogis(x), 2 * qlogis(x)), group = rep(c("a","b"), each=100))
#' p <- ggplot(df, aes(x=x, y=y, group=group))
#'
#' # Should work
#' p + geom_line(linetype = 2)
#' p + geom_line(aes(colour = group), linetype = 2)
#' p + geom_line(aes(colour = x))
#'
#' # Should fail
#' should_stop(p + geom_line(aes(colour = x), linetype=2))
#'
#' # Use the arrow parameter to add an arrow to the line
#' # See ?grid::arrow for more details
#' library(grid)
#' c <- ggplot(economics, aes(x = date, y = pop))
#' # Arrow defaults to "last"
#' c + geom_path(arrow = arrow())
#' c + geom_path(arrow = arrow(angle = 15, ends = "both", length = unit(0.6, "inches")))
#' }
geom_path <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, arrow = NULL, ...) {
  GeomPath$new(mapping = mapping, data = data, stat = stat, position = position,
  lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, arrow = arrow, ...)
}

GeomPath <- proto(Geom, {
  objname <- "path"

  draw_groups <- function(., ...) .$draw(...)

  draw <- function(., data, scales, coordinates, arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, ..., na.rm = FALSE) {
    if (!anyDuplicated(data$group)) {
      message("geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?")
    }

    keep <- function(x) {
      # from first non-missing to last non-missing
      first <- match(FALSE, x, nomatch = 1) - 1
      last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
      c(
        rep(FALSE, first),
        rep(TRUE, last - first),
        rep(FALSE, length(x) - last))
    }
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    missing <- !complete.cases(data[c("x", "y", "size", "colour",
      "linetype")])
    kept <- ave(missing, data$group, FUN=keep)
    data <- data[kept, ]
    # must be sorted on group
    data <- arrange(data, group)

    if (!all(kept) && !na.rm) {
      warning("Removed ", sum(!kept), " rows containing missing values",
        " (geom_path).", call. = FALSE)
    }

    munched <- coord_munch(coordinates, data, scales)

    # Silently drop lines with less than two points, preserving order
    rows <- ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())

    # Work out whether we should use lines or segments
    attr <- ddply(munched, .(group), function(df) {
      data.frame(
        solid = identical(unique(df$linetype), 1),
        constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      stop("geom_path: If you are using dotted or dashed lines",
        ", colour, size and linetype must be constant over the line",
        call.=FALSE)
    }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)

    if (!constant) {
      with(munched,
        segmentsGrob(
          x[!end], y[!end], x[!start], y[!start],
          default.units="native", arrow = arrow,
          gp = gpar(
            col = alpha(colour, alpha)[!end], fill = alpha(colour, alpha)[!end],
            lwd = size[!end] * .pt, lty = linetype[!end],
            lineend = lineend, linejoin = linejoin, linemitre = linemitre
          )
        )
      )
    } else {
      id <- match(munched$group, unique(munched$group))
      with(munched,
        polylineGrob(
          x, y, id = id,
          default.units = "native", arrow = arrow,
          gp = gpar(
            col = alpha(colour, alpha)[start], fill = alpha(colour, alpha)[start],
            lwd = size[start] * .pt, lty = linetype[start],
            lineend = lineend, linejoin = linejoin, linemitre = linemitre)
        )
      )
    }
  }

  draw_legend <- function(., data, ...) {
    data$arrow <- NULL
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
      ggname(.$my_name(), segmentsGrob(0.1, 0.5, 0.9, 0.5, default.units="npc",
      gp=gpar(col=alpha(colour, alpha), lwd=size * .pt,
        lty=linetype, lineend="butt")))
    )
  }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"

})

