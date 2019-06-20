#' @section Computed variables:
#' \describe{
#'   \item{count}{number of points in bin}
#'   \item{prop}{groupwise proportion}
#' }
#' @seealso [stat_bin()], which bins data in ranges and counts the
#'   cases in each range. It differs from `stat_count`, which counts the
#'   number of cases at each `x` position (without binning into ranges).
#'   [stat_bin()] requires continuous `x` data, whereas
#'   `stat_count` can be used for both discrete and continuous `x` data.
#'   `..prop..` corresponds to groupwise proportions, i.e. all proportions
#'   for a specific group sum to 1. The group could be define using the
#'   `group` aesthetic (see examples).
#'
#' @export
#' @rdname geom_bar
stat_count <- function(mapping = NULL, data = NULL,
                       geom = "bar", position = "stack",
                       ...,
                       width = NULL,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  params <- list(
    na.rm = na.rm,
    width = width,
    ...
  )
  if (!is.null(params$y)) {
    stop("stat_count() must not be used with a y aesthetic.", call. = FALSE)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatCount,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include stat-.r
StatCount <- ggproto("StatCount", Stat,
  required_aes = "x",
  default_aes = aes(y = stat(count), weight = 1),

  setup_params = function(data, params) {
    if (!is.null(data$y)) {
      stop("stat_count() must not be used with a y aesthetic.", call. = FALSE)
    }
    params
  },

  compute_panel = function(self, data, scales, width = NULL) {
    data$weight <- data$weight %||% rep(1, nrow(data))
    width <- width %||% (resolution(data$x) * 0.9)

    # sum weights for each combination of group and aesthetics
    # the use of . allows to consider all aesthetics defined in data
    cp <- aggregate(weight ~ ., data = data, sum, na.rm = TRUE)
    names(cp)[which(names(cp) == "weight")] <- "count"
    cp$count[is.na(cp$count)] <- 0

    # groupwise proportions
    f <- function(x) {sum(abs(x))}
    cp$prop <- cp$count / ave(cp$count, cp$group, FUN = f)
    cp$width <- width

    cp
  }
)
