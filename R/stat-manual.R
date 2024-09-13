
#' Manually compute transformations
#'
#' `stat_manual()` takes a function that computes a data transformation for
#' every group.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param fun Function that takes a data frame as input and returns a data
#'   frame or data frame-like list as output. The default (`identity()`) returns
#'   the data unchanged.
#' @param args A list of arguments to pass to the function given in `fun`.
#'
#' @eval rd_aesthetics("stat", "manual")
#' @section Aesthetics:
#' Input aesthetics are determined by the `fun` argument. Output aesthetics must
#' include those required by `geom`. Any aesthetic that is constant within a
#' group will be preserved even if dropped by `fun`.
#'
#' @export
#'
#' @examples
#' # A standard scatterplot
#' p <- ggplot(mtcars, aes(disp, mpg, colour = factor(cyl))) +
#'   geom_point()
#'
#' # The default just displays points as-is
#' p + stat_manual()
#'
#' # Using a custom function
#' make_hull <- function(data) {
#'   hull <- chull(x = data$x, y = data$y)
#'   data.frame(x = data$x[hull], y = data$y[hull])
#' }
#'
#' p + stat_manual(
#'   geom = "polygon",
#'   fun  = make_hull,
#'   fill = NA
#' )
#'
#' # Using the `with` function with quoting
#' p + stat_manual(
#'   fun  = with,
#'   args = list(expr = quote({
#'     hull <- chull(x, y)
#'     list(x = x[hull], y = y[hull])
#'   })),
#'   geom = "polygon", fill = NA
#' )
#'
#' # Using the `transform` function with quoting
#' p + stat_manual(
#'   geom = "segment",
#'   fun  = transform,
#'   args = list(
#'     xend = quote(mean(x)),
#'     yend = quote(mean(y))
#'   )
#' )
#'
#' # Using dplyr verbs with `vars()`
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'
#'   # Get centroids with `summarise()`
#'   p + stat_manual(
#'     size = 10, shape = 21,
#'     fun  = dplyr::summarise,
#'     args = vars(x = mean(x), y = mean(y))
#'   )
#'
#'   # Connect to centroid with `mutate`
#'   p + stat_manual(
#'     geom = "segment",
#'     fun  = dplyr::mutate,
#'     args = vars(xend = mean(x), yend = mean(y))
#'   )
#'
#'   # Computing hull with `reframe()`
#'   p + stat_manual(
#'     geom = "polygon", fill = NA,
#'     fun  = dplyr::reframe,
#'     args = vars(hull = chull(x, y), x = x[hull], y = y[hull])
#'   )
#' }
stat_manual <- function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    ...,
    fun = identity,
    args = list(),
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatManual,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      fun = fun,
      args = args,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatManual <- ggproto(
  "StatManual", Stat,

  setup_params = function(data, params) {
    params$fun <- allow_lambda(params$fun)
    check_function(params$fun, arg = "fun")
    params
  },

  compute_group = function(data, scales, fun = identity, args = list()) {
    as_gg_data_frame(inject(fun(data, !!!args)))
  }
)
