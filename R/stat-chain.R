#' Chain statistic computation
#'
#' This statistic layer can take multiple stats and chain these together
#' to transform the data in a series of computations.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param stats A character vector or list of statistical transformations to use
#'   for this layer. Every element needs to be one of the following:
#'   * A `Stat` ggproto subclass, for example `StatCount`
#'   * A string naming the stat. To give the stat as a string, strip the
#'     function name of the `stat_` prefix. For example, to use `stat_count()`,
#'     give the stat as `"count"`.
#'   * The result of [`link_stat()`] to pass parameters or mapping instructions.
#'
#' @seealso [link_stat()]
#' @details
#' The procedure in which stats are chained are as follows. First, the
#' layer-level, undelayed aesthetics in the `mapping` argument are evaluated.
#' The data that results from that evaluation is passed to the first stat in
#' the `stats` argument to perform that stat's computation. If that first stat
#' is a [`link_stat`] with an `after.stat` component, the `after.stat` component
#' is evaluated before passing on the data to the next stat in the `stats`
#' argument. The next components in the `stats` argument work the same: the
#' data is passed on to compute the stat, then `after.stat` is evaluated. In
#' essence, the `after.stat` allows control over how computed variables are
#' passed to the next stat in the chain. Finally, once all components in the
#' `stats` arguments have been handled, the staged after stat components of
#' the layer-level `mapping` is evaluated. Per usual, the data are then handled
#' by the position and geom parts of a layer.
#'
#' @export
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, colour = drv))
#' # Binning unique observations
#' p + stat_chain(stats = c("unique", "bin"))
#' # Controlling parameters
#' p + stat_chain(
#'   stats = list("unique", link_stat("bin", bins = 10))
#' )
#' # Evaluate expressions after computing stats
#' p + stat_chain(stats = list(
#'   link_stat("unique",  after.stat = aes(x = x + 1)),
#'   link_stat("density", after.stat = aes(y = density))
#' ))
#' # Note that the last `after.stat` argument serves the same role as the
#' # `after_stat()` function in the layer mapping, so the following is
#' # equivalent to the previous plot
#' p + stat_chain(
#'   mapping = aes(y = after_stat(density)),
#'   stats = list(link_stat("unique", after.stat = aes(x = x + 1)), "density")
#' )
stat_chain <- function(
    mapping = NULL,
    data = NULL,
    geom = "path",
    position = "identity",
    ...,
    stats = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatChain,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      stats = stats,
      ...
    )
  )
}

#' Parameterise a statistic computation
#'
#' This is a helper function for [`stat_chain()`] to pass parameters and declare
#' mappings.
#'
#' @param stat The statistical transformation to use on the data. The `stat`
#'   argument accepts the following:
#'   * A `Stat` ggproto subclass, for example `StatCount`.
#'   * A string naming the stat. To give the stat as a string, strip the
#'   function name of the `stat_` prefix. For example, for `stat_count()`, give
#'   the string `"count"`.
#' @param ... Other arguments passed to the stat as a parameter.
#' @param after.stat Set of aesthetic mappings created by [`aes()`] to be
#'   evaluated only after the stat has been computed.
#'
#' @seealso [stat_chain()]
#' @returns A list bundling the stat, parameters and mapping.
#' @export
#' @keywords internal
#'
#' @examples
#' # See `?stat_chain`
link_stat <- function(stat, ..., after.stat = aes()) {
  if (inherits(stat, "linked_stat")) {
    return(stat)
  }

  stat <- validate_subclass(stat, "Stat")

  params <- list2(...)
  extra <- setdiff(names(params), stat$parameters(TRUE))
  if (length(extra) > 0) {
    cli::cli_warn("Ignoring unknown parameters: {.arg {extra}}.")
    params <- params[setdiff(names(params), extra)]
  }

  structure(
    list(stat = stat, params = params, after_stat = validate_mapping(after.stat)),
    class = "linked_stat"
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatChain <- ggproto(
  "StatChain", Stat,

  extra_params = c("na.rm", "stats"),

  setup_params = function(data, params) {
    if (inherits(params$stats, "linked_stat")) {
      # When a single linked stat is passed outside a list, repair to list
      # When using a single stat, using the appropriate `stat_*()` constructor
      # is better, but we should consider programmatic use too.
      params$stats <- list(params$stats)
    }

    params$stats <- lapply(params$stats, link_stat)
    params
  },

  compute_layer = function(self, data, params, layout) {

    for (i in seq_along(params$stats)) {
      link <- params$stats[[i]]
      stat <- link$stat

      # Repeat `Layer$compute_statistic()` duty
      computed_param <- stat$setup_params(data, link$params)
      computed_param$na.rm <- computed_param$na.rm %||% params$na.rm
      data <- stat$setup_data(data, computed_param)
      data <- stat$compute_layer(data, computed_param, layout)
      if (nrow(data) < 1) {
        return(data)
      }

      # Repeat `Layer$map_statistic()` duty, skipping backtransforms and such
      aes <- stat$default_aes[is_calculated_aes(stat$default_aes)]
      # TODO: ideally we'd have access to Layer$computed_mapping to properly
      # not touch user-specified mappings.
      aes <- aes[setdiff(names(aes), names(data))]
      aes <- compact(defaults(link$after_stat, aes))
      if (length(aes) == 0) {
        next
      }
      new <- eval_aesthetics(substitute_aes(aes), data)
      check_nondata_cols(
        new, aes,
        problem = "Aesthetics must be valid computed stats.",
        hint    = "Did you specify the `redirect` argument correctly?"
      )
      data[names(new)] <- new
    }

    data
  }
)
