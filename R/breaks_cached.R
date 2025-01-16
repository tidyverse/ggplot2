#' Cache scale breaks
#'
#' This helper caches the output of another breaks function the first time it is
#' evaluated. All subsequent calls will return the same breaks vector
#' regardless of the provided limits. In general this is not what you want
#' since the breaks should change when the limits change. It is helpful in the
#' specific case that you are using `follow.scale` on [stat_bin()] and related
#' binning stats, because it ensures that the breaks are not recomputed after
#' they are used to define the bin edges.
#'
#' @export
#' @param breaks A function that takes the limits as input and returns breaks
#'   as output. See [continuous_scale()] for details.
#'
#' @return A wrapped breaks function suitable for use with ggplot scales.
#' @examples
#' discoveries_df <- data.frame(
#'   year = unlist(mapply(rep, time(discoveries), discoveries))
#' )
#' p <- ggplot(discoveries_df, aes(year)) +
#'  geom_histogram(follow.scale = "minor")
#'
#' # Using follow.scale with function breaks can cause misalignment as the scale
#' # can update the breaks after the bin edges are fixed by the stat
#' p + scale_x_continuous(breaks = scales::breaks_extended())
#'
#' # Wrapping the same breaks function avoids this issue but can leave you with
#' # sub-optimal breaks since they are no longer updated after stats
#' p + scale_x_continuous(breaks = breaks_cached(scales::breaks_extended()))
breaks_cached <- function(breaks) {
  if (! rlang::is_function(breaks)) {
    cli::cli_abort("{.arg breaks} must be a function")
  }

  cached <- ggplot2::ggproto(
    "BreaksCached", NULL,
    fn = breaks,
    cached = NULL,
    get_breaks = function(self, limits) {
      if (is.null(self$cached)) self$cached <- self$fn(limits)
      self$cached
    }
  )$get_breaks

  class(cached) <- c("ggplot2_cached_breaks", class(cached))
  cached
}

#' @export
format.ggplot2_cached_breaks <- function(x, ...) {
  bc <- environment(x)$self
  inner <- environment(bc$fn)$f

  paste0(
    "<cached breaks function>\n",
    ifelse(
      is.null(bc$cached),
      paste0("  ", format(inner), collapse = "\n"),
      paste0("  [", class(bc$cached), "] ", paste0(format(bc$cached), collapse = " "))
    )
  )
}

#' @export
print.ggplot2_cached_breaks <- function(x, ...) {
  cat(format(x), sep = "")
}
