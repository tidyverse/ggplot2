#' Scale for line patterns
#'
#' Default line types based on a set supplied by Richard Pearson,
#' University of Manchester. Continuous values can not be mapped to
#' line types unless `scale_linetype_binned()` is used. Still, as linetypes has
#' no inherent order, this use is not advised.
#'
#' @inheritParams scale_x_discrete
#' @inheritDotParams discrete_scale -expand -position -na.value -scale_name -palette
#' @param na.value The linetype to use for `NA` values.
#' @rdname scale_linetype
#' @seealso
#' The documentation for [differentiation related aesthetics][aes_linetype_size_shape].
#'
#' Other linetype scales: [scale_linetype_manual()], [scale_linetype_identity()].
#'
#' The `r link_book("line type section", "scales-other#sec-scale-linetype")`
#' @export
#' @examples
#' base <- ggplot(economics_long, aes(date, value01))
#' base + geom_line(aes(group = variable))
#' base + geom_line(aes(linetype = variable))
#'
#' # See scale_manual for more flexibility
#'
#' # Common line types ----------------------------
#' df_lines <- data.frame(
#'   linetype = factor(
#'     1:4,
#'     labels = c("solid", "longdash", "dashed", "dotted")
#'   )
#' )
#' ggplot(df_lines) +
#'   geom_hline(aes(linetype = linetype, yintercept = 0), linewidth = 2) +
#'   scale_linetype_identity() +
#'   facet_grid(linetype ~ .) +
#'   theme_void(20)
scale_linetype <- function(name = waiver(), ..., na.value = NA) {
  discrete_scale(
    "linetype", name = name,
    palette = NULL,
    na.value = na.value,
    ...
  )
}

#' @rdname scale_linetype
#' @export
scale_linetype_binned <- function(name = waiver(), ..., na.value = NA) {
  binned_scale(
    "linetype", name = name,
    palette = NULL,
    na.value = na.value,
    ...
  )
}

#' @rdname scale_linetype
#' @export
scale_linetype_continuous <- function(...) {
  cli::cli_abort(c(
    "A continuous variable cannot be mapped to the {.field linetype} aesthetic.",
    "i" = "Choose a different aesthetic or use {.fn scale_linetype_binned}."
  ))
}
#' @rdname scale_linetype
#' @export
scale_linetype_discrete <- scale_linetype
