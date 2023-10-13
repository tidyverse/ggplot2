#' Make a copy of a color scale for a different aesthetic
#' @param scale A color scale to copy from
#' @param aesthetic The new aesthetic or aesthetics to apply the color scale to
#' @export
reuse_scale <- function(scale, aesthetics) {
  check_function(scale)
  if (!"aesthetics" %in% fn_fmls_names(scale)) {
    cli::cli_abort(c(
      "The {.arg {deparse(substitute(scale))}} function must have an {.arg aesthetics} argument.",
      i = "{.fn {deparse(substitute(scale))}} does not have that argument."
    ))
  }
  formals(scale)$aesthetics <- aesthetics
  scale
}
