#' Make a copy of a color scale for a different aesthetic.
#' Example use: `scale_outlinecolor_hue <- reuse_scale(scale_color_hue, "outlinecolor")`
#' @param scale A color scale to copy from
#' @param aesthetic The name of the new aesthetic or aesthetics to apply the color scale to
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

#' Find every `scale_colour_*()` in the environment and remap it for a different colour-based aesthetic.
#' The new scale functions will be named `scale_[new_aesthetics]_*()`.
#'
#' @param new_aesthetics The name of one or more new colour-based aesthetics to create scales for
#' @rdname reuse_scale
#' @export
reuse_all_colour_scales <- function(new_aesthetics = c("fill")) {
  new_aesthetic_name <- paste(new_aesthetics, collapse="")
  # get colour scales
  all_variable_names <- ls(envir = parent.env(current_env()))
  scale_colour_names <- all_variable_names[grep("^scale_colour_.*", all_variable_names)]
  scale_colour_functions <- lapply(scale_colour_names, match.fun)
  names(scale_colour_functions) <- scale_colour_names
  # only functions that have an aesthetics argument
  scale_colour_arguments <- lapply(scale_colour_functions, fn_fmls_names)
  scale_colour_has_aesthetics_arg <- sapply(scale_colour_arguments, function(args) "aesthetics" %in% args)
  scale_colour_functions <- scale_colour_functions[scale_colour_has_aesthetics_arg]
  if (any(!scale_colour_has_aesthetics_arg))
    warning(
      "The following scale functions could not be mapped to a new aesthetic because they do not have an `aesthetics` argument:\n",
      paste(scale_colour_names[!scale_colour_has_aesthetics_arg], collapse = "\n"))
  # copy the color scale to the new aesthetics
  new_scales = lapply(scale_colour_functions, function(f) reuse_scale(f, aesthetics=new_aesthetics))
  names(new_scales) <- sub("colour", new_aesthetic_name, names(new_scales))
  env_bind(parent.env(current_env()), !!!new_scales)
}
