#' Partial scales
#'
#' Partial scales are useful for setting some scale parameters without
#' committing to any particular scale yet. Partial scales can be added to a plot
#' and will be combined with full scales.
#'
#' @param aesthetic A string specifying an aesthetic to create a partial
#'   scale for.
#' @param ... Arguments passed onto full scales.
#' @param call A `<call>` to report in warning and error messages.
#'
#' @return A `<ScalePartial>` object that can be added to a plot.
#' @export
#' @name partial-scales
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Adding a partial scale
#' p + scale_y(trans = "sqrt")
#'
#' # Partial scales can be stacked
#' p + scale_y(trans = "sqrt") + scale_y(breaks = seq(15, 45, by = 5))
#'
#' # When two scales declare the same parameter, the latter overrules the first
#' p + scale_y_continuous(name = "Highway Miles") +
#'   scale_y(name = "Title from partial scale")
#'
#' # But other parameters are kept and not overruled
#' p + scale_y(name = "Highway Miles",
#'             breaks = c(20, 30, 40),
#'             labels = c("A", "B", "C")) +
#'   scale_y_continuous(name = "Title from full scale")
scale_partial <- function(aesthetic, ..., call = caller_call()) {

  check_string(aesthetic, allow_empty = FALSE)
  aesthetic <- standardise_aes_names(aesthetic)

  args <- dots_list(..., .homonyms = "error")
  if (!is_named(args)) {
    cli::cli_abort("All arguments in {.code ...} must be named.", call = call)
  }

  args <- args[!vapply(args, is.waive, logical(1))]

  lambdas <- intersect(
    names(args),
    c("limits", "breaks", "labels", "rescaler", "oob", "minor_breaks")
  )
  args[lambdas] <- lapply(args[lambdas], allow_lambda)

  call <- call %||% current_call()

  ggproto(
    NULL, ScalePartial,
    call = call,
    aesthetics = aesthetic,
    params = args
  )
}

#' @export
#' @rdname partial-scales
scale_x <- function(...) scale_partial(aesthetic = "x", ...)
#' @export
#' @rdname partial-scales
scale_y <- function(...) scale_partial(aesthetic = "y", ...)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScalePartial <- ggproto(
  "ScalePartial", Scale,

  aesthetics = character(),
  params = list(),
  call = NULL,

  update_params = function(self, params, default = FALSE, call = self$call) {
    self$params <- defaults(params, self$params)
  },

  clone = function(self) {
    ggproto(NULL, self)
  },

  reset = function(self) NULL
)

resolve_partial <- function(scale) {
  if (!inherits(scale, "ScalePartial")) {
    return(scale)
  }
  if (is.null(scale$params$limits)) {
    return(NULL)
  }
  new <- limits(scale$params$limits, scale$aesthetics[1])
  new$update_params(scale$params, default = TRUE)
  new
}
