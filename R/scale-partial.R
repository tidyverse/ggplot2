

scale_x <- function(...) scale_partial(aesthetic = "x", ...)
scale_y <- function(...) scale_partial(aesthetic = "y", ...)

scale_partial <- function(aesthetic, ..., call = caller_env()) {

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

  ggproto(
    NULL, ScalePartial,
    aesthetics = aesthetic,
    params = args
  )
}

ScalePartial <- ggproto(
  "ScalePartial", Scale,

  aesthetics = character(),
  params = list(),

  update_params = function(self, params, default = FALSE) {
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
