

scale_x <- function(...) scale_partial("x", ...)
scale_y <- function(...) scale_partial("y", ...)

scale_partial <- function(aesthetic, ...) {

  aesthetic <- standardise_aes_names(aesthetic)

  args <- list2(...)
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
  }

)
