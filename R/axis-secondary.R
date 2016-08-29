#' @export
sec_axis <- function(trans = NULL, name = waiver(), breaks = waiver(), labels = waiver()) {
  if (!is.formula(trans)) stop("transformation for secondary axes must be a formula", call. = FALSE)
  ggproto(NULL, AxisSecondary,
    trans = trans,
    name = name,
    breaks = breaks,
    labels = labels
  )
}
#' @export
dup_axis <- function(trans = ~., name = derive(), breaks = derive(), labels = derive()) {
  sec_axis(trans, name, breaks, labels)
}
is.sec_axis <- function(x) {
  inherits(x, "AxisSecondary")
}
#' @export
derive <- function() {
  structure(list(), class = "derived")
}
is.derived <- function(x) {
  inherits(x, "derived")
}
#' @importFrom lazyeval f_eval
AxisSecondary <- ggproto("AxisSecondary", NULL,
  trans = NULL,
  axis = NULL,
  name = waiver(),
  breaks = waiver(),
  labels = waiver(),
  detail = 1000,
  empty = function(self) {
    is.null(self$trans)
  },
  init = function(self, scale) {
    if (self$empty()) return()
    if (!is.formula(self$trans)) stop("transformation for secondary axes must be a formula", call. = FALSE)
    if (is.derived(self$name)) self$name <- scale$name
    if (is.derived(self$breaks)) self$breaks <- scale$breaks
    if (is.derived(self$labels)) self$labels <- scale$labels
  },
  transform_range = function(self, range) {
    range <- structure(data.frame(range), names = '.')
    f_eval(self$trans, range)
  },
  break_info = function(self, range, scale) {
    if (self$empty()) return()
    inv_range <- scale$trans$inverse(range)
    old_range <- seq(inv_range[1], inv_range[2], length.out = self$detail)
    full_range <- self$transform_range(old_range)
    if (length(unique(sign(diff(full_range)))) != 1) stop("transformation for secondary axes must be monotonous")
    new_range <- full_range[c(1, self$detail)]
    temp_scale <- self$create_scale(new_range)
    range_info <- temp_scale$break_info()
    old_val <- lapply(range_info$major_source, function(x) which.min(abs(full_range - x)))
    old_val <- old_range[unlist(old_val)]
    old_val_trans <- scale$trans$transform(old_val)
    range_info$major[] <- round(rescale(scale$map(old_val_trans, range(old_val_trans)), from = range), digits = 3)
    names(range_info) <- paste0("sec.", names(range_info))
    range_info
  },
  create_scale = function(self, range) {
    scale <- ggproto(NULL, ScaleContinuousPosition,
      name = self$name,
      breaks = self$breaks,
      labels = self$labels,
      limits = range,
      expand = c(0, 0),
      trans = identity_trans()
    )
    scale$train(range)
    scale
  }
)
