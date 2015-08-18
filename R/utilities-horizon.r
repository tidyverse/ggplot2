# Horizontal layers
#
# A Geom is drawn vertically by convention, but the direction can be
# overridden with the `orient` parameter. When a Geom or a Stat is
# created with `orient = "h"`, the corresponding Layer's `flip` field
# is set to TRUE.
#
# The ggproto methods are called when the plot is being printed,
# through ggplot_build() and ggplot_gtable(). We only need to flip the
# labels at the most upstream function. Here is a scheme of ggplot's
# internals for reference:
#
# ggplot_build() triggers:
#
#   calculate_stats() (R/panel.r)
#     |-> Stat$compute_defaults()
#     |-> Layer$calc_statistic() -> Stat$compute() -> Stat$compute_group()
#
#   Note that calculate_stats() and Layer$calc_statistic() are soon to
#   be integrated into Layer$compute_statistic() so that:
#
#   Layer$compute_statistic()
#     |-> Stat$compute_defaults()
#     |-> Stat$compute_data()
#     |-> Stat$compute() -> Stat$compute_group()
#
#   Then we have:
#
#   Layer$map_statistic()
#   Layer$reparameterise() -> Geom$reparameterise()
#
#
# And ggplot_gtable() triggers the drawing methods:
#
#   Layer$make_grob() -> Geom$draw() -> Geom$draw_group()
#
#
# Sometimes it's not enough that the intermediate methods receive
# flipped data. This is why Geoms and Stats also get a `self$orient`
# param to help them do the right thing as a function of layer
# orientation.


# The following utilities are used to flip aesthetics when a Layer's
# orientation does not correspond to the Geom's natural
# orientation. They need to be evaluated before `Layer()` and `Stat()`
# because both use the method factory `flippable()`.

flip_aes <- function(x) {
  UseMethod("flip_aes")
}

flip_aes.character <- function(x) {
  lookup <- c(
    x = "y", xmin = "ymin", xmax = "ymax", xend = "yend",
    y = "x", ymin = "xmin", ymax = "xmax", yend = "xend"
  )
  flipped <- lookup[x]
  x[!is.na(flipped)] <- flipped[!is.na(flipped)]
  x
}

flip_aes.data.frame <- function(x) {
  names(x) <- flip_aes(names(x))
  x
}

flip_aes.list <- flip_aes.data.frame
flip_aes.uneval <- flip_aes.data.frame
flip_aes.default <- identity

flip_aes_if <- function(cond, x) {
  if (cond) {
    flip_aes(x)
  } else {
    x
  }
}

with_flipped_aes <- function(.x, .f, ...) {
  res <- .f(flip_aes(.x), ...)
  flip_aes(res)
}

flippable <- function(method) {
  function(self, data, ...) {
    if (self$flip) {
      with_flipped_aes(data, function(data, self, ...) {
        method(self, data, ...)
      }, self, ...)
    } else {
      method(self, data, ...)
    }
  }
}
