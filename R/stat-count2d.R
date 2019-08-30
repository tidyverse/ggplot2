#' @export
stat_count_2d <- function(mapping = NULL, data = NULL,
                          geom = "point", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  params <- list(
    na.rm = na.rm,
    ...
  )

  layer(
    data = data,
    mapping = mapping,
    stat = StatCount2D,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

StatCount2D <- ggproto("StatCount2D", Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",

  compute_group = function(data, scales, na.rm = FALSE) {
    data$weight <- data$weight %||% rep(1, nrow(data))
    counts <- dapply(data, c("x", "y"), function(d) {
      new_data_frame(list(
        count = sum(d$weight, na.rm = TRUE),
        x = d$x[1],
        y = d$y[1]
      ), n = 1)
    })
    counts$count[is.na(counts$count)] <- 0
    counts$prop <- counts$count / max(abs(counts$count))
    counts
  },

  compute_panel = function(self, data, scales, na.rm = FALSE) {
    data <- ggproto_parent(Stat, self)$compute_panel(data, scales, na.rm = na.rm)
    data <- data[order(data$count, decreasing = TRUE), , drop = FALSE]
    data
  },

  default_aes = aes(size = stat(count))
)
