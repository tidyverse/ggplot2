stat_manual <- function(
    mapping = NULL,
    data = NULL,
    geom = "point",
    position = "identity",
    ...,
    fun = identity,
    args = list(),
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatManual,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      fun = fun,
      args = args,
      ...
    )
  )
}

StatManual <- ggproto(
  "StatManual", Stat,

  setup_params = function(data, params) {
    params$fun <- allow_lambda(params$fun)
    check_function(params$fun, arg = "fun")
    params
  },

  compute_group = function(data, scales, fun = identity, args = list()) {
    as_gg_data_frame(inject(fun(data, !!!args)))
  }
)
