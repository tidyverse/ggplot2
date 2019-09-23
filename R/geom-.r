#' @include legend-draw.r
NULL

#' @section Geoms:
#'
#' All `geom_*` functions (like `geom_point`) return a layer that
#' contains a `Geom*` object (like `GeomPoint`). The `Geom*`
#' object is responsible for rendering the data in the plot.
#'
#' Each of the `Geom*` objects is a [ggproto()] object, descended
#' from the top-level `Geom`, and each implements various methods and
#' fields.
#'
#' Compared to `Stat` and `Position`, `Geom` is a little
#' different because the execution of the setup and compute functions is
#' split up. `setup_data` runs before position adjustments, and
#' `draw_layer` is not run until render time, much later. This
#' means there is no `setup_params` because it's hard to communicate
#' the changes.
#'
#' To create a new type of Geom object, you typically will want to
#' override one or more of the following:
#'
#'   - Either `draw_panel(self, data, panel_params, coord)` or
#'     `draw_group(self, data, panel_params, coord)`. `draw_panel` is
#'     called once per panel, `draw_group` is called once per group.
#'
#'     Use `draw_panel` if each row in the data represents a
#'     single element. Use `draw_group` if each group represents
#'     an element (e.g. a smooth, a violin).
#'
#'     `data` is a data frame of scaled aesthetics.
#'
#'     `panel_params` is a set of per-panel parameters for the
#'     `coord`. Generally, you should consider `panel_params`
#'     to be an opaque data structure that you pass along whenever you call
#'     a coord method.
#'
#'     You must always call `coord$transform(data, panel_params)` to
#'     get the (position) scaled data for plotting. To work with
#'     non-linear coordinate systems, you typically need to convert into a
#'     primitive geom (e.g. point, path or polygon), and then pass on to the
#'     corresponding draw method for munching.
#'
#'     Must return a grob. Use [zeroGrob()] if there's nothing to
#'     draw.
#'   - `draw_key`: Renders a single legend key.
#'   - `required_aes`: A character vector of aesthetics needed to
#'     render the geom.
#'   - `default_aes`: A list (generated by [aes()] of
#'     default values for aesthetics.
#'   - `setup_data`: Converts width and height to xmin and xmax,
#'     and ymin and ymax values. It can potentially set other values as well.
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Geom <- ggproto("Geom",
  required_aes = character(),
  non_missing_aes = character(),
  optional_aes = character(),

  default_aes = aes(),

  draw_key = draw_key_point,

  handle_na = function(self, data, params) {
    remove_missing(data, params$na.rm,
      c(self$required_aes, self$non_missing_aes),
      snake_class(self)
    )
  },

  draw_layer = function(self, data, params, layout, coord) {
    if (empty(data)) {
      n <- if (is.factor(data$PANEL)) nlevels(data$PANEL) else 1L
      return(rep(list(zeroGrob()), n))
    }

    # Trim off extra parameters
    params <- params[intersect(names(params), self$parameters())]

    args <- c(list(quote(data), quote(panel_params), quote(coord)), params)
    lapply(split(data, data$PANEL), function(data) {
      if (empty(data)) return(zeroGrob())

      panel_params <- layout$panel_params[[data$PANEL[1]]]
      do.call(self$draw_panel, args)
    })
  },

  draw_panel = function(self, data, panel_params, coord, ...) {
    groups <- split(data, factor(data$group))
    grobs <- lapply(groups, function(group) {
      self$draw_group(group, panel_params, coord, ...)
    })

    ggname(snake_class(self), gTree(
      children = do.call("gList", grobs)
    ))
  },

  draw_group = function(self, data, panel_params, coord) {
    stop("Not implemented")
  },

  setup_params = function(data, params) params,

  setup_data = function(data, params) data,

  # Combine data with defaults and set aesthetics from parameters
  use_defaults = function(self, data, params = list(), modifiers = aes()) {
    # Fill in missing aesthetics with their defaults
    missing_aes <- setdiff(names(self$default_aes), names(data))

    missing_eval <- lapply(self$default_aes[missing_aes], eval_tidy)
    # Needed for geoms with defaults set to NULL (e.g. GeomSf)
    missing_eval <- compact(missing_eval)

    if (empty(data)) {
      data <- as_gg_data_frame(missing_eval)
    } else {
      data[names(missing_eval)] <- missing_eval
    }

    if (length(modifiers) != 0) {
      env <- new.env(parent = baseenv())
      env$mapped <- mapped
      modified_aes <- new_data_frame(
        lapply(substitute_aes(modifiers),  eval_tidy, data, env)
      )
      names(modified_aes) <- rename_aes(names(modified_aes))

      # Check that all output are valid data
      nondata_modified <- check_nondata_cols(modified_aes)
      if (length(nondata_modified) > 0) {
        msg <- paste0(
          "Modifiers must return valid values. Problematic aesthetic(s): ",
          paste0(vapply(nondata_modified, function(x) {paste0(x, " = ", as_label(modifiers[[x]]))}, character(1)), collapse = ", "),
          ". \nDid you map your mod in the wrong layer?"
        )
        stop(msg, call. = FALSE)
      }

      names(modified_aes) <- names(modifiers)

      data <- cunion(modified_aes, data)
    }

    # Override mappings with params
    aes_params <- intersect(self$aesthetics(), names(params))
    check_aesthetics(params[aes_params], nrow(data))
    data[aes_params] <- params[aes_params]
    data
  },

  # Most parameters for the geom are taken automatically from draw_panel() or
  # draw_groups(). However, some additional parameters may be needed
  # for setup_data() or handle_na(). These can not be imputed automatically,
  # so the slightly hacky "extra_params" field is used instead. By
  # default it contains `na.rm`
  extra_params = c("na.rm"),

  parameters = function(self, extra = FALSE) {
    # Look first in draw_panel. If it contains ... then look in draw groups
    panel_args <- names(ggproto_formals(self$draw_panel))
    group_args <- names(ggproto_formals(self$draw_group))
    args <- if ("..." %in% panel_args) group_args else panel_args

    # Remove arguments of defaults
    args <- setdiff(args, names(ggproto_formals(Geom$draw_group)))

    if (extra) {
      args <- union(args, self$extra_params)
    }
    args
  },

  aesthetics = function(self) {
    c(union(self$required_aes, names(self$default_aes)), self$optional_aes, "group")
  }

)


#' Graphical units
#'
#' Multiply size in mm by these constants in order to convert to the units
#' that grid uses internally for `lwd` and `fontsize`.
#'
#' @name graphical-units
#' @keywords internal
#' @aliases NULL
NULL

#' @export
#' @rdname graphical-units
.pt <- 72.27 / 25.4
#' @export
#' @rdname graphical-units
.stroke <- 96 / 25.4

check_aesthetics <- function(x, n) {
  ns <- vapply(x, length, numeric(1))
  good <- ns == 1L | ns == n

  if (all(good)) {
    return()
  }

  stop(
    "Aesthetics must be either length 1 or the same as the data (", n, "): ",
    paste(names(which(!good)), collapse = ", "),
    call. = FALSE
  )
}
