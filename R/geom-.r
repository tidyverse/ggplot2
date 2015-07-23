#' @include legend-draw.r

Geom <- ggproto("Geom", TopLevel,
  type = "geom",

  required_aes = c(),

  default_aes = aes(),

  draw_key = draw_key_point,

  draw = function(...) {},

  draw_groups = function(self, data, scales, coordinates, ...) {
    if (empty(data)) return(zeroGrob())

    groups <- split(data, factor(data$group))
    grobs <- lapply(groups, function(group) self$draw(group, scales, coordinates, ...))

    # String like "bar" or "line"
    objname <- sub("^geom_", "", self$my_name())

    ggname(paste0(objname, "s"), gTree(
      children = do.call("gList", grobs)
    ))
  },

  reparameterise = function(data, params) data
)

# make_geom("point") returns GeomPoint
make_geom <- function(class) {
  name <- paste0("Geom", camelize(class, first = TRUE))
  if (!exists(name)) {
    stop("No geom called ", name, ".", call. = FALSE)
  }

  obj <- get(name)
  if (!inherits(obj, "Geom")) {
    stop("Found object is not a geom.", call. = FALSE)
  }

  obj
}
