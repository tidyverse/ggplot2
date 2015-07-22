Geom <- proto2("Geom", TopLevel,
  class = function(self) "geom",

  parameters = function(self) {
    # proto2 TODO: better way of getting formals for self$draw
    params <- formals(environment(self$draw)$res)
    params <- params[setdiff(names(params), c("self", "super", "data", "scales", "coordinates", "..."))]

    required <- rep(NA, length(self$required_aes))
    names(required) <- self$required_aes
    aesthetics <- c(self$default_aes, required)

    c(params, aesthetics[setdiff(names(aesthetics), names(params))])
  },

  required_aes = c(),

  default_aes = aes(),

  guide_geom = function(self) "point",

  draw = function(self, ...) {},

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

#     print = function(self, newline=TRUE) {
#       cat("geom_", self$objname, ": ", sep="") #  , clist(self$parameters())
#       if (newline) cat("\n")
#     },

  reparameterise = function(self, data, params) data

  # Html documentation ----------------------------------
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
