Geom <- proto2(
  class = "Geom",
  inherit = TopLevel,
  members = list(
    class = function(self) "geom",

    parameters = function(self) {
      # proto2 TODO: better way of getting formals for self$draw
      params <- formals(environment(self$draw)$res)
      params <- params[setdiff(names(params), c("self", "super", "data", "scales", "coordinates", "..."))]

      required <- rep(NA, length(self$required_aes))
      names(required) <- self$required_aes
      aesthetics <- c(self$default_aes(), required)

      c(params, aesthetics[setdiff(names(aesthetics), names(params))])
    },

    required_aes = c(),

    default_aes = function(self) aes(),

    default_pos = function(self) PositionIdentity,

    guide_geom = function(self) "point",

    draw = function(self, ...) {},

    draw_groups = function(self, data, scales, coordinates, ...) {
      if (empty(data)) return(zeroGrob())

      groups <- split(data, factor(data$group))
      grobs <- lapply(groups, function(group) self$draw(group, scales, coordinates, ...))

      ggname(paste(self$objname, "s", sep=""), gTree(
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
)
