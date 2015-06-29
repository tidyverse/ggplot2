Geom <- proto2(
  inherit = TopLevel,
  members = list(
    class = function() "geom",

    parameters = function() {
      params <- formals(self$draw)
      params <- params[setdiff(names(params), c("data","scales", "coordinates", "..."))]

      required <- rep(NA, length(self$required_aes))
      names(required) <- self$required_aes
      aesthetics <- c(self$default_aes(), required)

      c(params, aesthetics[setdiff(names(aesthetics), names(params))])
    },

    required_aes = c(),

    default_aes = function() aes(),

    default_pos = function() PositionIdentity,

    guide_geom = function() "point",

    draw = function(...) {},

    draw_groups = function(data, scales, coordinates, ...) {
      if (empty(data)) return(zeroGrob())

      groups <- split(data, factor(data$group))
      grobs <- lapply(groups, function(group) self$draw(group, scales, coordinates, ...))

      ggname(paste(self$objname, "s", sep=""), gTree(
        children = do.call("gList", grobs)
      ))
    },

    print = function(newline=TRUE) {
      cat("geom_", self$objname, ": ", sep="") #  , clist(self$parameters())
      if (newline) cat("\n")
    },

    reparameterise = function(data, params) data

    # Html documentation ----------------------------------
  )
)
