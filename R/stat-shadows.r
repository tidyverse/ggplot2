#' @rdname geom_shadows
#' @param shadows which shadows to draw. Accepts `"x"`, `"y"`, and `c("x", "y")`.
#' @param anchor where to draw the shadow. Accepts `list(x = 0, y = 0)`. 
#' @inheritParams stat_identity
#' @section Computed variables: 
#' \describe{
#'   \item{xmin}{lower limit of shadow along the x-axis}
#'   \item{xmax}{upper limit of shadow along the x-axis}
#'   \item{ymin}{lower limit of shadow along the y-axis}
#'   \item{ymax}{upper limit of shadow along the y-axis}
#' }
#' @export
stat_shadows <-  
  function(mapping = NULL, 
           data = NULL,
           geom = "shadows", 
           position = "identity",
           ...,
           # do I need to add the geom_shadows arguments here?
           shadows = list("x", "y"), 
           anchor = list(x = 0, y = 0),
           type = NULL,
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE) {
  layer(
    stat = StatShadows,  
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      # geom_shadows argument repeated here?
      shadows = shadows,
      anchor = anchor,  
      type = type,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatShadows <- 
  ggproto("StatShadows", Stat,

    required_aes = c("x", "y"), 

    # set up parameters, e.g. unpack from list
    setup_params = function(data, params) {
      params
    },

    # calculate shadows: returns data_frame with colnames: xmin, xmax, ymin, ymax 
    compute_group = function(data, scales, type = NULL, shadows = list("x", "y"), anchor = list(x = 0, y = 0), na.rm = TRUE) {
      # compute_group always takes (data, scales,...) 

      .compute_shadows(data = data, type = type, shadows = shadows, anchor = anchor)
      # the returned data will be available from the geom inside data
  }
)

# Calculate the shadows for each type / shadows / anchor
# Debugging: passing type, shadows, anchor directly?
# Do I need to pass params here?!
.compute_shadows <- function(data, type, shadows, anchor) {
  # How can I unpack the modified parameters here? setup_params(data, params)?
  # Warning: Ignoring unknown parameters: shadows, anchor
  # What do I need to do to access the params here?

  # initialize the dataframe to hold the computed statistics
  stats <- data_frame(xmin = numeric(), xmax = numeric(), ymin = numeric(), ymax = numeric())

  # compute default shadows (based on raw data)
  if (is.null(type)) {
    # compute shadows along the x-axis
    if (any(shadows == "x")) {
      stats <- rbind(stats, data_frame(
        xmin = min(data[, "x"]), 
        xmax = max(data[, "x"]), 
        ymin = params[["anchor"]][["y"]], 
        ymax = params[["anchor"]][["y"]]))  
    }
    # compute shadows along the y-axis
    if (any(shadows == "y")) {
      stats <- rbind(stats, data_frame(
        xmin = params[["anchor"]][["x"]], 
        xmax = params[["anchor"]][["x"]], 
        ymin = min(data[, "y"]), 
        ymax = max(data[, "y"]))) 
    } 
  }

  # compute shadows for 'type = quartiles'
  if (type == "quartile") {
    # compute shadows along the x-axis
    if (any(shadows == "x")) {
      stats <- rbind(stats, data_frame(
        xmin = quantile(data[, "x"], 0.25),
        xmax = quantile(data[, "x"], 0.75),
        ymin = quantile(data[, "y"], 0),
        ymax = quantile(data[, "y"], 0))) 
    }
    # compute shadows along the y-axis
    if (any(shadows == "y")) {
      stats <- rbind(stats, data_frame(
        xmin = quantile(data[, "x"], 0),
        xmax = quantile(data[, "x"], 0),
        ymin = quantile(data[, "y"], 0.25),
        ymax = quantile(data[, "y"], 0.75)))
    } 
  }
  # return the statistics
  row.names(stats) = NULL  # clean up 
  # the data looks like this:
  ##    xmin   xmax    ymin    ymax
  ## 1 16.275 28.025 1.51300 1.51300
  ## 2 10.400 10.400 2.49075 4.44625
  stats
}
