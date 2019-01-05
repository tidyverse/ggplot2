#' @rdname geom_shadow
#' @param type which type of shadow to draw. Accepts `type = "quartile"`, `type = "quantile"`, `type = "data"`. Also accepts a named list of values `type = list(x = c(0, 1), y = c(2, 3))`. 
#' @param anchor where to draw the shadow. Accepts any vector or list of values, e.g. `anchor = list(x = 0, y = 1)` or `anchor = c(0, 1)`. 
#' @param quantile which quantile shadow to draw. Accepts any vector or list of quantile values, e.g. `quantile = c(0.1, 0.9)`. 
#' @param variable which variable shadow to draw. Accepts `variable = "x"`, `variable = "y"`, or `variable = c("x", "y")`.
#' @inheritParams stat_identity
#' @section Computed variables: 
#' \describe{
#'   \item{xmin}{lower limit of shadow along the x-axis}
#'   \item{xmax}{upper limit of shadow along the x-axis}
#'   \item{ymin}{lower limit of shadow along the y-axis}
#'   \item{ymax}{upper limit of shadow along the y-axis}
#' } 
#' @export
stat_shadow <-  
  function(mapping = NULL, 
           data = NULL,
           geom = "shadow", 
           position = "identity",
           ...,
           type = NULL,
           anchor = list(x = 0, y = 0),
           quantile = NULL,
           variable = list("x", "y"), 
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatShadow,  
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      type = type,
      anchor = anchor,  
      quantile = quantile,
      variable = variable,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatShadow <- 
  ggproto("StatShadow", Stat,

    # required_aes are set in geom-shadow, do I need to repeat here?
    required_aes = c("x", "y"), 

    # do I need to repeat extra_params?
    extra_params = c("na.rm", "type", "anchor", "quantile", "variable"),

    # set up parameters, e.g. unpack from list
    setup_params = function(data, params) {      
      params 
    },

    # calculate shadow: returns data_frame with colnames: xmin, xmax, ymin, ymax 
    compute_group = function(data, scales, type = NULL, anchor = list(x = 0, y = 0), quantile = NULL, variable = list("x", "y"), na.rm = TRUE) {
    
    # compute_group always takes (data, scales,...) 

      .compute_shadow(data = data, type = type, anchor = anchor, quantile = quantile, variable = variable)
      # the returned data will be available from the geom inside data
  }
)

# Calculate the shadow for each type / anchor / quantile / variable
.compute_shadow <- function(data, type, anchor, quantile, variable) {

  # accept only recognized shadow types: 
    # NULL, c(double,double), "data", "quantile", "quartile"
  switch(!is.null(type) && !(is.numeric(type) && length(type) == 2) && (is.character(type) && !(type %in% c("data", "quantile", "quartile"))) || is.na(type), 
    message("Unrecognized shadow type"))
  
  if (!is.null(type) && type == "data" & !is.null(quantile)) {
    stop("`type = 'data'` is not compatible with argument `quantile`. If you want quantile shadows, set the `quantile` argument, e.g. `quantile = c(0.1, 0.9)` and omit the `type` argument or set it to `type = 'quantile'`. If you want shadows based on the range of the data, set `type = 'data'` and omit the `quantile` argument.", 
          call. = FALSE)
  }

  if (!is.null(type) && type == "quantile" && is.null(quantile)) {
    stop("If you set `type = 'quantile'`, you must also set the `quantile` argument, e.g. `quantile = c(0.1, 0.9)`. Make sure you are not mis-spelling `quantile` or `quartile`.", 
          call. = FALSE)
  }

  if (!is.null(type) && type == "quartile" && !is.null(quantile)) {
    stop("`type = 'quartile'` is not compatible with argument `quantile`. If you want quartile shadows, omit the `quantile` argument. Alternatively, you may set both `type = 'quantile'` and `quantile = c(0.25, 0.75)`. Make sure you are not mis-spelling `quantile` or `quartile`.", 
          call. = FALSE)
  }

  # compute default shadow (type = "data" or type = NULL) 
  if (is.null(type) && is.null(quantile) || type == "data" && is.null(quantile)) {
    # if type = "data", return (q0, q4), which computes min/max
    qs <- c(0, 1) 
  } else { # make sure type is non-NULL from here
  
    if (any(type == "quartile")) {
      # if type = quartile return (q1, q3)
      qs <- c(0.25, 0.75) 
    }

    if (is.numeric(quantile) && length(quantile) == 2) qs <- quantile 

    if (is.numeric(unlist(type)) & !(length(unlist(type)) %in% c(2, 4))) {
      stop("The optional argument `type` must be one of `'quartile'` or a numeric vector of length 2, e.g. `type = c(0.1, 0.9)`. The default `type = NULL` is equivalent to `type = c(0, 1) `. `type = 'quartile` is short-hand for `type = c(0.25, 0.75)`", 
          call. = FALSE)
    } 
  }

  # initialize values to NULL to let 'fail' gracefully 
  xmin = NULL; xmax = NULL; ymin = NULL; ymax = NULL;  
  shadows.x = NULL; shadows.y = NULL; 

  # if type is numeric of length 2 or 4, use supplied values to set shadow length
  if (is.double(type)) type <- as.list(type)  # in case a vector is passed
  if (is.numeric(unlist(type)) && length(unlist(type)) %in% c(2, 4)) { 
    xmin <- type[["x"]][[1]]
    xmax <- type[["x"]][[2]]
    ymin <- type[["y"]][[1]]
    ymax <- type[["y"]][[2]]
  }

  if (is.double(anchor) && length(anchor) == 2) {
    anchor <- setNames(as.list(anchor), c("x", "y")) 
  } else if (is.list(anchor) && length(anchor) == 2) {
    anchor <- setNames(anchor, c("x", "y")) 
  } else if (is.list(anchor) && length(anchor) == 1) {
    if (is.element("x", names(anchor))) { 
      anchor["y"] = 0 
    } else if (is.element("y", names(anchor))) {
      anchor["x"] = 0
    }
  } else { 
    stop("Argument anchor must be passed as a named list, e.g. `anchor = list(x = 0, y = 1)` or as a two-element vector, e.g. `anchor = c(0, 1)`. If no value is supplied, the anchor is set to zero, e.g. `anchor = list(y = 1)` is equivalent to `anchor = list(x = 0, y = 1)`.", call. = FALSE)
  }

  # compute shadow along the x-axis
  if (any(variable == "x")) {
    shadow.x <- c(
      xmin = xmin %||% as.numeric(stats::quantile(data[, "x"], qs[[1]])),
      xmax = xmax %||% as.numeric(stats::quantile(data[, "x"], qs[[2]])),
      ymin = anchor[["y"]], 
      ymax = anchor[["y"]]
    ) 
  }

  # compute shadow along the y-axis
  if (any(variable == "y")) {
    shadow.y <- c(
      xmin = anchor[["x"]], 
      xmax = anchor[["x"]], 
      ymin = ymin %||% as.numeric(stats::quantile(data[, "y"], qs[[1]])),
      ymax = ymax %||% as.numeric(stats::quantile(data[, "y"], qs[[2]]))
    ) 
  } 

  # prepare stats, including cases where only one variable is selected
  if (any(variable == "x") && any(variable == "y")) {
    stats <- c(x = shadow.x, y = shadow.y) 
  } else if (any(variable == "x")) {
    stats <- c(x = shadow.x) 
  } else {
    stats <- c(y = shadow.y) 
  }

  # store shadows in one data_frame
  stats <- new_data_frame(stats)

  # return the statistics
  stats
}

# end of stat-shadow.r