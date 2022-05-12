#' Quick plot
#'
#' `qplot()` is now deprecated in order to encourage the users to
#' learn [ggplot()] as it makes it easier to create complex graphics.
#'
#' @param x,y,... Aesthetics passed into each layer
#' @param data Data frame to use (optional).  If not specified, will create
#'   one, extracting vectors from the current environment.
#' @param facets faceting formula to use. Picks [facet_wrap()] or
#'   [facet_grid()] depending on whether the formula is one-
#'   or two-sided
#' @param margins See `facet_grid()`: display marginal facets?
#' @param geom Character vector specifying geom(s) to draw. Defaults to
#'  "point" if x and y are specified, and "histogram" if only x is specified.
#' @param stat,position `r lifecycle::badge("deprecated")`
#' @param xlim,ylim X and y axis limits
#' @param log Which variables to log transform ("x", "y", or "xy")
#' @param main,xlab,ylab Character vector (or expression) giving plot title,
#'   x axis label, and y axis label respectively.
#' @param asp The y/x aspect ratio
#' @export
qplot <- function(x, y, ..., data, facets = NULL, margins = FALSE,
                  geom = "auto", xlim = c(NA, NA),
                  ylim = c(NA, NA), log = "", main = NULL,
                  xlab = NULL, ylab = NULL,
                  asp = NA, stat = deprecated(), position = deprecated()) {

  lifecycle::deprecate_warn("3.4.0", "qplot()")

  caller_env <- parent.frame()

  if (!is.character(geom)) {
    abort("`geom` must be a character vector")
  }

  exprs <- enquos(x = x, y = y, ...)

  if (lifecycle::is_present(stat)) lifecycle::deprecate_stop("3.4.0", "qplot(stat)")
  if (lifecycle::is_present(position)) lifecycle::deprecate_stop("3.4.0", "qplot(position)")

  is_missing <- vapply(exprs, quo_is_missing, logical(1))
  # treat arguments as regular parameters if they are wrapped into I() or
  # if they don't have a name that is in the list of all aesthetics
  is_constant <- (!names(exprs) %in% ggplot_global$all_aesthetics) |
    vapply(exprs, quo_is_call, logical(1), name = "I")

  mapping <- new_aes(exprs[!is_missing & !is_constant], env = parent.frame())

  consts <- exprs[is_constant]

  aes_names <- names(mapping)
  mapping <- rename_aes(mapping)


  if (is.null(xlab)) {
    # Avoid <empty> label (#4170)
    if (quo_is_missing(exprs$x)) {
      xlab <- ""
    } else {
      xlab <- as_label(exprs$x)
    }
  }
  if (is.null(ylab)) {
    if (quo_is_missing(exprs$y)) {
      ylab <- ""
    } else {
      ylab <- as_label(exprs$y)
    }
  }

  if (missing(data)) {
    # If data not explicitly specified, will be pulled from workspace
    data <- new_data_frame()

    # Faceting variables must be in a data frame, so pull those out
    facetvars <- all.vars(facets)
    facetvars <- facetvars[facetvars != "."]
    names(facetvars) <- facetvars
    # FIXME?
    facetsdf <- as.data.frame(mget(facetvars, envir = caller_env))
    if (nrow(facetsdf)) data <- facetsdf
  }

  # Work out plot data, and modify aesthetics, if necessary
  if ("auto" %in% geom) {
    if ("sample" %in% aes_names) {
      geom[geom == "auto"] <- "qq"
    } else if (missing(y)) {
      x <- eval_tidy(mapping$x, data, caller_env)
      if (is.discrete(x)) {
        geom[geom == "auto"] <- "bar"
      } else {
        geom[geom == "auto"] <- "histogram"
      }
      if (is.null(ylab)) ylab <- "count"
    } else {
      if (missing(x)) {
        mapping$x <- quo(seq_along(!!mapping$y))
      }
      geom[geom == "auto"] <- "point"
    }
  }

  p <- ggplot(data, mapping, environment = caller_env)

  if (is.null(facets)) {
    p <- p + facet_null()
  } else if (is.formula(facets) && length(facets) == 2) {
    p <- p + facet_wrap(facets)
  } else {
    p <- p + facet_grid(facets = deparse(facets), margins = margins)
  }

  if (!is.null(main)) p <- p + ggtitle(main)

  # Add geoms/statistics
  for (g in geom) {
    # We reevaluate constants once per geom for historical reasons?
    params <- lapply(consts, eval_tidy)
    p <- p + do.call(paste0("geom_", g), params)
  }

  logv <- function(var) var %in% strsplit(log, "")[[1]]

  if (logv("x")) p <- p + scale_x_log10()
  if (logv("y")) p <- p + scale_y_log10()

  if (!is.na(asp)) p <- p + theme(aspect.ratio = asp)

  if (!missing(xlab)) p <- p + xlab(xlab)
  if (!missing(ylab)) p <- p + ylab(ylab)

  if (!missing(xlim) && !all(is.na(xlim))) p <- p + xlim(xlim)
  if (!missing(ylim) && !all(is.na(ylim))) p <- p + ylim(ylim)

  p
}

#' @export
#' @rdname qplot
quickplot <- qplot

is.constant <- function(x) {
  is_I_call <- function(x) is.call(x) && identical(x[[1]], quote(I))
  vapply(x, is_I_call, logical(1))
}
