#' Add components to a plot
#'
#' `+` is the key to constructing sophisticated ggplot2 graphics. It
#' allows you to start simple, then get more and more complex, checking your
#' work at each step.
#'
#' @section What can you add?:
#' You can add any of the following types of objects:
#'
#'   - An [aes()] object replaces the default aesthetics.
#'   - A layer created by a `geom_` or `stat_` function adds a
#'     new layer.
#'   - A `scale` overrides the existing scale.
#'   - A [theme()] modifies the current theme.
#'   - A `coord` overrides the current coordinate system.
#'   - A `facet` specification overrides the current faceting.
#'
#' To replace the current default data frame, you must use `%+%`,
#' due to S3 method precedence issues.
#'
#' You can also supply a list, in which case each element of the list will
#' be added in turn.
#'
#' @param e1 An object of class [ggplot()] or a [theme()].
#' @param e2 A plot component, as described below.
#' @seealso [theme()]
#' @export
#' @method + gg
#' @rdname gg-add
#' @examples
#' base <-
#'  ggplot(mpg, aes(displ, hwy)) +
#'  geom_point()
#' base + geom_smooth()
#'
#' # To override the data, you must use %+%
#' base %+% subset(mpg, fl == "p")
#'
#' # Alternatively, you can add multiple components with a list.
#' # This can be useful to return from a function.
#' base + list(subset(mpg, fl == "p"), geom_smooth())
"+.gg" <- function(e1, e2) {
  if (missing(e2)) {
    cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
      "i" = "Did you accidentally put {.code +} on a new line?"
    ))
  }

  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))

  if      (is.theme(e1))  add_theme(e1, e2, e2name)
  else if (is.ggplot(e1)) add_ggplot(e1, e2, e2name)
  else if (is.ggproto(e1)) {
    cli::cli_abort(c(
      "Cannot add {.cls ggproto} objects together.",
      "i" = "Did you forget to add this object to a {.cls ggplot} object?"
    ))
  }
}


#' @rdname gg-add
#' @export
"%+%" <- `+.gg`

add_ggplot <- function(p, object, objectname) {
  if (is.null(object)) return(p)

  p <- plot_clone(p)
  p <- ggplot_add(object, p, objectname)
  set_last_plot(p)
  p
}
#' Add custom objects to ggplot
#'
#' This generic allows you to add your own methods for adding custom objects to
#' a ggplot with [+.gg].
#'
#' @param object An object to add to the plot
#' @param plot The ggplot object to add `object` to
#' @param object_name The name of the object to add
#'
#' @return A modified ggplot object
#' @details
#' Custom methods for `ggplot_add()` are intended to update the `plot` variable
#' using information from a custom `object`. This can become convenient when
#' writing extensions that don't build on the pre-existing grammar like
#' layers, facets, coords and themes. The `ggplot_add()` function is never
#' intended to be used directly, but it is triggered when an object is added
#' to a plot via the `+` operator. Please note that the full `plot` object is
#' exposed at this point, which comes with the responsibility of returning
#' the plot intact.
#'
#' @keywords internal
#' @export
#' @examples
#' # making a new method for the generic
#' # in this example, we apply a text element to the text theme setting
#' ggplot_add.element_text <- function(object, plot, object_name) {
#'   plot + theme(text = object)
#' }
#'
#' # we can now use `+` to add our object to a plot
#' ggplot(mpg, aes(displ, cty)) +
#'   geom_point() +
#'   element_text(colour = "red")
#'
#' # clean-up
#' rm(ggplot_add.element_text)
ggplot_add <- function(object, plot, object_name) {
  UseMethod("ggplot_add")
}
#' @export
ggplot_add.default <- function(object, plot, object_name) {
  cli::cli_abort("Can't add {.var {object_name}} to a {.cls ggplot} object.")
}
#' @export
ggplot_add.NULL <- function(object, plot, object_name) {
  plot
}
#' @export
ggplot_add.data.frame <- function(object, plot, object_name) {
  plot$data <- object
  plot
}
#' @export
ggplot_add.function <- function(object, plot, object_name) {
  cli::cli_abort(c(
          "Can't add {.var {object_name}} to a {.cls ggplot} object",
    "i" = "Did you forget to add parentheses, as in {.fn {object_name}}?"
  ))
}
#' @export
ggplot_add.theme <- function(object, plot, object_name) {
  plot$theme <- add_theme(plot$theme, object)
  plot
}
#' @export
ggplot_add.Scale <- function(object, plot, object_name) {
  plot$scales$add(object)
  plot
}
#' @export
ggplot_add.labels <- function(object, plot, object_name) {
  update_labels(plot, object)
}
#' @export
ggplot_add.Guides <- function(object, plot, object_name) {
  update_guides(plot, object)
}
#' @export
ggplot_add.uneval <- function(object, plot, object_name) {
  plot$mapping <- defaults(object, plot$mapping)
  # defaults() doesn't copy class, so copy it.
  class(plot$mapping) <- class(object)
  plot
}
#' @export
ggplot_add.Coord <- function(object, plot, object_name) {
  if (!isTRUE(plot$coordinates$default)) {
    cli::cli_inform("Coordinate system already present. Adding new coordinate system, which will replace the existing one.")
  }

  plot$coordinates <- object
  plot
}
#' @export
ggplot_add.Facet <- function(object, plot, object_name) {
  plot$facet <- object
  plot
}
#' @export
ggplot_add.list <- function(object, plot, object_name) {
  for (o in object) {
    plot <- ggplot_add(o, plot, object_name)
  }
  plot
}
#' @export
ggplot_add.by <- function(object, plot, object_name) {
  ggplot_add.list(object, plot, object_name)
}

#' @export
ggplot_add.Layer <- function(object, plot, object_name) {
  layers_names <- new_layer_names(object, names(plot$layers))
  plot$layers <- append(plot$layers, object)
  names(plot$layers) <- layers_names
  plot
}

new_layer_names <- function(layer, existing) {
  new_name <- layer$name
  if (is.null(new_name)) {
    # Construct a name from the layer's call
    new_name <- call_name(layer$constructor)

    if (new_name %in% existing) {
      names <- c(existing, new_name)
      names <- vec_as_names(names, repair = "unique", quiet = TRUE)
      new_name <- names[length(names)]
    }
  }

  names <- c(existing, new_name)
  vec_as_names(names, repair = "check_unique")
}
