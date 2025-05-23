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
add_gg <- function(e1, e2) {
  if (missing(e2)) {
    cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
      "i" = "Did you accidentally put {.code +} on a new line?"
    ))
  }

  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))

  if      (is_theme(e1))  add_theme(e1, e2, e2name)
  # The `add_ggplot()` branch here is for backward compatibility with R < 4.3.0
  else if (is_ggplot(e1)) add_ggplot(e1, e2, e2name)
  else if (is_ggproto(e1)) {
    cli::cli_abort(c(
      "Cannot add {.cls ggproto} objects together.",
      "i" = "Did you forget to add this object to a {.cls ggplot} object?"
    ))
  }
}

if (getRversion() < "4.3.0") {
  S7::method(`+`, list(class_S3_gg, S7::class_any)) <- add_gg
}

S7::method(`+`, list(class_ggplot, S7::class_any)) <- function(e1, e2) {
  e2name <- deparse(substitute(e2, env = caller_env(2)))
  add_ggplot(e1, e2, e2name)
}

S7::method(`+`, list(class_theme, S7::class_any)) <- function(e1, e2) {
  e2name <- deparse(substitute(e2, env = caller_env(2)))
  add_theme(e1, e2, e2name)
}


#' @rdname gg-add
#' @export
"%+%" <- function(e1, e2) {
  if (getRversion() < "4.3.0") {
    add_gg(e1, e2)
  } else {
    deprecate_soft0("4.0.0", I("<ggplot> %+% x"), I("<ggplot> + x"))
    `+`(e1, e2)
  }
}

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
#' a ggplot with [+.gg][add_gg]. The `ggplot_add()` function is vestigial and
#' the `update_ggplot()` function should be used instead.
#'
#' @param object An object to add to the plot
#' @param plot The ggplot object to add `object` to
#'
#' @return A modified ggplot object
#' @details
#' Custom methods for `update_ggplot()` are intended to update the `plot` variable
#' using information from a custom `object`. This can become convenient when
#' writing extensions that don't build on the pre-existing grammar like
#' layers, facets, coords and themes. The `update_ggplot()` function is never
#' intended to be used directly, but it is triggered when an object is added
#' to a plot via the `+` operator. Please note that the full `plot` object is
#' exposed at this point, which comes with the responsibility of returning
#' the plot intact.
#'
#' @keywords internal
#' @export
#' @examples
#' # making a new method for the generic
#' # in this example, we enable adding text elements
#' S7::method(update_ggplot, list(element_text, class_ggplot)) <-
#'   function(object, plot, ...) {
#'     plot + theme(text = object)
#'   }
#'
#' # we can now use `+` to add our object to a plot
#' ggplot(mpg, aes(displ, cty)) +
#'   geom_point() +
#'   element_text(colour = "red")
#'
#' # clean-up
update_ggplot <- S7::new_generic("update_ggplot", c("object", "plot"))

S7::method(update_ggplot, list(S7::class_any, class_ggplot)) <-
  function(object, plot, object_name, ...) {
    cli::cli_abort("Can't add {.var {object_name}} to a {.cls ggplot} object.")
  }

S7::method(update_ggplot, list(S7::class_function, class_ggplot)) <-
  function(object, plot, object_name, ...) {
    cli::cli_abort(c(
      "Can't add {.var {object_name}} to a {.cls ggplot} object",
      "i" = "Did you forget to add parentheses, as in {.fn {object_name}}?"
    ))
  }

S7::method(update_ggplot, list(NULL, class_ggplot)) <-
  function(object, plot, ...) { plot }

S7::method(update_ggplot, list(S7::class_data.frame, class_ggplot)) <-
  function(object, plot, ...) { S7::set_props(plot, data = object) }

S7::method(update_ggplot, list(class_scale, class_ggplot)) <-
  function(object, plot, ...) {
    plot@scales$add(object)
    plot
  }

S7::method(update_ggplot, list(class_labels, class_ggplot)) <-
  function(object, plot, ...) { update_labels(plot, object) }

S7::method(update_ggplot, list(class_guides, class_ggplot)) <-
  function(object, plot, ...) {
    old <- plot@guides
    new <- ggproto(NULL, old)
    new$add(object)
    plot@guides <- new
    plot
  }

S7::method(update_ggplot, list(class_mapping, class_ggplot)) <-
  function(object, plot, ...) {
    S7::set_props(plot, mapping = class_mapping(defaults(object, plot@mapping)))
  }

S7::method(update_ggplot, list(class_theme, class_ggplot)) <-
  function(object, plot, ...) {
    S7::set_props(plot, theme = add_theme(plot@theme, object))
  }

S7::method(update_ggplot, list(class_coord, class_ggplot)) <-
  function(object, plot, ...) {
    if (!isTRUE(plot@coordinates$default)) {
      cli::cli_inform(c(
        "Coordinate system already present.",
        i = "Adding new coordinate system, which will replace the existing one."
      ))
    }
    S7::set_props(plot, coordinates = object)
  }

S7::method(update_ggplot, list(class_facet, class_ggplot)) <-
  function(object, plot, ...) { S7::set_props(plot, facet = object) }

S7::method(update_ggplot, list(class_layer, class_ggplot)) <-
  function(object, plot, ...) {
    layers_names <- new_layer_names(object, names2(plot@layers))
    object <- setNames(append(plot@layers, object), layers_names)
    S7::set_props(plot, layers = object)
  }

S7::method(update_ggplot, list(S7::class_list, class_ggplot)) <-
  function(object, plot, object_name, ...) {
    for (o in object) {
      plot <- ggplot_add(o, plot, object_name)
    }
    plot
  }

S7::method(update_ggplot, list(S7::new_S3_class("by"), class_ggplot)) <-
  function(object, plot, object_name, ...) {
    ggplot_add(unclass(object), plot, object_name)
  }

# For backward compatibility, ggplot_add still exists but by default it wraps
# `update_ggplot()`
#' @rdname update_ggplot
#' @export
ggplot_add <- function(object, plot, ...) {
  UseMethod("ggplot_add")
}

#' @export
ggplot_add.default <- function(object, plot, ...) {
  update_ggplot(object = object, plot = plot, ...)
}

new_layer_names <- function(layer, existing) {

  empty <- !nzchar(existing)
  if (any(empty)) {
    existing[empty] <- "unknown"
    existing <- vec_as_names(existing, repair = "unique", quiet = TRUE)
  }

  new_name <- layer$name
  if (is.null(new_name)) {
    # Construct a name from the layer's call
    new_name <- call_name(layer$constructor) %||% snake_class(layer$geom)

    if (new_name %in% existing) {
      names <- c(existing, new_name)
      names <- vec_as_names(names, repair = "unique", quiet = TRUE)
      new_name <- names[length(names)]
    }
  }

  names <- c(existing, new_name)
  vec_as_names(names, repair = "check_unique")
}
