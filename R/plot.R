#' Create a new ggplot
#'
#' `ggplot()` initializes a ggplot object. It can be used to
#' declare the input data frame for a graphic and to specify the
#' set of plot aesthetics intended to be common throughout all
#' subsequent layers unless specifically overridden.
#'
#' `ggplot()` is used to construct the initial plot object,
#' and is almost always followed by a plus sign (`+`) to add
#' components to the plot.
#'
#' There are three common patterns used to invoke `ggplot()`:
#'
#' * `ggplot(data = df, mapping = aes(x, y, other aesthetics))`
#' * `ggplot(data = df)`
#' * `ggplot()`
#'
#' The first pattern is recommended if all layers use the same
#' data and the same set of aesthetics, although this method
#' can also be used when adding a layer using data from another
#' data frame.
#'
#' The second pattern specifies the default data frame to use
#' for the plot, but no aesthetics are defined up front. This
#' is useful when one data frame is used predominantly for the
#' plot, but the aesthetics vary from one layer to another.
#'
#' The third pattern initializes a skeleton `ggplot` object, which
#' is fleshed out as layers are added. This is useful when
#' multiple data frames are used to produce different layers, as
#' is often the case in complex graphics.
#'
#' The `data =` and `mapping =` specifications in the arguments are optional
#' (and are often omitted in practice), so long as the data and the mapping
#' values are passed into the function in the right order. In the examples
#' below, however, they are left in place for clarity.
#'
#' @param data Default dataset to use for plot. If not already a data.frame,
#'   will be converted to one by [fortify()]. If not specified,
#'   must be supplied in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   If not specified, must be supplied in each layer added to the plot.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment `r lifecycle::badge("deprecated")` Used prior to tidy
#'   evaluation.
#' @export
#' @seealso
#' The `r link_book("first steps chapter", "getting-started")`
#' @examples
#' # Create a data frame with some sample data, then create a data frame
#' # containing the mean value for each group in the sample data.
#' set.seed(1)
#'
#' sample_df <- data.frame(
#'   group = factor(rep(letters[1:3], each = 10)),
#'   value = rnorm(30)
#' )
#'
#' group_means_df <- setNames(
#'   aggregate(value ~ group, sample_df, mean),
#'   c("group", "group_mean")
#' )
#'
#' # The following three code blocks create the same graphic, each using one
#' # of the three patterns specified above. In each graphic, the sample data
#' # are plotted in the first layer and the group means data frame is used to
#' # plot larger red points on top of the sample data in the second layer.
#'
#' # Pattern 1
#' # Both the `data` and `mapping` arguments are passed into the `ggplot()`
#' # call. Those arguments are omitted in the first `geom_point()` layer
#' # because they get passed along from the `ggplot()` call. Note that the
#' # second `geom_point()` layer re-uses the `x = group` aesthetic through
#' # that mechanism but overrides the y-position aesthetic.
#' ggplot(data = sample_df, mapping = aes(x = group, y = value)) +
#'   geom_point() +
#'   geom_point(
#'     mapping = aes(y = group_mean), data = group_means_df,
#'     colour = 'red', size = 3
#'   )
#'
#' # Pattern 2
#' # Same plot as above, passing only the `data` argument into the `ggplot()`
#' # call. The `mapping` arguments are now required in each `geom_point()`
#' # layer because there is no `mapping` argument passed along from the
#' # `ggplot()` call.
#' ggplot(data = sample_df) +
#'   geom_point(mapping = aes(x = group, y = value)) +
#'   geom_point(
#'     mapping = aes(x = group, y = group_mean), data = group_means_df,
#'     colour = 'red', size = 3
#'   )
#'
#' # Pattern 3
#' # Same plot as above, passing neither the `data` or `mapping` arguments
#' # into the `ggplot()` call. Both those arguments are now required in
#' # each `geom_point()` layer. This pattern can be particularly useful when
#' # creating more complex graphics with many layers using data from multiple
#' # data frames.
#' ggplot() +
#'   geom_point(mapping = aes(x = group, y = value), data = sample_df) +
#'   geom_point(
#'     mapping = aes(x = group, y = group_mean), data = group_means_df,
#'     colour = 'red', size = 3
#'   )
ggplot <- function(
  data = NULL,
  mapping = aes(),
  ...,
  environment = parent.frame()
) {
  UseMethod("ggplot")
}

#' @export
ggplot.default <-
  function(data, mapping = aes(), ..., environment = parent.frame()) {
    if (!missing(mapping)) {
      mapping <- validate_mapping(mapping)
    }
    if (missing(data)) {
      data <- NULL
    }

    data <- fortify(data, ...)

    p <- class_ggplot(
      data = data,
      mapping = mapping,
      plot_env = environment
    )
    class(p) <- union(union(c("ggplot2::ggplot", "ggplot"), class(p)), "gg")

    set_last_plot(p)
    p
  }

#' @export
ggplot.function <- function(data, ...) {
  # Added to avoid functions end in ggplot.default
  cli::cli_abort(c(
    "{.arg data} cannot be a function.",
    "i" = "Have you misspelled the {.arg data} argument in {.fn ggplot}?"
  ))
}

plot_clone <- function(plot) {
  p <- plot
  p@scales <- plot@scales$clone()
  p
}

#' Reports wether `x` is a type of object
#' @param x An object to test
#' @keywords internal
#' @export
#' @name is_tests
is_ggplot <- function(x) S7::S7_inherits(x, class_ggplot)

#' @export
#' @rdname is_tests
#' @usage is.ggplot(x) # Deprecated
is.ggplot <- function(x) {
  deprecate_soft0("3.5.2", "is.ggplot()", "is_ggplot()")
  is_ggplot(x)
}

#' Explicitly draw plot
#'
#' Generally, you do not need to print or plot a ggplot2 plot explicitly: the
#' default top-level print method will do it for you. You will, however, need
#' to call `print()` explicitly if you want to draw a plot inside a
#' function or for loop.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#' @keywords hplot
#' @return Invisibly returns the original plot.
#' @name print.ggplot
#' @aliases plot.ggplot
#' @examples
#' colours <- c("class", "drv", "fl")
#'
#' # Doesn't seem to do anything!
#' for (colour in colours) {
#'   ggplot(mpg, aes(displ, hwy, colour = .data[[colour]])) +
#'     geom_point()
#' }
#'
#' for (colour in colours) {
#'   print(ggplot(mpg, aes(displ, hwy, colour = .data[[colour]])) +
#'           geom_point())
#' }
local({
  S7::method(print, class_ggplot) <- S7::method(plot, class_ggplot) <-
    function(x, newpage = is.null(vp), vp = NULL, ...) {
      set_last_plot(x)
      if (newpage) {
        grid.newpage()
      }

      # Record dependency on 'ggplot2' on the display list
      # (AFTER grid.newpage())
      grDevices::recordGraphics(
        requireNamespace("ggplot2", quietly = TRUE),
        list(),
        getNamespace("ggplot2")
      )

      data <- ggplot_build(x)

      gtable <- ggplot_gtable(data)
      if (is.null(vp)) {
        grid.draw(gtable)
      } else {
        if (is.character(vp)) {
          seekViewport(vp)
        } else {
          pushViewport(vp)
        }
        grid.draw(gtable)
        upViewport()
      }

      if (isTRUE(getOption("BrailleR.VI")) && rlang::is_installed("BrailleR")) {
        print(asNamespace("BrailleR")$VI(x))
      }

      invisible(x)
    }
})

# The following extractors and subassignment operators are for a smooth
# transition and should be deprecated in the release cycle after 4.0.0
local({
  S7::method(`[[`, class_gg) <- S7::method(`$`, class_gg) <-
    function(x, i) {
      if (!S7::prop_exists(x, i) && S7::prop_exists(x, "meta")) {
        # This is a trick to bridge a gap between S3 and S7. We're allowing
        # for arbitrary fields by reading/writing to the 'meta' field when the
        # index does not point to an actual property.
        # The proper way to go about this is to implement new fields as properties
        # of a ggplot subclass.
        S7::prop(x, "meta")[[i]]
      } else {
        `[[`(S7::props(x), i)
      }
    }
  S7::method(`[`, class_gg) <- function(x, i) {
    `[`(S7::props(x), i)
  }
})

#' @export
`[<-.ggplot2::gg` <- function(x, i, value) {
  S7::props(x) <- `[<-`(S7::props(x), i, value)
  x
}

#' @export
`$<-.ggplot2::gg` <- function(x, i, value) {
  if (!S7::prop_exists(x, i) && S7::prop_exists(x, "meta")) {
    # See explanation in accessor
    S7::prop(x, "meta")[[i]] <- value
  } else {
    S7::props(x) <- `[[<-`(S7::props(x), i, value)
  }
  x
}

#' @export
`[[<-.ggplot2::gg` <- `$<-.ggplot2::gg`

#' @importFrom S7 convert
# S7 currently attaches the S3 method to the calling environment which gives `ggplot2:::as.list`
# Wrap in `local()` to provide a temp environment which throws away the attachment
local({
  S7::method(convert, list(from = class_ggplot, to = S7::class_list)) <-
    function(from, to) {
      S7::props(from)
    }

  S7::method(as.list, class_ggplot) <-
    function(x, ...) convert(x, S7::class_list)
})
