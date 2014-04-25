bolus <- function(x) UseMethod("bolus")
bolus.proto <- function(x) x$bolus()

# Create a bolus object
# A bolus is a list suitable for digesting.
#
# Most ggplot objects have components that should be hashed when creating
# a digest (especially since most ggplot objects are proto objects and
# are also self-documenting).  The bolus methods ensure that only appropriate
# components are digested.
#
# @alias bolus
# @alias bolus.proto
# @alias digest.ggplot
# @alias digest.proto
# @keyword internal
#X hash_tests <- list(
#X   list(
#X     ggplot() + scale_x_continuous() + scale_y_continuous(),
#X     ggplot() + scale_y_continuous() + scale_x_continuous()
#X   ),
#X   list(
#X     qplot(mpg, wt, data=mtcars, na.rm = FALSE),
#X     ggplot(mtcars, aes(y=wt, x=mpg)) + geom_point()
#X   ),
#X   list(
#X     qplot(mpg, wt, data=mtcars, xlab = "blah"),
#X     qplot(mpg, wt, data=mtcars) + xlab("blah")
#X   )
#X )
#X
#X lapply(hash_tests, function(equal) {
#X   hashes <- lapply(equal, digest.ggplot)
#X
#X   if (length(unique(hashes)) != 1) {
#X     lapply(equal, function(x) print(str(bolus(x))))
#X     stop("Above plots not equal")
#X   }
#X })
bolus.ggplot <- function(x, ...) {
  sort.by.name <- function(x) {
    if (is.null(names(x))) return(x)
    x[order(names(x))]
  }

  with(x, list(
    data = digest::digest(data),
    mapping = sort.by.name(mapping),
    layers = sapply(layers, function(x) x$hash()),
    scales = digest(scales),
    facet = facet$hash(),
    coord = coordinates$hash(),
    theme = digest::digest(defaults(x$theme, theme_get()))
  ))
}

digest.proto <- function(x, ...) x$hash(, ...)
digest.ggplot <- function(x, ...) {
  if (is.null(x)) return()
  digest::digest(bolus(x), ...)
}

TopLevel$settings <- function(.) {
  mget(setdiff(ls(., all.names=TRUE), c(".that", ".super")), .)
}

Layer$hash <- TopLevel$hash <- function(., ...) {
  digest::digest(.$bolus(), ...)
}
TopLevel$bolus <- function(.) {
  list(
    name = .$objname,
    settings = .$settings()
  )
}

Layer$bolus <- function(.) {
  params <- c(.$geom_params, .$stat_params)
  params <- params[!duplicated(params)]
  if (!is.null(params) && length(params) > 1) params <- params[order(names(params))]

  mapping <- .$mapping
  if (!is.null(mapping)) mapping <- mapping[order(names(mapping))]

  list(
    geom = .$geom$objname,
    stat = .$stat$objname,
    pos  = .$position$objname,
    pos_parms  = .$position$settings(),
    data = .$data,
    mapping = mapping,
    params = params,
    legend = .$legend
  )
}

