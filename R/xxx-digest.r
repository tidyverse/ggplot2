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
#X     qplot(mpg, wt, data=mtcars),
#X     ggplot(mtcars, aes(y=wt, x=mpg)) + geom_point()
#X   ),
#X   list(
#X     qplot(mpg, wt, data=mtcars, xlab = "blah"),
#X     qplot(mpg, wt, data=mtcars) + scale_x_continuous("blah")
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
  with(x, list(
    data = digest(data),
    mapping = defaults[order(names(defaults))],
    layers = sapply(layers, function(x) x$hash()),
    scales = scales$hash(),
    facet = facet$hash(),
    coord = coordinates$hash(),
    title = title,
    options = x[intersect(names(x), names(ggopt()))]
  ))
}

digest.proto <- function(x, ...) x$hash(, ...)
digest.ggplot <- function(x, ...) {
  try_require("digest")
  digest(bolus(x), ...)
}

TopLevel$settings <- function(.) {
  mget(setdiff(ls(., all.names=TRUE), c(".that", ".super")), .)
}

Layer$hash <- TopLevel$hash <- function(., ...) {
  digest(.$bolus(), ...)
}
Scales$hash <- function(.) {
  sc <- sapply(.$.scales, function(x) x$hash())
  sc[order(sc)]
}

Scales$bolus <- function(.) {
  lapply(.$.scales, function(x) x$bolus())
}
TopLevel$bolus <- function(.) {
  list(
    name = .$objname,
    settings = .$settings()
  )
}
Scale$bolus <- function(.) {
  list(
    name = .$objname,
    input = .$.input,
    output = .$.output,
    settings = .$settings()
  )
}
Layer$bolus <- function(.) {
  list(
    geom = .$geom$objname,
    stat = .$stat$objname,
    pos  = .$position$objname,
    pos_parms  = .$position$settings(),
    data = .$data,
    mapping = .$aesthetics,
    params = unique(c(.$geom_params, .$stat_params))
  )
}

