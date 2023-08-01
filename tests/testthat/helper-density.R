
# In R devel from 4.3.0 onwards, the density calculation has slightly changed,
# which affects visual snapshots that use a density calculation, like
# `geom_violin()` and `geom_density()`.
# See https://developer.r-project.org/blosxom.cgi/R-devel/NEWS/2023/05/03#n2023-05-03
#
# It has a backwards compatibility argument called 'old.coords' that can be used
# to perform the classic density calculation, which means we can stably use
# visual tests in R devel.
#
# Since that argument is not available in older versions, we have to use the
# following workaround. Here, we conditionally override the default
# density method to use `old.coords = TRUE`.
if ("old.coords" %in% names(formals(stats::density.default))) {
  registerS3method(
    "density", "default",
    function(..., old.coords = TRUE) {
      stats::density.default(..., old.coords = old.coords)
    }
  )
}
