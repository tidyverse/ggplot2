
enable_vdiffr <- TRUE

if (!requireNamespace("vdiffr", quietly = TRUE) ||
      utils::packageVersion("vdiffr") < "0.2.3.9000") {
  enable_vdiffr <- FALSE
}

expect_doppelganger <- function(title, fig,
                               path = NULL,
                               ...,
                               user_fonts = NULL,
                               verbose = FALSE) {
  if (!enable_vdiffr) {
    expect_error(regexp = NA, ggplot_build(fig))
    return(invisible(NULL))
  }

  vdiffr::expect_doppelganger(title, fig,
    path = path,
    ...,
    user_fonts = user_fonts,
    verbose = verbose
  )
}
