
enable_vdiffr <- TRUE

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
