
if (!interactive()) {
  theme_set(theme_test())
}

expect_copycat <- function(title, fig, path = NULL, ..., user_fonts = NULL) {
  if (!interactive()) {
    vdiffr::expect_doppelganger(title, fig, path = path, ..., user_fonts = user_fonts)
  }
}
