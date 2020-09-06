# vdiffr ignores failures when
#   - VDIFFR_RUN_TESTS is "false" (on Travis CI with older versions and dev version of R)
#   - CI is not set (on CRAN)

if (requireNamespace("vdiffr", quietly = TRUE)) {
  expect_doppelganger <- vdiffr::expect_doppelganger
} else {
  # If vdiffr is not available, assign a dummy function
  expect_doppelganger <- function(...) skip("vdiffr is not installed.")
}
