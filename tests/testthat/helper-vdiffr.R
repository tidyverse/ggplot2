# vdiffr ignores failures when
#   - VDIFFR_RUN_TESTS is "false" (on Travis CI with older versions and dev version of R)
#   - CI is not set (on CRAN)

# disable vdiffr if version is old
if (!requireNamespace("vdiffr", quietly = TRUE) ||
  utils::packageVersion("vdiffr") < "0.2.3.9001") {
  Sys.setenv(VDIFFR_RUN_TESTS = "false")
}

expect_doppelganger <- vdiffr::expect_doppelganger
