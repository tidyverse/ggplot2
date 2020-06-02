context("rlang conditions")

get_n_stop <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "stop")
}

get_n_warning <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "warning")
}

# Pattern is needed filter out files such as ggplot2.rdb, which is created when running covr::package_coverage()
R_files <- list.files("../../R", pattern = ".*\\.(R|r)$", full.names = TRUE)

test_that("do not use stop()", {
  stops <- vapply(R_files, get_n_stop, integer(1))
  expect_equal(sum(stops), 0)
})

test_that("do not use warning()", {
  warnings <- vapply(R_files, get_n_warning, integer(1))
  expect_equal(sum(warnings), 0)
})
