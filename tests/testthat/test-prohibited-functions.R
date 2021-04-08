context("rlang conditions")

get_n_stop <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "stop")
}

get_n_warning <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "warning")
}

get_n_data.frame <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "data.frame")
}

test_that("`get_n_*() detects number of calls properly", {
  withr::local_file("tmp.R")
  writeLines(
    c(
      'stop("foo!")',
      'warning("bar!")',
      "data.frame(x = 1)"
    ),
    "tmp.R"
  )

  expect_equal(get_n_stop("tmp.R"), 1)
  expect_equal(get_n_warning("tmp.R"), 1)
  expect_equal(get_n_data.frame("tmp.R"), 1)
})

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

test_that("do not use data.frame(), use `data_frame()` or `new_data_frame()`", {
  data.frames <- vapply(R_files, get_n_data.frame, integer(1))
  expect_equal(sum(data.frames), 0)
})
