context("rlang conditions")

get_stop <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  d[d$token == "SYMBOL_FUNCTION_CALL" & d$text == "stop", ]
}

get_warning <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  d[d$token == "SYMBOL_FUNCTION_CALL" & d$text == "warning", ]
}

test_that("do not use stop()", {
  stops <- do.call(rbind, lapply(list.files("../../R", full.names = TRUE), get_stop))
  stop_usage <- nrow(stops)
  expect_equal(stop_usage, 0)
})

test_that("do not use warning()", {
  warnings <- do.call(rbind, lapply(list.files("../../R", full.names = TRUE), get_warning))
  warning_usage <- nrow(warnings)
  expect_equal(warning_usage, 0)
})
