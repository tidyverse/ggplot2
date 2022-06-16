get_n_stop <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "stop")
}

get_n_abort <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "abort")
}

get_n_warning <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "warning")
}

get_n_warn <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "warn")
}

get_n_inform <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))
  sum(d$token == "SYMBOL_FUNCTION_CALL" & d$text == "inform")
}

get_n_data.frame <- function(f) {
  d <- getParseData(parse(f, keep.source = TRUE))

  idx_base <- d$token == "SYMBOL_PACKAGE" & d$text == "base"
  idx_colons <- d$token == "NS_GET" & d$text == "::"
  # exclude the case when the `data.frame` is prefixed with `base::`
  idx_base_prefixed <- c(FALSE, FALSE, idx_base[1:(nrow(d) - 2)]) & c(FALSE, idx_colons[1:(nrow(d) - 1)])

  idx_data.frame <- d$token == "SYMBOL_FUNCTION_CALL" & d$text == "data.frame"
  sum(idx_data.frame & !idx_base_prefixed)
}

test_that("`get_n_*() detects number of calls properly", {
  withr::local_file("tmp.R")
  writeLines(
    c(
      'stop("foo!")',
      'warning("bar!")',
      "data.frame(x = 1)",
      "base::data.frame(x = 1)"  # this is not counted
    ),
    "tmp.R"
  )

  expect_equal(get_n_stop("tmp.R"), 1)
  expect_equal(get_n_warning("tmp.R"), 1)
  expect_equal(get_n_data.frame("tmp.R"), 1)
})

# Pattern is needed filter out files such as ggplot2.rdb, which is created when running covr::package_coverage()
R_paths <- c(
  "../../R",                     # in the case of devtools::test()
  "../../00_pkg_src/ggplot2/R"   # in the case of R CMD check
)
R_files <- list.files(R_paths, pattern = ".*\\.(R|r)$", full.names = TRUE)

test_that("list up R files properly", {
  skip_on_covr()
  skip_on_cran()

  expect_true(length(R_files) > 0)
})

test_that("do not use stop()", {
  stops <- vapply(R_files, get_n_stop, integer(1))
  expect_equal(sum(stops), 0)
})

test_that("do not use abort()", {
  aborts <- vapply(R_files, get_n_abort, integer(1))
  expect_equal(sum(aborts), 0)
})

test_that("do not use warning()", {
  warnings <- vapply(R_files, get_n_warning, integer(1))
  expect_equal(sum(warnings), 0)
})

test_that("do not use warn()", {
  warns <- vapply(R_files, get_n_warn, integer(1))
  expect_equal(sum(warns), 0)
})

test_that("do not use inform()", {
  informs <- vapply(R_files, get_n_inform, integer(1))
  expect_equal(sum(informs), 0)
})

test_that("do not use data.frame(), use `data_frame()` or `new_data_frame()`, or add `base::` prefix", {
  data.frames <- vapply(R_files, get_n_data.frame, integer(1))
  expect_equal(sum(data.frames), 0)
})
