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
  tmp <- withr::local_tempfile(lines = c(
    'stop("foo!")',
    'warning("bar!")',
    "data.frame(x = 1)",
    "base::data.frame(x = 1)"  # this is not counted
  ))

  expect_equal(get_n_stop(tmp), 1)
  expect_equal(get_n_warning(tmp), 1)
  expect_equal(get_n_data.frame(tmp), 1)
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
  files <- R_files[!grepl("^import-standalone-", basename(R_files))]
  aborts <- vapply(files, get_n_abort, integer(1))
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

test_that("No new argument names use underscores", {

  # For context:
  # We decided to use dot.case for argument names in exported functions,
  # not snake_case.
  # For historical reasons, some functions have snake_case arguments, which
  # we do not want to change.
  # Here, we take a snapshot of those functions and their arguments so that
  # any new argument that uses an underscore will fail this test.
  # Removing a function that uses an underscore argument will also fail this
  # test, in which case the snapshot needs updating.

  ns <- getNamespace("ggplot2")
  exported <- getNamespaceExports(ns)

  functions <- mget(exported, ns, mode = "function", ifnotfound = list(NULL))
  functions <- functions[lengths(functions) > 0]

  formals <- lapply(functions, fn_fmls_names)

  underscore_args <- lapply(formals, function(x) x[grep("_", x, fixed = TRUE)])
  underscore_args <- underscore_args[lengths(underscore_args) > 0]
  underscore_args <- underscore_args[order(names(underscore_args))]

  expect_snapshot(underscore_args)
})
