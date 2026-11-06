# constructive (1.4.0)

* GitHub: <https://github.com/cynkra/constructive>
* Email: <mailto:antoine.fabri@gmail.com>
* GitHub mirror: <https://github.com/cran/constructive>

Run `revdepcheck::cloud_details(, "constructive")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       + `ggplot2::guide_bins(theme = ggplot2::theme(legend.axis.line = ggplot2::element_line(arrow = my_arrow)))`
       
       Backtrace:
           ▆
        1. └─constructive:::expect_construct(...) at test-s3-ggplot2-GuideBins.R:9:3
        2.   └─testthat::expect_equal(recreated, expected)
       ── Failure ('test-s3-ggplot2-GuideLegend.R:9:3'): GuideLegend ──────────────────
       Expected `recreated` to equal `expected`.
       Differences:
       actual vs expected
       - `ggplot2::guide_legend(theme = ggplot2::theme(legend.title = structure(ggplot2::element_text(face = "italic", `
       - `    colour = "red", size = 15), class = c("ggplot2::element_text", `
       - `    "element_text", "ggplot2::element", "element", "S7_object"))))`
       + `ggplot2::guide_legend(theme = ggplot2::theme(legend.title = ggplot2::element_text(face = "italic", `
       + `    colour = "red", size = 15)))`
       
       Backtrace:
           ▆
        1. └─constructive:::expect_construct(...) at test-s3-ggplot2-GuideLegend.R:9:3
        2.   └─testthat::expect_equal(recreated, expected)
       
       [ FAIL 3 | WARN 0 | SKIP 227 | PASS 289 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# irelink (0.0.1)

* GitHub: <https://github.com/christopherkenny/irelink>
* Email: <mailto:ctkenny@proton.me>
* GitHub mirror: <https://github.com/cran/irelink>

Run `revdepcheck::cloud_details(, "irelink")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
         'test-il_count_pairs.R:158:1', 'test-il_estimate.R:236:1',
         'test-il_score_pairs.R:81:1', 'test-il_score_pairs.R:94:1',
         'test-sql-profile.R:83:1', 'test-sql-profile.R:98:1',
         'test-stage6-plan.R:80:1', 'test-stage6-plan.R:87:1',
         'test-stage6-plan.R:98:1', 'test-stage6-plan.R:119:1',
         'test-stage6-plan.R:141:1', 'test-stage6-plan.R:163:1'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-sql-clustering.R:314:3'): SQL best_link with source_dataset matches R tie handling ──
       Expected `unname(sql_map["A"])` to equal `unname(sql_map["C"])`.
       Differences:
       `actual`:   "cluster_A"
       `expected`: "cluster_B"
       
       ── Failure ('test-sql-clustering.R:316:3'): SQL best_link with source_dataset matches R tie handling ──
       Expected `sql_map[c("A", "B", "C")] == sql_map["A"]` to equal `r_map[c("A", "B", "C")] == r_map["A"]`.
       Differences:
       `actual`:   TRUE FALSE FALSE
       `expected`: TRUE FALSE TRUE 
       
       
       [ FAIL 2 | WARN 0 | SKIP 22 | PASS 1155 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# NHSRplotthedots (0.2.2)

* GitHub: <https://github.com/nhs-r-community/NHSRplotthedots>
* Email: <mailto:thomas.smith103@nhs.net>
* GitHub mirror: <https://github.com/cran/NHSRplotthedots>

Run `revdepcheck::cloud_details(, "NHSRplotthedots")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
     Running the tests in ‘tests/testthat.R’ failed.
     Complete output:
       > library(testthat)
       > library(NHSRplotthedots)
       > 
       > test_check("NHSRplotthedots")
       Saving _problems/test-ptd_create_ggplot-85.R
       [ FAIL 1 | WARN 16 | SKIP 4 | PASS 555 ]
       
       ══ Skipped tests (4) ═══════════════════════════════════════════════════════════
       • On CRAN (4): 'test-geom_ptd_icon.R:206:1', 'test-ptd_spc.R:344:1',
         'test-ptd_spc.R:378:1', 'test-ptd_spc_standard.R:19:1'
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-ptd_create_ggplot.R:85:3'): it returns a ggplot object ───────
       Expected `inherits(pl, "ggplot2::labels")` to be TRUE.
       Differences:
       `actual`:   FALSE
       `expected`: TRUE 
       
       
       [ FAIL 1 | WARN 16 | SKIP 4 | PASS 555 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# nycOpenData (0.2.3)

* GitHub: <https://github.com/ropensci/nycOpenData>
* Email: <mailto:c.martinez0@outlook.com>
* GitHub mirror: <https://github.com/cran/nycOpenData>

Run `revdepcheck::cloud_details(, "nycOpenData")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     ! NYC Open Data request failed with HTTP status 503.
     Try again later, or verify your filters.
     
     Response: {"message":"Service unavailable","errorCode":"service-unavailable","data":{}}
     ---
     Backtrace:
         ▆
      1. └─nycOpenData::nyc_pull_dataset(dataset = "h9gi-nx95", limit = 2)
      2.   └─nycOpenData:::.nyc_catalog_tbl()
      3.     └─nycOpenData:::.nyc_dataset_request(...)
      4.       └─nycOpenData:::.nyc_get_json(endpoint, query_list, timeout_sec = timeout_sec)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'getting-started.Rmd' failed with diagnostics:
     NYC Open Data request failed with HTTP status 503.
     Try again later, or verify your filters.
     
     Response: {"message":"Service unavailable","errorCode":"service-unavailable","data":{}}
     --- failed re-building ‘getting-started.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘getting-started.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# PGRdup (0.3.0)

* GitHub: <https://github.com/aravind-j/PGRdup>
* Email: <mailto:j.aravind@icar.org.in>
* GitHub mirror: <https://github.com/cran/PGRdup>

Run `revdepcheck::cloud_details(, "PGRdup")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     <error/rlang_error>
     Error in `curl::curl_fetch_memory()`:
     ! Timeout was reached [cran.r-project.org]:
     SSL connection timeout
     ---
     Backtrace:
         x
      1. +-httr::GET(link)
      2. | \-httr:::request_perform(req, hu$handle$handle)
      3. |   +-httr:::request_fetch(req$output, req$url, handle)
      4. |   \-httr:::request_fetch.write_memory(req$output, req$url, handle)
      5. |     \-curl::curl_fetch_memory(url, handle = handle)
      6. \-curl:::raise_libcurl_error(...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
     Timeout was reached [cran.r-project.org]:
     SSL connection timeout
     --- failed re-building ‘Introduction.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘Introduction.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
     ```
     Error(s) in re-building vignettes:
       ...
     --- re-building ‘Introduction.Rmd’ using rmarkdown_notangle
     ! LaTeX Error: File `colortbl.sty' not found.
     
     ! Emergency stop.
     <read *> 
     
     Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
     LaTeX failed to compile Introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See Introduction.log for more info.
     --- failed re-building ‘Introduction.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘Introduction.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# PSAboot (1.3.9)

* GitHub: <https://github.com/jbryer/PSAboot>
* Email: <mailto:jason@bryer.org>
* GitHub mirror: <https://github.com/cran/PSAboot>

Run `revdepcheck::cloud_details(, "PSAboot")` for more info

## Newly broken

*   checking re-building of vignette outputs ... ERROR
     ```
     ...
     --- re-building ‘MatchingOrder.Rmd’ using rmarkdown
     --- finished re-building ‘MatchingOrder.Rmd’
     
     --- re-building ‘PSAboot.Rmd’ using rmarkdown
     
     Quitting from PSAboot.Rmd:131-141 [lalonde.boot]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     <error/rlang_error>
     Error in `tmp[[i]]$summary`:
     ! $ operator is invalid for atomic vectors
     ---
     Backtrace:
         ▆
      1. └─PSAboot::PSAboot(...)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     Error: processing vignette 'PSAboot.Rmd' failed with diagnostics:
     $ operator is invalid for atomic vectors
     --- failed re-building ‘PSAboot.Rmd’
     
     SUMMARY: processing the following file failed:
       ‘PSAboot.Rmd’
     
     Error: Vignette re-building failed.
     Execution halted
     ```

# wjake (1.0.1)

* GitHub: <https://github.com/wjakethompson/wjake>
* Email: <mailto:wjakethompson@gmail.com>
* GitHub mirror: <https://github.com/cran/wjake>

Run `revdepcheck::cloud_details(, "wjake")` for more info

## Newly broken

*   checking examples ... ERROR
     ```
     Running examples in ‘wjake-Ex.R’ failed
     The error most likely occurred in:
     
     > ### Name: write_pkg_bib
     > ### Title: Write a bibliography for R packages
     > ### Aliases: write_pkg_bib
     > 
     > ### ** Examples
     > 
     > write_pkg_bib(c("ggplot2", "gt"), file = tempfile(fileext = "bib"))
     Error in if (repo %in% c("CRAN", "RSPM")) { : argument is of length zero
     Calls: write_pkg_bib -> <Anonymous> -> unlist -> lapply -> FUN
     Execution halted
     ```

# xpose.xtras (0.2.2)

* GitHub: <https://github.com/jprybylski/xpose.xtras>
* Email: <mailto:jprybylski@gmail.com>
* GitHub mirror: <https://github.com/cran/xpose.xtras>

Run `revdepcheck::cloud_details(, "xpose.xtras")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Failure ('test-xplot_boxplot.R:166:3'): xplot_boxplot ───────────────────────
       Expected `... <- NULL` not to throw any errors.
       Actually got a <expressionStackOverflowError> with message:
         evaluation nested too deeply: infinite recursion / options(expressions=)?
       ── Failure ('test-xplot_boxplot.R:169:3'): xplot_boxplot ───────────────────────
       Expected `old_b <- xplot_boxplot(xpdb_x, aes(MED1, ETA1), type = "b", quiet = TRUE)` not to throw any errors.
       Actually got a <expressionStackOverflowError> with message:
         evaluation nested too deeply: infinite recursion / options(expressions=)?
       ── Error ('test-xplot_boxplot.R:11:18'): xplot_boxplot ─────────────────────────
       Error in `eval(code, test_env)`: object 'old_bo' not found
       Backtrace:
           ▆
        1. ├─testthat::expect_setequal(geoms_lists(old_bo), "GeomBoxplot") at test-xplot_boxplot.R:172:3
        2. │ └─testthat::quasi_label(enquo(object))
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. └─xpose.xtras (local) geoms_lists(old_bo)
        5.   └─purrr::map_chr(gg$layers, ~class(.x$geom)[1]) at test-xplot_boxplot.R:11:18
        6.     └─purrr:::map_("character", .x, .f, ..., .progress = .progress)
        7.       └─purrr:::vctrs_vec_compat(.x, .purrr_user_env)
       
       [ FAIL 3 | WARN 0 | SKIP 43 | PASS 1581 ]
       Error:
       ! Test failures.
       Execution halted
     ```

