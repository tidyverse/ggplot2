# afex

<details>

* Version: 1.1-1
* GitHub: https://github.com/singmann/afex
* Source code: https://github.com/cran/afex
* Date/Publication: 2022-04-29 23:30:07 UTC
* Number of recursive dependencies: 210

Run `cloud_details(, "afex")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘afex_analysing_accuracy_data.Rmd’ using rmarkdown
    Loading required package: lme4
    Loading required package: Matrix
    ************
    Welcome to afex. For support visit: http://afex.singmann.science/
    - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
    - Methods for calculating p-values with mixed(): 'S', 'KR', 'LRT', and 'PB'
    - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
    - NEWS: emmeans() for ANOVA models now uses model = 'multivariate' as default.
    ...
    --- finished re-building ‘assumptions_of_ANOVAs.Rmd’
    
    --- re-building ‘introduction-mixed-models.pdf.asis’ using asis
    --- finished re-building ‘introduction-mixed-models.pdf.asis’
    
    SUMMARY: processing the following file failed:
      ‘afex_plot_introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# antaresViz

<details>

* Version: 0.17
* GitHub: https://github.com/rte-antares-rpackage/antaresViz
* Source code: https://github.com/cran/antaresViz
* Date/Publication: 2021-11-24 09:20:02 UTC
* Number of recursive dependencies: 162

Run `cloud_details(, "antaresViz")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. └─antaresViz::plotXY(dta, "NODU", "LOAD", precision = 50, sizeOnCount = FALSE) at test-plotXY.R:7:4
       2.   ├─plotly::ggplotly(p)
       3.   └─plotly:::ggplotly.ggplot(p)
       4.     └─plotly::gg2list(...)
       5.       └─plotly:::layers2traces(data, prestats_data, layout, plot)
       6.         ├─plotly::to_basic(...)
       7.         └─plotly:::to_basic.GeomHex(...)
       8.           ├─base::`$<-`(`*tmp*`, "size", value = `<lgl>`)
       9.           └─base::`$<-.data.frame`(`*tmp*`, "size", value = `<lgl>`)
      
      [ FAIL 1 | WARN 2 | SKIP 13 | PASS 301 ]
      Error: Test failures
      Execution halted
    ```

# bayesplot

<details>

* Version: 1.9.0
* GitHub: https://github.com/stan-dev/bayesplot
* Source code: https://github.com/cran/bayesplot
* Date/Publication: 2022-03-10 11:20:23 UTC
* Number of recursive dependencies: 127

Run `cloud_details(, "bayesplot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      vline_at(c(3, 4), na.rm = FALSE) not equal to geom_vline(xintercept = c(3, 4)).
      Component "constructor": target, current do not match when deparsed
      ── Failure ('test-convenience-functions.R:30'): vline_* and hline_* return correct objects ──
      hline_at(c(3, 4), na.rm = FALSE) not equal to geom_hline(yintercept = c(3, 4)).
      Component "constructor": target, current do not match when deparsed
      ── Failure ('test-convenience-functions.R:38'): vline_at with 'fun' works ──────
      vline_at(x, colMeans) not equal to geom_vline(xintercept = colMeans(x), na.rm = TRUE).
      Component "constructor": target, current do not match when deparsed
      ── Failure ('test-convenience-functions.R:185'): overlay_function returns the correct object ──
      overlay_function(fun = "dnorm") not equal to stat_function(fun = "dnorm", inherit.aes = FALSE).
      Component "constructor": target, current do not match when deparsed
      
      [ FAIL 8 | WARN 23 | SKIP 49 | PASS 982 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        R     1.9Mb
        doc   3.8Mb
    ```

# bigsnpr

<details>

* Version: 1.11.4
* GitHub: https://github.com/privefl/bigsnpr
* Source code: https://github.com/cran/bigsnpr
* Date/Publication: 2022-10-21 16:05:07 UTC
* Number of recursive dependencies: 148

Run `cloud_details(, "bigsnpr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component "layers": Component 1: Component 12: Component 5: Component 21: Component 9: Component 3: Mean relative difference: 0.2645228
      Component "layers": Component 1: Component 12: Component 5: Component 21: Component 9: Component 4: Mean relative difference: 1.38146
      Component "layers": Component 1: Component 12: Component 5: Component 21: Component 19: Mean relative difference: 0.6696528
      ── Failure ('test-4-manhattan.R:22'): snp_manhattan() works with unordered data ──
      snp_manhattan(gwas, as.character(CHR), POS, npoints = 500) not equal to snp_manhattan(...).
      Component "layers": Component 1: Component 12: Component 5: Component 21: Component 9: Attributes: < Component "row.names": Mean relative difference: 0.6696528 >
      Component "layers": Component 1: Component 12: Component 5: Component 21: Component 9: Component 1: Mean relative difference: 1.396094
      Component "layers": Component 1: Component 12: Component 5: Component 21: Component 9: Component 2: Mean relative difference: 0.194434
      Component "layers": Component 1: Component 12: Component 5: Component 21: Component 9: Component 3: Mean relative difference: 0.2645228
      Component "layers": Component 1: Component 12: Component 5: Component 21: Component 9: Component 4: Mean relative difference: 1.38146
      Component "layers": Component 1: Component 12: Component 5: Component 21: Component 19: Mean relative difference: 0.6696528
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 75 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 20.8Mb
      sub-directories of 1Mb or more:
        libs  19.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘runonce’
      All declared Imports should be used.
    ```

# chronochrt

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/chronochrt
* Date/Publication: 2022-06-07 21:40:05 UTC
* Number of recursive dependencies: 94

Run `cloud_details(, "chronochrt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘chronochrt-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_chronochRt
    > ### Title: A chronological chart
    > ### Aliases: geom_chronochRt
    > 
    > ### ** Examples
    > 
    > 
    ...
     18. │                 └─ggplot2 (local) FUN(X[[i]], ...)
     19. │                   ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     20. │                   └─self$draw_panel(data, panel_params, coord, minimal = FALSE, year_lim = NULL)
     21. │                     └─chronochrt (local) draw_panel(...)
     22. └─base::.handleSimpleError(...)
     23.   └─rlang (local) h(simpleError(msg, call))
     24.     └─handlers[[1L]](cnd)
     25.       └─cli::cli_abort(...)
     26.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       39.     └─handlers[[1L]](cnd)
       40.       └─cli::cli_abort(...)
       41.         └─rlang::abort(...)
      Warning messages:
      1: Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      ℹ Please use `"xmax_uncorr"` instead of `.data$xmax_uncorr` 
      2: Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      ℹ Please use `"start"` instead of `.data$start` 
      3: Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      ℹ Please use `"end"` instead of `.data$end` 
      4: Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      ℹ Please use `"boundary_start"` instead of `.data$boundary_start` 
      5: Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      ℹ Please use `"boundary_end"` instead of `.data$boundary_end` 
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ChronochRt.Rmd’ using rmarkdown
    Scale for x is already present.
    Adding another scale for x, which will replace the existing scale.
    Quitting from lines 67-73 (ChronochRt.Rmd) 
    Error: processing vignette 'ChronochRt.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 1st layer.
    Caused by error in `draw_panel()`:
    ! object 'new_data_frame' not found
    ...
    ℹ Error occurred in the 1st layer.
    Caused by error in `draw_panel()`:
    ! object 'new_data_frame' not found
    --- failed re-building ‘Examples.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ChronochRt.Rmd’ ‘Examples.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# coveffectsplot

<details>

* Version: 1.0.2
* GitHub: https://github.com/smouksassi/coveffectsplot
* Source code: https://github.com/cran/coveffectsplot
* Date/Publication: 2022-05-30 10:50:02 UTC
* Number of recursive dependencies: 128

Run `cloud_details(, "coveffectsplot")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Exposure_Response_Example.Rmd’ using rmarkdown
    Building exprespmodel ... done.
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    Warning: The dot-dot notation (`..quantile..`) was deprecated in ggplot2 3.4.0.
    ℹ Please use `after_stat(quantile)` instead.
    Picking joint bandwidth of 0.015
    Picking joint bandwidth of 0.015
    Warning: Using the `size` aesthietic with geom_segment was deprecated in ggplot2 3.4.0.
    ...
    Picking joint bandwidth of 0.0248
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation ideoms with `aes()`
    --- finished re-building ‘introduction_to_coveffectsplot.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Pediatric_Cov_Sim.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘colourpicker’
      All declared Imports should be used.
    ```

# dataquieR

<details>

* Version: 1.0.11
* GitHub: NA
* Source code: https://github.com/cran/dataquieR
* Date/Publication: 2022-10-11 14:32:33 UTC
* Number of recursive dependencies: 134

Run `cloud_details(, "dataquieR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • int_datatype_matrix/intdtv30000.svg
      • int_datatype_matrix/intdtv40000.svg
      • int_datatype_matrix/intdtv50000.svg
      • int_datatype_matrix/integrity-datatype.svg
      • print/app-ex-repsumtab.svg
      • print/im-empty-repsumtab.svg
      • print/im-ex1-repsumtab.svg
      • print/im-ex2-repsumtab.svg
      • pro_applicability_matrix/appmatrix-plot-for-segment-v10000-ok.svg
      • pro_applicability_matrix/appmatrix-plot-ok.svg
      • util_heatmap_1th/util-heatmap-1th-1.svg
      • util_heatmap_1th/util-heatmap-1th-2.svg
      • util_heatmap_1th/util-heatmap-1th-3.svg
      Error: Test failures
      Execution halted
    ```

# deeptime

<details>

* Version: 0.2.3
* GitHub: https://github.com/willgearty/deeptime
* Source code: https://github.com/cran/deeptime
* Date/Publication: 2022-09-20 21:00:02 UTC
* Number of recursive dependencies: 175

Run `cloud_details(, "deeptime")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

# docxtools

<details>

* Version: 0.2.2
* GitHub: https://github.com/graphdr/docxtools
* Source code: https://github.com/cran/docxtools
* Date/Publication: 2020-06-03 18:40:03 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "docxtools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(docxtools)
      > 
      > test_check("docxtools")
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 22 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_put.R:21'): put_axes() attributes match expectations ─────────
      p$layers[[1]]$geom$non_missing_aes not identical to c("linetype", "size", "shape").
      1/3 mismatches
      x[2]: "linewidth"
      y[2]: "size"
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 22 ]
      Error: Test failures
      Execution halted
    ```

# DriveML

<details>

* Version: 0.1.4
* GitHub: https://github.com/daya6489/DriveML
* Source code: https://github.com/cran/DriveML
* Date/Publication: 2021-10-18 11:10:01 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "DriveML")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘DriveML.Rmd’ using rmarkdown
    Quitting from lines 329-331 (DriveML.Rmd) 
    Error: processing vignette 'DriveML.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 1st layer.
    Caused by error in `new_data_frame()`:
    ! `n` must be an integer of size 1.
    --- failed re-building ‘DriveML.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘DriveML.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked UTF-8 strings
    ```

# evaluate

<details>

* Version: 0.17
* GitHub: https://github.com/r-lib/evaluate
* Source code: https://github.com/cran/evaluate
* Date/Publication: 2022-10-07 15:00:07 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "evaluate")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      > 
      > if (require("testthat", quietly = TRUE)) test_check("evaluate")
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 60 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • empty test (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-evaluate.r:77'): return value of value handler inserted directly in output list ──
      classes(ev) not equal to c("source", "numeric", "source", "source", "source", "gg").
      Lengths differ: 7 is not 6
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 60 ]
      Error: Test failures
      Execution halted
    ```

# fdistr

<details>

* Version: 0.1.0
* GitHub: https://github.com/dtminnick/fdistr
* Source code: https://github.com/cran/fdistr
* Date/Publication: 2019-12-02 14:30:06 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "fdistr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(fdistr)
      > 
      > test_check("fdistr")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 13 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test_create_pareto.R:7'): (code run outside of `test_that()`) ───────
      Error in `create_pareto(table)`: object 'g' not found
      Backtrace:
          ▆
       1. └─fdistr::create_pareto(table) at test_create_pareto.R:7:0
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 13 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘usethis’ ‘utils’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked Latin-1 string
    ```

# ggbeeswarm

<details>

* Version: 0.6.0
* GitHub: https://github.com/eclarke/ggbeeswarm
* Source code: https://github.com/cran/ggbeeswarm
* Date/Publication: 2017-08-07 13:45:36 UTC
* Number of recursive dependencies: 32

Run `cloud_details(, "ggbeeswarm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggbeeswarm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: position_beeswarm
    > ### Title: Violin point-style plots to show overlapping points. x must be
    > ###   discrete.
    > ### Aliases: position_beeswarm
    > 
    > ### ** Examples
    > 
    ...
    Error:
    ! The `position` argument of `qplot()` was deprecated in ggplot2 2.0.0
      and is now defunct.
    Backtrace:
        ▆
     1. └─ggplot2::qplot(class, hwy, data = ggplot2::mpg, position = position_beeswarm())
     2.   └─lifecycle::deprecate_stop("2.0.0", "qplot(position)")
     3.     └─lifecycle:::deprecate_stop0(msg)
     4.       └─rlang::cnd_signal(...)
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘usageExamples.Rnw’ using Sweave
    Loading required package: ggplot2
    Error: processing vignette 'usageExamples.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'usageExamples.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `ae.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    ...
    l.55 \RequirePackage
                        [T1]{fontenc}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘usageExamples.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘usageExamples.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggconf

<details>

* Version: 0.1.3
* GitHub: https://github.com/caprice-j/ggconf
* Source code: https://github.com/cran/ggconf
* Date/Publication: 2018-04-08 17:44:46 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "ggconf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggconf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme2
    > ### Title: an enhanced version of ggplot2::theme()
    > ### Aliases: theme2
    > 
    > ### ** Examples
    > 
    > 
    ...
    + 
    + ggplot(mtcars) + geom_point(aes(wt, hp, color=cyl)) +
    +    theme2(txt(sz=20, f="bold"), aline(sz=2), l.key(c="black"))
    + 
    + 
    + 
    + }
    Error in ggplot2::element_line(fill = 1) : unused argument (fill = 1)
    Calls: theme2 ... exec_ggconf -> eval -> eval -> <Anonymous> -> find_args -> mget
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error ('test-theme2.R:37'): theme2 ──────────────────────────────────────────
      Error in `ggplot2::element_line(fill = 1)`: unused argument (fill = 1)
      Backtrace:
          ▆
       1. └─ggconf::theme2(...) at test-theme2.R:37:4
       2.   └─ggconf:::exec_ggconf(...)
       3.     ├─base::eval(parse(text = ggobj_verbose))
       4.     │ └─base::eval(parse(text = ggobj_verbose))
       5.     └─ggplot2::theme(...)
       6.       └─ggplot2:::find_args(..., complete = NULL, validate = NULL)
       7.         └─base::mget(args, envir = env)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 33 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggdag

<details>

* Version: 0.2.6
* GitHub: https://github.com/malcolmbarrett/ggdag
* Source code: https://github.com/cran/ggdag
* Date/Publication: 2022-08-26 21:34:37 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "ggdag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggdag-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Pathways
    > ### Title: Find Open Paths Between Variables
    > ### Aliases: Pathways dag_paths ggdag_paths ggdag_paths_fan
    > 
    > ### ** Examples
    > 
    > confounder_triangle(x_y_associated = TRUE) %>%
    ...
     29.       └─vctrs:::vec_ptype2.factor.ordered(...)
     30.         └─vctrs::vec_default_ptype2(x, y, ...)
     31.           ├─base::withRestarts(...)
     32.           │ └─base (local) withOneRestart(expr, restarts[[1L]])
     33.           │   └─base (local) doWithOneRestart(return(expr), restart)
     34.           └─vctrs::stop_incompatible_type(...)
     35.             └─vctrs:::stop_incompatible(...)
     36.               └─vctrs:::stop_vctrs(...)
     37.                 └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • geom_dag/geom-dag-edges-fan-is-fany.svg
      • geom_dag/geom-dag-label-repel-repels-labels.svg
      • ggdag/ggdag-classic-plots-basic-dag-classically.svg
      • paths/ggdag-paths-fan-draws-4-open-paths.svg
      • quick_plots/ggdag-butterfly-bias-is-a-butterfly.svg
      • quick_plots/ggdag-collider-triangle-is-triangle-too.svg
      • quick_plots/ggdag-confounder-triangle-is-triangle.svg
      • relations/ggdag-ancestors-identifies-v-w1-and-z1.svg
      • relations/ggdag-descendants-identifies-y-x-and-z1.svg
      • relations/ggdag-parents-identifies-z2-x-w1-and-w2.svg
      • themes/theme-dag-gray-grid.svg
      • themes/theme-dag-gray.svg
      • themes/theme-dag-grid.svg
      Error: Test failures
      Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bias-structures.Rmd’ using rmarkdown
    
    Attaching package: 'ggdag'
    
    The following object is masked from 'package:stats':
    
        filter
    
    --- finished re-building ‘bias-structures.Rmd’
    ...
    Quitting from lines 110-111 (intro-to-ggdag.Rmd) 
    Error: processing vignette 'intro-to-ggdag.Rmd' failed with diagnostics:
    Can't combine `..1$forcats::fct_inorder(as.factor(set), ordered = TRUE)` <factor<66fa2>> and `..3$forcats::fct_inorder(as.factor(set), ordered = TRUE)` <ordered<>>.
    --- failed re-building ‘intro-to-ggdag.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘intro-to-dags.Rmd’ ‘intro-to-ggdag.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggetho

<details>

* Version: 0.3.6
* GitHub: https://github.com/rethomics/ggetho
* Source code: https://github.com/cran/ggetho
* Date/Publication: 2020-04-29 19:30:02 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "ggetho")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggfun

<details>

* Version: 0.0.7
* GitHub: NA
* Source code: https://github.com/cran/ggfun
* Date/Publication: 2022-08-31 06:30:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "ggfun")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

# gghalves

<details>

* Version: 0.1.3
* GitHub: https://github.com/erocoar/gghalves
* Source code: https://github.com/cran/gghalves
* Date/Publication: 2022-05-30 10:10:08 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "gghalves")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gghalves-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_half_boxplot
    > ### Title: A half boxplot
    > ### Aliases: geom_half_boxplot
    > 
    > ### ** Examples
    > 
    > ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) + 
    ...
     24. │                           └─self$draw_group(group, panel_params, coord, ...)
     25. │                             └─gghalves (local) draw_group(...)
     26. │                               └─base::data.frame(...)
     27. │                                 └─base::stop(...)
     28. └─base::.handleSimpleError(...)
     29.   └─rlang (local) h(simpleError(msg, call))
     30.     └─handlers[[1L]](cnd)
     31.       └─cli::cli_abort(...)
     32.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘gghalves.Rmd’ using rmarkdown
    Quitting from lines 66-69 (gghalves.Rmd) 
    Error: processing vignette 'gghalves.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 1st layer.
    Caused by error in `data.frame()`:
    ! arguments imply differing number of rows: 1, 0
    --- failed re-building ‘gghalves.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘gghalves.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘grDevices’ ‘gtable’
      All declared Imports should be used.
    ```

# ggip

<details>

* Version: 0.2.2
* GitHub: https://github.com/davidchall/ggip
* Source code: https://github.com/cran/ggip
* Date/Publication: 2022-09-29 06:00:02 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "ggip")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("ggip")
      [ FAIL 2 | WARN 21 | SKIP 5 | PASS 92 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (5)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-geom-hilbert-outline.R:89'): networks outside 2D grid raise warning ──
      `layer_grob(p + geom_hilbert_outline(na.rm = TRUE))` produced warnings.
      ── Failure ('test-geom-hilbert-outline.R:99'): networks without outline are silently ignored ──
      `layer_grob(p)` produced warnings.
      
      [ FAIL 2 | WARN 21 | SKIP 5 | PASS 92 ]
      Error: Test failures
      Execution halted
    ```

# ggiraph

<details>

* Version: 0.8.3
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2022-08-19 11:00:14 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "ggiraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotate_interactive
    > ### Title: Create interactive annotations
    > ### Aliases: annotate_interactive
    > 
    > ### ** Examples
    > 
    > # add interactive annotation to a ggplot -------
    ...
     30. │                           └─grid:::validGP(list(...))
     31. │                             └─grid (local) numnotnull("lwd")
     32. │                               └─grid (local) check.length(gparname)
     33. │                                 └─base::stop(...)
     34. └─base::.handleSimpleError(...)
     35.   └─rlang (local) h(simpleError(msg, call))
     36.     └─handlers[[1L]](cnd)
     37.       └─cli::cli_abort(...)
     38.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
       37. │                                         └─ggplot2 (local) FUN(X[[i]], ...)
       38. │                                           └─ggplot2 (local) apply_fun(cur_data)
       39. │                                             └─ggiraph (local) fun(x, ...)
       40. │                                               ├─ggiraph:::new_data_frame(...)
       41. │                                               ├─base::nrow(unique(df[, c("alpha", "colour", "size", "linetype")]))
       42. │                                               ├─base::unique(df[, c("alpha", "colour", "size", "linetype")])
       43. │                                               ├─df[, c("alpha", "colour", "size", "linetype")]
       44. │                                               └─base::`[.data.frame`(df, , c("alpha", "colour", "size", "linetype"))
       45. │                                                 └─base::stop("undefined columns selected")
       46. └─base::.handleSimpleError(...)
       47.   └─rlang (local) h(simpleError(msg, call))
       48.     └─handlers[[1L]](cnd)
       49.       └─cli::cli_abort(...)
       50.         └─rlang::abort(...)
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.7Mb
      sub-directories of 1Mb or more:
        libs   8.9Mb
    ```

# ggiraphExtra

<details>

* Version: 0.3.0
* GitHub: https://github.com/cardiomoon/ggiraphExtra
* Source code: https://github.com/cran/ggiraphExtra
* Date/Publication: 2020-10-06 07:00:02 UTC
* Number of recursive dependencies: 101

Run `cloud_details(, "ggiraphExtra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraphExtra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggAncova
    > ### Title: Make an interactive plot for an ANCOVA model
    > ### Aliases: ggAncova ggAncova.default ggAncova.formula ggAncova.lm
    > 
    > ### ** Examples
    > 
    > require(moonBook)
    ...
     26. │                           ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     27. │                           └─self$draw_panel(...)
     28. │                             └─ggiraph (local) draw_panel(...)
     29. │                               └─ggplot2:::dapply(...)
     30. └─base::.handleSimpleError(...)
     31.   └─rlang (local) h(simpleError(msg, call))
     32.     └─handlers[[1L]](cnd)
     33.       └─cli::cli_abort(...)
     34.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggPredict.Rmd’ using rmarkdown
    Loading required package: ggplot2
    `geom_smooth()` using formula = 'y ~ x'
    Loading required package: ggiraph
    Loading required package: ggiraphExtra
    
    Attaching package: 'ggiraphExtra'
    
    The following objects are masked from 'package:moonBook':
    ...
    ℹ Error occurred in the 3rd layer.
    Caused by error in `rbind_dfs()`:
    ! could not find function "rbind_dfs"
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ggPredict.Rmd’ ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gglgbtq

<details>

* Version: 0.1.0
* GitHub: https://github.com/turtletopia/gglgbtq
* Source code: https://github.com/cran/gglgbtq
* Date/Publication: 2022-08-16 09:00:05 UTC
* Number of recursive dependencies: 57

Run `cloud_details(, "gglgbtq")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

# ggmap

<details>

* Version: 3.0.0
* GitHub: https://github.com/dkahle/ggmap
* Source code: https://github.com/cran/ggmap
* Date/Publication: 2019-02-05 10:19:04
* Number of recursive dependencies: 69

Run `cloud_details(, "ggmap")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

# ggmulti

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/ggmulti
* Date/Publication: 2022-10-08 15:30:05 UTC
* Number of recursive dependencies: 120

Run `cloud_details(, "ggmulti")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmulti-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_hist_
    > ### Title: More general histogram
    > ### Aliases: geom_hist_ geom_histogram_ geom_bar_ stat_hist_ stat_bin_
    > ###   stat_count_
    > 
    > ### ** Examples
    > 
    ...
     14.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     15.         └─l$compute_statistic(d, layout)
     16.           └─ggplot2 (local) compute_statistic(..., self = self)
     17.             └─self$stat$setup_params(data, self$stat_params)
     18.               └─ggmulti (local) setup_params(..., self = self)
     19.                 └─ggplot2::ggproto_parent(ggplot2::StatBin, self)$setup_params(...)
     20.                   └─ggplot2 (local) setup_params(..., self = self)
     21.                     └─cli::cli_abort(...)
     22.                       └─rlang::abort(...)
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

# ggprism

<details>

* Version: 1.0.3
* GitHub: https://github.com/csdaw/ggprism
* Source code: https://github.com/cran/ggprism
* Date/Publication: 2021-06-08 11:40:02 UTC
* Number of recursive dependencies: 104

Run `cloud_details(, "ggprism")` for more info

</details>

## Newly broken

*   checking whether package ‘ggprism’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggprism/new/ggprism.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggprism’ ...
** package ‘ggprism’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'new_data_frame' not found
Error: unable to load R code in package ‘ggprism’
Execution halted
ERROR: lazy loading failed for package ‘ggprism’
* removing ‘/tmp/workdir/ggprism/new/ggprism.Rcheck/ggprism’


```
### CRAN

```
* installing *source* package ‘ggprism’ ...
** package ‘ggprism’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggprism)


```
# ggpubr

<details>

* Version: 0.4.0
* GitHub: https://github.com/kassambara/ggpubr
* Source code: https://github.com/cran/ggpubr
* Date/Publication: 2020-06-27 06:20:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "ggpubr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpubr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: stat_conf_ellipse
    > ### Title: Plot confidence ellipses.
    > ### Aliases: stat_conf_ellipse
    > 
    > ### ** Examples
    > 
    > # Load data
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(message, call = call)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

# ggshadow

<details>

* Version: 0.0.2
* GitHub: https://github.com/marcmenem/ggshadow
* Source code: https://github.com/cran/ggshadow
* Date/Publication: 2021-01-22 08:50:03 UTC
* Number of recursive dependencies: 49

Run `cloud_details(, "ggshadow")` for more info

</details>

## Newly broken

*   checking whether package ‘ggshadow’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggshadow/new/ggshadow.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘ggshadow’ ...
** package ‘ggshadow’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'new_data_frame' not found
Error: unable to load R code in package ‘ggshadow’
Execution halted
ERROR: lazy loading failed for package ‘ggshadow’
* removing ‘/tmp/workdir/ggshadow/new/ggshadow.Rcheck/ggshadow’


```
### CRAN

```
* installing *source* package ‘ggshadow’ ...
** package ‘ggshadow’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggshadow)


```
# ggstance

<details>

* Version: 0.3.5
* GitHub: https://github.com/lionel-/ggstance
* Source code: https://github.com/cran/ggstance
* Date/Publication: 2020-12-17 19:40:02 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "ggstance")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggstar

<details>

* Version: 1.0.3
* GitHub: https://github.com/xiangpin/ggstar
* Source code: https://github.com/cran/ggstar
* Date/Publication: 2021-12-03 05:30:02 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "ggstar")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘cli’
    ```

# ggtern

<details>

* Version: 3.3.5
* GitHub: NA
* Source code: https://github.com/cran/ggtern
* Date/Publication: 2021-07-23 05:20:02 UTC
* Number of recursive dependencies: 43

Run `cloud_details(, "ggtern")` for more info

</details>

## Newly broken

*   checking whether package ‘ggtern’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggtern/new/ggtern.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hexbin’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘chemometrics’
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘sp’
    ```

## Installation

### Devel

```
* installing *source* package ‘ggtern’ ...
** package ‘ggtern’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'message_wrap' not found
Error: unable to load R code in package ‘ggtern’
Execution halted
ERROR: lazy loading failed for package ‘ggtern’
* removing ‘/tmp/workdir/ggtern/new/ggtern.Rcheck/ggtern’


```
### CRAN

```
* installing *source* package ‘ggtern’ ...
** package ‘ggtern’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggtern)


```
# iheiddown

<details>

* Version: 0.9.5
* GitHub: https://github.com/jhollway/iheiddown
* Source code: https://github.com/cran/iheiddown
* Date/Publication: 2022-09-27 07:50:09 UTC
* Number of recursive dependencies: 148

Run `cloud_details(, "iheiddown")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("iheiddown")
      [ FAIL 1 | WARN 1 | SKIP 2 | PASS 33 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (1)
      • On Linux (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-theme.R:28'): theme_iheid() grid is correct ──────────────────
      p[["theme"]][["panel.grid"]][["size"]] not identical to 0.2.
      target is NULL, current is numeric
      
      [ FAIL 1 | WARN 1 | SKIP 2 | PASS 33 ]
      Error: Test failures
      Execution halted
    ```

# inTextSummaryTable

<details>

* Version: 3.2.0
* GitHub: https://github.com/openanalytics/inTextSummaryTable
* Source code: https://github.com/cran/inTextSummaryTable
* Date/Publication: 2022-10-01 07:40:02 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "inTextSummaryTable")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("inTextSummaryTable")
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 978 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test_subjectProfileSummaryPlot-theme.R:39'): The line size is correctly set ──
      Error in `expect_setequal(ggDataLine$size, sizeLine)`: `object` and `expected` must both be vectors
      Backtrace:
          ▆
       1. └─testthat::expect_setequal(ggDataLine$size, sizeLine) at test_subjectProfileSummaryPlot-theme.R:39:8
       2.   └─rlang::abort("`object` and `expected` must both be vectors")
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 978 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.3Mb
      sub-directories of 1Mb or more:
        doc   9.7Mb
    ```

# jjAnno

<details>

* Version: 0.0.3
* GitHub: https://github.com/junjunlab/jjAnno
* Source code: https://github.com/cran/jjAnno
* Date/Publication: 2022-08-23 08:30:08 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "jjAnno")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘jjAnno-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annoRect
    > ### Title: annoRect
    > ### Aliases: annoRect
    > 
    > ### ** Examples
    > 
    > # ===============================
    ...
     11.                 └─base::lapply(...)
     12.                   └─ggplot2 (local) FUN(X[[i]], ...)
     13.                     └─base::lapply(...)
     14.                       └─ggplot2 (local) FUN(X[[i]], ...)
     15.                         └─scales[[i]][[method]](data[[var]][scale_index[[i]]])
     16.                           └─ggplot2 (local) map(..., self = self)
     17.                             └─ggplot2:::new_mapped_discrete(x)
     18.                               └─vctrs::vec_assert(as.vector(x), double())
     19.                                 └─rlang::abort(...)
    Execution halted
    ```

# lemon

<details>

* Version: 0.4.5
* GitHub: https://github.com/stefanedwards/lemon
* Source code: https://github.com/cran/lemon
* Date/Publication: 2020-06-08 09:10:03 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "lemon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lemon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotate_y_axis
    > ### Title: Annotations in the axis
    > ### Aliases: annotate_y_axis annotate_x_axis
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
        CoordCartesian, element_render
    
    > 
    > p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()
    > 
    > l <- p + annotate_y_axis('mark at', y=200, tick=TRUE)
    > l
    Error in if (is.primary) { : argument is of length zero
    Calls: <Anonymous> ... <Anonymous> -> draw -> <Anonymous> -> get_annotations
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_error(...) at test_lemon_plot.r:12:2
        2. │ └─testthat:::quasi_capture(...)
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─ggplot2:::`+.gg`(l, 1)
        7.   └─ggplot2:::add_ggplot(e1, e2, e2name)
        8.     ├─ggplot2::ggplot_add(object, p, objectname)
        9.     └─ggplot2:::ggplot_add.default(object, p, objectname)
       10.       └─cli::cli_abort("Can't add {.var {object_name}} to a {.cls ggplot} object.")
       11.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 137 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘capped-axes.Rmd’ using rmarkdown
    Quitting from lines 61-62 (capped-axes.Rmd) 
    Error: processing vignette 'capped-axes.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘capped-axes.Rmd’
    
    --- re-building ‘facet-rep-labels.Rmd’ using rmarkdown
    Quitting from lines 38-42 (facet-rep-labels.Rmd) 
    Error: processing vignette 'facet-rep-labels.Rmd' failed with diagnostics:
    ...
    
        ```{r %s}
    '
    --- finished re-building ‘lemon_print.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘capped-axes.Rmd’ ‘facet-rep-labels.Rmd’ ‘legends.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# lingtypology

<details>

* Version: 1.1.9
* GitHub: https://github.com/ropensci/lingtypology
* Source code: https://github.com/cran/lingtypology
* Date/Publication: 2022-06-24 12:10:02 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "lingtypology")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lingtypology-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggmap.feature
    > ### Title: Create a map with ggplot2
    > ### Aliases: ggmap.feature
    > 
    > ### ** Examples
    > 
    > ggmap.feature(c("Adyghe", "Russian"))
    ...
     20. │                   └─self$draw_panel(...)
     21. │                     └─ggplot2 (local) draw_panel(...)
     22. │                       └─ggplot2:::dapply(...)
     23. │                         └─base::lapply(...)
     24. │                           └─ggplot2 (local) FUN(X[[i]], ...)
     25. │                             └─ggplot2 (local) apply_fun(cur_data)
     26. │                               └─ggplot2 (local) fun(x, ...)
     27. │                                 └─vctrs::new_data_frame(...)
     28. └─rlang::abort(message = message)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 60596 marked UTF-8 strings
    ```

# listdown

<details>

* Version: 0.5.2
* GitHub: https://github.com/kaneplusplus/listdown
* Source code: https://github.com/cran/listdown
* Date/Publication: 2022-07-19 17:10:06 UTC
* Number of recursive dependencies: 73

Run `cloud_details(, "listdown")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.r’
    Running the tests in ‘tests/testthat.r’ failed.
    Last 13 lines of output:
      `ldb` not equal to read_reference("listdown-page-bundle.rds").
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Names: 10 string mismatches
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Length mismatch: comparison on first 11 components
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 2: Modes of target, current: call, list
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 2: target, current do not match when deparsed
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 3: 'target' is not an environment
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 4: 'current' is not an environment
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 5: Modes: list, logical
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 5: names for target but not for current
      Component "cc": Component "Sepal.Length": Component "Sepal.Width": Component "layers": Component 1: Component 5: Component 1: 1 element mismatch
      ...
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 35 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘workflowr’
    ```

# openalexR

<details>

* Version: 1.0.0
* GitHub: https://github.com/massimoaria/openalexR
* Source code: https://github.com/cran/openalexR
* Date/Publication: 2022-10-06 10:40:02 UTC
* Number of recursive dependencies: 78

Run `cloud_details(, "openalexR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘A_Brief_Introduction_to_openalexR.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Quitting from lines 207-213 (A_Brief_Introduction_to_openalexR.Rmd) 
    Error: processing vignette 'A_Brief_Introduction_to_openalexR.Rmd' failed with diagnostics:
    $ operator is invalid for atomic vectors
    --- failed re-building ‘A_Brief_Introduction_to_openalexR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘A_Brief_Introduction_to_openalexR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# pathviewr

<details>

* Version: 1.1.3
* GitHub: https://github.com/ropensci/pathviewr
* Source code: https://github.com/cran/pathviewr
* Date/Publication: 2022-08-22 07:50:14 UTC
* Number of recursive dependencies: 151

Run `cloud_details(, "pathviewr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-plot_by_subject.R:85'): top views created correctly via purrr::map ──
      environment(top_view[[4]][[3]][["facet"]][["super"]])[["args"]] not equal to NULL.
      Modes: list, NULL
      Lengths: 1, 0
      current is not list-like
      ── Failure ('test-plot_by_subject.R:161'): elev views created correctly via purrr::map ──
      environment(elev_view[[4]][[3]][["facet"]][["super"]])[["args"]] not equal to NULL.
      Modes: list, NULL
      Lengths: 1, 0
      current is not list-like
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 288 ]
      Error: Test failures
      Execution halted
    ```

# plotly

<details>

* Version: 4.10.0
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2021-10-09 21:10:07 UTC
* Number of recursive dependencies: 164

Run `cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • plotly/plotly-bar-inference.svg
      • plotly/plotly-box-data-array.svg
      • plotly/plotly-character-axis.svg
      • plotly/plotly-factor-axis.svg
      • plotly/plotly-group-within-trace.svg
      • plotly/plotly-histogram-vert.svg
      • plotly/plotly-histogram.svg
      • plotly/plotly-inherit-false.svg
      • plotly/plotly-scatterplot.svg
      • plotly/plotly-time-series-summary.svg
      • ticktext-linebreaks/ticktext-linebreaks-no-linebreaks.svg
      • ticktext-linebreaks/ticktext-linebreaks-one-cat.svg
      • ticktext-linebreaks/ticktext-linebreaks.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        htmlwidgets   4.0Mb
    ```

# plotmm

<details>

* Version: 0.1.0
* GitHub: https://github.com/pdwaggoner/plotmm
* Source code: https://github.com/cran/plotmm
* Date/Publication: 2020-07-10 08:40:02 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "plotmm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plotmm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_gmm
    > ### Title: Plots Mixture Components from Gaussian Mixture Models
    > ### Aliases: plot_gmm
    > 
    > ### ** Examples
    > 
    > if(require(mixtools)){
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(message, call = call)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘patchwork’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# qqboxplot

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/qqboxplot
* Date/Publication: 2022-03-25 08:00:05 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "qqboxplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qqboxplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_qqboxplot
    > ### Title: A modification of the boxplot with information about the tails
    > ### Aliases: geom_qqboxplot
    > 
    > ### ** Examples
    > 
    > p <- ggplot2::ggplot(simulated_data, ggplot2::aes(factor(group,
    ...
     13. │       └─l$compute_statistic(d, layout)
     14. │         └─ggplot2 (local) compute_statistic(..., self = self)
     15. │           └─self$stat$setup_params(data, self$stat_params)
     16. │             └─qqboxplot (local) setup_params(...)
     17. └─base::.handleSimpleError(...)
     18.   └─rlang (local) h(simpleError(msg, call))
     19.     └─handlers[[1L]](cnd)
     20.       └─cli::cli_abort(...)
     21.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘qqboxplot-basic-usage.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    ...
    ℹ Error occurred in the 1st layer.
    Caused by error in `has_groups()`:
    ! could not find function "has_groups"
    --- failed re-building ‘qqboxplot-paper-replication.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘qqboxplot-basic-usage.Rmd’ ‘qqboxplot-paper-replication.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# RJafroc

<details>

* Version: 2.1.1
* GitHub: NA
* Source code: https://github.com/cran/RJafroc
* Date/Publication: 2022-08-12 20:30:03 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "RJafroc")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      PlotRsmOperatingCharacteristics(...) not equal to `ret`.
      Component "wAFROCPlot": Component "layers": Component 1: Names: 10 string mismatches
      Component "wAFROCPlot": Component "layers": Component 1: Length mismatch: comparison on first 11 components
      Component "wAFROCPlot": Component "layers": Component 1: Component 2: Modes of target, current: call, list
      Component "wAFROCPlot": Component "layers": Component 1: Component 2: target, current do not match when deparsed
      Component "wAFROCPlot": Component "layers": Component 1: Component 3: 'target' is not an environment
      Component "wAFROCPlot": Component "layers": Component 1: Component 4: 'current' is not an environment
      Component "wAFROCPlot": Component "layers": Component 1: Component 5: Modes: list, logical
      Component "wAFROCPlot": Component "layers": Component 1: Component 5: names for target but not for current
      Component "wAFROCPlot": Component "layers": Component 1: Component 5: Length mismatch: comparison on first 1 components
      ...
      
      [ FAIL 17 | WARN 1 | SKIP 9 | PASS 1457 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        extdata   1.6Mb
        libs      2.7Mb
    ```

# robustlmm

<details>

* Version: 3.0-4
* GitHub: https://github.com/kollerma/robustlmm
* Source code: https://github.com/cran/robustlmm
* Date/Publication: 2022-09-17 20:36:08 UTC
* Number of recursive dependencies: 86

Run `cloud_details(, "robustlmm")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... WARNING
    ```
    Errors in running code in vignettes:
    when running code in ‘simulationStudies.Rnw’
      ...
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called ‘robustvarComp’
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
    > print(plot_robustnessDiagonal)
    
      When sourcing ‘simulationStudies.R’:
    Error: could not find function "rbind_dfs"
    Execution halted
    
      ‘rlmer.Rnw’ using ‘UTF-8’... OK
      ‘simulationStudies.Rnw’ using ‘UTF-8’... failed
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 53.8Mb
      sub-directories of 1Mb or more:
        doc               1.7Mb
        libs             49.6Mb
        simulationStudy   1.5Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘rlmer.Rnw’ using Sweave
    Loading required package: robustlmm
    Loading required package: lme4
    Loading required package: Matrix
    Warning in eval(expr, .GlobalEnv) :
      Current dir: /tmp/workdir/robustlmm/new/robustlmm.Rcheck/robustlmm/ has contents: CITATION, DESCRIPTION, INDEX, Meta, NAMESPACE, R, doc, help, html, libs, simulationStudy, xtraR
    Warning in eval(expr, .GlobalEnv) :
      doc dir: /tmp/workdir/robustlmm/new/robustlmm.Rcheck/robustlmm/doc has contents: Penicillin.R, ggplot.theme.R, index.html, plots.R, rlmer.R, rlmer.Rnw, rlmer.pdf, simulationStudies.R, simulationStudies.Rnw, simulationStudies.pdf, sleepstudy.R
    Warning in eval(expr, .GlobalEnv) :
    ...
     chunk 25 (label = plot_robustnessDiagonal) 
    Error in rbind_dfs(values[has_all]) : could not find function "rbind_dfs"
    
    --- failed re-building ‘simulationStudies.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘rlmer.Rnw’ ‘simulationStudies.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# scdhlm

<details>

* Version: 0.6.0
* GitHub: https://github.com/jepusto/scdhlm
* Source code: https://github.com/cran/scdhlm
* Date/Publication: 2022-07-07 22:20:02 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "scdhlm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11.             └─base::all.equal.list(Lt, Lc, ...)
       12.               ├─base::all.equal(...)
       13.               └─base::all.equal.function(...)
       14.                 └─base::all.equal.environment(...)
       15.                   └─base::all.equal.list(Lt, Lc, ...)
       16.                     ├─base::all.equal(...)
       17.                     └─base::all.equal.environment(...)
       18.                       └─base::all.equal.list(Lt, Lc, ...)
       19.                         ├─base::all.equal(...)
       20.                         └─base::all.equal.environment(...)
       21.                           └─base::as.list.environment(target, all.names = all.names, sorted = TRUE)
      
      [ FAIL 3 | WARN 1 | SKIP 10 | PASS 156 ]
      Error: Test failures
      Execution halted
    ```

# schtools

<details>

* Version: 0.3.0
* GitHub: https://github.com/SchlossLab/schtools
* Source code: https://github.com/cran/schtools
* Date/Publication: 2022-10-05 21:50:02 UTC
* Number of recursive dependencies: 115

Run `cloud_details(, "schtools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      plot_sovacool$theme not equal to structure(...).
      Names: 7 string mismatches
      Length mismatch: comparison on first 93 components
      Component "line": Names: 1 string mismatch
      Component "rect": Names: 1 string mismatch
      Component "axis.ticks": Names: 1 string mismatch
      Component "legend.background": Names: 1 string mismatch
      Component "legend.key": Names: 1 string mismatch
      Component "panel.background": Names: 1 string mismatch
      Component "panel.border": Names: 1 string mismatch
      ...
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 46 ]
      Error: Test failures
      Execution halted
    ```

# tcpl

<details>

* Version: 3.0.0
* GitHub: https://github.com/USEPA/CompTox-ToxCast-tcpl
* Source code: https://github.com/cran/tcpl
* Date/Publication: 2022-08-31 08:40:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "tcpl")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Assay_Registration.Rmd’ using rmarkdown
    tcpl (v3.0.0) loaded with the following settings:
      TCPL_DB:    /tmp/workdir/tcpl/new/tcpl.Rcheck/tcpl/csv
      TCPL_USER:  NA
      TCPL_HOST:  NA
      TCPL_DRVR:  tcplLite
    Default settings stored in tcpl config file. See ?tcplConf for more information.
    --- finished re-building ‘Assay_Registration.Rmd’
    
    ...
      TCPL_HOST:  NA
      TCPL_DRVR:  tcplLite
    Default settings stored in tcpl config file. See ?tcplConf for more information.
    --- finished re-building ‘Introduction_Appendices.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Data_processing.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# tvthemes

<details>

* Version: 1.3.1
* GitHub: https://github.com/Ryo-N7/tvthemes
* Source code: https://github.com/cran/tvthemes
* Date/Publication: 2022-07-19 06:10:02 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "tvthemes")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      thm$axis.ticks$size not equal to 0.15.
      target is NULL, current is numeric
      ── Failure ('test-spongebob.R:48'): theme_spongeBob works ──────────────────────
      thm$axis.ticks$size not equal to 0.15.
      target is NULL, current is numeric
      ── Failure ('test-thelastairbender.R:48'): theme_theLastAirbender works ────────
      thm$axis.ticks$size not equal to 0.15.
      target is NULL, current is numeric
      ── Failure ('test-thelastairbender.R:103'): theme_avatar works ─────────────────
      thm$axis.ticks$size not equal to 0.15.
      target is NULL, current is numeric
      
      [ FAIL 12 | WARN 43 | SKIP 1 | PASS 861 ]
      Error: Test failures
      Execution halted
    ```

# usmap

<details>

* Version: 0.6.0
* GitHub: https://github.com/pdil/usmap
* Source code: https://github.com/cran/usmap
* Date/Publication: 2022-02-27 17:10:05 UTC
* Number of recursive dependencies: 80

Run `cloud_details(, "usmap")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      plot_usmap(regions = "states")$layers not equal to plot_usmap(regions = "state")$layers.
      Component 1: Component 12: Component 5: Component 21: Component 11: 1 string mismatch
      Component 1: Component 12: Component 5: Component 21: Component 12: 1 string mismatch
      Component 2: Component 12: Component 5: Component 21: Component 11: 1 string mismatch
      Component 2: Component 12: Component 5: Component 21: Component 12: 1 string mismatch
      ── Failure ('test-plot.R:140'): singular regions can be used ───────────────────
      plot_usmap(regions = "counties")$layers not equal to plot_usmap(regions = "county")$layers.
      Component 1: Component 12: Component 5: Component 21: Component 11: 1 string mismatch
      Component 1: Component 12: Component 5: Component 21: Component 12: 1 string mismatch
      Component 2: Component 12: Component 5: Component 21: Component 11: 1 string mismatch
      Component 2: Component 12: Component 5: Component 21: Component 12: 1 string mismatch
      
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 163 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# xpose

<details>

* Version: 0.4.13
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2021-06-30 08:00:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_error(...) at test-xpose_save.R:33:2
        2. │ └─testthat:::quasi_capture(...)
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─xpose::xpose_save(plot = plot, file = paths_1)
        7.   └─ggplot2::ggsave(...)
        8.     └─ggplot2:::plot_dev(device, filename, dpi = dpi)
        9.       └─cli::cli_abort(...)
       10.         └─rlang::abort(...)
      
      [ FAIL 6 | WARN 4 | SKIP 7 | PASS 516 ]
      Error: Test failures
      Execution halted
    ```

