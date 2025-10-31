# adproclus

<details>

* Version: 2.0.0
* GitHub: https://github.com/henry-heppe/adproclus
* Source code: https://github.com/cran/adproclus
* Date/Publication: 2024-08-17 18:00:01 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "adproclus")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-visualize.R:52:9'): Scree plots low dimensional ──────────────
      Expected `plot_scree_adpc(model_selection, grid = TRUE)` to run without any conditions.
      i Actually got a <rlang_message> with text:
        `geom_line()`: Each group consists of only one observation.
        i Do you need to adjust the group aesthetic?
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 74 ]
      Error: Test failures
      Execution halted
    ```

# allMT

<details>

* Version: 0.1.0
* GitHub: https://github.com/tmungle/allMT
* Source code: https://github.com/cran/allMT
* Date/Publication: 2023-04-20 17:32:33 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "allMT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘allMT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_progression
    > ### Title: Graphical representation of maintenance therapy data for single
    > ###   patient
    > ### Aliases: plot_progression
    > 
    > ### ** Examples
    > 
    ...
    Maintenance therapy progression graph for the patient has been created
    Quitting
    Bye Bye: Did you know that Jupiter is biggest planet in our solar system :)?
    Warning in ggplot2::scale_y_continuous(trans = "log10", breaks = c(0, 0.5,  :
      log-10 transformation introduced infinite values.
    Warning in ggplot2::scale_y_continuous(trans = "log10", breaks = c(0, 0.5,  :
      log-10 transformation introduced infinite values.
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) compare_cohorts.Rd:35: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) compare_cohorts.Rd:36: Lost braces in \itemize; meant \describe ?
    ```

# braidReports

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/braidReports
* Date/Publication: 2025-09-03 18:30:08 UTC
* Number of recursive dependencies: 41

Run `revdepcheck::cloud_details(, "braidReports")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘braidReports-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: makeBraidReport
    > ### Title: Render a BRAID Report
    > ### Aliases: makeBraidReport
    > 
    > ### ** Examples
    > 
    > surface <- synergisticExample
    ...
    ! When `palette = NULL`, the `fallback.palette` must be defined.
    Backtrace:
        ▆
     1. └─braidReports::makeBraidReport(...)
     2.   └─braidReports:::recastFillScale(plotscales$color)
     3.     └─ggplot2::discrete_scale(...)
     4.       └─ggplot2:::check_fallback_palette(palette, fallback.palette, call = call)
     5.         └─cli::cli_abort("When {.code palette = NULL}, the {.arg fallback.palette} must be defined.")
     6.           └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘braidReports.Rmd’ using rmarkdown
    
    Quitting from braidReports.Rmd:82-86 [unnamed-chunk-4]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'braidReports.Rmd' failed with diagnostics:
    When `palette = NULL`, the `fallback.palette` must be defined.
    --- failed re-building ‘braidReports.Rmd’
    
    --- re-building ‘heatmaps.Rmd’ using rmarkdown
    ```

# bregr

<details>

* Version: 1.3.0
* GitHub: https://github.com/WangLabCSU/bregr
* Source code: https://github.com/cran/bregr
* Date/Publication: 2025-09-22 09:50:02 UTC
* Number of recursive dependencies: 219

Run `revdepcheck::cloud_details(, "bregr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bregr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: br_show_forest_ggstatsplot
    > ### Title: Show a forest plot with 'ggstatsplot' interface
    > ### Aliases: br_show_forest_ggstatsplot
    > 
    > ### ** Examples
    > 
    > if (rlang::is_installed("ggstats")) {
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_aesthetics(d, plot)
     14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
     15. │           └─ggplot2:::normalise_label(evaled$label)
     16. │             └─base::lapply(labels, `[`, 1)
     17. └─rlang (local) `<fn>`(`<ntSbsttE>`)
     18.   └─handlers[[1L]](cnd)
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

# bunching

<details>

* Version: 0.8.6
* GitHub: https://github.com/mavpanos/bunching
* Source code: https://github.com/cran/bunching
* Date/Publication: 2022-08-24 16:52:34 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::cloud_details(, "bunching")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bunching_examples.Rmd’ using rmarkdown
    
    tlmgr: Remote database at https://ctan.math.utah.edu/ctan/tex-archive/systems/texlive/tlnet
    (revision 76658 of the texlive-scripts package)
    seems to be older than the local installation
    (revision 76668 of texlive-scripts);
    please use a different mirror and/or wait a day or two.
    
    Warning in system2("tlmgr", args, ...) :
    ...
    
    Error: processing vignette 'bunching_theory.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/bunching/new/bunching.Rcheck/vign_test/bunching/vignettes/bunching_theory.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See bunching_theory.log for more info.
    --- failed re-building ‘bunching_theory.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘bunching_examples.Rmd’ ‘bunching_theory.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bunching_examples.Rmd’ using rmarkdown
    
    tlmgr: Remote database at https://mirrors.ibiblio.org/pub/mirrors/CTAN/systems/texlive/tlnet
    (revision 76658 of the texlive-scripts package)
    seems to be older than the local installation
    (revision 76668 of texlive-scripts);
    please use a different mirror and/or wait a day or two.
    
    Warning in system2("tlmgr", args, ...) :
    ...
    
    Error: processing vignette 'bunching_theory.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/bunching/old/bunching.Rcheck/vign_test/bunching/vignettes/bunching_theory.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See bunching_theory.log for more info.
    --- failed re-building ‘bunching_theory.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘bunching_examples.Rmd’ ‘bunching_theory.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# carbonr

<details>

* Version: 0.2.7
* GitHub: NA
* Source code: https://github.com/cran/carbonr
* Date/Publication: 2025-08-27 20:00:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "carbonr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(carbonr)
      > 
      > test_check("carbonr")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 147 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       28. │                         └─ggplot2:::normalise_label(evaled$label)
       29. │                           └─base::lapply(labels, `[`, 1)
       30. └─rlang (local) `<fn>`(`<ntSbsttE>`)
       31.   └─handlers[[1L]](cnd)
       32.     └─cli::cli_abort(...)
       33.       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 147 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 710 marked UTF-8 strings
    ```

# chest

<details>

* Version: 0.3.7
* GitHub: NA
* Source code: https://github.com/cran/chest
* Date/Publication: 2023-03-23 09:50:13 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "chest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘chest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: chest_plot
    > ### Title: Plot effect estimate and change-in-estimate values (ggplot type)
    > ### Aliases: chest_plot
    > 
    > ### ** Examples
    > 
    > vlist <- c("Age", "Sex", "Married", "Education", "Income")
    ...
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_position(d, layout)
     14.           └─ggplot2 (local) compute_position(..., self = self)
     15.             └─self$position$use_defaults(data, self$aes_params)
     16.               └─ggplot2 (local) use_defaults(..., self = self)
     17.                 └─ggplot2:::check_aesthetics(new, nrow(data))
     18.                   └─cli::cli_abort(...)
     19.                     └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘chest-vignette.Rmd’ using rmarkdown
    
    Quitting from chest-vignette.Rmd:58-65 [coxhp]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'chest-vignette.Rmd' failed with diagnostics:
    ...
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (6).
    ✖ Fix the following mappings: `nudge_x`.
    --- failed re-building ‘chest-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘chest-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# CNAIM

<details>

* Version: 2.1.4
* GitHub: https://github.com/Utiligize/CNAIM
* Source code: https://github.com/cran/CNAIM
* Date/Publication: 2022-08-31 08:40:22 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "CNAIM")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R      1.6Mb
        data   1.1Mb
        help   1.6Mb
    ```

# confidenceCurves

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/confidenceCurves
* Date/Publication: 2025-10-01 08:40:09 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "confidenceCurves")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘confidenceCurves-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: makeConfidenceCurves
    > ### Title: Frequentist confidence analysis for any treatment effect
    > ### Aliases: makeConfidenceCurves
    > 
    > ### ** Examples
    > 
    > makeConfidenceCurves(
    + theta.estimator = -0.22,
    + confidence.lower = -0.36,
    + confidence.upper = -0.07
    + )
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: makeConfidenceCurves -> <Anonymous> -> layer -> normalise_label -> lapply
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
          ▆
       1. └─confidenceCurves::makeConfidenceCurves(...) at test-confidenceCurves.R:2:1
       2.   └─ggplot2::annotate(...)
       3.     └─ggplot2::layer(...)
       4.       └─ggplot2:::normalise_label(aes_params$label)
       5.         └─base::lapply(labels, `[`, 1)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# ctsmTMB

<details>

* Version: 1.0.1
* GitHub: https://github.com/phillipbvetter/ctsmTMB
* Source code: https://github.com/cran/ctsmTMB
* Date/Publication: 2025-08-27 22:00:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "ctsmTMB")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ctsmTMB.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 31.3Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        libs  29.1Mb
    ```

# decisionSupport

<details>

* Version: 1.115
* GitHub: NA
* Source code: https://github.com/cran/decisionSupport
* Date/Publication: 2025-08-19 18:00:02 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "decisionSupport")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘decisionSupport-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: empirical_EVPI
    > ### Title: Expected value of perfect information (EVPI) for a simple model
    > ###   with the predictor variable sampled from a normal distribution with.
    > ### Aliases: empirical_EVPI summary.EVPI_res summary_empirical_EVPI
    > ###   plot.EVPI_res plot_empirical_EVPI
    > ### Keywords: "Value Information" of
    > 
    ...
     14. │         └─ggplot2 (local) finish_statistics(..., self = self)
     15. │           └─self$stat$finish_layer(data, self$computed_stat_params)
     16. │             └─ggplot2 (local) finish_layer(...)
     17. │               └─ggplot2::flipped_names(params$flipped_aes)
     18. └─base::.handleSimpleError(...)
     19.   └─rlang (local) h(simpleError(msg, call))
     20.     └─handlers[[1L]](cnd)
     21.       └─cli::cli_abort(...)
     22.         └─rlang::abort(...)
    Execution halted
    ```

# deeptime

<details>

* Version: 2.2.0
* GitHub: https://github.com/willgearty/deeptime
* Source code: https://github.com/cran/deeptime
* Date/Publication: 2025-06-19 19:10:07 UTC
* Number of recursive dependencies: 195

Run `revdepcheck::cloud_details(, "deeptime")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘deeptime-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_fill_geopattern
    > ### Title: Geologic pattern fill scale
    > ### Aliases: scale_fill_geopattern
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    Error in `check_fallback_palette()`:
    ! When `palette = NULL`, the `fallback.palette` must be defined.
    Backtrace:
        ▆
     1. └─deeptime::scale_fill_geopattern(name = NULL)
     2.   └─ggplot2::discrete_scale(...)
     3.     └─ggplot2:::check_fallback_palette(palette, fallback.palette, call = call)
     4.       └─cli::cli_abort("When {.code palette = NULL}, the {.arg fallback.palette} must be defined.")
     5.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(deeptime)
      > 
      > test_check("deeptime")
      NULL
      NULL
      NULL
    ...
      • patterns/scale-fill-geopattern-labels-new.svg
      • patterns/scale-fill-geopattern-limits-new.svg
      • patterns/scale-fill-geopattern-na-new.svg
      • patterns/scale-fill-geopattern-na2-new.svg
      • patterns/scale-fill-geopattern-new.svg
      • points_range/geom-points-range-aes-new.svg
      • points_range/geom-points-range-bg-new.svg
      • points_range/geom-points-range-h-new.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘coord.Rmd’ using rmarkdown
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggtree’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ggtree’
    ```

# effectplots

<details>

* Version: 0.2.2
* GitHub: https://github.com/mayer79/effectplots
* Source code: https://github.com/cran/effectplots
* Date/Publication: 2025-03-09 09:40:01 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "effectplots")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘effectplots-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: average_observed
    > ### Title: Average Observed
    > ### Aliases: average_observed
    > 
    > ### ** Examples
    > 
    > M <- average_observed(iris$Species, y = iris$Sepal.Length)
    ...
    'EffectData' object of length 1: 
    
         bin_mid bin_width   bin_mean  N weight y_mean      y_sd
    1     setosa       0.7     setosa 50     50  5.006 0.3524897
    2 versicolor       0.7 versicolor 50     50  5.936 0.5161711
    3  virginica       0.7  virginica 50     50  6.588 0.6358796
    > M |> plot()
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
       20.                                 └─scale$get_labels(breaks)
       21.                                   └─ggplot2 (local) get_labels(..., self = self)
       22.                                     └─self$scale$get_labels(breaks)
       23.                                       └─ggplot2 (local) get_labels(..., self = self)
       24.                                         └─ggplot2:::normalise_label(labels)
       25.                                           └─base::lapply(labels, `[`, 1)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 225 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘h2o’
    ```

# EIX

<details>

* Version: 1.2.0
* GitHub: https://github.com/ModelOriented/EIX
* Source code: https://github.com/cran/EIX
* Date/Publication: 2021-03-23 08:10:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "EIX")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EIX-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: importance
    > ### Title: Importance of variables and interactions in the model
    > ### Aliases: importance
    > 
    > ### ** Examples
    > 
    > library("EIX")
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_aesthetics(d, plot)
     14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
     15. │           └─ggplot2:::normalise_label(evaled$label)
     16. │             └─base::lapply(labels, `[`, 1)
     17. └─rlang (local) `<fn>`(`<ntSbsttE>`)
     18.   └─handlers[[1L]](cnd)
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘EIX.Rmd’ using rmarkdown
    ```

# fmeffects

<details>

* Version: 0.1.4
* GitHub: https://github.com/holgstr/fmeffects
* Source code: https://github.com/cran/fmeffects
* Date/Publication: 2024-11-05 18:50:02 UTC
* Number of recursive dependencies: 182

Run `revdepcheck::cloud_details(, "fmeffects")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fmeffects-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: came
    > ### Title: Computes a partitioning for a 'ForwardMarginalEffect'
    > ### Aliases: came
    > 
    > ### ** Examples
    > 
    > # Train a model and compute FMEs:
    ...
    
    PartitioningRpart of an FME object
    
    Method:  max.sd = 200
    
       n      cAME  SD(fME)  
     728  56.44523 165.7487 *
     340 -37.57207 108.1235  
     388 138.83153 163.6989  
    ---
    ```

## In both

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘fme_theory.Rmd’ using rmarkdown
    --- finished re-building ‘fme_theory.Rmd’
    
    --- re-building ‘fmeffects.Rmd’ using rmarkdown
    ```

# foqat

<details>

* Version: 2.0.8.2
* GitHub: https://github.com/tianshu129/foqat
* Source code: https://github.com/cran/foqat
* Date/Publication: 2023-09-30 06:10:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "foqat")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Air_Quality.Rmd’ using rmarkdown
    --- finished re-building ‘Air_Quality.Rmd’
    
    --- re-building ‘Atmospheric_Radiation.Rmd’ using rmarkdown
    --- finished re-building ‘Atmospheric_Radiation.Rmd’
    
    --- re-building ‘Basic_Functions.Rmd’ using rmarkdown
    --- finished re-building ‘Basic_Functions.Rmd’
    
    --- re-building ‘Particle_Size_Distribution.Rmd’ using rmarkdown
    ```

# frequency

<details>

* Version: 0.4.1
* GitHub: https://github.com/wilcoxa/frequency
* Source code: https://github.com/cran/frequency
* Date/Publication: 2021-01-11 14:00:03 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "frequency")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > Sys.setenv("R_TESTS" = "")
      > # Sys.setenv(LC_COLLATE = "C", LC_TIME = "C", LANGUAGE = "en")
      > Sys.setenv(LC_COLLATE = "C")
      > 
      > print(Sys.getlocale(category = "LC_ALL"))
      [1] "LC_CTYPE=C.UTF-8;LC_NUMERIC=C;LC_TIME=C.UTF-8;LC_COLLATE=C;LC_MONETARY=C.UTF-8;LC_MESSAGES=C.UTF-8;LC_PAPER=C.UTF-8;LC_NAME=C;LC_ADDRESS=C;LC_TELEPHONE=C;LC_MEASUREMENT=C.UTF-8;LC_IDENTIFICATION=C"
      > Sys.setlocale("LC_COLLATE", "C") # R CMD check uses this default
    ...
       59.                                                                 └─scale$get_labels(breaks)
       60.                                                                   └─ggplot2 (local) get_labels(..., self = self)
       61.                                                                     └─self$scale$get_labels(breaks)
       62.                                                                       └─ggplot2 (local) get_labels(..., self = self)
       63.                                                                         └─ggplot2:::normalise_label(labels)
       64.                                                                           └─base::lapply(labels, `[`, 1)
      
      [ FAIL 12 | WARN 1 | SKIP 2 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

# ggformula

<details>

* Version: 1.0.0
* GitHub: https://github.com/ProjectMOSAIC/ggformula
* Source code: https://github.com/cran/ggformula
* Date/Publication: 2025-10-06 05:10:25 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "ggformula")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from Rd file 'gf_lines.Rd':
    gf_abline
      Code: function(object = NULL, gformula = NULL, data = NULL, ...,
                     slope, intercept, color, linetype, linewidth, alpha,
                     xlab, ylab, title, subtitle, caption, stat =
                     "identity", show.legend = NA, show.help = NULL,
                     inherit = FALSE, environment = parent.frame())
      Docs: function(object = NULL, gformula = NULL, data = NULL, ...,
                     slope, intercept, color, linetype, linewidth, alpha,
                     xlab, ylab, title, subtitle, caption, show.legend =
    ...
                     xintercept, color, linetype, linewidth, alpha, xlab,
                     ylab, title, subtitle, caption, position = "identity",
                     show.legend = NA, show.help = NULL, inherit = FALSE,
                     environment = parent.frame())
      Argument names in code not in docs:
        stat
      Mismatches in argument names (first 3):
        Position: 15 Code: stat Docs: position
        Position: 16 Code: position Docs: show.legend
        Position: 17 Code: show.legend Docs: show.help
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    2.3Mb
        help   1.6Mb
    ```

# ggiraph

<details>

* Version: 0.9.2
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2025-10-07 15:00:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "ggiraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggiraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: guide_colourbar_interactive
    > ### Title: Create interactive continuous colour bar guide
    > ### Aliases: guide_colourbar_interactive guide_colorbar_interactive
    > 
    > ### ** Examples
    > 
    > # add interactive colourbar guide to a ggplot -------
    ...
    +           onclick = "alert(\"colourbar\")",
    +           tooltip = paste0("colourbar", abreak)
    +         )
    +       })
    +     }
    +   )
    > x <- girafe(ggobj = p3)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: girafe ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > if (requireNamespace("tinytest", quietly = TRUE)) {
      +   tinytest::test_package("ggiraph")
      + }
      
      test-annotate_interactive.R...    0 tests    
      test-annotate_interactive.R...    0 tests    
      test-annotate_interactive.R...    0 tests    
    ...
      test-guide_bins_interactive.R.   14 tests [0;32mOK[0m [0;34m0.9s[0m
      
      test-guide_legend_interactive.R    0 tests    
      test-guide_legend_interactive.R    0 tests    
      test-guide_legend_interactive.R    0 tests    
      test-guide_legend_interactive.R    0 tests    
      test-guide_legend_interactive.R    0 tests    
      test-guide_legend_interactive.R    2 tests [0;32mOK[0m Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
      Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        libs   5.2Mb
    ```

# ggparty

<details>

* Version: 1.0.0.1
* GitHub: https://github.com/martin-borkovec/ggparty
* Source code: https://github.com/cran/ggparty
* Date/Publication: 2025-07-10 16:31:21 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "ggparty")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggparty-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.party
    > ### Title: autoplot methods for party objects
    > ### Aliases: autoplot.party autoplot.constparty autoplot.modelparty
    > ###   autoplot.lmtree
    > 
    > ### ** Examples
    > 
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_aesthetics(d, plot)
     14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
     15. │           └─ggplot2:::normalise_label(evaled$label)
     16. │             └─base::lapply(labels, `[`, 1)
     17. └─rlang (local) `<fn>`(`<ntSbsttE>`)
     18.   └─handlers[[1L]](cnd)
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggparty)
      Loading required package: ggplot2
      Loading required package: partykit
      Loading required package: grid
      Loading required package: libcoin
      Loading required package: mvtnorm
    ...
       19. │                 └─ggplot2:::normalise_label(evaled$label)
       20. │                   └─base::lapply(labels, `[`, 1)
       21. └─rlang (local) `<fn>`(`<ntSbsttE>`)
       22.   └─handlers[[1L]](cnd)
       23.     └─cli::cli_abort(...)
       24.       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 94 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggparty-graphic-partying.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘survival’
      All declared Imports should be used.
    ```

# ggplot2.utils

<details>

* Version: 0.3.3
* GitHub: https://github.com/insightsengineering/ggplot2.utils
* Source code: https://github.com/cran/ggplot2.utils
* Date/Publication: 2025-07-09 09:10:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ggplot2.utils")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggplot2.utils)
      Loading required package: ggplot2
      > 
      > test_check("ggplot2.utils")
      
      Attaching package: 'dplyr'
    ...
       26. │               └─ggplot2:::normalise_label(evaled$label)
       27. │                 └─base::lapply(labels, `[`, 1)
       28. └─rlang (local) `<fn>`(`<ntSbsttE>`)
       29.   └─handlers[[1L]](cnd)
       30.     └─cli::cli_abort(...)
       31.       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 7 | PASS 40 ]
      Error: Test failures
      Execution halted
    ```

# ggpp

<details>

* Version: 0.5.9
* GitHub: https://github.com/aphalo/ggpp
* Source code: https://github.com/cran/ggpp
* Date/Publication: 2025-06-28 04:40:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "ggpp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotate
    > ### Title: Annotations supporting NPC
    > ### Aliases: annotate
    > 
    > ### ** Examples
    > 
    > 
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_aesthetics(d, plot)
     14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
     15. │           └─ggplot2:::normalise_label(evaled$label)
     16. │             └─base::lapply(labels, `[`, 1)
     17. └─rlang (local) `<fn>`(`<ntSbsttE>`)
     18.   └─handlers[[1L]](cnd)
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggpp)
      Loading required package: ggplot2
      Registered S3 methods overwritten by 'ggpp':
        method                  from   
        heightDetails.titleGrob ggplot2
        widthDetails.titleGrob  ggplot2
    ...
      * stat_fmt_tb/stat-fmt-tb-2.svg
      * stat_fmt_tb/stat-fmt-tb-3.svg
      * stat_fmt_tb/stat-fmt-tb-4.svg
      * stat_panel_counts/stat-group-counts-x.svg
      * stat_panel_counts/stat-group-counts-xy-color.svg
      * stat_panel_counts/stat-group-counts-y.svg
      * stat_panel_counts/stat-panel-counts-x.svg
      * stat_panel_counts/stat-panel-counts-y.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘grammar-extensions.Rmd’ using rmarkdown
    ```

# ggprism

<details>

* Version: 1.0.7
* GitHub: https://github.com/csdaw/ggprism
* Source code: https://github.com/cran/ggprism
* Date/Publication: 2025-08-23 17:50:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "ggprism")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggprism-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_pvalue
    > ### Title: Add p-values to a ggplot
    > ### Aliases: add_pvalue
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(...)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("ggprism")
      + }
      
      test-add_pvalue.R.............    0 tests    
      test-add_pvalue.R.............    0 tests    
    ...
       20.                   │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       21.                   │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
       22.                   │ └─base::withCallingHandlers(...)
       23.                   └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
       24.                     └─l$compute_geom_1(d)
       25.                       └─ggplot2 (local) compute_geom_1(..., self = self)
       26.                         └─ggplot2:::check_required_aesthetics(...)
       27.                           └─cli::cli_abort(...)
       28.                             └─rlang::abort(...)
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘axes.Rmd’ using rmarkdown
    ```

# ggpubr

<details>

* Version: 0.6.2
* GitHub: https://github.com/kassambara/ggpubr
* Source code: https://github.com/cran/ggpubr
* Date/Publication: 2025-10-17 05:10:16 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "ggpubr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpubr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: stat_bracket
    > ### Title: Add Brackets with Labels to a GGPlot
    > ### Aliases: stat_bracket geom_bracket
    > 
    > ### ** Examples
    > 
    > df <- ToothGrowth
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(...)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggpubr)
      Loading required package: ggplot2
      > 
      > test_check("ggpubr")
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 216 ]
      
    ...
      ℹ Actually got a <rlang_error> with text:
        Problem while setting up geom.
        ℹ Error occurred in the 2nd layer.
        Caused by error in `compute_geom_1()`:
        ! `geom_bracket()` requires the following missing aesthetics:
          annotation.
      
      [ FAIL 2 | WARN 3 | SKIP 0 | PASS 216 ]
      Error: Test failures
      Execution halted
    ```

# ggside

<details>

* Version: 0.4.0
* GitHub: https://github.com/jtlandis/ggside
* Source code: https://github.com/cran/ggside
* Date/Publication: 2025-09-13 05:10:41 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ggside")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from Rd file 'geom_xsideabline.Rd':
    geom_xsideabline
      Code: function(mapping = NULL, data = NULL, stat = "identity", ...,
                     slope, intercept, na.rm = FALSE, show.legend = NA,
                     inherit.aes = FALSE)
      Docs: function(mapping = NULL, data = NULL, ..., slope, intercept,
                     na.rm = FALSE, show.legend = NA)
      Argument names in code not in docs:
        stat inherit.aes
      Mismatches in argument names (first 3):
    ...
                     position = "identity", ..., xintercept, na.rm = FALSE,
                     show.legend = NA, inherit.aes = FALSE)
      Docs: function(mapping = NULL, data = NULL, position = "identity",
                     ..., xintercept, na.rm = FALSE, show.legend = NA)
      Argument names in code not in docs:
        stat inherit.aes
      Mismatches in argument names (first 3):
        Position: 3 Code: stat Docs: position
        Position: 4 Code: position Docs: ...
        Position: 5 Code: ... Docs: xintercept
    ```

# ggspectra

<details>

* Version: 0.3.17
* GitHub: https://github.com/aphalo/ggspectra
* Source code: https://github.com/cran/ggspectra
* Date/Publication: 2025-09-24 18:20:09 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "ggspectra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggspectra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.object_spct
    > ### Title: Plot one or more "object" spectra.
    > ### Aliases: autoplot.object_spct autoplot.object_mspct
    > 
    > ### ** Examples
    > 
    > autoplot(Ler_leaf.spct)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

# ggstatsplot

<details>

* Version: 0.13.3
* GitHub: https://github.com/IndrajeetPatil/ggstatsplot
* Source code: https://github.com/cran/ggstatsplot
* Date/Publication: 2025-10-05 11:00:02 UTC
* Number of recursive dependencies: 173

Run `revdepcheck::cloud_details(, "ggstatsplot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # graphics engine changed in this version, and so snapshots generated on
      > # previous R version won't work
      > if (getRversion() >= "4.4.0") {
      +   library(testthat)
      +   suppressPackageStartupMessages(library(ggstatsplot))
      +   test_check("ggstatsplot")
      + }
    ...
      • pairwise-ggsignif/within-non-parametric-all.svg
      • pairwise-ggsignif/within-non-parametric-only-non-significant.svg
      • pairwise-ggsignif/within-non-parametric-only-significant.svg
      • pairwise-ggsignif/within-parametric-all.svg
      • pairwise-ggsignif/within-parametric-only-significant.svg
      • pairwise-ggsignif/within-robust-all.svg
      • pairwise-ggsignif/within-robust-only-non-significant.svg
      • pairwise-ggsignif/within-robust-only-significant.svg
      Error: Test failures
      Execution halted
    ```

# ggsurveillance

<details>

* Version: 0.5.1
* GitHub: https://github.com/biostats-dev/ggsurveillance
* Source code: https://github.com/cran/ggsurveillance
* Date/Publication: 2025-07-02 10:00:09 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "ggsurveillance")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
    ...
      • stat_last_value/3-geom-label-last-value-nudge.svg
      • stat_last_value/4-geom-text-last-value-repel-min-segment.svg
      • stat_last_value/5-geom-label-last-value-repel-custom.svg
      • stat_last_value/6-stat-last-value-abs-nudge-date.svg
      • stat_last_value/7-stat-last-value-na-at-end.svg
      Error: Test failures
      In addition: Warning message:
      In Sys.setlocale("LC_ALL", "en_GB.UTF-8") :
        OS reports request to set locale to "en_GB.UTF-8" cannot be honored
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 364 marked UTF-8 strings
    ```

# GimmeMyPlot

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/GimmeMyPlot
* Date/Publication: 2023-10-18 16:10:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "GimmeMyPlot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GimmeMyPlot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_violin
    > ### Title: Violin plot
    > ### Aliases: plot_violin
    > 
    > ### ** Examples
    > 
    > library(RColorBrewer)
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(...)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Tutorial.Rmd’ using rmarkdown
    
    Quitting from Tutorial.Rmd:23-57 [violin]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'Tutorial.Rmd' failed with diagnostics:
    ...
    Caused by error in `compute_geom_1()`:
    ! `geom_bracket()` requires the following missing aesthetics:
      annotation.
    --- failed re-building ‘Tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# gluvarpro

<details>

* Version: 7.0
* GitHub: NA
* Source code: https://github.com/cran/gluvarpro
* Date/Publication: 2022-10-01 11:00:02 UTC
* Number of recursive dependencies: 33

Run `revdepcheck::cloud_details(, "gluvarpro")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gluvarpro-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotgvp
    > ### Title: plot glucose values and glucose variability measures
    > ### Aliases: plotgvp
    > 
    > ### ** Examples
    > 
    > data("datagvp1")
    > plotgvp(datagvp1)
    Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    ℹ The deprecated feature was likely used in the gluvarpro package.
      Please report the issue to the authors.
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: plotgvp
    Execution halted
    ```

# gosset

<details>

* Version: 1.4
* GitHub: https://github.com/agrdatasci/gosset
* Source code: https://github.com/cran/gosset
* Date/Publication: 2024-12-05 14:00:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "gosset")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘vignette-1-trait-prioritization-and-crop-performance.Rmd’ using rmarkdown_notangle
    ```

# infer

<details>

* Version: 1.0.9
* GitHub: https://github.com/tidymodels/infer
* Source code: https://github.com/cran/infer
* Date/Publication: 2025-06-26 17:50:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "infer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      • visualize/viz-assume-z-p-val-right.svg
      • visualize/viz-assume-z.svg
      • visualize/viz-fit-bare.svg
      • visualize/viz-fit-conf-int.svg
      • visualize/viz-fit-no-h0.svg
      • visualize/viz-fit-p-val-both.svg
      • visualize/viz-fit-p-val-left.svg
      • visualize/viz-fit-p-val-right.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘anova.Rmd’ using rmarkdown
    ```

# latex2exp

<details>

* Version: 0.9.6
* GitHub: https://github.com/stefano-meschiari/latex2exp
* Source code: https://github.com/cran/latex2exp
* Date/Publication: 2022-11-28 03:30:02 UTC
* Number of recursive dependencies: 66

Run `revdepcheck::cloud_details(, "latex2exp")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘supported-commands.Rmd’ using rmarkdown
    --- finished re-building ‘supported-commands.Rmd’
    
    --- re-building ‘using-latex2exp.Rmd’ using rmarkdown
    ```

# maraca

<details>

* Version: 1.0.1
* GitHub: https://github.com/AstraZeneca/maraca
* Source code: https://github.com/cran/maraca
* Date/Publication: 2025-07-28 15:20:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "maraca")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘maraca-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.hce
    > ### Title: Generic function to plot the hce object using plot().
    > ### Aliases: plot.hce
    > 
    > ### ** Examples
    > 
    > Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
    > Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
    > hce_dat <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
    +              CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
    +              seed = 31337)
    > plot(hce_dat)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: plot ... plot_maraca -> <Anonymous> -> layer -> normalise_label -> lapply
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘faq.Rmd’ using rmarkdown
    
    Quitting from faq.Rmd:34-64 [unnamed-chunk-1]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `FUN()`:
    ! object of type 'symbol' is not subsettable
    ---
    Backtrace:
    ...
     5.       └─ggplot2::layer(...)
     6.         └─ggplot2:::normalise_label(aes_params$label)
     7.           └─base::lapply(labels, `[`, 1)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'maraca.Rmd' failed with diagnostics:
    object of type 'symbol' is not subsettable
    --- failed re-building ‘maraca.Rmd’
    
    --- re-building ‘mosaic.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        doc   7.8Mb
    ```

# MetAlyzer

<details>

* Version: 1.1.0
* GitHub: https://github.com/nilsmechtel/MetAlyzer
* Source code: https://github.com/cran/MetAlyzer
* Date/Publication: 2024-12-06 14:00:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "MetAlyzer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MetAlyzer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotly_scatter
    > ### Title: Plotly Log2FC Scatter Plot
    > ### Aliases: plotly_scatter
    > 
    > ### ** Examples
    > 
    > 
    ...
    Warning: Partial NA coefficients for 2 probe(s)
    > 
    > p_scatter <- plotly_scatter(metalyzer_se)
    Warning in geom_rect(data = rects_df, inherit.aes = FALSE, aes(xmin = .data$Start,  :
      Ignoring unknown aesthetics: text
    Warning in geom_point(size = 0.5, aes(text = paste0(.data$Metabolite, "\nClass: ",  :
      Ignoring unknown aesthetics: text
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: plotly_scatter ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MetAlyzer_User_Guide.Rmd’ using rmarkdown
    ```

# metrica

<details>

* Version: 2.1.0
* GitHub: https://github.com/adriancorrendo/metrica
* Source code: https://github.com/cran/metrica
* Date/Publication: 2024-06-30 14:20:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "metrica")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Cheatsheet.Rmd’ using rmarkdown
    --- finished re-building ‘Cheatsheet.Rmd’
    
    --- re-building ‘JOSS_publication.Rmd’ using rmarkdown
    --- finished re-building ‘JOSS_publication.Rmd’
    
    --- re-building ‘Shinyapp.Rmd’ using rmarkdown
    --- finished re-building ‘Shinyapp.Rmd’
    
    ...
    --- re-building ‘apsim_open.Rmd’ using rmarkdown
    --- finished re-building ‘apsim_open.Rmd’
    
    --- re-building ‘available_metrics_classification.Rmd’ using rmarkdown
    --- finished re-building ‘available_metrics_classification.Rmd’
    
    --- re-building ‘available_metrics_regression.Rmd’ using rmarkdown
    --- finished re-building ‘available_metrics_regression.Rmd’
    
    --- re-building ‘classification_case.Rmd’ using rmarkdown
    ```

# microbial

<details>

* Version: 0.0.22
* GitHub: NA
* Source code: https://github.com/cran/microbial
* Date/Publication: 2025-10-06 12:30:02 UTC
* Number of recursive dependencies: 180

Run `revdepcheck::cloud_details(, "microbial")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘microbial-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotalpha
    > ### Title: plot alpha diversity
    > ### Aliases: plotalpha
    > 
    > ### ** Examples
    > 
    > {
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(...)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘microbial.Rmd’ using knitr
    ```

# mistr

<details>

* Version: 0.0.6
* GitHub: NA
* Source code: https://github.com/cran/mistr
* Date/Publication: 2023-02-22 15:20:03 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "mistr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘mistr-extensions.Rmd’ using rmarkdown
    tlmgr: package repository https://mirrors.rit.edu/CTAN/systems/texlive/tlnet (verified)
    [1/1, ??:??/??:??] install: extsizes [12k]
    running mktexlsr ...
    done running mktexlsr.
    tlmgr: package log updated: /opt/TinyTeX/texmf-var/web2c/tlmgr.log
    tlmgr: command log updated: /opt/TinyTeX/texmf-var/web2c/tlmgr-commands.log
    ! Package babel Error: Unknown option 'english'.
    (babel)                Suggested actions:
    ...
    
    Error: processing vignette 'mistr-introduction.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/mistr/new/mistr.Rcheck/vign_test/mistr/vignettes/mistr-introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See mistr-introduction.log for more info.
    --- failed re-building ‘mistr-introduction.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘mistr-extensions.Rmd’ ‘mistr-introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘mistr-extensions.Rmd’ using rmarkdown
    
    tlmgr: Remote database at https://mirror.clarkson.edu/ctan/systems/texlive/tlnet
    (revision 76658 of the texlive-scripts package)
    seems to be older than the local installation
    (revision 76668 of texlive-scripts);
    please use a different mirror and/or wait a day or two.
    
    Warning in system2("tlmgr", args, ...) :
    ...
    
    Error: processing vignette 'mistr-introduction.Rmd' failed with diagnostics:
    LaTeX failed to compile /tmp/workdir/mistr/old/mistr.Rcheck/vign_test/mistr/vignettes/mistr-introduction.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See mistr-introduction.log for more info.
    --- failed re-building ‘mistr-introduction.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘mistr-extensions.Rmd’ ‘mistr-introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mlr3viz

<details>

* Version: 0.10.1
* GitHub: https://github.com/mlr-org/mlr3viz
* Source code: https://github.com/cran/mlr3viz
* Date/Publication: 2025-01-16 16:40:02 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "mlr3viz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mlr3viz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.LearnerClassifRpart
    > ### Title: Plots for Rpart Learners
    > ### Aliases: autoplot.LearnerClassifRpart autoplot.LearnerRegrRpart
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("mlr3")) {
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_aesthetics(d, plot)
     14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
     15. │           └─ggplot2:::normalise_label(evaled$label)
     16. │             └─base::lapply(labels, `[`, 1)
     17. └─rlang (local) `<fn>`(`<ntSbsttE>`)
     18.   └─handlers[[1L]](cnd)
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (requireNamespace("testthat", quietly = TRUE)) {
      +   library("testthat")
      +   library("mlr3viz")
      +   test_check("mlr3viz")
      + }
      Starting 2 test processes
      [ FAIL 2 | WARN 78 | SKIP 27 | PASS 90 ]
    ...
      • TuningInstanceSingleCrit/tisc-surface-grid-50.svg
      • TuningInstanceSingleCrit/tisc-surface-regr-lm.svg
      • TuningInstanceSingleCrit/tisc-surface.svg
      • plot_learner_prediction/learner-prediction-1d-se.svg
      • plot_learner_prediction/learner-prediction-binary-prob.svg
      • plot_learner_prediction/learner-prediction-binary-response.svg
      • plot_learner_prediction/learner-prediction-categorical.svg
      • plot_learner_prediction/learner-prediction-prob.svg
      Error: Test failures
      Execution halted
    ```

# mvGPS

<details>

* Version: 1.2.2
* GitHub: https://github.com/williazo/mvGPS
* Source code: https://github.com/cran/mvGPS
* Date/Publication: 2021-12-07 08:20:15 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "mvGPS")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘mvGPS-intro.Rmd’ using rmarkdown
    
    Quitting from mvGPS-intro.Rmd:32-52 [dag_draw]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `FUN()`:
    ! object of type 'symbol' is not subsettable
    ---
    Backtrace:
    ...
    
    Error: processing vignette 'mvGPS-intro.Rmd' failed with diagnostics:
    object of type 'symbol' is not subsettable
    --- failed re-building ‘mvGPS-intro.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘mvGPS-intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rdpack’
      All declared Imports should be used.
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) bal.Rd:156: Escaped LaTeX specials: \&
    ```

# nett

<details>

* Version: 1.0.0
* GitHub: https://github.com/aaamini/nett
* Source code: https://github.com/cran/nett
* Date/Publication: 2022-11-09 10:50:05 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "nett")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Community_Detection.Rmd’ using rmarkdown
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) spec_clust.Rd:33: Lost braces; missing escapes or markup?
        33 | A label vector of size n x 1 with elements in {1,2,...,K}
           |                                               ^
    ```

# NMF

<details>

* Version: 0.28
* GitHub: NA
* Source code: https://github.com/cran/NMF
* Date/Publication: 2024-08-22 16:20:01 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "NMF")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NMF-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: profplot
    > ### Title: Plotting Expression Profiles
    > ### Aliases: profplot profplot.default
    > ### Keywords: aplot
    > 
    > ### ** Examples
    > 
    ...
    > profplot(res, res2)
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the NMF package.
      Please report the issue to the authors.
    `geom_smooth()` using formula = 'y ~ x'
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘NMF-vignette.Rnw’ using knitr
    Error in citation(x) : there is no package called 'doMC'
    Converted 9 of 10 package citations to BibTeX
    Writing 11 Bibtex entries ... OK
    Results written to file 'Rpackages.bib'
    Error: processing vignette 'NMF-vignette.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'NMF-vignette.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `a4wide.sty' not found.
    ...
    l.62 \usepackage
                    {xspace}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘heatmaps.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘NMF-vignette.Rnw’ ‘heatmaps.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photobiologyPlants

<details>

* Version: 0.6.1-1
* GitHub: https://github.com/aphalo/photobiologyplants
* Source code: https://github.com/cran/photobiologyPlants
* Date/Publication: 2025-10-04 15:40:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "photobiologyPlants")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘r4p-introduction.Rmd’ using rmarkdown
    --- finished re-building ‘r4p-introduction.Rmd’
    
    --- re-building ‘user-guide.Rmd’ using rmarkdown
    ```

# PTXQC

<details>

* Version: 1.1.3
* GitHub: https://github.com/cbielow/PTXQC
* Source code: https://github.com/cran/PTXQC
* Date/Publication: 2025-07-15 12:30:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "PTXQC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PTXQC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_MBRAlign
    > ### Title: Plot MaxQuant Match-between-runs alignment performance.
    > ### Aliases: plot_MBRAlign
    > 
    > ### ** Examples
    > 
    > 
    ...
    >  data = data.frame(fc.raw.file_ext = "file A", ## more than one would be possible
    +                    calibrated.retention.time = c(20:100), 
    +                    retention.time.calibration = 6 + sin((20:100)/10))
    >  data$rtdiff = rnorm(nrow(data))
    >  data$RTdiff_in = c("green", "red")[1 + (abs(data$rtdiff) > 0.7)]
    >  
    >  plot_MBRAlign(data, c(-10, 10), "fancy subtitle", 0.7)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(PTXQC)
      Loading package PTXQC (version 1.1.3)
      > 
      > ##
      > ## calls all code in PTXQC/tests/testthat/test*.R
      > ##
    ...
       43.                         └─self$extract_key(...)
       44.                           └─ggplot2 (local) extract_key(...)
       45.                             └─scale$get_labels(breaks)
       46.                               └─ggplot2 (local) get_labels(..., self = self)
       47.                                 └─ggplot2:::normalise_label(labels)
       48.                                   └─base::lapply(labels, `[`, 1)
      
      [ FAIL 1 | WARN 26 | SKIP 0 | PASS 131 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.5Mb
      sub-directories of 1Mb or more:
        R          1.5Mb
        doc        4.0Mb
        examples   2.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rlang’
      All declared Imports should be used.
    ```

# QurvE

<details>

* Version: 1.1.2
* GitHub: https://github.com/NicWir/QurvE
* Source code: https://github.com/cran/QurvE
* Date/Publication: 2025-09-19 13:40:13 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "QurvE")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘QurvE-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: flFitSpline
    > ### Title: Perform a smooth spline fit on fluorescence data
    > ### Aliases: flFitSpline
    > 
    > ### ** Examples
    > 
    > # load example dataset
    ...
    > # Perform linear fit
    > TestFit <- flFitSpline(time = time,
    +                        fl_data = data,
    +                        ID = 'TestFit',
    +                        control = fl.control(fit.opt = 's', x_type = 'time'))
    > 
    > plot(TestFit)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: plot ... plot.flFitSpline -> annotate -> layer -> normalise_label -> lapply
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        R           1.5Mb
        doc         2.1Mb
        shiny_app   1.2Mb
    ```

# saros

<details>

* Version: 1.5.4
* GitHub: https://github.com/NIFU-NO/saros
* Source code: https://github.com/cran/saros
* Date/Publication: 2025-06-04 12:10:06 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "saros")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘saros-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: girafe
    > ### Title: Pull global plotting settings before displaying plot
    > ### Aliases: girafe
    > 
    > ### ** Examples
    > 
    > plot <- makeme(data = ex_survey, dep = b_1)
    ...
    ℹ Did you misspell an argument name?
    Warning in (function (title = waiver(), theme = NULL, position = NULL, direction = NULL,  :
      Arguments in `...` must be used.
    ✖ Problematic argument:
    • data_id = "fill.guide"
    ℹ Did you misspell an argument name?
    > girafe(plot)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: girafe ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

# scales

<details>

* Version: 1.4.0
* GitHub: https://github.com/r-lib/scales
* Source code: https://github.com/cran/scales
* Date/Publication: 2025-04-24 11:00:02 UTC
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "scales")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scales-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: label_number_auto
    > ### Title: Label numbers, avoiding scientific notation where possible
    > ### Aliases: label_number_auto
    > 
    > ### ** Examples
    > 
    > # Very small and very large numbers get scientific notation
    ...
    > demo_continuous(c(0, 1e6), labels = label_number_auto())
    scale_x_continuous(labels = label_number_auto())
    > 
    > # Transformation is applied individually so you get as little
    > # scientific notation as possible
    > demo_log10(c(1, 1e7), labels = label_number_auto())
    scale_x_log10(labels = label_number_auto())
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

# scGate

<details>

* Version: 1.7.2
* GitHub: https://github.com/carmonalab/scGate
* Source code: https://github.com/cran/scGate
* Date/Publication: 2025-07-23 10:50:02 UTC
* Number of recursive dependencies: 178

Run `revdepcheck::cloud_details(, "scGate")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scGate-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_tree
    > ### Title: Plot model tree
    > ### Aliases: plot_tree
    > 
    > ### ** Examples
    > 
    > library(ggparty)
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_aesthetics(d, plot)
     14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
     15. │           └─ggplot2:::normalise_label(evaled$label)
     16. │             └─base::lapply(labels, `[`, 1)
     17. └─rlang (local) `<fn>`(`<ntSbsttE>`)
     18.   └─handlers[[1L]](cnd)
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

# sigminer

<details>

* Version: 2.3.1
* GitHub: https://github.com/ShixiangWang/sigminer
* Source code: https://github.com/cran/sigminer
* Date/Publication: 2024-05-11 08:50:02 UTC
* Number of recursive dependencies: 212

Run `revdepcheck::cloud_details(, "sigminer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sigminer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_adj_p
    > ### Title: Get Adjust P Values from Group Comparison
    > ### Aliases: get_adj_p
    > 
    > ### ** Examples
    > 
    > library(ggpubr)
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(...)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        R         1.5Mb
        data      2.0Mb
        extdata   1.0Mb
        help      1.7Mb
        libs      1.3Mb
    ```

# sjPlot

<details>

* Version: 2.9.0
* GitHub: https://github.com/strengejacke/sjPlot
* Source code: https://github.com/cran/sjPlot
* Date/Publication: 2025-07-10 19:00:05 UTC
* Number of recursive dependencies: 195

Run `revdepcheck::cloud_details(, "sjPlot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sjPlot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sjp.poly
    > ### Title: Plot polynomials for (generalized) linear regression
    > ### Aliases: sjp.poly
    > 
    > ### ** Examples
    > 
    > library(sjmisc)
    ...
    
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the sjPlot package.
      Please report the issue at <https://github.com/strengejacke/sjPlot/issues>.
    `geom_smooth()` using formula = 'y ~ x'
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

# starvz

<details>

* Version: 0.8.3
* GitHub: https://github.com/schnorr/starvz
* Source code: https://github.com/cran/starvz
* Date/Publication: 2025-06-18 22:00:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "starvz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘starvz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: panel_memory_state
    > ### Title: Create a memory state space time
    > ### Aliases: panel_memory_state
    > 
    > ### ** Examples
    > 
    > panel_memory_state(data = starvz_sample_lu)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        libs   3.9Mb
    ```

# StratifiedMedicine

<details>

* Version: 1.0.5
* GitHub: https://github.com/thomasjemielita/StratifiedMedicine
* Source code: https://github.com/cran/StratifiedMedicine
* Date/Publication: 2022-03-29 23:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "StratifiedMedicine")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘SM_PRISM.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc   5.0Mb
    ```

# tcgaViz

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/tcgaViz
* Date/Publication: 2023-04-04 15:40:02 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "tcgaViz")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Tutorial.Rmd’ using rmarkdown
    
    Quitting from Tutorial.Rmd:34-43 [plot]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    Error: processing vignette 'Tutorial.Rmd' failed with diagnostics:
    ...
    Caused by error in `compute_geom_1()`:
    ! `geom_bracket()` requires the following missing aesthetics:
      annotation.
    --- failed re-building ‘Tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# testcorr

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/testcorr
* Date/Publication: 2025-06-12 17:30:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "testcorr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘testcorr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: iid.test
    > ### Title: Testing iid property
    > ### Aliases: iid.test
    > 
    > ### ** Examples
    > 
    > x <- rnorm(100)
    > iid.test(x, max.lag = 10)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: iid.test ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘testcorr.Rnw’ using Sweave
    Error: processing vignette 'testcorr.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'testcorr.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `thumbpdf.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.13 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘testcorr.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘testcorr.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# vecmatch

<details>

* Version: 1.2.0
* GitHub: https://github.com/Polymerase3/vecmatch
* Source code: https://github.com/cran/vecmatch
* Date/Publication: 2025-07-08 12:00:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "vecmatch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vecmatch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: estimate_gps
    > ### Title: Calculate treatment allocation probabilities
    > ### Aliases: estimate_gps
    > 
    > ### ** Examples
    > 
    > 
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(...)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘optimizing-matching.Rmd’ using rmarkdown
    --- finished re-building ‘optimizing-matching.Rmd’
    
    --- re-building ‘vecmatch.Rmd’ using rmarkdown
    
    Quitting from vecmatch.Rmd:50-63 [unnamed-chunk-2]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NULL
    ...
    Caused by error in `compute_geom_1()`:
    ! `geom_bracket()` requires the following missing aesthetics:
      annotation.
    --- failed re-building ‘vecmatch.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vecmatch.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘MatchIt’
    ```

# voiceR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/voiceR
* Date/Publication: 2023-09-12 20:30:02 UTC
* Number of recursive dependencies: 191

Run `revdepcheck::cloud_details(, "voiceR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘voiceR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: comparisonPlots
    > ### Title: Create boxplots for extracted audio features
    > ### Aliases: comparisonPlots
    > 
    > ### ** Examples
    > 
    > comparisonPlots(testAudioData, by = "Condition")
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(...)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        Audios   1.1Mb
        data     6.5Mb
    ```

# volcano3D

<details>

* Version: 2.0.9
* GitHub: https://github.com/KatrionaGoldmann/volcano3D
* Source code: https://github.com/cran/volcano3D
* Date/Publication: 2023-05-17 11:00:02 UTC
* Number of recursive dependencies: 168

Run `revdepcheck::cloud_details(, "volcano3D")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘volcano3D-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: boxplot_trio
    > ### Title: Boxplot to compare groups
    > ### Aliases: boxplot_trio
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
      9.       │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     10.       │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_1(d)
     14.           └─ggplot2 (local) compute_geom_1(..., self = self)
     15.             └─ggplot2:::check_required_aesthetics(...)
     16.               └─cli::cli_abort(...)
     17.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Vignette.rmd’ using rmarkdown
    ```

# wql

<details>

* Version: 1.0.3
* GitHub: https://github.com/jsta/wql
* Source code: https://github.com/cran/wql
* Date/Publication: 2025-09-02 13:10:02 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "wql")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wql-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: seasonTrend
    > ### Title: Determine seasonal trends
    > ### Aliases: seasonTrend
    > ### Keywords: Graphics ts
    > 
    > ### ** Examples
    > 
    ...
    187    s36      7 0.09100000   0.028716892 1.763604e-01 0.694
    188    s36      8 0.19594203   0.052359457 1.081614e-01 0.714
    189    s36      9 0.27897727   0.069193483 1.649011e-03 0.633
    190    s36     10 0.28311741   0.075931996 4.479054e-03 0.694
    191    s36     11 0.26103896   0.099035285 1.276679e-04 0.633
    192    s36     12 0.22566667   0.082398303 1.734336e-02 0.796
    > seasonTrend(x, plot = TRUE, ncol = 4)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘wql-package.Rmd’ using rmarkdown
    ```

# WRTDStidal

<details>

* Version: 1.1.4
* GitHub: https://github.com/fawda123/WRTDStidal
* Source code: https://github.com/cran/WRTDStidal
* Date/Publication: 2023-10-20 09:00:11 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "WRTDStidal")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘WRTDStidal-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fitmoplot
    > ### Title: Plot the fitted results for a tidal object by month
    > ### Aliases: fitmoplot fitmoplot.tidal fitmoplot.tidalmean
    > 
    > ### ** Examples
    > 
    > 
    > ## load a fitted tidal object
    > data(tidfit)
    > 
    > # plot using defaults
    > fitmoplot(tidfit)
    Error in FUN(X[[i]], ...) : object of type 'symbol' is not subsettable
    Calls: <Anonymous> ... <Anonymous> -> get_labels -> normalise_label -> lapply
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘overview.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    4.3Mb
    ```

