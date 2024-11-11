# activAnalyzer

<details>

* Version: 2.1.1
* GitHub: https://github.com/pydemull/activAnalyzer
* Source code: https://github.com/cran/activAnalyzer
* Date/Publication: 2024-05-05 22:40:03 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "activAnalyzer")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘activAnalyzer.Rmd’
      ...
    > p3 <- accum_metrics_sed$p_UBD
    
    > p4 <- accum_metrics_sed$p_gini
    
    > (p1 | p2)/(p3 | p4) + plot_layout(guides = "collect") & 
    +     theme(legend.position = "bottom")
    
      When sourcing ‘activAnalyzer.R’:
    Error: object is not a unit
    Execution halted
    
      ‘activAnalyzer.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘activAnalyzer.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        R         1.5Mb
        doc       1.0Mb
        extdata   2.0Mb
    ```

# actxps

<details>

* Version: 1.5.0
* GitHub: https://github.com/mattheaphy/actxps
* Source code: https://github.com/cran/actxps
* Date/Publication: 2024-06-25 12:40:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "actxps")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘actxps.Rmd’
      ...
    # ℹ 2 more variables: ae_expected_1 <dbl>, ae_expected_2 <dbl>
    
    > autoplot(exp_res)
    Warning: thematic was unable to resolve `bg='auto'`. Try providing an actual color (or `NA`) to the `bg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `fg='auto'`. Try providing an actual color (or `NA`) to the `fg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `accent='auto'`. Try providing an actual color (or `NA`) to the `accent` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    
    ...
    
      When sourcing ‘transactions.R’:
    Error: Internal error: adjust_color() expects an input of length 1
    Execution halted
    
      ‘actxps.Rmd’ using ‘UTF-8’... failed
      ‘exp_summary.Rmd’ using ‘UTF-8’... OK
      ‘exposures.Rmd’ using ‘UTF-8’... OK
      ‘misc.Rmd’ using ‘UTF-8’... failed
      ‘transactions.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘actxps.Rmd’ using rmarkdown
    Warning: thematic was unable to resolve `bg='auto'`. Try providing an actual color (or `NA`) to the `bg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `fg='auto'`. Try providing an actual color (or `NA`) to the `fg` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    Warning: thematic was unable to resolve `accent='auto'`. Try providing an actual color (or `NA`) to the `accent` argument of `thematic_on()`. By the way, 'auto' is only officially supported in `shiny::renderPlot()`, some rmarkdown scenarios (specifically, `html_document()` with `theme!=NULL`), in RStudio, or if `auto_config_set()` is used.
    
    Quitting from lines 129-130 [plot] (actxps.Rmd)
    Error: processing vignette 'actxps.Rmd' failed with diagnostics:
    Internal error: adjust_color() expects an input of length 1
    --- failed re-building ‘actxps.Rmd’
    ...
    Quitting from lines 205-211 [trx-plot] (transactions.Rmd)
    Error: processing vignette 'transactions.Rmd' failed with diagnostics:
    Internal error: adjust_color() expects an input of length 1
    --- failed re-building ‘transactions.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘actxps.Rmd’ ‘misc.Rmd’ ‘transactions.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# adaptr

<details>

* Version: 1.4.0
* GitHub: https://github.com/INCEPTdk/adaptr
* Source code: https://github.com/cran/adaptr
* Date/Publication: 2024-05-03 12:10:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "adaptr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘adaptr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_history
    > ### Title: Plot trial metric history
    > ### Aliases: plot_history plot_history.trial_result
    > ###   plot_history.trial_results
    > 
    > ### ** Examples
    > 
    ...
    +   # Run a single simulation with a fixed random seed
    +   res <- run_trial(binom_trial, seed = 12345)
    + 
    +   # Plot total allocations to each arm according to overall total allocations
    +   plot_history(res, x_value = "total n", y_value = "n")
    + 
    + }
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot_history ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(adaptr)
      Loading 'adaptr' package v1.4.0.
      For instructions, type 'help("adaptr")'
      or see https://inceptdk.github.io/adaptr/.
      > 
      > test_check("adaptr")
    ...
      • plot_history/history-plot-binomial-single-ratio-ys-look.svg
      • plot_metrics_ecdf/errors.svg
      • plot_metrics_ecdf/selected.svg
      • plot_metrics_ecdf/size-only.svg
      • plot_metrics_ecdf/superior.svg
      • plot_status/status-plot-across-arms-binomial.svg
      • plot_status/status-plot-for-all-arms-binomial.svg
      • plot_status/status-plot-for-arm-c-binom.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Overview.Rmd’
      ...
    
    > plot_convergence(calibrated_binom_trial$best_sims, 
    +     metrics = c("size mean", "prob_superior", "prob_equivalence"), 
    +     n_split = 4)
    
    > plot_status(calibrated_binom_trial$best_sims, x_value = "total n")
    
      When sourcing ‘Overview.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Advanced-example.Rmd’ using ‘UTF-8’... OK
      ‘Basic-examples.Rmd’ using ‘UTF-8’... OK
      ‘Overview.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Advanced-example.Rmd’ using rmarkdown
    ```

# AeRobiology

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/AeRobiology
* Date/Publication: 2019-06-03 06:20:03 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "AeRobiology")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘my-vignette.Rmd’
      ...
    Warning: Ignoring unknown labels:
    • `size = "14"`
    
    > iplot_abundance(munich_pollen, interpolation = FALSE, 
    +     export.plot = FALSE, export.result = FALSE, n.types = 3, 
    +     y.start = 2011, y.end = .... [TRUNCATED] 
    
      When sourcing ‘my-vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘my-vignette.Rmd’ using ‘UTF-8’... failed
    ```

# agricolaeplotr

<details>

* Version: 0.5.0
* GitHub: https://github.com/jensharbers/agricolaeplotr
* Source code: https://github.com/cran/agricolaeplotr
* Date/Publication: 2024-01-17 16:42:04 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "agricolaeplotr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(agricolaeplotr)
      
      Type 'citation("agricolaeplotr")' for citing this R package in publications.
      
      Attaching package: 'agricolaeplotr'
      
    ...
      `expected` is a character vector ('ROW')
      ── Failure ('testall.R:847:3'): plot a plot design from FielDHub package shows COLUMN as x axis ──
      p$labels$x (`actual`) not identical to "COLUMN" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('COLUMN')
      
      [ FAIL 30 | WARN 92 | SKIP 0 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

# alien

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/alien
* Date/Publication: 2024-06-19 16:20:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "alien")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basic_usage.Rmd’
      ...
    2 0.3330822 6.848124e-06 1.620061e+04
    3 0.3377835 1.902532e-07 5.997150e+05
    4 0.3425512 5.285577e-09 2.220028e+07
    
    > plot_snc(model, cumulative = T) + coord_cartesian(ylim = c(0, 
    +     150)) + scale_y_continuous(breaks = seq(0, 150, 50)) + ylab("Cumulative discove ..." ... [TRUNCATED] 
    
      When sourcing ‘basic_usage.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘basic_usage.Rmd’ using ‘UTF-8’... failed
      ‘native_discoveries.Rmd’ using ‘UTF-8’... OK
      ‘simulations.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘basic_usage.Rmd’ using rmarkdown
    ```

# AlphaPart

<details>

* Version: 0.9.8
* GitHub: NA
* Source code: https://github.com/cran/AlphaPart
* Date/Publication: 2022-11-15 21:40:05 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "AlphaPart")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AlphaPart-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: print.plotSummaryAlphaPart
    > ### Title: Print a plot generate by the function 'plotSummaryAlphaPart'
    > ### Aliases: print.plotSummaryAlphaPart
    > 
    > ### ** Examples
    > 
    > ## Partition additive genetic values
    ...
    4   4 1 105.00000       66 39.00000
    
    > 
    > ## Plot the partitions
    > p <- plot(ret, ylab=c("BV for trait 1", "BV for trait 2"), xlab="Generation")
    > print(p[[1]])
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: print ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘alphapart-variance.Rmd’
      ...
    
    > data <- readRDS("./../inst/extdata/AlphaPartCattleSim.rds") %>% 
    +     dplyr::mutate(across(generation:mother, as.numeric)) %>% 
    +     dplyr::rename .... [TRUNCATED] 
    Warning in gzfile(file, "rb") :
      cannot open compressed file './../inst/extdata/AlphaPartCattleSim.rds', probable reason 'No such file or directory'
    
      When sourcing ‘alphapart-variance.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘alphapart-variance.Rmd’ using ‘UTF-8’... failed
      ‘alphapart-vignette.Rmd’ using ‘UTF-8’... OK
    ```

# AnalysisLin

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/AnalysisLin
* Date/Publication: 2024-01-30 00:10:10 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "AnalysisLin")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AnalysisLin-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bar_plot
    > ### Title: Bar Plots for Categorical Variables
    > ### Aliases: bar_plot
    > 
    > ### ** Examples
    > 
    > data(iris)
    > bar_plot(iris)
    Error in pm[[2]] : subscript out of bounds
    Calls: bar_plot ... plotly_build -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# animbook

<details>

* Version: 1.0.0
* GitHub: https://github.com/KrisanatA/animbook
* Source code: https://github.com/cran/animbook
* Date/Publication: 2023-12-05 17:50:07 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "animbook")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘animbook-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: anim_animate
    > ### Title: Modified the ggplot object
    > ### Aliases: anim_animate
    > 
    > ### ** Examples
    > 
    > animbook <- anim_prep(data = osiris, id = ID, values = sales, time = year, group = japan)
    ...
              transform it into an animated object
    > 
    > animate <- anim_animate(plot)
    You can now pass it to gganimate::animate().
                       The recommended setting is nframes = 89
    > 
    > plotly::ggplotly(animate)
    Error in pm[[2]] : subscript out of bounds
    Calls: <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# ANN2

<details>

* Version: 2.3.4
* GitHub: https://github.com/bflammers/ANN2
* Source code: https://github.com/cran/ANN2
* Date/Publication: 2020-12-01 10:00:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "ANN2")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ANN2)
      > 
      > # Only test if not on mac
      > if (tolower(Sys.info()[["sysname"]]) != "darwin") {
      +   test_check("ANN2")
      + }
    ...
      ── Failure ('test-plotting.R:59:3'): the reconstruction_plot.ANN() function works correctly ──
      p_AE$labels$colour not equal to "col".
      target is NULL, current is character
      ── Failure ('test-plotting.R:77:3'): the compression_plot.ANN() function works correctly ──
      p_AE$labels$colour not equal to "col".
      target is NULL, current is character
      
      [ FAIL 5 | WARN 1 | SKIP 4 | PASS 143 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 58.9Mb
      sub-directories of 1Mb or more:
        cereal   1.4Mb
        libs    57.3Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# AnnoProbe

<details>

* Version: 0.1.7
* GitHub: https://github.com/jmzeng1314/AnnoProbe
* Source code: https://github.com/cran/AnnoProbe
* Date/Publication: 2022-11-14 08:30:11 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "AnnoProbe")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AnnoProbe-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: check_diff_genes
    > ### Title: Check a list of genes how they show difference.
    > ### Aliases: check_diff_genes
    > 
    > ### ** Examples
    > 
    > attach(GSE95166)
    ...
     11. │           └─ggplot2:::`+.gg`(...)
     12. │             └─ggplot2:::add_ggplot(e1, e2, e2name)
     13. │               ├─ggplot2::ggplot_add(object, p, objectname)
     14. │               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     15. │                 └─ggplot2:::new_layer_names(object, names(plot$layers))
     16. └─base::.handleSimpleError(...)
     17.   └─purrr (local) h(simpleError(msg, call))
     18.     └─cli::cli_abort(...)
     19.       └─rlang::abort(...)
    Execution halted
    ```

# ANOFA

<details>

* Version: 0.1.3
* GitHub: https://github.com/dcousin3/ANOFA
* Source code: https://github.com/cran/ANOFA
* Date/Publication: 2023-11-18 14:20:08 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "ANOFA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ANOFA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Gillet1993
    > ### Title: Gillet1993
    > ### Aliases: Gillet1993
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    > # run the base analysis
    > w <- anofa( Freq ~ species * location * florished, Gillet1993)
    > 
    > # display a plot of the results
    > anofaPlot(w)
    superb::FYI: The variables will be plotted in that order: species, location, florished (use factorOrder to change).
    Error in superb::superbPlot(cdata, BSFactors = bsfact, variables = as.character(w$freqColumn),  : 
      superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
    Calls: anofaPlot -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("ANOFA")
      Loading required package: ANOFA
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 149 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-ANOFA-2.R:36:5'): TESTS of emFrequencies function (2/3) ────────
    ...
          statistic = "count", errorbar = "CI", gamma = confidenceLevel, 
          plotStyle = plotStyle, errorbarParams = errorbarParams, ...)`: superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
      Backtrace:
          ▆
       1. └─ANOFA::anofaPlot(w, Freq ~ B) at test-ANOFA-3.R:46:5
       2.   └─superb::superbPlot(...)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 149 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ConfidenceIntervals.Rmd’
      ...
    > library(ANOFA)
    
    > w <- anofa(obsfreq ~ vocation * gender, LightMargolin1971)
    
    > anofaPlot(w)
    superb::FYI: The variables will be plotted in that order: vocation, gender (use factorOrder to change).
    
    ...
    
    > anofaPlot(w)
    
      When sourcing ‘WhatIsANOFA.R’:
    Error: superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
    Execution halted
    
      ‘ConfidenceIntervals.Rmd’ using ‘UTF-8’... failed
      ‘DataFormatsForFrequencies.Rmd’ using ‘UTF-8’... OK
      ‘WhatIsANOFA.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ConfidenceIntervals.Rmd’ using rmarkdown
    
    Quitting from lines 70-73 [unnamed-chunk-2] (ConfidenceIntervals.Rmd)
    Error: processing vignette 'ConfidenceIntervals.Rmd' failed with diagnostics:
    superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
    --- failed re-building ‘ConfidenceIntervals.Rmd’
    
    --- re-building ‘DataFormatsForFrequencies.Rmd’ using rmarkdown
    ...
    Quitting from lines 108-109 [unnamed-chunk-4] (WhatIsANOFA.Rmd)
    Error: processing vignette 'WhatIsANOFA.Rmd' failed with diagnostics:
    superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
    --- failed re-building ‘WhatIsANOFA.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ConfidenceIntervals.Rmd’ ‘WhatIsANOFA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ANOPA

<details>

* Version: 0.1.3
* GitHub: https://github.com/dcousin3/ANOPA
* Source code: https://github.com/cran/ANOPA
* Date/Publication: 2024-03-22 19:40:05 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "ANOPA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ANOPA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ArringtonEtAl2002
    > ### Title: Arrington et al. (2002) dataset
    > ### Aliases: ArringtonEtAl2002
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
            Africa Nocturnal    Omnivore 0 0
     North America Nocturnal Detritivore 0 0
    Warning: ANOPA::warning(1): Some cells have zero over zero data. Imputing...
    > 
    > # make a plot with all the factors
    > anopaPlot(w)
    Error in superb::superbPlot(wdata, BSFactors = bsfact, WSFactors = wsfact,  : 
      superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
    Calls: anopaPlot -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("ANOPA")
      Loading required package: ANOPA
      ------------------------------------------------------------ 
      Design is: 2 x ( 3 ) with 2 independent groups.
      ------------------------------------------------------------
      1.Between-Subject Factors ( 2 groups ) :
    ...
              1) "UA" else "none"), plotStyle = plotStyle, errorbarParams = errorbarParams, 
          ...)`: superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
      Backtrace:
          ▆
       1. └─ANOPA::anopaPlot(w) at test-ANOPA-4.R:106:9
       2.   └─superb::superbPlot(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 131 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘A-WhatIsANOPA.Rmd’
      ...
    > w <- anopa({
    +     nSuccess
    +     nParticipants
    + } ~ DistractingTask, ArticleExample1)
    
    > anopaPlot(w)
    
    ...
      When sourcing ‘E-ArcsineIsAsinine.R’:
    Error: superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
    Execution halted
    
      ‘A-WhatIsANOPA.Rmd’ using ‘UTF-8’... failed
      ‘B-DataFormatsForProportions.Rmd’ using ‘UTF-8’... OK
      ‘C-ConfidenceIntervals.Rmd’ using ‘UTF-8’... failed
      ‘D-ArringtonExample.Rmd’ using ‘UTF-8’... failed
      ‘E-ArcsineIsAsinine.Rmd’ using ‘UTF-8’... failed
      ‘F-TestingTypeIError.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘A-WhatIsANOPA.Rmd’ using rmarkdown
    
    Quitting from lines 182-183 [unnamed-chunk-5] (A-WhatIsANOPA.Rmd)
    Error: processing vignette 'A-WhatIsANOPA.Rmd' failed with diagnostics:
    superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
    --- failed re-building ‘A-WhatIsANOPA.Rmd’
    
    --- re-building ‘B-DataFormatsForProportions.Rmd’ using rmarkdown
    --- finished re-building ‘B-DataFormatsForProportions.Rmd’
    ...
    
    --- re-building ‘F-TestingTypeIError.Rmd’ using rmarkdown
    --- finished re-building ‘F-TestingTypeIError.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘A-WhatIsANOPA.Rmd’ ‘C-ConfidenceIntervals.Rmd’
      ‘D-ArringtonExample.Rmd’ ‘E-ArcsineIsAsinine.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# AntMAN

<details>

* Version: 1.1.0
* GitHub: https://github.com/bbodin/AntMAN
* Source code: https://github.com/cran/AntMAN
* Date/Publication: 2021-07-23 10:00:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "AntMAN")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘AntMAN-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: AM_mix_hyperparams_uninorm
    > ### Title: univariate Normal mixture hyperparameters
    > ### Aliases: AM_mix_hyperparams_uninorm
    > 
    > ### ** Examples
    > 
    >      
    ...
    Press [enter] to continue
    Plotting pmf for M,K
    NULL
    Press [enter] to continue
    Plotting traces from M,K
    Press [enter] to continue
    Plotting values from M,K
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘UnitTest_AM_binder.R’
      Running ‘UnitTest_AM_coclustering.R’
      Running ‘UnitTest_AM_demo.R’
      Running ‘UnitTest_AM_extract.R’
      Running ‘UnitTest_AM_mcmc.R’
    Running the tests in ‘tests/UnitTest_AM_mcmc.R’ failed.
    Complete output:
      > #######################################################################################
      > ###############
      > ############### AntMAN Package : Tests and Examples
    ...
      Press [enter] to continue
      Plotting pmf for M,K
      NULL
      Press [enter] to continue
      Plotting traces from M,K
      Press [enter] to continue
      Plotting values from M,K
      Error in if (new_name %in% existing) { : argument is of length zero
      Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
      Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        libs   6.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rdpack’ ‘mcclust’
      All declared Imports should be used.
    ```

# APCI

<details>

* Version: 1.0.8
* GitHub: NA
* Source code: https://github.com/cran/APCI
* Date/Publication: 2024-09-02 20:20:06 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "APCI")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘1_tests.R’
    Running the tests in ‘tests/1_tests.R’ failed.
    Complete output:
      > # install the package and use this script to test the package
      > library("APCI")
      > # or: remotes::install_github("jiahui1902/APCI")
      > test_data <- APCI::women9017
      > test_data$acc <- as.factor(test_data$acc)
      > test_data$pcc <- as.factor(test_data$pcc)
      > test_data$educc <- as.factor(test_data$educc)
    ...
       -0.335818939   0.165402344   0.138957101  -0.357703237   0.229441985 
          acc8:pcc3     acc9:pcc3     acc1:pcc4     acc2:pcc4     acc3:pcc4 
       -0.147848556   0.146360984  -0.436635793   0.062363971   0.289676120 
          acc4:pcc4     acc5:pcc4     acc6:pcc4     acc7:pcc4     acc8:pcc4 
        0.266502847   0.199035811  -0.082410026  -0.140171983  -0.274808726 
          acc9:pcc4     acc1:pcc5     acc2:pcc5     acc3:pcc5     acc4:pcc5 
        0.070541348   0.052280642   0.320968547  -0.136111903  -0.102002632 
          acc5:pcc5     acc6:pcc5     acc7:pcc5     acc8:pcc5     acc9:pcc5 
       -0.553458810  -0.333938836   0.340338956   0.670285259  -0.300340437 
      Killed
    ```

# aplot

<details>

* Version: 0.2.3
* GitHub: https://github.com/YuLab-SMU/aplot
* Source code: https://github.com/cran/aplot
* Date/Publication: 2024-06-17 09:50:01 UTC
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "aplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘aplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: insert_left
    > ### Title: plot-insertion
    > ### Aliases: insert_left insert_right insert_top insert_bottom
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > ap
    > ap[2, 1] <- ap[2, 1] + theme_bw()
    > ap[2, 1] <- ap[2, 1] + 
    +             aes(color = as.factor(am)) + 
    +             scale_color_manual(values = c('steelblue', 'darkgreen'))
    > ap[1, 1] <- ap[1, 1] + theme(axis.line.x.bottom=element_line())
    > ap
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# applicable

<details>

* Version: 0.1.1
* GitHub: https://github.com/tidymodels/applicable
* Source code: https://github.com/cran/applicable
* Date/Publication: 2024-04-25 00:00:04 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "applicable")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(applicable)
      Loading required package: ggplot2
      > 
      > test_check("applicable")
      Loading required package: dplyr
    ...
      `expected` is a character vector ('percentile')
      ── Failure ('test-plot.R:36:3'): output of autoplot.apd_pca is correct when options=distance are provided ──
      ad_plot$labels$y (`actual`) not equal to "percentile" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('percentile')
      
      [ FAIL 3 | WARN 0 | SKIP 22 | PASS 90 ]
      Error: Test failures
      Execution halted
    ```

# ASRgenomics

<details>

* Version: 1.1.4
* GitHub: NA
* Source code: https://github.com/cran/ASRgenomics
* Date/Publication: 2024-01-29 21:20:02 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "ASRgenomics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ASRgenomics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kinship.pca
    > ### Title: Performs a Principal Component Analysis (PCA) based on a kinship
    > ###   matrix K
    > ### Aliases: kinship.pca
    > 
    > ### ** Examples
    > 
    ...
     13. │               └─ggplot2:::`+.gg`(...)
     14. │                 └─ggplot2:::add_ggplot(e1, e2, e2name)
     15. │                   ├─ggplot2::ggplot_add(object, p, objectname)
     16. │                   └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     17. │                     └─ggplot2:::new_layer_names(object, names(plot$layers))
     18. └─base::.handleSimpleError(...)
     19.   └─purrr (local) h(simpleError(msg, call))
     20.     └─cli::cli_abort(...)
     21.       └─rlang::abort(...)
    Execution halted
    ```

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
      > # * https://r-pkgs.org/tests.html
    ...
       16. │                   └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       17. │                     └─ggplot2:::new_layer_names(object, names(plot$layers))
       18. └─base::.handleSimpleError(...)
       19.   └─purrr (local) h(simpleError(msg, call))
       20.     └─cli::cli_abort(...)
       21.       └─rlang::abort(...)
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 249 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        data   8.5Mb
    ```

# autocogs

<details>

* Version: 0.1.4
* GitHub: https://github.com/schloerke/autocogs
* Source code: https://github.com/cran/autocogs
* Date/Publication: 2021-05-29 17:00:05 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "autocogs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(autocogs)
      > 
      > test_check("autocogs")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 228 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        8.         └─autocogs (local) FUN(X[[i]], ...)
        9.           └─base::lapply(...)
       10.             └─autocogs (local) FUN(X[[i]], ...)
       11.               ├─base::do.call(fn, args)
       12.               └─autocogs (local) `<fn>`(...)
       13.                 └─base::do.call(loess, c(core_params, params$method.args))
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 228 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘broom’ ‘diptest’ ‘ggplot2’ ‘hexbin’ ‘moments’
      All declared Imports should be used.
    ```

# autoplotly

<details>

* Version: 0.1.4
* GitHub: https://github.com/terrytangyuan/autoplotly
* Source code: https://github.com/cran/autoplotly
* Date/Publication: 2021-04-18 06:50:11 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "autoplotly")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autoplotly-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplotly
    > ### Title: Automatic Visualization of Popular Statistical Results Using
    > ###   'plotly.js' and 'ggplot2'
    > ### Aliases: autoplotly
    > 
    > ### ** Examples
    > 
    > # Automatically generate interactive plot for results produced by `stats::prcomp`
    > p <- autoplotly(prcomp(iris[c(1, 2, 3, 4)]), data = iris,
    +                 colour = 'Species', label = TRUE, label.size = 3, frame = TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: autoplotly ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(autoplotly)
      > 
      > test_check("autoplotly")
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 1 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        5.     └─ggfortify::ggbiplot(...)
        6.       └─ggplot2:::`+.gg`(...)
        7.         └─ggplot2:::add_ggplot(e1, e2, e2name)
        8.           ├─ggplot2::ggplot_add(object, p, objectname)
        9.           └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       10.             └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

# autoReg

<details>

* Version: 0.3.3
* GitHub: https://github.com/cardiomoon/autoReg
* Source code: https://github.com/cran/autoReg
* Date/Publication: 2023-11-14 05:53:27 UTC
* Number of recursive dependencies: 214

Run `revdepcheck::cloud_details(, "autoReg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autoReg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: adjustedPlot.survreg
    > ### Title: Draw predicted survival curve with an object survreg
    > ### Aliases: adjustedPlot.survreg
    > 
    > ### ** Examples
    > 
    > library(survival)
    > x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
    > adjustedPlot(x)
    > adjustedPlot(x,addCox=TRUE)
    Warning: Removed 42 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Automatic_Regression_Modeling.Rmd’
      ...
    Species                 setosa     (N=50)    Mean ± SD  5.0 ± 0.4                                                                                        
                            versicolor (N=50)    Mean ± SD  5.9 ± 0.5  1.46 (1.24 to 1.68, p<.001)  1.44 (1.16 to 1.71, p<.001)  1.47 (1.23 to 1.70, p<.001) 
                            virginica  (N=50)    Mean ± SD  6.6 ± 0.6  1.95 (1.75 to 2.14, p<.001)  1.87 (1.62 to 2.11, p<.001)  1.97 (1.76 to 2.17, p<.001) 
    ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
    
    > modelPlot(fit1, imputed = TRUE)
    
    ...
    
      When sourcing ‘Survival.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Automatic_Regression_Modeling.Rmd’ using ‘UTF-8’... failed
      ‘Bootstrap_Prediction.Rmd’ using ‘UTF-8’... OK
      ‘Getting_started.Rmd’ using ‘UTF-8’... failed
      ‘Statiastical_test_in_gaze.Rmd’ using ‘UTF-8’... OK
      ‘Survival.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Automatic_Regression_Modeling.Rmd’ using rmarkdown
    
    Quitting from lines 142-143 [unnamed-chunk-15] (Automatic_Regression_Modeling.Rmd)
    Error: processing vignette 'Automatic_Regression_Modeling.Rmd' failed with diagnostics:
    object is not a unit
    --- failed re-building ‘Automatic_Regression_Modeling.Rmd’
    
    --- re-building ‘Bootstrap_Prediction.Rmd’ using rmarkdown
    ```

# baggr

<details>

* Version: 0.7.8
* GitHub: https://github.com/wwiecek/baggr
* Source code: https://github.com/cran/baggr
* Date/Publication: 2024-02-12 18:20:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "baggr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘baggr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: baggr_plot
    > ### Title: Plotting method in baggr package
    > ### Aliases: baggr_plot
    > 
    > ### ** Examples
    > 
    > fit <- baggr(schools, pooling = "none")
    Automatically chose Rubin model with aggregate data based on input data.
    Setting prior for mean in each group using 10 times the max effect :
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(baggr)
      Loading required package: Rcpp
      This is baggr; see vignette('baggr') for tutorial, ?baggr for basic help.
      For execution on a local, multicore CPU with excess RAM call:
      options(mc.cores = parallel::detectCores())
      > 
    ...
       10.           └─bayesplot::mcmc_areas(...)
       11.             └─ggplot2:::`+.gg`(...)
       12.               └─ggplot2:::add_ggplot(e1, e2, e2name)
       13.                 ├─ggplot2::ggplot_add(object, p, objectname)
       14.                 └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       15.                   └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 6 | WARN 0 | SKIP 6 | PASS 488 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘baggr.Rmd’
      ...
    2.5%  0.4303903
    mean  0.8619752
    97.5% 0.9998572
    
    
    > plot(baggr_schools, order = FALSE)
    
      When sourcing ‘baggr.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘baggr.Rmd’ using ‘UTF-8’... failed
      ‘baggr_binary.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘baggr.Rmd’ using rmarkdown
    
    Quitting from lines 272-273 [unnamed-chunk-9] (baggr.Rmd)
    Error: processing vignette 'baggr.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘baggr.Rmd’
    
    --- re-building ‘baggr_binary.Rmd’ using rmarkdown
    --- finished re-building ‘baggr_binary.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘baggr.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 196.3Mb
      sub-directories of 1Mb or more:
        libs  194.5Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# banter

<details>

* Version: 0.9.6
* GitHub: NA
* Source code: https://github.com/cran/banter
* Date/Publication: 2023-02-12 21:32:29 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "banter")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘banter-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotDetectorTrace
    > ### Title: Plot BANTER Detector Traces
    > ### Aliases: plotDetectorTrace
    > 
    > ### ** Examples
    > 
    > data(train.data)
    ...
    > bant.mdl <- addBanterDetector(
    +   bant.mdl, train.data$detectors, 
    +   ntree = 50, sampsize = 1, num.cores = 1
    + )
    > 
    > plotDetectorTrace(bant.mdl)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: plotDetectorTrace ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# bartMan

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/bartMan
* Date/Publication: 2024-07-24 12:10:02 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "bartMan")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bartMan-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mdsBart
    > ### Title: mdsBart
    > ### Aliases: mdsBart
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("dbarts", quietly = TRUE)) {
    ...
      |                                                                            
      |======================================================================| 100%
    Extracting Observation Data...
    
    Getting proximites...
    Getting MDS...
    Performing procrustes...
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: mdsBart -> suppressMessages -> withCallingHandlers
    Execution halted
    ```

# BasketballAnalyzeR

<details>

* Version: 0.5.0
* GitHub: https://github.com/sndmrc/BasketballAnalyzeR
* Source code: https://github.com/cran/BasketballAnalyzeR
* Date/Publication: 2020-06-26 09:00:11 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "BasketballAnalyzeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BasketballAnalyzeR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scatterplot
    > ### Title: Draws a scatter plot or a matrix of scatter plots
    > ### Aliases: scatterplot
    > 
    > ### ** Examples
    > 
    > # Single scatter plot
    ...
    > X <- data.frame(AST=Pbox.sel$AST/Pbox.sel$MIN,TOV=Pbox.sel$TOV/Pbox.sel$MIN)
    > X$PTSpm <- Pbox.sel$PTS/Pbox.sel$MIN
    > mypal <- colorRampPalette(c("blue","yellow","red"))
    > scatterplot(X, data.var=c("AST","TOV"), z.var="PTSpm", labels=1:nrow(X), palette=mypal)
    > # Matrix of scatter plots
    > data <- Pbox[1:50, c("PTS","P3M","P2M","OREB","Team")]
    > scatterplot(data, data.var=1:4, z.var="Team")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘circlize’ ‘hexbin’ ‘scales’ ‘sna’
      All declared Imports should be used.
    ```

# bayefdr

<details>

* Version: 0.2.1
* GitHub: https://github.com/VallejosGroup/bayefdr
* Source code: https://github.com/cran/bayefdr
* Date/Publication: 2022-10-26 19:35:06 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "bayefdr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bayefdr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: traceplot
    > ### Title: Trace, marginal density histogram, and autocorrelation plot of
    > ###   MCMC draws.
    > ### Aliases: traceplot
    > 
    > ### ** Examples
    > 
    > x <- rnorm(1000)
    > traceplot(x)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: traceplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayefdr)
      > 
      > test_check("bayefdr")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 14 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        8.         └─base::Reduce(`+`, c(list(noGeomPlot), layers))
        9.           └─ggplot2:::`+.gg`(init, x[[i]])
       10.             └─ggplot2:::add_ggplot(e1, e2, e2name)
       11.               ├─ggplot2::ggplot_add(object, p, objectname)
       12.               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       13.                 └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 14 ]
      Error: Test failures
      Execution halted
    ```

# bayesAB

<details>

* Version: 1.1.3
* GitHub: https://github.com/FrankPortman/bayesAB
* Source code: https://github.com/cran/bayesAB
* Date/Publication: 2021-06-25 00:50:02 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "bayesAB")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayesAB)
      > 
      > test_check("bayesAB")
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 140 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-dists.R:34:3'): Success ──────────────────────────────────────
      plotNormalInvGamma(3, 1, 1, 1)$labels$y not equal to "sig_sq".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 140 ]
      Error: Test failures
      Execution halted
    ```

# BayesGrowth

<details>

* Version: 1.0.0
* GitHub: https://github.com/jonathansmart/BayesGrowth
* Source code: https://github.com/cran/BayesGrowth
* Date/Publication: 2023-11-21 18:10:08 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "BayesGrowth")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘MCMC-example.Rmd’
      ...
    > ggplot(growth_curve, aes(Age, LAA)) + geom_point(data = example_data, 
    +     aes(Age, Length), alpha = 0.3) + geom_lineribbon(aes(ymin = .lower, 
    +  .... [TRUNCATED] 
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
      When sourcing ‘MCMC-example.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 14, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, FALSE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NUL
    Execution halted
    
      ‘MCMC-example.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MCMC-example.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 84.8Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        libs  82.6Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# BayesianReasoning

<details>

* Version: 0.4.2
* GitHub: https://github.com/gorkang/BayesianReasoning
* Source code: https://github.com/cran/BayesianReasoning
* Date/Publication: 2023-11-14 11:33:20 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "BayesianReasoning")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(BayesianReasoning)
      > 
      > test_check("BayesianReasoning")
      
       Plot created in: ./FP_10_sens_100_screening_1667_diagnostic_44.png
      
    ...
      `names(expected)` is absent
      ── Failure ('test-PPV_heatmap.R:1097:3'): Plot with line overlay ───────────────
      vapply(p$result$layers, function(x) class(x$geom)[1], "") (`actual`) not identical to c("GeomTile", "GeomSegment", "GeomPoint", "GeomMarkRect") (`expected`).
      
      `names(actual)` is a character vector ('geom_tile', 'annotate', 'annotate...3', 'geom_mark_rect')
      `names(expected)` is absent
      
      [ FAIL 8 | WARN 56 | SKIP 4 | PASS 115 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘PPV_NPV.Rmd’
      ...
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    Warning in ggforce::geom_mark_rect(aes(label = paste0(translated_labels$label_PPV_NPV,  :
      All aesthetics have length 1, but the data has 10201 rows.
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    
      When sourcing ‘PPV_NPV.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘PPV_NPV.Rmd’ using ‘UTF-8’... failed
      ‘introduction.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PPV_NPV.Rmd’ using rmarkdown
    ```

# BayesMallows

<details>

* Version: 2.2.2
* GitHub: https://github.com/ocbe-uio/BayesMallows
* Source code: https://github.com/cran/BayesMallows
* Date/Publication: 2024-08-17 13:00:03 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "BayesMallows")` for more info

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
      `expected` is a character vector ('interaction(chain, cluster)')
      ── Failure ('test-assess_convergence.R:217:3'): assess_convergence.BayesMallowsMixtures works ──
      p$labels$colour (`actual`) not equal to "cluster" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('cluster')
      
      [ FAIL 10 | WARN 0 | SKIP 6 | PASS 435 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 23.7Mb
      sub-directories of 1Mb or more:
        doc    2.7Mb
        libs  20.1Mb
    ```

# bayesplay

<details>

* Version: 0.9.3
* GitHub: https://github.com/bayesplay/bayesplay
* Source code: https://github.com/cran/bayesplay
* Date/Publication: 2023-04-13 12:10:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "bayesplay")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bayesplay-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: visual_compare
    > ### Title: Visually compare two models
    > ### Aliases: visual_compare
    > 
    > ### ** Examples
    > 
    > # define two models
    ...
    > h1_mod <- prior(family = "normal", mean = 0, sd = 10)
    > m0 <- extract_predictions(data_model * h0_mod)
    > m1 <- extract_predictions(data_model * h1_mod)
    > 
    > # visually compare the model
    > visual_compare(m0, m1)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘advanced.Rmd’
      ...
    > plot(posterior1, add_prior = TRUE) + labs(title = "prior and posterior distribution", 
    +     subtitle = "for a binomial likelihood and beta prior")
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      conversion failure on 'ϴ' in 'mbcsToSbcs': dot substituted for <cf>
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      conversion failure on 'ϴ' in 'mbcsToSbcs': dot substituted for <b4>
    
    ...
    > visual_compare(d_model1, d_model2)
    
      When sourcing ‘plots.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘advanced.Rmd’ using ‘UTF-8’... failed
      ‘basic.Rmd’ using ‘UTF-8’... OK
      ‘default_ttests.Rmd’ using ‘UTF-8’... OK
      ‘plots.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘advanced.Rmd’ using rmarkdown
    ```

# bayesplot

<details>

* Version: 1.11.1
* GitHub: https://github.com/stan-dev/bayesplot
* Source code: https://github.com/cran/bayesplot
* Date/Publication: 2024-02-15 05:30:11 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "bayesplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bayesplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MCMC-intervals
    > ### Title: Plot interval estimates from MCMC draws
    > ### Aliases: MCMC-intervals mcmc_intervals mcmc_areas mcmc_areas_ridges
    > ###   mcmc_intervals_data mcmc_areas_data mcmc_areas_ridges_data
    > 
    > ### ** Examples
    > 
    ...
    
    $Parameter
    [1] "alpha"   "sigma"   "beta[1]" "beta[2]" "beta[3]" "beta[4]"
    
    > 
    > color_scheme_set("brightblue")
    > mcmc_intervals(x)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: mcmc_intervals ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayesplot)
      This is bayesplot version 1.11.1
      - Online documentation and vignettes at mc-stan.org/bayesplot
      - bayesplot theme set to bayesplot::theme_default()
         * Does _not_ affect other ggplot2 plots
         * See ?bayesplot_theme_set for details on theme setting
    ...
        5. └─bayesplot::ppc_violin_grouped(y, yrep, group)
        6.   └─ggplot2:::`+.gg`(...)
        7.     └─ggplot2:::add_ggplot(e1, e2, e2name)
        8.       ├─ggplot2::ggplot_add(object, p, objectname)
        9.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       10.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 6 | WARN 1 | SKIP 73 | PASS 994 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plotting-mcmc-draws.Rmd’
      ...
    
    
    > color_scheme_set("red")
    
    > mcmc_intervals(posterior, pars = c("cyl", "drat", 
    +     "am", "sigma"))
    
    ...
    > fit_cp <- sampling(schools_mod_cp, data = schools_dat, 
    +     seed = 803214055, control = list(adapt_delta = 0.9))
    
      When sourcing ‘visual-mcmc-diagnostics.R’:
    Error: error in evaluating the argument 'object' in selecting a method for function 'sampling': object 'schools_mod_cp' not found
    Execution halted
    
      ‘graphical-ppcs.Rmd’ using ‘UTF-8’... OK
      ‘plotting-mcmc-draws.Rmd’ using ‘UTF-8’... failed
      ‘visual-mcmc-diagnostics.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        R     4.0Mb
        doc   3.8Mb
    ```

# bayestestR

<details>

* Version: 0.14.0
* GitHub: https://github.com/easystats/bayestestR
* Source code: https://github.com/cran/bayestestR
* Date/Publication: 2024-07-24 14:10:02 UTC
* Number of recursive dependencies: 209

Run `revdepcheck::cloud_details(, "bayestestR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bayestestR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bayesfactor_restricted
    > ### Title: Bayes Factors (BF) for Order Restricted Models
    > ### Aliases: bayesfactor_restricted bf_restricted
    > ###   bayesfactor_restricted.stanreg bayesfactor_restricted.brmsfit
    > ###   bayesfactor_restricted.blavaan bayesfactor_restricted.emmGrid
    > ###   as.logical.bayesfactor_restricted
    > 
    ...
    + )
    > 
    > 
    > (b <- bayesfactor_restricted(posterior, hypothesis = hyps, prior = prior))
    Bayes Factor (Order-Restriction)
    
    Hypothesis    P(Prior) P(Posterior)    BF
    A > B & B > C     0.16         0.23  1.39
    A > B & A > C     0.36         0.59  1.61
    C > A             0.46         0.34 0.742
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(bayestestR)
      > 
      > test_check("bayestestR")
      Starting 2 test processes
      [ FAIL 3 | WARN 2 | SKIP 74 | PASS 180 ]
      
    ...
       14.                 └─brms:::eval2(call, envir = args, enclos = envir)
       15.                   └─base::eval(expr, envir, ...)
       16.                     └─base::eval(expr, envir, ...)
       17.                       └─rstan (local) .fun(model_code = .x1)
       18.                         └─rstan:::cxxfunctionplus(...)
       19.                           └─base::sink(type = "output")
      
      [ FAIL 3 | WARN 2 | SKIP 74 | PASS 180 ]
      Error: Test failures
      Execution halted
    ```

# BCEA

<details>

* Version: 2.4.6
* GitHub: https://github.com/n8thangreen/BCEA
* Source code: https://github.com/cran/BCEA
* Date/Publication: 2024-02-16 15:00:08 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "BCEA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BCEA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ceac.plot.bcea
    > ### Title: Cost-Effectiveness Acceptability Curve (CEAC) Plot
    > ### Aliases: ceac.plot.bcea ceac.plot
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
    > he <- BCEA::bcea(eff, cost)
    No reference selected. Defaulting to first intervention.
    > ceac.plot(he)
    > 
    > ceac.plot(he, graph = "base")
    > ceac.plot(he, graph = "ggplot2")
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(BCEA)
      The BCEA version loaded is: 2.4.6
      
      Attaching package: 'BCEA'
      
      The following object is masked from 'package:graphics':
    ...
       3.   └─BCEA:::eib_plot_ggplot(he, graph_params, ...)
       4.     └─ggplot2:::`+.gg`(...)
       5.       └─ggplot2:::add_ggplot(e1, e2, e2name)
       6.         ├─ggplot2::ggplot_add(object, p, objectname)
       7.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       8.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 1 | SKIP 7 | PASS 159 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CEriskav.Rmd’
      ...
    
    > CEriskav(bcea_smoke) <- r
    
    > plot(bcea_smoke)
    
    > plot(bcea_smoke, graph = "ggplot")
    
    ...
    
      ‘CEriskav.Rmd’ using ‘UTF-8’... failed
      ‘bcea.Rmd’ using ‘UTF-8’... failed
      ‘ceac.Rmd’ using ‘UTF-8’... failed
      ‘ceef.Rmd’ using ‘UTF-8’... OK
      ‘ceplane.Rmd’ using ‘UTF-8’... failed
      ‘contour.Rmd’ using ‘UTF-8’... failed
      ‘eib.Rmd’ using ‘UTF-8’... failed
      ‘paired_vs_multiple_comps.Rmd’ using ‘UTF-8’... OK
      ‘Set_bcea_parameters.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CEriskav.Rmd’ using rmarkdown
    
    Quitting from lines 41-46 [unnamed-chunk-3] (CEriskav.Rmd)
    Error: processing vignette 'CEriskav.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘CEriskav.Rmd’
    
    --- re-building ‘bcea.Rmd’ using rmarkdown
    ```

# BDgraph

<details>

* Version: 2.73
* GitHub: NA
* Source code: https://github.com/cran/BDgraph
* Date/Publication: 2024-08-23 13:20:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "BDgraph")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘BDgraph-Examples.Rmd’
      ...
    Sensitivity         1   0.714       0.714
    MCC                 1   0.827       0.827
    
    > plotroc(list(bdgraph.obj, bdgraph.mpl.obj), data.sim, 
    +     cut = 200, labels = c("BDgraph", "BDgraph.mpl"), color = c("blue", 
    +         "red"))
    
      When sourcing ‘BDgraph-Examples.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘BDgraph-Examples.Rmd’ using ‘UTF-8’... failed
      ‘Introduction-BDgraph.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘BDgraph-Examples.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        libs   6.8Mb
    ```

# BEAMR

<details>

* Version: 1.1.0
* GitHub: https://github.com/annaSeffernick/BEAMR
* Source code: https://github.com/cran/BEAMR
* Date/Publication: 2024-07-27 16:00:06 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "BEAMR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BEAMR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gen_beam_plot_list
    > ### Title: Generate BEAM Plot List
    > ### Aliases: gen_beam_plot_list
    > 
    > ### ** Examples
    > 
    > data(beam_stats)
    ...
    +                              beam.specs=beam_stats$beam.specs)
    > plot.list <- gen_beam_plot_list(beam.result=beam_stats, beam.specs=plot.specs,
    +                                 beam.feat.pvals=test.feat.pvals,
    +                                 number.pairs=1, set.id="ENSG00000099810",
    +                                 feat.id=NULL, title.size=11,
    +                                 pair.order="omic", endpt.order=NULL)
    Error in if (new_name %in% existing) { : argument is of length zero
    Error in plot.temp$plot : $ operator is invalid for atomic vectors
    Calls: gen_beam_plot_list
    Execution halted
    ```

# beastt

<details>

* Version: 0.0.1
* GitHub: https://github.com/GSK-Biostatistics/beastt
* Source code: https://github.com/cran/beastt
* Date/Publication: 2024-06-20 15:50:16 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "beastt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘beastt-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_dist
    > ### Title: Plot Distribution
    > ### Aliases: plot_dist
    > 
    > ### ** Examples
    > 
    > library(distributional)
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘binary.Rmd’
      ...
    
    > plot_dist(pwr_prior)
    
      When sourcing ‘binary.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ...
      When sourcing ‘continuous.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 
    Execution halted
    
      ‘binary.Rmd’ using ‘UTF-8’... failed
      ‘continuous.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘binary.Rmd’ using rmarkdown
    ```

# BeeGUTS

<details>

* Version: 1.1.3
* GitHub: https://github.com/bgoussen/BeeGUTS
* Source code: https://github.com/cran/BeeGUTS
* Date/Publication: 2023-09-18 15:40:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "BeeGUTS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BeeGUTS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: traceplot
    > ### Title: Plotting method for traces and densities for 'beeSurvFit'
    > ###   objects
    > ### Aliases: traceplot traceplot.beeSurvFit
    > 
    > ### ** Examples
    > 
    > data(fitBetacyfluthrin_Chronic)
    > traceplot(fitBetacyfluthrin_Chronic)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: traceplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Tutorial.Rmd’
      ...
    Chain 1:  Elapsed Time: 137.867 seconds (Warm-up)
    Chain 1:                75.093 seconds (Sampling)
    Chain 1:                212.96 seconds (Total)
    Chain 1: 
    
    > traceplot(fit)
    
      When sourcing ‘Tutorial.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Tutorial.Rmd’ using rmarkdown
    
    Quitting from lines 45-58 [example] (Tutorial.Rmd)
    Error: processing vignette 'Tutorial.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 78.9Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        libs  74.3Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# besthr

<details>

* Version: 0.3.2
* GitHub: NA
* Source code: https://github.com/cran/besthr
* Date/Publication: 2023-04-14 08:50:08 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "besthr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘besthr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.hrest
    > ### Title: plots the 'hrest' object
    > ### Aliases: plot.hrest
    > 
    > ### ** Examples
    > 
    > 
    >  d1 <- make_data()
    >  hr_est <- estimate(d1, score, group)
    >  plot(hr_est)
    Picking joint bandwidth of 0.68
    Error in as.unit(value) : object is not coercible to a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> [<- -> [<-.unit -> as.unit
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basic-use.Rmd’
      ...
    Confidence Intervals (0.025, 0.975)
     3.7475, 8.2525
    
    100 bootstrap resamples.
    > plot(hr_est_1)
    Picking joint bandwidth of 0.381
    
      When sourcing ‘basic-use.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘basic-use.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘basic-use.Rmd’ using rmarkdown
    
    Quitting from lines 34-44 [unnamed-chunk-2] (basic-use.Rmd)
    Error: processing vignette 'basic-use.Rmd' failed with diagnostics:
    object is not coercible to a unit
    --- failed re-building ‘basic-use.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘basic-use.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# betaclust

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/betaclust
* Date/Publication: 2023-09-29 10:20:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "betaclust")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘vignettes.Rmd’
      ...
    +     data = pca.methylation.data[, 2:5], patient_number = 1, plot_type = "ggplot")
    
    > plot(threshold_out, what = "kernel density", threshold = TRUE, 
    +     data = pca.methylation.data[, 2:5], plot_type = "ggplot")
    
    > plot(threshold_out, what = "uncertainty")
    
      When sourcing ‘vignettes.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘vignettes.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘vignettes.Rmd’ using rmarkdown
    ```

# biclustermd

<details>

* Version: 0.2.3
* GitHub: https://github.com/jreisner/biclustermd
* Source code: https://github.com/cran/biclustermd
* Date/Publication: 2021-06-17 15:10:06 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "biclustermd")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(biclustermd)
      Loading required package: ggplot2
      Loading required package: tidyr
      
      Attaching package: 'tidyr'
      
    ...
      ── Failure ('test-autoplot_biclustermd.R:6:3'): autoplot_biclustermd() correctly plots cluster lines ──
      ap$data[[3]]$xintercept[-1] not equal to cumsum(colSums(sbc$P)) + 0.5.
      Classes differ: 'mapped_discrete'/'numeric' is not 'numeric'
      ── Failure ('test-autoplot_biclustermd.R:7:3'): autoplot_biclustermd() correctly plots cluster lines ──
      ap$data[[4]]$yintercept[-1] not equal to cumsum(colSums(sbc$Q)) + 0.5.
      Classes differ: 'mapped_discrete'/'numeric' is not 'numeric'
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 66 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘nycflights13’
      All declared Imports should be used.
    ```

# biodosetools

<details>

* Version: 3.6.1
* GitHub: https://github.com/biodosetools-team/biodosetools
* Source code: https://github.com/cran/biodosetools
* Date/Publication: 2022-11-16 16:00:02 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "biodosetools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(biodosetools)
      > 
      > test_check("biodosetools")
      ! Problem with `glm()` -> constraint ML optimization will be used instead
      ! Problem with `glm()` -> constraint ML optimization will be used instead
      number of iterations= 43 
    ...
          actual                 | expected                  
      [2] "Estimation"           | "Estimation"           [2]
      [3] "Dose (Gy)"            | "Dose (Gy)"            [3]
      [4] "Translocations/cells" | "Translocations/cells" [4]
                                 - "yield_low"            [5]
                                 - "yield_upp"            [6]
      
      [ FAIL 4 | WARN 0 | SKIP 1 | PASS 232 ]
      Error: Test failures
      Execution halted
    ```

# BioPred

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/BioPred
* Date/Publication: 2024-06-06 16:50:09 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "BioPred")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Tutorial.Rmd’
      ...
    </table>
    > tutorial_data$biogroup = ifelse(tutorial_data$x2 <= 
    +     0.5, "biomarker_positive", "biomarker_negative")
    
    > res = subgrp_perf_pred(yvar = "y.time", censorvar = "y.event", 
    +     grpvar = "biogroup", grpname = c("biomarker_positive", "biomarker_negative"),  .... [TRUNCATED] 
    
      When sourcing ‘Tutorial.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Tutorial.Rmd’ using rmarkdown
    ```

# BlandAltmanLeh

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/BlandAltmanLeh
* Date/Publication: 2015-12-23 23:32:17
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "BlandAltmanLeh")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Intro.Rmd’
      ...
    > b <- 0.02 * a + 0.3 * rnorm(150)
    
    > library(ggExtra)
    
    > print(ggMarginal(bland.altman.plot(a, b, graph.sys = "ggplot2"), 
    +     type = "histogram", size = 4))
    
      When sourcing ‘Intro.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Intro.Rmd’... failed
    ```

# bmggum

<details>

* Version: 0.1.0
* GitHub: https://github.com/Naidantu/bmggum
* Source code: https://github.com/cran/bmggum
* Date/Publication: 2021-04-09 08:50:06 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "bmggum")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bmggum-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bayesplot
    > ### Title: bayesian convergence diagnosis plotting function
    > ### Aliases: bayesplot
    > 
    > ### ** Examples
    > 
    > Data <- c(1,4,2,3)
    ...
    Chain 1: 
    Warning: There were 3 divergent transitions after warmup. See
    https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    to find out why this is a problem and how to eliminate them.
    Warning: Examine the pairs() plot to diagnose sampling problems
    
    > bayesplot(mod, 'alpha', 'density', inc_warmup=FALSE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: bayesplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 206.7Mb
      sub-directories of 1Mb or more:
        libs  206.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RcppParallel’ ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# boxly

<details>

* Version: 0.1.1
* GitHub: https://github.com/Merck/boxly
* Source code: https://github.com/cran/boxly
* Date/Publication: 2023-10-24 02:40:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "boxly")` for more info

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
       16.   ├─plotly::add_trace(...)
       17.   │ └─plotly::add_data(p, data)
       18.   │   └─plotly:::is.plotly(p)
       19.   ├─plotly::ggplotly(p, tooltip = "text", dynamicTicks = TRUE)
       20.   └─plotly:::ggplotly.ggplot(p, tooltip = "text", dynamicTicks = TRUE)
       21.     └─plotly::gg2list(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 25 ]
      Error: Test failures
      Execution halted
    ```

# braidReports

<details>

* Version: 0.5.4
* GitHub: NA
* Source code: https://github.com/cran/braidReports
* Date/Publication: 2021-01-05 18:20:09 UTC
* Number of recursive dependencies: 30

Run `revdepcheck::cloud_details(, "braidReports")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘braidReports-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: makeBRAIDreport
    > ### Title: Make a BRAID Report Page
    > ### Aliases: makeBRAIDreport
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
     22. │                       └─grid::convertUnit(short, "cm", valueOnly = TRUE)
     23. │                         ├─grid:::upgradeUnit(x)
     24. │                         └─grid:::upgradeUnit.default(x)
     25. │                           └─base::stop("Not a unit object")
     26. └─base::.handleSimpleError(`<fn>`, "Not a unit object", base::quote(upgradeUnit.default(x)))
     27.   └─rlang (local) h(simpleError(msg, call))
     28.     └─handlers[[1L]](cnd)
     29.       └─cli::cli_abort(...)
     30.         └─rlang::abort(...)
    Execution halted
    ```

# BRcal

<details>

* Version: 0.0.4
* GitHub: https://github.com/apguthrie/BRcal
* Source code: https://github.com/cran/BRcal
* Date/Publication: 2024-06-25 11:30:08 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "BRcal")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘BRcal-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lineplot
    > ### Title: Lineplot for LLO-adjusted Probability Predictions
    > ### Aliases: lineplot
    > 
    > ### ** Examples
    > 
    > 
    ...
    > # Simulated 100 binary event outcomes using x
    > y <- rbinom(100, 1, x)  # By construction, x is well calibrated.
    > 
    > # Lineplot show change in probabilities from original to MLE-recalibration to 
    > # specified Levels of Boldness-Recalibration via t_levels
    > # Return a list with dataframe used to construct plot with return_df=TRUE
    > lp1 <- lineplot(x, y, t_levels=c(0.98, 0.95), return_df=TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: lineplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘hockey_vignette.Rmd’
      ...
    [841] 0.4804441 0.7670617 0.4668403 0.4104682 0.6058493 0.4249086 0.6581869
    [848] 0.7194199 0.4534938 0.7421488 0.6726924 0.3255808 0.5005185 0.6483056
    [855] 0.7210362 0.6593455 0.4586214 0.7750603 0.5841900 0.4826292 0.4080026
    [862] 0.6701504 0.6561462 0.4814185 0.7421488 0.6786381 0.3255808 0.4814569
    
    > lineplot(hockey$x, hockey$y)
    
      When sourcing ‘hockey_vignette.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘hockey_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘hockey_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 180-181 [unnamed-chunk-11] (hockey_vignette.Rmd)
    Error: processing vignette 'hockey_vignette.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘hockey_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘hockey_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# breathtestcore

<details>

* Version: 0.8.7
* GitHub: https://github.com/dmenne/breathtestcore
* Source code: https://github.com/cran/breathtestcore
* Date/Publication: 2024-01-24 15:02:47 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "breathtestcore")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > options(Ncpus = parallelly::availableCores(omit = 1))
      > test_check("breathtestcore")
      Loading required package: breathtestcore
      Starting 1 test process
      [ FAIL 3 | WARN 11 | SKIP 4 | PASS 356 ]
    ...
      `expected`: 10
      ── Failure ('test_plot_breathtestfit.R:81:3'): Plot multiple groups data only (no fit) ──
      length(p) (`actual`) not equal to length(ggplot()) (`expected`).
      
        `actual`: 11
      `expected`: 10
      
      [ FAIL 3 | WARN 11 | SKIP 4 | PASS 356 ]
      Error: Test failures
      Execution halted
    ```

# brolgar

<details>

* Version: 1.0.1
* GitHub: https://github.com/njtierney/brolgar
* Source code: https://github.com/cran/brolgar
* Date/Publication: 2024-05-10 14:50:34 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "brolgar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘brolgar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facet_sample
    > ### Title: Facet data into groups to facilitate exploration
    > ### Aliases: facet_sample
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > ggplot(heights,
    + aes(x = year,
    +     y = height_cm,
    +     group = country)) +
    +   geom_line() +
    +   facet_sample()
    Error in if (params$as.table) { : argument is of length zero
    Calls: <Anonymous> ... <Anonymous> -> setup -> <Anonymous> -> compute_layout
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘exploratory-modelling.Rmd’
      ...
    Warning in is.na(non_null_default_aes[[aes_param_name]]) :
      is.na() applied to non-(list or vector) of type 'language'
    
      When sourcing ‘exploratory-modelling.R’:
    Error: ℹ In index: 1.
    ℹ With name: geom_line.
    Caused by error in `aes_param_name %in% names(non_null_default_aes) && is.na(non_null_default_aes[[
    ...
    Error: argument is of length zero
    Execution halted
    
      ‘exploratory-modelling.Rmd’ using ‘UTF-8’... failed
      ‘finding-features.Rmd’ using ‘UTF-8’... failed
      ‘getting-started.Rmd’ using ‘UTF-8’... failed
      ‘id-interesting-obs.Rmd’ using ‘UTF-8’... OK
      ‘longitudinal-data-structures.Rmd’ using ‘UTF-8’... OK
      ‘mixed-effects-models.Rmd’ using ‘UTF-8’... failed
      ‘visualisation-gallery.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exploratory-modelling.Rmd’ using rmarkdown
    
    Quitting from lines 47-56 [use-gg-highlight] (exploratory-modelling.Rmd)
    Error: processing vignette 'exploratory-modelling.Rmd' failed with diagnostics:
    ℹ In index: 1.
    ℹ With name: geom_line.
    Caused by error in `aes_param_name %in% names(non_null_default_aes) && is.na(non_null_default_aes[[
        aes_param_name]])`:
    ! 'length = 2' in coercion to 'logical(1)'
    --- failed re-building ‘exploratory-modelling.Rmd’
    
    --- re-building ‘finding-features.Rmd’ using rmarkdown
    ```

# calibrationband

<details>

* Version: 0.2.1
* GitHub: https://github.com/marius-cp/calibrationband
* Source code: https://github.com/cran/calibrationband
* Date/Publication: 2022-08-09 14:40:02 UTC
* Number of recursive dependencies: 38

Run `revdepcheck::cloud_details(, "calibrationband")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘calibrationband-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.calibrationband
    > ### Title: Plotting monotone confidence bands
    > ### Aliases: plot.calibrationband autoplot.calibrationband
    > ###   autolayer.calibrationband
    > 
    > ### ** Examples
    > 
    ...
    > p <- function(x,s){p = 1/(1+((1/x*(1-x))^(s+1)));return(p)}
    > dat <- data.frame(pr=x, y=rbinom(n,1,p(x,s)))
    > 
    > cb <- calibration_bands(x=dat$pr, y=dat$y,alpha=0.05, method="round", digits =3)
    > 
    > #simple plotting
    > plot(cb)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# cartograflow

<details>

* Version: 1.0.5
* GitHub: https://github.com/fbahoken/cartogRaflow
* Source code: https://github.com/cran/cartograflow
* Date/Publication: 2023-10-17 22:40:21 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "cartograflow")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cartograflow-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: flowgini
    > ### Title: Analysis of flow concentration (Gini coefficient)
    > ### Aliases: flowgini
    > 
    > ### ** Examples
    > 
    > library(cartograflow)
    ...
    ℹ Use `flowcum` instead.
    Warning: Use of `x$linkcum` is discouraged.
    ℹ Use `linkcum` instead.
    Warning: Use of `x$flowcum` is discouraged.
    ℹ Use `flowcum` instead.
    Warning: Use of `x$flowcum` is discouraged.
    ℹ Use `flowcum` instead.
    Error in pm[[2]] : subscript out of bounds
    Calls: flowgini ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# cases

<details>

* Version: 0.1.1
* GitHub: https://github.com/maxwestphal/cases
* Source code: https://github.com/cran/cases
* Date/Publication: 2023-05-18 08:30:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "cases")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘example_wdbc.Rmd’
      ...
    13   TRUE      FALSE
    14  FALSE      FALSE
    15  FALSE      FALSE
    
    
    > visualize(results_bm)
    
    ...
    +     regu = TRU .... [TRUNCATED] 
    
    > visualize(results_comp)
    
      When sourcing ‘package_overview.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘example_wdbc.Rmd’ using ‘UTF-8’... failed
      ‘package_overview.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘example_wdbc.Rmd’ using rmarkdown
    data_wdbc                package:cases                 R Documentation
    
    _B_r_e_a_s_t _C_a_n_c_e_r _W_i_s_c_o_n_s_i_n (_D_i_a_g_n_o_s_t_i_c) _D_a_t_a _S_e_t
    
    _D_e_s_c_r_i_p_t_i_o_n:
    
         Dataset documentation can be found at the source website and
         references below.
    ...
    Quitting from lines 160-168 [viz_comp] (package_overview.Rmd)
    Error: processing vignette 'package_overview.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘package_overview.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘example_wdbc.Rmd’ ‘package_overview.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# cats

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/cats
* Date/Publication: 2022-03-11 10:20:07 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "cats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: trial_ocs
    > ### Title: Calculates the operating characteristics of the cohort trial
    > ### Aliases: trial_ocs
    > 
    > ### ** Examples
    > 
    > 
    ...
    + safety_prob = safety_prob, Bayes_Sup1 = Bayes_Sup1, Bayes_Sup2 = Bayes_Sup2,
    + cohort_offset = cohort_offset, sr_first_pos = sr_first_pos,
    + missing_prob = missing_prob, cohort_fixed = cohort_fixed, accrual_type = accrual_type,
    + accrual_param = accrual_param, hist_lag = hist_lag, analysis_times = analysis_times,
    + time_trend = time_trend, cohorts_start = cohorts_start, cohorts_sim = cohorts_sim,
    + iter = 2, coresnum = 1, save = FALSE, ret_list = TRUE, plot_ocs = TRUE
    + )
    Error in pm[[2]] : subscript out of bounds
    Calls: trial_ocs -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘epitools’ ‘forcats’ ‘purrr’
      All declared Imports should be used.
    ```

# ceterisParibus

<details>

* Version: 0.4.2
* GitHub: https://github.com/pbiecek/ceterisParibus
* Source code: https://github.com/cran/ceterisParibus
* Date/Publication: 2020-03-28 03:10:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "ceterisParibus")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ceterisParibus)
      Loading required package: ggplot2
      Loading required package: gower
      > 
      > test_check("ceterisParibus")
      Welcome to DALEX (version: 2.4.3).
    ...
        5. └─ceterisParibus:::plot_interactive.what_if_explainer(wi_rf_all)
        6.   └─ggplot2:::`+.gg`(...)
        7.     └─ggplot2:::add_ggplot(e1, e2, e2name)
        8.       ├─ggplot2::ggplot_add(object, p, objectname)
        9.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       10.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 29 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# cfda

<details>

* Version: 0.11.0
* GitHub: https://github.com/modal-inria/cfda
* Source code: https://github.com/cran/cfda
* Date/Publication: 2023-10-07 15:50:05 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "cfda")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cfda-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compute_duration
    > ### Title: Compute duration of individuals
    > ### Aliases: compute_duration
    > 
    > ### ** Examples
    > 
    > # Simulate the Jukes-Cantor model of nucleotide replacement
    ...
    > d_JK <- generate_Markov(n = 10, K = K, P = PJK, lambda = lambda_PJK, Tmax = 10)
    > 
    > 
    > # compute duration of each individual
    > duration <- compute_duration(d_JK)
    > 
    > hist(duration)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: hist ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cfda)
      Loading required package: fda
      Loading required package: splines
      Loading required package: fds
      Loading required package: rainbow
      Loading required package: MASS
    ...
        7. └─cfda:::hist.njump(njump)
        8.   └─ggplot2:::`+.gg`(...)
        9.     └─ggplot2:::add_ggplot(e1, e2, e2name)
       10.       ├─ggplot2::ggplot_add(object, p, objectname)
       11.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       12.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 2 | WARN 1 | SKIP 12 | PASS 351 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cfda.Rmd’
      ...
    
    > head(nJump)
     1  2  3  4  5  6 
    17  9  3 13 10 13 
    
    > hist(nJump)
    
      When sourcing ‘cfda.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘cfda.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cfda.Rmd’ using rmarkdown
    ```

# cheem

<details>

* Version: 0.4.0.0
* GitHub: https://github.com/nspyrison/cheem
* Source code: https://github.com/cran/cheem
* Date/Publication: 2023-11-08 21:30:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "cheem")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cheem)
      --------------------------------------------------------
      cheem --- version 0.4.0.0
      Please share bugs, suggestions, and feature requests at:
      https://github.com/nspyrison/cheem/issues/
      --------------------------------------------------------
    ...
       13. │   ├─utils::modifyList(x %||% list(), y %||% list(), ...)
       14. │   │ └─base::stopifnot(is.list(x), is.list(val))
       15. │   └─x %||% list()
       16. ├─plotly::ggplotly(...)
       17. └─plotly:::ggplotly.ggplot(...)
       18.   └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting-started-with-cheem.Rmd’
      ...
    
    > knitr::opts_chunk$set(echo = TRUE, include = TRUE, 
    +     results = "show", eval = FALSE, message = FALSE, warning = FALSE, 
    +     error = FALSE, co .... [TRUNCATED] 
    
    > knitr::include_graphics("../inst/shiny_apps/cheem/www/lime_nonlinear.png")
    
      When sourcing ‘getting-started-with-cheem.R’:
    Error: Cannot find the file(s): "../inst/shiny_apps/cheem/www/lime_nonlinear.png"
    Execution halted
    
      ‘getting-started-with-cheem.Rmd’ using ‘UTF-8’... failed
    ```

# chillR

<details>

* Version: 0.75
* GitHub: NA
* Source code: https://github.com/cran/chillR
* Date/Publication: 2023-11-27 22:20:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "chillR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘chillR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_scenarios
    > ### Title: Plot historic and future scenarios for climate-related metrics
    > ###   ('ggplot2' version)
    > ### Aliases: plot_scenarios
    > 
    > ### ** Examples
    > 
    ...
    >                                    
    > # Plot the climate scenarios
    > 
    > plot_scenarios(climate_scenario_list, metric = 'Chill_Portions',
    +                add_historic = TRUE, size = 2, shape = 3, color = 'blue',
    +                outlier_shape = 12, historic_color = 'skyblue',
    +                group_by = c("Year", "Scenario"))
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# chronicle

<details>

* Version: 0.3
* GitHub: NA
* Source code: https://github.com/cran/chronicle
* Date/Publication: 2021-06-25 05:00:02 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "chronicle")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘chronicle-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_barplot
    > ### Title: Create a bar plot from a data frame through ggplotly
    > ### Aliases: make_barplot
    > 
    > ### ** Examples
    > 
    > make_barplot(dt = iris, bars = 'Species', value = 'Sepal.Length')
    Error in pm[[2]] : subscript out of bounds
    Calls: make_barplot -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘chronicle.Rmd’
      ...
    +     filename = "quick_demo", title = "A quick chronicle demo", 
    +     author =  .... [TRUNCATED] 
    
    Quitting from lines 34-46 [unnamed-chunk-3] (quick_demo.Rmd)
    
      When sourcing ‘chronicle.R’:
    Error: ℹ In index: 1.
    Caused by error in `pm[[2]]`:
    ! subscript out of bounds
    Execution halted
    
      ‘chronicle.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘chronicle.Rmd’ using rmarkdown
    
    Quitting from lines 38-67 [unnamed-chunk-3] (chronicle.Rmd)
    Error: processing vignette 'chronicle.Rmd' failed with diagnostics:
    ℹ In index: 1.
    Caused by error in `pm[[2]]`:
    ! subscript out of bounds
    --- failed re-building ‘chronicle.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘chronicle.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘dplyr’ ‘prettydoc’ ‘rmdformats’ ‘skimr’
      All declared Imports should be used.
    ```

# CINNA

<details>

* Version: 1.2.2
* GitHub: NA
* Source code: https://github.com/cran/CINNA
* Date/Publication: 2023-08-08 16:40:02 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "CINNA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CINNA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pca_centralities
    > ### Title: PCA Centrality Measures
    > ### Aliases: pca_centralities
    > 
    > ### ** Examples
    > 
    > # Create a data frame with multiple observations
    ...
     13. │               └─e1 %+% e2
     14. │                 └─ggplot2:::add_ggplot(e1, e2, e2name)
     15. │                   ├─ggplot2::ggplot_add(object, p, objectname)
     16. │                   └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     17. │                     └─ggplot2:::new_layer_names(object, names(plot$layers))
     18. └─base::.handleSimpleError(...)
     19.   └─purrr (local) h(simpleError(msg, call))
     20.     └─cli::cli_abort(...)
     21.       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CINNA.Rmd’
      ...
    > calc_cent <- calculate_centralities(zachary, include = pr_cent[1:10])
    
    > pca_centralities(calc_cent)
    
      When sourcing ‘CINNA.R’:
    Error: ℹ In index: 1.
    ℹ With name: contrib.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    Execution halted
    
      ‘CINNA.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘CINNA.Rmd’ using rmarkdown
    
    Quitting from lines 231-234 [unnamed-chunk-11] (CINNA.Rmd)
    Error: processing vignette 'CINNA.Rmd' failed with diagnostics:
    ℹ In index: 1.
    ℹ With name: contrib.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    --- failed re-building ‘CINNA.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘CINNA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘circlize’ ‘utils’
      All declared Imports should be used.
    ```

# circhelp

<details>

* Version: 1.1
* GitHub: https://github.com/achetverikov/circhelp
* Source code: https://github.com/cran/circhelp
* Date/Publication: 2024-07-04 17:10:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "circhelp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘circhelp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: remove_cardinal_biases
    > ### Title: Remove cardinal biases
    > ### Aliases: remove_cardinal_biases
    > 
    > ### ** Examples
    > 
    > 
    > # Data in orientation domain from Pascucci et al. (2019, PLOS Bio),
    > # https://doi.org/10.5281/zenodo.2544946
    > 
    > ex_data <- Pascucci_et_al_2019_data[observer == 4, ]
    > remove_cardinal_biases(ex_data$err, ex_data$orientation, plots = "show")
    Error in as.unit(value) : object is not coercible to a unit
    Calls: remove_cardinal_biases ... assemble_guides -> guides_build -> [<- -> [<-.unit -> as.unit
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cardinal_biases.Rmd’
      ...
    +     90)) +  .... [TRUNCATED] 
    
    > ex_subj_data <- data[observer == 4, ]
    
    > res <- remove_cardinal_biases(ex_subj_data$err, ex_subj_data$orientation, 
    +     plots = "show")
    
      When sourcing ‘cardinal_biases.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘cardinal_biases.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cardinal_biases.Rmd’ using rmarkdown
    ```

# clifro

<details>

* Version: 3.2-5
* GitHub: https://github.com/ropensci/clifro
* Source code: https://github.com/cran/clifro
* Date/Publication: 2021-05-24 05:50:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "clifro")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > library(clifro)
      > 
      > test_check("clifro")
      [ FAIL 1 | WARN 1 | SKIP 4 | PASS 10 ]
      
    ...
      • On CRAN (4): 'test-cf_find_station.R:4:3', 'test-cf_last_query.R:4:3',
        'test-cf_query.R:4:3', 'test-cf_station.R:4:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-windrose.R:15:3'): windrose ──────────────────────────────────
      tt$labels inherits from `'NULL'` not `'character'`.
      
      [ FAIL 1 | WARN 1 | SKIP 4 | PASS 10 ]
      Error: Test failures
      Execution halted
    ```

# clinDataReview

<details>

* Version: 1.6.1
* GitHub: https://github.com/openanalytics/clinDataReview
* Source code: https://github.com/cran/clinDataReview
* Date/Publication: 2024-06-18 09:10:05 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "clinDataReview")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘clinDataReview-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scatterplotClinData
    > ### Title: Scatterplot of variables of interest for clinical data
    > ###   visualization.
    > ### Aliases: scatterplotClinData
    > 
    > ### ** Examples
    > 
    ...
    + 	data = dataPlot, 
    + 	xVar = "ADY",
    + 	yVar = "LBSTRESN",
    + 	aesPointVar = list(color = "TRTP", fill = "TRTP"),
    + 	aesLineVar = list(group = "USUBJID", color = "TRTP"),
    + 	labelVars = labelVars
    + )
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: scatterplotClinData ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(clinDataReview)
      > 
      > test_check("clinDataReview")
        adding: report.html (deflated 63%)
        adding: report_dependencies169f5c7b66d/ (stored 0%)
        adding: report_dependencies169f5c7b66d/file169f105f2e2e.html (deflated 8%)
    ...
       11.     ├─base::withCallingHandlers(...)
       12.     └─ggplot2:::`+.gg`(gg, do.call(layerFunction, argsGeom))
       13.       └─ggplot2:::add_ggplot(e1, e2, e2name)
       14.         ├─ggplot2::ggplot_add(object, p, objectname)
       15.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       16.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 35 | WARN 0 | SKIP 31 | PASS 453 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘clinDataReview-dataPreprocessing.Rmd’ using rmarkdown
    --- finished re-building ‘clinDataReview-dataPreprocessing.Rmd’
    
    --- re-building ‘clinDataReview-dataVisualization.Rmd’ using rmarkdown
    
    Quitting from lines 167-208 [timeProfiles] (clinDataReview-dataVisualization.Rmd)
    Error: processing vignette 'clinDataReview-dataVisualization.Rmd' failed with diagnostics:
    argument is of length zero
    ...
    --- failed re-building ‘clinDataReview-dataVisualization.Rmd’
    
    --- re-building ‘clinDataReview-reporting.Rmd’ using rmarkdown
    --- finished re-building ‘clinDataReview-reporting.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘clinDataReview-dataVisualization.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   4.3Mb
    ```

# clinUtils

<details>

* Version: 0.2.0
* GitHub: https://github.com/openanalytics/clinUtils
* Source code: https://github.com/cran/clinUtils
* Date/Publication: 2024-05-17 14:50:06 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "clinUtils")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘clinUtils-vignette.Rmd’
      ...
    
        layout
    
    
    > listPlotsInteractiveLB <- sapply(listPlotsLB, function(ggplot) ggplotly(ggplot) %>% 
    +     partial_bundle(), simplify = FALSE)
    
      When sourcing ‘clinUtils-vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘clinUtils-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘clinUtils-vignette.Rmd’ using rmarkdown
    ```

## Newly fixed

*   checking running R code from vignettes ... WARNING
    ```
    Errors in running code in vignettes:
    when running code in ‘clinUtils-vignette.Rmd’
      ...
    
    
    
    
    
    Quitting from lines 2-4 [lab-hist-interactive1]
    
      When sourcing ‘clinUtils-vignette.R’:
    Error: there is no package called 'webshot'
    Execution halted
    
      ‘clinUtils-vignette.Rmd’ using ‘UTF-8’... failed
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc   6.5Mb
    ```

# cloneRate

<details>

* Version: 0.2.3
* GitHub: https://github.com/bdj34/cloneRate
* Source code: https://github.com/cran/cloneRate
* Date/Publication: 2023-09-22 15:40:02 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "cloneRate")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cloneRate-dataAnalysis.Rmd’
      ...
    
    > fitColor <- colorPal[6]
    
    > ggplot(PD9478_long, aes(x = Age, y = VAF)) + theme_bw() + 
    +     coord_cartesian(xlim = c(min(x), max(x)), ylim = c(-0.01, 
    +         0.52), expand  .... [TRUNCATED] 
    
      When sourcing ‘cloneRate-dataAnalysis.R’:
    Error: `expand` must be a logical vector, not the number 0.
    Execution halted
    
      ‘cloneRate-dataAnalysis.Rmd’ using ‘UTF-8’... failed
      ‘cloneRate-simulate.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cloneRate-dataAnalysis.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 47.4Mb
      sub-directories of 1Mb or more:
        doc    1.0Mb
        libs  45.2Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# clustEff

<details>

* Version: 0.3.1
* GitHub: NA
* Source code: https://github.com/cran/clustEff
* Date/Publication: 2024-01-23 08:52:55 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "clustEff")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘clustEff-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clustEff-package
    > ### Title: Clusters of effects curves
    > ### Aliases: clustEff-package
    > ### Keywords: package
    > 
    > ### ** Examples
    > 
    ...
     13. │           └─ggplot2:::`+.gg`(p, do.call(geom_line, option))
     14. │             └─ggplot2:::add_ggplot(e1, e2, e2name)
     15. │               ├─ggplot2::ggplot_add(object, p, objectname)
     16. │               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     17. │                 └─ggplot2:::new_layer_names(object, names(plot$layers))
     18. └─base::.handleSimpleError(...)
     19.   └─purrr (local) h(simpleError(msg, call))
     20.     └─cli::cli_abort(...)
     21.       └─rlang::abort(...)
    Execution halted
    ```

# ClustImpute

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/ClustImpute
* Date/Publication: 2021-05-31 07:40:11 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "ClustImpute")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Example_on_simulated_data.Rmd’
      ...
    > dat4plot$true_clust_fct <- factor(true_clust)
    
    > p_base <- ggplot(dat4plot, aes(x = x, y = y, color = true_clust_fct)) + 
    +     geom_point()
    
    > ggExtra::ggMarginal(p_base, groupColour = TRUE, groupFill = TRUE)
    
      When sourcing ‘Example_on_simulated_data.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Example_on_simulated_data.Rmd’ using ‘UTF-8’... failed
      ‘description_of_algorithm.Rnw’ using ‘UTF-8’... OK
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Example_on_simulated_data.Rmd’ using rmarkdown
    
    Quitting from lines 49-53 [unnamed-chunk-3] (Example_on_simulated_data.Rmd)
    Error: processing vignette 'Example_on_simulated_data.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Example_on_simulated_data.Rmd’
    
    --- re-building ‘description_of_algorithm.Rnw’ using Sweave
    Error: processing vignette 'description_of_algorithm.Rnw' failed with diagnostics:
    ...
    l.6 \usepackage
                   {Sweave}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘description_of_algorithm.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘Example_on_simulated_data.Rmd’ ‘description_of_algorithm.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# cmstatr

<details>

* Version: 0.9.3
* GitHub: https://github.com/cmstatr/cmstatr
* Source code: https://github.com/cran/cmstatr
* Date/Publication: 2024-03-14 14:30:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "cmstatr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cmstatr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: nested_data_plot
    > ### Title: Create a plot of nested sources of variation
    > ### Aliases: nested_data_plot
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    
        intersect, setdiff, setequal, union
    
    > carbon.fabric.2 %>%
    +   filter(test == "WT" & condition == "RTD") %>%
    +   nested_data_plot(strength,
    +                    groups = c(batch, panel))
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: %>% ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(cmstatr)
      > 
      > test_check("cmstatr")
      Starting 2 test processes
      [ FAIL 6 | WARN 0 | SKIP 6 | PASS 1396 ]
    ...
        6.   └─cmstatr:::draw_vert_lines_to_labels(g, elm_list, vline_args)
        7.     └─ggplot2:::`+.gg`(...)
        8.       └─ggplot2:::add_ggplot(e1, e2, e2name)
        9.         ├─ggplot2::ggplot_add(object, p, objectname)
       10.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       11.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 6 | WARN 0 | SKIP 6 | PASS 1396 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cmstatr_Graphing.Rmd’
      ...
    +     by = "condition") %>% inner_join(a_basis_pooled_results, 
    +     by = "condition") .... [TRUNCATED] 
    
    > carbon.fabric.2 %>% mutate(panel = as.character(panel)) %>% 
    +     filter(test == "WT") %>% nested_data_plot(strength, groups = c(batch, 
    +     pane .... [TRUNCATED] 
    
      When sourcing ‘cmstatr_Graphing.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘adktest.Rmd’ using ‘UTF-8’... OK
      ‘cmstatr_Graphing.Rmd’ using ‘UTF-8’... failed
      ‘cmstatr_Tutorial.Rmd’ using ‘UTF-8’... OK
      ‘cmstatr_Validation.Rmd’ using ‘UTF-8’... OK
      ‘hk_ext.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘adktest.Rmd’ using rmarkdown
    --- finished re-building ‘adktest.Rmd’
    
    --- re-building ‘cmstatr_Graphing.Rmd’ using rmarkdown
    ```

# codaredistlm

<details>

* Version: 0.1.0
* GitHub: https://github.com/tystan/codaredistlm
* Source code: https://github.com/cran/codaredistlm
* Date/Publication: 2022-12-22 19:50:06 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "codaredistlm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘codaredistlm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_delta_comp
    > ### Title: Plot redistributed time-use predictions from compositional ilr
    > ###   multiple linear regression model fit
    > ### Aliases: plot_delta_comp
    > 
    > ### ** Examples
    > 
    ...
    |     |      x|
    |:----|------:|
    |0%   | 1439.9|
    |25%  | 1440.0|
    |50%  | 1440.0|
    |75%  | 1440.0|
    |100% | 1440.1|
    ---
    
    ---
    ```

# coefplot

<details>

* Version: 1.2.8
* GitHub: NA
* Source code: https://github.com/cran/coefplot
* Date/Publication: 2022-01-14 09:42:47 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "coefplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘coefplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: buildModelCI
    > ### Title: buildModelCI
    > ### Aliases: buildModelCI
    > 
    > ### ** Examples
    > 
    > 
    ...
    cut.C         327.4816 model1
    cut.Q        -574.8626 model1
    cut.L        1187.6004 model1
    carat        7843.1229 model1
    (Intercept) -2732.2382 model1
    > coefplot(model1)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# CohortPlat

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/CohortPlat
* Date/Publication: 2022-02-14 09:30:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "CohortPlat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CohortPlat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_trial
    > ### Title: Plots the cohort trial study overview given stage data.
    > ### Aliases: plot_trial
    > 
    > ### ** Examples
    > 
    > 
    ...
    + stage_data = stage_data, cohort_random = cohort_random, cohorts_max = cohorts_max,
    + sr_drugs_pos = sr_drugs_pos, target_rr = target_rr, sharing_type = sharing_type,
    + safety_prob = safety_prob, Bayes_Sup = Bayes_Sup, prob_rr_transform = prob_rr_transform,
    + cohort_offset = cohort_offset, Bayes_Fut = Bayes_Fut, sr_first_pos = sr_first_pos
    + )
    > 
    > plot_trial(res_list, unit = "n")
    Error in pm[[2]] : subscript out of bounds
    Calls: plot_trial -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘my-vignette.Rmd’
      ...
    
    > set.seed(50)
    
    > ocs1 <- trial_ocs(n_int = n_int, n_fin = n_fin, rr_comb = rr_comb, 
    +     rr_mono = rr_mono, rr_back = rr_back, rr_plac = rr_plac, 
    +     rr_transfo .... [TRUNCATED] 
    
      When sourcing ‘my-vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘my-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘my-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 1043-1073 [unnamed-chunk-20] (my-vignette.Rmd)
    Error: processing vignette 'my-vignette.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘my-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘my-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# complmrob

<details>

* Version: 0.7.0
* GitHub: https://github.com/dakep/complmrob
* Source code: https://github.com/cran/complmrob
* Date/Publication: 2019-09-17 18:10:02 UTC
* Number of recursive dependencies: 31

Run `revdepcheck::cloud_details(, "complmrob")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘complmrob-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.complmrob
    > ### Title: Diagnostic plots for the robust regression model with
    > ###   compositional covariates
    > ### Aliases: plot.complmrob
    > 
    > ### ** Examples
    > 
    > data <- data.frame(lifeExp = state.x77[, "Life Exp"], USArrests[ , -3])
    > mUSArr <- complmrob(lifeExp ~ ., data = data)
    > plot(mUSArr)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# conjoint

<details>

* Version: 1.41
* GitHub: NA
* Source code: https://github.com/cran/conjoint
* Date/Publication: 2018-07-26 13:00:03 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "conjoint")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘conjoint-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: caSegmentation
    > ### Title: Function caSegmentation divides respondents on clusters
    > ### Aliases: caSegmentation
    > ### Keywords: multivariate
    > 
    > ### ** Examples
    > 
    ...
    Available components:
    
    [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    [6] "betweenss"    "size"         "iter"         "ifault"      
    > util<-as.data.frame(segments$util)
    > set.seed(123)
    > ggplot2::autoplot(kmeans(util,3),data=util,label=TRUE,label.size=4,frame=TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# conquestr

<details>

* Version: 1.3.4
* GitHub: NA
* Source code: https://github.com/cran/conquestr
* Date/Publication: 2024-07-24 06:00:01 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "conquestr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plotting.Rmd’
      ...
    > myRout <- ConQuestRout()
    no rout file provided, loading the example rout file instead
    
    > myPlot <- plotRout(myRout)
    
    > myPlot
    
    ...
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘data-cleaning-functions-in-conquestr.Rmd’ using ‘UTF-8’... OK
      ‘generateResponses.Rmd’ using ‘UTF-8’... OK
      ‘intro-to-conquestr.Rmd’ using ‘UTF-8’... OK
      ‘itanal-in-conquestr.Rmd’ using ‘UTF-8’... OK
      ‘plotting.Rmd’ using ‘UTF-8’... failed
      ‘responseProbs.Rmd’ using ‘UTF-8’... OK
      ‘test_item_review_sheet_markdown.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘data-cleaning-functions-in-conquestr.Rmd’ using rmarkdown
    --- finished re-building ‘data-cleaning-functions-in-conquestr.Rmd’
    
    --- re-building ‘generateResponses.Rmd’ using rmarkdown
    ```

# CoreMicrobiomeR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/CoreMicrobiomeR
* Date/Publication: 2024-04-03 20:03:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "CoreMicrobiomeR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘CoreMicrobiomeR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: group_bar_plots
    > ### Title: Grouped Bar Plots Based on Sample Size
    > ### Aliases: group_bar_plots
    > 
    > ### ** Examples
    > 
    > #To run input data
    ...
    +  top_percentage = 10  # Adjust the percentage as needed for core/non-core OTUs
    + )
    Warning encountered during diversity analysis:you have empty rows: their dissimilarities may be
                     meaningless in method “bray”
    > #To run grouped bar plot function
    > plot_group_bar <- group_bar_plots(core_1$final_otu_table_bef_filter,
    + core_1$final_otu_aft_filter, 10)
    Error in pm[[2]] : subscript out of bounds
    Calls: group_bar_plots -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# correlationfunnel

<details>

* Version: 0.2.0
* GitHub: https://github.com/business-science/correlationfunnel
* Source code: https://github.com/cran/correlationfunnel
* Date/Publication: 2020-06-09 04:40:03 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "correlationfunnel")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following object is masked from 'package:testthat':
      
    ...
          ▆
       1. ├─correlationfunnel::plot_correlation_funnel(...) at test-plot_correlation_funnel.R:23:1
       2. └─correlationfunnel:::plot_correlation_funnel.data.frame(...)
       3.   ├─plotly::ggplotly(g, tooltip = "text")
       4.   └─plotly:::ggplotly.ggplot(g, tooltip = "text")
       5.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 3 | SKIP 0 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# corrViz

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/corrViz
* Date/Publication: 2023-06-30 11:40:07 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "corrViz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘corrViz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: animSolar
    > ### Title: animSolar
    > ### Aliases: animSolar
    > 
    > ### ** Examples
    > 
    > cm <- cor(mtcars)
    ...
      All aesthetics have length 1, but the data has 250 rows.
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    Warning in geom_text(data = solar_system, aes(x = 0, y = 0, label = sun),  :
      All aesthetics have length 1, but the data has 250 rows.
    ℹ Please consider using `annotate()` or provide this layer with data containing
      a single row.
    Error in pm[[2]] : subscript out of bounds
    Calls: animSolar -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘corrViz.Rmd’
      ...
    > library(corrViz)
    
    > cm <- cor(mtcars)
    
    > corrHeatmap(mat = cm, display = "all", reorder = TRUE, 
    +     pal = colorRampPalette(c("darkblue", "white", "darkred"))(100))
    
      When sourcing ‘corrViz.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘corrViz.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘corrViz.Rmd’ using rmarkdown
    
    Quitting from lines 76-81 [heatmap] (corrViz.Rmd)
    Error: processing vignette 'corrViz.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘corrViz.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘corrViz.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        doc   6.7Mb
    ```

# corx

<details>

* Version: 1.0.7.2
* GitHub: https://github.com/conig/corx
* Source code: https://github.com/cran/corx
* Date/Publication: 2023-06-16 04:10:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "corx")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(corx)
      > 
      > test_check("corx")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 69 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       14. │               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       15. │                 └─ggplot2:::new_layer_names(object, names(plot$layers))
       16. └─base::.handleSimpleError(...)
       17.   └─purrr (local) h(simpleError(msg, call))
       18.     └─cli::cli_abort(...)
       19.       └─rlang::abort(...)
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 69 ]
      Error: Test failures
      Execution halted
    ```

# cosinor2

<details>

* Version: 0.2.1
* GitHub: https://github.com/amutak/cosinor2
* Source code: https://github.com/cran/cosinor2
* Date/Publication: 2018-10-15 16:10:03 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "cosinor2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘cosinor2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cosinor.PR
    > ### Title: Percent Rhythm
    > ### Aliases: cosinor.PR
    > 
    > ### ** Examples
    > 
    > fit.temperature<-cosinor.lm(Temperature~time(Time), period = 24, data = temperature_zg)
    ...
    1 0.9838823      0.9680243       0
    > 
    > fit.november<-population.cosinor.lm(data = PANAS_november, time = PANAS_time,
    + period = 7)
         MESOR Amplitude Acrophase
    1 1.435419 0.2662682 -5.544496
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: population.cosinor.lm ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cosinor2.Rmd’
      ...
    Loading required package: cosinor
    
    > fit.panas.cosinor <- population.cosinor.lm(data = PANAS_november, 
    +     time = PANAS_time, period = 7)
         MESOR Amplitude Acrophase
    1 1.435419 0.2662682 -5.544496
    
      When sourcing ‘cosinor2.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘cosinor2.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘cosinor2.Rmd’ using rmarkdown
    
    Quitting from lines 47-48 [unnamed-chunk-2] (cosinor2.Rmd)
    Error: processing vignette 'cosinor2.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘cosinor2.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘cosinor2.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# CoSMoS

<details>

* Version: 2.1.0
* GitHub: https://github.com/TycheLab/CoSMoS
* Source code: https://github.com/cran/CoSMoS
* Date/Publication: 2021-05-29 23:20:08 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "CoSMoS")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘vignette.Rmd’
      ...
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    
    > precip_ggamma <- analyzeTS(TS = precip, season = "month", 
    +     dist = "ggamma", acsID = "weibull", lag.max = 12)
    
    > reportTS(aTS = precip_ggamma, method = "dist") + theme_light()
    
      When sourcing ‘vignette.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘vignette.Rmd’ using rmarkdown
    ```

# countfitteR

<details>

* Version: 1.4
* GitHub: https://github.com/BioGenies/countfitteR
* Source code: https://github.com/cran/countfitteR
* Date/Publication: 2020-09-30 21:30:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "countfitteR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(countfitteR)
      > 
      > test_check("countfitteR")
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 34 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('testing.R:45:3'): plot_fit ────────────────────────────────────────
      p$labels[[1]] not equal to "x".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 34 ]
      Error: Test failures
      Execution halted
    ```

# coursekata

<details>

* Version: 0.18.0
* GitHub: https://github.com/coursekata/coursekata-r
* Source code: https://github.com/cran/coursekata
* Date/Publication: 2024-08-16 20:20:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "coursekata")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘coursekata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: middle
    > ### Title: Find a percentage of a distribution
    > ### Aliases: middle tails lower upper
    > 
    > ### ** Examples
    > 
    > 
    ...
    > tails(1:10, .5)
     [1]  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE
    > 
    > sampling_distribution <- do(1000) * mean(rnorm(100, 5, 10))
    > sampling_distribution %>%
    +   gf_histogram(~mean, data = sampling_distribution, fill = ~ middle(mean, .68)) %>%
    +   gf_refine(scale_fill_manual(values = c("blue", "coral")))
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: %>% ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(coursekata)
      Loading required package: dslabs
      Loading required package: fivethirtyeight
      Some larger datasets need to be installed separately, like senators and
      house_district_forecast. To install these, we recommend you install the
      fivethirtyeightdata package by running:
    ...
      • gf_model-visual/gf-violin-cond-mod-y-on-x-pred-on-color.svg
      • gf_model-visual/gf-violin-cond-mod-y-on-x-pred-on-facet.svg
      • gf_model-visual/gf-violin-cond-mod-y-on-y-pred-on-color.svg
      • gf_model-visual/gf-violin-cond-mod-y-on-y-pred-on-facet.svg
      • gf_model-visual/gf-violin-cond-mod-y-on-y.svg
      • gf_model-visual/gf-violin-horizontal-mull-mod-y-on-x.svg
      • gf_model-visual/gf-violin-mull-mod-y-on-x-2.svg
      • gf_model-visual/gf-violin-null-mod-y-on-y.svg
      Error: Test failures
      Execution halted
    ```

# covidcast

<details>

* Version: 0.5.2
* GitHub: https://github.com/cmu-delphi/covidcast
* Source code: https://github.com/cran/covidcast
* Date/Publication: 2023-07-12 23:40:06 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "covidcast")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(covidcast)
      We encourage COVIDcast API users to register on our mailing list:
      https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api
      We'll send announcements about new data sources, package updates,
      server maintenance, and new features.
      > 
    ...
      • plot/default-county-choropleth.svg
      • plot/default-hrr-choropleth-with-include.svg
      • plot/default-msa-choropleth-with-include.svg
      • plot/default-state-choropleth-with-include.svg
      • plot/default-state-choropleth-with-range.svg
      • plot/state-choropleth-with-no-metadata.svg
      • plot/state-line-graph-with-range.svg
      • plot/state-line-graph-with-stderrs.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘external-data.Rmd’
      ...
    Warning: Metadata for signal mean and standard deviation not available; defaulting to observed mean and standard deviation to set plot range.
    Warning in ggplot2::guide_colorbar(title = NULL, horizontal = TRUE, barheight = legend_height,  :
      Arguments in `...` must be used.
    ✖ Problematic argument:
    • horizontal = TRUE
    ℹ Did you misspell an argument name?
    
    ...
    Error: Rate limit exceeded when fetching data from API anonymously. See the "API keys" section of the `covidcast_signal()` documentation for information on registering for an API key.
    ℹ Message from server:
    ℹ Rate limit exceeded for anonymous queries. To remove this limit, register a free API key at https://api.delphi.cmu.edu/epidata/admin/registration_form
    Execution halted
    
      ‘correlation-utils.Rmd’ using ‘UTF-8’... OK
      ‘covidcast.Rmd’ using ‘UTF-8’... OK
      ‘external-data.Rmd’ using ‘UTF-8’... failed
      ‘multi-signals.Rmd’ using ‘UTF-8’... OK
      ‘plotting-signals.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘correlation-utils.Rmd’ using rmarkdown
    --- finished re-building ‘correlation-utils.Rmd’
    
    --- re-building ‘covidcast.Rmd’ using rmarkdown
    
    Quitting from lines 38-45 [unnamed-chunk-1] (covidcast.Rmd)
    Error: processing vignette 'covidcast.Rmd' failed with diagnostics:
    Rate limit exceeded when fetching data from API anonymously. See the "API keys" section of the `covidcast_signal()` documentation for information on registering for an API key.
    ℹ Message from server:
    ...
    ℹ Message from server:
    ℹ Rate limit exceeded for anonymous queries. To remove this limit, register a free API key at https://api.delphi.cmu.edu/epidata/admin/registration_form
    --- failed re-building ‘plotting-signals.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘covidcast.Rmd’ ‘external-data.Rmd’ ‘multi-signals.Rmd’
      ‘plotting-signals.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
    ```

# Coxmos

<details>

* Version: 1.0.2
* GitHub: https://github.com/BiostatOmics/Coxmos
* Source code: https://github.com/cran/Coxmos
* Date/Publication: 2024-03-25 20:32:38 UTC
* Number of recursive dependencies: 194

Run `revdepcheck::cloud_details(, "Coxmos")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Coxmos-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getAutoKM
    > ### Title: getAutoKM
    > ### Aliases: getAutoKM
    > 
    > ### ** Examples
    > 
    > data("X_proteomic")
    ...
    > X_train <- X_proteomic[index_train,1:50]
    > Y_train <- Y_proteomic[index_train,]
    > X_test <- X_proteomic[-index_train,1:50]
    > Y_test <- Y_proteomic[-index_train,]
    > splsicox.model <- splsicox(X_train, Y_train, n.comp = 2, penalty = 0.5, x.center = TRUE,
    + x.scale = TRUE)
    > getAutoKM(type = "LP", model = splsicox.model)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: getAutoKM ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Coxmos-MO-pipeline.Rmd’
      ...
    
    $proteomic
    
    
    > LST_KM_RES_LP <- getAutoKM(type = "LP", model = lst_models$`SB.sPLS-DRCOX`, 
    +     comp = 1:4, top = 10, ori_data = T, BREAKTIME = NULL, only_sig =  .... [TRUNCATED] 
    
    ...
    Warning in data("Y_proteomic") : data set ‘Y_proteomic’ not found
    
    > X <- X_proteomic
    
      When sourcing ‘Coxmos-pipeline.R’:
    Error: object 'X_proteomic' not found
    Execution halted
    
      ‘Coxmos-MO-pipeline.Rmd’ using ‘UTF-8’... failed
      ‘Coxmos-pipeline.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   2.5Mb
        doc    2.9Mb
    ```

# cpr

<details>

* Version: 0.4.0
* GitHub: https://github.com/dewittpe/cpr
* Source code: https://github.com/cran/cpr
* Date/Publication: 2024-02-15 15:40:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "cpr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cpr.Rmd’
      ...
    Warning: Removed 25 rows containing missing values or values outside the scale range
    (`geom_rug()`).
    Warning: Removed 26 rows containing missing values or values outside the scale range
    (`geom_rug()`).
    Warning: Removed 38 rows containing missing values or values outside the scale range
    (`geom_rug()`).
    
      When sourcing ‘cpr.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘cnr.Rmd’ using ‘UTF-8’... OK
      ‘cpr.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cnr.Rmd’ using rmarkdown
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘test-bsplineD.R’
      Running ‘test-bsplines.R’
      Running ‘test-btensor.R’
      Running ‘test-build_tensor.R’
      Running ‘test-cn.R’
      Running ‘test-cnr.R’
      Running ‘test-coef_vcov.R’
    Running the tests in ‘tests/test-coef_vcov.R’ failed.
    Complete output:
      > # Tests for extracting coefficients and vcov matrix from regression fits
    ...
      +   stopifnot(identical(names(COEF_VCOV), c("theta", "coef", "vcov_theta", "vcov")))
      +   stopifnot(identical(COEF_VCOV$theta, numeric(0)))
      +   stopifnot(identical(COEF_VCOV$coef, fixef(fit)))
      +   stopifnot(identical(COEF_VCOV$vcov_theta, matrix(0)[FALSE, FALSE]))
      +   stopifnot(identical(COEF_VCOV$vcov, as.matrix(vcov(fit))))
      + })
      Error in initializePtr() : 
        function 'cholmod_factor_ldetA' not provided by package 'Matrix'
      Calls: with ... initialize -> <Anonymous> -> initializePtr -> .Call
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
        libs   4.1Mb
    ```

# cpsvote

<details>

* Version: 0.1.0
* GitHub: https://github.com/Reed-EVIC/cpsvote
* Source code: https://github.com/cran/cpsvote
* Date/Publication: 2020-11-05 16:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "cpsvote")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘voting.Rmd’
      ...
    +          .... [TRUNCATED] 
    
    > library(usmap)
    
    > cps16 %>% as_survey_design(weights = turnout_weight) %>% 
    +     mutate(state = STATE) %>% group_by(state) %>% summarize(turnout = survey_mean(hurach .... [TRUNCATED] 
    
      When sourcing ‘voting.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘add-variables.Rmd’ using ‘UTF-8’... OK
      ‘background.Rmd’ using ‘UTF-8’... OK
      ‘basics.Rmd’ using ‘UTF-8’... OK
      ‘voting.Rmd’ using ‘UTF-8’... failed
    ```

# crimeutils

<details>

* Version: 0.5.1
* GitHub: https://github.com/jacobkap/crimeutils
* Source code: https://github.com/cran/crimeutils
* Date/Publication: 2022-12-07 15:10:07 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "crimeutils")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘crimeutils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_linetype_crim
    > ### Title: A set of linetypes
    > ### Aliases: scale_linetype_crim
    > 
    > ### ** Examples
    > 
    > ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp, linetype = as.character(cyl))) +
    +   ggplot2::geom_line(size = 1) +
    +   scale_linetype_crim() +
    +   theme_crim()
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# crmPack

<details>

* Version: 1.0.6
* GitHub: https://github.com/openpharma/crmPack
* Source code: https://github.com/cran/crmPack
* Date/Publication: 2024-06-26 15:00:14 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "crmPack")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘crmPack-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DataMixture-class
    > ### Title: Class for the data with mixture sharing
    > ### Aliases: DataMixture-class .DataMixture
    > ### Keywords: classes
    > 
    > ### ** Examples
    > 
    ...
    +                            refDose = 50)
    > 
    > nodata <- Data(doseGrid=doseGrid)
    > 
    > priorSamples <- mcmc(nodata, model, options)
    > plot(priorSamples, model, nodata)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘example.Rnw’
      ...
    [1] -5.070681
    
    > newDLTmodel@phi2
    [1] 1.125107
    
    > print(plot(samples, model, data))
    
      When sourcing ‘example.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘guidelines.Rmd’ using ‘UTF-8’... OK
      ‘example.Rnw’ using ‘UTF-8’... failed
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘guidelines.Rmd’ using rmarkdown
    --- finished re-building ‘guidelines.Rmd’
    
    --- re-building ‘example.Rnw’ using Sweave
    Loading required package: ggplot2
    Registered S3 method overwritten by 'crmPack':
      method       from  
      print.gtable gtable
    Type crmPackHelp() to open help browser
    ...
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    
    --- failed re-building ‘example.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘example.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# crosshap

<details>

* Version: 1.4.0
* GitHub: https://github.com/jacobimarsh/crosshap
* Source code: https://github.com/cran/crosshap
* Date/Publication: 2024-03-31 15:40:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "crosshap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘crosshap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: build_bot_halfeyeplot
    > ### Title: Bot hap-pheno raincloud plot
    > ### Aliases: build_bot_halfeyeplot
    > 
    > ### ** Examples
    > 
    > 
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
    Execution halted
    ```

# ctrialsgov

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/ctrialsgov
* Date/Publication: 2021-10-18 16:00:02 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "ctrialsgov")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ctrialsgov)
      > 
      > test_check("ctrialsgov")
      [NCT04553939] ible Local Advanved |Bladder| Cancer
      [NCT03517995]  of Sulforaphane in |Bladder| Cancer Chemoprevent
      [NCT04210479]       Comparison of |Bladder| Filling vs. Non-Fil
    ...
          ▆
       1. ├─ctrialsgov::ctgov_to_plotly(p) at test-plot.R:12:3
       2. └─ctrialsgov:::ctgov_to_plotly.ctgov_bar_plot(p)
       3.   ├─plotly::ggplotly(p, tooltip = "text")
       4.   └─plotly:::ggplotly.ggplot(p, tooltip = "text")
       5.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 6 | SKIP 0 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1350 marked UTF-8 strings
    ```

# cubble

<details>

* Version: 1.0.0
* GitHub: https://github.com/huizezhang-sherry/cubble
* Source code: https://github.com/cran/cubble
* Date/Publication: 2024-08-27 15:20:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "cubble")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cb5match.Rmd’
      ...
    > p2 <- ggplot(res_tm_long, aes(x = date, y = matched, 
    +     group = type, color = type)) + geom_line() + facet_wrap(vars(group)) + 
    +     scale_colo .... [TRUNCATED] 
    
    > (p1 | p2) + patchwork::plot_layout(guides = "collect") + 
    +     plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")
    
    ...
    Error: subscript out of bounds
    Execution halted
    
      ‘cb1class.Rmd’ using ‘UTF-8’... OK
      ‘cb2create.Rmd’ using ‘UTF-8’... OK
      ‘cb3tsibblesf.Rmd’ using ‘UTF-8’... OK
      ‘cb4glyph.Rmd’ using ‘UTF-8’... OK
      ‘cb5match.Rmd’ using ‘UTF-8’... failed
      ‘cb6interactive.Rmd’ using ‘UTF-8’... failed
      ‘cb7misc.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cb1class.Rmd’ using rmarkdown
    --- finished re-building ‘cb1class.Rmd’
    
    --- re-building ‘cb2create.Rmd’ using rmarkdown
    --- finished re-building ‘cb2create.Rmd’
    
    --- re-building ‘cb3tsibblesf.Rmd’ using rmarkdown
    --- finished re-building ‘cb3tsibblesf.Rmd’
    
    --- re-building ‘cb4glyph.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        doc    1.4Mb
    ```

# curtailment

<details>

* Version: 0.2.6
* GitHub: https://github.com/martinlaw/curtailment
* Source code: https://github.com/cran/curtailment
* Date/Publication: 2023-10-25 15:50:04 UTC
* Number of recursive dependencies: 36

Run `revdepcheck::cloud_details(, "curtailment")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘curtailment-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: drawDiagramGeneric
    > ### Title: drawDiagramGeneric
    > ### Aliases: drawDiagramGeneric
    > 
    > ### ** Examples
    > 
    >  go <- cbind(6:8, rep(8,3))
    ...
      4.   └─ggplot2:::ggplot_build.ggplot(x)
      5.     └─layout$setup(data, plot$data, plot$plot_env)
      6.       └─ggplot2 (local) setup(..., self = self)
      7.         └─self$coord$setup_params(data)
      8.           └─ggplot2 (local) setup_params(..., self = self)
      9.             └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
     10.               └─ggplot2:::check_logical(expand)
     11.                 └─ggplot2:::stop_input_type(...)
     12.                   └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

# dabestr

<details>

* Version: 2023.9.12
* GitHub: https://github.com/ACCLAB/dabestr
* Source code: https://github.com/cran/dabestr
* Date/Publication: 2023-10-13 11:50:06 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "dabestr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tutorial_deltadelta.Rmd’
      ...
    > paired_delta2.mean_diff <- load(df, x = Treatment, 
    +     y = Measurement, experiment = Genotype, colour = Rep, delta2 = TRUE, 
    +     idx = list(c(" ..." ... [TRUNCATED] 
    
    > dabest_plot(paired_delta2.mean_diff, raw_marker_size = 0.5, 
    +     raw_marker_alpha = 0.3)
    
    ...
    Error: argument is of length zero
    Execution halted
    
      ‘datasets.Rmd’ using ‘UTF-8’... OK
      ‘plot_aesthetics.Rmd’ using ‘UTF-8’... OK
      ‘tutorial_basics.Rmd’ using ‘UTF-8’... OK
      ‘tutorial_deltadelta.Rmd’ using ‘UTF-8’... failed
      ‘tutorial_minimeta.Rmd’ using ‘UTF-8’... failed
      ‘tutorial_proportion_plots.Rmd’ using ‘UTF-8’... OK
      ‘tutorial_repeated_measures.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘datasets.Rmd’ using rmarkdown
    --- finished re-building ‘datasets.Rmd’
    
    --- re-building ‘plot_aesthetics.Rmd’ using rmarkdown
    ```

# DAISIEprep

<details>

* Version: 0.4.0
* GitHub: https://github.com/joshwlambert/DAISIEprep
* Source code: https://github.com/cran/DAISIEprep
* Date/Publication: 2024-04-02 11:30:06 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "DAISIEprep")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(DAISIEprep)
      > 
      > test_check("DAISIEprep")
      [ FAIL 4 | WARN 2 | SKIP 14 | PASS 2211 ]
      
      ══ Skipped tests (14) ══════════════════════════════════════════════════════════
    ...
      ── Failure ('test-plot_phylod.R:8:3'): plot_phylod runs silent without error ───
      `plot_phylod(phylod = phylod, node_pies = FALSE)` produced warnings.
      ── Failure ('test-plot_phylod.R:13:3'): plot_phylod runs silent without error ──
      `plot_phylod(phylod = phylod, node_pies = TRUE)` produced warnings.
      ── Failure ('test-plot_phylod.R:18:3'): plot_phylod runs silent without error ──
      `plot_phylod(phylod = phylod, node_pies = TRUE)` produced warnings.
      
      [ FAIL 4 | WARN 2 | SKIP 14 | PASS 2211 ]
      Error: Test failures
      Execution halted
    ```

# dbmss

<details>

* Version: 2.9-2
* GitHub: https://github.com/EricMarcon/dbmss
* Source code: https://github.com/cran/dbmss
* Date/Publication: 2024-08-24 11:00:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "dbmss")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dbmss-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DEnvelope
    > ### Title: Estimation of the confidence envelope of the D function under
    > ###   its null hypothesis
    > ### Aliases: DEnvelope
    > 
    > ### ** Examples
    > 
    ...
    > r <- 0:30
    > NumberOfSimulations <- 20
    > Alpha <- .05
    > # Plot the envelope (after normalization by pi.r^2)
    > autoplot(DEnvelope(X, r, NumberOfSimulations, Alpha,
    +     "V. Americana", "Q. Rosea", Intertype = TRUE), ./(pi*r^2) ~ r)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘dbmss.Rmd’
      ...
    
    > autoplot(paracou16, labelSize = expression("Basal area (" ~ 
    +     cm^2 ~ ")"), labelColor = "Species")
    
    > autoplot(Mhat(paracou16, ReferenceType = "V. Americana", 
    +     NeighborType = "Q. Rosea"), main = "")
    
      When sourcing ‘dbmss.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘dbmss.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dbmss.Rmd’ using rmarkdown
    ```

# deeptime

<details>

* Version: 2.0.0
* GitHub: https://github.com/willgearty/deeptime
* Source code: https://github.com/cran/deeptime
* Date/Publication: 2024-08-19 07:00:43 UTC
* Number of recursive dependencies: 197

Run `revdepcheck::cloud_details(, "deeptime")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(deeptime)
      > 
      > test_check("deeptime")
      Scale for y is already present.
      Adding another scale for y, which will replace the existing scale.
      Scale for y is already present.
    ...
      • patterns/geo-pattern2-new.svg
      • patterns/scale-fill-geopattern-labels-new.svg
      • patterns/scale-fill-geopattern-limits-new.svg
      • patterns/scale-fill-geopattern-na-new.svg
      • patterns/scale-fill-geopattern-na2-new.svg
      • points_range/geom-points-range-aes-new.svg
      • points_range/geom-points-range-bg-new.svg
      • points_range/geom-points-range-h-new.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘coord_geo.Rmd’
      ...
    +     y = n)) + scale_x_reverse("Age (Ma)") + ylab("Coral Genera") + 
    +     coord_geo(xlim =  .... [TRUNCATED] 
    
    > ggplot(coral_div) + geom_line(aes(x = stage_age, y = n)) + 
    +     scale_x_reverse("Age (Ma)") + ylab("Coral Genera") + coord_geo(dat = "periods", 
    + .... [TRUNCATED] 
    
    ...
    Error: argument is of length zero
    Execution halted
    
      ‘coord.Rmd’ using ‘UTF-8’... OK
      ‘coord_geo.Rmd’ using ‘UTF-8’... failed
      ‘geo.Rmd’ using ‘UTF-8’... OK
      ‘ggarrange2.Rmd’ using ‘UTF-8’... OK
      ‘phylogenies.Rmd’ using ‘UTF-8’... OK
      ‘time.Rmd’ using ‘UTF-8’... failed
      ‘traits.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘coord.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        R      2.8Mb
        doc    1.8Mb
        help   1.1Mb
    ```

# descriptio

<details>

* Version: 1.3
* GitHub: https://github.com/nicolas-robette/descriptio
* Source code: https://github.com/cran/descriptio
* Date/Publication: 2024-03-07 11:00:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "descriptio")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘descriptio-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggassoc_crosstab
    > ### Title: Proportional area plot
    > ### Aliases: ggassoc_crosstab
    > ### Keywords: multivariate aplot
    > 
    > ### ** Examples
    > 
    > data(Movies)
    > ggassoc_crosstab(data=Movies, mapping=ggplot2::aes(Genre, Country))
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: ggassoc_crosstab ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘FactoMineR’, ‘vcd’
    ```

# directlabels

<details>

* Version: 2024.1.21
* GitHub: https://github.com/tdhock/directlabels
* Source code: https://github.com/cran/directlabels
* Date/Publication: 2024-01-24 19:20:07 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "directlabels")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘examples.Rmd’
      ...
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    
      When sourcing ‘examples.R’:
    Error: Problem while computing stat.
    ℹ Error occurred in the 3rd layer.
    Caused by error in `get()`:
    ! object 'last.qp' of mode 'function' was not found
    Execution halted
    
      ‘examples.Rmd’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘examples.Rmd’ using knitr
    ```

# disto

<details>

* Version: 0.2.0
* GitHub: https://github.com/talegari/disto
* Source code: https://github.com/cran/disto
* Date/Publication: 2018-08-02 12:50:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "disto")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘disto-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.disto
    > ### Title: Plot a disto object
    > ### Aliases: plot.disto
    > 
    > ### ** Examples
    > 
    > temp <- stats::dist(iris[,1:4])
    ...
    > dio  <- disto(objectname = "temp")
    > plot(dio, type = "heatmap")
    > plot(dio, type = "dendrogram")
    Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    of ggplot2 3.3.4.
    ℹ The deprecated feature was likely used in the factoextra package.
      Please report the issue at <https://github.com/kassambara/factoextra/issues>.
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘vignette_disto.Rmd’
      ...
    
    > plot(dio, type = "dendrogram")
    Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    of ggplot2 3.3.4.
    ℹ The deprecated feature was likely used in the factoextra package.
      Please report the issue at <https://github.com/kassambara/factoextra/issues>.
    
      When sourcing ‘vignette_disto.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘vignette_disto.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘vignette_disto.Rmd’ using rmarkdown
    
    Quitting from lines 42-72 [unnamed-chunk-1] (vignette_disto.Rmd)
    Error: processing vignette 'vignette_disto.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘vignette_disto.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘vignette_disto.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘proxy’
      All declared Imports should be used.
    ```

# distributional

<details>

* Version: 0.4.0
* GitHub: https://github.com/mitchelloharawild/distributional
* Source code: https://github.com/cran/distributional
* Date/Publication: 2024-02-07 13:30:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "distributional")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘distributional-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dist_truncated
    > ### Title: Truncate a distribution
    > ### Aliases: dist_truncated
    > 
    > ### ** Examples
    > 
    > dist <- dist_truncated(dist_normal(2,1), lower = 0)
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
    Execution halted
    ```

# dittoViz

<details>

* Version: 1.0.1
* GitHub: https://github.com/dtm2451/dittoViz
* Source code: https://github.com/cran/dittoViz
* Date/Publication: 2024-02-02 00:00:12 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "dittoViz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dittoViz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: barPlot
    > ### Title: Outputs a stacked bar plot to show the percent composition of
    > ###   samples, groups, clusters, or other groupings
    > ### Aliases: barPlot
    > 
    > ### ** Examples
    > 
    ...
    15     3        D    12                          32 0.3750000
    16     4        D     8                          32 0.2500000
    > # through hovering the cursor over the relevant parts of the plot
    > if (requireNamespace("plotly", quietly = TRUE)) {
    +     barPlot(example_df, "clustering", group.by = "groups",
    +         do.hover = TRUE)
    +     }
    Error in pm[[2]] : subscript out of bounds
    Calls: barPlot -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dittoViz)
      Loading required package: ggplot2
      > test_check("dittoViz")
      [ FAIL 89 | WARN 2 | SKIP 0 | PASS 63 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        6.   └─dittoViz:::.yPlot_add_data_y_direction(...)
        7.     └─ggplot2:::`+.gg`(p, do.call(geom_boxplot, boxplot.args))
        8.       └─ggplot2:::add_ggplot(e1, e2, e2name)
        9.         ├─ggplot2::ggplot_add(object, p, objectname)
       10.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       11.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 89 | WARN 2 | SKIP 0 | PASS 63 ]
      Error: Test failures
      Execution halted
    ```

# dotwhisker

<details>

* Version: 0.8.2
* GitHub: https://github.com/fsolt/dotwhisker
* Source code: https://github.com/cran/dotwhisker
* Date/Publication: 2024-06-07 12:20:06 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "dotwhisker")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dotwhisker-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: small_multiple
    > ### Title: Generate a 'Small Multiple' Plot of Regression Results
    > ### Aliases: small_multiple
    > 
    > ### ** Examples
    > 
    > library(broom)
    ...
    +  m[[i]] <- update(m[[i-1]], paste(". ~ . +", ordered_vars[i]))
    +  m123456_df <- rbind(m123456_df, m[[i]] %>% tidy %>% by_2sd(mtcars) %>%
    +    mutate(model = paste("Model", i)))
    + }
    > 
    > # Generate a 'small multiple' plot
    > small_multiple(m123456_df)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: small_multiple ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘dotwhisker-vignette.Rmd’
      ...
    +     wt = "Weight", cyl = "Cylinders", disp = "Displacement", 
    +    .... [TRUNCATED] 
    
    > small_multiple(m123456_df) + theme_bw(base_size = 4) + 
    +     ylab("Coefficient Estimate") + geom_hline(yintercept = 0, 
    +     colour = "grey60", li .... [TRUNCATED] 
    
    ...
    
    > small_multiple(results_df, show_stats = FALSE) + scale_x_discrete(limits = model_names) + 
    +     theme_bw() + ylab("Coefficient Estimate") + geom_hl .... [TRUNCATED] 
    
      When sourcing ‘kl2007_examples.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘dotwhisker-vignette.Rmd’ using ‘UTF-8’... failed
      ‘kl2007_examples.Rmd’ using ‘ASCII’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dotwhisker-vignette.Rmd’ using rmarkdown
    ```

# DRomics

<details>

* Version: 2.5-2
* GitHub: https://github.com/aursiber/DRomics
* Source code: https://github.com/cran/DRomics
* Date/Publication: 2024-01-31 09:30:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "DRomics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘DRomics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PCAdataplot
    > ### Title: Performs and plots the results of a PCA on omic data
    > ### Aliases: PCAdataplot
    > 
    > ### ** Examples
    > 
    > 
    ...
    Number of items: 100 
    Identifiers of the first 20 items:
     [1] "1"    "2"    "3"    "4"    "5.1"  "6.1"  "7.1"  "8.1"  "9.1"  "10.1"
    [11] "11.1" "12.1" "13.1" "14.1" "15"   "16.1" "17.1" "18.1" "19.1" "20.1"
    Data were normalized between arrays using the following method: cyclicloess 
    > plot(o)
    > PCAdataplot(o)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: PCAdataplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘DRomics_vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘DRomics_vignette.Rmd’
      ...
    [16] "ENSDARG00000100660" "ENSDARG00000113107" "ENSDARG00000099787"
    [19] "ENSDARG00000112451" "ENSDARG00000070546"
    Data were normalized with respect to library size and tranformed using 
    the following method: rlog
    
    > PCAdataplot(o, batch = zebraf$batch) + theme_bw()
    
      When sourcing ‘DRomics_vignette.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘DRomics_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc   2.9Mb
    ```

# dtwclust

<details>

* Version: 6.0.0
* GitHub: https://github.com/asardaes/dtwclust
* Source code: https://github.com/cran/dtwclust
* Date/Publication: 2024-07-23 08:50:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "dtwclust")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(dtwclust)
      Loading required package: proxy
      
      Attaching package: 'proxy'
      
      The following objects are masked from 'package:stats':
      
    ...
        7.   └─dtwclust (local) .local(x, y = y, ...)
        8.     └─ggplot2:::`+.gg`(gg, do_call(ggrepel::geom_label_repel, labels))
        9.       └─ggplot2:::add_ggplot(e1, e2, e2name)
       10.         ├─ggplot2::ggplot_add(object, p, objectname)
       11.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       12.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 0 | SKIP 15 | PASS 1930 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘parallelization-considerations.Rmd’ using rmarkdown_notangle
    --- finished re-building ‘parallelization-considerations.Rmd’
    
    --- re-building ‘timing-experiments.Rmd’ using rmarkdown_notangle
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.5Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    2.1Mb
        libs  11.1Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# duke

<details>

* Version: 0.0.3
* GitHub: https://github.com/aidangildea/duke
* Source code: https://github.com/cran/duke
* Date/Publication: 2023-12-15 21:50:16 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "duke")` for more info

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
       2. │ └─testthat:::expect_condition_matching(...)
       3. │   └─testthat:::quasi_capture(...)
       4. │     ├─testthat (local) .capture(...)
       5. │     │ └─base::withCallingHandlers(...)
       6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       7. └─testthat::expect_equal(ggplot2::layer_data(p3)$fill[9], "#B5B5B5")
      
      [ FAIL 1 | WARN 9 | SKIP 0 | PASS 27 ]
      Error: Test failures
      Execution halted
    ```

# easysurv

<details>

* Version: 2.0.1
* GitHub: https://github.com/Maple-Health-Group/easysurv
* Source code: https://github.com/cran/easysurv
* Date/Publication: 2024-06-21 10:30:06 UTC
* Number of recursive dependencies: 156

Run `revdepcheck::cloud_details(, "easysurv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘easysurv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_km
    > ### Title: Generate Kaplan-Meier estimates
    > ### Aliases: get_km
    > 
    > ### ** Examples
    > 
    > km_results <- get_km(
    ...
    Poor     Poor     228    145 3.101736 0.1772520 2.183562 1.978082 2.619178
           Median follow-up
    Good           4.452055
    Medium         4.712329
    Poor           4.115068
    
    Error in .construct_risktable(x, geom_blank.times = NULL, geom_blank.risktable_stats = "{n.risk}",  : 
      argument "risktable_height" is missing, with no default
    Calls: <Anonymous> ... ggsurvfit_build -> <Anonymous> -> .construct_risktable
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘easysurv.Rmd’
      ...
             Median follow-up
    Tab+Vis          2.217659
    Tab->Vis         2.220397
    Tab              2.308008
    Vis              2.198494
    
    
      When sourcing ‘easysurv.R’:
    Error: argument "risktable_height" is missing, with no default
    Execution halted
    
      ‘easysurv.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘easysurv.Rmd’ using rmarkdown
    
    Quitting from lines 149-157 [km] (easysurv.Rmd)
    Error: processing vignette 'easysurv.Rmd' failed with diagnostics:
    argument "risktable_height" is missing, with no default
    --- failed re-building ‘easysurv.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘easysurv.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# EGAnet

<details>

* Version: 2.0.7
* GitHub: https://github.com/hfgolino/EGAnet
* Source code: https://github.com/cran/EGAnet
* Date/Publication: 2024-09-02 20:00:01 UTC
* Number of recursive dependencies: 190

Run `revdepcheck::cloud_details(, "EGAnet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EGAnet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dimensionStability
    > ### Title: Dimension Stability Statistics from 'bootEGA'
    > ### Aliases: dimensionStability
    > 
    > ### ** Examples
    > 
    > # Load data
    ...
     16. │                 └─e1 %+% e2
     17. │                   └─ggplot2:::add_ggplot(e1, e2, e2name)
     18. │                     ├─ggplot2::ggplot_add(object, p, objectname)
     19. │                     └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     20. │                       └─ggplot2:::new_layer_names(object, names(plot$layers))
     21. └─base::.handleSimpleError(...)
     22.   └─purrr (local) h(simpleError(msg, call))
     23.     └─cli::cli_abort(...)
     24.       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   3.5Mb
    ```

# EGM

<details>

* Version: 0.1.0
* GitHub: https://github.com/shah-in-boots/EGM
* Source code: https://github.com/cran/EGM
* Date/Publication: 2024-05-23 16:10:05 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "EGM")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(EGM)
      Loading required package: vctrs
      Loading required package: data.table
      > EGM::set_wfdb_path("/usr/local/bin")
      > 
      > test_check("EGM")
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-ggm.R:63:2'): theming works ──────────────────────────────────
      g$labels$x (`actual`) not equal to "sample" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('sample')
      
      [ FAIL 1 | WARN 0 | SKIP 19 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

# emmeans

<details>

* Version: 1.10.4
* GitHub: https://github.com/rvlenth/emmeans
* Source code: https://github.com/cran/emmeans
* Date/Publication: 2024-08-21 03:00:01 UTC
* Number of recursive dependencies: 159

Run `revdepcheck::cloud_details(, "emmeans")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘emmeans-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auto.noise
    > ### Title: Auto Pollution Filter Noise
    > ### Aliases: auto.noise
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > # (Based on belief that noise/10 is in decibel units)
    > noise.lm <- lm(noise/10 ~ size * type * side, data = auto.noise)
    > 
    > # Interaction plot of predictions
    > emmip(noise.lm, type ~ size | side)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: emmip ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basics.Rmd’
      ...
    'emmGrid' object with variables:
        source = fish, soy, skim
        percent =  9, 12, 15, 18
    Transformation: “inverse” 
    
    > emmip(RG4, source ~ percent, style = "factor")
    
    ...
      ‘messy-data.Rmd’ using ‘UTF-8’... failed
      ‘models.Rmd’ using ‘UTF-8’... OK
      ‘predictions.Rmd’ using ‘UTF-8’... failed
      ‘re-engineering-clds.rmd’ using ‘UTF-8’... OK
      ‘sophisticated.Rmd’ using ‘UTF-8’... failed
      ‘transformations.Rmd’ using ‘UTF-8’... failed
      ‘utilities.Rmd’ using ‘UTF-8’... OK
      ‘vignette-topics.Rmd’ using ‘UTF-8’... OK
      ‘xplanations.Rmd’ using ‘UTF-8’... failed
      ‘xtending.Rmd’ using ‘UTF-8’... OK
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'CARBayes', 'coxme', 'gee', 'geepack', 'MCMCglmm', 'MCMCpack',
      'mice', 'pscl', 'rstanarm', 'sommer'
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘AQuickStart.Rmd’ using rmarkdown
    --- finished re-building ‘AQuickStart.Rmd’
    
    --- re-building ‘FAQs.Rmd’ using rmarkdown
    --- finished re-building ‘FAQs.Rmd’
    
    --- re-building ‘basics.Rmd’ using rmarkdown
    
    Quitting from lines 260-262 [unnamed-chunk-13] (basics.Rmd)
    Error: processing vignette 'basics.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘basics.Rmd’
    
    --- re-building ‘comparisons.Rmd’ using rmarkdown
    ```

# EMMIXmfa

<details>

* Version: 2.0.14
* GitHub: https://github.com/suren-rathnayake/EMMIXmfa
* Source code: https://github.com/cran/EMMIXmfa
* Date/Publication: 2024-01-25 20:30:02 UTC
* Number of recursive dependencies: 59

Run `revdepcheck::cloud_details(, "EMMIXmfa")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EMMIXmfa-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: factor_scores
    > ### Title: Computes Factor Scores
    > ### Aliases: factor_scores factor_scores.mcfa factor_scores.mctfa
    > ###   plot.emmix
    > ### Keywords: cluster multivariate models
    > 
    > ### ** Examples
    ...
    > Y <- iris[-c(sel_subset), -5]
    > Y <- as.matrix(Y)
    > clust <- predict(model, Y)
    > 
    > fa_scores <- factor_scores(model, Y)
    > # Visualizing new data in factor space
    > plot_factors(fa_scores, type = "Umean", clust = clust)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot_factors ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# entropart

<details>

* Version: 1.6-15
* GitHub: https://github.com/EricMarcon/entropart
* Source code: https://github.com/cran/entropart
* Date/Publication: 2024-08-26 19:30:09 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "entropart")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘entropart-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Accumulation
    > ### Title: Diversity accumulation.
    > ### Aliases: DivAC EntAC as.AccumCurve is.AccumCurve autoplot.AccumCurve
    > ###   plot.AccumCurve
    > 
    > ### ** Examples
    > 
    ...
     11.       │ └─base::withCallingHandlers(...)
     12.       └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13.         └─l$compute_geom_2(d, theme = plot$theme)
     14.           └─ggplot2 (local) compute_geom_2(..., self = self)
     15.             └─self$geom$use_defaults(...)
     16.               └─ggplot2 (local) use_defaults(..., self = self)
     17.                 └─ggplot2:::check_aesthetics(new_params, nrow(data))
     18.                   └─cli::cli_abort(...)
     19.                     └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘entropart.Rmd’
      ...
    
    > autoplot(Abd18, Distribution = "lnorm")
    
      When sourcing ‘entropart.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (149).
    ✖ Fix the following mappings: `shape`, `colour`, and `size`.
    Execution halted
    
      ‘entropart.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘entropart.Rmd’ using rmarkdown
    
    Quitting from lines 53-55 [PlotN18] (entropart.Rmd)
    Error: processing vignette 'entropart.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `check_aesthetics()`:
    ! Aesthetics must be either length 1 or the same as the data (149).
    ✖ Fix the following mappings: `shape`, `colour`, and `size`.
    --- failed re-building ‘entropart.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘entropart.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# EnvStats

<details>

* Version: 3.0.0
* GitHub: https://github.com/alexkowa/EnvStats
* Source code: https://github.com/cran/EnvStats
* Date/Publication: 2024-08-24 23:10:05 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "EnvStats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EnvStats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_stripchart
    > ### Title: 1-D Scatter Plots with Confidence Intervals Using ggplot2
    > ### Aliases: geom_stripchart
    > ### Keywords: hplot htest
    > 
    > ### ** Examples
    > 
    ...
    > 
    >   p + geom_stripchart() +
    +     labs(x = "Number of Cylinders", y = "Miles per Gallon")
    Warning: The `fun.y` argument of `stat_summary()` is deprecated as of ggplot2 3.3.0.
    ℹ Please use the `fun` argument instead.
    ℹ The deprecated feature was likely used in the EnvStats package.
      Please report the issue to the authors.
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: +.gg ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        help   3.5Mb
    ```

# epiCleanr

<details>

* Version: 0.2.0
* GitHub: https://github.com/truenomad/epiCleanr
* Source code: https://github.com/cran/epiCleanr
* Date/Publication: 2023-09-28 12:20:05 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "epiCleanr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘epiCleanr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: handle_outliers
    > ### Title: Detect and Handle Outliers in Dataset
    > ### Aliases: handle_outliers
    > 
    > ### ** Examples
    > 
    > 
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc    2.9Mb
        help   2.5Mb
    ```

# epiphy

<details>

* Version: 0.5.0
* GitHub: https://github.com/chgigot/epiphy
* Source code: https://github.com/cran/epiphy
* Date/Publication: 2023-11-16 11:20:10 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "epiphy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘epiphy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clump
    > ### Title: Regroup observational data into even clumps of individuals.
    > ### Aliases: clump clump.intensity
    > 
    > ### ** Examples
    > 
    > my_incidence <- incidence(tomato_tswv$field_1929)
    ...
     18. │               └─ggplot2 (local) setup_params(...)
     19. │                 └─ggplot2:::make_summary_fun(...)
     20. │                   └─rlang::as_function(fun.data)
     21. │                     └─base::get(x, envir = env, mode = "function")
     22. └─base::.handleSimpleError(...)
     23.   └─rlang (local) h(simpleError(msg, call))
     24.     └─handlers[[1L]](cnd)
     25.       └─cli::cli_abort(...)
     26.         └─rlang::abort(...)
    Execution halted
    ```

# EQUALSTATS

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/EQUALSTATS
* Date/Publication: 2024-09-06 16:10:15 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "EQUALSTATS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EQUALSTATS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: function.Survival_Analysis
    > ### Title: Perform Survival Analysis
    > ### Aliases: function.Survival_Analysis
    > 
    > ### ** Examples
    > 
    > # Create simulated data ####
    ...
    > rv$second_menu_choice <- NA
    > rv$entry[[1]] <- "Admission to care home"
    > rv$entry[[2]] <- "Follow-up"
    > rv$entry[[3]] <- "Treatment"
    > # Final function ####
    > Results <- function.Survival_Analysis(Predefined_lists, rv)
    Error in .construct_risktable(x, geom_blank.times = NULL, geom_blank.risktable_stats = c("{n.risk}",  : 
      argument "risktable_height" is missing, with no default
    Calls: function.Survival_Analysis
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘lmerTest’
    ```

# ergm.multi

<details>

* Version: 0.2.1
* GitHub: https://github.com/statnet/ergm.multi
* Source code: https://github.com/cran/ergm.multi
* Date/Publication: 2024-02-20 23:20:05 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "ergm.multi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ergm.multi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gofN
    > ### Title: Linear model diagnostics for multinetwork linear models
    > ### Aliases: gofN [.gofN augment.gofN summary.gofN
    > 
    > ### ** Examples
    > 
    > data(samplk)
    ...
    > 
    > ### If 'ggplot2' and 'ggrepel' are installed, illustrate the autoplot() method.
    > if(require("ggplot2") && requireNamespace("ggrepel")){
    +   autoplot(fit.gof)
    + }
    Loading required package: ggplot2
    Loading required namespace: ggrepel
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: autoplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Goeyvaerts_reproduction.Rmd’
      ...
    [1] 0.9763295
    
    
    
    > autoplot(gof.wd)
    Loading required namespace: ggrepel
    
      When sourcing ‘Goeyvaerts_reproduction.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Goeyvaerts_reproduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Goeyvaerts_reproduction.Rmd’ using rmarkdown
    
    Quitting from lines 157-158 [unnamed-chunk-16] (Goeyvaerts_reproduction.Rmd)
    Error: processing vignette 'Goeyvaerts_reproduction.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Goeyvaerts_reproduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Goeyvaerts_reproduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# esci

<details>

* Version: 1.0.3
* GitHub: https://github.com/rcalinjageman/esci
* Source code: https://github.com/cran/esci
* Date/Publication: 2024-07-08 21:40:10 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "esci")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘esci-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: estimate_mdiff_2x2_between
    > ### Title: Estimates for a 2x2 between-subjects design with a continuous
    > ###   outcome variable
    > ### Aliases: estimate_mdiff_2x2_between
    > 
    > ### ** Examples
    > 
    ...
    +   estimates_from_summary$interaction,
    +   effect_size = "mean"
    + )
    Warning: Using size for a discrete variable is not advised.
    Warning: Using alpha for a discrete variable is not advised.
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, 
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(esci)
      > 
      > test_check("esci")
      Loading required package: Matrix
      Loading required package: metadat
      Loading required package: numDeriv
    ...
       17. │             └─self$geom$use_defaults(...)
       18. └─base::.handleSimpleError(...)
       19.   └─rlang (local) h(simpleError(msg, call))
       20.     └─handlers[[1L]](cnd)
       21.       └─cli::cli_abort(...)
       22.         └─rlang::abort(...)
      
      [ FAIL 14 | WARN 0 | SKIP 0 | PASS 3182 ]
      Error: Test failures
      Execution halted
    ```

# evalITR

<details>

* Version: 1.0.0
* GitHub: https://github.com/MichaelLLi/evalITR
* Source code: https://github.com/cran/evalITR
* Date/Publication: 2023-08-25 23:10:06 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "evalITR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cv_multiple_alg.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cv_multiple_alg.Rmd’
      ...
        intersect, setdiff, setequal, union
    
    
    > load("../data/star.rda")
    Warning in readChar(con, 5L, useBytes = TRUE) :
      cannot open compressed file '../data/star.rda', probable reason 'No such file or directory'
    
    ...
    Execution halted
    
      ‘cv_multiple_alg.Rmd’ using ‘UTF-8’... failed
      ‘cv_single_alg.Rmd’ using ‘UTF-8’... failed
      ‘install.Rmd’ using ‘UTF-8’... OK
      ‘paper_alg1.Rmd’ using ‘UTF-8’... OK
      ‘sample_split.Rmd’ using ‘UTF-8’... failed
      ‘sample_split_caret.Rmd’ using ‘UTF-8’... failed
      ‘user_itr.Rmd’ using ‘UTF-8’... failed
      ‘user_itr_algs.Rmd’ using ‘UTF-8’... failed
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forcats’ ‘rqPen’ ‘utils’
      All declared Imports should be used.
    ```

# eventstudyr

<details>

* Version: 1.1.3
* GitHub: https://github.com/JMSLab/eventstudyr
* Source code: https://github.com/cran/eventstudyr
* Date/Publication: 2024-03-04 15:00:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "eventstudyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(eventstudyr)
      > 
      > test_check("eventstudyr")
      Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead3. To specify a different proxyIV use the proxyIV argument.
      Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead3. To specify a different proxyIV use the proxyIV argument.
      Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead3. To specify a different proxyIV use the proxyIV argument.
    ...
      `expected` is a character vector ('ci_lower')
      ── Failure ('test-EventStudyPlot.R:128:5'): confidence intervals are appropriately present or absent ──
      p_ci$labels$ymax (`actual`) not equal to "ci_upper" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('ci_upper')
      
      [ FAIL 6 | WARN 0 | SKIP 0 | PASS 258 ]
      Error: Test failures
      Execution halted
    ```

# EvoPhylo

<details>

* Version: 0.3.2
* GitHub: https://github.com/tiago-simoes/EvoPhylo
* Source code: https://github.com/cran/EvoPhylo
* Date/Publication: 2022-11-03 17:00:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "EvoPhylo")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘EvoPhylo-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_clusters
    > ### Title: Estimate and plot character partitions
    > ### Aliases: make_clusters plot.cluster_df
    > 
    > ### ** Examples
    > 
    > # See vignette("char-part") for how to use this
    ...
    > # tSNE (3 dimensions; default is 2)
    > cluster_df_tsne <- make_clusters(Dmatrix, k = 3, tsne = TRUE,
    +                                  tsne_dim = 2)
    > 
    > # Plot clusters, plots divided into 2 rows, and increasing
    > # overlap of text labels (default = 10)
    > plot(cluster_df_tsne, nrow = 2, max.overlaps = 20)
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘char-part.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘char-part.Rmd’
      ...
    +     collapse = TRUE, dpi = 300)
    
    > devtools::load_all(".")
    
      When sourcing ‘char-part.R’:
    Error: Could not find a root 'DESCRIPTION' file that starts with '^Package' in
    '/tmp/Rtmp6FtfyP/file1a7972246c9b/vignettes'.
    ...
    ℹ Are you in your project directory and does your project have a 'DESCRIPTION'
      file?
    Execution halted
    
      ‘char-part.Rmd’ using ‘UTF-8’... failed
      ‘data_treatment.Rmd’ using ‘UTF-8’... OK
      ‘fbd-params.Rmd’ using ‘UTF-8’... failed
      ‘offset_handling.Rmd’ using ‘UTF-8’... failed
      ‘rates-selection_BEAST2.Rmd’ using ‘UTF-8’... failed
      ‘rates-selection_MrBayes.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data      2.5Mb
        doc       1.6Mb
        extdata   2.4Mb
    ```

# expirest

<details>

* Version: 0.1.6
* GitHub: https://github.com/piusdahinden/expirest
* Source code: https://github.com/cran/expirest
* Date/Publication: 2024-03-25 16:30:02 UTC
* Number of recursive dependencies: 46

Run `revdepcheck::cloud_details(, "expirest")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(expirest)
      > 
      > test_check("expirest")
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 1122 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Failure ('test-plot_expirest_wisle.R:260:3'): plot_expirest_wisle_succeeds ──
      tmp4l2[["Graph"]]$labels has length 0, not length 8.
      ── Failure ('test-plot_expirest_wisle.R:264:3'): plot_expirest_wisle_succeeds ──
      tmp4b1[["Graph"]]$labels has length 0, not length 5.
      ── Failure ('test-plot_expirest_wisle.R:269:3'): plot_expirest_wisle_succeeds ──
      tmp4b2[["Graph"]]$labels has length 0, not length 5.
      
      [ FAIL 9 | WARN 0 | SKIP 0 | PASS 1122 ]
      Error: Test failures
      Execution halted
    ```

# explainer

<details>

* Version: 1.0.1
* GitHub: https://github.com/PERSIMUNE/explainer
* Source code: https://github.com/cran/explainer
* Date/Publication: 2024-04-18 09:00:02 UTC
* Number of recursive dependencies: 183

Run `revdepcheck::cloud_details(, "explainer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘explainer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: eDecisionCurve
    > ### Title: Decision Curve Plot
    > ### Aliases: eDecisionCurve
    > 
    > ### ** Examples
    > 
    > library("explainer")
    ...
    > mylrn$train(maintask, splits$train)
    > myplot <- eDecisionCurve(
    +   task = maintask,
    +   trained_model = mylrn,
    +   splits = splits,
    +   seed = seed
    + )
    Error in pm[[2]] : subscript out of bounds
    Calls: eDecisionCurve -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggpmisc’
      All declared Imports should be used.
    ```

# exuber

<details>

* Version: 1.0.2
* GitHub: https://github.com/kvasilopoulos/exuber
* Source code: https://github.com/cran/exuber
* Date/Publication: 2023-03-22 23:10:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "exuber")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘exuber.Rmd’
      ...
    370   1   1   1    0
    371   1   1   0    0
    372   1   1   0    0
    
    > autoplot(est_stocks)
    Using `radf_crit` for `cv`.
    
    ...
    
    > autoplot(estimation, crit_values)
    
      When sourcing ‘plotting.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘exuber.Rmd’ using ‘UTF-8’... failed
      ‘plotting.Rmd’ using ‘UTF-8’... failed
      ‘simulation.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘exuber.Rmd’ using rmarkdown
    
    Quitting from lines 73-74 [plot-radf] (exuber.Rmd)
    Error: processing vignette 'exuber.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘exuber.Rmd’
    
    --- re-building ‘plotting.Rmd’ using rmarkdown
    
    Quitting from lines 58-59 [autoplot-basic] (plotting.Rmd)
    Error: processing vignette 'plotting.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘plotting.Rmd’
    
    --- re-building ‘simulation.Rmd’ using rmarkdown
    ```

# ezEDA

<details>

* Version: 0.1.1
* GitHub: https://github.com/kviswana/ezEDA
* Source code: https://github.com/cran/ezEDA
* Date/Publication: 2021-06-29 04:40:10 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "ezEDA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ezEDA)
      > 
      > test_check("ezEDA")
      [ FAIL 22 | WARN 0 | SKIP 0 | PASS 57 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Error ('test_two_measures_relationship.R:19:3'): y axis is labeled 'hwy' ────
      Error in `expect_match(p$labels$y, "hwy")`: is.character(act$val) is not TRUE
      Backtrace:
          ▆
       1. └─testthat::expect_match(p$labels$y, "hwy") at test_two_measures_relationship.R:19:3
       2.   └─base::stopifnot(is.character(act$val))
      
      [ FAIL 22 | WARN 0 | SKIP 0 | PASS 57 ]
      Error: Test failures
      Execution halted
    ```

# ezplot

<details>

* Version: 0.7.13
* GitHub: NA
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2024-01-28 11:30:05 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "ezplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ezplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: model_plot
    > ### Title: model_plot
    > ### Aliases: model_plot
    > 
    > ### ** Examples
    > 
    > y = rnorm(26)
    > df = data.frame(ID = 1:26, actual = y + rnorm(26), fitted = y, id = letters)
    > model_plot(df, "ID", "actual", "fitted")
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# fable.prophet

<details>

* Version: 0.1.0
* GitHub: https://github.com/mitchelloharawild/fable.prophet
* Source code: https://github.com/cran/fable.prophet
* Date/Publication: 2020-08-20 09:30:03 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "fable.prophet")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro.Rmd’
      ...
     9 Domestic mdl    2019 Dec sample[5000] 5338093.
    10 Domestic mdl    2020 Jan sample[5000] 4888643.
    # ℹ 62 more rows
    
    > fc %>% autoplot(lax_passengers)
    
      When sourcing ‘intro.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL
    Execution halted
    
      ‘intro.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro.Rmd’ using rmarkdown
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# fabletools

<details>

* Version: 0.4.2
* GitHub: https://github.com/tidyverts/fabletools
* Source code: https://github.com/cran/fabletools
* Date/Publication: 2024-04-22 11:22:41 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fabletools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.fbl_ts
    > ### Title: Plot a set of forecasts
    > ### Aliases: autoplot.fbl_ts autolayer.fbl_ts
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    > library(fable)
    > library(tsibbledata)
    > fc <- aus_production %>% model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + 
    +     season("A"))) %>% forecast(h = "3 years")
    > fc %>% autoplot(aus_production)
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NU
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following object is masked from 'package:testthat':
      
    ...
       28.                                   └─ggplot2 (local) compute_geom_2(..., self = self)
       29.                                     └─self$geom$use_defaults(...)
      ── Failure ('test-graphics.R:327:3'): autoplot_dcmp_ts() ───────────────────────
      `print(p)` produced warnings.
      ── Failure ('test-graphics.R:346:3'): autoplot_dcmp_ts() ───────────────────────
      `print(p)` produced warnings.
      
      [ FAIL 4 | WARN 5 | SKIP 1 | PASS 267 ]
      Error: Test failures
      Execution halted
    ```

# factoextra

<details>

* Version: 1.0.7
* GitHub: https://github.com/kassambara/factoextra
* Source code: https://github.com/cran/factoextra
* Date/Publication: 2020-04-01 21:20:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "factoextra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘factoextra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: eclust
    > ### Title: Visual enhancement of clustering analysis
    > ### Aliases: eclust
    > 
    > ### ** Examples
    > 
    > # Load and scale data
    ...
     12. │             └─ggplot2:::`+.gg`(...)
     13. │               └─ggplot2:::add_ggplot(e1, e2, e2name)
     14. │                 ├─ggplot2::ggplot_add(object, p, objectname)
     15. │                 └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     16. │                   └─ggplot2:::new_layer_names(object, names(plot$layers))
     17. └─base::.handleSimpleError(...)
     18.   └─purrr (local) h(simpleError(msg, call))
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

# fairmodels

<details>

* Version: 1.2.1
* GitHub: https://github.com/ModelOriented/fairmodels
* Source code: https://github.com/cran/fairmodels
* Date/Publication: 2022-08-23 19:50:06 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "fairmodels")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(fairmodels)
      > 
      > 
      > test_check("fairmodels")
      Welcome to DALEX (version: 2.4.3).
      Find examples and detailed introduction at: http://ema.drwhy.ai/
    ...
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 312 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_plot_density.R:14:3'): Test plot_density ─────────────────────
      plt$labels$x not equal to "probability".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 312 ]
      Error: Test failures
      Execution halted
    ```

# fastR2

<details>

* Version: 1.2.4
* GitHub: https://github.com/rpruim/fastR2
* Source code: https://github.com/cran/fastR2
* Date/Publication: 2023-11-09 06:30:03 UTC
* Number of recursive dependencies: 165

Run `revdepcheck::cloud_details(, "fastR2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fastR2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ACTgpa
    > ### Title: ACT scores and GPA
    > ### Aliases: ACTgpa
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > 
    > gf_point(GPA ~ ACT, data = ACTgpa)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: gf_point ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        data      2.0Mb
        snippet   3.7Mb
    ```

# faux

<details>

* Version: 1.2.1
* GitHub: https://github.com/debruine/faux
* Source code: https://github.com/cran/faux
* Date/Publication: 2023-04-20 07:00:11 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "faux")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘codebook.Rmd’ using rmarkdown
    --- finished re-building ‘codebook.Rmd’
    
    --- re-building ‘continuous.Rmd’ using rmarkdown
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘faux-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: beta2norm
    > ### Title: Convert beta to normal
    > ### Aliases: beta2norm
    > 
    > ### ** Examples
    > 
    > 
    > x <- rbeta(10000, 2, 3)
    > y <- beta2norm(x)
    [32mshape1 was set to 1.96704823352025[39m
    [32mshape2 was set to 2.94110338061547[39m
    > g <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x, y))
    > ggExtra::ggMarginal(g, type = "histogram")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(faux)
      
      ************
      Welcome to faux. For support and examples visit:
      https://debruine.github.io/faux/
      - Get and set global package options with: faux_options()
    ...
        6.       └─methods (local) `<rfMthdDf>`(...)
        7.         └─methods::new(def, ...)
        8.           ├─methods::initialize(value, ...)
        9.           └─methods::initialize(value, ...)
       10.             └─.Object$initialize(...)
       11.               └─lme4 (local) initializePtr()
      
      [ FAIL 20 | WARN 6 | SKIP 14 | PASS 1331 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘norta.Rmd’
      ...
    
    > p <- ggplot(dat, aes(uniform_var, poisson_var)) + 
    +     geom_point() + geom_smooth()
    
    > ggMarginal(p, type = "histogram")
    `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
    
    ...
    Error: argument is of length zero
    Execution halted
    
      ‘codebook.Rmd’ using ‘UTF-8’... OK
      ‘continuous.Rmd’ using ‘UTF-8’... OK
      ‘contrasts.Rmd’ using ‘UTF-8’... OK
      ‘norta.Rmd’ using ‘UTF-8’... failed
      ‘rnorm_multi.Rmd’ using ‘UTF-8’... OK
      ‘sim_design.Rmd’ using ‘UTF-8’... OK
      ‘sim_df.Rmd’ using ‘UTF-8’... OK
    ```

# fddm

<details>

* Version: 1.0-2
* GitHub: https://github.com/rtdists/fddm
* Source code: https://github.com/cran/fddm
* Date/Publication: 2024-07-02 16:00:07 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "fddm")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘benchmark.Rmd’
      ...
    > mi <- min(bm_vec[, -seq_len(t_idx)])
    
    > ma <- max(bm_vec[, (t_idx + 1):(ncol(bm_vec) - 4)])
    
    > ggplot(mbm_vec, aes(x = factor(FuncName, levels = Names_vec), 
    +     y = time, color = factor(FuncName, levels = Names_vec), fill = factor(FuncName, .... [TRUNCATED] 
    
    ...
    
      When sourcing ‘pfddm.R’:
    Error: Not a unit object
    Execution halted
    
      ‘benchmark.Rmd’ using ‘UTF-8’... failed
      ‘example.Rmd’ using ‘UTF-8’... OK
      ‘math.Rmd’ using ‘UTF-8’... OK
      ‘pfddm.Rmd’ using ‘UTF-8’... failed
      ‘validity.Rmd’ using ‘UTF-8’... OK
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.6Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
        libs  14.1Mb
    ```

# feasts

<details>

* Version: 0.3.2
* GitHub: https://github.com/tidyverts/feasts
* Source code: https://github.com/cran/feasts
* Date/Publication: 2024-03-15 09:10:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "feasts")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(feasts)
      Loading required package: fabletools
      > 
      > test_check("feasts")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 101 ]
      
    ...
      ── Error ('test-graphics.R:192:3'): gg_tsdisplay() plots ───────────────────────
      Error in `p + ggplot2::labs(x = "x", y = "y", title = "title")`: non-numeric argument to binary operator
      ── Failure ('test-graphics.R:273:3'): gg_arma() plots ──────────────────────────
      p_built$plot$labels[c("x", "y")] not equivalent to list(x = "Re(1/root)", y = "Im(1/root)").
      Component "x": 1 string mismatch
      Component "y": 1 string mismatch
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 101 ]
      Error: Test failures
      Execution halted
    ```

# fergm

<details>

* Version: 1.1.4
* GitHub: https://github.com/benjamin-w-campbell/fergm
* Source code: https://github.com/cran/fergm
* Date/Publication: 2018-10-17 22:20:11 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "fergm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fergm-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: coef_posterior_density
    > ### Title: Plots the posterior density for FERGM model terms.
    > ### Aliases: coef_posterior_density
    > ### Keywords: FERGM interpret summary
    > 
    > ### ** Examples
    > 
    ...
    > data("ergm.fit")
    > data("fergm.fit")
    > data("mesa")
    > 
    > # rstan functions
    > # Histogram of the posterior
    > rstan::stan_hist(fergm.fit$stan.fit, par = "beta")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# ffp

<details>

* Version: 0.2.2
* GitHub: https://github.com/Reckziegel/FFP
* Source code: https://github.com/cran/ffp
* Date/Publication: 2022-09-29 15:10:06 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "ffp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ffp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scenario_density
    > ### Title: Plot Scenarios
    > ### Aliases: scenario_density scenario_histogram
    > 
    > ### ** Examples
    > 
    > x <- diff(log(EuStockMarkets))[, 1]
    > p <- exp_decay(x, 0.005)
    > 
    > scenario_density(x, p, 500)
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL,
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

# fic

<details>

* Version: 1.0.0
* GitHub: https://github.com/chjackson/fic
* Source code: https://github.com/cran/fic
* Date/Publication: 2019-04-13 08:32:39 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "fic")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘linear.Rnw’
      ...
    
    > library(ggplot2)
    
    > if (requireNamespace("GGally", quietly = TRUE)) {
    +     GGally::ggpairs(mtcars[, c("mpg", "am", "wt", "qsec", "disp", 
    +         "hp")], aes(colour  .... [TRUNCATED] 
    
    ...
      When sourcing 'linear.R':
    Error: argument is of length zero
    Execution halted
    
      ‘fic.Rnw’ using ‘UTF-8’... OK
      ‘linear.Rnw’ using ‘UTF-8’... failed
      ‘loss.Rnw’ using ‘UTF-8’... OK
      ‘multistate.Rnw’ using ‘UTF-8’... OK
      ‘skewnormal.Rnw’ using ‘UTF-8’... OK
      ‘survival.Rnw’ using ‘UTF-8’... OK
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘numDeriv’
      All declared Imports should be used.
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘fic.Rnw’ using knitr
    Error: processing vignette 'fic.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'fic.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `grfext.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ...
    --- finished re-building ‘skewnormal.Rnw’
    
    --- re-building ‘survival.Rnw’ using knitr
    --- finished re-building ‘survival.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘fic.Rnw’ ‘linear.Rnw’ ‘multistate.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# fido

<details>

* Version: 1.1.1
* GitHub: https://github.com/jsilve24/fido
* Source code: https://github.com/cran/fido
* Date/Publication: 2024-06-05 21:30:06 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "fido")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fido-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.pibblefit
    > ### Title: Plot Summaries of Posterior Distribution of pibblefit Parameters
    > ### Aliases: plot.pibblefit
    > 
    > ### ** Examples
    > 
    > sim <- pibble_sim(N=10, D=4, Q=3)
    > fit <- pibble(sim$Y, sim$X)
    > plot(fit, par="Lambda")
    Scale for colour is already present.
    Adding another scale for colour, which will replace the existing scale.
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(fido)
      > 
      > #Sys.setenv(KMP_DUPLICATE_LIB_OK="TRUE")
      > test_check("fido")
        [1]  0.27980164 -0.69169550 -0.53205652  0.11488451 -0.42419872  2.20261388
        [7] -1.62190133 -0.90893172  0.07891428  0.75060681  0.43593605  0.26819442
    ...
       21.                           └─base::Map(...)
       22.                             └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       23.                               └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
       24.                                 └─layer$compute_geom_2(key, single_params, theme)
       25.                                   └─ggplot2 (local) compute_geom_2(..., self = self)
       26.                                     └─self$geom$use_defaults(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 114 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘non-linear-models.Rmd’
      ...
    
    The following object is masked from ‘package:dplyr’:
    
        select
    
    
      When sourcing ‘non-linear-models.R’:
    Error: package or namespace load failed for ‘MCMCpack’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
     namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
    Execution halted
    
      ‘introduction-to-fido.Rmd’ using ‘UTF-8’... OK
      ‘mitigating-pcrbias.Rmd’ using ‘UTF-8’... OK
      ‘non-linear-models.Rmd’ using ‘UTF-8’... failed
      ‘orthus.Rmd’ using ‘UTF-8’... OK
      ‘picking_priors.Rmd’ using ‘UTF-8’... OK
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 106.5Mb
      sub-directories of 1Mb or more:
        data    4.0Mb
        libs  100.6Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction-to-fido.Rmd’ using rmarkdown
    --- finished re-building ‘introduction-to-fido.Rmd’
    
    --- re-building ‘mitigating-pcrbias.Rmd’ using rmarkdown
    --- finished re-building ‘mitigating-pcrbias.Rmd’
    
    --- re-building ‘non-linear-models.Rmd’ using rmarkdown
    ```

# fitdistrplus

<details>

* Version: 1.2-1
* GitHub: https://github.com/lbbe-software/fitdistrplus
* Source code: https://github.com/cran/fitdistrplus
* Date/Publication: 2024-07-12 12:20:02 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "fitdistrplus")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fitdistrplus-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CIcdfplot
    > ### Title: Empirical cumulative distribution function with pointwise
    > ###   confidence intervals on probabilities or on quantiles
    > ### Aliases: CIcdfplot
    > ### Keywords: distribution
    > 
    > ### ** Examples
    ...
    > f1 <- fitdist(s1, "exp")
    > b1 <- bootdist(f1, niter= 11) #voluntarily low to decrease computation time
    > 
    > # plot 95 percent bilateral confidence intervals on y values (probabilities)
    > CIcdfplot(b1, CI.level= 95/100, CI.output = "probability")
    > if (ggplotEx) CIcdfplot(b1, CI.level= 95/100, CI.output = "probability", plotstyle = "ggplot")
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘t-CIcdfplot.R’
    Running the tests in ‘tests/t-CIcdfplot.R’ failed.
    Complete output:
      > library(fitdistrplus)
      Loading required package: MASS
      Loading required package: survival
      > 
      > nbboot <- 201
      > nbboot <- 10
      > ggplotEx <- requireNamespace("ggplot2", quietly = TRUE)
    ...
      97.5 % 2.152084 2.778622
      > 
      > par(mfrow=c(1,2))
      > CIcdfplot(b1, CI.level=95/100, CI.output = "probability", CI.fill="grey80", CI.col="black")
      > CIcdfplot(b1, CI.level=95/100, CI.output = "quantile", datacol="blue")
      > if(ggplotEx) CIcdfplot(b1, CI.level=95/100, CI.output = "probability", CI.fill="grey80", CI.col="black", plotstyle = "ggplot")
      Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
        invalid line type: must be length 2, 4, 6 or 8
      Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘FAQ.Rmd’
      ...
    
    > dcomp <- denscomp(list(fitW, fitln, fitg), legendtext = c("Weibull", 
    +     "lognormal", "gamma"), xlab = "serving sizes (g)", xlim = c(0, 
    +     25 .... [TRUNCATED] 
    
    > dcomp + ggplot2::theme_minimal() + ggplot2::ggtitle("Ground beef fits")
    
      When sourcing ‘FAQ.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘FAQ.Rmd’ using ‘UTF-8’... failed
      ‘Optimalgo.Rmd’ using ‘UTF-8’... OK
      ‘fitdistrplus_vignette.Rmd’ using ‘UTF-8’... OK
      ‘starting-values.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘FAQ.Rmd’ using rmarkdown
    ```

# fitlandr

<details>

* Version: 0.1.0
* GitHub: https://github.com/Sciurus365/fitlandr
* Source code: https://github.com/cran/fitlandr
* Date/Publication: 2023-02-10 10:40:02 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "fitlandr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fitlandr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fit_2d_vf
    > ### Title: Estimate a 2D vector field
    > ### Aliases: fit_2d_vf
    > 
    > ### ** Examples
    > 
    > # generate data
    > single_output_grad <- simlandr::sim_fun_grad(length = 200, seed = 1614)
    > # fit the vector field
    > v2 <- fit_2d_vf(single_output_grad, x = "x", y = "y", method = "MVKE")
    > plot(v2)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# flexsurv

<details>

* Version: 2.3.2
* GitHub: https://github.com/chjackson/flexsurv
* Source code: https://github.com/cran/flexsurv
* Date/Publication: 2024-08-17 05:50:02 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "flexsurv")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘standsurv.Rmd’
      ...
    5       0     967  Good 2.649315 68.74975   25110 1986-05-18 female   Good
    6       0     629  Good 1.723288 64.53328   23570 1987-03-07 female   Good
    
    > km <- survfit(Surv(recyrs, censrec) ~ group2, data = bc)
    
    > kmsurvplot <- ggsurvplot(km)
    
      When sourcing ‘standsurv.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘standsurv.Rmd’ using ‘UTF-8’... failed
      ‘flexsurv.Rnw’ using ‘UTF-8’... OK
      ‘multistate.Rnw’ using ‘UTF-8’... OK
      ‘distributions.Rnw’ using ‘UTF-8’... OK
      ‘flexsurv-examples.Rnw’ using ‘UTF-8’... OK
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    1.9Mb
        libs   3.4Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘standsurv.Rmd’ using rmarkdown
    
    Quitting from lines 113-116 [unnamed-chunk-4] (standsurv.Rmd)
    Error: processing vignette 'standsurv.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘standsurv.Rmd’
    
    --- re-building ‘flexsurv.Rnw’ using knitr
    --- finished re-building ‘flexsurv.Rnw’
    ...
                                         ^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘flexsurv-examples.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘standsurv.Rmd’ ‘multistate.Rnw’ ‘distributions.Rnw’
      ‘flexsurv-examples.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# flipr

<details>

* Version: 0.3.3
* GitHub: https://github.com/LMJL-Alea/flipr
* Source code: https://github.com/cran/flipr
* Date/Publication: 2023-08-23 09:00:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "flipr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘alternative.Rmd’ using rmarkdown
    --- finished re-building ‘alternative.Rmd’
    
    --- re-building ‘exactness.Rmd’ using rmarkdown
    
    Quitting from lines 142-177 [unnamed-chunk-1] (exactness.Rmd)
    Error: processing vignette 'exactness.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘exactness.Rmd’
    
    --- re-building ‘flipr.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘exactness.Rmd’
      ...
    
    > library(flipr)
    
    > load("../R/sysdata.rda")
    Warning in readChar(con, 5L, useBytes = TRUE) :
      cannot open compressed file '../R/sysdata.rda', probable reason 'No such file or directory'
    
    ...
      cannot open compressed file '../R/sysdata.rda', probable reason 'No such file or directory'
    
      When sourcing ‘plausibility.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘alternative.Rmd’ using ‘UTF-8’... OK
      ‘exactness.Rmd’ using ‘UTF-8’... failed
      ‘flipr.Rmd’ using ‘UTF-8’... failed
      ‘plausibility.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 11.2Mb
      sub-directories of 1Mb or more:
        doc    9.1Mb
        libs   1.4Mb
    ```

# FLOPART

<details>

* Version: 2024.6.19
* GitHub: NA
* Source code: https://github.com/cran/FLOPART
* Date/Publication: 2024-06-20 21:30:10 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::cloud_details(, "FLOPART")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘comparison.Rmd’
      ...
    
    > if (require("ggplot2")) {
    +     ggplot() + ggtitle("Models with label constraints (FLOPART) and without (penalty values)") + 
    +         scale_fill_m .... [TRUNCATED] 
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
      When sourcing ‘comparison.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘comparison.Rmd’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘comparison.Rmd’ using knitr
    ```

# fmesher

<details>

* Version: 0.1.7
* GitHub: https://github.com/inlabru-org/fmesher
* Source code: https://github.com/cran/fmesher
* Date/Publication: 2024-07-01 13:00:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "fmesher")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fmesher-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fm_mesh_1d
    > ### Title: Make a 1D mesh object
    > ### Aliases: fm_mesh_1d
    > 
    > ### ** Examples
    > 
    > if (require("ggplot2")) {
    ...
    +     boundary = c("neumann", "free"),
    +     degree = 2
    +   )
    +   ggplot() +
    +     geom_fm(data = m, xlim = c(0.5, 10.5))
    + }
    Loading required package: ggplot2
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: +.gg ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basic_use.Rmd’
      ...
    
    > plot(mesh2)
    
    > suppressPackageStartupMessages(library(ggplot2))
    
    > ggplot() + geom_fm(data = mesh2)
    
      When sourcing ‘basic_use.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘basic_use.Rmd’ using ‘UTF-8’... failed
      ‘fmesher_library.Rmd’ using ‘UTF-8’... OK
      ‘inla_conversion.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘basic_use.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.5Mb
      sub-directories of 1Mb or more:
        libs  10.5Mb
    ```

# fmf

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/fmf
* Date/Publication: 2020-09-03 07:32:12 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "fmf")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fmf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot
    > ### Title: PCA Plot of the Noise Score of Each Individual
    > ### Aliases: plot
    > 
    > ### ** Examples
    > 
    > 
    ...
     17. │ └─ggplot2:::`+.gg`(...)
     18. │   └─ggplot2:::add_ggplot(e1, e2, e2name)
     19. │     ├─ggplot2::ggplot_add(object, p, objectname)
     20. │     └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     21. │       └─ggplot2:::new_layer_names(object, names(plot$layers))
     22. └─base::.handleSimpleError(...)
     23.   └─purrr (local) h(simpleError(msg, call))
     24.     └─cli::cli_abort(...)
     25.       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

# forestly

<details>

* Version: 0.1.1
* GitHub: https://github.com/Merck/forestly
* Source code: https://github.com/cran/forestly
* Date/Publication: 2024-07-08 19:40:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "forestly")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘forest-plot-static.Rmd’
      ...
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      conversion failure on 'Treatment← Favor →Placebo' in 'mbcsToSbcs': dot substituted for <e2>
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      conversion failure on 'Treatment← Favor →Placebo' in 'mbcsToSbcs': dot substituted for <86>
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      conversion failure on 'Treatment← Favor →Placebo' in 'mbcsToSbcs': dot substituted for <92>
    
      When sourcing ‘forest-plot-static.R’:
    Error: object is not a unit
    Execution halted
    
      ‘forest-plot-static.Rmd’ using ‘UTF-8’... failed
      ‘forestly-cran.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘forest-plot-static.Rmd’ using rmarkdown
    ```

# FossilSim

<details>

* Version: 2.4.0
* GitHub: NA
* Source code: https://github.com/cran/FossilSim
* Date/Publication: 2024-09-05 19:40:02 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "FossilSim")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘FossilSim-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.fbdrange
    > ### Title: Plot oriented tree with stratigraphic ranges
    > ### Aliases: plot.fbdrange
    > 
    > ### ** Examples
    > 
    > tree_file <- system.file("extdata", "fbdrange.trees", package = "FossilSim")
    > fbdr <- get_fbdrange_from_file(tree_file)
    > p <- plot(fbdr, smart.labels = TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# FPDclustering

<details>

* Version: 2.3.1
* GitHub: NA
* Source code: https://github.com/cran/FPDclustering
* Date/Publication: 2024-01-30 00:10:06 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "FPDclustering")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘FPDclustering-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GPDC
    > ### Title: Gaussian PD-Clustering
    > ### Aliases: GPDC
    > 
    > ### ** Examples
    > 
    > #Load the data
    ...
    > #Results
    > table(res$label,ais$sex)
       
          f   m
      1  95   1
      2   5 101
    > plot(res)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# frailtyEM

<details>

* Version: 1.0.1
* GitHub: https://github.com/tbalan/frailtyEM
* Source code: https://github.com/cran/frailtyEM
* Date/Publication: 2019-09-22 13:00:10 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "frailtyEM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘frailtyEM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: summary.emfrail
    > ### Title: Summary for 'emfrail' objects
    > ### Aliases: summary.emfrail
    > 
    > ### ** Examples
    > 
    > data("bladder")
    ...
        filter
    
    The following object is masked from ‘package:graphics’:
    
        layout
    
    > ggplotly(pl2)
    Error in pm[[2]] : subscript out of bounds
    Calls: ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘frailtyEM_manual.Rnw’ using Sweave
    Loading required package: survival
    Loading required package: gridExtra
    Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use
    "none" instead as of ggplot2 3.3.4.
    Warning: Removed 2 rows containing missing values or values outside
    the scale range (`geom_path()`).
    Warning in data("kidney") : data set ‘kidney’ not found
    Warning in emfrail(Surv(time, status) ~ age + sex + cluster(id), data = kidney,  :
    ...
    l.179   \RequirePackage{grfext}\relax
                                         ^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘frailtyEM_manual.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘frailtyEM_manual.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# funcharts

<details>

* Version: 1.5.0
* GitHub: https://github.com/unina-sfere/funcharts
* Source code: https://github.com/cran/funcharts
* Date/Publication: 2024-07-19 12:00:31 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "funcharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘funcharts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pca_mfd
    > ### Title: Multivariate functional principal components analysis
    > ### Aliases: pca_mfd
    > 
    > ### ** Examples
    > 
    > library(funcharts)
    > mfdobj <- data_sim_mfd()
    > pca_obj <- pca_mfd(mfdobj)
    > plot_pca_mfd(pca_obj)
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# FunnelPlotR

<details>

* Version: 0.5.0
* GitHub: https://github.com/nhs-r-community/FunnelPlotR
* Source code: https://github.com/cran/FunnelPlotR
* Date/Publication: 2024-04-12 08:40:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "FunnelPlotR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘FunnelPlotR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: funnel_plot
    > ### Title: Funnel plots for comparing institutional performance
    > ### Aliases: funnel_plot
    > 
    > ### ** Examples
    > 
    > # We will use the 'medpar' dataset from the 'COUNT' package.
    ...
    > # Draw plot, returning just the plot object
    > fp<-funnel_plot(medpar, denominator=prds, numerator=los,
    + group = provnum, limit=95, title="An example funnel plot")
    > 
    > # Methods for viewing/extracting
    > print(fp)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: print ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘changing_funnel_plot_options.Rmd’
      ...
    +     family = "poisson", data = medpar)
    
    > medpar$prds <- predict(mod, newdata = medpar, type = "response")
    
    > funnel_plot(medpar, denominator = prds, numerator = los, 
    +     group = provnum, limit = 99, label = "outlier", draw_unadjusted = TRUE)
    
    ...
    +     group = provnum, title = "Length of Stay Funnel plot for `medpar` data", 
    +     draw .... [TRUNCATED] 
    Plotting using unadjusted limits
    
      When sourcing ‘funnel_plots.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘changing_funnel_plot_options.Rmd’ using ‘UTF-8’... failed
      ‘funnel_plots.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘changing_funnel_plot_options.Rmd’ using rmarkdown
    
    Quitting from lines 33-49 [dtsetup] (changing_funnel_plot_options.Rmd)
    Error: processing vignette 'changing_funnel_plot_options.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘changing_funnel_plot_options.Rmd’
    
    --- re-building ‘funnel_plots.Rmd’ using rmarkdown
    ```

# genekitr

<details>

* Version: 1.2.8
* GitHub: https://github.com/GangLiLab/genekitr
* Source code: https://github.com/cran/genekitr
* Date/Publication: 2024-09-06 13:00:06 UTC
* Number of recursive dependencies: 202

Run `revdepcheck::cloud_details(, "genekitr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘genekitr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotVenn
    > ### Title: Venn plot for groups of genes
    > ### Aliases: plotVenn
    > 
    > ### ** Examples
    > 
    > k1 = requireNamespace("ComplexUpset",quietly = TRUE)
    ...
    +   use_venn = FALSE,
    +   main_text_size = 15,
    +   legend_text_size = 8,
    +   legend_position = 'left'
    + )
    + }
    Color length should be same with venn_list, auto assign colors...
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plotVenn ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# geoheatmap

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/geoheatmap
* Date/Publication: 2024-09-05 15:40:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "geoheatmap")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘geoheatmap.Rmd’
      ...
    +     facet_col = "state", value_col = "teams", merge_col = "name_de", 
    +      .... [TRUNCATED] 
    
    > geoheatmap(facet_data = football_teams, grid_data = de_states_grid1, 
    +     facet_col = "state", value_col = "teams", merge_col = "name_de", 
    +      .... [TRUNCATED] 
    
      When sourcing ‘geoheatmap.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘geoheatmap.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘geoheatmap.Rmd’ using rmarkdown
    ```

# geomtextpath

<details>

* Version: 0.1.4
* GitHub: https://github.com/AllanCameron/geomtextpath
* Source code: https://github.com/cran/geomtextpath
* Date/Publication: 2024-06-13 06:40:02 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "geomtextpath")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘geomtextpath-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_textsf
    > ### Title: Visualise sf objects with labels
    > ### Aliases: geom_textsf geom_labelsf
    > 
    > ### ** Examples
    > 
    > ggplot(waterways) +
    ...
     19. │                   ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     20. │                   └─self$draw_panel(data, panel_params, coord, na.rm = FALSE, legend = "other")
     21. │                     └─geomtextpath (local) draw_panel(...)
     22. │                       └─geomtextpath:::sf_textgrob(...)
     23. └─base::.handleSimpleError(...)
     24.   └─rlang (local) h(simpleError(msg, call))
     25.     └─handlers[[1L]](cnd)
     26.       └─cli::cli_abort(...)
     27.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(geomtextpath)
      Loading required package: ggplot2
      > 
      > test_check("geomtextpath")
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 463 ]
      
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-sf.R:91:3'): We can make grobs from sf features ────────────────
      Error in `(x$boxlinewidth %||% defaults$linewidth[type_ind]) * 3.779528`: non-numeric argument to binary operator
      Backtrace:
          ▆
       1. └─geomtextpath:::sf_textgrob(river, as_textbox = TRUE) at test-sf.R:91:3
      
      [ FAIL 1 | WARN 0 | SKIP 4 | PASS 463 ]
      Error: Test failures
      Execution halted
    ```

# geostan

<details>

* Version: 0.6.2
* GitHub: https://github.com/ConnorDonegan/geostan
* Source code: https://github.com/cran/geostan
* Date/Publication: 2024-06-04 09:45:37 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "geostan")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘geostan-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: print.geostan_fit
    > ### Title: print or plot a fitted geostan model
    > ### Aliases: print.geostan_fit plot.geostan_fit
    > 
    > ### ** Examples
    > 
    > data(georgia)
    ...
    log(income) -1.013   0.001 0.019 -1.049 -1.029 -1.014 -0.998 -0.974   188 1.003
    
    Samples were drawn using NUTS(diag_e) at Tue Sep 10 09:34:29 2024.
    For each parameter, n_eff is a crude measure of effective sample size,
    and Rhat is the potential scale reduction factor on split chains (at 
    convergence, Rhat=1).
    > plot(fit)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 129.8Mb
      sub-directories of 1Mb or more:
        libs  127.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RcppParallel’ ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# GGally

<details>

* Version: 2.2.1
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2024-02-14 00:53:32 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "GGally")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GGally-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: +.gg
    > ### Title: Modify a 'ggmatrix' object by adding an 'ggplot2' object to all
    > ###   plots
    > ### Aliases: +.gg add_to_ggmatrix
    > 
    > ### ** Examples
    > 
    ...
    > p_ <- GGally::print_if_interactive
    > data(tips)
    > 
    > pm <- ggpairs(tips[, 2:4], ggplot2::aes(color = sex))
    > ## change to black and white theme
    > pm + ggplot2::theme_bw()
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (requireNamespace("testthat", quietly = TRUE)) {
      +   library(testthat)
      +   library(GGally)
      + 
      +   test_check("GGally")
      + }
    ...
       20. │ └─grid:::grid.draw.grob(x$children[[i]], recording = FALSE)
       21. │   └─grDevices::recordGraphics(drawGrob(x), list(x = x), getNamespace("grid"))
       22. └─grid:::drawGrob(x)
       23.   ├─grid::drawDetails(x, recording = FALSE)
       24.   └─grid:::drawDetails.polyline(x, recording = FALSE)
       25.     └─grid:::grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow)
      
      [ FAIL 20 | WARN 1 | SKIP 22 | PASS 426 ]
      Error: Test failures
      Execution halted
    ```

# gganimate

<details>

* Version: 1.0.9
* GitHub: https://github.com/thomasp85/gganimate
* Source code: https://github.com/cran/gganimate
* Date/Publication: 2024-02-27 14:00:03 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "gganimate")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gganimate)
      Loading required package: ggplot2
      > 
      > test_check("gganimate")
      [ FAIL 1 | WARN 3 | SKIP 1 | PASS 5 ]
      
    ...
       3. ├─gganimate::animate(p, nframes = 2) at test-anim_save.R:14:5
       4. └─gganimate:::animate.gganim(p, nframes = 2)
       5.   └─args$renderer(frames_vars$frame_source, args$fps)
       6.     └─gganimate:::png_dim(frames[1])
       7.       └─cli::cli_abort("Provided file ({file}) does not exist")
       8.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 3 | SKIP 1 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gganimate.Rmd’
      ...
    Theme element `panel.grid.major.y` is missing
    Theme element `panel.grid.major.x` is missing
    Warning: Failed to plot frame
    Caused by error in `UseMethod()`:
    ! no applicable method for 'element_grob' applied to an object of class "NULL"
    
      When sourcing ‘gganimate.R’:
    Error: Provided file (/tmp/RtmpPBufaF/165c2292cd97/gganim_plot0001.png) does
    not exist
    Execution halted
    
      ‘gganimate.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gganimate.Rmd’ using rmarkdown
    ```

# ggbrain

<details>

* Version: 0.8.1
* GitHub: https://github.com/michaelhallquist/ggbrain
* Source code: https://github.com/cran/ggbrain
* Date/Publication: 2023-03-21 18:00:05 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "ggbrain")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggbrain_introduction.Rmd’
      ...
    
    > gg_obj <- gg_base + geom_brain(definition = "underlay", 
    +     fill_scale = scale_fill_gradient(low = "grey8", high = "grey62"), 
    +     show_legend  .... [TRUNCATED] 
    
    > gg_obj$render()
    
    ...
    
    > plot(gg_obj)
    
      When sourcing ‘ggbrain_labels.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘ggbrain_aesthetics.Rmd’ using ‘UTF-8’... OK
      ‘ggbrain_introduction.Rmd’ using ‘UTF-8’... failed
      ‘ggbrain_labels.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggbrain_aesthetics.Rmd’ using rmarkdown
    --- finished re-building ‘ggbrain_aesthetics.Rmd’
    
    --- re-building ‘ggbrain_introduction.Rmd’ using rmarkdown
    
    Quitting from lines 238-239 [unnamed-chunk-16] (ggbrain_introduction.Rmd)
    Error: processing vignette 'ggbrain_introduction.Rmd' failed with diagnostics:
    argument is of length zero
    ...
    Quitting from lines 47-54 [unnamed-chunk-2] (ggbrain_labels.Rmd)
    Error: processing vignette 'ggbrain_labels.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘ggbrain_labels.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘ggbrain_introduction.Rmd’ ‘ggbrain_labels.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.8Mb
      sub-directories of 1Mb or more:
        doc       3.0Mb
        extdata   1.6Mb
        libs      5.5Mb
    ```

# ggbreak

<details>

* Version: 0.1.2
* GitHub: https://github.com/YuLab-SMU/ggbreak
* Source code: https://github.com/cran/ggbreak
* Date/Publication: 2023-06-26 05:40:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "ggbreak")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggbreak-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_wrap
    > ### Title: scale-wrap
    > ### Aliases: scale_wrap
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > library(ggbreak)
    > p <- ggplot(economics, aes(x=date, y = unemploy, colour = uempmed)) +
    +      geom_line()
    > p + scale_wrap(n=4)
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> -> print.ggwrap
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggbreak.Rmd’
      ...
    > p1 + p2
    
    > p2 + scale_x_break(c(18, 21))
    
    > p1 + scale_x_break(c(7, 17), scales = 1.5) + scale_x_break(c(18, 
    +     21), scales = 2)
    
      When sourcing ‘ggbreak.R’:
    Error: second argument must be a list
    Execution halted
    
      ‘ggbreak.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggbreak.Rmd’ using rmarkdown
    ```

# ggcharts

<details>

* Version: 0.2.1
* GitHub: https://github.com/thomas-neitmann/ggcharts
* Source code: https://github.com/cran/ggcharts
* Date/Publication: 2020-05-20 00:40:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "ggcharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggcharts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bar_chart
    > ### Title: Bar Chart
    > ### Aliases: bar_chart column_chart
    > 
    > ### ** Examples
    > 
    > data(biomedicalrevenue)
    > revenue2018 <- biomedicalrevenue[biomedicalrevenue$year == 2018, ]
    > revenue_roche <- biomedicalrevenue[biomedicalrevenue$company == "Roche", ]
    > 
    > ## By default bar_chart() creates a horizontal and sorted plot
    > bar_chart(revenue2018, company, revenue)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: bar_chart ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggcharts)
      Loading required package: ggplot2
      > 
      > test_check("ggcharts")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 4 ]
      
    ...
       4. └─ggcharts::bar_chart(revenue_2018, company, revenue)
       5.   └─ggplot2:::`+.gg`(...)
       6.     └─ggplot2:::add_ggplot(e1, e2, e2name)
       7.       ├─ggplot2::ggplot_add(object, p, objectname)
       8.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       9.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 4 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘customize.Rmd’
      ...
    > dreaded_lang <- tibble::tribble(~language, ~pct, "VBA", 
    +     75.2, "Objective-C", 68.7, "Assembly", 64.4, "C", 57.5, "PHP", 
    +     54.2, "Erlang", .... [TRUNCATED] 
    
    > (chart <- lollipop_chart(dreaded_lang, language, pct, 
    +     highlight = "R"))
    
    ...
    
    > lollipop_chart(diamonds, cut, highlight = "Good")
    
      When sourcing ‘themes.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘customize.Rmd’ using ‘UTF-8’... failed
      ‘highlight.Rmd’ using ‘UTF-8’... failed
      ‘themes.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘customize.Rmd’ using rmarkdown
    
    Quitting from lines 25-44 [unnamed-chunk-2] (customize.Rmd)
    Error: processing vignette 'customize.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘customize.Rmd’
    
    --- re-building ‘highlight.Rmd’ using rmarkdown
    
    Quitting from lines 37-44 [unnamed-chunk-2] (highlight.Rmd)
    Error: processing vignette 'highlight.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘highlight.Rmd’
    
    --- re-building ‘themes.Rmd’ using rmarkdown
    ```

# ggdark

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/ggdark
* Date/Publication: 2019-01-11 17:30:06 UTC
* Number of recursive dependencies: 46

Run `revdepcheck::cloud_details(, "ggdark")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggdark-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dark_mode
    > ### Title: Activate dark mode on a 'ggplot2' theme
    > ### Aliases: dark_mode
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > 
    > p1 <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
    +   geom_point()
    > 
    > p1  # theme returned by theme_get()
    > p1 + dark_mode()  # activate dark mode on theme returned by theme_get()
    Error in match(x, table, nomatch = 0L) : 
      'match' requires vector arguments
    Calls: dark_mode -> %in%
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggdark)
      > 
      > test_check("ggdark")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      ── Error ('test_dark_mode.R:10:1'): (code run outside of `test_that()`) ────────
      Error in `match(x, table, nomatch = 0L)`: 'match' requires vector arguments
      Backtrace:
          ▆
       1. └─ggdark::dark_mode(light_theme) at test_dark_mode.R:10:1
       2.   └─geoms[["GeomPoint"]]$default_aes$colour %in% ...
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggdist

<details>

* Version: 3.3.2
* GitHub: https://github.com/mjskay/ggdist
* Source code: https://github.com/cran/ggdist
* Date/Publication: 2024-03-05 05:30:23 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "ggdist")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggdist-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Pr_
    > ### Title: Probability expressions in ggdist aesthetics
    > ### Aliases: Pr_ p_
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    + )
    > 
    > # map density onto alpha of the fill
    > ggplot(df, aes(y = name, xdist = d)) +
    +   stat_slabinterval(aes(alpha = !!p_(x)))
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NU
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
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
      • test.stat_sample_slabinterval/nas-with-na-rm-true.svg
      • test.subguide/dots-subguide-with-side-vertical.svg
      • test.subguide/integer-subguide-with-zero-range.svg
      • test.subguide/slab-subguide-with-inside-labels-vertical.svg
      • test.subguide/slab-subguide-with-outside-labels-vert.svg
      • test.subguide/slab-subguide-with-outside-labels.svg
      • test.subguide/slab-subguide-with-side-vertical.svg
      • test.theme_ggdist/facet-titles-on-left.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dotsinterval.Rmd’ using rmarkdown
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    
    Quitting from lines 49-161 [dotsinterval_components] (dotsinterval.Rmd)
    Error: processing vignette 'dotsinterval.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ...
    
    --- re-building ‘freq-uncertainty-vis.Rmd’ using rmarkdown
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘dotsinterval.Rmd’
      ...
    +     xdist = dist)) + geom_hline(yintercept = 0:1, color = "gray95") + 
    +     stat_dotsin .... [TRUNCATED] 
    
      When sourcing ‘dotsinterval.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `use_defaults()`:
    ...
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 
        0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 7, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), 
    Execution halted
    
      ‘dotsinterval.Rmd’ using ‘UTF-8’... failed
      ‘freq-uncertainty-vis.Rmd’ using ‘UTF-8’... failed
      ‘lineribbon.Rmd’ using ‘UTF-8’... failed
      ‘slabinterval.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    1.3Mb
        help   1.5Mb
    ```

# ggeasy

<details>

* Version: 0.1.4
* GitHub: https://github.com/jonocarroll/ggeasy
* Source code: https://github.com/cran/ggeasy
* Date/Publication: 2023-03-12 10:00:23 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "ggeasy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggeasy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: easy_labs
    > ### Title: Easily add ggplot labels using label attribute of 'data.frame'
    > ###   column
    > ### Aliases: easy_labs
    > 
    > ### ** Examples
    > 
    ...
    +   ggplot2::geom_line(ggplot2::aes(colour=Species))
    > 
    > p
    > 
    > p + easy_labs()
    > p + easy_labs(title = "Plot Title", subtitle = 'Plot Subtitle', x = 'x axis label')
    Error in utils::modifyList(p_labs, as.list(unlist(man_labs))) : 
      is.list(x) is not TRUE
    Calls: +.gg ... ggplot_add.easy_labs -> easy_update_labs -> <Anonymous> -> stopifnot
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggeasy)
      > 
      > test_check("ggeasy")
      [ FAIL 6 | WARN 0 | SKIP 1 | PASS 505 ]
      
      ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
    ...
       1. └─ggeasy (local) expect_eqNe(easy_res$labels[sort(names(easy_res$labels))], hard_res$labels[sort(names(hard_res$labels))]) at test-labs.R:76:3
       2.   └─testthat::expect_equal(..., check.environment = FALSE) at test-labs.R:6:16
      
      [ FAIL 6 | WARN 0 | SKIP 1 | PASS 505 ]
      Deleting unused snapshots:
      • labs/labels-attrib.svg
      • labs/labels-manual.svg
      • labs/labels-mytitle.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘shortcuts.Rmd’
      ...
    
    > p1 <- p + labs(title = "default labels")
    
    > p2 <- p + easy_labs() + labs(title = "Replace titles with column labels")
    
    > p3 <- p + easy_labs(x = "My x axis") + labs(title = "Manually add x axis label")
    
      When sourcing ‘shortcuts.R’:
    Error: is.list(x) is not TRUE
    Execution halted
    
      ‘shortcuts.Rmd’ using ‘UTF-8’... failed
      ‘tests_and_coverage.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘shortcuts.Rmd’ using rmarkdown
    ```

# ggedit

<details>

* Version: 0.4.1
* GitHub: https://github.com/yonicd/ggedit
* Source code: https://github.com/cran/ggedit
* Date/Publication: 2024-03-04 14:40:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "ggedit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggedit-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dput.ggedit
    > ### Title: Convert ggplot object to a string call
    > ### Aliases: dput.ggedit
    > 
    > ### ** Examples
    > 
    > 
    >  pList$pointSmooth #original compiled plot
    `geom_smooth()` using formula = 'y ~ x'
    Error in compute_geom_2(..., self = self) : 
      unused arguments (list(6), list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, 
    Calls: <Anonymous> ... get_layer_key -> Map -> mapply -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

# ggESDA

<details>

* Version: 0.2.0
* GitHub: https://github.com/kiangkiangkiang/ggESDA
* Source code: https://github.com/cran/ggESDA
* Date/Publication: 2022-08-19 08:40:10 UTC
* Number of recursive dependencies: 214

Run `revdepcheck::cloud_details(, "ggESDA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggESDA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BLOOD
    > ### Title: BLOOD data example
    > ### Aliases: BLOOD
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > data(BLOOD)
    > ggInterval_minmax(BLOOD, aes(x = Hematocrit))
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: ggInterval_minmax ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggESDA.Rmd’
      ...
    [1] "knit_image_paths" "knit_asis"       
    
    > CONCEPT <- rep(c("FRA", "HUS", "INC", "ISA", "JPL", 
    +     "KHA", "LOT", "PHI", "ROM"), each = 3)
    
    > p <- ggInterval_PCA(facedata, poly = T, concepts_group = CONCEPT)
    
      When sourcing ‘ggESDA.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘ggESDA.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggESDA.Rmd’ using rmarkdown
    
    Quitting from lines 390-406 [ggInterval_PCA] (ggESDA.Rmd)
    Error: processing vignette 'ggESDA.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘ggESDA.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggESDA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggExtra

<details>

* Version: 0.10.1
* GitHub: https://github.com/daattali/ggExtra
* Source code: https://github.com/cran/ggExtra
* Date/Publication: 2023-08-21 14:40:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "ggExtra")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggExtra.Rmd’
      ...
    
    > p1 <- ggplot(df1, aes(x, y)) + geom_point() + theme_bw()
    
    > p1
    
    > ggMarginal(p1)
    
      When sourcing ‘ggExtra.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘ggExtra.Rmd’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggExtra.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘scales’ ‘utils’
      All declared Imports should be used.
    ```

# ggfacto

<details>

* Version: 0.3.1
* GitHub: https://github.com/BriceNocenti/ggfacto
* Source code: https://github.com/cran/ggfacto
* Date/Publication: 2024-08-30 15:00:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "ggfacto")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfacto-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MCA2
    > ### Title: Multiple Correspondence Analysis
    > ### Aliases: MCA2
    > 
    > ### ** Examples
    > 
    >  data(tea, package = "FactoMineR")
    > res.mca <- MCA2(tea, active_vars = 1:18)
    > 
    > res.mca %>%
    +   ggmca(tea, sup_vars = c("SPC"), ylim = c(NA, 1.2), text_repel = TRUE) %>%
    +   ggi() #to make the graph interactive
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: %>% ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# ggfixest

<details>

* Version: 0.1.0
* GitHub: https://github.com/grantmcdermott/ggfixest
* Source code: https://github.com/cran/ggfixest
* Date/Publication: 2023-12-14 08:00:06 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "ggfixest")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > ## Throttle CPU threads if R CMD check (for CRAN)
      > 
      > if (any(grepl("_R_CHECK", names(Sys.getenv()), fixed = TRUE))) {
      +     # fixest
      +     if (requireNamespace("fixest", quietly = TRUE)) {
      +         library(fixest)
      +         setFixest_nthreads(1)
    ...
      ----- FAILED[]: test_ggiplot.R<192--192>
       call| expect_snapshot_plot(p19a, label = "ggiplot_multi_complex_kitchen_iid")
       diff| 1774
       info| Diff plot saved to: _tinysnapshot_review/ggiplot_multi_complex_kitchen_iid.png
      ----- FAILED[]: test_ggiplot.R<193--193>
       call| expect_snapshot_plot(p19b, label = "ggiplot_multi_complex_kitchen_iid")
       diff| 1774
       info| Diff plot saved to: _tinysnapshot_review/ggiplot_multi_complex_kitchen_iid.png
      Error: 16 out of 101 tests failed
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggiplot.Rmd’
      ...
    > iplot(list(TWFE = est_twfe_grp, `Sun & Abraham (2020)` = est_sa20_grp), 
    +     ref.line = -1, main = "Staggered treatment: Split mutli-sample")
    The degrees of freedom for the t distribution could not be deduced. Using a Normal distribution instead.
    Note that you can provide the argument `df.t` directly.
    
      When sourcing ‘ggiplot.R’:
    Error: in iplot(list(TWFE = est_twfe_grp, `Sun & Abraham (2...: 
    The 1st element of 'object' raises and error:
    Error in nb * sd : non-numeric argument to binary operator
    Execution halted
    
      ‘ggiplot.Rmd’ using ‘UTF-8’... failed
    ```

# ggfocus

<details>

* Version: 1.0.0
* GitHub: https://github.com/Freguglia/ggfocus
* Source code: https://github.com/cran/ggfocus
* Date/Publication: 2020-01-23 13:20:02 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::cloud_details(, "ggfocus")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction_to_ggfocus.Rmd’
      ...
    +     geom_point() + scale_alpha_focus(c("A", "B"), alpha_other = 0.5) + 
    +     scale_c .... [TRUNCATED] 
    
    > ggplot(datasets::airquality, aes(x = Day, y = Temp, 
    +     linetype = factor(Month), group = factor(Month))) + geom_line() + 
    +     scale_linetype_f .... [TRUNCATED] 
    
      When sourcing ‘introduction_to_ggfocus.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘introduction_to_ggfocus.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction_to_ggfocus.Rmd’ using rmarkdown
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggforce

<details>

* Version: 0.4.2
* GitHub: https://github.com/thomasp85/ggforce
* Source code: https://github.com/cran/ggforce
* Date/Publication: 2024-02-19 11:00:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ggforce")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggforce-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: facet_row
    > ### Title: One-dimensional facets
    > ### Aliases: facet_row facet_col
    > 
    > ### ** Examples
    > 
    > # Standard use
    > ggplot(mtcars) +
    +   geom_point(aes(disp, mpg)) +
    +   facet_col(~gear)
    Error in space$x : $ operator is invalid for atomic vectors
    Calls: <Anonymous> ... <Anonymous> -> draw_panels -> <Anonymous> -> init_gtable
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.8Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   1.2Mb
        libs  25.0Mb
    ```

# ggformula

<details>

* Version: 0.12.0
* GitHub: https://github.com/ProjectMOSAIC/ggformula
* Source code: https://github.com/cran/ggformula
* Date/Publication: 2023-11-09 12:30:07 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "ggformula")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggformula-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: discrete_breaks
    > ### Title: Discrete Breaks
    > ### Aliases: discrete_breaks
    > 
    > ### ** Examples
    > 
    > x <- rbinom(100, 100, 0.4)
    > p <- gf_bar( ~ x)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: gf_bar ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggformula)
      Loading required package: ggplot2
      Loading required package: scales
      Loading required package: ggridges
      
      New to ggformula?  Try the tutorials: 
    ...
      • layer-factory/gf-text1.svg
      • layer-factory/gf-text2.svg
      • layer-factory/gf-tile1.svg
      • layer-factory/proportions-within-all-dodge.svg
      • layer-factory/proportions-within-fill-dodge.svg
      • layer-factory/proportions-within-fill-facet-grid-and-group.svg
      • layer-factory/proportions-within-fill-facet-grid.svg
      • layer-factory/proportions-within-group-facet-grid.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggformula.Rmd’
      ...
    
    > theme_set(theme_light())
    
    > library(ggformula)
    
    > gf_point(mpg ~ hp, data = mtcars)
    
      When sourcing ‘ggformula.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘ggformula.Rmd’ using ‘UTF-8’... failed
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'gf_abline':
    gf_hline
      Code: function(object = NULL, gformula = NULL, data = NULL, ...,
                     yintercept, color, linetype, linewidth, alpha, xlab,
                     ylab, title, subtitle, caption, position = "identity",
                     show.legend = NA, show.help = NULL, inherit = FALSE,
                     environment = parent.frame())
      Docs: function(object = NULL, gformula = NULL, data = NULL, ...,
                     yintercept, color, linetype, linewidth, alpha, xlab,
                     ylab, title, subtitle, caption, show.legend = NA,
    ...
                     xintercept, color, linetype, linewidth, alpha, xlab,
                     ylab, title, subtitle, caption, show.legend = NA,
                     show.help = NULL, inherit = FALSE, environment =
                     parent.frame())
      Argument names in code not in docs:
        position
      Mismatches in argument names (first 3):
        Position: 15 Code: position Docs: show.legend
        Position: 16 Code: show.legend Docs: show.help
        Position: 17 Code: show.help Docs: inherit
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggformula.Rmd’ using rmarkdown
    
    Quitting from lines 106-109 [simple-example] (ggformula.Rmd)
    Error: processing vignette 'ggformula.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘ggformula.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggformula.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘akima’, ‘ggforce’
    ```

# ggfortify

<details>

* Version: 0.4.17
* GitHub: https://github.com/sinhrks/ggfortify
* Source code: https://github.com/cran/ggfortify
* Date/Publication: 2024-04-17 04:30:04 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "ggfortify")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfortify-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.cv.glmnet
    > ### Title: Autoplot 'glmnet::cv.glmnet'
    > ### Aliases: autoplot.cv.glmnet
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("survival", quietly = TRUE)) {
    +   autoplot(glmnet::cv.glmnet(data.matrix(Orange[-3]), data.matrix(Orange[3])))
    + }
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: autoplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > suppressWarnings(RNGversion("3.5.0"))
      > set.seed(1, sample.kind = "Rejection")
      > 
      > test_check('ggfortify')
      Loading required package: ggfortify
    ...
       3.   └─ggfortify:::autoplot.ts(original.data, columns = "Data", ...)
       4.     └─ggplot2:::`+.gg`(p, do.call(geom_factory, args))
       5.       └─ggplot2:::add_ggplot(e1, e2, e2name)
       6.         ├─ggplot2::ggplot_add(object, p, objectname)
       7.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       8.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 32 | WARN 10 | SKIP 47 | PASS 358 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basics.Rmd’
      ...
    +     warning = FALSE)
    
    > library(ggfortify)
    Loading required package: ggplot2
    
    > autoplot(AirPassengers)
    
    ...
    Error: argument is of length zero
    Execution halted
    
      ‘basics.Rmd’ using ‘UTF-8’... failed
      ‘plot_dist.Rmd’ using ‘UTF-8’... failed
      ‘plot_lm.Rmd’ using ‘UTF-8’... OK
      ‘plot_map.Rmd’ using ‘UTF-8’... failed
      ‘plot_pca.Rmd’ using ‘UTF-8’... OK
      ‘plot_surv.Rmd’ using ‘UTF-8’... failed
      ‘plot_ts.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘basics.Rmd’ using knitr
    
    Attaching package: 'zoo'
    
    The following objects are masked from 'package:base':
    
        as.Date, as.Date.numeric
    
    
    ...
    
    Quitting from lines 20-22 [unnamed-chunk-1] (plot_dist.Rmd)
    Error: processing vignette 'plot_dist.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘plot_dist.Rmd’
    
    --- re-building ‘plot_lm.Rmd’ using knitr
    --- finished re-building ‘plot_lm.Rmd’
    
    --- re-building ‘plot_map.Rmd’ using knitr
    ```

# gggenomes

<details>

* Version: 1.0.1
* GitHub: https://github.com/thackl/gggenomes
* Source code: https://github.com/cran/gggenomes
* Date/Publication: 2024-08-30 11:40:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "gggenomes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gggenomes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: flip
    > ### Title: Flip bins and sequences
    > ### Aliases: flip flip_seqs sync
    > 
    > ### ** Examples
    > 
    > library(patchwork)
    ...
    > p4 <- p %>%
    +   add_clusters(emale_cogs) %>%
    +   sync() + labs(caption = "shared orthologs")
    Joining with `by = join_by(feat_id)`
    Flipping: E4-10_086,E4-10_112,RCC970_016B
    > 
    > p0 + p1 + p2 + p3 + p4 + plot_layout(nrow = 1, guides = "collect")
    Error in as.unit(value) : object is not coercible to a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> [<- -> [<-.unit -> as.unit
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘flip.Rmd’
      ...
    > p4 <- p %>% add_clusters(emale_cogs) %>% sync() + 
    +     labs(caption = "shared orthologs")
    Joining with `by = join_by(feat_id)`
    Flipping: E4-10_086,E4-10_112,RCC970_016B
    
    > p0 + p1 + p2 + p3 + p4 + plot_layout(nrow = 1, guides = "collect")
    
      When sourcing ‘flip.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘emales.Rmd’ using ‘UTF-8’... OK
      ‘flip.Rmd’ using ‘UTF-8’... failed
      ‘gggenomes.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘emales.Rmd’ using rmarkdown
    --- finished re-building ‘emales.Rmd’
    
    --- re-building ‘flip.Rmd’ using rmarkdown
    
    Quitting from lines 17-44 [unnamed-chunk-2] (flip.Rmd)
    Error: processing vignette 'flip.Rmd' failed with diagnostics:
    object is not coercible to a unit
    --- failed re-building ‘flip.Rmd’
    ...
                            virophages)
    emale_tirs              Terminal inverted repeats of 6 EMALE genomes
    
    --- finished re-building ‘gggenomes.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘flip.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggh4x

<details>

* Version: 0.2.8
* GitHub: https://github.com/teunbrand/ggh4x
* Source code: https://github.com/cran/ggh4x
* Date/Publication: 2024-01-23 21:00:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "ggh4x")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggh4x)
      Loading required package: ggplot2
      > 
      > test_check("ggh4x")
      [ FAIL 1 | WARN 20 | SKIP 18 | PASS 757 ]
      
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-facet_wrap2.R:64:3'): facet_wrap2() can some repeat axes ─────
      sum(ctrl) (`actual`) not equal to 4L (`expected`).
      
        `actual`: 3
      `expected`: 4
      
      [ FAIL 1 | WARN 20 | SKIP 18 | PASS 757 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Facets.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Statistics.Rmd’
      ...
    > incorrect$x[15] <- sqrt(2)
    
    > ggplot(incorrect, aes(x, colour = group)) + stat_theodensity(distri = "nbinom")
    
      When sourcing ‘Statistics.R’:
    Error: Problem while computing stat.
    ℹ Error occurred in the 1st layer.
    Caused by error in `setup_params()`:
    ! A discrete 'nbinom' distribution cannot be fitted to continuous data.
    Execution halted
    
      ‘Facets.Rmd’ using ‘UTF-8’... OK
      ‘Miscellaneous.Rmd’ using ‘UTF-8’... OK
      ‘PositionGuides.Rmd’ using ‘UTF-8’... OK
      ‘Statistics.Rmd’ using ‘UTF-8’... failed
      ‘ggh4x.Rmd’ using ‘UTF-8’... OK
    ```

# gghighlight

<details>

* Version: 0.4.1
* GitHub: https://github.com/yutannihilation/gghighlight
* Source code: https://github.com/cran/gghighlight
* Date/Publication: 2023-12-16 01:00:02 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "gghighlight")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gghighlight-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gghighlight
    > ### Title: Highlight Data With Predicate
    > ### Aliases: gghighlight
    > 
    > ### ** Examples
    > 
    > d <- data.frame(
    ...
      8. │           ├─purrr:::with_indexed_errors(...)
      9. │           │ └─base::withCallingHandlers(...)
     10. │           ├─purrr:::call_with_cleanup(...)
     11. │           └─gghighlight (local) .f(.x[[i]], .y[[i]], ...)
     12. │             └─gghighlight:::get_default_aes_param(nm, layer$geom, layer$mapping)
     13. └─base::.handleSimpleError(...)
     14.   └─purrr (local) h(simpleError(msg, call))
     15.     └─cli::cli_abort(...)
     16.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gghighlight)
      Loading required package: ggplot2
      > 
      > test_check("gghighlight")
      label_key: type
      label_key: type
    ...
       15.     └─cli::cli_abort(...)
       16.       └─rlang::abort(...)
      
      [ FAIL 2 | WARN 2 | SKIP 1 | PASS 178 ]
      Deleting unused snapshots:
      • vdiffr/simple-bar-chart-with-facet.svg
      • vdiffr/simple-line-chart.svg
      • vdiffr/simple-point-chart.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gghighlight.Rmd’
      ...
    Warning in is.na(non_null_default_aes[[aes_param_name]]) :
      is.na() applied to non-(list or vector) of type 'language'
    
      When sourcing ‘gghighlight.R’:
    Error: ℹ In index: 1.
    ℹ With name: geom_point.
    Caused by error in `aes_param_name %in% names(non_null_default_aes) && is.na(non_null_default_aes[[
        aes_param_name]])`:
    ! 'length = 2' in coercion to 'logical(1)'
    Execution halted
    
      ‘gghighlight.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘gghighlight.Rmd’ using rmarkdown
    ```

# ggHoriPlot

<details>

* Version: 1.0.1
* GitHub: https://github.com/rivasiker/ggHoriPlot
* Source code: https://github.com/cran/ggHoriPlot
* Date/Publication: 2022-10-11 16:22:33 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "ggHoriPlot")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggHoriPlot.Rmd’
      ...
    > mid <- sum(range(dat_tab$y, na.rm = T))/2
    
    > b <- plotAllLayers(dat_tab, mid, cutpoints$cuts, cutpoints$color)
    
    > b/a + plot_layout(guides = "collect", heights = c(6, 
    +     1))
    
      When sourcing ‘ggHoriPlot.R’:
    Error: object is not a unit
    Execution halted
    
      ‘examples.Rmd’ using ‘UTF-8’... OK
      ‘ggHoriPlot.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘examples.Rmd’ using rmarkdown
    ```

# ggiraph

<details>

* Version: 0.8.10
* GitHub: https://github.com/davidgohel/ggiraph
* Source code: https://github.com/cran/ggiraph
* Date/Publication: 2024-05-17 12:10:02 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "ggiraph")` for more info

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
    > 
    > gg <- ggplot(mtcars, aes(x = disp, y = qsec )) +
    +   geom_point(size=2) +
    +   annotate_interactive(
    +     "rect", xmin = 100, xmax = 400, fill = "red",
    +     data_id = "an_id", tooltip = "a tooltip",
    +     ymin = 18, ymax = 20, alpha = .5)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: +.gg ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
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
      test-geom_density_interactive.R    8 tests [0;32mOK[0m [0;36m8ms[0m
      
      test-geom_dotplot_interactive.R    0 tests    
      test-geom_dotplot_interactive.R    0 tests    
      test-geom_dotplot_interactive.R    0 tests    
      test-geom_dotplot_interactive.R    0 tests    
      test-geom_dotplot_interactive.R    0 tests    
      test-geom_dotplot_interactive.R    8 tests [0;32mOK[0m Error in if (new_name %in% existing) { : argument is of length zero
      Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        libs   7.1Mb
    ```

# ggiraphExtra

<details>

* Version: 0.3.0
* GitHub: https://github.com/cardiomoon/ggiraphExtra
* Source code: https://github.com/cran/ggiraphExtra
* Date/Publication: 2020-10-06 07:00:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "ggiraphExtra")` for more info

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
        addLabelDf, getMapping
    
    > require(ggplot2)
    Loading required package: ggplot2
    > require(ggiraph)
    Loading required package: ggiraph
    > ggAncova(radial,aes(age,NTAV,color=sex),interactive=TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: ggAncova ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggPredict.Rmd’
      ...
    
    
    > require(plyr)
    Loading required package: plyr
    
    > ggPredict(fit, se = TRUE, interactive = TRUE)
    
    ...
    
    > ggPoints(aes(x = wt, y = mpg, color = am), data = mtcars, 
    +     method = "lm", interactive = TRUE)
    
      When sourcing ‘introduction.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘ggPredict.Rmd’ using ‘UTF-8’... failed
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggPredict.Rmd’ using rmarkdown
    ```

# ggmatplot

<details>

* Version: 0.1.2
* GitHub: https://github.com/xuan-liang/ggmatplot
* Source code: https://github.com/cran/ggmatplot
* Date/Publication: 2022-05-17 02:20:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "ggmatplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmatplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggmatplot
    > ### Title: ggmatplot
    > ### Aliases: ggmatplot
    > 
    > ### ** Examples
    > 
    > 
    > # Define a data set
    > iris_sub <- subset(iris, Species == "setosa")
    > ggmatplot(iris_sub[, c(1, 3)], iris_sub[, c(2, 4)])
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: ggmatplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggmatplot)
      Loading required package: ggplot2
      > library(tibble)
      > library(stats)
      > 
      > test_check("ggmatplot")
    ...
      • ggmatplot_parameters/single-color-scatterplot.svg
      • ggmatplot_parameters/single-fill-density-plot.svg
      • ggmatplot_parameters/single-linetype-line-plot.svg
      • ggmatplot_parameters/single-shape-scatterplot.svg
      • ggmatplot_parameters/three-color-violin-plot.svg
      • ggmatplot_parameters/three-fill-color-violin-plot.svg
      • ggmatplot_parameters/three-linetype-line-plot.svg
      • ggmatplot_parameters/three-shape-scatterplot.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggmatplot.Rmd’
      ...
    6 -0.5325916 -0.54457006
    
    > library(ggmatplot)
    Loading required package: ggplot2
    
    > ggmatplot(x, z)
    
      When sourcing ‘ggmatplot.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘ggmatplot.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggmatplot.Rmd’ using rmarkdown
    
    Quitting from lines 47-50 [point-plot] (ggmatplot.Rmd)
    Error: processing vignette 'ggmatplot.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘ggmatplot.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggmatplot.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggmice

<details>

* Version: 0.1.0
* GitHub: https://github.com/amices/ggmice
* Source code: https://github.com/cran/ggmice
* Date/Publication: 2023-08-07 14:20:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "ggmice")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘old_friends.Rmd’
      ...
        layout
    
    
    > p <- plot_flux(dat)
    
    > ggplotly(p)
    
      When sourcing ‘old_friends.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘ggmice.Rmd’ using ‘UTF-8’... OK
      ‘old_friends.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggmice.Rmd’ using rmarkdown
    ```

# ggmulti

<details>

* Version: 1.0.7
* GitHub: NA
* Source code: https://github.com/cran/ggmulti
* Date/Publication: 2024-04-09 09:40:05 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "ggmulti")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmulti-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: coord_radial
    > ### Title: Radial axes
    > ### Aliases: coord_radial
    > 
    > ### ** Examples
    > 
    > if(require("dplyr")) {
    ...
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NU
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > 
      > 
      > library(testthat)
      > library(ggmulti)
      Loading required package: ggplot2
      
      Attaching package: 'ggmulti'
    ...
       16.                         └─base::Map(...)
       17.                           └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       18.                             └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
       19.                               └─layer$compute_geom_2(key, single_params, theme)
       20.                                 └─ggplot2 (local) compute_geom_2(..., self = self)
       21.                                   └─self$geom$use_defaults(...)
      
      [ FAIL 4 | WARN 5 | SKIP 0 | PASS 30 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘glyph.Rmd’
      ...
    +     Sepal.Width, colour = Species), serialaxes.data = iris, axes.layout = "radia ..." ... [TRUNCATED] 
    
      When sourcing ‘glyph.R’:
    Error: Base operators are not defined for quosures. Do you need to unquote the
    quosure?
    
    # Bad: myquosure / rhs
    ...
    
    > p
    
      When sourcing ‘highDim.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘glyph.Rmd’ using ‘UTF-8’... failed
      ‘highDim.Rmd’ using ‘UTF-8’... failed
      ‘histogram-density-.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘glyph.Rmd’ using rmarkdown
    ```

# ggpackets

<details>

* Version: 0.2.1
* GitHub: https://github.com/dgkf/ggpackets
* Source code: https://github.com/cran/ggpackets
* Date/Publication: 2022-10-10 23:30:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "ggpackets")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpackets-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggpacket
    > ### Title: A container for lazy ggplot layers
    > ### Aliases: ggpacket
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > 
    > # create a ggpacket directly, setting some fixed argument settings
    > ggpk_simple <- ggpacket() %+% geom_line(color = "red") %+% geom_point()
    > ggplot(mtcars, aes(x = wt, y = mpg)) + ggpk_simple()
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: +.gg ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > library(ggplot2)
      > library(ggpackets)
      
      Attaching package: 'ggpackets'
      
    ...
       10.           └─ggpackets (local) f(init, x[[i]])
       11.             └─ggplot2:::`+.gg`(gg, ggpk_i)
       12.               └─ggplot2:::add_ggplot(e1, e2, e2name)
       13.                 ├─ggplot2::ggplot_add(object, p, objectname)
       14.                 └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       15.                   └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 4 | WARN 1 | SKIP 0 | PASS 27 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘composing-functions.Rmd’
      ...
    +     geom_point(size = 3)
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
    > ggplot(Loblolly) + aes(x = age, y = height, color = Seed) + 
    +     ggpk_my_template() + ggtitle("Growth of Loblolly Pines")
    
    ...
    > diamonds %>% sample_frac(0.01) %>% arrange(cut) %>% 
    +     ggplot() + aes(color = cut) + ggpk_dot_matrix(size = 3, width = 30)
    
      When sourcing ‘miscellaneous-examples.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘composing-functions.Rmd’ using ‘UTF-8’... failed
      ‘ggpackets.Rmd’ using ‘UTF-8’... failed
      ‘miscellaneous-examples.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘composing-functions.Rmd’ using rmarkdown
    
    Quitting from lines 58-62 [simple_ggpacket_output] (composing-functions.Rmd)
    Error: processing vignette 'composing-functions.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘composing-functions.Rmd’
    
    --- re-building ‘ggpackets.Rmd’ using rmarkdown
    ```

# ggparallel

<details>

* Version: 0.4.0
* GitHub: https://github.com/heike/ggparallel
* Source code: https://github.com/cran/ggparallel
* Date/Publication: 2024-03-09 22:00:02 UTC
* Number of recursive dependencies: 51

Run `revdepcheck::cloud_details(, "ggparallel")` for more info

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
       12.                     └─self$get_layer_key(params, layers[include], data[include], theme)
       13.                       └─ggplot2 (local) get_layer_key(...)
       14.                         └─base::Map(...)
       15.                           └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       16.                             └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
       17.                               └─layer$compute_geom_2(key, single_params, theme)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# ggparty

<details>

* Version: 1.0.0
* GitHub: https://github.com/martin-borkovec/ggparty
* Source code: https://github.com/cran/ggparty
* Date/Publication: 2019-07-18 10:54:06 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "ggparty")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggparty-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_node_label
    > ### Title: Draw (multi-line) labels at nodes
    > ### Aliases: geom_node_label geom_node_info geom_node_splitvar
    > 
    > ### ** Examples
    > 
    > library(ggparty)
    ...
     30. │                         └─ggplot2:::add_ggplot(e1, e2, e2name)
     31. │                           ├─ggplot2::ggplot_add(object, p, objectname)
     32. │                           └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     33. │                             └─ggplot2:::new_layer_names(object, names(plot$layers))
     34. └─base::.handleSimpleError(...)
     35.   └─rlang (local) h(simpleError(msg, call))
     36.     └─handlers[[1L]](cnd)
     37.       └─cli::cli_abort(...)
     38.         └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggparty-graphic-partying.Rmd’
      ...
    +     0.55 .... [TRUNCATED] 
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
      When sourcing ‘ggparty-graphic-partying.R’:
    Error: Problem while converting geom to grob.
    ℹ Error occurred in the 5th layer.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    Execution halted
    
      ‘ggparty-graphic-partying.Rmd’ using ‘UTF-8’... failed
      ‘on-the-edge.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggparty-graphic-partying.Rmd’ using rmarkdown
    
    Quitting from lines 42-122 [unnamed-chunk-2] (ggparty-graphic-partying.Rmd)
    Error: processing vignette 'ggparty-graphic-partying.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 5th layer.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    --- failed re-building ‘ggparty-graphic-partying.Rmd’
    
    --- re-building ‘on-the-edge.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘survival’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggpicrust2

<details>

* Version: 1.7.3
* GitHub: https://github.com/cafferychen777/ggpicrust2
* Source code: https://github.com/cran/ggpicrust2
* Date/Publication: 2023-11-08 16:10:02 UTC
* Number of recursive dependencies: 231

Run `revdepcheck::cloud_details(, "ggpicrust2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpicrust2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pathway_pca
    > ### Title: Perform Principal Component Analysis (PCA) on functional pathway
    > ###   abundance data and create visualizations of the PCA results.
    > ### Aliases: pathway_pca
    > 
    > ### ** Examples
    > 
    ...
    > 
    > # Create example metadata
    > # Please ensure the sample IDs in the metadata have the column name "sample_name"
    > metadata_example <- data.frame(sample_name = colnames(kegg_abundance_example),
    +                                group = factor(rep(c("Control", "Treatment"), each = 5)))
    > 
    > pca_plot <- pathway_pca(kegg_abundance_example, metadata_example, "group")
    Error in identicalUnits(x) : object is not a unit
    Calls: pathway_pca ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ComplexHeatmap’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        R      2.1Mb
        data   2.0Mb
    ```

# ggplotlyExtra

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/ggplotlyExtra
* Date/Publication: 2019-12-02 16:20:06 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "ggplotlyExtra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggplotlyExtra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplotly_histogram
    > ### Title: Clean 'ggplot2' Histogram to be Converted to 'Plotly'
    > ### Aliases: ggplotly_histogram
    > 
    > ### ** Examples
    > 
    > 
    ...
    + xlab("len")
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    Warning in geom_bar(data = layerdata, mapping = aes(x = .data$x, y = .data$count,  :
      Ignoring unknown aesthetics: label1, label2, and label3
    > 
    > # convert `ggplot` object to `plotly` object
    > ggplotly(p, tooltip = c("Range", "count", "density"))
    Error in pm[[2]] : subscript out of bounds
    Calls: ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggPMX

<details>

* Version: 1.2.11
* GitHub: https://github.com/ggPMXdevelopment/ggPMX
* Source code: https://github.com/cran/ggPMX
* Date/Publication: 2023-11-30 16:10:06 UTC
* Number of recursive dependencies: 181

Run `revdepcheck::cloud_details(, "ggPMX")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggPMX-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pmx_config
    > ### Title: This function can be used to define the pmx configuration used
    > ###   in plots. e.g. Monolix/Nonmem
    > ### Aliases: pmx_config
    > 
    > ### ** Examples
    > 
    ...
    +   cats = c("SEX"),
    +   conts = c("WT0", "AGE0"),
    +   strats = "STUD"
    + )
    NO FINEGRID FILE:
            we will use instead predictions.txt for individual plots
    Warning: Duplicated aesthetics after name standardisation: colour
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: pmx_mlx ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggPMX-guide.Rmd’
      ...
    
    > work_dir <- file.path(theophylline, "Monolix")
    
    > input_data <- file.path(theophylline, "data_pk.csv")
    
    > ctr <- theophylline()
    
      When sourcing ‘ggPMX-guide.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘ggPMX-guide.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggPMX-guide.Rmd’ using rmarkdown
    
    Quitting from lines 25-37 [load_package] (ggPMX-guide.Rmd)
    Error: processing vignette 'ggPMX-guide.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘ggPMX-guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggPMX-guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘lixoftConnectors’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        doc        1.1Mb
        help       2.4Mb
        testdata   4.8Mb
    ```

# ggpol

<details>

* Version: 0.0.7
* GitHub: https://github.com/erocoar/ggpol
* Source code: https://github.com/cran/ggpol
* Date/Publication: 2020-11-08 13:40:02 UTC
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "ggpol")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpol-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GeomConfmat
    > ### Title: Confusion Matrix
    > ### Aliases: GeomConfmat geom_confmat stat_confmat
    > 
    > ### ** Examples
    > 
    > x <- sample(LETTERS[seq(4)], 50, replace = TRUE)
    ...
     21. │                     └─ggpol (local) draw_panel(...)
     22. │                       └─base::lapply(GeomText$default_aes[missing_aes], rlang::eval_tidy)
     23. │                         └─rlang (local) FUN(X[[i]], ...)
     24. ├─ggplot2::from_theme(fontsize)
     25. └─base::.handleSimpleError(...)
     26.   └─rlang (local) h(simpleError(msg, call))
     27.     └─handlers[[1L]](cnd)
     28.       └─cli::cli_abort(...)
     29.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘grDevices’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ggprism

<details>

* Version: 1.0.5
* GitHub: https://github.com/csdaw/ggprism
* Source code: https://github.com/cran/ggprism
* Date/Publication: 2024-03-21 10:50:02 UTC
* Number of recursive dependencies: 97

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
    +   "OJ",    "VC",    0.0606, 36
    + )
    > 
    > # boxplot (or another geom...)
    > ggplot(tg, aes(x = supp, y = len)) +
    +   geom_boxplot() +
    +   add_pvalue(two.means)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: +.gg ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
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
      test-add_pvalue.R.............    0 tests    
      test-add_pvalue.R.............    0 tests    
      test-add_pvalue.R.............    0 tests    
      test-add_pvalue.R.............    0 tests    
      test-add_pvalue.R.............    0 tests    
      test-add_pvalue.R.............    0 tests    
      test-add_pvalue.R.............    0 tests    
      test-add_pvalue.R.............    0 tests    Error in if (new_name %in% existing) { : argument is of length zero
      Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggprism.Rmd’
      ...
    +     p.adj = 0.0606, y.position = 36)
    
    > p1 <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
    +     geom_boxplot(aes(fill = factor(supp))) + scale_fill_prism(palette = "candy_bright ..." ... [TRUNCATED] 
    
    > p2 <- p1 + add_pvalue(df_p_val)
    
    ...
    
      When sourcing ‘pvalues.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘axes.Rmd’ using ‘UTF-8’... OK
      ‘colours.Rmd’ using ‘UTF-8’... OK
      ‘ggprism.Rmd’ using ‘UTF-8’... failed
      ‘pvalues.Rmd’ using ‘UTF-8’... failed
      ‘themes.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘axes.Rmd’ using rmarkdown
    ```

# ggpubr

<details>

* Version: 0.6.0
* GitHub: https://github.com/kassambara/ggpubr
* Source code: https://github.com/cran/ggpubr
* Date/Publication: 2023-02-10 16:20:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "ggpubr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpubr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: add_summary
    > ### Title: Add Summary Statistics onto a ggplot.
    > ### Aliases: add_summary mean_se_ mean_sd mean_ci mean_range median_iqr
    > ###   median_hilow_ median_q1q3 median_mad median_range
    > 
    > ### ** Examples
    > 
    ...
     10. │         └─ggplot2:::`+.gg`(...)
     11. │           └─ggplot2:::add_ggplot(e1, e2, e2name)
     12. │             ├─ggplot2::ggplot_add(object, p, objectname)
     13. │             └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     14. │               └─ggplot2:::new_layer_names(object, names(plot$layers))
     15. └─base::.handleSimpleError(...)
     16.   └─purrr (local) h(simpleError(msg, call))
     17.     └─cli::cli_abort(...)
     18.       └─rlang::abort(...)
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
      [ FAIL 46 | WARN 3 | SKIP 0 | PASS 51 ]
      
    ...
       13. │             └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       14. │               └─ggplot2:::new_layer_names(object, names(plot$layers))
       15. └─base::.handleSimpleError(...)
       16.   └─purrr (local) h(simpleError(msg, call))
       17.     └─cli::cli_abort(...)
       18.       └─rlang::abort(...)
      
      [ FAIL 46 | WARN 3 | SKIP 0 | PASS 51 ]
      Error: Test failures
      Execution halted
    ```

# ggrain

<details>

* Version: 0.0.4
* GitHub: https://github.com/njudd/ggrain
* Source code: https://github.com/cran/ggrain
* Date/Publication: 2024-01-23 11:50:02 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "ggrain")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggrain-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_rain
    > ### Title: Raincloud Plots
    > ### Aliases: geom_rain
    > 
    > ### ** Examples
    > 
    > e1 <- ggplot(iris, aes(Species, Sepal.Width, fill = Species))
    > e1 + geom_rain()
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: +.gg ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggrain.Rmd’
      ...
    > library(ggrain)
    Loading required package: ggplot2
    
    > ggplot(iris, aes(1, Sepal.Width)) + geom_rain() + 
    +     theme_classic() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
    +   .... [TRUNCATED] 
    
      When sourcing ‘ggrain.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘ggrain.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘ggrain.Rmd’ using rmarkdown
    
    Quitting from lines 36-41 [most basic raincloud possible] (ggrain.Rmd)
    Error: processing vignette 'ggrain.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘ggrain.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘ggrain.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# ggraph

<details>

* Version: 2.2.1
* GitHub: https://github.com/thomasp85/ggraph
* Source code: https://github.com/cran/ggraph
* Date/Publication: 2024-03-07 12:40:02 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "ggraph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggraph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_conn_bundle
    > ### Title: Create hierarchical edge bundles between node connections
    > ### Aliases: geom_conn_bundle geom_conn_bundle2 geom_conn_bundle0
    > 
    > ### ** Examples
    > 
    > # Create a graph of the flare class system
    ...
    +   ) +
    +   geom_node_point(aes(filter = leaf, colour = class)) +
    +   scale_edge_colour_distiller('', direction = 1, guide = 'edge_direction') +
    +   coord_fixed() +
    +   ggforce::theme_no_axes()
    Error in get_layer_key(...) : 
      unused argument (list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, list(), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 
        0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, 
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> process_layers -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Edges.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Edges.Rmd’
      ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    
    ...
      font family 'Arial' not found in PostScript font database
    
      When sourcing ‘tidygraph.R’:
    Error: invalid font type
    Execution halted
    
      ‘Edges.Rmd’ using ‘UTF-8’... failed
      ‘Layouts.Rmd’ using ‘UTF-8’... failed
      ‘Nodes.Rmd’ using ‘UTF-8’... failed
      ‘tidygraph.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    3.9Mb
        libs   2.8Mb
    ```

# ggredist

<details>

* Version: 0.0.2
* GitHub: https://github.com/alarm-redist/ggredist
* Source code: https://github.com/cran/ggredist
* Date/Publication: 2022-11-23 11:20:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "ggredist")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggredist-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_district_text
    > ### Title: Label Map Regions
    > ### Aliases: geom_district_text geom_district_label
    > ###   stat_district_coordinates StatDistrictCoordinates GeomDistrictText
    > ### Keywords: datasets
    > 
    > ### ** Examples
    ...
     22. │                       └─coord$transform(data, panel_params)
     23. │                         └─ggplot2 (local) transform(..., self = self)
     24. │                           └─ggplot2:::sf_rescale01(...)
     25. │                             └─sf::st_normalize(x, c(x_range[1], y_range[1], x_range[2], y_range[2]))
     26. └─base::.handleSimpleError(...)
     27.   └─rlang (local) h(simpleError(msg, call))
     28.     └─handlers[[1L]](cnd)
     29.       └─cli::cli_abort(...)
     30.         └─rlang::abort(...)
    Execution halted
    ```

# ggRtsy

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ggRtsy
* Date/Publication: 2023-09-15 19:12:05 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ggRtsy")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggplot2)
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following object is masked from 'package:testthat':
    ...
       13. │         │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
       14. │         └─vctrs::vec_as_location(i, n, names = names, arg = arg, call = call)
       15. └─vctrs (local) `<fn>`()
       16.   └─vctrs:::stop_subscript_oob(...)
       17.     └─vctrs:::stop_subscript(...)
       18.       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 3 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Vignette.Rmd’
      ...
    |Antique White |(238, 223, 204) |#eedfcc |
    
    > RectangleFiller(plotExample, c("#e32636", "#9966cc", 
    +     "#f4c2c2", "#e16827"))
    
      When sourcing ‘Vignette.R’:
    Error: Can't extract rows past the end.
    ℹ Location 1 doesn't exist.
    ℹ There are only 0 rows.
    Execution halted
    
      ‘Vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Vignette.Rmd’ using rmarkdown
    
    Quitting from lines 48-49 [unnamed-chunk-2] (Vignette.Rmd)
    Error: processing vignette 'Vignette.Rmd' failed with diagnostics:
    Can't extract rows past the end.
    ℹ Location 1 doesn't exist.
    ℹ There are only 0 rows.
    --- failed re-building ‘Vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 19 marked UTF-8 strings
    ```

# ggseqplot

<details>

* Version: 0.8.4
* GitHub: https://github.com/maraab23/ggseqplot
* Source code: https://github.com/cran/ggseqplot
* Date/Publication: 2024-05-17 21:40:03 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "ggseqplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggseqplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggseqrfplot
    > ### Title: Relative Frequency Sequence Plot
    > ### Aliases: ggseqrfplot
    > 
    > ### ** Examples
    > 
    > # Load additional library for fine-tuning the plots
    ...
     [>] Pseudo/medoid-based-F statistic: 6.870317, p-value: 3.09994e-08
    > 
    > # ... with ggseqrfplot
    > ggseqrfplot(biofam.seq, weighted = FALSE, diss = diss, k = 12, grp.meth="first")
     [>] Using k=12 frequency groups with grp.meth='first'
     [>] Pseudo/medoid-based-R2: 0.4620155
     [>] Pseudo/medoid-based-F statistic: 6.870317, p-value: 3.09994e-08
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggseqplot)
      Loading required package: TraMineR
      
      TraMineR stable version 2.2-10 (Built: 2024-05-22)
      Website: http://traminer.unige.ch
      Please type 'citation("TraMineR")' for citation information.
    ...
      Backtrace:
          ▆
       1. ├─testthat::expect_s3_class(ggseqtrplot(biofam.seq), "ggplot") at test-ggseqtrplot.R:35:3
       2. │ └─testthat::quasi_label(enquo(object), arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─ggseqplot::ggseqtrplot(biofam.seq)
      
      [ FAIL 1 | WARN 1045 | SKIP 0 | PASS 131 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggseqplot.Rmd’
      ...
    > p1 + p2 + plot_layout(guides = "collect") & scale_fill_manual(values = canva_palettes$`Fun and tropical`[1:4]) & 
    +     theme_ipsum(base_family = "" .... [TRUNCATED] 
    Scale for fill is already present.
    Adding another scale for fill, which will replace the existing scale.
    Scale for fill is already present.
    Adding another scale for fill, which will replace the existing scale.
    
      When sourcing ‘ggseqplot.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘ggseqplot.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ggseqplot.Rmd’ using rmarkdown
    ```

# ggside

<details>

* Version: 0.3.1
* GitHub: https://github.com/jtlandis/ggside
* Source code: https://github.com/cran/ggside
* Date/Publication: 2024-03-01 09:12:37 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "ggside")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggplot2)
      > library(ggside)
      Registered S3 method overwritten by 'ggside':
        method from   
        +.gg   ggplot2
      > 
    ...
      • ops_meaningful/alpha-0-5-from-function.svg
      • side_layers/boxplot2.svg
      • vdiff_irisScatter/collapsed-histo.svg
      • vdiff_irisScatter/facetgrid-collapsed-density.svg
      • vdiff_irisScatter/facetgrid-histo.svg
      • vdiff_irisScatter/facetgrid-side-density.svg
      • vdiff_irisScatter/stacked-side-density.svg
      • vdiff_irisScatter/yside-histo.svg
      Error: Test failures
      Execution halted
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'geom_xsideabline':
    geom_xsidehline
      Code: function(mapping = NULL, data = NULL, position = "identity",
                     ..., yintercept, na.rm = FALSE, show.legend = NA)
      Docs: function(mapping = NULL, data = NULL, ..., yintercept, na.rm =
                     FALSE, show.legend = NA)
      Argument names in code not in docs:
        position
      Mismatches in argument names (first 3):
        Position: 3 Code: position Docs: ...
    ...
      Docs: function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", ..., lineend = "butt", linejoin
                     = "round", linemitre = 10, arrow = NULL, na.rm =
                     FALSE, show.legend = NA, inherit.aes = TRUE)
      Argument names in code not in docs:
        arrow.fill
      Mismatches in argument names:
        Position: 10 Code: arrow.fill Docs: na.rm
        Position: 11 Code: na.rm Docs: show.legend
        Position: 12 Code: show.legend Docs: inherit.aes
    ```

# ggsmc

<details>

* Version: 0.1.2.0
* GitHub: https://github.com/richardgeveritt/ggsmc
* Source code: https://github.com/cran/ggsmc
* Date/Publication: 2024-07-27 17:00:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "ggsmc")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Visualising.Rmd’
      ...
    20 /tmp/RtmpuaCJEi/100d113de41a/gganim_plot0020.png
    
    > data(lv_output)
    
    > animate_reveal_time_series(lv_output, parameters = c("X", 
    +     "Y"), alpha = 0.5, ylimits = c(0, 600), duration = 10)
    
      When sourcing ‘Visualising.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘Visualising.Rmd’ using ‘UTF-8’... failed
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 175 marked UTF-8 strings
    ```

# ggspatial

<details>

* Version: 1.1.9
* GitHub: https://github.com/paleolimbot/ggspatial
* Source code: https://github.com/cran/ggspatial
* Date/Publication: 2023-08-17 15:32:38 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "ggspatial")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggspatial-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotation_spatial_hline
    > ### Title: Projected horizontal and vertical lines
    > ### Aliases: annotation_spatial_hline annotation_spatial_vline
    > ###   GeomSpatialXline
    > ### Keywords: datasets
    > 
    > ### ** Examples
    ...
     25. │                             └─grid:::validGP(list(...))
     26. │                               └─grid (local) numnotnull("fontsize")
     27. │                                 └─grid (local) check.length(gparname)
     28. │                                   └─base::stop(...)
     29. └─base::.handleSimpleError(...)
     30.   └─rlang (local) h(simpleError(msg, call))
     31.     └─handlers[[1L]](cnd)
     32.       └─cli::cli_abort(...)
     33.         └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggspatial)
      > 
      > test_check("ggspatial")
      Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1; sf_use_s2() is TRUE
      [ FAIL 1 | WARN 1 | SKIP 22 | PASS 195 ]
      
    ...
       33. │                                           └─base::stop(...)
       34. └─base::.handleSimpleError(...)
       35.   └─rlang (local) h(simpleError(msg, call))
       36.     └─handlers[[1L]](cnd)
       37.       └─cli::cli_abort(...)
       38.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 22 | PASS 195 ]
      Error: Test failures
      Execution halted
    ```

# ggstatsplot

<details>

* Version: 0.12.4
* GitHub: https://github.com/IndrajeetPatil/ggstatsplot
* Source code: https://github.com/cran/ggstatsplot
* Date/Publication: 2024-07-06 16:22:07 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "ggstatsplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggstatsplot-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggscatterstats
    > ### Title: Scatterplot with marginal distributions and statistical results
    > ### Aliases: ggscatterstats
    > 
    > ### ** Examples
    > 
    > set.seed(123)
    ...
    +   iris,
    +   x = Sepal.Width,
    +   y = Petal.Length,
    +   label.var = Species,
    +   label.expression = Sepal.Length > 7.6
    + ) +
    +   ggplot2::geom_rug(sides = "b")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: ggscatterstats ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ggstatsplot.Rmd’
      ...
        author = {Indrajeet Patil},
        title = {{Visualizations with statistical details: The {'ggstatsplot'} approach}},
        journal = {{Journal of Open Source Software}},
      }
    
    > ggbetweenstats(iris, Species, Sepal.Length)
    
      When sourcing ‘ggstatsplot.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘additional.Rmd’ using ‘UTF-8’... OK
      ‘ggstatsplot.Rmd’ using ‘UTF-8’... failed
    ```

# ggtern

<details>

* Version: 3.5.0
* GitHub: NA
* Source code: https://github.com/cran/ggtern
* Date/Publication: 2024-03-24 21:50:02 UTC
* Number of recursive dependencies: 42

Run `revdepcheck::cloud_details(, "ggtern")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggtern-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotate
    > ### Title: Create an annotation layer (ggtern version).
    > ### Aliases: annotate
    > 
    > ### ** Examples
    > 
    > ggtern() + 
    ...
      3.   ├─ggtern::ggplot_build(x)
      4.   └─ggtern:::ggplot_build.ggplot(x)
      5.     └─ggtern:::layers_add_or_remove_mask(plot)
      6.       └─ggint$plot_theme(plot)
      7.         └─ggplot2:::validate_theme(theme)
      8.           └─base::mapply(...)
      9.             └─ggplot2 (local) `<fn>`(...)
     10.               └─cli::cli_abort(...)
     11.                 └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘sp’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘chemometrics’
    ```

# ggupset

<details>

* Version: 0.4.0
* GitHub: https://github.com/const-ae/ggupset
* Source code: https://github.com/cran/ggupset
* Date/Publication: 2024-06-24 10:10:04 UTC
* Number of recursive dependencies: 46

Run `revdepcheck::cloud_details(, "ggupset")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggupset-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: axis_combmatrix
    > ### Title: Convert delimited text labels into a combination matrix axis
    > ### Aliases: axis_combmatrix
    > 
    > ### ** Examples
    > 
    >   library(ggplot2)
    ...
    Datsun 710        Cyl: 4_Gears: 4
    Hornet 4 Drive    Cyl: 6_Gears: 3
    Hornet Sportabout Cyl: 8_Gears: 3
    Valiant           Cyl: 6_Gears: 3
    >   ggplot(mtcars, aes(x=combined)) +
    +     geom_bar() +
    +     axis_combmatrix(sep = "_")
    Error in as.unit(e2) : object is not coercible to a unit
    Calls: <Anonymous> ... polylineGrob -> is.unit -> unit.c -> Ops.unit -> as.unit
    Execution halted
    ```

# ggVennDiagram

<details>

* Version: 1.5.2
* GitHub: https://github.com/gaospecial/ggVennDiagram
* Source code: https://github.com/cran/ggVennDiagram
* Date/Publication: 2024-02-20 08:10:02 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "ggVennDiagram")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘fully-customed.Rmd’
      ...
    [1] "b" "c" "e" "h" "k" "q" "s" "y"
    
    
    > ggVennDiagram(y, show_intersect = TRUE, set_color = "black")
    Warning in geom_text(aes(label = .data$count, text = .data$item), data = region_label) :
      Ignoring unknown aesthetics: text
    
    ...
      Ignoring unknown aesthetics: text
    
      When sourcing ‘using-ggVennDiagram.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘VennCalculator.Rmd’ using ‘UTF-8’... OK
      ‘fully-customed.Rmd’ using ‘UTF-8’... failed
      ‘using-ggVennDiagram.Rmd’ using ‘UTF-8’... failed
      ‘using-new-shapes.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘VennCalculator.Rmd’ using rmarkdown
    --- finished re-building ‘VennCalculator.Rmd’
    
    --- re-building ‘fully-customed.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        doc    9.5Mb
        help   1.1Mb
    ```

# GimmeMyPlot

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/GimmeMyPlot
* Date/Publication: 2023-10-18 16:10:02 UTC
* Number of recursive dependencies: 114

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
    +     width_text = 5,
    +     pch_colour = "gray30",
    +     pch_alpha = 0.5,
    +     width_title = 30,
    +     lwd = 1.25,
    +     digits = 2
    + )
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot_violin ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
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
       4. └─GimmeMyPlot::plot_violin(df)
       5.   └─ggplot2:::`+.gg`(...)
       6.     └─ggplot2:::add_ggplot(e1, e2, e2name)
       7.       ├─ggplot2::ggplot_add(object, p, objectname)
       8.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       9.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 1 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Tutorial.Rmd’
      ...
    > df[, 3] <- runif(10, 1, 2)
    
    > colnames(df) <- paste0("X", seq(3))
    
    > plot_violin(df, title = "Some random variables", colour = brewer.pal(9, 
    +     "Set1")[seq(3)])
    
      When sourcing ‘Tutorial.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Tutorial.Rmd’ using rmarkdown
    
    Quitting from lines 24-57 [violin] (Tutorial.Rmd)
    Error: processing vignette 'Tutorial.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# giniVarCI

<details>

* Version: 0.0.1-3
* GitHub: NA
* Source code: https://github.com/cran/giniVarCI
* Date/Publication: 2024-01-08 10:30:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "giniVarCI")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘giniVarCI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fcompareCI
    > ### Title: Comparisons of variance estimates and confidence intervals for
    > ###   the Gini index in finite populations
    > ### Aliases: fcompareCI
    > 
    > ### ** Examples
    > 
    ...
    > data(eusilc, package="laeken")
    > y <- eusilc$eqIncome[eusilc$db040 == "Burgenland"]
    > w <- eusilc$rb050[eusilc$db040 == "Burgenland"]
    > 
    > # Estimation of the Gini index and confidence intervals using different methods.
    > fcompareCI(y, w)
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: fcompareCI
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.5Mb
      sub-directories of 1Mb or more:
        libs  12.0Mb
    ```

# gMCPLite

<details>

* Version: 0.1.5
* GitHub: https://github.com/Merck/gMCPLite
* Source code: https://github.com/cran/gMCPLite
* Date/Publication: 2024-01-11 19:30:02 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "gMCPLite")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘GraphicalMultiplicity.Rmd’
      ...
       <td style="text-align:right;"> 0.8600 </td>
       <td style="text-align:right;"> 0.1400 </td>
      </tr>
    </tbody>
    </table>
    > plot(os, plottype = "HR", xlab = "Events")
    
      When sourcing ‘GraphicalMultiplicity.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘GraphicalMultiplicity.Rmd’ using ‘UTF-8’... failed
      ‘hGraph.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘GraphicalMultiplicity.Rmd’ using rmarkdown
    ```

# gMOIP

<details>

* Version: 1.5.2
* GitHub: https://github.com/relund/gMOIP
* Source code: https://github.com/cran/gMOIP
* Date/Publication: 2024-02-21 21:30:05 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "gMOIP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gMOIP-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: convexHull
    > ### Title: Find the convex hull of a set of points.
    > ### Aliases: convexHull
    > 
    > ### ** Examples
    > 
    > ## 1D
    ...
    
    $pts
      p1 p2 pt  vtx
    1  1  1  1 TRUE
    2  2  2  1 TRUE
    
    > plotHull2D(pts, drawPoints = TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plotHull2D ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘bi-objective_2x.Rmd’
      ...
    
    > b <- c(3, 27, 90)
    
    > obj <- matrix(c(7, -10, -10, -10), nrow = 2)
    
    > plotBiObj2D(A, b, obj, addTriangles = FALSE)
    
    ...
      When sourcing ‘polytope_2d.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘bi-objective_2x.Rmd’ using ‘UTF-8’... failed
      ‘bi-objective_3x_ex1.Rmd’ using ‘UTF-8’... OK
      ‘intro.Rmd’ using ‘UTF-8’... failed
      ‘polytope_2d.Rmd’ using ‘UTF-8’... failed
      ‘polytope_3d_ex1.Rmd’ using ‘UTF-8’... OK
      ‘tri-objective.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bi-objective_2x.Rmd’ using rmarkdown
    
    Quitting from lines 73-78 [2DLP] (bi-objective_2x.Rmd)
    Error: processing vignette 'bi-objective_2x.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘bi-objective_2x.Rmd’
    
    --- re-building ‘bi-objective_3x_ex1.Rmd’ using rmarkdown
    ```

# GofCens

<details>

* Version: 1.1
* GitHub: NA
* Source code: https://github.com/cran/GofCens
* Date/Publication: 2024-07-29 08:40:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "GofCens")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GofCens-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kmPlot
    > ### Title: Plot of the Kaplen-Meier and parametric estimations
    > ### Aliases: kmPlot kmPlot.default kmPlot.formula kmPlot.probPlot
    > ###   kmPlot.probPlot
    > 
    > ### ** Examples
    > 
    ...
       Scale: 10.039 
    
    > 
    > # Plots for censored data using ggplot2
    > data(colon)
    Warning in data(colon) : data set ‘colon’ not found
    > kmPlot(Surv(time, status) ~ 1, colon, distr= "lognormal", ggp = TRUE) 
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: kmPlot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# greatR

<details>

* Version: 2.0.0
* GitHub: https://github.com/ruthkr/greatR
* Source code: https://github.com/cran/greatR
* Date/Publication: 2024-04-09 22:40:07 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "greatR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘process-results.Rmd’
      ...
    
    > reg_summary$non_registered_genes
    [1] "BRAA02G018970.3C"
    
    > plot(reg_summary, type = "registered", scatterplot_size = c(4, 
    +     3.5))
    
      When sourcing ‘process-results.R’:
    Error: object is not a unit
    Execution halted
    
      ‘data-requirement.Rmd’ using ‘UTF-8’... OK
      ‘process-results.Rmd’ using ‘UTF-8’... failed
      ‘register-data-manually.Rmd’ using ‘UTF-8’... OK
      ‘register-data.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘data-requirement.Rmd’ using rmarkdown
    --- finished re-building ‘data-requirement.Rmd’
    
    --- re-building ‘process-results.Rmd’ using rmarkdown
    
    Quitting from lines 76-81 [plot-summary-results] (process-results.Rmd)
    Error: processing vignette 'process-results.Rmd' failed with diagnostics:
    object is not a unit
    ...
    --- finished re-building ‘register-data-manually.Rmd’
    
    --- re-building ‘register-data.Rmd’ using rmarkdown
    --- finished re-building ‘register-data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘process-results.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Greymodels

<details>

* Version: 2.0.1
* GitHub: https://github.com/havishaJ/Greymodels
* Source code: https://github.com/cran/Greymodels
* Date/Publication: 2022-12-05 12:42:35 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "Greymodels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Greymodels-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Plots
    > ### Title: plots
    > ### Aliases: plots plotrm plotsmv1 plotsmv2 plotsigndgm plots_mdbgm12
    > 
    > ### ** Examples
    > 
    >   # Plots - EPGM (1, 1) model
    ...
    +     geom_point(data = set4, aes(x = CI, y = y), shape = 23, color = "black") +
    +     geom_line(data = xy1, aes(x = x, y = y,color = "Raw Data")) +
    +     geom_line(data = xy2, aes(x = x, y = y,color = "Fitted&Forecasts")) +
    +     geom_line(data = set3, aes(x = CI, y = y,color = "LowerBound"), linetype=2) +
    +     geom_line(data = set4, aes(x = CI, y = y,color = "UpperBound"), linetype=2) +
    +     scale_color_manual(name = "Label",values = colors)
    >   r <- ggplotly(p)
    Error in pm[[2]] : subscript out of bounds
    Calls: ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# gsDesign

<details>

* Version: 3.6.4
* GitHub: https://github.com/keaven/gsDesign
* Source code: https://github.com/cran/gsDesign
* Date/Publication: 2024-07-26 23:20:10 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "gsDesign")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gsDesign-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: summary.gsDesign
    > ### Title: Bound Summary and Z-transformations
    > ### Aliases: summary.gsDesign print.gsDesign gsBoundSummary xprint
    > ###   print.gsBoundSummary gsBValue gsDelta gsRR gsHR gsCPz
    > ### Keywords: design
    > 
    > ### ** Examples
    ...
                     ~RR at bound   0.6605   0.6605
                 P(Cross) if RR=1   0.0239   0.9761
               P(Cross) if RR=0.5   0.9000   0.1000
    > gsRR(z = xp$lower$bound, i = 1:3, xrr)
    [1] 1.0732500 0.8211496        NA
    > plot(xrr, plottype = "RR")
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gsDesign)
      > 
      > test_check("gsDesign")
      Linear spending function with none = [ FAIL 15 | WARN 9 | SKIP 98 | PASS 1799 ]
      
      ══ Skipped tests (98) ══════════════════════════════════════════════════════════
    ...
       20. │ └─grid:::grid.draw.grob(x$children[[i]], recording = FALSE)
       21. │   └─grDevices::recordGraphics(drawGrob(x), list(x = x), getNamespace("grid"))
       22. └─grid:::drawGrob(x)
       23.   ├─grid::drawDetails(x, recording = FALSE)
       24.   └─grid:::drawDetails.polyline(x, recording = FALSE)
       25.     └─grid:::grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow)
      
      [ FAIL 15 | WARN 9 | SKIP 98 | PASS 1799 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ConditionalPowerPlot.Rmd’
      ...
    
    > cp <- gsCP(x = update, i = 1, zi = -qnorm(p), theta = theta)
    
    > plot(cp, xval = hr, xlab = "Future HR", ylab = "Conditional Power/Error", 
    +     main = "Conditional probability of crossing future bound", 
    +     o .... [TRUNCATED] 
    
    ...
      ‘PoissonMixtureModel.Rmd’ using ‘UTF-8’... OK
      ‘SpendingFunctionOverview.Rmd’ using ‘UTF-8’... OK
      ‘SurvivalOverview.Rmd’ using ‘UTF-8’... OK
      ‘VaccineEfficacy.Rmd’ using ‘UTF-8’... OK
      ‘binomialSPRTExample.Rmd’ using ‘UTF-8’... OK
      ‘gsDesignPackageOverview.Rmd’ using ‘UTF-8’... failed
      ‘gsSurvBasicExamples.Rmd’ using ‘UTF-8’... failed
      ‘hGraph.Rmd’ using ‘UTF-8’... OK
      ‘nNormal.Rmd’ using ‘UTF-8’... OK
      ‘toInteger.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ConditionalErrorSpending.Rmd’ using rmarkdown
    ```

# gtExtras

<details>

* Version: 0.5.0
* GitHub: https://github.com/jthomasmock/gtExtras
* Source code: https://github.com/cran/gtExtras
* Date/Publication: 2023-09-15 22:32:06 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "gtExtras")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(gtExtras)
      Loading required package: gt
      
      Attaching package: 'gt'
      
      The following object is masked from 'package:testthat':
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-gt_plt_bar.R:44:3'): gt_plt_bar svg is created and has specific values ──
      `bar_neg_vals` (`actual`) not equal to c("49.19", "32.79", "16.40", "16.40", "32.79", "49.19") (`expected`).
      
      `actual`:   "49.19" "32.79" "16.40" "0.00"  "0.00"  "0.00" 
      `expected`: "49.19" "32.79" "16.40" "16.40" "32.79" "49.19"
      
      [ FAIL 1 | WARN 14 | SKIP 23 | PASS 115 ]
      Error: Test failures
      Execution halted
    ```

# HaploCatcher

<details>

* Version: 1.0.4
* GitHub: NA
* Source code: https://github.com/cran/HaploCatcher
* Date/Publication: 2023-04-21 23:32:39 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "HaploCatcher")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘An_Intro_to_HaploCatcher.Rmd’
      ...
    > set.seed(NULL)
    
    > results1 <- auto_locus(geno_mat = geno_mat, gene_file = gene_comp, 
    +     gene_name = "sst1_solid_stem", marker_info = marker_info, 
    +     chromosom .... [TRUNCATED] 
    Loading required package: lattice
    
      When sourcing ‘An_Intro_to_HaploCatcher.R’:
    Error: object is not a unit
    Execution halted
    
      ‘An_Intro_to_HaploCatcher.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘An_Intro_to_HaploCatcher.Rmd’ using rmarkdown
    
    Quitting from lines 242-253 [example_models_1] (An_Intro_to_HaploCatcher.Rmd)
    Error: processing vignette 'An_Intro_to_HaploCatcher.Rmd' failed with diagnostics:
    object is not a unit
    --- failed re-building ‘An_Intro_to_HaploCatcher.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘An_Intro_to_HaploCatcher.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# healthyR

<details>

* Version: 0.2.2
* GitHub: https://github.com/spsanderson/healthyR
* Source code: https://github.com/cran/healthyR
* Date/Publication: 2024-07-01 13:20:02 UTC
* Number of recursive dependencies: 146

Run `revdepcheck::cloud_details(, "healthyR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting-started.Rmd’
      ...
    +     .by = "month", .interactive = FALSE)
    Warning: Ignoring unknown labels:
    • `colour = "Legend"`
    
    > ts_alos_plt(.data = df_tbl, .date_col = Date, .value_col = Values, 
    +     .by = "month", .interactive = TRUE)
    
      When sourcing ‘getting-started.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘getting-started.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   2.5Mb
        doc    3.7Mb
    ```

# healthyR.ts

<details>

* Version: 0.3.0
* GitHub: https://github.com/spsanderson/healthyR.ts
* Source code: https://github.com/cran/healthyR.ts
* Date/Publication: 2023-11-15 06:00:05 UTC
* Number of recursive dependencies: 200

Run `revdepcheck::cloud_details(, "healthyR.ts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘healthyR.ts-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidy_fft
    > ### Title: Tidy Style FFT
    > ### Aliases: tidy_fft
    > 
    > ### ** Examples
    > 
    > suppressPackageStartupMessages(library(dplyr))
    ...
    > a <- tidy_fft(
    +   .data = data_tbl,
    +   .value_col = value,
    +   .date_col = date_col,
    +   .harmonics = 3,
    +   .frequency = 12
    + )
    Error in pm[[2]] : subscript out of bounds
    Calls: tidy_fft -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘using-tidy-fft.Rmd’
      ...
    $ value    <dbl> 112, 118, 132, 129, 121, 135, 148, 148, 136, 119, 104, 118, 1…
    
    > suppressPackageStartupMessages(library(timetk))
    
    > data_tbl %>% plot_time_series(.date_var = date_col, 
    +     .value = value)
    
      When sourcing ‘using-tidy-fft.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘getting-started.Rmd’ using ‘UTF-8’... OK
      ‘using-tidy-fft.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘getting-started.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        doc   5.2Mb
    ```

# heatmaply

<details>

* Version: 1.5.0
* GitHub: https://github.com/talgalili/heatmaply
* Source code: https://github.com/cran/heatmaply
* Date/Publication: 2023-10-06 20:50:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "heatmaply")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(heatmaply)
      Loading required package: plotly
      Loading required package: ggplot2
      
      Attaching package: 'plotly'
      
    ...
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. ├─heatmaply:::predict_colors(ggplotly(g), plot_method = "ggplot")
       7. ├─plotly::ggplotly(g)
       8. └─plotly:::ggplotly.ggplot(g)
       9.   └─plotly::gg2list(...)
      
      [ FAIL 58 | WARN 0 | SKIP 0 | PASS 193 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘heatmaply.Rmd’
      ...
    
    > library("heatmaply")
    
    > library("heatmaply")
    
    > heatmaply(mtcars)
    
      When sourcing ‘heatmaply.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘heatmaply.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘heatmaply.Rmd’ using rmarkdown
    
    Quitting from lines 109-111 [unnamed-chunk-5] (heatmaply.Rmd)
    Error: processing vignette 'heatmaply.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘heatmaply.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘heatmaply.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc   5.1Mb
    ```

# hermiter

<details>

* Version: 2.3.1
* GitHub: https://github.com/MikeJaredS/hermiter
* Source code: https://github.com/cran/hermiter
* Date/Publication: 2024-03-06 23:50:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "hermiter")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘hermiter.Rmd’
      ...
    > p2 <- ggplot(df_pdf_cdf) + geom_tile(aes(X, Y, fill = pdf_est)) + 
    +     scale_fill_continuous_sequential(palette = "Oslo", breaks = seq(0, 
    +       .... [TRUNCATED] 
    
    > p1 + ggtitle("Actual PDF") + theme(legend.title = element_blank()) + 
    +     p2 + ggtitle("Estimated PDF") + theme(legend.title = element_blank()) +  .... [TRUNCATED] 
    
      When sourcing ‘hermiter.R’:
    Error: object is not a unit
    Execution halted
    
      ‘hermiter.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘hermiter.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        R      2.6Mb
        doc    1.9Mb
        libs   2.0Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# heumilkr

<details>

* Version: 0.2.0
* GitHub: https://github.com/lschneiderbauer/heumilkr
* Source code: https://github.com/cran/heumilkr
* Date/Publication: 2024-04-01 13:50:06 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "heumilkr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘clarke_wright_performance.Rmd’
      ...
    +     "F", "tai"), group_desc = c("Augerat A, 1995", "Augerat B, 1995", 
    +     "Christofides and ..." ... [TRUNCATED] 
    
    > ggMarginal(ggplot(merge(result, description, by = "group"), 
    +     aes(x = n_site, y = clarke_wright_perf_xi, color = group_desc)) + 
    +     geom_poi .... [TRUNCATED] 
    
      When sourcing ‘clarke_wright_performance.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘clarke_wright_performance.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘clarke_wright_performance.Rmd’ using rmarkdown
    
    Quitting from lines 69-97 [perf_scale_based_graph] (clarke_wright_performance.Rmd)
    Error: processing vignette 'clarke_wright_performance.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘clarke_wright_performance.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘clarke_wright_performance.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# hilldiv

<details>

* Version: 1.5.1
* GitHub: https://github.com/anttonalberdi/hilldiv
* Source code: https://github.com/cran/hilldiv
* Date/Publication: 2019-10-01 14:40:02 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "hilldiv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hilldiv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: div_test_plot
    > ### Title: Diversity test plotting
    > ### Aliases: div_test_plot
    > ### Keywords: chart comparison hill numbers
    > 
    > ### ** Examples
    > 
    ...
     11. │           └─ggplot2:::`+.gg`(...)
     12. │             └─ggplot2:::add_ggplot(e1, e2, e2name)
     13. │               ├─ggplot2::ggplot_add(object, p, objectname)
     14. │               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     15. │                 └─ggplot2:::new_layer_names(object, names(plot$layers))
     16. └─base::.handleSimpleError(...)
     17.   └─purrr (local) h(simpleError(msg, call))
     18.     └─cli::cli_abort(...)
     19.       └─rlang::abort(...)
    Execution halted
    ```

# hmclearn

<details>

* Version: 0.0.5
* GitHub: NA
* Source code: https://github.com/cran/hmclearn
* Date/Publication: 2020-10-05 06:40:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "hmclearn")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hmclearn-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: hmclearn-plots
    > ### Title: Plotting for MCMC visualization and diagnostics provided by
    > ###   'bayesplot' package
    > ### Aliases: hmclearn-plots mcmc_intervals mcmc_intervals.hmclearn
    > ###   mcmc_areas mcmc_areas.hmclearn mcmc_hist mcmc_hist.hmclearn
    > ###   mcmc_hist_by_chain mcmc_hist_by_chain.hmclearn mcmc_dens
    > ###   mcmc_dens.hmclearn mcmc_scatter mcmc_scatter.hmclearn mcmc_hex
    ...
    +           param = list(y=y, X=X),
    +           parallel=FALSE, chains=2)
    > 
    > mcmc_trace(f, burnin=100)
    > mcmc_hist(f, burnin=100)
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    > mcmc_intervals(f, burnin=100)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: mcmc_intervals ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# HTLR

<details>

* Version: 0.4-4
* GitHub: https://github.com/longhaiSK/HTLR
* Source code: https://github.com/cran/HTLR
* Date/Publication: 2022-10-22 12:47:53 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "HTLR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘simu.Rmd’
      ...
    [1] "median"
    
    > post.t <- as.matrix(fit.t2, k = 2)
    
    > mcmc_intervals(post.t, pars = c("Intercept", "V1", 
    +     "V2", "V3", "V1000"))
    
      When sourcing ‘simu.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘simu.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘simu.Rmd’ using rmarkdown
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    1.1Mb
        libs   5.4Mb
    ```

# HVT

<details>

* Version: 24.5.2
* GitHub: https://github.com/Mu-Sigma/HVT
* Source code: https://github.com/cran/HVT
* Date/Publication: 2024-05-15 08:50:21 UTC
* Number of recursive dependencies: 200

Run `revdepcheck::cloud_details(, "HVT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘HVT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getTransitionProbability
    > ### Title: Creating Transition Probabilities list
    > ### Aliases: getTransitionProbability
    > ### Keywords: Transition_or_Prediction
    > 
    > ### ** Examples
    > 
    ...
      Ignoring unknown parameters: `check_overlap`
    Scale for x is already present.
    Adding another scale for x, which will replace the existing scale.
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Warning in geom_polygon(data = boundaryCoords2, aes(x = bp.x, y = bp.y,  :
      Ignoring unknown aesthetics: text
    Error in pm[[2]] : subscript out of bounds
    Calls: scoreHVT -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# hypsoLoop

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/hypsoLoop
* Date/Publication: 2022-02-08 09:00:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "hypsoLoop")` for more info

</details>

## Newly broken

*   checking whether package ‘hypsoLoop’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::set_theme’ by ‘sjPlot::set_theme’ when loading ‘hypsoLoop’
    See ‘/tmp/workdir/hypsoLoop/new/hypsoLoop.Rcheck/00install.out’ for details.
    ```

# ibdsim2

<details>

* Version: 2.1.1
* GitHub: https://github.com/magnusdv/ibdsim2
* Source code: https://github.com/cran/ibdsim2
* Date/Publication: 2024-09-08 07:40:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "ibdsim2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ibdsim2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotSegmentDistribution
    > ### Title: Scatter plots of IBD segment distributions
    > ### Aliases: plotSegmentDistribution
    > 
    > ### ** Examples
    > 
    > 
    ...
    Skip recomb  : -
    Total time used: 0.00288 secs
    > 
    > # By default, the IBD segments of the "leaves" are computed and plotted
    > plotSegmentDistribution(simPat, simMat, type = "ibd1", ids = 4:5,
    +                         labels = c("HSpat", "HSmat"))
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# ICtest

<details>

* Version: 0.3-5
* GitHub: NA
* Source code: https://github.com/cran/ICtest
* Date/Publication: 2022-05-18 07:30:29 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "ICtest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ICtest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplot.ictest
    > ### Title: Scatterplot Matrix for a ictest Object using ggplot2
    > ### Aliases: ggplot.ictest
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    ...
    > # The aesthetics variables
    > mapvar <- data.frame(iris[, 5])
    > colnames(mapvar) <- "species"
    > 
    > TestCov <- PCAasymp(X, k = 2)
    > ggplot(TestCov)
    > ggplot(TestCov, aes(color = species), mapvar = mapvar, which = "k")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc    2.5Mb
        libs   2.9Mb
    ```

# idiogramFISH

<details>

* Version: 2.0.13
* GitHub: NA
* Source code: https://github.com/cran/idiogramFISH
* Date/Publication: 2023-08-22 16:50:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "idiogramFISH")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R     1.5Mb
        doc   2.0Mb
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘index.Rmd’
      ...
    > if (requireNamespace("RCurl", quietly = TRUE)) {
    +     v <- sub("Version: ", "", readLines("../DESCRIPTION")[3])
    +     pkg <- "idiogramFISH"
    +     l .... [TRUNCATED] 
    Warning in file(con, "r") :
      cannot open file '../DESCRIPTION': No such file or directory
    
      When sourcing ‘index.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘AVignette.Rmd’ using ‘UTF-8’... OK
      ‘index.Rmd’ using ‘UTF-8’... failed
    ```

# IDMIR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/IDMIR
* Date/Publication: 2023-11-09 15:30:02 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "IDMIR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IDMIR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PlotSurvival
    > ### Title: PlotSurvival
    > ### Aliases: PlotSurvival
    > 
    > ### ** Examples
    > 
    > # Obtain the example data
    ...
    > GEP<-GetData_Mirna("GEP")
    > survival<-GetData_Mirna("survival")
    > MiRNAs<-c("hsa-miR-21-5p","hsa-miR-26a-5p","hsa-miR-369-5p","hsa-miR-1238-3p","hsa-miR-10b-5p")
    > # Run the function
    > SingleMiRNA_CRData<-SingleMiRNA_CRModel(GEP,
    + "hsa-miR-21-5p",survival,cutoff.point=NULL)
    > PlotSurvival(SingleMiRNA_CRData)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: PlotSurvival ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘IDMIR.Rmd’
      ...
    > survival <- GetData_Mirna("survival")
    
    > SingleMiRNA_CRData <- SingleMiRNA_CRModel(GEP, "hsa-miR-21-5p", 
    +     cutoff.point = NULL, survival)
    
    > PlotSurvival(SingleMiRNA_CRData)
    
      When sourcing ‘IDMIR.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘IDMIR.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘IDMIR.Rmd’ using rmarkdown
    
    Quitting from lines 120-130 [unnamed-chunk-7] (IDMIR.Rmd)
    Error: processing vignette 'IDMIR.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘IDMIR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘IDMIR.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# idopNetwork

<details>

* Version: 0.1.2
* GitHub: https://github.com/cxzdsa2332/idopNetwork
* Source code: https://github.com/cran/idopNetwork
* Date/Publication: 2023-04-18 06:50:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "idopNetwork")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘idopNetwork_vignette.Rmd’
      ...
    
    > qdODE_plot_base(ode.test)
    
    > ode.module = test_result$d1_module
    
    > qdODE_plot_all(ode.module)
    
      When sourcing ‘idopNetwork_vignette.R’:
    Error: object is not a unit
    Execution halted
    
      ‘idopNetwork_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘idopNetwork_vignette.Rmd’ using rmarkdown
    ```

# ihclust

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/ihclust
* Date/Publication: 2022-04-27 07:20:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "ihclust")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ihclust-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ihclust
    > ### Title: Iterative Hierarchical Clustering (IHC)
    > ### Aliases: ihclust
    > 
    > ### ** Examples
    > 
    > # This is an example not using the permutation approach
    ...
     12. │             └─ggplot2:::`+.gg`(p, do.call(geom_line, option))
     13. │               └─ggplot2:::add_ggplot(e1, e2, e2name)
     14. │                 ├─ggplot2::ggplot_add(object, p, objectname)
     15. │                 └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     16. │                   └─ggplot2:::new_layer_names(object, names(plot$layers))
     17. └─base::.handleSimpleError(...)
     18.   └─purrr (local) h(simpleError(msg, call))
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

# immunarch

<details>

* Version: 0.9.1
* GitHub: https://github.com/immunomind/immunarch
* Source code: https://github.com/cran/immunarch
* Date/Publication: 2024-03-18 19:10:06 UTC
* Number of recursive dependencies: 198

Run `revdepcheck::cloud_details(, "immunarch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘immunarch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geneUsageAnalysis
    > ### Title: Post-analysis of V-gene and J-gene statistics: PCA, clustering,
    > ###   etc.
    > ### Aliases: geneUsageAnalysis
    > 
    > ### ** Examples
    > 
    ...
     17. │                 └─ggplot2:::`+.gg`(p, do.call(geom_line, option))
     18. │                   └─ggplot2:::add_ggplot(e1, e2, e2name)
     19. │                     ├─ggplot2::ggplot_add(object, p, objectname)
     20. │                     └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     21. │                       └─ggplot2:::new_layer_names(object, names(plot$layers))
     22. └─base::.handleSimpleError(...)
     23.   └─purrr (local) h(simpleError(msg, call))
     24.     └─cli::cli_abort(...)
     25.       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   5.5Mb
        doc    1.6Mb
    ```

# incidental

<details>

* Version: 0.1
* GitHub: NA
* Source code: https://github.com/cran/incidental
* Date/Publication: 2020-09-16 09:50:03 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "incidental")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘incidental-tutorial.Rmd’
      ...
    
    > data_subset = do.call("rbind", model_df_list)
    
    > ggplot(data_subset, aes(x = Time, y = Reported)) + 
    +     geom_point(color = "coral2", shape = 3) + geom_line(aes(x = Time, 
    +     y = Ihat), color  .... [TRUNCATED] 
    
      When sourcing ‘incidental-tutorial.R’:
    Error: `x` must be a vector, not a <data.frame/incidence_spline_model_df> object.
    Execution halted
    
      ‘incidental-tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘incidental-tutorial.Rmd’ using rmarkdown
    ```

# infer

<details>

* Version: 1.0.7
* GitHub: https://github.com/tidymodels/infer
* Source code: https://github.com/cran/infer
* Date/Publication: 2024-03-25 21:50:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "infer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘infer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: shade_confidence_interval
    > ### Title: Add information about confidence interval
    > ### Aliases: shade_confidence_interval shade_ci
    > 
    > ### ** Examples
    > 
    > # find the point estimate---mean number of hours worked per week
    ...
    +                           type = "se")
    > 
    > 
    > # and plot it!
    > boot_dist %>%
    +   visualize() +
    +   shade_confidence_interval(ci)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: %>% ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
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
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       18. │     └─ggplot2:::add_ggplot(e1, e2, e2name)
       19. │       ├─ggplot2::ggplot_add(object, p, objectname)
       20. │       └─ggplot2:::ggplot_add.list(object, p, objectname)
       21. │         ├─ggplot2::ggplot_add(o, plot, object_name)
       22. │         └─ggplot2:::ggplot_add.Layer(o, plot, object_name)
       23. │           └─ggplot2:::new_layer_names(object, names(plot$layers))
       24. └─base::.handleSimpleError(...)
       25.   └─testthat (local) h(simpleError(msg, call))
       26.     └─rlang::abort(...)
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘anova.Rmd’
      ...
    +     generate(reps = 1000, type = "permute") %>% calculate( .... [TRUNCATED] 
    Dropping unused factor levels DK from the supplied explanatory variable
    'partyid'.
    
    > null_dist %>% visualize() + shade_p_value(observed_f_statistic, 
    +     direction = "greater")
    
    ...
      When sourcing ‘t_test.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘anova.Rmd’ using ‘UTF-8’... failed
      ‘chi_squared.Rmd’ using ‘UTF-8’... failed
      ‘infer.Rmd’ using ‘UTF-8’... failed
      ‘observed_stat_examples.Rmd’ using ‘UTF-8’... failed
      ‘paired.Rmd’ using ‘UTF-8’... failed
      ‘t_test.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘anova.Rmd’ using rmarkdown
    ```

# injurytools

<details>

* Version: 1.0.3
* GitHub: https://github.com/lzumeta/injurytools
* Source code: https://github.com/cran/injurytools
* Date/Publication: 2023-11-14 17:20:05 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "injurytools")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘model-injury-data-ii.Rmd’
      ...
                       n events median 0.95LCL 0.95UCL
    seasonb=2017/2018 23     16    265     152      NA
    seasonb=2018/2019 19     17    106      84     165
    
    > ggsurvplot(fit, data = injd_sub, palette = c("#E7B800", 
    +     "#2E9FDF")) + xlab("Time [calendar days]") + ylab(expression("Survival probability  ( ..." ... [TRUNCATED] 
    
      When sourcing ‘model-injury-data-ii.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘estimate-epi-measures.Rmd’ using ‘UTF-8’... OK
      ‘model-injury-data-i.Rmd’ using ‘UTF-8’... OK
      ‘model-injury-data-ii.Rmd’ using ‘UTF-8’... failed
      ‘prepare-injury-data.Rmd’ using ‘UTF-8’... OK
      ‘visualize-injury-data.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘estimate-epi-measures.Rmd’ using rmarkdown
    --- finished re-building ‘estimate-epi-measures.Rmd’
    
    --- re-building ‘model-injury-data-i.Rmd’ using rmarkdown
    ```

# inlabru

<details>

* Version: 2.11.1
* GitHub: https://github.com/inlabru-org/inlabru
* Source code: https://github.com/cran/inlabru
* Date/Publication: 2024-07-01 23:30:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "inlabru")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘inlabru-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: seals
    > ### Title: Seal pups
    > ### Aliases: seals seals_sp
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > if (require(ggplot2, quietly = TRUE)) {
    +   ggplot() +
    +     geom_fm(data = seals_sp$mesh) +
    +     gg(seals_sp$points)
    + }
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: +.gg ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (require(testthat, quietly = TRUE)) {
      +   test_check("inlabru")
      + }
      Loading required package: inlabru
      Loading required package: fmesher
      Starting 2 test processes
      [ FAIL 2 | WARN 0 | SKIP 49 | PASS 144 ]
    ...
        9. │         └─INLA:::expand.inla.stack.responses(responses)
       10. │           └─base::lapply(...)
       11. │             └─INLA (local) FUN(X[[i]], ...)
       12. │               └─dplyr::bind_rows(...)
       13. │                 └─vctrs::vec_rbind(!!!dots, .names_to = .id, .error_call = current_env())
       14. └─rlang::abort(message = message)
      
      [ FAIL 2 | WARN 0 | SKIP 49 | PASS 144 ]
      Error: Test failures
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘stars’
    ```

# insurancerating

<details>

* Version: 0.7.4
* GitHub: https://github.com/mharinga/insurancerating
* Source code: https://github.com/cran/insurancerating
* Date/Publication: 2024-05-20 11:30:03 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "insurancerating")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘insurancerating-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.univariate
    > ### Title: Automatically create a ggplot for objects obtained from
    > ###   univariate()
    > ### Aliases: autoplot.univariate
    > 
    > ### ** Examples
    > 
    ...
    > xzip <- univariate(MTPL, x = bm, severity = amount, nclaims = nclaims,
    + exposure = exposure, by = zip)
    > autoplot(xzip, show_plots = 1:2)
    Warning: Removed 16 rows containing missing values or values outside the scale range
    (`geom_point()`).
    Warning: Removed 5 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# inTextSummaryTable

<details>

* Version: 3.3.3
* GitHub: https://github.com/openanalytics/inTextSummaryTable
* Source code: https://github.com/cran/inTextSummaryTable
* Date/Publication: 2024-06-12 18:30:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "inTextSummaryTable")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(inTextSummaryTable)
      > 
      > test_check("inTextSummaryTable")
      [ FAIL 62 | WARN 0 | SKIP 0 | PASS 878 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        6. └─inTextSummaryTable::subjectProfileSummaryPlot(...)
        7.   └─ggplot2:::`+.gg`(gg, do.call(geom_line, argsGeomLine))
        8.     └─ggplot2:::add_ggplot(e1, e2, e2name)
        9.       ├─ggplot2::ggplot_add(object, p, objectname)
       10.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       11.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 62 | WARN 0 | SKIP 0 | PASS 878 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘inTextSummaryTable-aesthetics.Rmd’
      ...
    
    > summaryTable <- data.frame(visit = c(1, 2, 1, 2), 
    +     TRT = c("A", "A", "B", "B"), statMean = rnorm(4))
    
    > subjectProfileSummaryPlot(data = summaryTable, xVar = "visit", 
    +     colorVar = "TRT")
    
    ...
    Error: argument is of length zero
    Execution halted
    
      ‘inTextSummaryTable-advanced.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-aesthetics.Rmd’ using ‘UTF-8’... failed
      ‘inTextSummaryTable-createTables.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-exportTables.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-introduction.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-standardTables.Rmd’ using ‘UTF-8’... OK
      ‘inTextSummaryTable-visualization.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘inTextSummaryTable-advanced.Rmd’ using rmarkdown
    --- finished re-building ‘inTextSummaryTable-advanced.Rmd’
    
    --- re-building ‘inTextSummaryTable-aesthetics.Rmd’ using rmarkdown
    
    Quitting from lines 211-224 [aesthetics-defaultsVisualization] (inTextSummaryTable-aesthetics.Rmd)
    Error: processing vignette 'inTextSummaryTable-aesthetics.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘inTextSummaryTable-aesthetics.Rmd’
    ...
    Error: processing vignette 'inTextSummaryTable-visualization.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘inTextSummaryTable-visualization.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘inTextSummaryTable-aesthetics.Rmd’
      ‘inTextSummaryTable-visualization.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.3Mb
      sub-directories of 1Mb or more:
        doc   9.7Mb
    ```

# inventorize

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/inventorize
* Date/Publication: 2022-05-31 22:20:09 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "inventorize")` for more info

</details>

## Newly broken

*   checking whether package ‘inventorize’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/inventorize/new/inventorize.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘inventorize’ ...
** package ‘inventorize’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in pm[[2]] : subscript out of bounds
Error: unable to load R code in package ‘inventorize’
Execution halted
ERROR: lazy loading failed for package ‘inventorize’
* removing ‘/tmp/workdir/inventorize/new/inventorize.Rcheck/inventorize’


```
### CRAN

```
* installing *source* package ‘inventorize’ ...
** package ‘inventorize’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Warning in qgamma(service_level, alpha, beta) : NaNs produced
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (inventorize)


```
# IPV

<details>

* Version: 1.0.0
* GitHub: https://github.com/NilsPetras/IPV
* Source code: https://github.com/cran/IPV
* Date/Publication: 2022-09-30 15:00:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "IPV")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IPV-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: nested_chart
    > ### Title: Nested Chart
    > ### Aliases: nested_chart
    > 
    > ### ** Examples
    > 
    > # as simple as that
    ...
      6.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
      7.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      8.             └─vctrs::vec_as_names(names, repair = "check_unique")
      9.               └─vctrs (local) `<fn>`()
     10.                 └─vctrs:::validate_unique(names = names, arg = arg, call = call)
     11.                   └─vctrs:::stop_names_cannot_be_empty(names, call = call)
     12.                     └─vctrs:::stop_names(...)
     13.                       └─vctrs:::stop_vctrs(...)
     14.                         └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ipv-vignette.Rmd’
      ...
    Facet circle radius set to 0.211 based on the data.
    cor_spacing set to 0.193 based on the data.
    Relative scaling set to 3.78 based on the data.
    Axis tick set to 0.1 based on the data.
    dist_construct_label set to 0.5 based on the data.
    
      When sourcing ‘ipv-vignette.R’:
    Error: Names can't be empty.
    ✖ Empty name found at location 4.
    Execution halted
    
      ‘ipv-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ipv-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
    ```

# IRon

<details>

* Version: 0.1.4
* GitHub: https://github.com/nunompmoniz/IRon
* Source code: https://github.com/cran/IRon
* Date/Publication: 2023-01-20 07:20:06 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "IRon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘IRon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sera
    > ### Title: Squared Error-Relevance Area (SERA)
    > ### Aliases: sera
    > 
    > ### ** Examples
    > 
    > library(IRon)
    ...
     16. │             └─self$stat$setup_params(data, self$stat_params)
     17. │               └─ggplot2 (local) setup_params(...)
     18. │                 └─base::match.fun(method)
     19. │                   └─base::get(as.character(FUN), mode = "function", envir = envir)
     20. └─base::.handleSimpleError(...)
     21.   └─rlang (local) h(simpleError(msg, call))
     22.     └─handlers[[1L]](cnd)
     23.       └─cli::cli_abort(...)
     24.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

# irt

<details>

* Version: 0.2.9
* GitHub: https://github.com/egonulates/irt
* Source code: https://github.com/cran/irt
* Date/Publication: 2024-02-20 20:40:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "irt")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘irt-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_distractor_icc
    > ### Title: Plot Empirical Item or Test characteristic curve
    > ### Aliases: plot_distractor_icc
    > 
    > ### ** Examples
    > 
    > n_item <- 10 # sample(8:12, 1)
    ...
    > raw_resp <- matrix(sample(LETTERS[1:4], n_item * n_theta, replace = TRUE),
    +                    nrow = n_theta, ncol = n_item,
    +                    dimnames = list(paste0("Examinee-", 1:n_theta),
    +                                    paste0("Item_", 1:n_item)))
    > key <- sample(LETTERS[1:4], n_item, replace = TRUE)
    > plot_distractor_icc(raw_resp, 3, key)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: plot_distractor_icc ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 20.3Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        libs  18.0Mb
    ```

# isoorbi

<details>

* Version: 1.3.1
* GitHub: https://github.com/isoverse/isoorbi
* Source code: https://github.com/cran/isoorbi
* Date/Publication: 2024-08-27 05:10:03 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "isoorbi")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘dual_inlet.Rmd’
      ...
    |20230518_05_USGS32_vs_USGS34 |         16|     7|reference   |changeover |      NA|         10695|       11020|         65.019|       66.994|
    |20230518_05_USGS32_vs_USGS34 |         17|     7|reference   |data       |      NA|         11025|       12335|         67.025|       74.985|
    
    > orbi_plot_raw_data(df_w_blocks, isotopocules = "15N", 
    +     y = ions.incremental)
    
      When sourcing ‘dual_inlet.R’:
    ...
    
      When sourcing ‘shot_noise.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘dual_inlet.Rmd’ using ‘UTF-8’... failed
      ‘flow_injection.Rmd’ using ‘UTF-8’... OK
      ‘isoxl_demo.Rmd’ using ‘UTF-8’... OK
      ‘quick_start.Rmd’ using ‘UTF-8’... OK
      ‘shot_noise.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘dual_inlet.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc       2.0Mb
        extdata   3.3Mb
    ```

# ivDiag

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/ivDiag
* Date/Publication: 2023-09-17 06:00:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "ivDiag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ivDiag-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ltz
    > ### Title: Local-to-Zero Test
    > ### Aliases: ltz
    > 
    > ### ** Examples
    > 
    > data(ivDiag)
    > controls <- c('altitudine', 'escursione', 'costal', 'nearsea', 'population', 
    +     'pop2', 'gini_land', 'gini_income')
    > ltz_out <- ltz(data = gsz, Y = "totassoc_p", D = "libero_comune_allnord", 
    +     Z = "bishopcity", controls = controls, weights = "population", 
    +     prior = c(0.178, 0.137))
    > plot_ltz(ltz_out)    
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot_ltz ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# ivreg

<details>

* Version: 0.6-3
* GitHub: https://github.com/zeileis/ivreg
* Source code: https://github.com/cran/ivreg
* Date/Publication: 2024-04-20 15:22:35 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "ivreg")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ivreg.Rmd’
      ...
    | F                    | 204.932   |         |
    +----------------------+-----------+---------+
    | RMSE                 | 0.37      | 0.40    |
    +----------------------+-----------+---------+ 
    
    > modelplot(m_list, coef_omit = "Intercept|experience")
    
      When sourcing ‘ivreg.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Diagnostics-for-2SLS-Regression.Rmd’ using ‘UTF-8’... OK
      ‘ivreg.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Diagnostics-for-2SLS-Regression.Rmd’ using rmarkdown
    ```

# jarbes

<details>

* Version: 2.2.1
* GitHub: NA
* Source code: https://github.com/cran/jarbes
* Date/Publication: 2024-06-07 09:20:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "jarbes")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘hmr.Rmd’
      ...
    Warning: Contour data has duplicated x, y coordinates.
    ℹ 15940 duplicated rows have been dropped.
    Warning: Removed 161 rows containing non-finite outside the scale range
    (`stat_contour()`).
    Warning: Removed 92 rows containing missing values or values outside the scale range
    (`geom_point()`).
    
      When sourcing ‘hmr.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘bmeta.Rmd’ using ‘UTF-8’... OK
      ‘hmr.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bmeta.Rmd’ using rmarkdown
    ```

# karel

<details>

* Version: 0.1.1
* GitHub: https://github.com/mpru/karel
* Source code: https://github.com/cran/karel
* Date/Publication: 2022-03-26 21:50:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "karel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘karel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: acciones
    > ### Title: Acciones que Karel puede realizar
    > ### Aliases: acciones avanzar girar_izquierda poner_coso juntar_coso
    > ###   girar_derecha darse_vuelta
    > 
    > ### ** Examples
    > 
    ...
     1. └─karel::ejecutar_acciones()
     2.   ├─base::suppressWarnings(...)
     3.   │ └─base::withCallingHandlers(...)
     4.   ├─gganimate::animate(...)
     5.   └─gganimate:::animate.gganim(...)
     6.     └─args$renderer(frames_vars$frame_source, args$fps)
     7.       └─gganimate:::png_dim(frames[1])
     8.         └─cli::cli_abort("Provided file ({file}) does not exist")
     9.           └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(karel)
      > 
      > test_check("karel")
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 78 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
        5.     ├─gganimate::animate(...)
        6.     └─gganimate:::animate.gganim(...)
        7.       └─args$renderer(frames_vars$frame_source, args$fps)
        8.         └─gganimate:::png_dim(frames[1])
        9.           └─cli::cli_abort("Provided file ({file}) does not exist")
       10.             └─rlang::abort(...)
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 78 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gifski’
      All declared Imports should be used.
    ```

# kDGLM

<details>

* Version: 1.2.0
* GitHub: https://github.com/silvaneojunior/kDGLM
* Source code: https://github.com/cran/kDGLM
* Date/Publication: 2024-05-25 09:50:03 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "kDGLM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘kDGLM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: forecast.fitted_dlm
    > ### Title: Auxiliary function for forecasting
    > ### Aliases: forecast.fitted_dlm
    > 
    > ### ** Examples
    > 
    > 
    ...
    > forecast(fitted.data, 24,
    +   chickenPox = list(Total = rep(175, 24)), # Optional
    +   Vaccine.1.Covariate = rep(TRUE, 24),
    +   Vaccine.2.Covariate = rep(TRUE, 24)
    + )
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Error in pm[[2]] : subscript out of bounds
    Calls: forecast ... lapply -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘fitting.Rmd’
      ...
    > outcome <- Multinom(p = c("p.1", "p.2"), data = chickenPox[, 
    +     c(2, 3, 5)])
    
    > fitted.model <- fit_model(structure * 2, chickenPox = outcome)
    
    > forecast(fitted.model, t = 24, plot = "base")
    
      When sourcing ‘fitting.R’:
    Error: Error: Missing extra argument: Vaccine.1.Covariate
    Execution halted
    
      ‘example1.Rmd’ using ‘UTF-8’... OK
      ‘fitting.Rmd’ using ‘UTF-8’... failed
      ‘intro.Rmd’ using ‘UTF-8’... OK
      ‘outcomes.Rmd’ using ‘UTF-8’... OK
      ‘structures.Rmd’ using ‘UTF-8’... OK
    ```

# KMEANS.KNN

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/KMEANS.KNN
* Date/Publication: 2024-05-17 09:20:12 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "KMEANS.KNN")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘KMEANS.KNN-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: KMEANS_FUNCTION
    > ### Title: KMEANS_FUNCTION
    > ### Aliases: KMEANS_FUNCTION
    > 
    > ### ** Examples
    > 
    > data(iris)
    ...
     12. │             └─ggplot2:::`+.gg`(...)
     13. │               └─ggplot2:::add_ggplot(e1, e2, e2name)
     14. │                 ├─ggplot2::ggplot_add(object, p, objectname)
     15. │                 └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     16. │                   └─ggplot2:::new_layer_names(object, names(plot$layers))
     17. └─base::.handleSimpleError(...)
     18.   └─purrr (local) h(simpleError(msg, call))
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
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
       15. │                 └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       16. │                   └─ggplot2:::new_layer_names(object, names(plot$layers))
       17. └─base::.handleSimpleError(...)
       18.   └─purrr (local) h(simpleError(msg, call))
       19.     └─cli::cli_abort(...)
       20.       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 11 ]
      Error: Test failures
      Execution halted
    ```

# latentcor

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/latentcor
* Date/Publication: 2022-09-05 20:50:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "latentcor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘latentcor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: latentcor
    > ### Title: Estimate latent correlation for mixed types.
    > ### Aliases: latentcor
    > 
    > ### ** Examples
    > 
    > # Example 1 - truncated data type, same type for all variables
    ...
    > R_approx = latentcor(X = X, types = "tru", method = "approx")$R
    > proc.time() - start_time
       user  system elapsed 
      0.027   0.000   0.027 
    > # Heatmap for latent correlation matrix.
    > Heatmap_R_approx = latentcor(X = X, types = "tru", method = "approx",
    +                              showplot = TRUE)$plotR
    Error in pm[[2]] : subscript out of bounds
    Calls: latentcor ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# lcars

<details>

* Version: 0.3.8
* GitHub: https://github.com/leonawicz/lcars
* Source code: https://github.com/cran/lcars
* Date/Publication: 2023-09-10 04:10:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "lcars")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lcars-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lcars_border
    > ### Title: LCARS border plot
    > ### Aliases: lcars_border
    > 
    > ### ** Examples
    > 
    > lcars_border()
    ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Error in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid font type
    Calls: lcars_border ... drawDetails -> drawDetails.text -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘lcars.Rmd’
      ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family '0.5' not found in PostScript font database
    
      When sourcing ‘lcars.R’:
    Error: invalid font type
    Execution halted
    
      ‘lcars.Rmd’ using ‘UTF-8’... failed
    ```

# lemon

<details>

* Version: 0.4.9
* GitHub: https://github.com/stefanedwards/lemon
* Source code: https://github.com/cran/lemon
* Date/Publication: 2024-02-08 08:00:08 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "lemon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lemon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: annotate_y_axis
    > ### Title: Annotations on the axis
    > ### Aliases: annotate_y_axis annotate_x_axis
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > 
    > p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()
    > 
    > l <- p + annotate_y_axis('mark at', y=200, tick=TRUE)
    > l
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... polylineGrob -> is.unit -> unit.c -> identicalUnits
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(lemon)
      > 
      > 
      > if (TRUE) {
      +   test_check("lemon")
      + } #else {
    ...
       17.                           ├─grid::unit.c(unit(1, "npc"), unit(1, "npc") - tick.length)
       18.                           └─grid:::Ops.unit(unit(1, "npc"), tick.length)
       19.                             └─grid:::as.unit(e2)
      
      [ FAIL 1 | WARN 0 | SKIP 3 | PASS 138 ]
      Deleting unused snapshots:
      • facet/facet-rep-wrap-spacing.svg
      • facet_aux/facet-rep-wrap.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘capped-axes.Rmd’
      ...
    > p + coord_capped_cart(bottom = "right")
    
    > p + coord_capped_cart(bottom = "right", left = "none")
    
    > ggplot(dat1, aes(gp, y)) + geom_point(position = position_jitter(width = 0.2, 
    +     height = 0)) + coord_capped_cart(left = "none", bottom = bracke .... [TRUNCATED] 
    
    ...
      When sourcing ‘legends.R’:
    Error: Could not find panel named `panel-1-5`.
    Execution halted
    
      ‘capped-axes.Rmd’ using ‘UTF-8’... failed
      ‘facet-rep-labels.Rmd’ using ‘UTF-8’... failed
      ‘geoms.Rmd’ using ‘UTF-8’... OK
      ‘gtable_show_lemonade.Rmd’ using ‘UTF-8’... OK
      ‘legends.Rmd’ using ‘UTF-8’... failed
      ‘lemon_print.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘capped-axes.Rmd’ using rmarkdown
    ```

# lfproQC

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/lfproQC
* Date/Publication: 2024-09-06 13:00:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "lfproQC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lfproQC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Boxplot_data
    > ### Title: Creating Boxplot for a dataset
    > ### Aliases: Boxplot_data
    > 
    > ### ** Examples
    > 
    > Boxplot_data(yeast_data)
    Using Majority protein IDs as id variables
    Warning: Removed 269 rows containing non-finite outside the scale range
    (`stat_boxplot()`).
    Error in pm[[2]] : subscript out of bounds
    Calls: Boxplot_data -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘user_guide.Rmd’
      ...
    > yeast$`Best combinations`
      PCV_best_combination PEV_best_combination PMAD_best_combination
    1              knn_rlr              lls_vsn               lls_rlr
    
    > Boxplot_data(yeast$knn_rlr_data)
    Using Majority protein IDs as id variables
    
      When sourcing ‘user_guide.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘user_guide.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user_guide.Rmd’ using rmarkdown
    
    Quitting from lines 53-54 [unnamed-chunk-8] (user_guide.Rmd)
    Error: processing vignette 'user_guide.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘user_guide.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user_guide.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

# lgpr

<details>

* Version: 1.2.4
* GitHub: https://github.com/jtimonen/lgpr
* Source code: https://github.com/cran/lgpr
* Date/Publication: 2023-09-24 06:50:02 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "lgpr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # Short tests
      > #  - focus on testing that everything runs
      > #  - should take less than minute
      > library(testthat)
      > library(lgpr)
      Attached lgpr 1.2.4, using rstan 2.32.6. Type ?lgpr to get started.
      > 
    ...
       3.     └─bayesplot::mcmc_areas(sf, regex_pars = regex_pars, ...)
       4.       └─ggplot2:::`+.gg`(...)
       5.         └─ggplot2:::add_ggplot(e1, e2, e2name)
       6.           ├─ggplot2::ggplot_add(object, p, objectname)
       7.           └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       8.             └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 3 | WARN 1 | SKIP 0 | PASS 434 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 156.9Mb
      sub-directories of 1Mb or more:
        R       1.5Mb
        libs  155.0Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# LightLogR

<details>

* Version: 0.3.8
* GitHub: https://github.com/tscnlab/LightLogR
* Source code: https://github.com/cran/LightLogR
* Date/Publication: 2024-07-04 17:00:02 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "LightLogR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘LightLogR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: aggregate_Date
    > ### Title: Aggregate dates to a single day
    > ### Aliases: aggregate_Date
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > #gg_days without aggregation
    > sample.data.environment %>%
    +  gg_days()
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: %>% ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# LMoFit

<details>

* Version: 0.1.7
* GitHub: NA
* Source code: https://github.com/cran/LMoFit
* Date/Publication: 2024-05-14 07:33:23 UTC
* Number of recursive dependencies: 62

Run `revdepcheck::cloud_details(, "LMoFit")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘LMoFit.Rmd’
      ...
    
    > lspace_BrIII
    
      When sourcing ‘LMoFit.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `compute_geom_2()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, 
        c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, 
    Execution halted
    
      ‘LMoFit.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘LMoFit.Rmd’ using rmarkdown
    
    Quitting from lines 236-237 [unnamed-chunk-15] (LMoFit.Rmd)
    Error: processing vignette 'LMoFit.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `compute_geom_2()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, 
    ...
        NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 11, list("white", NA, NULL, NULL, TRUE), list(), 5.5, NULL, NULL, list("grey92", NULL, NULL, NULL, FALSE, TRUE), list(), list(), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 
    0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, c(5.5, 5.5, 5.5, 5.5), list("white", "black", 2, NULL, TRUE), NULL, NULL, "inherit", "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, 
        NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75, list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5)))
    --- failed re-building ‘LMoFit.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘LMoFit.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

# lnmixsurv

<details>

* Version: 3.1.6
* GitHub: NA
* Source code: https://github.com/cran/lnmixsurv
* Date/Publication: 2024-09-03 15:20:08 UTC
* Number of recursive dependencies: 196

Run `revdepcheck::cloud_details(, "lnmixsurv")` for more info

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
      <subscriptOutOfBoundsError/error/condition>
      Error in `pm[[2]]`: subscript out of bounds
      Backtrace:
          ▆
       1. └─testthat::expect_snapshot(plot(mod1)) at test-survival_ln_mixture_em-methods.R:25:3
       2.   └─rlang::cnd_signal(state$error)
      
      [ FAIL 1 | WARN 0 | SKIP 7 | PASS 50 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘expectation_maximization.Rmd’
      ...
    +     x, data = data, iter = 200, starting_seed = 20, number_em_search = 0)
    
    > gg <- plot_fit_on_data(model_em, data)$ggplot
    
    > plot(model_em)
    Loading required namespace: plotly
    
      When sourcing ‘expectation_maximization.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘compare.Rmd’ using ‘UTF-8’... OK
      ‘expectation_maximization.Rmd’ using ‘UTF-8’... failed
      ‘intercept_only.Rmd’ using ‘UTF-8’... OK
      ‘lnmixsurv.Rmd’ using ‘UTF-8’... OK
      ‘parallel_computation.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘compare.Rmd’ using rmarkdown
    
    warning: solve(): system is singular; attempting approx solution
    
    warning: solve(): system is singular; attempting approx solution
    
    warning: solve(): system is singular; attempting approx solution
    
    warning: solve(): system is singular; attempting approx solution
    ...
    
    warning: solve(): system is singular; attempting approx solution
    
    warning: solve(): system is singular; attempting approx solution
    
    warning: solve(): system is singular; attempting approx solution
    
    warning: solve(): system is singular; attempting approx solution
    
    warning: solve(): system is singular; attempting approx solution
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.0Mb
      sub-directories of 1Mb or more:
        doc    4.0Mb
        libs   5.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘purrr’ ‘readr’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rstanarm’
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# LocalControl

<details>

* Version: 1.1.4
* GitHub: https://github.com/OHDSI/LocalControl
* Source code: https://github.com/cran/LocalControl
* Date/Publication: 2024-09-04 22:30:18 UTC
* Number of recursive dependencies: 41

Run `revdepcheck::cloud_details(, "LocalControl")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘LocalControl-jss-2020.Rnw’
      ...
    Warning: A numeric `legend.position` argument in `theme()` was deprecated in
    ggplot2 3.5.0.
    ℹ Please use the `legend.position.inside` argument of `theme()`
      instead.
    
    > grid.arrange(plotz$rad_1, plotz$rad_11, ncol = 1)
    
      When sourcing 'LocalControl-jss-2020.R':
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘LocalControl-jss-2020.Rnw’ using ‘UTF-8’... failed
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc    2.0Mb
        libs   2.7Mb
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘LocalControl-jss-2020.Rnw’ using Sweave
    Loading required package: data.table
    Loading required package: colorspace
    Loading required package: RColorBrewer
    Loading required package: gridExtra
    Loading required package: ggplot2
    Loading required package: rpart
    Loading required package: rpart.plot
    Loading required package: LocalControl
    ...
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    
    --- failed re-building 'LocalControl-jss-2020.Rnw'
    
    SUMMARY: processing the following file failed:
      'LocalControl-jss-2020.Rnw'
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# LocalCop

<details>

* Version: 0.0.1
* GitHub: https://github.com/mlysy/LocalCop
* Source code: https://github.com/cran/LocalCop
* Date/Publication: 2024-03-21 14:50:06 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "LocalCop")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘LocalCop-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘LocalCop-vignette.Rmd’
      ...
    > tibble(x = x0, True = BiCopEta2Tau(family, eta = eta_fun(x0)), 
    +     Fitted = BiCopEta2Tau(fitseq$eta, family = family)) %>% pivot_longer(True:Fitt .... [TRUNCATED] 
    Warning: Removed 51 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Warning: Removed 51 rows containing missing values or values outside the scale range
    (`geom_point()`).
    
      When sourcing ‘LocalCop-vignette.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘LocalCop-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 55.7Mb
      sub-directories of 1Mb or more:
        libs  55.3Mb
    ```

# LongDat

<details>

* Version: 1.1.2
* GitHub: https://github.com/CCY-dev/LongDat
* Source code: https://github.com/cran/LongDat
* Date/Publication: 2023-07-17 05:40:02 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "LongDat")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘LongDat_cont_tutorial.Rmd’
      ...
    </table></div>
    > test_plot <- cuneiform_plot(result_table = test_cont[[1]], 
    +     title_size = 15)
    [1] "Finished plotting successfully!"
    
    > test_plot
    
    ...
    [1] "Finished plotting successfully!"
    
    > test_plot
    
      When sourcing ‘LongDat_disc_tutorial.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘LongDat_cont_tutorial.Rmd’ using ‘UTF-8’... failed
      ‘LongDat_disc_tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘LongDat_cont_tutorial.Rmd’ using rmarkdown
    Warning in eng_r(options) :
      Failed to tidy R code in chunk 'unnamed-chunk-3'. Reason:
    Error : The formatR package is required by the chunk option tidy = TRUE but not installed; tidy = TRUE will be ignored.
    
    Warning in eng_r(options) :
      Failed to tidy R code in chunk 'unnamed-chunk-4'. Reason:
    Error : The formatR package is required by the chunk option tidy = TRUE but not installed; tidy = TRUE will be ignored.
    
    ...
    Quitting from lines 181-182 [unnamed-chunk-11] (LongDat_disc_tutorial.Rmd)
    Error: processing vignette 'LongDat_disc_tutorial.Rmd' failed with diagnostics:
    object is not coercible to a unit
    --- failed re-building ‘LongDat_disc_tutorial.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘LongDat_cont_tutorial.Rmd’ ‘LongDat_disc_tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# longreadvqs

<details>

* Version: 0.1.3
* GitHub: https://github.com/NakarinP/longreadvqs
* Source code: https://github.com/cran/longreadvqs
* Date/Publication: 2024-08-26 19:30:05 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "longreadvqs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘longreadvqs-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vqscompare
    > ### Title: Comparing viral quasispecies profile and operational taxonomic
    > ###   unit (OTU) classified by k-means clustering between samples
    > ### Aliases: vqscompare
    > 
    > ### ** Examples
    > 
    ...
     13. │           └─ggplot2:::`+.gg`(...)
     14. │             └─ggplot2:::add_ggplot(e1, e2, e2name)
     15. │               ├─ggplot2::ggplot_add(object, p, objectname)
     16. │               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     17. │                 └─ggplot2:::new_layer_names(object, names(plot$layers))
     18. └─base::.handleSimpleError(...)
     19.   └─purrr (local) h(simpleError(msg, call))
     20.     └─cli::cli_abort(...)
     21.       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘longreadvqs-vignette.Rmd’
      ...
    
    > comp <- vqscompare(samplelist = list(s1, s2, s3, s4_fix), 
    +     lab_name = "Sample", kmeans.n = 10)
    
      When sourcing ‘longreadvqs-vignette.R’:
    Error: ℹ In index: 1.
    ℹ With name: Dim.2.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    Execution halted
    
      ‘longreadvqs-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘longreadvqs-vignette.Rmd’ using rmarkdown
    ```

# lpdensity

<details>

* Version: 2.4
* GitHub: NA
* Source code: https://github.com/cran/lpdensity
* Date/Publication: 2023-01-21 23:50:02 UTC
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "lpdensity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lpdensity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lpdensity
    > ### Title: Local Polynomial Density Estimation and Inference
    > ### Aliases: lpdensity
    > 
    > ### ** Examples
    > 
    > # Generate a random sample
    ...
    18      1.2878    0.6298     457    0.1749    0.0097     0.1222 ,  0.2179    
    19      1.5768    0.6298     299    0.1221    0.0086     0.0863 ,  0.1719    
    =============================================================================
    > 
    > # Plot the estimates and confidence intervals
    > plot(est1, legendTitle="My Plot", legendGroups=c("X"))
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# lspartition

<details>

* Version: 0.4
* GitHub: NA
* Source code: https://github.com/cran/lspartition
* Date/Publication: 2019-08-08 22:40:06 UTC
* Number of recursive dependencies: 34

Run `revdepcheck::cloud_details(, "lspartition")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lspartition-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lsprobust.plot
    > ### Title: Graphic Presentation of Results for 'lspartition' Package
    > ### Aliases: lsprobust.plot
    > 
    > ### ** Examples
    > 
    > x   <- runif(500)
    > y   <- sin(4*x)+rnorm(500)
    > est <- lsprobust(y, x)
    > lsprobust.plot(est)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# LSTbook

<details>

* Version: 0.5.0
* GitHub: https://github.com/dtkaplan/LSTbook
* Source code: https://github.com/cran/LSTbook
* Date/Publication: 2024-02-23 19:20:15 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "LSTbook")` for more info

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
      
      [ FAIL 1 | WARN 1 | SKIP 4 | PASS 73 ]
      Deleting unused snapshots:
      • model_plot/four-facets.png
      • pointplot/1-var-plot.png
      • pointplot/bird-logistic-plot.png
      • pointplot/color-and-facet-na.png
      • pointplot/logistic-fun-plot.png
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘DAGs.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘DAGs.Rmd’
      ...
    3 -1.35  -11.9  
    4  0.429   0.171
    5  0.622   5.85 
    6  2.35   16.5  
    
    > Wrong_way <- datasim_make(x = rnorm(n, mean = 0, sd = 2))
    
    ...
    
    > model_plot(height_model)
    
      When sourcing ‘modeling.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘DAGs.Rmd’ using ‘UTF-8’... failed
      ‘LSTbook.Rmd’ using ‘UTF-8’... OK
      ‘modeling.Rmd’ using ‘UTF-8’... failed
    ```

*   checking loading without being on the library search path ... WARNING
    ```
    Error: package or namespace load failed for ‘LSTbook’:
     .onLoad failed in loadNamespace() for 'LSTbook', details:
      call: loadNamespace(name)
      error: there is no package called ‘mosaicData’
    Execution halted
    
    It looks like this package has a loading problem when not on .libPaths:
    see the messages for details.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
    ```

# manydata

<details>

* Version: 0.9.3
* GitHub: https://github.com/globalgov/manydata
* Source code: https://github.com/cran/manydata
* Date/Publication: 2024-05-06 19:00:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "manydata")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(manydata)
      manydata 0.9.3
      Please see manydata.ch for more information.
      Type 'citation("manydata")' for citing this R package in publications.
      > 
      > test_check("manydata")
    ...
      ── Failure ('test_compare.R:8:3'): plot for compare_categories returns the correct output format ──
      Names of `db` ('data', 'layers', 'scales', 'guides', 'mapping', 'theme', 'coordinates', 'facet', 'plot_env', 'layout', 'labels') don't match 'data', 'layers', 'scales', 'guides', 'mapping', 'theme', 'coordinates', 'facet', 'plot_env', 'layout'
      ── Failure ('test_compare.R:74:3'): compare_missing() and plot_missing() returns the correct output format ──
      `pl` has length 11, not length 10.
      ── Failure ('test_compare.R:76:3'): compare_missing() and plot_missing() returns the correct output format ──
      Names of `pl` ('data', 'layers', 'scales', 'guides', 'mapping', 'theme', 'coordinates', 'facet', 'plot_env', 'layout', 'labels') don't match 'data', 'layers', 'scales', 'guides', 'mapping', 'theme', 'coordinates', 'facet', 'plot_env', 'layout'
      
      [ FAIL 4 | WARN 0 | SKIP 3 | PASS 121 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

# manymome

<details>

* Version: 0.2.2
* GitHub: https://github.com/sfcheung/manymome
* Source code: https://github.com/cran/manymome
* Date/Publication: 2024-06-05 23:30:03 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "manymome")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘manymome-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_effect_vs_w
    > ### Title: Plot an Effect Against a Moderator
    > ### Aliases: plot_effect_vs_w
    > 
    > ### ** Examples
    > 
    > 
    ...
    +                                 y = "y",
    +                                 m = "m",
    +                                 fit = fit_lm,
    +                                 sd_from_mean = seq(-2, 2, length.out = 10),
    +                                 boot_ci = TRUE,
    +                                 boot_out = boot_out_lm)
    > p <- plot_effect_vs_w(out_lm)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot_effect_vs_w ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# mapbayr

<details>

* Version: 0.10.0
* GitHub: https://github.com/FelicienLL/mapbayr
* Source code: https://github.com/cran/mapbayr
* Date/Publication: 2023-07-17 08:20:02 UTC
* Number of recursive dependencies: 68

Run `revdepcheck::cloud_details(, "mapbayr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mapbayr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mapbayr_plot
    > ### Title: Make mapbayr plot
    > ### Aliases: mapbayr_plot
    > 
    > ### ** Examples
    > 
    > aug <- data.frame(
    ...
    > obs <- data.frame(
    +   ID = 1, time = c(6, 20), evid = 0,
    +   mdv = c(0,1), DV = c(0.5, 5), cmt = 2
    +   )
    > 
    > mapbayr_plot(aug, obs)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# MBNMAdose

<details>

* Version: 0.4.3
* GitHub: NA
* Source code: https://github.com/cran/MBNMAdose
* Date/Publication: 2024-04-18 12:42:47 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "MBNMAdose")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘outputs-4.Rmd’
      ...
    
    > plot(trip.emax)
    
      When sourcing ‘outputs-4.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ...
    Execution halted
    
      ‘consistencychecking-3.Rmd’ using ‘UTF-8’... OK
      ‘dataexploration-1.Rmd’ using ‘UTF-8’... OK
      ‘mbnmadose-overview.Rmd’ using ‘UTF-8’... OK
      ‘metaregression-6.Rmd’ using ‘UTF-8’... OK
      ‘nma_in_mbnmadose.Rmd’ using ‘UTF-8’... OK
      ‘outputs-4.Rmd’ using ‘UTF-8’... failed
      ‘predictions-5.Rmd’ using ‘UTF-8’... OK
      ‘runmbnmadose-2.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘consistencychecking-3.Rmd’ using rmarkdown
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked Latin-1 strings
    ```

# MBNMAtime

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/MBNMAtime
* Date/Publication: 2023-10-14 15:20:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "MBNMAtime")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘consistencychecking-3.Rmd’ using rmarkdown
    
    Quitting from lines 141-146 [unnamed-chunk-8] (consistencychecking-3.Rmd)
    Error: processing vignette 'consistencychecking-3.Rmd' failed with diagnostics:
    unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, 
        NULL, NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, "grey20", TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, 
        NULL, NULL, NULL, NULL, list("transparent", NA, NULL, NULL, FALSE), NULL, 2, NULL, NULL, list("transparent", NA, NULL, NULL, FALSE), 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list(), list(NULL, "grey20", NULL, NULL, TRUE), NULL, NULL, 
        NULL, list("grey92", NULL, NULL, NULL, FALSE, "grey92", TRUE), list("grey95", NULL, NULL, NULL, FALSE, "grey95", FALSE), list("grey95", 0.5, NULL, NULL, FALSE, "grey95", FALSE), NULL, NULL, NULL, NULL, FALSE, list("white", NA, NULL, NULL, FALSE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(
            NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list("lightsteelblue1", "black", NULL, NULL, FALSE), NULL, NULL, "on", "inside", list(NULL, NULL, "black", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    --- failed re-building ‘consistencychecking-3.Rmd’
    
    --- re-building ‘dataexploration-1.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘consistencychecking-3.Rmd’
      ...
    |-> direct         |        |  0.228| -0.213|  0.684|
    |-> indirect       |        | -0.515| -0.891| -0.137|
    |                  |        |       |       |       |
    
    > plot(nodesplit, plot.type = "forest")
    
      When sourcing ‘consistencychecking-3.R’:
    ...
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 
    Execution halted
    
      ‘consistencychecking-3.Rmd’ using ‘UTF-8’... failed
      ‘dataexploration-1.Rmd’ using ‘UTF-8’... failed
      ‘mbnmatime-overview.Rmd’ using ‘UTF-8’... OK
      ‘outputs-4.Rmd’ using ‘UTF-8’... failed
      ‘predictions-5.Rmd’ using ‘UTF-8’... OK
      ‘runmbnmatime-2.Rmd’ using ‘UTF-8’... OK
    ```

# mecoturn

<details>

* Version: 0.3.0
* GitHub: https://github.com/ChiLiubio/mecoturn
* Source code: https://github.com/cran/mecoturn
* Date/Publication: 2023-09-10 13:40:02 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "mecoturn")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mecoturn-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: betaturn
    > ### Title: Analyze the 'turnover' of microbial communities.
    > ### Aliases: betaturn
    > 
    > ### ** Examples
    > 
    > 
    ...
     13. │               └─ggplot2:::`+.gg`(...)
     14. │                 └─ggplot2:::add_ggplot(e1, e2, e2name)
     15. │                   ├─ggplot2::ggplot_add(object, p, objectname)
     16. │                   └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     17. │                     └─ggplot2:::new_layer_names(object, names(plot$layers))
     18. └─base::.handleSimpleError(...)
     19.   └─purrr (local) h(simpleError(msg, call))
     20.     └─cli::cli_abort(...)
     21.       └─rlang::abort(...)
    Execution halted
    ```

# MetaNet

<details>

* Version: 0.1.2
* GitHub: https://github.com/Asa12138/MetaNet
* Source code: https://github.com/cran/MetaNet
* Date/Publication: 2024-03-25 20:40:07 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "MetaNet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MetaNet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as.ggig
    > ### Title: Transfer an igraph object to a ggig
    > ### Aliases: as.ggig
    > 
    > ### ** Examples
    > 
    > as.ggig(co_net, coors = c_net_layout(co_net)) -> ggig
    > plot(ggig)
    Warning: Removed 446 rows containing missing values or values outside the scale range
    (`geom_text()`).
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
    ```

# metR

<details>

* Version: 0.15.0
* GitHub: https://github.com/eliocamp/metR
* Source code: https://github.com/cran/metR
* Date/Publication: 2024-02-09 00:40:02 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "metR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘metR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_contour_tanaka
    > ### Title: Illuminated contours
    > ### Aliases: geom_contour_tanaka GeomContourTanaka
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
     19. │                   ├─rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
     20. │                   └─self$draw_panel(...)
     21. │                     └─metR (local) draw_panel(...)
     22. │                       └─metR:::stopf(...)
     23. │                         └─base::stop(e)
     24. └─rlang (local) `<fn>`(`<smplErrr>`)
     25.   └─handlers[[1L]](cnd)
     26.     └─cli::cli_abort(...)
     27.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(metR)
      > # library(vdiffr)
      > 
      > on_cran <- !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
      > if (on_cran) data.table::setDTthreads(2)
      > 
    ...
      • vis-streamline/streamline-ywrapped.svg
      • vis-text-contour/labels-text.svg
      • vis-text-contour/minsize.svg
      • vis-text-contour/placement-fraction.svg
      • vis-text-contour/placement-minmax-horizontal.svg
      • vis-text-contour/placement-minmax-vertical.svg
      • vis-text-contour/placement-n.svg
      • vis-text-contour/text-contour-norotate.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Visualization-tools.Rmd’
      ...
    
    > ggplot(temperature[lev == 300], aes(lon, lat, z = air.z)) + 
    +     geom_contour_fill() + geom_contour_tanaka() + scale_fill_divergent()
    
      When sourcing ‘Visualization-tools.R’:
    Error: Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error:
    ! geom_path: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line.
    Execution halted
    
      ‘Visualization-tools.Rmd’ using ‘UTF-8’... failed
      ‘Working-with-data.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Visualization-tools.Rmd’ using knitr
    
    Quitting from lines 241-245 [unnamed-chunk-16] (Visualization-tools.Rmd)
    Error: processing vignette 'Visualization-tools.Rmd' failed with diagnostics:
    Problem while converting geom to grob.
    ℹ Error occurred in the 2nd layer.
    Caused by error:
    ! geom_path: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line.
    ...
    --- failed re-building ‘Visualization-tools.Rmd’
    
    --- re-building ‘Working-with-data.Rmd’ using knitr
    --- finished re-building ‘Working-with-data.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Visualization-tools.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   2.0Mb
        doc    1.8Mb
    ```

# metrica

<details>

* Version: 2.1.0
* GitHub: https://github.com/adriancorrendo/metrica
* Source code: https://github.com/cran/metrica
* Date/Publication: 2024-06-30 14:20:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "metrica")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘regression_case.Rmd’
      ...
    +     mutate(Year = seq(2001, 2020, by = 1))
    
    > wheat_time %>% ggplot2::ggplot(aes(x = Year)) + geom_point(aes(y = pred, 
    +     fill = "Predicted", shape = "Predicted")) + geom_point(aes(y = obs,  .... [TRUNCATED] 
    Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ℹ Please use `linewidth` instead.
    
    ...
    Execution halted
    
      ‘Cheatsheet.Rmd’ using ‘UTF-8’... OK
      ‘JOSS_publication.Rmd’ using ‘UTF-8’... OK
      ‘Shinyapp.Rmd’ using ‘UTF-8’... OK
      ‘apsim_open.Rmd’ using ‘UTF-8’... OK
      ‘available_metrics_classification.Rmd’ using ‘UTF-8’... OK
      ‘available_metrics_regression.Rmd’ using ‘UTF-8’... OK
      ‘classification_case.Rmd’ using ‘UTF-8’... OK
      ‘regression_case.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
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

# miceRanger

<details>

* Version: 1.5.0
* GitHub: https://github.com/FarrellDay/miceRanger
* Source code: https://github.com/cran/miceRanger
* Date/Publication: 2021-09-06 15:20:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "miceRanger")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘usingMiceRanger.Rmd’
      ...
    > plotList <- lapply(plotVars, function(x) {
    +     missIndx <- is.na(ampIris[, get(x)])
    +     impVsAmp <- data.table(originalData = iris[missIndx, x], .... [TRUNCATED] 
    
      When sourcing ‘usingMiceRanger.R’:
    Error: ℹ In index: 1.
    ℹ With name: imputedData.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    Execution halted
    
      ‘diagnosticPlotting.Rmd’ using ‘UTF-8’... OK
      ‘miceAlgorithm.Rmd’ using ‘UTF-8’... OK
      ‘usingMiceRanger.Rmd’ using ‘UTF-8’... failed
    ```

# microbial

<details>

* Version: 0.0.21
* GitHub: NA
* Source code: https://github.com/cran/microbial
* Date/Publication: 2024-05-15 18:20:02 UTC
* Number of recursive dependencies: 179

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
     12. │         └─ggplot2:::`+.gg`(...)
     13. │           └─ggplot2:::add_ggplot(e1, e2, e2name)
     14. │             ├─ggplot2::ggplot_add(object, p, objectname)
     15. │             └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     16. │               └─ggplot2:::new_layer_names(object, names(plot$layers))
     17. └─base::.handleSimpleError(...)
     18.   └─purrr (local) h(simpleError(msg, call))
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘microbial.Rmd’
      ...
    > plotbar(phy, level = "Phylum")
    
    > plotalpha(physeq, group = "group")
    
      When sourcing ‘microbial.R’:
    Error: ℹ In index: 1.
    ℹ With name: val.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    Execution halted
    
      ‘microbial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘microbial.Rmd’ using knitr
    ```

# MicrobiomeSurv

<details>

* Version: 0.1.0
* GitHub: https://github.com/N-T-Huyen/MicrobiomeSurv
* Source code: https://github.com/cran/MicrobiomeSurv
* Date/Publication: 2023-10-12 06:20:02 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "MicrobiomeSurv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MicrobiomeSurv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: EstimateHR
    > ### Title: Classification, Survival Estimation and Visualization
    > ### Aliases: EstimateHR
    > 
    > ### ** Examples
    > 
    > # Prepare data
    ...
    +                                 Mean = TRUE)
    > 
    > # Using the function
    > est_HR_fam_shan_w3 = EstimateHR(Risk.Scores = lasso_fam_shan_w3$Risk.Scores,
    +                                 Data.Survival = lasso_fam_shan_w3$Data.Survival,
    +                                 Prognostic = prog_fam_shan_w3, Plots = TRUE,
    +                                 Mean = TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: EstimateHR ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# migraph

<details>

* Version: 1.4.2
* GitHub: https://github.com/stocnet/migraph
* Source code: https://github.com/cran/migraph
* Date/Publication: 2024-09-04 12:00:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "migraph")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(manynet)
      > library(migraph)
      > 
      > test_check("migraph")
      Starting 2 test processes
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 46 ]
    ...
      `expected` is a character vector ('Statistic')
      ── Failure ('test-model_tests.R:73:3'): qap plot works ─────────────────────────
      qapplot$labels$x (`actual`) not identical to "Statistic" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('Statistic')
      
      [ FAIL 2 | WARN 2 | SKIP 0 | PASS 46 ]
      Error: Test failures
      Execution halted
    ```

# mikropml

<details>

* Version: 1.6.1
* GitHub: https://github.com/SchlossLab/mikropml
* Source code: https://github.com/cran/mikropml
* Date/Publication: 2023-08-21 15:10:05 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "mikropml")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mikropml)
      > test_check("mikropml")
      Fraction of data in the training set: 0.778 
      	Groups in the training set: A C D 
      	Groups in the testing set: B
      Fraction of data in the training set: 0.778 
    ...
      `names(expected)` is absent
      ── Failure ('test-plot.R:140:3'): plot_mean_prc uses geom ribbon, line, and hline ──
      ... %>% unlist() (`actual`) not equal to c(...) (`expected`).
      
      `names(actual)` is a character vector ('geom_ribbon1', 'geom_ribbon2', 'geom_ribbon3', 'geom_ribbon4', 'geom_line1', ...)
      `names(expected)` is absent
      
      [ FAIL 2 | WARN 19 | SKIP 12 | PASS 314 ]
      Error: Test failures
      Execution halted
    ```

# MiMIR

<details>

* Version: 1.5
* GitHub: NA
* Source code: https://github.com/cran/MiMIR
* Date/Publication: 2024-02-01 08:50:02 UTC
* Number of recursive dependencies: 191

Run `revdepcheck::cloud_details(, "MiMIR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MiMIR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: LOBOV_accuracies
    > ### Title: LOBOV_accuracies
    > ### Aliases: LOBOV_accuracies
    > 
    > ### ** Examples
    > 
    > require(pROC)
    ...
      56 metabolites x  500 samples 
    | Pruning samples on5SD:
      56 metabolites x  500 samples 
    | Performing scaling ...  DONE!
    | Imputation ...  DONE!
    > p_avail<-colnames(b_p)[c(1:5)]
    > LOBOV_accuracies(sur$surrogates, b_p, p_avail, MiMIR::acc_LOBOV)
    Error in pm[[2]] : subscript out of bounds
    Calls: LOBOV_accuracies -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# miRetrieve

<details>

* Version: 1.3.4
* GitHub: NA
* Source code: https://github.com/cran/miRetrieve
* Date/Publication: 2021-09-18 17:30:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "miRetrieve")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(miRetrieve)
      > 
      > test_check("miRetrieve")
      [ FAIL 1 | WARN 11 | SKIP 0 | PASS 202 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Backtrace:
          ▆
       1. └─miRetrieve::compare_mir_terms_scatter(df_merged, "miR-21", title = "Test_title") at test-comparemirterms.R:56:1
       2.   ├─plotly::ggplotly(plot)
       3.   └─plotly:::ggplotly.ggplot(plot)
       4.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 11 | SKIP 0 | PASS 202 ]
      Error: Test failures
      Execution halted
    ```

# MiscMetabar

<details>

* Version: 0.9.3
* GitHub: https://github.com/adrientaudiere/MiscMetabar
* Source code: https://github.com/cran/MiscMetabar
* Date/Publication: 2024-09-09 09:20:01 UTC
* Number of recursive dependencies: 420

Run `revdepcheck::cloud_details(, "MiscMetabar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MiscMetabar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggscatt_pq
    > ### Title: Scatterplot with marginal distributions and statistical results
    > ###   against Hill diversity of phyloseq object
    > ### Aliases: ggscatt_pq
    > 
    > ### ** Examples
    > 
    ...
    +   )
    + }
    Loading required namespace: ggstatsplot
    Taxa are now in columns.
    Cleaning suppress 0 taxa and 0 samples.
    Taxa are now in rows.
    Joining with `by = join_by(Sample)`
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: ggscatt_pq ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(MiscMetabar)
      Loading required package: phyloseq
      Loading required package: ggplot2
      Loading required package: dada2
      Loading required package: Rcpp
      Loading required package: dplyr
    ...
       14.       └─ggplot2:::add_ggplot(e1, e2, e2name)
       15.         ├─ggplot2::ggplot_add(object, p, objectname)
       16.         └─ggplot2:::ggplot_add.list(object, p, objectname)
       17.           ├─ggplot2::ggplot_add(o, plot, object_name)
       18.           └─ggplot2:::ggplot_add.Layer(o, plot, object_name)
       19.             └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 2 | WARN 0 | SKIP 76 | PASS 82 ]
      Error: Test failures
      Execution halted
    ```

# misspi

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/misspi
* Date/Publication: 2023-10-17 09:50:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "misspi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘misspi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: evaliq
    > ### Title: Evaluate the Imputation Quality
    > ### Aliases: evaliq
    > 
    > ### ** Examples
    > 
    > # A very quick example
    ...
    > # Default plot
    > er.eval <- evaliq(x.true[na.idx], x.est[na.idx])
    `geom_smooth()` using formula = 'y ~ x'
    > 
    > # Interactive plot
    > er.eval <- evaliq(x.true[na.idx], x.est[na.idx], interactive = TRUE)
    `geom_smooth()` using formula = 'y ~ x'
    Error in pm[[2]] : subscript out of bounds
    Calls: evaliq -> print -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# mixpoissonreg

<details>

* Version: 1.0.0
* GitHub: https://github.com/vpnsctl/mixpoissonreg
* Source code: https://github.com/cran/mixpoissonreg
* Date/Publication: 2021-03-10 19:50:06 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "mixpoissonreg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mixpoissonreg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.mixpoissonreg
    > ### Title: Autoplot Method for 'mixpoissonreg' Objects
    > ### Aliases: autoplot.mixpoissonreg autoplot
    > 
    > ### ** Examples
    > 
    > daysabs_prog <- mixpoissonregML(daysabs ~ prog, data = Attendance)
    > autoplot(daysabs_prog)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: autoplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mixpoissonreg)
      > 
      > test_check("mixpoissonreg")
      
      Negative Binomial Regression - Expectation-Maximization Algorithm
      
    ...
       2. └─mixpoissonreg:::autoplot.mixpoissonreg(fit_ml1, nrow = 2)
       3.   └─ggplot2:::`+.gg`(...)
       4.     └─ggplot2:::add_ggplot(e1, e2, e2name)
       5.       ├─ggplot2::ggplot_add(object, p, objectname)
       6.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       7.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 28 | SKIP 0 | PASS 35 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘influence-mixpoissonreg.Rmd’
      ...
    5    2.747544 -0.2460342 -0.006644830   -0.4252217      -1.269154
    6    2.746786 -0.2443429 -0.006641383   -0.4268347      -1.269387
    
    > plot(fit, which = c(3, 4, 5))
    
    > autoplot(fit, which = c(3, 4, 5))
    
    ...
    
      When sourcing ‘tutorial-mixpoissonreg.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘influence-mixpoissonreg.Rmd’ using ‘UTF-8’... failed
      ‘intervals-mixpoissonreg.Rmd’ using ‘UTF-8’... OK
      ‘ml-mixpoissonreg.Rmd’ using ‘UTF-8’... failed
      ‘tidyverse-mixpoissonreg.Rmd’ using ‘UTF-8’... failed
      ‘tutorial-mixpoissonreg.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘influence-mixpoissonreg.Rmd’ using rmarkdown
    ```

# mizer

<details>

* Version: 2.5.1
* GitHub: https://github.com/sizespectrum/mizer
* Source code: https://github.com/cran/mizer
* Date/Publication: 2024-03-08 23:10:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "mizer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mizer)
      > 
      > test_check("mizer")
      [ FAIL 10 | WARN 0 | SKIP 5 | PASS 1251 ]
      
    ...
      • plots/plot-spectra.svg
      • plots/plot-yield-by-gear.svg
      • plots/plot-yield.svg
      • plots/plotfishing-mortality.svg
      • plots/plotfmort-truncated.svg
      • plots/plotpredation-mortality.svg
      • plots/plotpredmort-truncated.new.svg
      • plots/plotpredmort-truncated.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc    1.5Mb
        help   1.8Mb
    ```

# mlr3spatiotempcv

<details>

* Version: 2.3.1
* GitHub: https://github.com/mlr-org/mlr3spatiotempcv
* Source code: https://github.com/cran/mlr3spatiotempcv
* Date/Publication: 2024-04-17 12:10:05 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "mlr3spatiotempcv")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mlr3spatiotempcv-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.ResamplingCustomCV
    > ### Title: Visualization Functions for Non-Spatial CV Methods.
    > ### Aliases: autoplot.ResamplingCustomCV plot.ResamplingCustomCV
    > 
    > ### ** Examples
    > 
    > if (mlr3misc::require_namespaces(c("sf", "patchwork"), quietly = TRUE)) {
    ...
    + 
    +   autoplot(resampling, task) +
    +     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
    +   autoplot(resampling, task, fold_id = 1)
    +   autoplot(resampling, task, fold_id = c(1, 2)) *
    +     ggplot2::scale_x_continuous(breaks = seq(-79.085, -79.055, 0.01))
    + }
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘spatiotemp-viz.Rmd’
      ...
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
    
    > knitr::include_graphics("../man/figures/sptcv_cstf_multiplot.png")
    
      When sourcing ‘spatiotemp-viz.R’:
    Error: Cannot find the file(s): "../man/figures/sptcv_cstf_multiplot.png"
    Execution halted
    
      ‘mlr3spatiotempcv.Rmd’ using ‘UTF-8’... OK
      ‘spatiotemp-viz.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        data   3.5Mb
    ```

# mlr3viz

<details>

* Version: 0.9.0
* GitHub: https://github.com/mlr-org/mlr3viz
* Source code: https://github.com/cran/mlr3viz
* Date/Publication: 2024-07-01 12:30:02 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "mlr3viz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mlr3viz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.OptimInstanceBatchSingleCrit
    > ### Title: Plots for Optimization Instances
    > ### Aliases: autoplot.OptimInstanceBatchSingleCrit
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("mlr3") && requireNamespace("bbotk") && requireNamespace("patchwork")) {
    ...
    INFO  [09:22:56.650] [bbotk]   5.884797  2.2371095 -32.51896
    INFO  [09:22:56.650] [bbotk]  -7.841127 -0.8872557 -91.31148
    INFO  [09:22:56.668] [bbotk] Finished optimizing after 20 evaluation(s)
    INFO  [09:22:56.669] [bbotk] Result:
    INFO  [09:22:56.670] [bbotk]        x1        x2  x_domain        y
    INFO  [09:22:56.670] [bbotk]     <num>     <num>    <list>    <num>
    INFO  [09:22:56.670] [bbotk]  2.582281 -2.940254 <list[2]> 9.657379
    Error in identicalUnits(x) : object is not a unit
    Calls: print ... assemble_guides -> guides_build -> unit.c -> identicalUnits
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
      [ FAIL 4 | WARN 1 | SKIP 24 | PASS 84 ]
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

# modeltime.resample

<details>

* Version: 0.2.3
* GitHub: https://github.com/business-science/modeltime.resample
* Source code: https://github.com/cran/modeltime.resample
* Date/Publication: 2023-04-12 15:50:02 UTC
* Number of recursive dependencies: 227

Run `revdepcheck::cloud_details(, "modeltime.resample")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > # Machine Learning
      > library(tidymodels)
      ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
      ✔ broom        1.0.6          ✔ recipes      1.1.0     
      ✔ dials        1.3.0          ✔ rsample      1.2.1     
    ...
          ▆
       1. ├─m750_models_resample %>% ... at test-modeltime_fit_resamples.R:116:5
       2. └─modeltime.resample::plot_modeltime_resamples(., .interactive = TRUE)
       3.   ├─plotly::ggplotly(g)
       4.   └─plotly:::ggplotly.ggplot(g)
       5.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 16 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘crayon’ ‘dials’ ‘glue’ ‘parsnip’
      All declared Imports should be used.
    ```

# moreparty

<details>

* Version: 0.4
* GitHub: NA
* Source code: https://github.com/cran/moreparty
* Date/Publication: 2023-11-22 14:30:02 UTC
* Number of recursive dependencies: 165

Run `revdepcheck::cloud_details(, "moreparty")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Titanic_example.Rmd’
      ...
    24      Age        57 0.3187270
    25 Embarked Cherbourg 0.4603041
     [ reached 'max' / getOption("max.print") -- omitted 2 rows ]
    
    > ggForestEffects(pdep, vline = mean(pred_foret), xlab = "Probability of survival") + 
    +     xlim(c(0, 1))
    
      When sourcing ‘Titanic_example.R’:
    Error: `x` must be a vector, not a <data.frame/partial> object.
    Execution halted
    
      ‘Titanic_example.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Titanic_example.Rmd’ using rmarkdown
    ```

# mosaicCalc

<details>

* Version: 0.6.4
* GitHub: https://github.com/ProjectMOSAIC/mosaicCalc
* Source code: https://github.com/cran/mosaicCalc
* Date/Publication: 2024-07-26 15:50:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "mosaicCalc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mosaicCalc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: box_set
    > ### Title: Evenly spaced samples across a one- or two-dim domain
    > ### Aliases: box_set
    > 
    > ### ** Examples
    > 
    > box_set(x*y ~ x & y, domain(x=0:1, y=0:1), n = 4)
    ...
    [1] 0
    > # a polygon
    > poly <- tibble(x = c(1:9, 8:1), y = c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9))
    > boxes <- box_set(1 ~ x & y, poly, dx = 1)
    > gf_polygon(y ~ x, data = poly, color="blue", fill="blue", alpha=0.2) %>%
    +   gf_rect((y - dy/3) + (y + dy/3) ~ (x - dx/3) + (x + dx/3),
    +   data = boxes)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: %>% ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Calculus_with_R.Rmd’
      ...
    
    > soln <- integrateODE(SIR, bounds(t = 0:20))
    Solution containing functions S(t), I(t).
    
    > traj_plot(S(t) ~ I(t), soln, color = "blue") %>% vectorfield_plot(SIR, 
    +     bounds(I = 0:75, S = 60:400), transform = I, npts = 20, alpha = 0.6)
    
    ...
    
    > gf_point(flipper_length_mm ~ body_mass_g, data = palmerpenguins::penguins)
    
      When sourcing ‘quick-reference.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Calculus_with_R.Rmd’ using ‘UTF-8’... failed
      ‘Instructors.Rmd’ using ‘UTF-8’... failed
      ‘quick-reference.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Calculus_with_R.Rmd’ using rmarkdown
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 56 marked UTF-8 strings
    ```

# mosaicData

<details>

* Version: 0.20.4
* GitHub: https://github.com/ProjectMOSAIC/mosaicData
* Source code: https://github.com/cran/mosaicData
* Date/Publication: 2023-11-05 05:50:02 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "mosaicData")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mosaicData-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Birthdays
    > ### Title: US Births in 1969 - 1988
    > ### Aliases: Birthdays
    > 
    > ### ** Examples
    > 
    > data(Birthdays)
    ...
        IQR, binom.test, cor, cor.test, cov, fivenum, median, prop.test,
        quantile, sd, t.test, var
    
    The following objects are masked from ‘package:base’:
    
        max, mean, min, prod, range, sample, sum
    
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: gf_point ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# mosaicModel

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/mosaicModel
* Date/Publication: 2017-09-22 16:21:41 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "mosaicModel")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Basics.Rmd’
      ...
    +     out.width = "45%")
    
    > mtcars <- mtcars %>% mutate(transmission = ifelse(am, 
    +     "manual", "automatic"))
    
    > gf_point(mpg ~ hp, color = ~transmission, data = mtcars)
    
      When sourcing ‘Basics.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Basics.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Basics.Rmd’ using rmarkdown
    
    Quitting from lines 66-68 [fuel_intro] (Basics.Rmd)
    Error: processing vignette 'Basics.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Basics.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Basics.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘caret’ ‘ggformula’ ‘knitr’ ‘testthat’ ‘tidyverse’
      All declared Imports should be used.
    ```

# mppR

<details>

* Version: 1.5.0
* GitHub: https://github.com/vincentgarin/mppR
* Source code: https://github.com/cran/mppR
* Date/Publication: 2024-02-22 17:20:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "mppR")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘MPP_ME_QTL_detect.Rmd’
      ...
    F2     -2.9970560 0.18508863 -0.00806081     0.1266095
    F283   10.2417600 1.78749082 -0.04223259     2.0862106
    DK105   0.1792433 0.03829626          NA            NA
    
    > plot_QxEC(Qeff, EC = EC, env_id = c("CIAM", "TUM", 
    +     "INRA", "KWS"), QTL = 2, EC_id = "cum rain", trait_id = "DMY")
    
      When sourcing ‘MPP_ME_QTL_detect.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘MPP_ME_QTL_detect.Rmd’ using ‘UTF-8’... failed
      ‘mppR_gen_vignette.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MPP_ME_QTL_detect.Rmd’ using rmarkdown
    ```

# MSCMT

<details>

* Version: 1.4.0
* GitHub: NA
* Source code: https://github.com/cran/MSCMT
* Date/Publication: 2024-03-19 10:20:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "MSCMT")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘WorkingWithMSCMT.Rmd’
      ...
                    (Predictor weights V are standardized by sum(V)=1)
     
    
    > library(ggplot2)
    
    > ggplot(res, type = "comparison")
    
      When sourcing ‘WorkingWithMSCMT.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘CheckingSynth.Rmd’ using ‘UTF-8’... OK
      ‘UsingTimeSeries.Rmd’ using ‘UTF-8’... OK
      ‘WorkingWithMSCMT.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘CheckingSynth.Rmd’ using rmarkdown
    --- finished re-building ‘CheckingSynth.Rmd’
    
    --- re-building ‘UsingTimeSeries.Rmd’ using rmarkdown
    --- finished re-building ‘UsingTimeSeries.Rmd’
    
    --- re-building ‘WorkingWithMSCMT.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 156-158 [unnamed-chunk-8] (WorkingWithMSCMT.Rmd)
    Error: processing vignette 'WorkingWithMSCMT.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘WorkingWithMSCMT.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘WorkingWithMSCMT.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# mstate

<details>

* Version: 0.3.3
* GitHub: https://github.com/hputter/mstate
* Source code: https://github.com/cran/mstate
* Date/Publication: 2024-07-11 21:30:06 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "mstate")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mstate-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.Cuminc
    > ### Title: Plot method for Cuminc objects
    > ### Aliases: plot.Cuminc
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
      4.   └─ggplot2:::ggplot_build.ggplot(x)
      5.     └─layout$setup(data, plot$data, plot$plot_env)
      6.       └─ggplot2 (local) setup(..., self = self)
      7.         └─self$coord$setup_params(data)
      8.           └─ggplot2 (local) setup_params(..., self = self)
      9.             └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
     10.               └─ggplot2:::check_logical(expand)
     11.                 └─ggplot2:::stop_input_type(...)
     12.                   └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘visuals_demo.Rmd’
      ...
    
    > msf.WW <- msfit(object = c1, newdata = WW, trans = tmat)
    
    > plot(msf.WW)
    
    > plot(msf.WW, use.ggplot = TRUE)
    
      When sourcing ‘visuals_demo.R’:
    Error: `expand` must be a logical vector, not the number 0.
    Execution halted
    
      ‘visuals_demo.Rmd’ using ‘UTF-8’... failed
      ‘Tutorial.Rnw’ using ‘UTF-8’... OK
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘visuals_demo.Rmd’ using rmarkdown
    ```

# mtb

<details>

* Version: 0.1.8
* GitHub: https://github.com/yh202109/mtb
* Source code: https://github.com/cran/mtb
* Date/Publication: 2022-10-20 17:22:35 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "mtb")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mtb)
      > 
      > test_check("mtb")
      [ FAIL 2 | WARN 13 | SKIP 0 | PASS 56 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
                 - "yend"       [6]           
                 - "xmin"       [7]           
                 - "xmax"       [8]           
                 - "ymin"       [9]           
                 - "ymax"       [10]          
      ... ...      ...          and 3 more ...
      
      [ FAIL 2 | WARN 13 | SKIP 0 | PASS 56 ]
      Error: Test failures
      Execution halted
    ```

# mulgar

<details>

* Version: 1.0.2
* GitHub: https://github.com/dicook/mulgar
* Source code: https://github.com/cran/mulgar
* Date/Publication: 2023-08-25 22:00:02 UTC
* Number of recursive dependencies: 43

Run `revdepcheck::cloud_details(, "mulgar")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mulgar-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggmcbic
    > ### Title: Produces an mclust summary plot with ggplot
    > ### Aliases: ggmcbic
    > 
    > ### ** Examples
    > 
    > require(mclust)
    ...
    Type 'citation("mclust")' for citing this R package in publications.
    > data(clusters)
    > clusters_BIC <- mclustBIC(clusters[,1:5], G=2:6)
    > ggmcbic(clusters_BIC)
    Warning: Removed 5 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        data   8.5Mb
    ```

# MultivariateAnalysis

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/MultivariateAnalysis
* Date/Publication: 2024-04-08 18:40:03 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "MultivariateAnalysis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘MultivariateAnalysis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ContribuicaoRelativa
    > ### Title: Contribuicao das variaveis independentes para o agrupamento
    > ### Aliases: ContribuicaoRelativa
    > 
    > ### ** Examples
    > 
    > 
    ...
     12. │             └─ggplot2:::`+.gg`(p, do.call(geom_line, option))
     13. │               └─ggplot2:::add_ggplot(e1, e2, e2name)
     14. │                 ├─ggplot2::ggplot_add(object, p, objectname)
     15. │                 └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     16. │                   └─ggplot2:::new_layer_names(object, names(plot$layers))
     17. └─base::.handleSimpleError(...)
     18.   └─purrr (local) h(simpleError(msg, call))
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

# mxfda

<details>

* Version: 0.2.1
* GitHub: https://github.com/julia-wrobel/mxfda
* Source code: https://github.com/cran/mxfda
* Date/Publication: 2024-05-08 11:00:02 UTC
* Number of recursive dependencies: 221

Run `revdepcheck::cloud_details(, "mxfda")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
    ```

# neatmaps

<details>

* Version: 2.1.0
* GitHub: https://github.com/PhilBoileau/neatmaps
* Source code: https://github.com/cran/neatmaps
* Date/Publication: 2019-05-12 19:10:03 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "neatmaps")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘neatmaps-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: consClustResTable
    > ### Title: Consensus Cluster Results in a Table
    > ### Aliases: consClustResTable
    > 
    > ### ** Examples
    > 
    > # create the data frame using the network, node and edge attributes
    ...
    > df <- netsDataFrame(network_attr_df,
    +                     node_attr_df,
    +                     edge_df)
    > 
    > # run the neatmap code on df
    > neat_res <- neatmap(df, scale_df = "ecdf", max_k = 3, reps = 100, 
    +                     xlab = "vars", ylab = "nets", xlab_cex = 1, ylab_cex = 1)
    Error in pm[[2]] : subscript out of bounds
    Calls: neatmap ... %>% -> layout -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
    ```

# neatStats

<details>

* Version: 1.13.3
* GitHub: https://github.com/gasparl/neatstats
* Source code: https://github.com/cran/neatStats
* Date/Publication: 2022-12-07 20:50:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "neatStats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘neatStats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: peek_neat
    > ### Title: Cursory Summaries and Plots per Group
    > ### Aliases: peek_neat
    > 
    > ### ** Examples
    > 
    > 
    ...
     11. │           └─ggplot2:::`+.gg`(...)
     12. │             └─ggplot2:::add_ggplot(e1, e2, e2name)
     13. │               ├─ggplot2::ggplot_add(object, p, objectname)
     14. │               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     15. │                 └─ggplot2:::new_layer_names(object, names(plot$layers))
     16. └─base::.handleSimpleError(...)
     17.   └─purrr (local) h(simpleError(msg, call))
     18.     └─cli::cli_abort(...)
     19.       └─rlang::abort(...)
    Execution halted
    ```

# netcom

<details>

* Version: 2.1.7
* GitHub: https://github.com/langendorfr/netcom
* Source code: https://github.com/cran/netcom
* Date/Publication: 2024-06-04 17:50:05 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "netcom")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tutorial.Rmd’
      ...
    > networks <- c(networks_undisturbed, networks_disturbed)
    
    > comparisons <- netcom::compare(networks, method = "align")
    
    > stats::prcomp(comparisons) %>% ggplot2::autoplot(data = tibble(Kind = c(rep("Undisturbed", 
    +     num_networks), rep("Disturbed", num_networks))), c .... [TRUNCATED] 
    
      When sourcing ‘tutorial.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘tutorial.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggfortify’ ‘ggplot2’ ‘ggraph’ ‘reshape2’
      All declared Imports should be used.
    ```

# NetFACS

<details>

* Version: 0.5.0
* GitHub: NA
* Source code: https://github.com/cran/NetFACS
* Date/Publication: 2022-12-06 17:32:35 UTC
* Number of recursive dependencies: 101

Run `revdepcheck::cloud_details(, "NetFACS")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NetFACS-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: network_conditional
    > ### Title: Create a network based on conditional probabilities of dyads of
    > ###   elements
    > ### Aliases: network_conditional
    > 
    > ### ** Examples
    > 
    ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Error in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid font type
    Calls: <Anonymous> ... drawDetails -> drawDetails.text -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘netfacs_tutorial.Rmd’
      ...
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Arial Narrow' not found in PostScript font database
    
      When sourcing ‘netfacs_tutorial.R’:
    Error: invalid font type
    Execution halted
    
      ‘netfacs_tutorial.Rmd’ using ‘UTF-8’... failed
    ```

# neuroUp

<details>

* Version: 0.3.1
* GitHub: https://github.com/eduardklap/neuroUp
* Source code: https://github.com/cran/neuroUp
* Date/Publication: 2024-08-28 08:20:05 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "neuroUp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘neuroUp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: estim_corr
    > ### Title: Estimate correlations
    > ### Aliases: estim_corr
    > 
    > ### ** Examples
    > 
    > data_gambling <- gambling
    ...
     8 100       -0.0787 -0.271 0.120  2               NA
     9 140       -0.0555 -0.219 0.111  2               NA
    10 221       -0.0405 -0.172 0.0920 2               NA
    # ℹ 45 more rows
    
    $fig_corr
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
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
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       25.     └─grid:::grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 10 ]
      Deleting unused snapshots:
      • estim_corr/create-fig-corr-nozero.svg
      • estim_diff/create-fig-cohen-s-d.svg
      • estim_diff/create-fig-d-nozero.svg
      • estim_diff/create-fig-nozero.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘neuroUp.Rmd’
      ...
    > set.seed(1234)
    
    > feedback_estim <- estim_diff(feedback_data, c("mfg_learning", 
    +     "mfg_application"), 20:271, 20, "Feedback middle frontal gyrus")
    
    > feedback_estim$fig_diff
    
      When sourcing ‘neuroUp.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘neuroUp.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘neuroUp.Rmd’ using rmarkdown
    
    Quitting from lines 92-93 [unnamed-chunk-5] (neuroUp.Rmd)
    Error: processing vignette 'neuroUp.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘neuroUp.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘neuroUp.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# NHSRplotthedots

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/NHSRplotthedots
* Date/Publication: 2021-11-03 20:20:10 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "NHSRplotthedots")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(NHSRplotthedots)
      > 
      > test_check("NHSRplotthedots")
      [ FAIL 1 | WARN 733 | SKIP 3 | PASS 431 ]
      
    ...
      
      `actual$type` is absent
      `expected$type` is a character vector ('type')
      
      `actual$text` is absent
      `expected$text` is a character vector ('text')
      
      [ FAIL 1 | WARN 733 | SKIP 3 | PASS 431 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘NHSRdatasets’ ‘grid’ ‘utils’
      All declared Imports should be used.
    ```

# nichetools

<details>

* Version: 0.3.1
* GitHub: https://github.com/benjaminhlina/nichetools
* Source code: https://github.com/cran/nichetools
* Date/Publication: 2024-09-06 17:00:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "nichetools")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘using-nichetools-with-the-package-SIBER.Rmd’
      ...
    +     option = "A", alpha = 0.75)
    
    > ggplot() + stat_pointinterval(data = bays_95_overlap, 
    +     aes(x = group_1, y = prop_overlap, point_fill = group_2), 
    +     interval_colour = "gre ..." ... [TRUNCATED] 
    
      When sourcing ‘using-nichetools-with-the-package-SIBER.R’:
    Error: unused argument (theme = list(list("black", 0.681818181818182, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.681818181818182, 1, TRUE), list("", "plain", "black", 15, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.681818181818182, 0.681818181818182, 1, 1, "", 5.27189705271897, 2.04545454545455, 19, TRUE), 7.5, c(7.5, 7.5, 7.5, 7.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, 
        NULL, c(3.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 3.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 3.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(3, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL,
    Execution halted
    
      ‘using-nichetools-with-the-package-SIBER.Rmd’ using ‘UTF-8’... failed
      ‘using-nichetools-with-the-package-nicheROVER.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘using-nichetools-with-the-package-SIBER.Rmd’ using rmarkdown
    ```

# NIMAA

<details>

* Version: 0.2.1
* GitHub: https://github.com/jafarilab/NIMAA
* Source code: https://github.com/cran/NIMAA
* Date/Publication: 2022-04-11 14:12:45 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::cloud_details(, "NIMAA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘NIMAA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: extractSubMatrix
    > ### Title: Extract the non-missing submatrices from a given matrix.
    > ### Aliases: extractSubMatrix
    > 
    > ### ** Examples
    > 
    > # load part of the beatAML data
    ...
    +  row.vars = "inhibitor")
    binmatnest.temperature 
                  13.21221 
    Size of Square: 	 66 rows x  66 columns 
    Size of Rectangular_row: 	 6 rows x  105 columns 
    Size of Rectangular_col: 	 99 rows x  2 columns 
    Size of Rectangular_element_max: 	 59 rows x  79 columns 
    Error in pm[[2]] : subscript out of bounds
    Calls: extractSubMatrix ... plotSubmatrix -> print -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(NIMAA)
      Warning message:
      In check_dep_version() : ABI version mismatch: 
      lme4 was built with Matrix ABI version 1
      Current Matrix ABI version is 0
      Please re-install lme4 from source or restore original 'Matrix' package
    ...
       1. └─NIMAA::extractSubMatrix(...) at test-extract-nonmissing-submatrix.R:5:3
       2.   └─NIMAA:::plotSubmatrix(...)
       3.     ├─base::print(plotly::ggplotly(p))
       4.     ├─plotly::ggplotly(p)
       5.     └─plotly:::ggplotly.ggplot(p)
       6.       └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 4 | SKIP 0 | PASS 7 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘NIMAA-vignette.Rmd’
      ...
    
    > beatAML_incidence_matrix <- plotIncMatrix(x = beatAML_data, 
    +     index_nominal = c(2, 1), index_numeric = 3, print_skim = FALSE, 
    +     plot_weigh .... [TRUNCATED] 
    
    Na/missing values Proportion: 	 0.2603 
    
      When sourcing ‘NIMAA-vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘NIMAA-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘NIMAA-vignette.Rmd’ using rmarkdown
    
    Quitting from lines 49-57 [plotIncMatrix function] (NIMAA-vignette.Rmd)
    Error: processing vignette 'NIMAA-vignette.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘NIMAA-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘NIMAA-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    4.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# nonmem2R

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/nonmem2R
* Date/Publication: 2024-03-11 17:30:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "nonmem2R")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nonmem2R-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vpcfig2
    > ### Title: Visual Predictive Check (VPC) based on Perl-speaks-NONMEM (PsN)
    > ###   generated VPC files (ggplot2-version).
    > ### Aliases: vpcfig2
    > 
    > ### ** Examples
    > 
    ...
    VPC based on files:
       /tmp/workdir/nonmem2R/new/nonmem2R.Rcheck/nonmem2R/extdata/vpctab004.dat 
    and
       /tmp/workdir/nonmem2R/new/nonmem2R.Rcheck/nonmem2R/extdata/vpc_results.csv 
    Facetting was set using:
     facet_wrap(~strata) 
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘VPCvignette.Rmd’
      ...
    VPC based on files:
       /tmp/workdir/nonmem2R/new/nonmem2R.Rcheck/nonmem2R/extdata/vpctab004.dat 
    and
       /tmp/workdir/nonmem2R/new/nonmem2R.Rcheck/nonmem2R/extdata/vpc_results.csv 
    Facetting was set using:
     facet_wrap(~strata) 
    
      When sourcing ‘VPCvignette.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘GOFvignette.Rmd’ using ‘UTF-8’... OK
      ‘VPCvignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘GOFvignette.Rmd’ using rmarkdown
    ```

# nphRCT

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/nphRCT
* Date/Publication: 2024-06-27 12:30:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "nphRCT")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘explanation.Rmd’
      ...
    
    > km <- survfit(Surv(time, event) ~ arm, data = dat)
    
    > p_km <- survminer::ggsurvplot(km, data = dat, risk.table = TRUE, 
    +     break.x.by = 6, legend.title = "", xlab = "Time (months)", 
    +     ylab = "Ov ..." ... [TRUNCATED] 
    
      When sourcing ‘explanation.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘explanation.Rmd’ using ‘UTF-8’... failed
      ‘weighted_log_rank_tests.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘explanation.Rmd’ using rmarkdown
    
    Quitting from lines 44-73 [unnamed-chunk-1] (explanation.Rmd)
    Error: processing vignette 'explanation.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘explanation.Rmd’
    
    --- re-building ‘weighted_log_rank_tests.Rmd’ using rmarkdown
    ```

# nprobust

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/nprobust
* Date/Publication: 2020-08-26 10:40:02 UTC
* Number of recursive dependencies: 30

Run `revdepcheck::cloud_details(, "nprobust")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nprobust-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: nprobust.plot
    > ### Title: Graphical Presentation of Results from 'nprobust' Package.
    > ### Aliases: nprobust.plot
    > 
    > ### ** Examples
    > 
    > x   <- runif(500) 
    > y   <- sin(4*x) + rnorm(500)
    > est <- lprobust(y,x)
    > nprobust.plot(est)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

# nzelect

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/nzelect
* Date/Publication: 2017-10-02 20:35:23 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "nzelect")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nzelect-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: polls
    > ### Title: New Zealand Opinion Polls
    > ### Aliases: polls
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
    ℹ This can happen when ggplot fails to infer the correct grouping structure in
      the data.
    ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
      variable into a factor?
    Warning: Removed 159 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘README.Rmd’
      ...
    > proportions <- nzge %>% filter(election_year == 2014) %>% 
    +     group_by(voting_place, voting_type) %>% summarise(`proportion Labour` = sum(votes[p .... [TRUNCATED] 
    `summarise()` has grouped output by 'voting_place'. You can override using the
    `.groups` argument.
    
    > ggpairs(proportions, aes(colour = voting_type), columns = 3:5)
    
      When sourcing ‘README.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘README.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘README.Rmd’ using rmarkdown
    
    Quitting from lines 64-82 [unnamed-chunk-3] (README.Rmd)
    Error: processing vignette 'README.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘README.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘README.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6409 marked UTF-8 strings
    ```

# OBIC

<details>

* Version: 3.0.3
* GitHub: https://github.com/AgroCares/Open-Bodem-Index-Calculator
* Source code: https://github.com/cran/OBIC
* Date/Publication: 2024-09-09 08:30:02 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "OBIC")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘obic_workability.Rmd’
      ...
    > gg2 <- ggplot(data = dt, aes(x = field, fill = field)) + 
    +     geom_col(aes(y = I_P_WO)) + theme_bw() + theme(axis.text = element_text(size = 10, 
     .... [TRUNCATED] 
    
    > (gg | gg2) + plot_layout(guides = "collect") + plot_annotation(caption = "Baseline workability scores.", 
    +     theme = theme(plot.caption = element .... [TRUNCATED] 
    
      When sourcing ‘obic_workability.R’:
    Error: object is not a unit
    Execution halted
    
      ‘description-of-the-columns.Rmd’ using ‘UTF-8’... OK
      ‘obic_introduction.Rmd’ using ‘UTF-8’... OK
      ‘obic_score_aggregation.Rmd’ using ‘UTF-8’... OK
      ‘obic_water_functions.Rmd’ using ‘UTF-8’... OK
      ‘obic_workability.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘description-of-the-columns.Rmd’ using rmarkdown
    --- finished re-building ‘description-of-the-columns.Rmd’
    
    --- re-building ‘obic_introduction.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    1.4Mb
    ```

# oceanic

<details>

* Version: 0.1.7
* GitHub: NA
* Source code: https://github.com/cran/oceanic
* Date/Publication: 2024-06-11 03:40:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "oceanic")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘oceanic-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dotplot
    > ### Title: dotplot
    > ### Aliases: dotplot
    > 
    > ### ** Examples
    > 
    > dotplot(141,23)
    ...
      4.   └─ggplot2:::ggplot_build.ggplot(x)
      5.     └─layout$setup(data, plot$data, plot$plot_env)
      6.       └─ggplot2 (local) setup(..., self = self)
      7.         └─self$coord$setup_params(data)
      8.           └─ggplot2 (local) setup_params(..., self = self)
      9.             └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
     10.               └─ggplot2:::check_logical(expand)
     11.                 └─ggplot2:::stop_input_type(...)
     12.                   └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   8.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1242 marked UTF-8 strings
    ```

# oddsratio

<details>

* Version: 2.0.1
* GitHub: https://github.com/pat-s/oddsratio
* Source code: https://github.com/cran/oddsratio
* Date/Publication: 2020-05-24 22:00:02 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "oddsratio")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘oddsratio-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: insert_or
    > ### Title: Insert odds ratios of GAM(M)s into smoothing function
    > ### Aliases: insert_or
    > 
    > ### ** Examples
    > 
    > library(oddsratio)
    ...
      5.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
      6.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      7.           └─vctrs::vec_as_names(names, repair = "check_unique")
      8.             └─vctrs (local) `<fn>`()
      9.               └─vctrs:::validate_unique(names = names, arg = arg, call = call)
     10.                 └─vctrs:::stop_names_cannot_be_empty(names, call = call)
     11.                   └─vctrs:::stop_names(...)
     12.                     └─vctrs:::stop_vctrs(...)
     13.                       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘oddsratio.Rmd’
      ...
    +     pred = "x2", values = c(0.4, 0.6))
    
    > insert_or(plot, or_object2, or_yloc = 2.1, values_yloc = 2, 
    +     line_col = "green4", text_col = "black", rect_col = "green4", 
    +     rect_alpha = .... [TRUNCATED] 
    
      When sourcing ‘oddsratio.R’:
    Error: Names can't be empty.
    ✖ Empty name found at location 1.
    Execution halted
    
      ‘oddsratio.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘oddsratio.Rmd’ using rmarkdown
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gam’
    ```

# ofpetrial

<details>

* Version: 0.1.1
* GitHub: https://github.com/DIFM-Brain/ofpetrial
* Source code: https://github.com/cran/ofpetrial
* Date/Publication: 2024-05-15 08:50:03 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "ofpetrial")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ofpetrial-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: check_ortho_with_chars
    > ### Title: Check the orthogonality with field/topographic characteristics
    > ### Aliases: check_ortho_with_chars
    > 
    > ### ** Examples
    > 
    > data(td_single_input)
    ...
     33. │       └─ggplot2:::`+.gg`(init, x[[i]])
     34. │         └─ggplot2:::add_ggplot(e1, e2, e2name)
     35. │           ├─ggplot2::ggplot_add(object, p, objectname)
     36. │           └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     37. │             └─ggplot2:::new_layer_names(object, names(plot$layers))
     38. └─base::.handleSimpleError(...)
     39.   └─purrr (local) h(simpleError(msg, call))
     40.     └─cli::cli_abort(...)
     41.       └─rlang::abort(...)
    Execution halted
    ```

# OmicNavigator

<details>

* Version: 1.13.13
* GitHub: https://github.com/abbvie-external/OmicNavigator
* Source code: https://github.com/cran/OmicNavigator
* Date/Publication: 2023-08-25 20:40:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "OmicNavigator")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > # Test files in inst/tinytest/
      > if (requireNamespace("tinytest", quietly = TRUE)) {
      +   suppressMessages(tinytest::test_package("OmicNavigator"))
      + }
      
      testAdd.R.....................    0 tests    
      testAdd.R.....................    0 tests    
    ...
      testPlot.R....................  140 tests [0;32mOK[0m 
      testPlot.R....................  140 tests [0;32mOK[0m 
      testPlot.R....................  141 tests [0;32mOK[0m 
      testPlot.R....................  141 tests [0;32mOK[0m 
      testPlot.R....................  141 tests [0;32mOK[0m 
      testPlot.R....................  142 tests [0;32mOK[0m 
      testPlot.R....................  142 tests [0;32mOK[0m 
      testPlot.R....................  143 tests [0;32mOK[0m Error in pm[[2]] : subscript out of bounds
      Calls: suppressMessages ... plotStudy -> f -> <Anonymous> -> ggplotly.ggplot -> gg2list
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘OmicNavigatorAPI.Rnw’ using Sweave
    OmicNavigator R package version: 1.13.13
    The app is not installed. Install it with installApp()
    Installing study "ABC" in /tmp/RtmpFyTBK9/file1d2273c45a46
    Exporting study "ABC" as an R package
    Note: No maintainer email was specified. Using the placeholder: Unknown <unknown@unknown>
    Calculating pairwise overlaps. This may take a while...
    Exported study to /tmp/RtmpFyTBK9/ONstudyABC
    Success!
    ...
    l.14 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘OmicNavigatorUsersGuide.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘OmicNavigatorAPI.Rnw’ ‘OmicNavigatorUsersGuide.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# omu

<details>

* Version: 1.1.2
* GitHub: https://github.com/connor-reid-tiffany/Omu
* Source code: https://github.com/cran/omu
* Date/Publication: 2024-03-06 23:40:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "omu")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘omu-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PCA_plot
    > ### Title: Create a PCA plot
    > ### Aliases: PCA_plot
    > 
    > ### ** Examples
    > 
    > PCA_plot(count_data = c57_nos2KO_mouse_countDF, metadata = c57_nos2KO_mouse_metadata,
    + variable = "Treatment", color = "Treatment", response_variable = "Metabolite")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: PCA_plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Omu_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 97-104 [unnamed-chunk-4] (Omu_vignette.Rmd)
    Error: processing vignette 'Omu_vignette.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Omu_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Omu_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Omu_vignette.Rmd’
      ...
    
    > library(knitr)
    
    > load("../data/c57_nos2KO_mouse_countDF.rda")
    Warning in readChar(con, 5L, useBytes = TRUE) :
      cannot open compressed file '../data/c57_nos2KO_mouse_countDF.rda', probable reason 'No such file or directory'
    
      When sourcing ‘Omu_vignette.R’:
    Error: cannot open the connection
    Execution halted
    
      ‘Omu_vignette.Rmd’ using ‘UTF-8’... failed
    ```

# OncoBayes2

<details>

* Version: 0.8-9
* GitHub: NA
* Source code: https://github.com/cran/OncoBayes2
* Date/Publication: 2023-07-20 18:40:05 UTC
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "OncoBayes2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘OncoBayes2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_blrm
    > ### Title: Plot a fitted model
    > ### Aliases: plot_blrm plot_toxicity_curve plot_toxicity_intervals
    > ###   plot_toxicity_intervals_stacked plot_toxicity_curve.blrmfit
    > ###   plot_toxicity_curve.blrm_trial plot_toxicity_intervals.blrmfit
    > ###   plot_toxicity_intervals.blrm_trial
    > ###   plot_toxicity_intervals_stacked.blrmfit
    ...
    > # Plot the dose-toxicity curve
    > plot_toxicity_curve(blrmfit,
    +                     x = "drug_A",
    +                     group = ~ group_id * drug_B,
    +                     newdata = subset(dose_info_combo2, group_id == "trial_AB"),
    +                     facet_args = list(ncol = 4))
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 76.7Mb
      sub-directories of 1Mb or more:
        libs  74.7Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# oncomsm

<details>

* Version: 0.1.4
* GitHub: https://github.com/Boehringer-Ingelheim/oncomsm
* Source code: https://github.com/cran/oncomsm
* Date/Publication: 2023-04-17 07:00:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "oncomsm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following objects are masked from 'package:stats':
      
          filter, lag
    ...
       10.               └─grid::unit.c(legend.box.margin[4], widths, legend.box.margin[2])
       11.                 └─grid:::identicalUnits(x)
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 59 ]
      Deleting unused snapshots:
      • plots/plot-mstate-srp-model-2.svg
      • plots/plot-mstate-srp-model-3.svg
      • plots/plot-srp-model-2.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘avoiding-bias.Rmd’
      ...
    
    > mdl <- create_srpmodel(A = define_srp_prior(median_t_q05 = c(1, 
    +     4, 12), median_t_q95 = c(6, 8, 36), shape_q05 = c(0.99, 0.99, 
    +     0.99), s .... [TRUNCATED] 
    
    > plot(mdl, confidence = 0.9)
    
    ...
    
    > plot(mdl, parameter_sample = smpl_prior, confidence = 0.75)
    
      When sourcing ‘oncomsm.R’:
    Error: object is not a unit
    Execution halted
    
      ‘avoiding-bias.Rmd’ using ‘UTF-8’... failed
      ‘oncomsm.Rmd’ using ‘UTF-8’... failed
      ‘prior-choice.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘avoiding-bias.Rmd’ using rmarkdown
    
    Quitting from lines 35-46 [unnamed-chunk-2] (avoiding-bias.Rmd)
    Error: processing vignette 'avoiding-bias.Rmd' failed with diagnostics:
    object is not a unit
    --- failed re-building ‘avoiding-bias.Rmd’
    
    --- re-building ‘oncomsm.Rmd’ using rmarkdown
    
    Quitting from lines 211-215 [plotting-the-prior] (oncomsm.Rmd)
    Error: processing vignette 'oncomsm.Rmd' failed with diagnostics:
    object is not a unit
    --- failed re-building ‘oncomsm.Rmd’
    
    --- re-building ‘prior-choice.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 59.2Mb
      sub-directories of 1Mb or more:
        doc    1.1Mb
        libs  57.0Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# OneSampleLogRankTest

<details>

* Version: 0.9.2
* GitHub: NA
* Source code: https://github.com/cran/OneSampleLogRankTest
* Date/Publication: 2024-02-03 12:30:15 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "OneSampleLogRankTest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘OneSampleLogRankTest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotKM
    > ### Title: Plot Kaplan-Meier Curve against Population
    > ### Aliases: plotKM
    > 
    > ### ** Examples
    > 
    > # load data
    > data(dataSurv_small)
    > data(dataPop_2018_2021)
    > 
    > plotKM(dataSurv_small, dataPop_2018_2021, type = "exact")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plotKM ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘oneSampleLogRankTest.Rmd’
      ...
    $estimate
      std_mort_ratio_est       lwr      upr
    1           1.531173 0.8302562 2.823816
    
    
    > plotKM(dataSurv, dataPop_2018_2021_race_sex_eth, type = "approximate")
    
      When sourcing ‘oneSampleLogRankTest.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘oneSampleLogRankTest.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘oneSampleLogRankTest.Rmd’ using rmarkdown
    
    Quitting from lines 74-77 [unnamed-chunk-3] (oneSampleLogRankTest.Rmd)
    Error: processing vignette 'oneSampleLogRankTest.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘oneSampleLogRankTest.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘oneSampleLogRankTest.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# onpoint

<details>

* Version: 1.0.5
* GitHub: https://github.com/r-spatialecology/onpoint
* Source code: https://github.com/cran/onpoint
* Date/Publication: 2024-01-10 14:03:06 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "onpoint")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘onpoint-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.env_summarized
    > ### Title: plot.env_summarized
    > ### Aliases: plot.env_summarized
    > 
    > ### ** Examples
    > 
    > set.seed(42)
    ...
    39.
    
    Done.
    > 
    > x <- summarize_envelope(cluster_env)
    > plot(x)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# ordbetareg

<details>

* Version: 0.7.2
* GitHub: https://github.com/saudiwin/ordbetareg_pack
* Source code: https://github.com/cran/ordbetareg
* Date/Publication: 2023-08-10 07:30:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "ordbetareg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ordbetareg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pp_check_ordbeta
    > ### Title: Accurate Posterior Predictive Plots for Ordbetareg Models
    > ### Aliases: pp_check_ordbeta
    > 
    > ### ** Examples
    > 
    > 
    ...
     16. │             └─ggplot2 (local) setup_params(...)
     17. │               └─ggplot2:::make_summary_fun(...)
     18. │                 └─rlang::as_function(fun.data)
     19. │                   └─base::get(x, envir = env, mode = "function")
     20. └─base::.handleSimpleError(...)
     21.   └─rlang (local) h(simpleError(msg, call))
     22.     └─handlers[[1L]](cnd)
     23.       └─cli::cli_abort(...)
     24.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 36 marked UTF-8 strings
    ```

# packcircles

<details>

* Version: 0.3.6
* GitHub: https://github.com/mbedward/packcircles
* Source code: https://github.com/cran/packcircles
* Date/Publication: 2023-09-08 06:30:02 UTC
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "packcircles")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘progressive_packing.Rmd’
      ...
    +     scale_fill_man .... [TRUNCATED] 
    
    > if (requireNamespace("ggiraph")) {
    +     gg <- ggplot(data = dat.gg) + ggiraph::geom_polygon_interactive(aes(x, 
    +         y, group = id, fill = fac .... [TRUNCATED] 
    Loading required namespace: ggiraph
    
      When sourcing ‘progressive_packing.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘graph_packing.Rmd’ using ‘UTF-8’... OK
      ‘intro.Rmd’ using ‘UTF-8’... OK
      ‘progressive_packing.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘graph_packing.Rmd’ using rmarkdown
    --- finished re-building ‘graph_packing.Rmd’
    
    --- re-building ‘intro.Rmd’ using rmarkdown
    ```

# pafr

<details>

* Version: 0.0.2
* GitHub: https://github.com/dwinter/pafr
* Source code: https://github.com/cran/pafr
* Date/Publication: 2020-12-08 10:20:12 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "pafr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pafr)
      Loading required package: ggplot2
      > 
      > test_check("pafr")
      [ FAIL 6 | WARN 2 | SKIP 0 | PASS 70 ]
      
    ...
      ── Failure ('test_plot.r:11:5'): dotplot works produces a plot ─────────────────
      unname(labs["xintercept"]) not equal to "xintercept".
      target is NULL, current is character
      ── Failure ('test_plot.r:12:5'): dotplot works produces a plot ─────────────────
      unname(labs["yintercept"]) not equal to "yintercept".
      target is NULL, current is character
      
      [ FAIL 6 | WARN 2 | SKIP 0 | PASS 70 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# patchwork

<details>

* Version: 1.2.0
* GitHub: https://github.com/thomasp85/patchwork
* Source code: https://github.com/cran/patchwork
* Date/Publication: 2024-01-08 14:40:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "patchwork")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘patchwork-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: free
    > ### Title: Free a plot from alignment
    > ### Aliases: free
    > 
    > ### ** Examples
    > 
    > # Sometimes you have a plot that defies good composition alginment, e.g. due
    ...
    > p1 / p2
    > 
    > # We can fix this be using free
    > free(p1) / p2
    > 
    > # We can still collect guides like before
    > free(p1) / p2 + plot_layout(guides = "collect")
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# pathviewr

<details>

* Version: 1.1.7
* GitHub: https://github.com/ropensci/pathviewr
* Source code: https://github.com/cran/pathviewr
* Date/Publication: 2023-03-08 08:10:05 UTC
* Number of recursive dependencies: 182

Run `revdepcheck::cloud_details(, "pathviewr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pathviewr)
      > #library(vdiffr)
      > 
      > test_check("pathviewr")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 286 ]
      
    ...
      ── Error ('test-plot_by_subject.R:168:3'): elev views wrangled correctly via tidyverse ──
      Error in `expect_match(elev_all_plots[[3]][[4]][["labels"]][["x"]], "position_height")`: is.character(act$val) is not TRUE
      Backtrace:
          ▆
       1. └─testthat::expect_match(...) at test-plot_by_subject.R:168:3
       2.   └─base::stopifnot(is.character(act$val))
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 286 ]
      Error: Test failures
      Execution halted
    ```

# patientProfilesVis

<details>

* Version: 2.0.9
* GitHub: https://github.com/openanalytics/patientProfilesVis
* Source code: https://github.com/cran/patientProfilesVis
* Date/Publication: 2024-06-18 09:00:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "patientProfilesVis")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘patientProfiles-template-SDTM.Rmd’
      ...
    > patientProfilesPlots <- c(patientProfilesPlots, list(MH = mhPlots))
    
    > cmPlots <- subjectProfileIntervalPlot(data = dataAll$CM, 
    +     paramVar = c("CMTRT", "CMDOSE", "CMDOSU", "CMROUTE", "CMDOSFRQ"), 
    +     timeStartVa .... [TRUNCATED] 
    171 record(s) with missing Study Day of Start of Medication and 208 record(s) with missing Study Day of End of Medication are imputed with minimal imputation.
    
    ...
    +     paramVar = "AETERM", timeStartVar = "AESTDY", timeEndVar = "AEENDY", 
    +     colorVar = " ..." ... [TRUNCATED] 
    3 record(s) with missing Study Day of Start of Adverse Event and 19 record(s) with missing Study Day of End of Adverse Event are imputed with minimal imputation.
    
      When sourcing ‘patientProfilesVis-introduction.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘patientProfiles-template-SDTM.Rmd’ using ‘UTF-8’... failed
      ‘patientProfilesVis-introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘patientProfiles-template-SDTM.Rmd’ using rmarkdown
    
    Quitting from lines 129-153 [patientProfiles-concomitantMedications] (patientProfiles-template-SDTM.Rmd)
    Error: processing vignette 'patientProfiles-template-SDTM.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘patientProfiles-template-SDTM.Rmd’
    
    --- re-building ‘patientProfilesVis-introduction.Rmd’ using rmarkdown
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(patientProfilesVis)
      > 
      > test_check("patientProfilesVis")
      [ FAIL 100 | WARN 0 | SKIP 20 | PASS 145 ]
      
      ══ Skipped tests (20) ══════════════════════════════════════════════════════════
    ...
       11.                 └─patientProfilesVis (local) .fun(piece, ...)
       12.                   └─ggplot2:::`+.gg`(...)
       13.                     └─ggplot2:::add_ggplot(e1, e2, e2name)
       14.                       ├─ggplot2::ggplot_add(object, p, objectname)
       15.                       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       16.                         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 100 | WARN 0 | SKIP 20 | PASS 145 ]
      Error: Test failures
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        doc   5.2Mb
    ```

# PCADSC

<details>

* Version: 0.8.0
* GitHub: https://github.com/annepetersen1/PCADSC
* Source code: https://github.com/cran/PCADSC
* Date/Publication: 2017-04-19 10:07:43 UTC
* Number of recursive dependencies: 35

Run `revdepcheck::cloud_details(, "PCADSC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PCADSC-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CEPlot
    > ### Title: Cumulative eigenvalue plot
    > ### Aliases: CEPlot
    > 
    > ### ** Examples
    > 
    > #load iris data
    ...
    > CEPlot(irisPCADSC_fast)
    Warning: The `guide` argument in `scale_*()` cannot be `FALSE`. This was deprecated in
    ggplot2 3.3.4.
    ℹ Please use "none" instead.
    ℹ The deprecated feature was likely used in the PCADSC package.
      Please report the issue at <https://github.com/annepetersen1/PCADSC/issues>.
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘pander’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘data.table’
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# pcutils

<details>

* Version: 0.2.6
* GitHub: https://github.com/Asa12138/pcutils
* Source code: https://github.com/cran/pcutils
* Date/Publication: 2024-06-25 21:20:05 UTC
* Number of recursive dependencies: 281

Run `revdepcheck::cloud_details(, "pcutils")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pcutils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gghist
    > ### Title: gg histogram
    > ### Aliases: gghist
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("ggpubr")) {
    ...
     12. │           └─ggplot2:::`+.gg`(...)
     13. │             └─ggplot2:::add_ggplot(e1, e2, e2name)
     14. │               ├─ggplot2::ggplot_add(object, p, objectname)
     15. │               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     16. │                 └─ggplot2:::new_layer_names(object, names(plot$layers))
     17. └─base::.handleSimpleError(...)
     18.   └─purrr (local) h(simpleError(msg, call))
     19.     └─cli::cli_abort(...)
     20.       └─rlang::abort(...)
    Execution halted
    ```

# pdxTrees

<details>

* Version: 0.4.0
* GitHub: https://github.com/mcconvil/pdxTrees
* Source code: https://github.com/cran/pdxTrees
* Date/Publication: 2020-08-17 14:00:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "pdxTrees")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘pdxTrees-vignette.Rmd’
      ...
    +     y = Pollution_Removal_value, color = Mature_Size)) + geom_point(size = 2, 
    +   .... [TRUNCATED] 
    
    > berkeley_graph + transition_states(states = Mature_Size, 
    +     transition_length = 10, state_length = 8) + enter_grow() + 
    +     exit_shrink()
    
      When sourcing ‘pdxTrees-vignette.R’:
    Error: argument "theme" is missing, with no default
    Execution halted
    
      ‘pdxTrees-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘pdxTrees-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# personalized

<details>

* Version: 0.2.7
* GitHub: https://github.com/jaredhuling/personalized
* Source code: https://github.com/cran/personalized
* Date/Publication: 2022-06-27 20:20:03 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "personalized")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > Sys.setenv("R_TESTS" = "")
      > library(testthat)
      > library(personalized)
      Loading required package: glmnet
      Loading required package: Matrix
      Loaded glmnet 4.1-8
      Loading required package: mgcv
    ...
       4. └─personalized:::plot.subgroup_validated(subgrp.val, type = "stability")
       5.   ├─plotly::subplot(...)
       6.   │ └─plotly:::dots2plots(...)
       7.   ├─plotly::ggplotly(p.primary, tooltip = paste0("tooltip", 1:4))
       8.   └─plotly:::ggplotly.ggplot(...)
       9.     └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 215 ]
      Error: Test failures
      Execution halted
    ```

# phyloseqGraphTest

<details>

* Version: 0.1.1
* GitHub: https://github.com/jfukuyama/phyloseqGraphTest
* Source code: https://github.com/cran/phyloseqGraphTest
* Date/Publication: 2024-02-05 19:00:02 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "phyloseqGraphTest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘phyloseqGraphTest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_test_network
    > ### Title: Plots the graph used for testing
    > ### Aliases: plot_test_network
    > 
    > ### ** Examples
    > 
    > library(phyloseq)
    ...
    > plot_test_network(gt)
    Warning in fortify(data, ...) :
      Arguments in `...` must be used.
    ✖ Problematic argument:
    • layout = layout
    ℹ Did you misspell an argument name?
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘gt_vignette.Rmd’
      ...
    > plot_test_network(gt)
    Warning in fortify(data, ...) :
      Arguments in `...` must be used.
    ✖ Problematic argument:
    • layout = layout
    ℹ Did you misspell an argument name?
    
      When sourcing ‘gt_vignette.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘gt_vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘gt_vignette.Rmd’ using rmarkdown
    
    Quitting from lines 175-176 [unnamed-chunk-5] (gt_vignette.Rmd)
    Error: processing vignette 'gt_vignette.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘gt_vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘gt_vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# PieGlyph

<details>

* Version: 1.0.0
* GitHub: https://github.com/rishvish/PieGlyph
* Source code: https://github.com/cran/PieGlyph
* Date/Publication: 2024-06-28 12:00:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "PieGlyph")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PieGlyph-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_pie_interactive
    > ### Title: Scatter plots with interactive pie-chart glyphs
    > ### Aliases: geom_pie_interactive
    > 
    > ### ** Examples
    > 
    > #' ## Load libraries
    ...
    > # One of the interactive aesthetics is tooltip. It is set that by default
    > # it shows the value and percentage of each slice in the pie-chart.
    > # Hover over any pie-chart in the plot to see this
    > plot_obj1 <- ggplot(data = plot_data, aes(x = system, y = response)) +
    +                geom_pie_interactive(slices = c("A", "B", "C", "D"),
    +                                     data = plot_data)+
    +                theme_classic()
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: +.gg ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

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
      • geom_pie_interactive/custom-tooltip.svg
      • geom_pie_interactive/data-id.svg
      • geom_pie_interactive/long-form-data-works.svg
      • geom_pie_interactive/multiple-interactive-parameters.svg
      • geom_pie_interactive/only-one-attribute.svg
      • pie-grob/pie-grob-with-multiple-values-no-fill-works.svg
      • pie-grob/pie-grob-with-single-non-zero-value-no-fill-works.svg
      • pie-grob/pie-grob-with-single-non-zero-value-works.svg
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PieGlyph.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘PieGlyph.Rmd’
      ...
    +     geom_pie_glyph(slices = "Attributes", values = "values") + 
    +     theme_cla .... [TRUNCATED] 
    
    > plot_obj <- ggplot(data = plot_data) + geom_pie_interactive(aes(x = system, 
    +     y = response, data_id = system), slices = c("A", "B", "C", 
    +     .... [TRUNCATED] 
    
    ...
      before plotting.
    Execution halted
    
      ‘PieGlyph.Rmd’ using ‘UTF-8’... failed
      ‘interactive-pie-glyphs.Rmd’ using ‘UTF-8’... failed
      ‘multinomial-classification-example.Rmd’ using ‘UTF-8’... OK
      ‘pie-lollipop-example.Rmd’ using ‘UTF-8’... OK
      ‘spatial-example.Rmd’ using ‘UTF-8’... OK
      ‘time-series-example.Rmd’ using ‘UTF-8’... failed
      ‘unusual-situations.Rmd’ using ‘UTF-8’... failed
    ```

# Plasmidprofiler

<details>

* Version: 0.1.6
* GitHub: NA
* Source code: https://github.com/cran/Plasmidprofiler
* Date/Publication: 2017-01-06 01:10:47
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "Plasmidprofiler")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Plasmidprofiler-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: main
    > ### Title: Main: Run everything
    > ### Aliases: main
    > 
    > ### ** Examples
    > 
    > main(blastdata,
    ...
    Saving 12 x 7 in image
    Warning: Vectorized input to `element_text()` is not officially supported.
    ℹ Results may be unexpected or may change in future versions of ggplot2.
    Warning in geom_tile(aes(x = Plasmid, y = Sample, label = AMR_gene, fill = Inc_group,  :
      Ignoring unknown aesthetics: label and text
    Warning: Use of `report$Sureness` is discouraged.
    ℹ Use `Sureness` instead.
    Error in pm[[2]] : subscript out of bounds
    Calls: main ... <Anonymous> -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# platetools

<details>

* Version: 0.1.7
* GitHub: https://github.com/swarchal/platetools
* Source code: https://github.com/cran/platetools
* Date/Publication: 2024-03-07 16:50:02 UTC
* Number of recursive dependencies: 48

Run `revdepcheck::cloud_details(, "platetools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(platetools)
      > 
      > test_check("platetools")
      [ FAIL 2 | WARN 1 | SKIP 4 | PASS 187 ]
      
      ══ Skipped tests (4) ═══════════════════════════════════════════════════════════
    ...
      length(out96) not equal to length(ggplot()).
      1/1 mismatches
      [1] 11 - 10 == 1
      ── Failure ('test-plot_wrapper.R:34:5'): returns expected ggplot object ────────
      names(out96) not equal to names(ggplot()).
      Lengths differ: 11 is not 10
      
      [ FAIL 2 | WARN 1 | SKIP 4 | PASS 187 ]
      Error: Test failures
      Execution halted
    ```

# PLNmodels

<details>

* Version: 1.2.0
* GitHub: https://github.com/pln-team/PLNmodels
* Source code: https://github.com/cran/PLNmodels
* Date/Publication: 2024-03-05 15:50:03 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "PLNmodels")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘PLNPCA.Rmd’
      ...
    3 "$contrib" "contributions of the individuals"
    
    > factoextra::fviz_pca_biplot(myPCA_ICL)
    
      When sourcing ‘PLNPCA.R’:
    Error: ℹ In index: 1.
    ℹ With name: y.
    ...
    ! argument is of length zero
    Execution halted
    
      ‘Import_data.Rmd’ using ‘UTF-8’... OK
      ‘PLN.Rmd’ using ‘UTF-8’... OK
      ‘PLNLDA.Rmd’ using ‘UTF-8’... OK
      ‘PLNPCA.Rmd’ using ‘UTF-8’... failed
      ‘PLNmixture.Rmd’ using ‘UTF-8’... failed
      ‘PLNnetwork.Rmd’ using ‘UTF-8’... OK
      ‘Trichoptera.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Import_data.Rmd’ using rmarkdown
    --- finished re-building ‘Import_data.Rmd’
    
    --- re-building ‘PLN.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 26.0Mb
      sub-directories of 1Mb or more:
        R      1.0Mb
        data   3.5Mb
        doc    2.1Mb
        libs  18.6Mb
    ```

# plotBart

<details>

* Version: 0.1.7
* GitHub: https://github.com/priism-center/plotBart
* Source code: https://github.com/cran/plotBart
* Date/Publication: 2022-05-27 07:50:06 UTC
* Number of recursive dependencies: 94

Run `revdepcheck::cloud_details(, "plotBart")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(vdiffr)
      > library(plotBart)
      Loading required package: bartCause
      Loading required package: ggplot2
      > 
      > test_check("plotBart") # run tests
    ...
      • plots/mod-search.svg
      • plots/overlappscoresdensity.svg
      • plots/overlapvarsdensity.svg
      • plots/pate.svg
      • plots/sate.svg
      • plots/supportchi.svg
      • plots/supportsd.svg
      • plots/waterfall2.svg
      Error: Test failures
      Execution halted
    ```

# plotDK

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/plotDK
* Date/Publication: 2021-10-01 08:00:02 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "plotDK")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(plotDK)
      > 
      > test_check("plotDK")
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 46 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      Error in `expect_setequal(c("x", "y", "group", "subgroup", "text", "fill"), 
          names(labels))`: `object` and `expected` must both be vectors
      Backtrace:
          ▆
       1. └─testthat::expect_setequal(c("x", "y", "group", "subgroup", "text", "fill"), names(labels)) at test-plotDK.R:67:5
       2.   └─rlang::abort("`object` and `expected` must both be vectors")
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 46 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mapproj’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 12992 marked UTF-8 strings
    ```

# plotly

<details>

* Version: 4.10.4
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2024-01-13 22:40:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘plotly-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: style
    > ### Title: Modify trace(s)
    > ### Aliases: style
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    + # this clobbers the previously supplied marker.line.color
    + style(p, marker.line = list(width = 2.5), marker.size = 10)
    + ## Don't show: 
    + }) # examplesIf
    > (p <- ggplotly(qplot(data = mtcars, wt, mpg, geom = c("point", "smooth"))))
    Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
    Error in pm[[2]] : subscript out of bounds
    Calls: <Anonymous> ... eval -> eval -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("plotly")
      Loading required package: ggplot2
      
      Attaching package: 'plotly'
      
      The following object is masked from 'package:ggplot2':
    ...
      • plotly-subplot/subplot-bump-axis-annotation.svg
      • plotly-subplot/subplot-bump-axis-image.svg
      • plotly-subplot/subplot-bump-axis-shape-shared.svg
      • plotly-subplot/subplot-bump-axis-shape.svg
      • plotly-subplot/subplot-reposition-annotation.svg
      • plotly-subplot/subplot-reposition-image.svg
      • plotly-subplot/subplot-reposition-shape-fixed.svg
      • plotly-subplot/subplot-reposition-shape.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R             1.0Mb
        htmlwidgets   4.0Mb
    ```

# pmartR

<details>

* Version: 2.4.5
* GitHub: https://github.com/pmartR/pmartR
* Source code: https://github.com/cran/pmartR
* Date/Publication: 2024-05-21 15:50:02 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "pmartR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pmartR)
      > 
      > test_check("pmartR")
      [ FAIL 1 | WARN 1 | SKIP 11 | PASS 2375 ]
      
      ══ Skipped tests (11) ══════════════════════════════════════════════════════════
    ...
      • plots/plot-spansres-color-high-color-low.svg
      • plots/plot-spansres.svg
      • plots/plot-statres-anova-volcano.svg
      • plots/plot-statres-anova.svg
      • plots/plot-statres-combined-volcano.svg
      • plots/plot-statres-combined.svg
      • plots/plot-statres-gtest.svg
      • plots/plot-totalcountfilt.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.8Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        help   1.5Mb
        libs   6.7Mb
    ```

# pmxTools

<details>

* Version: 1.3
* GitHub: https://github.com/kestrel99/pmxTools
* Source code: https://github.com/cran/pmxTools
* Date/Publication: 2023-02-21 16:00:08 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "pmxTools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pmxTools)
      Loading required package: patchwork
      > 
      > test_check("pmxTools")
      [ FAIL 1 | WARN 1 | SKIP 12 | PASS 110 ]
      
    ...
       24.     └─handlers[[1L]](cnd)
       25.       └─cli::cli_abort(...)
       26.         └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 12 | PASS 110 ]
      Deleting unused snapshots:
      • plot/conditioned-distplot.svg
      • plot/perc.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘DiagrammeR’
    ```

# posterior

<details>

* Version: 1.6.0
* GitHub: https://github.com/stan-dev/posterior
* Source code: https://github.com/cran/posterior
* Date/Publication: 2024-07-03 23:00:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "posterior")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘pareto_diagnostics.Rmd’ using rmarkdown
    --- finished re-building ‘pareto_diagnostics.Rmd’
    
    --- re-building ‘posterior.Rmd’ using rmarkdown
    --- finished re-building ‘posterior.Rmd’
    
    --- re-building ‘rvar.Rmd’ using rmarkdown
    
    Quitting from lines 530-533 [mixture] (rvar.Rmd)
    ...
        NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), NULL, 2, NULL, NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list("grey92", NA, NULL, NULL, TRUE), list(), NULL, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, 
            "white", TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, NULL, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list("grey85", 
            NA, NULL, NULL, TRUE), NULL, NULL, "on", "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    --- failed re-building ‘rvar.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘rvar.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘rvar.Rmd’
      ...
    > y
    rvar<4000>[3] mean ± sd:
    [1] 3.00 ± 1.00  2.02 ± 0.99  0.96 ± 0.99 
    
    > X + y
    
      When sourcing ‘rvar.R’:
    Error: Cannot broadcast array of shape [4000,3,1] to array of shape [4000,4,3]:
    All dimensions must be 1 or equal.
    Execution halted
    
      ‘pareto_diagnostics.Rmd’ using ‘UTF-8’... OK
      ‘posterior.Rmd’ using ‘UTF-8’... OK
      ‘rvar.Rmd’ using ‘UTF-8’... failed
    ```

# PPQplan

<details>

* Version: 1.1.0
* GitHub: https://github.com/allenzhuaz/PPQplan
* Source code: https://github.com/cran/PPQplan
* Date/Publication: 2020-10-08 04:30:06 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "PPQplan")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PPQnote.Rmd’ using rmarkdown
    --- finished re-building ‘PPQnote.Rmd’
    
    --- re-building ‘PPQplan-vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘PPQplan-vignette.Rmd’
      ...
    
    > devtools::load_all()
    
      When sourcing ‘PPQplan-vignette.R’:
    Error: Could not find a root 'DESCRIPTION' file that starts with '^Package' in
    '/tmp/RtmpeoIsxA/file11156538c652/vignettes'.
    ℹ Are you in your project directory and does your project have a 'DESCRIPTION'
      file?
    Execution halted
    
      ‘PPQnote.Rmd’ using ‘UTF-8’... OK
      ‘PPQplan-vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.1Mb
      sub-directories of 1Mb or more:
        doc  12.0Mb
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# ppseq

<details>

* Version: 0.2.5
* GitHub: https://github.com/zabore/ppseq
* Source code: https://github.com/cran/ppseq
* Date/Publication: 2024-09-04 22:20:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "ppseq")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘one_sample_expansion.Rmd’
      ...
      
    </table>
    </div>
    
    > ptest <- plot(one_sample_cal_tbl, type1_range = c(0.05, 
    +     0.1), minimum_power = 0.7, plotly = TRUE)
    
    ...
    
    > ptest <- plot(two_sample_cal_tbl, type1_range = c(0.05, 
    +     0.1), minimum_power = 0.7, plotly = TRUE)
    
      When sourcing ‘two_sample_randomized.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘one_sample_expansion.Rmd’ using ‘UTF-8’... failed
      ‘two_sample_randomized.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘one_sample_expansion.Rmd’ using rmarkdown
    
    Quitting from lines 183-188 [unnamed-chunk-13] (one_sample_expansion.Rmd)
    Error: processing vignette 'one_sample_expansion.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘one_sample_expansion.Rmd’
    
    --- re-building ‘two_sample_randomized.Rmd’ using rmarkdown
    ...
    Quitting from lines 179-184 [unnamed-chunk-13] (two_sample_randomized.Rmd)
    Error: processing vignette 'two_sample_randomized.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘two_sample_randomized.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘one_sample_expansion.Rmd’ ‘two_sample_randomized.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc  10.5Mb
    ```

# PPtreeregViz

<details>

* Version: 2.0.5
* GitHub: https://github.com/sunsmiling/PPtreeregViz
* Source code: https://github.com/cran/PPtreeregViz
* Date/Publication: 2022-12-23 19:20:05 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "PPtreeregViz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PPtreeregViz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: PPregNodeViz
    > ### Title: Node visualization
    > ### Aliases: PPregNodeViz
    > ### Keywords: tree
    > 
    > ### ** Examples
    > 
    > data(dataXY)
    > Model <- PPTreereg(Y~., data = dataXY, DEPTH = 2)
    > PPregNodeViz(Model,node.id=1)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: PPregNodeViz ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘PPtreeregViz.Rmd’
      ...
    
    > plot(Tree.Imp)
    
    > plot(Tree.Imp, marginal = TRUE, num_var = 5)
    
    > PPregNodeViz(Model, node.id = 1)
    
      When sourcing ‘PPtreeregViz.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘PPtreeregViz.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘PPtreeregViz.Rmd’ using rmarkdown
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

# precintcon

<details>

* Version: 2.3.0
* GitHub: https://github.com/lucasvenez/precintcon
* Source code: https://github.com/cran/precintcon
* Date/Publication: 2016-07-17 13:49:19
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "precintcon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘precintcon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pplot.deciles
    > ### Title: Plot deciles
    > ### Aliases: pplot.deciles precintcon.plot.deciles
    > ### Keywords: deciles precipitation
    > 
    > ### ** Examples
    > 
    ...
      9. │           └─layout$setup(data, plot$data, plot$plot_env)
     10. │             └─ggplot2 (local) setup(..., self = self)
     11. │               └─base::lapply(...)
     12. │                 └─ggplot2 (local) FUN(X[[i]], ...)
     13. │                   └─ggplot2::map_data(...)
     14. │                     └─vctrs::vec_slice(data, facet_vals$.index)
     15. └─vctrs:::stop_scalar_type(`<fn>`(`<df[,11]>`), "x", `<env>`)
     16.   └─vctrs:::stop_vctrs(...)
     17.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = call)
    Execution halted
    ```

# precrec

<details>

* Version: 0.14.4
* GitHub: https://github.com/evalclass/precrec
* Source code: https://github.com/cran/precrec
* Date/Publication: 2023-10-11 22:10:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "precrec")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    > autoplot(mscurves)
    Warning in ggplot2::fortify(object, raw_curves = raw_curves, reduce_points = reduce_points) :
      Arguments in `...` must be used.
    ✖ Problematic argument:
    • raw_curves = raw_curves
    ℹ Did you misspell an argument name?
    
      When sourcing ‘introduction.R’:
    Error: object is not a unit
    Execution halted
    
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        libs   4.3Mb
    ```

# priorsense

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/priorsense
* Date/Publication: 2024-07-16 10:30:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "priorsense")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘priorsense-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: powerscale_plots
    > ### Title: Diagnostic plots for power-scaling sensitivity
    > ### Aliases: powerscale_plots powerscale_plot_dens powerscale_plot_ecdf
    > ###   powerscale_plot_ecdf.powerscaled_sequence powerscale_plot_quantities
    > ###   powerscale_plot_quantities.powerscaled_sequence
    > 
    > ### ** Examples
    > 
    > ex <- example_powerscale_model()
    > 
    > powerscale_plot_dens(ex$draws)
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL,
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘powerscaling.Rmd’
      ...
      <chr>    <dbl>      <dbl> <chr>              
    1 mu       0.435      0.644 prior-data conflict
    2 sigma    0.361      0.677 prior-data conflict
    
    > powerscale_plot_dens(fit, variable = "mu", facet_rows = "variable")
    
      When sourcing ‘powerscaling.R’:
    Error: unused argument (theme = list(list("black", 0.545454545454545, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.545454545454545, 1, TRUE), list("sans", "plain", "black", 12, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.545454545454545, 0.545454545454545, 1, 1, "sans", 4.21751764217518, 1.63636363636364, 19, TRUE), 6, c(6, 6, 6, 6), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, 
        NULL, c(3, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 3, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 3), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.4, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NU
    Execution halted
    
      ‘powerscaling.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘powerscaling.Rmd’ using rmarkdown
    
    Quitting from lines 124-125 [unnamed-chunk-7] (powerscaling.Rmd)
    Error: processing vignette 'powerscaling.Rmd' failed with diagnostics:
    unused argument (theme = list(list("black", 0.545454545454545, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.545454545454545, 1, TRUE), list("sans", "plain", "black", 12, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.545454545454545, 0.545454545454545, 1, 1, "sans", 4.21751764217518, 1.63636363636364, 19, TRUE), 6, c(6, 6, 6, 6), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, 
        NULL, c(3, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 3, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 3), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.4, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.4, 0), 
        NULL, TRUE), NULL, list(), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.4), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.4, 0, 2.4), NULL, TRUE), list("grey20", 0.3, NULL, NULL, FALSE, "grey20", FALSE), NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, 0.4, NULL, NULL, FALSE, NULL, FALSE), 
        NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, list(), NULL, 2, NULL, NULL, list(), 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 13, 0, NULL, NULL, NULL, NULL, NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list(), list(), 1.5, NULL, NULL, list(), NULL, list(NULL, 0.5, NULL, NULL, FALSE, NULL, TRUE), NULL, NULL, 
        NULL, NULL, FALSE, list(), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 6, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 6, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(6, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", NULL, NULL, list(), NULL, NULL, "on", "outside", list(NULL, NULL, "grey10", 0.9, NULL, NULL, NULL, NULL, c(4.8, 4.8, 4.8, 4.8), NULL, FALSE), NULL, NULL, 
        NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 3, 3, list(), list(NULL, NULL, NULL, NULL, FALSE, NULL, TRUE), list(NULL, NULL, NULL, NULL, FALSE, NULL, TRUE), list(NULL, NULL, NULL, NULL, FALSE, NULL, TRUE), list(NULL, NULL, NULL, NULL, 0.5, 0.5, 0, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), 0.666666666666667, 0.333333333333333))
    --- failed re-building ‘powerscaling.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘powerscaling.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# probably

<details>

* Version: 1.0.3
* GitHub: https://github.com/tidymodels/probably
* Source code: https://github.com/cran/probably
* Date/Publication: 2024-02-23 03:20:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "probably")` for more info

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
      
      `actual$ymin` is absent
      `expected$ymin` is a character vector ('lower')
      
      `actual$ymax` is absent
      `expected$ymax` is a character vector ('upper')
      
      [ FAIL 2 | WARN 0 | SKIP 46 | PASS 466 ]
      Error: Test failures
      Execution halted
    ```

# processmapR

<details>

* Version: 0.5.5
* GitHub: https://github.com/bupaverse/processmapr
* Source code: https://github.com/cran/processmapR
* Date/Publication: 2024-08-30 13:10:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "processmapR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(processmapR)
      
      Attaching package: 'processmapR'
      
      The following object is masked from 'package:stats':
      
    ...
       10.   └─processmapR:::return_plotly(p, plotly)
       11.     ├─plotly::ggplotly(p)
       12.     └─plotly:::ggplotly.ggplot(p)
       13.       └─plotly::gg2list(...)
      ── Failure ('test_trace_explorer.R:240:3'): test trace_explorer on eventlog with param `plotly` ──
      `chart` inherits from 'gg'/'ggplot' not 'plotly'.
      
      [ FAIL 6 | WARN 0 | SKIP 10 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

# projpred

<details>

* Version: 2.8.0
* GitHub: https://github.com/stan-dev/projpred
* Source code: https://github.com/cran/projpred
* Date/Publication: 2023-12-15 00:00:02 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "projpred")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘projpred-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as.matrix.projection
    > ### Title: Extract projected parameter draws and coerce to matrix
    > ### Aliases: as.matrix.projection
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    2 X1           1.70  0.155  1.40    1.97 
    3 X3           0.924 0.266  0.560   1.45 
    4 X5          -1.21  0.111 -1.53   -0.602
    5 sigma        2.01  0.183  1.83    2.35 
    > if (requireNamespace("bayesplot", quietly = TRUE)) {
    +     print(bayesplot::mcmc_intervals(prj_mat))
    + }
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘projpred.Rmd’
      ...
    
    
    > bayesplot_theme_set(ggplot2::theme_bw())
    
    > mcmc_intervals(prj_mat) + ggplot2::coord_cartesian(xlim = c(-1.5, 
    +     1.6))
    
      When sourcing ‘projpred.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘latent.Rmd’ using ‘UTF-8’... OK
      ‘projpred.Rmd’ using ‘UTF-8’... failed
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(projpred)
      This is projpred version 2.8.0. NOTE: In projpred 2.7.0, the default search method was set to "forward" (for all kinds of models).
      > 
      > test_check("projpred")
      Warning in check_dep_version() :
        ABI version mismatch: 
    ...
       41. │                                                           └─methods (local) `<rfMthdDf>`(...)
       42. │                                                             └─methods::new(def, ...)
       43. │                                                               ├─methods::initialize(value, ...)
       44. │                                                               └─methods::initialize(value, ...)
       45. │                                                                 └─.Object$initialize(...)
       46. │                                                                   └─lme4 (local) initializePtr()
       47. └─base::.handleSimpleError(...)
       48.   └─testthat (local) h(simpleError(msg, call))
       49.     └─rlang::abort(...)
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        doc    1.4Mb
        libs   3.1Mb
    ```

# psborrow

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/psborrow
* Date/Publication: 2023-03-03 10:30:07 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "psborrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(psborrow)
      > 
      > test_check("psborrow")
      [ FAIL 10 | WARN 0 | SKIP 1 | PASS 142 ]
      
      ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
    ...
      `expected` is a character vector ('ref')
      ── Failure ('test-plots.R:126:5'): Ensure output is producing a ggplot2 object with appropriate parameters ──
      p1$labels$yintercept (`actual`) not equal to "ref" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('ref')
      
      [ FAIL 10 | WARN 0 | SKIP 1 | PASS 142 ]
      Error: Test failures
      Execution halted
    ```

# pubh

<details>

* Version: 1.3.7
* GitHub: https://github.com/josie-athens/pubh
* Source code: https://github.com/cran/pubh
* Date/Publication: 2024-08-17 02:20:02 UTC
* Number of recursive dependencies: 187

Run `revdepcheck::cloud_details(, "pubh")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
            ───────────────────────────────────────────────────────────────
              R2 = 0.123                                                   
    
    Column names: Parameter, Coefficient, Pr(>|t|)
    
    > multiple(model_bwt, ~race)$df
    
    ...
    > gf_labs(gf_facet_wrap(gf_errorbar(gf_point(estimate_means(model_norm, 
    +     c("race", "smoke")), Mean ~ race), CI_low + CI_high ~ race, 
    +     widt .... [TRUNCATED] 
    
      When sourcing ‘regression.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘introduction.Rmd’ using ‘UTF-8’... failed
      ‘regression.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘introduction.Rmd’ using rmarkdown
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘pubh-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Kirkwood
    > ### Title: Body weight and plasma volume.
    > ### Aliases: Kirkwood
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > data(Kirkwood)
    > 
    > Kirkwood |>
    +   gf_point(volume ~ weight) |>
    +   gf_lm(col = "indianred3", interval = "confidence", fill = "indianred3")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: gf_lm ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# PUPMSI

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PUPMSI
* Date/Publication: 2022-05-31 10:50:04 UTC
* Number of recursive dependencies: 32

Run `revdepcheck::cloud_details(, "PUPMSI")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PUPMSI-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BradleyMSI
    > ### Title: Bradley Moisture Sorption Isotherm
    > ### Aliases: BradleyMSI
    > 
    > ### ** Examples
    > 
    > WaterAct <- c(0.1145,0.2274,0.3265,0.4291,0.6342,0.7385,0.8274,0.9573)
    ...
     Adsorption 0.002148893 0.001891231 4.617743e-06 0.04298251 7.517135e-18
     Desorption 0.004371350 0.003905913 1.910870e-05 0.10015162 3.122502e-17
    Constants of Bradley Sorption Model
     Isotherm   k1           k2      
     Adsorption 1.752876e-10 3.525154
     Desorption 3.920525e-12 8.480898
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: BradleyMSI -> BradleyMSIPlot
    Execution halted
    ```

# qacBase

<details>

* Version: 1.0.3
* GitHub: https://github.com/rkabacoff/qacBase
* Source code: https://github.com/cran/qacBase
* Date/Publication: 2022-02-09 22:20:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "qacBase")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qacBase-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scatter
    > ### Title: Scatterplot
    > ### Aliases: scatter
    > 
    > ### ** Examples
    > 
    > scatter(cars74, hp, mpg)
    > scatter(cars74, wt, hp)
    > p <- scatter(ggplot2::mpg, displ, hwy,
    +         margin="histogram",
    +         title="Engine Displacement vs. Highway Mileage")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: scatter ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# qPCRhelper

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/qPCRhelper
* Date/Publication: 2023-02-23 14:00:08 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "qPCRhelper")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qPCRhelper-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qPCRhelper
    > ### Title: qPCRhelper
    > ### Aliases: qPCRhelper
    > 
    > ### ** Examples
    > 
    > ## Create sample table with expected 'Sample', 'Group', and gene Ct columns
    ...
     11. │           └─ggplot2:::`+.gg`(...)
     12. │             └─ggplot2:::add_ggplot(e1, e2, e2name)
     13. │               ├─ggplot2::ggplot_add(object, p, objectname)
     14. │               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     15. │                 └─ggplot2:::new_layer_names(object, names(plot$layers))
     16. └─base::.handleSimpleError(...)
     17.   └─purrr (local) h(simpleError(msg, call))
     18.     └─cli::cli_abort(...)
     19.       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘qPCRhelper.Rmd’
      ...
    +     ref.gene = "GAPDH", ref.group = "Control")
    Warning in (function (mapping = NULL, data = NULL, geom = "boxplot", position = "dodge2",  :
      Ignoring unknown aesthetics: fill
    
      When sourcing ‘qPCRhelper.R’:
    Error: ℹ In index: 1.
    ℹ With name: log2RelExp.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    Execution halted
    
      ‘qPCRhelper.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘qPCRhelper.Rmd’ using rmarkdown
    
    Quitting from lines 18-29 [setup] (qPCRhelper.Rmd)
    Error: processing vignette 'qPCRhelper.Rmd' failed with diagnostics:
    ℹ In index: 1.
    ℹ With name: log2RelExp.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    --- failed re-building ‘qPCRhelper.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘qPCRhelper.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# r2dii.plot

<details>

* Version: 0.4.0
* GitHub: https://github.com/RMI-PACTA/r2dii.plot
* Source code: https://github.com/cran/r2dii.plot
* Date/Publication: 2024-02-29 16:40:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "r2dii.plot")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(r2dii.plot)
      > 
      > test_check("r2dii.plot")
      Scale for colour is already present.
      Adding another scale for colour, which will replace the existing scale.
    ...
      `expected` is a character vector ('year')
      ── Failure ('test-plot_trajectory.R:41:3'): outputs default axis labels ────────
      p$labels$y (`actual`) not equal to "value" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('value')
      
      [ FAIL 2 | WARN 2 | SKIP 40 | PASS 122 ]
      Error: Test failures
      Execution halted
    ```

# r2spss

<details>

* Version: 0.3.2
* GitHub: https://github.com/aalfons/r2spss
* Source code: https://github.com/cran/r2spss
* Date/Publication: 2022-05-25 11:00:08 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "r2spss")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘r2spss-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ANOVA
    > ### Title: One-way and Two-way ANOVA
    > ### Aliases: ANOVA to_SPSS.ANOVA_SPSS print.ANOVA_SPSS plot.ANOVA_SPSS
    > ### Keywords: htest
    > 
    > ### ** Examples
    > 
    ...
    \arrayrulecolor{darkgraySPSS}\hline
     Total & 495.474 & 416 &  &  &  \\
    \arrayrulecolor{black}\hline
    \noalign{\smallskip}
    \end{NiceTabular}
    
    > plot(oneway)  # create profile plot
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘r2spss-intro.Rnw’
      ...
     Valid N (listwise) & 417 &  &  &  &  \\
    \hline
    \noalign{\smallskip}
    \end{tabular}
    
    > histogram(Eredivisie, "logMarketValue")
    
      When sourcing 'r2spss-intro.R':
    Error: argument is of length zero
    Execution halted
    
      ‘r2spss-intro.Rnw’ using ‘UTF-8’... failed
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘r2spss-intro.Rnw’ using knitr
    
    Quitting from lines 327-328 [unnamed-chunk-8] (r2spss-intro.Rnw)
    Error: processing vignette 'r2spss-intro.Rnw' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘r2spss-intro.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘r2spss-intro.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# radiant.basics

<details>

* Version: 1.6.6
* GitHub: https://github.com/radiant-rstats/radiant.basics
* Source code: https://github.com/cran/radiant.basics
* Date/Publication: 2024-05-15 04:30:07 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "radiant.basics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.basics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.clt
    > ### Title: Plot method for the Central Limit Theorem simulation
    > ### Aliases: plot.clt
    > 
    > ### ** Examples
    > 
    > clt("Uniform", 100, 100, unif_min = 10, unif_max = 20) %>% plot()
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: %>% ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# radiant.data

<details>

* Version: 1.6.6
* GitHub: https://github.com/radiant-rstats/radiant.data
* Source code: https://github.com/cran/radiant.data
* Date/Publication: 2024-05-14 23:30:02 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "radiant.data")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.data-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: visualize
    > ### Title: Visualize data using ggplot2 <https://ggplot2.tidyverse.org/>
    > ### Aliases: visualize
    > 
    > ### ** Examples
    > 
    > visualize(diamonds, "price:cut", type = "dist", fillcol = "red")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: visualize ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# radiant.model

<details>

* Version: 1.6.6
* GitHub: https://github.com/radiant-rstats/radiant.model
* Source code: https://github.com/cran/radiant.model
* Date/Publication: 2024-05-15 09:10:08 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::cloud_details(, "radiant.model")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘radiant.model-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.simulater
    > ### Title: Plot method for the simulater function
    > ### Aliases: plot.simulater
    > 
    > ### ** Examples
    > 
    > simdat <- simulater(
    ...
    +   const = "cost 3",
    +   norm = "demand 2000 1000",
    +   discrete = "price 5 8 .3 .7",
    +   form = "profit = demand * (price - cost)",
    +   seed = 1234
    + )
    > plot(simdat, bins = 25)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# Radviz

<details>

* Version: 0.9.3
* GitHub: https://github.com/yannabraham/Radviz
* Source code: https://github.com/cran/Radviz
* Date/Publication: 2022-03-25 18:10:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "Radviz")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Radviz-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Radviz
    > ### Title: Radviz Projection of Multidimensional Data
    > ### Aliases: Radviz
    > 
    > ### ** Examples
    > 
    > data(iris)
    > das <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
    > S <- make.S(das)
    > rv <- do.radviz(iris,S)
    > plot(rv,anchors.only=FALSE)
    Error in plot.radviz(rv, anchors.only = FALSE) : 
      'language' object cannot be coerced to type 'double'
    Calls: plot -> plot.radviz
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘multivariate_analysis.Rmd’
      ...
    
    > classic.S <- make.S(get.optim(classic.optim))
    
    > btcells.rv <- do.radviz(btcells.df, classic.S)
    
    > plot(btcells.rv) + geom_point(aes(color = Treatment))
    
    ...
    [1] 15792    18
    
    > ct.rv
    
      When sourcing ‘single_cell_projections.R’:
    Error: 'language' object cannot be coerced to type 'double'
    Execution halted
    
      ‘multivariate_analysis.Rmd’ using ‘UTF-8’... failed
      ‘single_cell_projections.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘multivariate_analysis.Rmd’ using rmarkdown
    ```

# randomForestExplainer

<details>

* Version: 0.10.1
* GitHub: https://github.com/ModelOriented/randomForestExplainer
* Source code: https://github.com/cran/randomForestExplainer
* Date/Publication: 2020-07-11 20:30:02 UTC
* Number of recursive dependencies: 105

Run `revdepcheck::cloud_details(, "randomForestExplainer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘randomForestExplainer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_min_depth_interactions
    > ### Title: Plot the top mean conditional minimal depth
    > ### Aliases: plot_min_depth_interactions
    > 
    > ### ** Examples
    > 
    > forest <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100)
    ...
    The first warning was:
    ℹ In argument: `Petal.Width = min(Petal.Width, na.rm = TRUE)`.
    ℹ In group 1: `tree = 1` and `split var = "Petal.Length"`.
    Caused by warning in `min()`:
    ! no non-missing arguments to min; returning Inf
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 203 remaining warnings.
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(randomForestExplainer)
      > 
      > test_check("randomForestExplainer")
      [ FAIL 6 | WARN 70 | SKIP 0 | PASS 55 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       24. │ └─grid:::grid.draw.grob(x$children[[i]], recording = FALSE)
       25. │   └─grDevices::recordGraphics(drawGrob(x), list(x = x), getNamespace("grid"))
       26. └─grid:::drawGrob(x)
       27.   ├─grid::drawDetails(x, recording = FALSE)
       28.   └─grid:::drawDetails.segments(x, recording = FALSE)
       29.     └─grid:::grid.Call.graphics(...)
      
      [ FAIL 6 | WARN 70 | SKIP 0 | PASS 55 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘randomForestExplainer.Rmd’
      ...
    3                  3.308
    23                 2.600
    33                 1.288
    8                  3.512
    
    > plot_min_depth_interactions(interactions_frame)
    
      When sourcing ‘randomForestExplainer.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘randomForestExplainer.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘randomForestExplainer.Rmd’ using rmarkdown
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# rassta

<details>

* Version: 1.0.6
* GitHub: https://github.com/bafuentes/rassta
* Source code: https://github.com/cran/rassta
* Date/Publication: 2024-08-19 06:20:13 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "rassta")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rassta-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: select_functions
    > ### Title: Select Constrained Univariate Distribution Functions
    > ### Aliases: select_functions
    > 
    > ### ** Examples
    > 
    > require(terra)
    ...
    > tvars <- terra::rast(tf)
    > # Single-layer SpatRaster of topographic classification units
    > ## 5 classification units
    > tcf <- list.files(path = p, pattern = "topography.tif", full.names = TRUE)
    > tcu <- terra::rast(tcf)
    > # Automatic selection of distribution functions
    > tdif <- select_functions(cu.rast = tcu, var.rast = tvars, fun = mean)
    Error in pm[[2]] : subscript out of bounds
    Calls: select_functions -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("rassta")
      + }
      
      Attaching package: 'rassta'
      
    ...
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    
      test_select_functions.R.......    0 tests    Error in pm[[2]] : subscript out of bounds
      Calls: <Anonymous> ... select_functions -> <Anonymous> -> ggplotly.ggplot -> gg2list
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘signature.Rmd’
      ...
    > clim.var <- rast(vardir)
    
    > clim.cu <- rast(paste(d, "/climate.tif", sep = ""))
    
    > clim.difun <- select_functions(cu.rast = clim.cu, 
    +     var.rast = clim.var, mode = "auto")
    
    ...
      When sourcing ‘signature.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘classunits.Rmd’ using ‘UTF-8’... OK
      ‘modeling.Rmd’ using ‘UTF-8’... OK
      ‘sampling.Rmd’ using ‘UTF-8’... OK
      ‘signature.Rmd’ using ‘UTF-8’... failed
      ‘similarity.Rmd’ using ‘UTF-8’... OK
      ‘stratunits.Rmd’ using ‘UTF-8’... OK
    ```

# rater

<details>

* Version: 1.3.1
* GitHub: https://github.com/jeffreypullin/rater
* Source code: https://github.com/cran/rater
* Date/Publication: 2023-09-11 17:40:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "rater")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rater)
      * The rater package uses `Stan` to fit bayesian models.
      * If you are working on a local, multicore CPU with excess RAM please call:
      * options(mc.cores = parallel::detectCores())
      * This will allow Stan to run inference on multiple cores in parallel.
      > 
    ...
      ── Failure ('test_plotting.R:65:3'): plot_theta_points output has correct type ──
      get_geoms(ds_plot) (`actual`) not equal to c("GeomPoint", "GeomErrorbar") (`expected`).
      
      `names(actual)` is a character vector ('geom_point', 'geom_errorbar')
      `names(expected)` is absent
      
      [ FAIL 4 | WARN 0 | SKIP 6 | PASS 375 ]
      Error: Test failures
      In addition: There were 14 warnings (use warnings() to see them)
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 128.0Mb
      sub-directories of 1Mb or more:
        libs  127.3Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# RBesT

<details>

* Version: 1.7-3
* GitHub: https://github.com/Novartis/RBesT
* Source code: https://github.com/cran/RBesT
* Date/Publication: 2024-01-08 15:20:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "RBesT")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RBesT-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: forest_plot
    > ### Title: Forest Plot
    > ### Aliases: forest_plot
    > 
    > ### ** Examples
    > 
    > # we consider the example AS MAP analysis
    ...
    Please consider increasing the MCMC simulation size.
    
    AS> ## Recover user set sampling defaults
    AS> options(.user_mc_options)
    > 
    > # default forest plot for a gMAP analysis
    > forest_plot(map_AS)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: forest_plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    
    MAP Prior MCMC sample
      mean     sd   2.5%    50%  97.5% 
    0.2580 0.0842 0.1120 0.2510 0.4590 
    
    > pl <- plot(map_mcmc)
    
      When sourcing ‘introduction.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘introduction.Rmd’ using rmarkdown
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    Warning in hook_png(..., cmd = "pngquant", post_process = function(x) { :
      cannot find pngquant; please install and put it in PATH
    
    Quitting from lines 104-126 [unnamed-chunk-4] (introduction.Rmd)
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘introduction.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘introduction.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 97.6Mb
      sub-directories of 1Mb or more:
        libs  95.7Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# rddensity

<details>

* Version: 2.5
* GitHub: NA
* Source code: https://github.com/cran/rddensity
* Date/Publication: 2024-01-22 21:10:07 UTC
* Number of recursive dependencies: 29

Run `revdepcheck::cloud_details(, "rddensity")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rddensity-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rddensity
    > ### Title: Manipulation Testing Using Local Polynomial Density Estimation
    > ### Aliases: rddensity
    > 
    > ### ** Examples
    > 
    > ### Continuous Density
    ...
    sum densities       0.6532              3.9572              0.0042              
    
    > 
    > ### Plotting using rdplotdensity()
    > # 1. From -2 to 2 with 25 evaluation points at each side
    > plot1 <- rdplotdensity(rdd, x, plotRange = c(-2, 2), plotN = 25)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: rdplotdensity ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# RecordTest

<details>

* Version: 2.2.0
* GitHub: https://github.com/JorgeCastilloMateo/RecordTest
* Source code: https://github.com/cran/RecordTest
* Date/Publication: 2023-08-07 20:20:08 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "RecordTest")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RecordTest-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: N.plot
    > ### Title: Number of Records Plot
    > ### Aliases: N.plot
    > 
    > ### ** Examples
    > 
    > # Plot at Zaragoza, with linear weights and error bar as RIs aesthetic
    > N.plot(ZaragozaSeries, weights = function(t) t-1, conf.aes = "errorbar")
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘RecordTest.Rmd’
      ...
    +     R_lower = Olympic_records_200m$value, Trows = 27)
    
    > records(or200m, type = "points", alpha = c(1, 0, 1, 
    +     0)) + ggplot2::ylab("seconds")
    
    > N.plot(or200m, record = c(0, 1, 0, 0))
    
      When sourcing ‘RecordTest.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘RecordTest.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘RecordTest.Rmd’ using rmarkdown
    ```

# reda

<details>

* Version: 0.5.4
* GitHub: https://github.com/wenjie2wang/reda
* Source code: https://github.com/cran/reda
* Date/Publication: 2022-07-08 21:50:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "reda")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reda-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mcf
    > ### Title: Mean Cumulative Function (MCF)
    > ### Aliases: mcf mcf,formula-method mcf,rateReg-method
    > 
    > ### ** Examples
    > 
    > library(reda)
    ...
    > 
    > ## Example 2. the simulated data
    > simuMcf <- mcf(Recur(time, ID, event) ~ group + gender,
    +                data = simuDat, ID %in% 1 : 50)
    > plot(simuMcf, conf.int = TRUE, lty = 1 : 4,
    +      legendName = "Treatment & Gender")
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘reda-intro.Rmd’
      ...
    +     data = simuDat, subset = ID %in% seq_len(50))
    
    > plot(valveMcf0, conf.int = TRUE, mark.time = TRUE, 
    +     addOrigin = TRUE, col = 2) + ggplot2::xlab("Days") + ggplot2::theme_bw()
    
    > plot(simuMcf, conf.int = TRUE, lty = 1:4, legendName = "Treatment & Gender")
    
      When sourcing ‘reda-intro.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘reda-Recur.Rmd’ using ‘UTF-8’... OK
      ‘reda-intro.Rmd’ using ‘UTF-8’... failed
      ‘reda-simulate.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘reda-Recur.Rmd’ using rmarkdown
    --- finished re-building ‘reda-Recur.Rmd’
    
    --- re-building ‘reda-intro.Rmd’ using rmarkdown
    
    Quitting from lines 123-129 [plot-sampleMcf] (reda-intro.Rmd)
    Error: processing vignette 'reda-intro.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    ...
    --- failed re-building ‘reda-intro.Rmd’
    
    --- re-building ‘reda-simulate.Rmd’ using rmarkdown
    --- finished re-building ‘reda-simulate.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘reda-intro.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        doc    3.3Mb
        libs   4.1Mb
    ```

# redist

<details>

* Version: 4.2.0
* GitHub: https://github.com/alarm-redist/redist
* Source code: https://github.com/cran/redist
* Date/Publication: 2024-01-13 13:20:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "redist")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘redist.Rmd’
      ...
    # ℹ 991 more rows
    
    > library(patchwork)
    
    > hist(plan_sum, max_dev) + hist(iowa_plans, comp) + 
    +     plot_layout(guides = "collect")
    
      When sourcing ‘redist.R’:
    Error: object is not a unit
    Execution halted
    
      ‘common_args.Rmd’ using ‘UTF-8’... OK
      ‘flip.Rmd’ using ‘UTF-8’... OK
      ‘map-preproc.Rmd’ using ‘UTF-8’... OK
      ‘redist.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘common_args.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 29.5Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        libs  25.5Mb
    ```

# registr

<details>

* Version: 2.1.0
* GitHub: NA
* Source code: https://github.com/cran/registr
* Date/Publication: 2022-10-02 21:40:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "registr")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘incomplete_curves.Rmd’
      ...
    +     ggplot(reg4_joint$Y, aes(x = t_hat, y = value, group = id)) + 
    +         geom_line(alpha = 0.3) + xlab("t [registered]") + .... [TRUNCATED] 
    
    > if (have_ggplot2) {
    +     plot(reg4_joint$fpca_obj)
    + }
    
    ...
    > if (have_ggplot2 && requireNamespace("cowplot", quietly = TRUE)) {
    +     registr:::plot.fpca(bfpca_object)
    + }
    
      When sourcing ‘registr.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘incomplete_curves.Rmd’ using ‘UTF-8’... failed
      ‘registr.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘incomplete_curves.Rmd’ using rmarkdown
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘registr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fpca_gauss
    > ### Title: Functional principal components analysis via variational EM
    > ### Aliases: fpca_gauss
    > 
    > ### ** Examples
    > 
    > data(growth_incomplete)
    ...
    > 
    > # estimate 2 FPCs
    > fpca_obj = fpca_gauss(Y = growth_incomplete, npc = 2)
    Warning in (function (npc, npc_varExplained = NULL, Kt, maxiter, print.iter,  :
      fpca_gauss convergence not reached. Try increasing maxiter.
    > plot(fpca_obj)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(registr)
      Warning message:
      In check_dep_version() : ABI version mismatch: 
      lme4 was built with Matrix ABI version 1
      Current Matrix ABI version is 0
      Please re-install lme4 from source or restore original 'Matrix' package
    ...
       12.         └─methods (local) `<rfMthdDf>`(...)
       13.           └─methods::new(def, ...)
       14.             ├─methods::initialize(value, ...)
       15.             └─methods::initialize(value, ...)
       16.               └─.Object$initialize(...)
       17.                 └─lme4 (local) initializePtr()
      
      [ FAIL 13 | WARN 6 | SKIP 6 | PASS 166 ]
      Error: Test failures
      Execution halted
    ```

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        doc    1.8Mb
        libs   3.1Mb
    ```

# reliabilitydiag

<details>

* Version: 0.2.1
* GitHub: https://github.com/aijordan/reliabilitydiag
* Source code: https://github.com/cran/reliabilitydiag
* Date/Publication: 2022-06-29 00:20:06 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "reliabilitydiag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reliabilitydiag-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: c.reliabilitydiag
    > ### Title: Combining reliability diagram objects
    > ### Aliases: c.reliabilitydiag
    > 
    > ### ** Examples
    > 
    > data("precip_Niamey_2016", package = "reliabilitydiag")
    > 
    > X <- precip_Niamey_2016[c("EMOS", "ENS")]
    > Y <- precip_Niamey_2016$obs
    > r0 <- reliabilitydiag0(Y)
    > r1 <- c(r0, X, EPC = precip_Niamey_2016$EPC, region.level = NA)
    > r1
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# relliptical

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/relliptical
* Date/Publication: 2024-02-07 12:50:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "relliptical")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘relliptical-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: rtelliptical
    > ### Title: Sampling Random Numbers from Truncated Multivariate Elliptical
    > ###   Distributions
    > ### Aliases: rtelliptical
    > 
    > ### ** Examples
    > 
    ...
    > Sigma = matrix(c(1,-0.5,-0.5,1), 2, 2)
    > lower = c(-2, -2)
    > upper = c(3, 2)
    > sample4 = rtelliptical(2000, mu, Sigma, lower, upper, gFun=function(t){t^(-1/2)*exp(-2*t^(1/4))})
    > f1 = ggplot(data.frame(sample4), aes(x=X1,y=X2)) + geom_point(size=0.50) +
    +      labs(x=expression(X[1]), y=expression(X[2]), subtitle="Kotz(2,1/4,1/2)") + theme_bw()
    > ggMarginal(f1, type="histogram", fill="grey")
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: ggMarginal ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        libs   6.5Mb
    ```

# reportRmd

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/reportRmd
* Date/Publication: 2023-11-16 17:00:03 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "reportRmd")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reportRmd-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotuv
    > ### Title: Plot multiple bivariate relationships in a single plot
    > ### Aliases: plotuv
    > ### Keywords: plot
    > 
    > ### ** Examples
    > 
    ...
    </table>> plotuv(data=pembrolizumab,  response='cbr',
    + covs=c('age','sex','l_size','baseline_ctdna'),showN=TRUE)
    Warning: `stat(count)` was deprecated in ggplot2 3.4.0.
    ℹ Please use `after_stat(count)` instead.
    ℹ The deprecated feature was likely used in the reportRmd package.
      Please report the issue to the authors.
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘reportRmd.Rmd’
      ...
    > ctDNA <- clear_labels(ctDNA)
    
    > plotuv(data = pembrolizumab, response = "orr", covs = c("age", 
    +     "cohort", "pdl1", "change_ctdna_group"))
    Boxplots not shown for categories with fewer than 20 observations.
    Boxplots not shown for categories with fewer than 20 observations.
    
      When sourcing ‘reportRmd.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘reportRmd.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘reportRmd.Rmd’ using rmarkdown
    
    Quitting from lines 380-383 [unnamed-chunk-30] (reportRmd.Rmd)
    Error: processing vignette 'reportRmd.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘reportRmd.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘reportRmd.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# reservr

<details>

* Version: 0.0.3
* GitHub: https://github.com/AshesITR/reservr
* Source code: https://github.com/cran/reservr
* Date/Publication: 2024-06-24 16:40:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "reservr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘reservr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: blended_transition
    > ### Title: Transition functions for blended distributions
    > ### Aliases: blended_transition blended_transition_inv
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    +   theme_bw() %+%
    +   theme(
    +     legend.position = "bottom", legend.box = "horizontal"
    +   ) %+%
    +   guides(color = guide_legend(direction = "horizontal", title = ""), linetype = guide_none()) %+%
    +   scale_linetype_manual(values = c("TRUE" = 1, "FALSE" = 3))
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘distributions.Rmd’ using rmarkdown
    
    Quitting from lines 170-227 [unnamed-chunk-10] (distributions.Rmd)
    Error: processing vignette 'distributions.Rmd' failed with diagnostics:
    object is not a unit
    --- failed re-building ‘distributions.Rmd’
    
    --- re-building ‘jss_paper.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘distributions.Rmd’
      ...
    
    > attr(trunc_fit$logLik, "nobs")
    [1] 62
    
    > plot_distributions(true = norm, fit1 = norm, fit2 = norm2, 
    +     fit3 = dist_normal(3), .x = seq(-2, 7, 0.01), with_params = list(true = list(mean  .... [TRUNCATED] 
    
    ...
    
    > dist$sample(1)
    
      When sourcing ‘jss_paper.R’:
    Error: invalid arguments
    Execution halted
    
      ‘distributions.Rmd’ using ‘UTF-8’... failed
      ‘jss_paper.Rmd’ using ‘UTF-8’... failed
      ‘tensorflow.Rmd’ using ‘UTF-8’... OK
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.8Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    1.2Mb
        libs  12.8Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# RestoreNet

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/RestoreNet
* Date/Publication: 2024-02-15 11:00:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "RestoreNet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RestoreNet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get.scatterpie
    > ### Title: Clonal pie-chart
    > ### Aliases: get.scatterpie
    > 
    > ### ** Examples
    > 
    > rcts <- c("A->1", "B->1", "C->1", "D->1",
    ...
    computing A^-1...
    computing rho...
    computing Chi2...
    computing KLdiv...
    computing BhattDist...
    > 
    > get.scatterpie(re.res, txt = TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: get.scatterpie ... ggplot_add.layer_scatterpie -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘RestoreNet.ltx’ using tex
    Error: processing vignette 'RestoreNet.ltx' failed with diagnostics:
    Running 'texi2dvi' on 'RestoreNet.ltx' failed.
    LaTeX errors:
    ! LaTeX Error: File `realboxes.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    ...
    l.12 \usepackage
                    {amssymb}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘RestoreNet.ltx’
    
    SUMMARY: processing the following file failed:
      ‘RestoreNet.ltx’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# rfPermute

<details>

* Version: 2.5.2
* GitHub: https://github.com/EricArcher/rfPermute
* Source code: https://github.com/cran/rfPermute
* Date/Publication: 2023-08-23 17:40:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "rfPermute")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rfPermute-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotTrace
    > ### Title: Plot Trace
    > ### Aliases: plotTrace
    > 
    > ### ** Examples
    > 
    > library(randomForest)
    ...
    randomForest 4.7-1.1
    Type rfNews() to see new features/changes/bug fixes.
    > data(mtcars)
    > 
    > rf <- randomForest(factor(am) ~ ., mtcars)
    > plotTrace(rf)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: plotTrace ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# RKorAPClient

<details>

* Version: 0.8.1
* GitHub: https://github.com/KorAP/RKorAPClient
* Source code: https://github.com/cran/RKorAPClient
* Date/Publication: 2024-05-02 11:42:54 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "RKorAPClient")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("RKorAPClient")
      > 
      > test_check("RKorAPClient")
      <KorAPConnection> 
      apiUrl:  https://korap.ids-mannheim.de/api/v1.0/ 
      [ FAIL 1 | WARN 0 | SKIP 30 | PASS 25 ]
    ...
        'test-demos.R:129:3', 'test-textMetadata.R:2:3', 'test-textMetadata.R:9:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-misc.R:224:5'): geom_freq_by_year_ci works correctly ─────────
      gpt[["labels"]][["url"]] not equal to "webUIRequestUrl".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 0 | SKIP 30 | PASS 25 ]
      Error: Test failures
      Execution halted
    ```

# roahd

<details>

* Version: 1.4.3
* GitHub: https://github.com/astamm/roahd
* Source code: https://github.com/cran/roahd
* Date/Publication: 2021-11-04 00:10:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "roahd")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘roahd-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.depthgram
    > ### Title: Specialized method to plot 'depthgram' objects
    > ### Aliases: plot.depthgram
    > 
    > ### ** Examples
    > 
    > N <- 50
    ...
    +   N,
    +   centerline = sin(2 * pi * grid),
    +   Cov = Cov
    + )
    > names <- paste0("id_", 1:nrow(Data[[1]]))
    > DG <- depthgram(Data, marginal_outliers = TRUE, ids = names)
    > plot(DG)
    Error in pm[[2]] : subscript out of bounds
    Calls: plot ... plotly_build -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        data   5.0Mb
        doc    1.7Mb
    ```

# robCompositions

<details>

* Version: 2.4.1
* GitHub: NA
* Source code: https://github.com/cran/robCompositions
* Date/Publication: 2023-08-25 15:30:06 UTC
* Number of recursive dependencies: 148

Run `revdepcheck::cloud_details(, "robCompositions")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘robCompositions-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: biplot.pcaCoDa
    > ### Title: Biplot method
    > ### Aliases: biplot.pcaCoDa
    > ### Keywords: aplot
    > 
    > ### ** Examples
    > 
    ...
    > ## with labels for the scores:
    > data(arcticLake)
    > rownames(arcticLake) <- paste(sample(letters[1:26], nrow(arcticLake), replace=TRUE), 
    +                               1:nrow(arcticLake), sep="")
    > pc <- pcaCoDa(arcticLake, method="classical")
    > plot(pc, xlabs=rownames(arcticLake), which = 2)
    > plot(pc, xlabs=rownames(arcticLake), which = 3)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 24.6Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs  21.3Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘imputation.Rnw’ using knitr
    Error: processing vignette 'imputation.Rnw' failed with diagnostics:
    Running 'texi2dvi' on 'imputation.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `scrartcl.cls' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: cls)
    
    ...
    l.3 \usepackage
                   [pdftex]{hyperref}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘robCompositions-overview.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘imputation.Rnw’ ‘robCompositions-overview.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# romic

<details>

* Version: 1.1.3
* GitHub: NA
* Source code: https://github.com/cran/romic
* Date/Publication: 2023-09-21 05:40:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "romic")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘romic-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_bivariate
    > ### Title: Bivariate Plot
    > ### Aliases: plot_bivariate
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    > brauer_augmented <- brauer_2008_tidy %>%
    +   add_pcs(npcs = 5) %>%
    +   tomic_to("triple_omic")
    40 features dropped due to missing values
    > 
    > tomic_table <- brauer_augmented$samples
    > plot_bivariate(tomic_table, "PC1", "PC2", "nutrient", "nutrient", 0.5, 10)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot_bivariate ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
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
       1. └─romic::plot_bivariate(...) at test-module_ggbiv.R:8:3
       2.   └─ggplot2:::`+.gg`(ggplot(tomic_table, running_aes), plot_call)
       3.     └─ggplot2:::add_ggplot(e1, e2, e2name)
       4.       ├─ggplot2::ggplot_add(object, p, objectname)
       5.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       6.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 2 | WARN 0 | SKIP 7 | PASS 64 ]
      Error: Test failures
      Execution halted
    ```

# roptions

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/roptions
* Date/Publication: 2020-05-11 11:10:06 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "roptions")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘roptions-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: box.spread
    > ### Title: Box Spread Strategy Function
    > ### Aliases: box.spread
    > 
    > ### ** Examples
    > 
    > box.spread(100, 105, 95, 110, 3.2, 2.6, 1.1, 2.4)
    ...
    35         5.7
    36         5.7
    37         5.7
    38         5.7
    39         5.7
    40         5.7
    41         5.7
    Error in pm[[2]] : subscript out of bounds
    Calls: box.spread -> print -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# rSAFE

<details>

* Version: 0.1.4
* GitHub: https://github.com/ModelOriented/rSAFE
* Source code: https://github.com/cran/rSAFE
* Date/Publication: 2022-08-13 13:20:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "rSAFE")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rSAFE)
      Welcome to rSAFE (version: 0.1.4).
      > 
      > test_check("rSAFE")
      Single variables processing...
    ...
        8.       └─ggplot2:::add_ggplot(e1, e2, e2name)
        9.         ├─ggplot2::ggplot_add(object, p, objectname)
       10.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       11.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 10 | SKIP 0 | PASS 56 ]
      Deleting unused snapshots:
      • extraction/plot-continuous-variable.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘example_apartments.Rmd’
      ...
    
    > plot(safe_extractor, variable = "district")
    Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    of ggplot2 3.3.4.
    ℹ The deprecated feature was likely used in the rSAFE package.
      Please report the issue at <https://github.com/ModelOriented/rSAFE/issues>.
    
    ...
    of ggplot2 3.3.4.
    ℹ The deprecated feature was likely used in the rSAFE package.
      Please report the issue at <https://github.com/ModelOriented/rSAFE/issues>.
    
      When sourcing ‘example_hr.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘example_apartments.Rmd’ using ‘UTF-8’... failed
      ‘example_hr.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘example_apartments.Rmd’ using rmarkdown
    ```

# santaR

<details>

* Version: 1.2.4
* GitHub: https://github.com/adwolfer/santaR
* Source code: https://github.com/cran/santaR
* Date/Publication: 2024-03-07 00:30:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "santaR")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(santaR)
      
      This is santaR version 1.2.4 
      
      > 
      > test_check("santaR")
    ...
      1/1 mismatches
      [1] 11 - 10 == 1
      ── Failure ('test_dfSearch-plot_nbTP_histogram.R:69:3'): change dfCuttOff ──────
      length(result_nbTPHisto) not equal to length(ggplot2::ggplot()).
      1/1 mismatches
      [1] 11 - 10 == 1
      
      [ FAIL 8 | WARN 4 | SKIP 0 | PASS 681 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘getting-started.Rmd’
      ...
    
    > knitr::include_graphics("../man/figures/santaR-approach.jpg")
    
      When sourcing ‘getting-started.R’:
    Error: Cannot find the file(s): "../man/figures/santaR-approach.jpg"
    Execution halted
    when running code in ‘selecting-optimal-df.Rmd’
    ...
    Execution halted
    
      ‘advanced-command-line-functions.Rmd’ using ‘UTF-8’... OK
      ‘automated-command-line.Rmd’ using ‘UTF-8’... OK
      ‘getting-started.Rmd’ using ‘UTF-8’... failed
      ‘plotting-options.Rmd’ using ‘UTF-8’... OK
      ‘prepare-input-data.Rmd’ using ‘UTF-8’... OK
      ‘selecting-optimal-df.Rmd’ using ‘UTF-8’... failed
      ‘theoretical-background.Rmd’ using ‘UTF-8’... OK
      ‘santaR-GUI.pdf.asis’ using ‘UTF-8’... OK
    ```

# saros

<details>

* Version: 1.2.0
* GitHub: https://github.com/NIFU-NO/saros
* Source code: https://github.com/cran/saros
* Date/Publication: 2024-09-03 07:40:02 UTC
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "saros")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘saros-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fig_height_h_barchart2
    > ### Title: Estimate figure height for a horizontal bar chart
    > ### Aliases: fig_height_h_barchart2
    > 
    > ### ** Examples
    > 
    > fig_height_h_barchart2(makeme(data=ex_survey, dep=b_1:b_3, indep=x1_sex))
    Warning in fortify(data, ...) :
      Arguments in `...` must be used.
    ✖ Problematic argument:
    • cumulative = TRUE
    ℹ Did you misspell an argument name?
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: fig_height_h_barchart2 ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(saros)
      > 
      > testthat::test_check("saros")
      Starting 2 test processes
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 194 ]
    ...
        9.   └─saros:::make_content.cat_plot_html(...)
       10.     └─ggplot2:::`+.gg`(...)
       11.       └─ggplot2:::add_ggplot(e1, e2, e2name)
       12.         ├─ggplot2::ggplot_add(object, p, objectname)
       13.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       14.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 194 ]
      Error: Test failures
      Execution halted
    ```

# scatterpie

<details>

* Version: 0.2.4
* GitHub: NA
* Source code: https://github.com/cran/scatterpie
* Date/Publication: 2024-08-28 17:20:02 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "scatterpie")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scatterpie-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_scatterpie
    > ### Title: geom_scatterpie
    > ### Aliases: geom_scatterpie geom_scatterpie2
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    > d$C <- abs(rnorm(5, sd=3))
    > 
    > ggplot() + 
    + geom_scatterpie(
    +   aes(x=x, y=y), data=d, cols=c("A", "B", "C")
    + ) + 
    + coord_fixed()
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: +.gg ... ggplot_add.layer_scatterpie -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘scatterpie.Rmd’
      ...
    5   12.928774 -11.288549      4 0.34754260 3.144288 3.789556 2.295894
    8 -126.506123  29.230687      5 0.95161857 3.029335 1.048951 2.471943
    9  -68.685285   6.192712      6 0.04502772 3.203072 2.596539 4.439393
    
    > ggplot() + geom_scatterpie(aes(x = long, y = lat, 
    +     group = region), data = d, cols = LETTERS[1:4]) + coord_equal()
    
      When sourcing ‘scatterpie.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘scatterpie.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘scatterpie.Rmd’ using rmarkdown
    
    Quitting from lines 52-54 [unnamed-chunk-3] (scatterpie.Rmd)
    Error: processing vignette 'scatterpie.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘scatterpie.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘scatterpie.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# scdtb

<details>

* Version: 0.1.0
* GitHub: https://github.com/mightymetrika/scdtb
* Source code: https://github.com/cran/scdtb
* Date/Publication: 2024-04-30 08:50:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "scdtb")` for more info

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
      ── Failure ('test-mixed_model_analysis.R:119:3'): mixed_model_analysis uses the .participant variable to label data points
                when .participant is not NULL ──
      res$plot$labels$shape (`actual`) not equal to "factor(part)" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('factor(part)')
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 45 ]
      Error: Test failures
      Execution halted
    ```

# scoringutils

<details>

* Version: 1.2.2
* GitHub: https://github.com/epiforecasts/scoringutils
* Source code: https://github.com/cran/scoringutils
* Date/Publication: 2023-11-29 15:50:10 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "scoringutils")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘scoringutils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_predictions
    > ### Title: Plot Predictions vs True Values
    > ### Aliases: plot_predictions
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
    +     by = c("target_type", "location"),
    +     range = c(0, 50, 90, 95)
    +   ) +
    +   facet_wrap(~ location + target_type, scales = "free_y") +
    +   aes(fill = model, color = model)
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NU
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> compute_geom_2 -> <Anonymous>
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘metric-details.Rmd’ using rmarkdown
    --- finished re-building ‘metric-details.Rmd’
    
    --- re-building ‘scoring-forecasts-directly.Rmd’ using rmarkdown
    --- finished re-building ‘scoring-forecasts-directly.Rmd’
    
    --- re-building ‘scoringutils.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘scoringutils.Rmd’
      ...
    The following messages were produced when checking inputs:
    1.  144 values for `prediction` are NA in the data provided and the corresponding rows were removed. This may indicate a problem if unexpected.
    
    > example_quantile %>% make_NA(what = "truth", target_end_date >= 
    +     "2021-07-15", target_end_date < "2021-05-22") %>% make_NA(what = "forecast",  .... [TRUNCATED] 
    
      When sourcing ‘scoringutils.R’:
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL
    Execution halted
    
      ‘metric-details.Rmd’ using ‘UTF-8’... OK
      ‘scoring-forecasts-directly.Rmd’ using ‘UTF-8’... OK
      ‘scoringutils.Rmd’ using ‘UTF-8’... failed
    ```

# scUtils

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/scUtils
* Date/Publication: 2020-06-25 16:20:02 UTC
* Number of recursive dependencies: 52

Run `revdepcheck::cloud_details(, "scUtils")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(scUtils)
      > 
      > test_check("scUtils")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 32 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-plots.R:59:3'): all kinds of colnames are allowed ────────────
      p$labels not equal to list(y = "Dim2", x = "Dim1", colour = "expression").
      Length mismatch: comparison on first 2 components
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 32 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# SCVA

<details>

* Version: 1.3.1
* GitHub: NA
* Source code: https://github.com/cran/SCVA
* Date/Publication: 2020-01-09 22:50:10 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "SCVA")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SCVA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: graphly
    > ### Title: Interactive plot of single-case data
    > ### Aliases: graphly
    > ### Keywords: Single-case design Graph
    > 
    > ### ** Examples
    > 
    > data(AB)
    > graphly(design = "AB", data = AB)
    Error in pm[[2]] : subscript out of bounds
    Calls: graphly -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# sdmTMB

<details>

* Version: 0.6.0
* GitHub: https://github.com/pbs-assess/sdmTMB
* Source code: https://github.com/cran/sdmTMB
* Date/Publication: 2024-05-30 00:00:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "sdmTMB")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sdmTMB-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: emmeans.sdmTMB
    > ### Title: Estimated marginal means with the 'emmeans' package with
    > ###   'sdmTMB'
    > ### Aliases: emmeans.sdmTMB
    > 
    > ### ** Examples
    > 
    ...
     year2013 - year2017 -0.15358 0.259 959  -0.593  0.9342
     year2015 - year2017  0.03703 0.263 959   0.141  0.9990
    
    P value adjustment: tukey method for comparing a family of 4 estimates 
    
    > emmeans::emmip(fit2, year ~ depth_scaled, at = list(depth_scaled = seq(-2.5, 
    +     2.5, length.out = 50)), CIs = TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 88.5Mb
      sub-directories of 1Mb or more:
        libs  86.4Mb
    ```

# SDMtune

<details>

* Version: 1.3.1
* GitHub: https://github.com/ConsBiol-unibern/SDMtune
* Source code: https://github.com/cran/SDMtune
* Date/Publication: 2023-07-03 12:20:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "SDMtune")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(SDMtune)
      
         _____  ____   __  ___ __
        / ___/ / __ \ /  |/  // /_ __  __ ____   ___
        \__ \ / / / // /|_/ // __// / / // __ \ / _ \
       ___/ // /_/ // /  / // /_ / /_/ // / / //  __/
    ...
      `expected` is a character vector ('Var2')
      ── Failure ('test-plotCor.R:6:3'): The plot has the correct labels and text size ──
      p$labels$y (`actual`) not equal to "Var1" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('Var1')
      
      [ FAIL 2 | WARN 0 | SKIP 55 | PASS 315 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘basic-use.Rmd’
      ...
    [1] 0.8336850 0.8672387
    
    > folds <- randomFolds(data, k = 4, only_presence = TRUE, 
    +     seed = 25)
    
    > auc(cv_model)
    
      When sourcing ‘basic-use.R’:
    Error: object 'cv_model' not found
    Execution halted
    
      ‘basic-use.Rmd’ using ‘UTF-8’... failed
      ‘hyper-tuning.Rmd’ using ‘UTF-8’... OK
      ‘presence-absence.Rmd’ using ‘UTF-8’... OK
      ‘var-selection.Rmd’ using ‘UTF-8’... OK
    ```

# sedproxy

<details>

* Version: 0.7.5
* GitHub: https://github.com/EarthSystemDiagnostics/sedproxy
* Source code: https://github.com/cran/sedproxy
* Date/Publication: 2023-02-26 10:50:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "sedproxy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sedproxy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ClimToProxyClim
    > ### Title: Simulate sediment archived proxy records from an input climate
    > ###   signal.
    > ### Aliases: ClimToProxyClim
    > 
    > ### ** Examples
    > 
    ...
    > 
    > PlotPFMs(PFM$everything, max.replicates = 1, stage.order = "seq") +
    +   facet_wrap(~stage)
    Joining with `by = join_by(stage, scale)`
    Scale for alpha is already present.
    Adding another scale for alpha, which will replace the existing scale.
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction-to-sedproxy.Rmd’
      ...
      Timepoint(s) 1, 101 are in the mixed layer
    
    > PlotPFMs(PFM)
    Joining with `by = join_by(stage, scale)`
    Scale for alpha is already present.
    Adding another scale for alpha, which will replace the existing scale.
    
      When sourcing ‘introduction-to-sedproxy.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘effect-of-climate-dependent-flux.Rmd’ using ‘UTF-8’... OK
      ‘introduction-to-sedproxy.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘effect-of-climate-dependent-flux.Rmd’ using rmarkdown
    ```

# see

<details>

* Version: 0.9.0
* GitHub: https://github.com/easystats/see
* Source code: https://github.com/cran/see
* Date/Publication: 2024-09-06 04:30:02 UTC
* Number of recursive dependencies: 243

Run `revdepcheck::cloud_details(, "see")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘see-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_binomdensity
    > ### Title: Add dot-densities for binary 'y' variables
    > ### Aliases: geom_binomdensity
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
     14. │         └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     15. │           └─l$compute_geom_2(d, theme = plot$theme)
     16. │             └─ggplot2 (local) compute_geom_2(..., self = self)
     17. │               └─self$geom$use_defaults(...)
     18. └─base::.handleSimpleError(...)
     19.   └─rlang (local) h(simpleError(msg, call))
     20.     └─handlers[[1L]](cnd)
     21.       └─cli::cli_abort(...)
     22.         └─rlang::abort(...)
    Execution halted
    ```

# seedreg

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/seedreg
* Date/Publication: 2022-07-07 21:20:02 UTC
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "seedreg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘seedreg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: lineplot
    > ### Title: Graph: line chart
    > ### Aliases: lineplot
    > 
    > ### ** Examples
    > 
    > data("substrate")
    ...
     16. │             └─ggplot2 (local) setup_params(...)
     17. │               └─ggplot2:::make_summary_fun(...)
     18. │                 └─rlang::as_function(fun.data)
     19. │                   └─base::get(x, envir = env, mode = "function")
     20. └─base::.handleSimpleError(...)
     21.   └─rlang (local) h(simpleError(msg, call))
     22.     └─handlers[[1L]](cnd)
     23.       └─cli::cli_abort(...)
     24.         └─rlang::abort(...)
    Execution halted
    ```

# semfindr

<details>

* Version: 0.1.8
* GitHub: https://github.com/sfcheung/semfindr
* Source code: https://github.com/cran/semfindr
* Date/Publication: 2024-04-08 13:30:03 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "semfindr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘semfindr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cfa_dat2
    > ### Title: Sample Data: A CFA Model with an Influential Case
    > ### Aliases: cfa_dat2
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
       .x5                0.227    0.136    1.675    0.094
       .x6                0.472    0.108    4.378    0.000
        f1                0.073    0.063    1.161    0.245
        f2                0.149    0.078    1.926    0.054
    
    > inf_out <- influence_stat(fit)
    > gcd_plot(inf_out)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: gcd_plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(semfindr)
      > 
      > test_check("semfindr")
      Starting 2 test processes
      [ FAIL 2 | WARN 0 | SKIP 13 | PASS 387 ]
      
    ...
       1. └─semfindr::index_plot(fit_est_change, "gcd") at test-index_plot.R:22:1
       2.   └─ggplot2:::`+.gg`(p, do.call(ggplot2::geom_point, point_aes))
       3.     └─ggplot2:::add_ggplot(e1, e2, e2name)
       4.       ├─ggplot2::ggplot_add(object, p, objectname)
       5.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       6.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 2 | WARN 0 | SKIP 13 | PASS 387 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘semfindr.Rmd’
      ...
    
    Note:
    - Only the first 10 case(s) is/are displayed. Set ‘first’ to NULL to display all cases.
    - Cases sorted by Mahalanobis distance in decreasing order.
    
    > gcd_plot(fit_influence, largest_gcd = 3)
    
    ...
    > gcd_plot(fit_influence, largest_gcd = 3)
    
      When sourcing ‘user_id.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘casewise_scores.Rmd’ using ‘UTF-8’... OK
      ‘selecting_cases.Rmd’ using ‘UTF-8’... OK
      ‘semfindr.Rmd’ using ‘UTF-8’... failed
      ‘user_id.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘casewise_scores.Rmd’ using rmarkdown
    ```

# sensiPhy

<details>

* Version: 0.8.5
* GitHub: https://github.com/paternogbc/sensiPhy
* Source code: https://github.com/cran/sensiPhy
* Date/Publication: 2020-04-02 14:50:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "sensiPhy")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sensiPhy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: clade_physig
    > ### Title: Influential clade detection - Phylogenetic signal
    > ### Aliases: clade_physig
    > 
    > ### ** Examples
    > 
    > data(alien)
    ...
    4       0.8975481               0.25
    5       0.8827772               0.40
    
    > sensi_plot(clade, "Bovidae")
    Warning: Use of `nd$estimate` is discouraged.
    ℹ Use `estimate` instead.
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: sensi_plot ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘sensiPhy_vignette.Rmd’
      ...
    
      When sourcing ‘sensiPhy_vignette.R’:
    Error: attempt to use zero-length variable name
    Execution halted
    
      ‘sensiPhy_vignette.Rmd’ using ‘UTF-8’... failed
    ```

# sglg

<details>

* Version: 0.2.2
* GitHub: NA
* Source code: https://github.com/cran/sglg
* Date/Publication: 2022-09-04 03:50:01 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "sglg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sglg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: deviance_residuals
    > ### Title: Deviance Residuals for a Generalized Log-gamma Regression Model
    > ### Aliases: deviance_residuals
    > 
    > ### ** Examples
    > 
    > # Example 1
    > n <- 300
    > error <- rglg(n,0,1,1)
    > y <- 0.5 + error
    > fit <- glg(y~1,data=as.data.frame(y))
    > deviance_residuals(fit)
    Error in pm[[2]] : subscript out of bounds
    Calls: deviance_residuals ... dots2plots -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# sgsR

<details>

* Version: 1.4.5
* GitHub: https://github.com/tgoodbody/sgsR
* Source code: https://github.com/cran/sgsR
* Date/Publication: 2024-03-03 15:10:02 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "sgsR")` for more info

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
      > # * https://r-pkgs.org/tests.html
    ...
      `expected` is a character vector ('zq90')
      ── Failure ('test-utils-plot.R:19:3'): scatter messages ────────────────────────
      o1$labels$x (`actual`) not equal to "pzabove2" (`expected`).
      
      `actual` is NULL
      `expected` is a character vector ('pzabove2')
      
      [ FAIL 2 | WARN 115 | SKIP 19 | PASS 508 ]
      Error: Test failures
      Execution halted
    ```

# SHAPforxgboost

<details>

* Version: 0.1.3
* GitHub: https://github.com/liuyanguu/SHAPforxgboost
* Source code: https://github.com/cran/SHAPforxgboost
* Date/Publication: 2023-05-29 17:20:07 UTC
* Number of recursive dependencies: 112

Run `revdepcheck::cloud_details(, "SHAPforxgboost")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SHAPforxgboost-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scatter.plot.diagonal
    > ### Title: Make customized scatter plot with diagonal line and R2 printed.
    > ### Aliases: scatter.plot.diagonal
    > 
    > ### ** Examples
    > 
    > scatter.plot.diagonal(data = iris, x = "Sepal.Length", y = "Petal.Length")
    [1] "R2 is 0.76 ."
    `geom_smooth()` using formula = 'y ~ x'
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: scatter.plot.diagonal ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# ShapleyOutlier

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/ShapleyOutlier
* Date/Publication: 2023-02-20 10:30:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "ShapleyOutlier")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ShapleyOutlier-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MOE
    > ### Title: Detecting cellwise outliers using Shapley values based on local
    > ###   outlyingness.
    > ### Aliases: MOE
    > 
    > ### ** Examples
    > 
    ...
    > mu <- rep(0,p)
    > Sigma <- matrix(0.9, p, p); diag(Sigma) = 1
    > Sigma_inv <- solve(Sigma)
    > x <- c(0,1,2,2.3,2.5)
    > MOE_x <- MOE(x = x, mu = mu, Sigma = Sigma)
    > plot(MOE_x)
    Error in grid.Call.graphics(C_segments, x$x0, x$y0, x$x1, x$y1, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: plot ... drawDetails -> drawDetails.segments -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘ShapleyOutlier_examples.Rmd’
      ...
    
    > rownames(phi_SCD) <- paste("Step", 0:(nrow(phi_SCD) - 
    +     1))
    
    > plot(new_shapley(phi = phi_SCD), abbrev.var = FALSE, 
    +     abbrev.obs = FALSE, sort.obs = FALSE, sort.var = FALSE)
    
      When sourcing ‘ShapleyOutlier_examples.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘ShapleyOutlier_examples.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘ShapleyOutlier_examples.Rmd’ using rmarkdown
    ```

# shinipsum

<details>

* Version: 0.1.1
* GitHub: https://github.com/Thinkr-open/shinipsum
* Source code: https://github.com/cran/shinipsum
* Date/Publication: 2024-02-09 15:50:05 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "shinipsum")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(shinipsum)
      > 
      > test_check("shinipsum")
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 3150 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
      `a` has length 11, not length 10.
      Backtrace:
          ▆
       1. └─base::lapply(...) at test-ggplot.R:3:3
       2.   └─shinipsum (local) FUN(X[[i]], ...)
       3.     └─testthat::expect_length(a, expected_length) at test-ggplot.R:8:7
      
      [ FAIL 2 | WARN 1 | SKIP 0 | PASS 3150 ]
      Error: Test failures
      Execution halted
    ```

# signatureSurvival

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/signatureSurvival
* Date/Publication: 2023-07-19 11:10:02 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "signatureSurvival")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘signatureSurvival-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MKMplot
    > ### Title: Multivariate Kaplan-Meier survival curve plot
    > ### Aliases: MKMplot
    > ### Keywords: multivariate survival analysis
    > 
    > ### ** Examples
    > 
    ...
    Loading required package: survival
    > require(ggplot2)
    Loading required package: ggplot2
    > data(GSE50081)
    > MKMplot(data=GSE50081,mol=56,X=c("t.stage","n.stage",	"m.stage"),time="month",
    + status="status1",sml="none",quant=c("No",-0.2,0.2),plotmethod="ggsurvplot",
    + adjx = 5)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: MKMplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

# SimCorrMix

<details>

* Version: 0.1.1
* GitHub: https://github.com/AFialkowski/SimCorrMix
* Source code: https://github.com/cran/SimCorrMix
* Date/Publication: 2018-07-01 13:31:03 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "SimCorrMix")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SimCorrMix-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_simpdf_theory
    > ### Title: Plot Simulated Probability Density Function and Target PDF by
    > ###   Distribution Name or Function for Continuous or Count Variables
    > ### Aliases: plot_simpdf_theory
    > ### Keywords: plot
    > 
    > ### ** Examples
    ...
    +   mix_sixths = c(0, 0))
    Total Simulation time: 0 minutes 
    > plot_simpdf_theory(Nmix$Y_mix[, 1],
    +   title = "Mixture of Normal Distributions",
    +   fx = function(x) 0.4 * dnorm(x, -2, 1) + 0.6 * dnorm(x, 2, 1),
    +   lower = -5, upper = 5)
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘cont_mixture.Rmd’
      ...
    > sim_cdf_prob(sim_y = Nmix2$Y_mix[, 1], delta = y_star)$cumulative_prob
    [1] 0.9504
    
    > plot_simpdf_theory(sim_y = Nmix2$Y_mix[, 1], ylower = -10, 
    +     yupper = 10, title = "PDF of Mixture of Normal Distributions", 
    +     fx = fx, low .... [TRUNCATED] 
    
    ...
    
      When sourcing ‘workflow.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘cont_mixture.Rmd’ using ‘UTF-8’... failed
      ‘corr_mixture.Rmd’ using ‘UTF-8’... OK
      ‘method_comp.Rmd’ using ‘UTF-8’... OK
      ‘variable_types.Rmd’ using ‘UTF-8’... OK
      ‘workflow.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘cont_mixture.Rmd’ using rmarkdown
    
    Quitting from lines 129-132 [unnamed-chunk-8] (cont_mixture.Rmd)
    Error: processing vignette 'cont_mixture.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘cont_mixture.Rmd’
    
    --- re-building ‘corr_mixture.Rmd’ using rmarkdown
    --- finished re-building ‘corr_mixture.Rmd’
    ...
    Quitting from lines 294-308 [unnamed-chunk-17] (workflow.Rmd)
    Error: processing vignette 'workflow.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘workflow.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘cont_mixture.Rmd’ ‘workflow.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘grid’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘PoisNor’
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# SimMultiCorrData

<details>

* Version: 0.2.2
* GitHub: https://github.com/AFialkowski/SimMultiCorrData
* Source code: https://github.com/cran/SimMultiCorrData
* Date/Publication: 2018-06-28 17:37:55 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "SimMultiCorrData")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘dist_comp.Rmd’
      ...
    
    > 1 - pnorm(z_prime)
    [1] 0.04999249
    
    > plot_sim_pdf_theory(sim_y = H_exp$continuous_variable[, 
    +     1], Dist = "Exponential", params = 0.5)
    
    ...
    Execution halted
    
      ‘benefits.Rmd’ using ‘UTF-8’... OK
      ‘dist_comp.Rmd’ using ‘UTF-8’... failed
      ‘errorloop.Rmd’ using ‘UTF-8’... OK
      ‘functions.Rmd’ using ‘UTF-8’... OK
      ‘method_comp.Rmd’ using ‘UTF-8’... OK
      ‘sixth_validpdf.Rmd’ using ‘UTF-8’... failed
      ‘variable_types.Rmd’ using ‘UTF-8’... OK
      ‘workflow.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘benefits.Rmd’ using rmarkdown
    --- finished re-building ‘benefits.Rmd’
    
    --- re-building ‘dist_comp.Rmd’ using rmarkdown
    
    Quitting from lines 107-109 [unnamed-chunk-11] (dist_comp.Rmd)
    Error: processing vignette 'dist_comp.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘dist_comp.Rmd’
    ...
    
    Quitting from lines 208-214 [unnamed-chunk-15] (sixth_validpdf.Rmd)
    Error: processing vignette 'sixth_validpdf.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘sixth_validpdf.Rmd’
    
    --- re-building ‘variable_types.Rmd’ using rmarkdown
    --- finished re-building ‘variable_types.Rmd’
    
    --- re-building ‘workflow.Rmd’ using rmarkdown
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘BinNonNor’, ‘PoisNor’, ‘PoisBinOrdNor’, ‘PoisBinOrdNonNor’
    ```

# SimNPH

<details>

* Version: 0.5.5
* GitHub: https://github.com/SimNPH/SimNPH
* Source code: https://github.com/cran/SimNPH
* Date/Publication: 2024-03-04 10:10:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "SimNPH")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(SimNPH)
      Loading required package: SimDesign
      Loading required package: survival
      > 
      > test_check("SimNPH")
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 343 ]
    ...
      
      `names(actual)`:   "x"    
      `names(expected)`: "x" "y"
      
      `actual$y` is absent
      `expected$y` is a character vector ('mpg')
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 343 ]
      Error: Test failures
      Execution halted
    ```

# slendr

<details>

* Version: 0.9.1
* GitHub: https://github.com/bodkan/slendr
* Source code: https://github.com/cran/slendr
* Date/Publication: 2024-02-21 23:20:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "slendr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘slendr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: area
    > ### Title: Calculate the area covered by the given slendr object
    > ### Aliases: area
    > 
    > ### ** Examples
    > 
    > region_a <- region("A", center = c(20, 50), radius = 20)
    ...
      6.       └─ggplot2 (local) setup(..., self = self)
      7.         └─self$coord$setup_params(data)
      8.           └─ggplot2 (local) setup_params(..., self = self)
      9.             └─ggproto_parent(Coord, self)$setup_params(data)
     10.               └─ggplot2 (local) setup_params(..., self = self)
     11.                 └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
     12.                   └─ggplot2:::check_logical(expand)
     13.                     └─ggplot2:::stop_input_type(...)
     14.                       └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘vignette-01-tutorial.Rmd’
      ...
    dependencies automatically by running the function `setup_env()`.
    
    > init_env()
    
      When sourcing ‘vignette-01-tutorial.R’:
    Error: Could not activate slendr's Python environment because it is not
    present on your system ('Python-3.12_msprime-1.3.1_tskit-0.5.6_pyslim-1.0.4_tspop-0.0.2').
    ...
      ‘vignette-01-tutorial.Rmd’ using ‘UTF-8’... failed
      ‘vignette-02-grid-model.Rmd’ using ‘UTF-8’... failed
      ‘vignette-03-interactions.Rmd’ using ‘UTF-8’... failed
      ‘vignette-04-nonspatial-models.Rmd’ using ‘UTF-8’... failed
      ‘vignette-05-tree-sequences.Rmd’ using ‘UTF-8’... failed
      ‘vignette-06-locations.Rmd’ using ‘UTF-8’... failed
      ‘vignette-07-backends.Rmd’ using ‘UTF-8’... failed
      ‘vignette-08-nonslendr-tskit.Rmd’ using ‘UTF-8’... failed
      ‘vignette-09-paper.Rmd’ using ‘UTF-8’... failed
      ‘vignette-10-tracts.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   3.7Mb
    ```

# smallsets

<details>

* Version: 2.0.0
* GitHub: https://github.com/lydialucchesi/smallsets
* Source code: https://github.com/cran/smallsets
* Date/Publication: 2023-12-05 00:00:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "smallsets")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘smallsets-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Smallset_Timeline
    > ### Title: Smallset Timeline
    > ### Aliases: Smallset_Timeline
    > 
    > ### ** Examples
    > 
    > set.seed(145)
    > 
    > Smallset_Timeline(
    +   data = s_data,
    +   code = system.file("s_data_preprocess.R", package = "smallsets")
    + )
    Error in as.unit(value) : object is not coercible to a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> [<- -> [<-.unit -> as.unit
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘smallsets.Rmd’
      ...
    > library(smallsets)
    
    > set.seed(145)
    
    > Smallset_Timeline(data = s_data, code = system.file("s_data_preprocess.R", 
    +     package = "smallsets"))
    
      When sourcing ‘smallsets.R’:
    Error: object is not coercible to a unit
    Execution halted
    
      ‘smallsets.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘smallsets.Rmd’ using rmarkdown
    
    Quitting from lines 36-42 [timeline1] (smallsets.Rmd)
    Error: processing vignette 'smallsets.Rmd' failed with diagnostics:
    object is not coercible to a unit
    --- failed re-building ‘smallsets.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘smallsets.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘gurobi’
    ```

# spinifex

<details>

* Version: 0.3.7.0
* GitHub: https://github.com/nspyrison/spinifex
* Source code: https://github.com/cran/spinifex
* Date/Publication: 2024-01-29 14:40:02 UTC
* Number of recursive dependencies: 165

Run `revdepcheck::cloud_details(, "spinifex")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(spinifex)
      Loading required package: tourr
      --------------------------------------------------------
      spinifex --- version 0.3.7.0
      Please share bugs, suggestions, and feature requests at:
    ...
       2. │ └─base::withCallingHandlers(...)
       3. └─spinifex::play_tour_path(tour_path = tpath, data = dat_std, angle = 1)
       4.   └─spinifex (local) render_type(frames = tour_df, ...)
       5.     ├─plotly::ggplotly(p = gg, tooltip = "tooltip")
       6.     └─plotly:::ggplotly.ggplot(p = gg, tooltip = "tooltip")
       7.       └─plotly::gg2list(...)
      
      [ FAIL 3 | WARN 4 | SKIP 0 | PASS 80 ]
      Error: Test failures
      Execution halted
    ```

# sport

<details>

* Version: 0.2.1
* GitHub: https://github.com/gogonzo/sport
* Source code: https://github.com/cran/sport
* Date/Publication: 2024-01-08 23:50:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "sport")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > 
      > test_check("sport")
      Loading required package: sport
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 238 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test_output.R:30:3'): Scale is labelled 'r' ───────────────────────
      p$labels$y not identical to "r".
      target is NULL, current is character
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 238 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7504 marked UTF-8 strings
    ```

# spotoroo

<details>

* Version: 0.1.4
* GitHub: https://github.com/TengMCing/spotoroo
* Source code: https://github.com/cran/spotoroo
* Date/Publication: 2023-08-21 05:50:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "spotoroo")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(spotoroo)
      > 
      > test_check("spotoroo")
      
      -------------------------------- SPOTOROO 0.1.4 --------------------------------
      
    ...
      i Actually got a <simpleError> with text:
        argument is of length zero
      ── Failure ('test-plot_spotoroo.R:64:3'): plot_spotoroo() works ────────────────
      Expected `plot_spotoroo(result, type = "timeline")` to run without any errors.
      i Actually got a <simpleError> with text:
        argument is of length zero
      
      [ FAIL 2 | WARN 5 | SKIP 0 | PASS 65 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Clustering-hot-spots.Rmd’
      ...
    
    ────────────────────────────────────────────────────────────────────────────────
    
    > plot_spotoroo(result, type = "def")
    
    > plot_spotoroo(result, type = "timeline")
    
      When sourcing ‘Clustering-hot-spots.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Clustering-hot-spots.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Clustering-hot-spots.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# SqueakR

<details>

* Version: 1.3.0
* GitHub: https://github.com/osimon81/SqueakR
* Source code: https://github.com/cran/SqueakR
* Date/Publication: 2022-06-28 09:20:04 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "SqueakR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘SqueakR.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘SqueakR.Rmd’
      ...
     $ experimenters    : NULL
     $ experimental_data: list()
    
    > my_new_data <- add_timepoint_data(data_path = "../inst/extdata/Example_Mouse_Data.xlsx", 
    +     t1 = 5, t2 = 25)
    Adding call features Excel file to workspace...
    
      When sourcing ‘SqueakR.R’:
    Error: `path` does not exist: ‘../inst/extdata/Example_Mouse_Data.xlsx’
    Execution halted
    
      ‘SqueakR.Rmd’ using ‘UTF-8’... failed
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.8Mb
      sub-directories of 1Mb or more:
        doc   8.2Mb
    ```

# stability

<details>

* Version: 0.5.0
* GitHub: https://github.com/myaseen208/stability
* Source code: https://github.com/cran/stability
* Date/Publication: 2018-10-02 17:50:03 UTC
* Number of recursive dependencies: 47

Run `revdepcheck::cloud_details(, "stability")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘stability-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ammi_biplot
    > ### Title: Additive Main Effects and Multiplicative Interaction (AMMI)
    > ###   Biplot
    > ### Aliases: ammi_biplot ammi_biplot.default
    > 
    > ### ** Examples
    > 
    ...
    >      ammi_biplot(
    +             .data = ge_data
    +           , .y    = Yield
    +           , .rep  = Rep
    +           , .gen  = Gen
    +           , .env  = Env
    +       )
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: ammi_biplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# statgenGWAS

<details>

* Version: 1.0.9
* GitHub: https://github.com/Biometris/statgenGWAS
* Source code: https://github.com/cran/statgenGWAS
* Date/Publication: 2022-10-13 15:30:43 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "statgenGWAS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("statgenGWAS")
      + }
      
      test_GWAS.R...................    0 tests    
      test_GWAS.R...................    0 tests    
    ...
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <86>
      3: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <90>
      4: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <e2>
      5: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <86>
      6: In grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  :
        conversion failure on '← 2@3' in 'mbcsToSbcs': dot substituted for <90>
      Execution halted
    ```

## In both

*   checking C++ specification ... NOTE
    ```
      Specified C++11: please drop specification unless essential
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 15.2Mb
      sub-directories of 1Mb or more:
        data   7.0Mb
        libs   7.2Mb
    ```

# statgenHTP

<details>

* Version: 1.0.6.1
* GitHub: https://github.com/Biometris/statgenHTP
* Source code: https://github.com/cran/statgenHTP
* Date/Publication: 2023-04-14 08:20:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "statgenHTP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘statgenHTP-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: removeSerieOut
    > ### Title: Replace outliers for series of observations by NA
    > ### Aliases: removeSerieOut
    > 
    > ### ** Examples
    > 
    > ## Run the function to fit P-splines on a subset of genotypes.
    ...
     18. │ └─ggplot2:::`+.gg`(...)
     19. │   └─ggplot2:::add_ggplot(e1, e2, e2name)
     20. │     ├─ggplot2::ggplot_add(object, p, objectname)
     21. │     └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     22. │       └─ggplot2:::new_layer_names(object, names(plot$layers))
     23. └─base::.handleSimpleError(...)
     24.   └─purrr (local) h(simpleError(msg, call))
     25.     └─cli::cli_abort(...)
     26.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if ( requireNamespace("tinytest", quietly=TRUE) ){
      +   tinytest::test_package("statgenHTP")
      + }
      
      test_TP.R.....................    0 tests    
      test_TP.R.....................    0 tests    
    ...
      test_detectSerieOut.R.........   16 tests [0;31m1 fails[0m Error in eval(expr, envir = e) : object 'serieOut1' not found
      Calls: <Anonymous> ... lapply -> FUN -> eval -> eval -> expect_inherits -> fun
      In addition: Warning messages:
      1:  125 failed to parse. 
      2: Ignoring unknown labels:
      • `colour = ""` 
      3: The following genotypes have less than 3 plotIds and are skipped in the outlier detection:
      G12
       
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Overview_HTP.Rmd’
      ...
    > outVator <- detectSerieOut(corrDat = spatCorrectedVator, 
    +     predDat = predDat, coefDat = coefDat, trait = "EffpsII_corr", 
    +     genotypes = sub .... [TRUNCATED] 
    
      When sourcing ‘Overview_HTP.R’:
    Error: ℹ In index: 1.
    ℹ With name: y.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    Execution halted
    
      ‘Overview_HTP.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Overview_HTP.Rmd’ using rmarkdown
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘asreml’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

# sugrrants

<details>

* Version: 0.2.9
* GitHub: https://github.com/earowang/sugrrants
* Source code: https://github.com/cran/sugrrants
* Date/Publication: 2024-03-12 05:20:03 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "sugrrants")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sugrrants-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: frame_calendar
    > ### Title: Rearrange a temporal data frame to a calendar-based data format
    > ###   using linear algebra
    > ### Aliases: frame_calendar prettify
    > 
    > ### ** Examples
    > 
    ...
    +   frame_calendar(x = Time, y = Hourly_Counts, date = Date, nrow = 4)
    > 
    > # ggplot
    > p1 <- calendar_df %>%
    +   ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
    +   geom_line()
    > prettify(p1, size = 3, label.padding = unit(0.15, "lines"))
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: prettify ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘frame-calendar.Rmd’
      ...
    > p1 <- centre_calendar %>% ggplot(aes(x = .Time, y = .Hourly_Counts, 
    +     group = Date)) + geom_line()
    
    > p1
    
    > prettify(p1)
    
      When sourcing ‘frame-calendar.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘frame-calendar.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘frame-calendar.Rmd’ using rmarkdown
    ```

# superb

<details>

* Version: 0.95.15
* GitHub: https://github.com/dcousin3/superb
* Source code: https://github.com/cran/superb
* Date/Publication: 2024-08-17 19:00:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "superb")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘superb-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GRD
    > ### Title: Generate random data
    > ### Aliases: GRD
    > 
    > ### ** Examples
    > 
    >  # Simplest example using all the default arguments: 
    ...
    	Difficulty; levels: 1, 2, 3, 4, 5
    2.Within-Subject Factors ( 1 repeated measures ):
    3.Subjects per group ( 500 total subjects ):
    	 100
     ------------------------------------------------------------ 
    >  # show the mean performance as a function of difficulty:
    >  superbPlot(dta, BSFactors = "Difficulty", variables="DV")
    Error in superbPlot(dta, BSFactors = "Difficulty", variables = "DV") : 
      superb::ERROR: The function superbPlot.bar is not a known function for making plots with superbPlot. Exiting...
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("superb")
      Loading required package: superb
      [ FAIL 48 | WARN 0 | SKIP 0 | PASS 93 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test_compatibility.R:36:2'): TESTS (1/1) ────────────────────────────
    ...
       1. ├─testthat::expect_message(...) at test_superbPlot.R:810:5
       2. │ └─testthat:::quasi_capture(enquo(object), label, capture_messages)
       3. │   ├─testthat (local) .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. └─superb::superbPlot(...)
      
      [ FAIL 48 | WARN 0 | SKIP 0 | PASS 93 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘CustomizingSuperbPlots.Rmd’
      ...
    > mylabels <- c("Verbal", "Numerical", "Spatial", "Creativity", 
    +     "Intrapersonal", "Interpersonal")
    
    > pltA <- superbPlot(dtaA, WSFactors = "Domain(6)", 
    +     variables = mylabels, adjustments = list(purpose = "difference", 
    +         decorrelation = .... [TRUNCATED] 
    
    ...
      ‘Vignette7.Rmd’ using ‘UTF-8’... failed
      ‘Vignette8.Rmd’ using ‘UTF-8’... failed
      ‘Vignette9.Rmd’ using ‘UTF-8’... failed
      ‘VignetteA.Rmd’ using ‘UTF-8’... failed
      ‘VignetteB.Rmd’ using ‘UTF-8’... failed
      ‘VignetteC.Rmd’ using ‘UTF-8’... failed
      ‘VignetteD.Rmd’ using ‘UTF-8’... OK
      ‘VignetteE.Rmd’ using ‘UTF-8’... failed
      ‘VignetteF.Rmd’ using ‘UTF-8’... failed
      ‘VignetteG.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘CustomizingSuperbPlots.Rmd’ using rmarkdown
    
    Quitting from lines 91-107 [unnamed-chunk-4] (CustomizingSuperbPlots.Rmd)
    Error: processing vignette 'CustomizingSuperbPlots.Rmd' failed with diagnostics:
    superb::ERROR: The function superbPlot.raincloud is not a known function for making plots with superbPlot. Exiting...
    --- failed re-building ‘CustomizingSuperbPlots.Rmd’
    
    --- re-building ‘TheMakingOf.Rmd’ using rmarkdown
    
    ...
    --- failed re-building ‘Vignette1.Rmd’
    
    --- re-building ‘Vignette2.Rmd’ using rmarkdown
    
    Quitting from lines 39-48 [unnamed-chunk-2] (Vignette2.Rmd)
    Error: processing vignette 'Vignette2.Rmd' failed with diagnostics:
    superb::ERROR: The function superbPlot.line is not a known function for making plots with superbPlot. Exiting...
    --- failed re-building ‘Vignette2.Rmd’
    
    --- re-building ‘Vignette3.Rmd’ using rmarkdown
    ```

# surveyexplorer

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/surveyexplorer
* Date/Publication: 2024-06-07 09:50:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "surveyexplorer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘surveyexplorer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: multi_freq
    > ### Title: Generate an UpSet plot for multiple-choice questions
    > ### Aliases: multi_freq
    > 
    > ### ** Examples
    > 
    > 
    ...
    
    > 
    > #Basic Upset plot
    > 
    > #Use `group_by` to partition the question into several groups
    >  multi_freq(berlinbears, question = dplyr::starts_with('will_eat'), group_by
    +  = gender)
    Error in as.unit(e2) : object is not coercible to a unit
    Calls: <Anonymous> ... polylineGrob -> is.unit -> unit.c -> Ops.unit -> as.unit
    Execution halted
    ```

# survivalAnalysis

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/survivalAnalysis
* Date/Publication: 2022-02-11 14:00:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "survivalAnalysis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘survivalAnalysis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: kaplan_meier_plot
    > ### Title: Kaplan Meier plots from survival results.
    > ### Aliases: kaplan_meier_plot
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    ...
    
    > survival::aml %>%
    +   analyse_survival(vars(time, status), x) %>%
    +   kaplan_meier_plot(break.time.by="breakByMonth",
    +                     xlab=".OS.months",
    +                     risk.table=TRUE,
    +                     ggtheme=ggplot2::theme_bw(10))
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: %>% ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘multivariate.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘univariate.Rmd’
      ...
    ECOG 2-3 vs. ECOG 1-2     1.41 2.0     2.82 <0.001
    ECOG 1-2 vs. ECOG 2-3     0.35 0.5     0.71 <0.001
    
    
    
    > kaplan_meier_plot(result)
    
      When sourcing ‘univariate.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘multivariate.Rmd’ using ‘UTF-8’... OK
      ‘univariate.Rmd’ using ‘UTF-8’... failed
    ```

# survminer

<details>

* Version: 0.4.9
* GitHub: https://github.com/kassambara/survminer
* Source code: https://github.com/cran/survminer
* Date/Publication: 2021-03-09 09:50:03 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "survminer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘survminer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BRCAOV.survInfo
    > ### Title: Breast and Ovarian Cancers Survival Information
    > ### Aliases: BRCAOV.survInfo
    > 
    > ### ** Examples
    > 
    > data(BRCAOV.survInfo)
    ...
    The following object is masked from ‘package:survminer’:
    
        myeloma
    
    > fit <- survfit(Surv(times, patient.vital_status) ~ admin.disease_code,
    +            data = BRCAOV.survInfo)
    > ggsurvplot(fit, data = BRCAOV.survInfo, risk.table = TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: ggsurvplot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(survminer)
      Loading required package: ggplot2
      Loading required package: ggpubr
      > 
      > test_check("survminer")
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 0 ]
    ...
       4.     └─survminer (local) `<fn>`(...)
       5.       └─ggplot2:::`+.gg`(...)
       6.         └─ggplot2:::add_ggplot(e1, e2, e2name)
       7.           ├─ggplot2::ggplot_add(object, p, objectname)
       8.           └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       9.             └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Informative_Survival_Plots.Rmd’
      ...
    
    
    > fit <- survfit(Surv(times, patient.vital_status) ~ 
    +     admin.disease_code, data = BRCAOV.survInfo)
    
    > ggsurvplot(fit, data = BRCAOV.survInfo, risk.table = TRUE)
    
    ...
    > ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE)
    
      When sourcing ‘Specifiying_weights_in_log-rank_comparisons.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘Informative_Survival_Plots.Rmd’ using ‘UTF-8’... failed
      ‘Playing_with_fonts_and_texts.Rmd’ using ‘UTF-8’... failed
      ‘Specifiying_weights_in_log-rank_comparisons.Rmd’ using ‘UTF-8’... failed
      ‘ggforest-show-interactions-hazard-ratio.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Informative_Survival_Plots.Rmd’ using rmarkdown
    
    Quitting from lines 66-72 [unnamed-chunk-4] (Informative_Survival_Plots.Rmd)
    Error: processing vignette 'Informative_Survival_Plots.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Informative_Survival_Plots.Rmd’
    
    --- re-building ‘Playing_with_fonts_and_texts.Rmd’ using rmarkdown
    
    ...
    --- failed re-building ‘Playing_with_fonts_and_texts.Rmd’
    
    --- re-building ‘Specifiying_weights_in_log-rank_comparisons.Rmd’ using rmarkdown
    
    Quitting from lines 98-99 [unnamed-chunk-4] (Specifiying_weights_in_log-rank_comparisons.Rmd)
    Error: processing vignette 'Specifiying_weights_in_log-rank_comparisons.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Specifiying_weights_in_log-rank_comparisons.Rmd’
    
    --- re-building ‘ggforest-show-interactions-hazard-ratio.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc   5.6Mb
    ```

# survParamSim

<details>

* Version: 0.1.6
* GitHub: https://github.com/yoshidk6/survParamSim
* Source code: https://github.com/cran/survParamSim
* Date/Publication: 2022-06-03 08:10:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "survParamSim")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘survParamSim.Rmd’
      ...
    +     "Lev+5FU")), depth = as.nu .... [TRUNCATED] 
    
    > survfit.colon <- survfit(Surv(time, status) ~ rx, 
    +     data = colon2)
    
    > survminer::ggsurvplot(survfit.colon)
    
      When sourcing ‘survParamSim.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘survParamSim.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘survParamSim.Rmd’ using rmarkdown
    
    Quitting from lines 53-58 [plot_raw_data] (survParamSim.Rmd)
    Error: processing vignette 'survParamSim.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘survParamSim.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘survParamSim.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# survstan

<details>

* Version: 0.0.7.1
* GitHub: https://github.com/fndemarqui/survstan
* Source code: https://github.com/cran/survstan
* Date/Publication: 2024-04-12 16:50:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "survstan")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘LRT.Rmd’
      ...
    > ipass <- ipass %>% mutate(arm = as.factor(ipass$arm), 
    +     arm = ifelse(arm == 1, "gefitinib", "carboplatin/paclitaxel"))
    
    > km <- survfit(Surv(time, status) ~ arm, data = ipass)
    
    > ggsurv(km)
    
      When sourcing ‘LRT.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘LRT.Rmd’ using ‘UTF-8’... failed
      ‘survstan.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘LRT.Rmd’ using rmarkdown
    
    Quitting from lines 31-42 [unnamed-chunk-2] (LRT.Rmd)
    Error: processing vignette 'LRT.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘LRT.Rmd’
    
    --- re-building ‘survstan.Rmd’ using rmarkdown
    --- finished re-building ‘survstan.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘LRT.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 80.3Mb
      sub-directories of 1Mb or more:
        libs  79.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RcppParallel’ ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# SVMMaj

<details>

* Version: 0.2.9.2
* GitHub: NA
* Source code: https://github.com/cran/SVMMaj
* Date/Publication: 2024-08-19 08:20:13 UTC
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "SVMMaj")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘SVMMaj-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: print.q.svmmaj
    > ### Title: SVM-Maj Algorithm
    > ### Aliases: print.q.svmmaj svmmaj svmmaj.default
    > 
    > ### ** Examples
    > 
    > 
    ...
    
    > model3 <- svmmaj(
    +   diabetes$X, diabetes$y, weight.obs = weight.obs,
    +   spline.knots = 3, spline.degree = 2
    + )
    > plotWeights(model3, plotdim = c(2, 4))
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: plotWeights ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘paper.Rnw’
      ...
    
                        TP        FP Precision
        negative     0.741     0.259     0.879
        positive     0.817     0.183     0.636
    
    > plotWeights(model.spline)
    
      When sourcing ‘paper.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘paper.Rnw’... failed
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘paper.Rnw’ using Sweave
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    Scale for y is already present.
    Adding another scale for y, which will replace the existing scale.
    
    Error: processing vignette 'paper.Rnw' failed with diagnostics:
     chunk 20 (label = splineweightsplot) 
    ...
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    
    --- failed re-building ‘paper.Rnw’
    
    SUMMARY: processing the following file failed:
      ‘paper.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Sysrecon

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/Sysrecon
* Date/Publication: 2023-02-20 08:50:02 UTC
* Number of recursive dependencies: 58

Run `revdepcheck::cloud_details(, "Sysrecon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Sysrecon-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Sysrecon
    > ### Title: Sysrecon
    > ### Aliases: Sysrecon
    > 
    > ### ** Examples
    > 
    > 
    ...
    Warning in fortify(data, ...) :
      Arguments in `...` must be used.
    ✖ Problematic arguments:
    • as.Date = as.Date
    • yscale_mapping = yscale_mapping
    • hang = hang
    ℹ Did you misspell an argument name?
    Error in as.unit(value) : object is not coercible to a unit
    Calls: Sysrecon ... assemble_guides -> guides_build -> [<- -> [<-.unit -> as.unit
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 38 marked UTF-8 strings
    ```

# tabledown

<details>

* Version: 1.0.0
* GitHub: https://github.com/masiraji/tabledown
* Source code: https://github.com/cran/tabledown
* Date/Publication: 2024-05-02 13:40:03 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "tabledown")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tabledown-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggreliability_plotly
    > ### Title: A Function for Creating Item Response Theory based reliability
    > ###   plot based on plotly.
    > ### Aliases: ggreliability_plotly
    > 
    > ### ** Examples
    > 
    ...
    Iteration: 17, Log-Lik: -5351.363, Max-Change: 0.00011
    Iteration: 18, Log-Lik: -5351.363, Max-Change: 0.00054
    Iteration: 19, Log-Lik: -5351.363, Max-Change: 0.00012
    Iteration: 20, Log-Lik: -5351.363, Max-Change: 0.00035
    Iteration: 21, Log-Lik: -5351.363, Max-Change: 0.00010
    > 
    > plot <- ggreliability_plotly(data, model)
    Error in pm[[2]] : subscript out of bounds
    Calls: ggreliability_plotly -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 551 marked UTF-8 strings
    ```

# tcgaViz

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/tcgaViz
* Date/Publication: 2023-04-04 15:40:02 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "tcgaViz")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Tutorial.Rmd’
      ...
    #   `P-value adjusted` <dbl>, Significance <chr>
    
    > plot(df, stats = stats)
    
      When sourcing ‘Tutorial.R’:
    Error: ℹ In index: 1.
    ℹ With name: value.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    Execution halted
    
      ‘Tutorial.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘Tutorial.Rmd’ using rmarkdown
    
    Quitting from lines 35-43 [plot] (Tutorial.Rmd)
    Error: processing vignette 'Tutorial.Rmd' failed with diagnostics:
    ℹ In index: 1.
    ℹ With name: value.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    --- failed re-building ‘Tutorial.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘Tutorial.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# TCIU

<details>

* Version: 1.2.6
* GitHub: https://github.com/SOCR/TCIU
* Source code: https://github.com/cran/TCIU
* Date/Publication: 2024-05-17 23:40:21 UTC
* Number of recursive dependencies: 163

Run `revdepcheck::cloud_details(, "TCIU")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TCIU-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fmri_image
    > ### Title: interactive graph object of the fMRI image
    > ### Aliases: fmri_image
    > 
    > ### ** Examples
    > 
    > fmri_generate = fmri_simulate_func(dim_data = c(64, 64, 40), mask = mask)
    > fmri_image(fmri_generate$fmri_data, option='manually', voxel_location = c(40,22,33), time = 4)
    Error in pm[[2]] : subscript out of bounds
    Calls: fmri_image ... add_trace -> add_data -> ggplotly -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tciu-LT-kimesurface.Rmd’
      ...
    > require(ggplot2)
    
    > sample_save[[1]]
    
    > sample_save[[2]]
    
      When sourcing ‘tciu-LT-kimesurface.R’:
    ...
    
    > fmri_image(fmri_generate$fmri_data, option = "manually", 
    +     voxel_location = c(40, 22, 33), time = 4)
    
      When sourcing ‘tciu-fMRI-analytics.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘tciu-LT-kimesurface.Rmd’ using ‘UTF-8’... failed
      ‘tciu-fMRI-analytics.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘tciu-LT-kimesurface.Rmd’ using rmarkdown
    
    Quitting from lines 159-160 [unnamed-chunk-5] (tciu-LT-kimesurface.Rmd)
    Error: processing vignette 'tciu-LT-kimesurface.Rmd' failed with diagnostics:
    unused arguments (list(1, 2), list(list("black", 0.727272727272727, 1, "butt", FALSE, TRUE), list("white", "black", 0.727272727272727, 1, TRUE), list("", "plain", "black", 16, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(4, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 4, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 4, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 
        NULL, 1, -90, NULL, c(0, 0, 0, 4), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(3.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 3.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, NULL, c(0, 3.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 3.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, 
        NULL, NULL, c(0, 3.2, 0, 3.2), NULL, TRUE), list("grey20", NULL, NULL, NULL, FALSE, TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 4, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), c(8, 8, 8, 8), 16, NULL, NULL, NULL, 1.2, NULL, NULL, 8, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, 
        NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, "bold", "black", 14, 0, NULL, NULL, NULL, NULL, NULL, FALSE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, c(0, 0, 0, 0), list(), 16, list("grey92", NA, NULL, NULL, TRUE), list(), 8, NULL, NULL, list("white", NULL, NULL, NULL, FALSE, TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, TRUE), NULL, list(), NULL, list(), FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0.5, 1, NULL, 
    ...
    Quitting from lines 184-185 [unnamed-chunk-5] (tciu-fMRI-analytics.Rmd)
    Error: processing vignette 'tciu-fMRI-analytics.Rmd' failed with diagnostics:
    subscript out of bounds
    --- failed re-building ‘tciu-fMRI-analytics.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘tciu-LT-kimesurface.Rmd’ ‘tciu-fMRI-analytics.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.1Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        doc   12.0Mb
    ```

# tcpl

<details>

* Version: 3.1.0
* GitHub: https://github.com/USEPA/CompTox-ToxCast-tcpl
* Source code: https://github.com/cran/tcpl
* Date/Publication: 2023-10-06 19:50:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "tcpl")` for more info

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
       20. │ └─grid:::grid.draw.grob(x$children[[i]], recording = FALSE)
       21. │   └─grDevices::recordGraphics(drawGrob(x), list(x = x), getNamespace("grid"))
       22. └─grid:::drawGrob(x)
       23.   ├─grid::drawDetails(x, recording = FALSE)
       24.   └─grid:::drawDetails.polyline(x, recording = FALSE)
       25.     └─grid:::grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow)
      
      [ FAIL 2 | WARN 4 | SKIP 3 | PASS 55 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Data_processing-Archive_tcpl_v2.Rmd’
      ...
    Loaded L4 AEID2 (7 rows; 0.03 secs)
    Processed L5 AEID2 (7 rows; 0.08 secs)
    Writing level 5 data for 2 ids...
    Completed delete cascade for 2 ids (0.03 secs)
    Writing level 5 complete. (0.03 secs)
    Loaded L5 AEID1 (6 rows; 0.11 secs)
    
      When sourcing ‘Data_processing-Archive_tcpl_v2.R’:
    Error: attempt to apply non-function
    Execution halted
    
      ‘Assay_Registration.Rmd’ using ‘UTF-8’... OK
      ‘Data_processing-Archive_tcpl_v2.Rmd’ using ‘UTF-8’... failed
      ‘Data_processing.Rmd’ using ‘UTF-8’... OK
      ‘Data_retrieval.Rmd’ using ‘UTF-8’... OK
      ‘Introduction_Appendices.Rmd’ using ‘UTF-8’... OK
    ```

# tern

<details>

* Version: 0.9.5
* GitHub: https://github.com/insightsengineering/tern
* Source code: https://github.com/cran/tern
* Date/Publication: 2024-06-21 04:40:06 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "tern")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tern-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: g_km
    > ### Title: Kaplan-Meier plot
    > ### Aliases: g_km kaplan_meier
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    +   control_surv = control_surv_timepoint(conf_level = 0.9),
    +   col = c("grey25", "grey50", "grey75"),
    +   annot_at_risk_title = FALSE,
    +   lty = 1:3,
    +   font_size = 8
    + )
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    4.5Mb
        help   3.3Mb
    ```

# thematic

<details>

* Version: 0.1.6
* GitHub: https://github.com/rstudio/thematic
* Source code: https://github.com/cran/thematic
* Date/Publication: 2024-07-29 15:50:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "thematic")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘thematic-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: sequential_gradient
    > ### Title: Control parameters of the sequential colorscale
    > ### Aliases: sequential_gradient
    > 
    > ### ** Examples
    > 
    > 
    > # Gradient from fg to accent
    > fg <- sequential_gradient(1, 0)
    > thematic_on("black", "white", "salmon", sequential = fg)
    > ggplot2::qplot(1:10, 1:10, color = 1:10)
    Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    Error in adjust_color(user_default$colour, bg, fg, accent) : 
      Internal error: adjust_color() expects an input of length 1
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> update_defaults -> adjust_color
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(thematic)
      > 
      > test_check("thematic")
      [ FAIL 9 | WARN 1 | SKIP 7 | PASS 27 ]
      
      ══ Skipped tests (7) ═══════════════════════════════════════════════════════════
    ...
       10.             └─base::Map(...)
       11.               └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       12.                 └─thematic (local) `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]])
       13.                   ├─ggplot2::update_geom_defaults(...)
       14.                   │ └─ggplot2:::update_defaults(geom, "Geom", new, env = parent.frame())
       15.                   └─thematic:::adjust_color(user_default$colour, bg, fg, accent)
      
      [ FAIL 9 | WARN 1 | SKIP 7 | PASS 27 ]
      Error: Test failures
      Execution halted
    ```

# Thermistor

<details>

* Version: 1.1.0
* GitHub: NA
* Source code: https://github.com/cran/Thermistor
* Date/Publication: 2024-04-05 15:43:02 UTC
* Number of recursive dependencies: 28

Run `revdepcheck::cloud_details(, "Thermistor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Thermistor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_voltageCurve
    > ### Title: Plot the V-DeltaT Curve
    > ### Aliases: plot_voltageCurve
    > 
    > ### ** Examples
    > 
    > ### only target curve
    ...
    > ThVal <- CompValues$ThVal
    > ThBeta <- CompValues$ThBeta
    > Vnew <- voltageCurve(Tdata, R_id, Res, ThVal, ThBeta)
    > plot_voltageCurve(Tdata, OnlyTarget = FALSE, Pdata = Vnew)
    Warning in ggplot2::geom_line(ggplot2::aes(x = xid, y = Vdata, colour = "target",  :
      Ignoring unknown aesthetics: shape
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# tidybayes

<details>

* Version: 3.0.6
* GitHub: https://github.com/mjskay/tidybayes
* Source code: https://github.com/cran/tidybayes
* Date/Publication: 2023-08-12 23:30:02 UTC
* Number of recursive dependencies: 200

Run `revdepcheck::cloud_details(, "tidybayes")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidybayes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare_levels
    > ### Title: Compare the value of draws of some variable from a Bayesian
    > ###   model for different levels of a factor
    > ### Aliases: compare_levels
    > ### Keywords: manip
    > 
    > ### ** Examples
    ...
     12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
     13. │       └─l$compute_geom_2(d, theme = plot$theme)
     14. │         └─ggplot2 (local) compute_geom_2(..., self = self)
     15. │           └─self$geom$use_defaults(...)
     16. └─base::.handleSimpleError(...)
     17.   └─rlang (local) h(simpleError(msg, call))
     18.     └─handlers[[1L]](cnd)
     19.       └─cli::cli_abort(...)
     20.         └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This is necessary because some tests fail otherwise; see https://github.com/hadley/testthat/issues/144
      > Sys.setenv("R_TESTS" = "")
      > 
      > library(testthat)
      > library(tidybayes)
      > 
      > test_check("tidybayes")
    ...
      • test.geom_interval/grouped-intervals-h-stat.svg
      • test.geom_pointinterval/grouped-pointintervals-h-stat.svg
      • test.stat_dist_slabinterval/ccdfintervalh-using-args.svg
      • test.stat_eye/one-parameter-horizontal-eye-mode-hdi.svg
      • test.stat_eye/one-parameter-horizontal-half-eye.svg
      • test.stat_eye/one-parameter-vertical-eye.svg
      • test.stat_eye/one-parameter-vertical-halfeye.svg
      • test.stat_eye/two-parameter-factor-horizontal-eye-fill.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tidy-brms.Rmd’
      ...
    +     ]) %>% median_qi(condition_mean = b_Intercept + r_condition, 
    +     .width = c(0.95, 0 .... [TRUNCATED] 
    
      When sourcing ‘tidy-brms.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ...
    
      When sourcing ‘tidybayes.R’:
    Error: error in evaluating the argument 'object' in selecting a method for function 'sampling': object 'ABC_stan' not found
    Execution halted
    
      ‘tidy-brms.Rmd’ using ‘UTF-8’... failed
      ‘tidy-posterior.Rmd’ using ‘UTF-8’... failed
      ‘tidy-rstanarm.Rmd’ using ‘UTF-8’... failed
      ‘tidybayes-residuals.Rmd’ using ‘UTF-8’... failed
      ‘tidybayes.Rmd’ using ‘UTF-8’... failed
    ```

# tidycat

<details>

* Version: 0.1.2
* GitHub: https://github.com/guyabel/tidycat
* Source code: https://github.com/cran/tidycat
* Date/Publication: 2021-08-02 04:20:01 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "tidycat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidycat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidy_categorical
    > ### Title: Expand broom::tidy() Outputs for Categorical Parameter Estimates
    > ### Aliases: tidy_categorical
    > 
    > ### ** Examples
    > 
    > # strip ordering in factors (currently ordered factor not supported)
    ...
    > ggplot(data = d0,
    +        mapping = aes(x = level, colour = reference,
    +                      y = estimate, ymin = conf.low, ymax = conf.high)) +
    +   facet_row(facets = vars(variable), scales = "free_x", space = "free") +
    +   geom_hline(yintercept = 0, linetype = "dashed") +
    +   geom_pointrange() +
    +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
    Error in space$x : $ operator is invalid for atomic vectors
    Calls: <Anonymous> ... <Anonymous> -> draw_panels -> <Anonymous> -> init_gtable
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘intro.Rmd’
      ...
    
    > library(ggforce)
    
    > ggplot(data = d0, mapping = aes(x = level, y = estimate, 
    +     colour = reference, ymin = conf.low, ymax = conf.high)) + 
    +     facet_col(facets =  .... [TRUNCATED] 
    
      When sourcing ‘intro.R’:
    Error: $ operator is invalid for atomic vectors
    Execution halted
    
      ‘intro.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘intro.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# tidyCDISC

<details>

* Version: 0.2.1
* GitHub: https://github.com/Biogen-Inc/tidyCDISC
* Source code: https://github.com/cran/tidyCDISC
* Date/Publication: 2023-03-16 14:20:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "tidyCDISC")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tidyCDISC)
      > library(shinyjs)
      
      Attaching package: 'shinyjs'
      
    ...
        6. ├─plotly::config(...)
        7. │ └─plotly:::modify_list(p$x$config, args)
        8. │   ├─utils::modifyList(x %||% list(), y %||% list(), ...)
        9. │   │ └─base::stopifnot(is.list(x), is.list(val))
       10. │   └─x %||% list()
       11. └─plotly::layout(...)
      
      [ FAIL 1 | WARN 1 | SKIP 15 | PASS 91 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        data   2.0Mb
        doc    1.8Mb
    ```

# tidydr

<details>

* Version: 0.0.5
* GitHub: https://github.com/YuLab-SMU/tidydr
* Source code: https://github.com/cran/tidydr
* Date/Publication: 2023-03-08 09:20:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "tidydr")` for more info

</details>

## Newly broken

*   checking whether package ‘tidydr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tidydr/new/tidydr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tidydr’ ...
** package ‘tidydr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'len0_null' not found
Error: unable to load R code in package ‘tidydr’
Execution halted
ERROR: lazy loading failed for package ‘tidydr’
* removing ‘/tmp/workdir/tidydr/new/tidydr.Rcheck/tidydr’


```
### CRAN

```
* installing *source* package ‘tidydr’ ...
** package ‘tidydr’ successfully unpacked and MD5 sums checked
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
* DONE (tidydr)


```
# tidysdm

<details>

* Version: 0.9.5
* GitHub: https://github.com/EvolEcolGroup/tidysdm
* Source code: https://github.com/cran/tidysdm
* Date/Publication: 2024-06-23 19:40:02 UTC
* Number of recursive dependencies: 180

Run `revdepcheck::cloud_details(, "tidysdm")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a0_tidysdm_overview.Rmd’
      ...
    > climate_vars <- names(climate_present)
    
    > lacerta_thin <- lacerta_thin %>% bind_cols(terra::extract(climate_present, 
    +     lacerta_thin, ID = FALSE))
    
    > lacerta_thin %>% plot_pres_vs_bg(class)
    
      When sourcing ‘a0_tidysdm_overview.R’:
    Error: object is not a unit
    Execution halted
    
      ‘a0_tidysdm_overview.Rmd’ using ‘UTF-8’... failed
      ‘a1_palaeodata_application.Rmd’ using ‘UTF-8’... OK
      ‘a2_tidymodels_additions.Rmd’ using ‘UTF-8’... OK
      ‘a3_troubleshooting.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a0_tidysdm_overview.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   2.5Mb
        doc    2.0Mb
    ```

# tidySEM

<details>

* Version: 0.2.7
* GitHub: https://github.com/cjvanlissa/tidySEM
* Source code: https://github.com/cran/tidySEM
* Date/Publication: 2024-06-04 09:46:01 UTC
* Number of recursive dependencies: 229

Run `revdepcheck::cloud_details(, "tidySEM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidySEM-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: edit_graph
    > ### Title: Edit graph elements
    > ### Aliases: edit_graph edit_nodes edit_edges
    > 
    > ### ** Examples
    > 
    > p <- prepare_graph(layout = get_layout("x", rows = 1))
    > p <- edit_graph(p, {colour = "blue"}, element = "nodes")
    > plot(p)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(tidySEM)
      Loading required package: OpenMx
      To take full advantage of multiple cores, use:
        mxOption(key='Number of Threads', value=parallel::detectCores()) #now
        Sys.setenv(OMP_NUM_THREADS=parallel::detectCores()) #before library(OpenMx)
      Registered S3 method overwritten by 'tidySEM':
    ...
       4.     └─tidySEM:::.plot_edges_internal(p, df_edges)
       5.       └─ggplot2:::`+.gg`(p, do.call(geom_path, argslist))
       6.         └─ggplot2:::add_ggplot(e1, e2, e2name)
       7.           ├─ggplot2::ggplot_add(object, p, objectname)
       8.           └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       9.             └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 17 | WARN 1 | SKIP 6 | PASS 65 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Plotting_graphs.Rmd’
      ...
    
    > suppressWarnings({
    +     HS.model <- " visual  =~ x1 + x2 + x3\n              textual =~ x4 + x5 + x6\n              speed   =~ x7 + x8 + x9 "
    +     .... [TRUNCATED] 
    
    > p <- graph_sem(model = fit, text_size = 2, fix_coord = TRUE)
    
    ...
    
      ‘Generating_syntax.Rmd’ using ‘UTF-8’... OK
      ‘Plotting_graphs.Rmd’ using ‘UTF-8’... failed
      ‘SMART_LCA_checklist.Rmd’ using ‘UTF-8’... OK
      ‘Tabulating_results.Rmd’ using ‘UTF-8’... OK
      ‘lca_confirmatory.Rmd’ using ‘UTF-8’... OK
      ‘lca_exploratory.Rmd’ using ‘UTF-8’... OK
      ‘lca_lcga.Rmd’ using ‘UTF-8’... OK
      ‘lca_ordinal.Rmd’ using ‘UTF-8’... OK
      ‘sem_graph.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Generating_syntax.Rmd’ using rmarkdown
    --- finished re-building ‘Generating_syntax.Rmd’
    
    --- re-building ‘Plotting_graphs.Rmd’ using rmarkdown
    
    Quitting from lines 77-80 [unnamed-chunk-5] (Plotting_graphs.Rmd)
    Error: processing vignette 'Plotting_graphs.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘Plotting_graphs.Rmd’
    ...
    Quitting from lines 48-51 [unnamed-chunk-3] (sem_graph.Rmd)
    Error: processing vignette 'sem_graph.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘sem_graph.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘Plotting_graphs.Rmd’ ‘sem_graph.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# tidytreatment

<details>

* Version: 0.2.2
* GitHub: https://github.com/bonStats/tidytreatment
* Source code: https://github.com/cran/tidytreatment
* Date/Publication: 2022-02-21 09:00:07 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "tidytreatment")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘use-tidytreatment-BART.Rmd’
      ...
    +     by = ".row") %>% ggplot() + stat_halfeye(aes(x = z, y = fit)) + 
    +     facet_wrap(~c1, l .... [TRUNCATED] 
    
      When sourcing ‘use-tidytreatment-BART.R’:
    Error: Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
        NULL, 0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 2.75, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 
    Execution halted
    
      ‘use-tidytreatment-BART.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘use-tidytreatment-BART.Rmd’ using rmarkdown
    
    Quitting from lines 163-177 [plot-tidy-bart] (use-tidytreatment-BART.Rmd)
    Error: processing vignette 'use-tidytreatment-BART.Rmd' failed with diagnostics:
    Problem while setting up geom aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error in `use_defaults()`:
    ! unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.75, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, 
    ...
        NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), NULL, 2, NULL, NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list("white", NA, NULL, NULL, TRUE), list(NULL, "grey20", NULL, NULL, TRUE), NULL, NULL, NULL, list("grey92", 
            NULL, NULL, NULL, FALSE, "grey92", TRUE), NULL, list(NULL, 0.5, NULL, NULL, FALSE, NULL, TRUE), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, TRUE), "topleft", 
        NULL, NULL, list("grey85", "grey20", NULL, NULL, TRUE), NULL, NULL, "on", "inside", list(NULL, NULL, "grey10", 0.8, NULL, NULL, NULL, NULL, c(4.4, 4.4, 4.4, 4.4), NULL, TRUE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
    --- failed re-building ‘use-tidytreatment-BART.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘use-tidytreatment-BART.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘bartMachine’
    ```

# timeplyr

<details>

* Version: 0.8.2
* GitHub: https://github.com/NicChr/timeplyr
* Source code: https://github.com/cran/timeplyr
* Date/Publication: 2024-08-17 13:40:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "timeplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘timeplyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: time_ggplot
    > ### Title: Quick time-series ggplot
    > ### Aliases: time_ggplot
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    ...
    > data.table::setDTthreads(threads = 2L)
    > collapse::set_collapse(nthreads = 1L)
    > ## End(Don't show)
    > # It's as easy as this
    > AirPassengers %>%
    +   ts_as_tibble() %>%
    +   time_ggplot(time, value)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: %>% ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# timetk

<details>

* Version: 2.9.0
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2023-10-31 22:30:02 UTC
* Number of recursive dependencies: 205

Run `revdepcheck::cloud_details(, "timetk")` for more info

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
        7. └─timetk:::plot_time_series.grouped_df(...)
        8.   ├─timetk::plot_time_series(...)
        9.   └─timetk:::plot_time_series.data.frame(...)
       10.     ├─plotly::ggplotly(g, dynamicTicks = TRUE)
       11.     └─plotly:::ggplotly.ggplot(g, dynamicTicks = TRUE)
       12.       └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 406 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

# tinyarray

<details>

* Version: 2.4.2
* GitHub: https://github.com/xjsun1221/tinyarray
* Source code: https://github.com/cran/tinyarray
* Date/Publication: 2024-06-13 14:20:02 UTC
* Number of recursive dependencies: 243

Run `revdepcheck::cloud_details(, "tinyarray")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tinyarray-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: box_surv
    > ### Title: box_surv
    > ### Aliases: box_surv
    > 
    > ### ** Examples
    > 
    > if(requireNamespace("ggpubr",quietly = TRUE)) {
    + k = box_surv(log2(exp_hub1+1),exprSet_hub1,meta1);k[[1]]
    + }else{
    +  warning("Package 'ggpubr' needed for this function to work.
    +         Please install it by install.packages('ggpubr')")
    + }
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: box_surv ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ComplexHeatmap’
    ```

# tipmap

<details>

* Version: 0.5.2
* GitHub: https://github.com/Boehringer-Ingelheim/tipmap
* Source code: https://github.com/cran/tipmap
* Date/Publication: 2023-08-14 10:30:03 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "tipmap")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘introduction.Rmd’
      ...
    
    MAP Prior MCMC sample:
                    mean    sd  2.5%  50% 97.5%
    theta_resp_pred 1.43 0.356 0.661 1.44   2.1
    
    > plot(map_mcmc)$forest_model
    
      When sourcing ‘introduction.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘expert_elicitation.Rmd’ using ‘UTF-8’... OK
      ‘introduction.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘expert_elicitation.Rmd’ using rmarkdown
    ```

# tornado

<details>

* Version: 0.1.3
* GitHub: https://github.com/bertcarnell/tornado
* Source code: https://github.com/cran/tornado
* Date/Publication: 2024-01-21 17:30:02 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "tornado")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tornado-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: importance.cv.glmnet
    > ### Title: Plot Variable Importance for a GLMNET model
    > ### Aliases: importance.cv.glmnet
    > 
    > ### ** Examples
    > 
    > if (requireNamespace("glmnet", quietly = TRUE))
    ...
    +   form <- formula(mpg ~ cyl*wt*hp)
    +   mf <- model.frame(form, data = mtcars)
    +   mm <- model.matrix(mf, mf)
    +   gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
    +   imp <- importance(gtest, mtcars, form, nperm = 50)
    +   plot(imp)
    + }
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > if (require(testthat))
      + {
      +   library(tornado)
      + 
      +   test_check("tornado")
      + }
      Loading required package: testthat
    ...
       2. └─tornado:::plot.tornado_plot(torn, plot = FALSE, xlabel = "Probability of Class 1")
       3.   └─ggplot2:::`+.gg`(...)
       4.     └─ggplot2:::add_ggplot(e1, e2, e2name)
       5.       ├─ggplot2::ggplot_add(object, p, objectname)
       6.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       7.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 28 | WARN 14 | SKIP 0 | PASS 86 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘tornadoVignette.Rmd’
      ...
    > lm1 <- lm(mpg ~ cyl * wt * hp, data = mtcars)
    
    > torn1 <- tornado::tornado(lm1, type = "PercentChange", 
    +     alpha = 0.1)
    
    > plot(torn1, xlabel = "MPG", geom_bar_control = list(width = 0.4))
    
      When sourcing ‘tornadoVignette.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘tornadoVignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘tornadoVignette.Rmd’ using rmarkdown
    
    Quitting from lines 106-109 [lm1] (tornadoVignette.Rmd)
    Error: processing vignette 'tornadoVignette.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘tornadoVignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tornadoVignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# TOSTER

<details>

* Version: 0.8.3
* GitHub: NA
* Source code: https://github.com/cran/TOSTER
* Date/Publication: 2024-05-08 16:40:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "TOSTER")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TOSTER-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dataTOSTone
    > ### Title: TOST One Sample T-Test
    > ### Aliases: dataTOSTone
    > 
    > ### ** Examples
    > 
    > library("TOSTER")
    ...
                      N      Mean        Median      SD           SE           
     ───────────────────────────────────────────────────────────────────────── 
       Sepal.Width    150    3.057333    3.000000    0.4358663    0.03558833   
     ───────────────────────────────────────────────────────────────────────── 
    
    Error in use_defaults(..., self = self) : 
      unused argument (theme = list(list("black", 0.727272727272727, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.727272727272727, 1, TRUE), list("", "plain", "black", 16, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.727272727272727, 0.727272727272727, 1, 1, "", 5.62335685623357, 2.18181818181818, 19, TRUE), 8, c(8, 8, 8, 8), NULL, NULL, list(NULL, NULL, "#333333", NULL, NULL, NULL, NULL, 
        NULL, c(10, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 4, 0), NULL, TRUE), NULL, list(NULL, NULL, "#333333", NULL, NULL, NULL, 90, NULL, c(0, 10, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 4), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, "#333333", NULL, NULL, NULL, NULL, NULL, c(5, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NU
    Calls: <Anonymous> ... <Anonymous> -> <Anonymous> -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(TOSTER)
      
      Attaching package: 'TOSTER'
      
      The following object is masked from 'package:testthat':
    ...
       26.                                     └─base::Map(...)
       27.                                       └─base::mapply(FUN = f, ..., SIMPLIFY = FALSE)
       28.                                         └─ggplot2 (local) `<fn>`(layer = dots[[1L]][[1L]], df = dots[[2L]][[1L]])
       29.                                           └─layer$compute_geom_2(key, single_params, theme)
       30.                                             └─ggplot2 (local) compute_geom_2(..., self = self)
       31.                                               └─self$geom$use_defaults(...)
      
      [ FAIL 8 | WARN 0 | SKIP 0 | PASS 1034 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘IntroTOSTt.Rmd’
      ...
    mean of x mean of y 
         0.75      2.33 
    
    
    > plot(res1, type = "cd")
    
      When sourcing ‘IntroTOSTt.R’:
    ...
    Error: unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 
        0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 7, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, "bold", NULL, 11, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(), NULL, list
    Execution halted
    
      ‘IntroTOSTt.Rmd’ using ‘UTF-8’... failed
      ‘IntroductionToTOSTER.Rmd’ using ‘UTF-8’... OK
      ‘SMD_calcs.Rmd’ using ‘UTF-8’... OK
      ‘correlations.Rmd’ using ‘UTF-8’... OK
      ‘robustTOST.Rmd’ using ‘UTF-8’... failed
      ‘the_ftestTOSTER.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘IntroTOSTt.Rmd’ using rmarkdown
    ```

# TreatmentPatterns

<details>

* Version: 2.6.9
* GitHub: https://github.com/darwin-eu/TreatmentPatterns
* Source code: https://github.com/cran/TreatmentPatterns
* Date/Publication: 2024-09-02 12:40:06 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "TreatmentPatterns")` for more info

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
       22. ├─testthat::expect_s3_class(output$charAgePlot$html, "html") at test-CharacterizationPlots.R:50:9
       23. │ └─testthat::quasi_label(enquo(object), arg = "object")
       24. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       25. ├─output$charAgePlot
       26. └─shiny:::`$.shinyoutput`(output, charAgePlot)
       27.   └─.subset2(x, "impl")$getOutput(name)
      
      [ FAIL 1 | WARN 39 | SKIP 21 | PASS 138 ]
      Error: Test failures
      Execution halted
    ```

# TreatmentSelection

<details>

* Version: 2.1.1
* GitHub: NA
* Source code: https://github.com/cran/TreatmentSelection
* Date/Publication: 2017-08-11 18:55:47 UTC
* Number of recursive dependencies: 30

Run `revdepcheck::cloud_details(, "TreatmentSelection")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TreatmentSelection-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: compare.trtsel
    > ### Title: compare the performance of two treatment selection markers
    > ### Aliases: compare.trtsel
    > 
    > ### ** Examples
    > 
    > 
    ...
    > # Plot treatment effect curves with pointwise confidence intervals
    > ## use more bootstraps in practice
    > compare(x = trtsel.Y1, x2 = trtsel.Y2,
    +                                 bootstraps = 10, plot = TRUE,      
    +                                 ci = "horizontal",  conf.bands = TRUE) 
    Bootstrap bias-correction will be implemented to correct for over-optimism bias in estimation.
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: compare ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# TreeDep

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/TreeDep
* Date/Publication: 2018-12-02 17:50:03 UTC
* Number of recursive dependencies: 32

Run `revdepcheck::cloud_details(, "TreeDep")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TreeDep-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: TreeDep_plot
    > ### Title: TreeDep_plot - Generates a plot for selected variables and
    > ###   dates.
    > ### Aliases: TreeDep_plot
    > 
    > ### ** Examples
    > 
    ...
    + start_day = 25,
    + stop_day = 3)
    Warning: Removed 12 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Warning: Removed 12 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

# TreeDist

<details>

* Version: 2.9.1
* GitHub: https://github.com/ms609/TreeDist
* Source code: https://github.com/cran/TreeDist
* Date/Publication: 2024-09-07 09:20:02 UTC
* Number of recursive dependencies: 229

Run `revdepcheck::cloud_details(, "TreeDist")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘treespace.Rmd’
      ...
    [1] "Epoch: 3 finished. 212 datapoints changed bestmatch"
    [1] "Epoch: 4 started"
    [1] "Epoch: 4 finished. 203 datapoints changed bestmatch"
    [1] "Epoch: 5 started"
    [1] "Epoch: 5 finished. 165 datapoints changed bestmatch"
    [1] "---- Esom Training Finished ----"
    
    ...
    
      ‘Generalized-RF.Rmd’ using ‘UTF-8’... OK
      ‘Robinson-Foulds.Rmd’ using ‘UTF-8’... OK
      ‘Using-TreeDist.Rmd’ using ‘UTF-8’... OK
      ‘compare-treesets.Rmd’ using ‘UTF-8’... OK
      ‘different-leaves.Rmd’ using ‘UTF-8’... OK
      ‘information.Rmd’ using ‘UTF-8’... OK
      ‘landscapes.Rmd’ using ‘UTF-8’... OK
      ‘treespace.Rmd’ using ‘UTF-8’... failed
      ‘using-distances.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Generalized-RF.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        doc    1.2Mb
        libs   5.9Mb
    ```

# treeheatr

<details>

* Version: 0.2.1
* GitHub: https://github.com/trang1618/treeheatr
* Source code: https://github.com/cran/treeheatr
* Date/Publication: 2020-11-19 21:00:03 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "treeheatr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘treeheatr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: draw_tree
    > ### Title: Draws the conditional decision tree.
    > ### Aliases: draw_tree
    > 
    > ### ** Examples
    > 
    > x <- compute_tree(penguins, target_lab = 'species')
    > draw_tree(x$dat, x$fit, x$term_dat, x$layout)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: draw_tree ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘explore.Rmd’
      ...
    
    > library(treeheatr)
    
    > penguins <- na.omit(penguins)
    
    > heat_tree(penguins, target_lab = "species")
    
      When sourcing ‘explore.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘explore.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘explore.Rmd’ using rmarkdown
    
    Quitting from lines 33-36 [unnamed-chunk-2] (explore.Rmd)
    Error: processing vignette 'explore.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘explore.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘explore.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# trelliscopejs

<details>

* Version: 0.2.6
* GitHub: https://github.com/hafen/trelliscopejs
* Source code: https://github.com/cran/trelliscopejs
* Date/Publication: 2021-02-01 08:00:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "trelliscopejs")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(trelliscopejs)
      > 
      > test_check("trelliscopejs")
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 0 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
    ...
       4. └─base::lapply(...)
       5.   └─trelliscopejs (local) FUN(X[[i]], ...)
       6.     ├─base::do.call(plotly::ggplotly, c(list(p = q), plotly_args))
       7.     ├─plotly (local) `<fn>`(p = `<gg>`)
       8.     └─plotly:::ggplotly.ggplot(p = `<gg>`)
       9.       └─plotly::gg2list(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

# tricolore

<details>

* Version: 1.2.4
* GitHub: https://github.com/jschoeley/tricolore
* Source code: https://github.com/cran/tricolore
* Date/Publication: 2024-05-15 15:00:02 UTC
* Number of recursive dependencies: 108

Run `revdepcheck::cloud_details(, "tricolore")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tricolore-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ColorKeySextant
    > ### Title: Sextant Scheme Legend
    > ### Aliases: ColorKeySextant
    > ### Keywords: internal
    > 
    > ### ** Examples
    > 
    ...
      3.   ├─ggtern::ggplot_build(x)
      4.   └─ggtern:::ggplot_build.ggplot(x)
      5.     └─ggtern:::layers_add_or_remove_mask(plot)
      6.       └─ggint$plot_theme(plot)
      7.         └─ggplot2:::validate_theme(theme)
      8.           └─base::mapply(...)
      9.             └─ggplot2 (local) `<fn>`(...)
     10.               └─cli::cli_abort(...)
     11.                 └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘choropleth_maps_with_tricolore.Rmd’
      ...
    
    > plot_educ <- ggplot(euro_example) + geom_sf(aes(fill = rgb, 
    +     geometry = geometry), size = 0.1) + scale_fill_identity()
    
    > plot_educ
    
      When sourcing ‘choropleth_maps_with_tricolore.R’:
    Error: The `tern.axis.ticks.length.major` theme element must be a <unit>
    object.
    Execution halted
    
      ‘choropleth_maps_with_tricolore.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘choropleth_maps_with_tricolore.Rmd’ using rmarkdown
    
    Quitting from lines 61-72 [unnamed-chunk-4] (choropleth_maps_with_tricolore.Rmd)
    Error: processing vignette 'choropleth_maps_with_tricolore.Rmd' failed with diagnostics:
    The `tern.axis.ticks.length.major` theme element must be a <unit>
    object.
    --- failed re-building ‘choropleth_maps_with_tricolore.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘choropleth_maps_with_tricolore.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# triptych

<details>

* Version: 0.1.3
* GitHub: https://github.com/aijordan/triptych
* Source code: https://github.com/cran/triptych
* Date/Publication: 2024-06-13 15:50:02 UTC
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "triptych")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘triptych-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.triptych
    > ### Title: Plot methods for the triptych classes
    > ### Aliases: plot.triptych autoplot.triptych plot.triptych_murphy
    > ###   autoplot.triptych_murphy plot.triptych_reliability
    > ###   autoplot.triptych_reliability plot.triptych_roc autoplot.triptych_roc
    > ###   plot.triptych_mcbdsc autoplot.triptych_mcbdsc
    > 
    > ### ** Examples
    > 
    > data(ex_binary, package = "triptych")
    > tr <- triptych(ex_binary)
    > 
    > dplyr::slice(tr, 1, 3, 6, 9) |> autoplot()
    Error in identicalUnits(x) : object is not a unit
    Calls: <Anonymous> ... assemble_guides -> guides_build -> unit.c -> identicalUnits
    Execution halted
    ```

# tsnet

<details>

* Version: 0.1.0
* GitHub: https://github.com/bsiepe/tsnet
* Source code: https://github.com/cran/tsnet
* Date/Publication: 2024-02-28 11:30:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "tsnet")` for more info

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
        unused argument (theme = list(list("black", 0.5, 1, "butt", FALSE, "black", TRUE), list("white", "black", 0.5, 1, TRUE), list("", "plain", "black", 11, 0.5, 0.5, 0, 0.9, c(0, 0, 0, 0), FALSE, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list("black", "white", "#3366FF", 0.5, 0.5, 1, 1, "", 3.86605783866058, 1.5, 19, TRUE), 5.5, c(5.5, 5.5, 5.5, 5.5), NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(7, 0, 0, 0), NULL, FALSE), list(NULL, NULL, NULL, NULL, NULL, 
            0, NULL, NULL, c(0, 0, 2.75, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, 90, NULL, c(0, 7, 0, 0), NULL, FALSE), NULL, list(NULL, NULL, NULL, NULL, NULL, 1, -90, NULL, c(0, 0, 0, 2.75), NULL, TRUE), list(NULL, NULL, "grey30", 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 1, NULL, NULL, c(2.2, 0, 0, 0), NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, c(0, 0, 2.2, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 1, NULL, NULL, 
            NULL, c(0, 2.2, 0, 0), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, c(0, 0, 0, 2.2), NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, c(0, 2.2, 0, 2.2), NULL, TRUE), list("grey70", 0.5, NULL, NULL, FALSE, "grey70", TRUE), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.5, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.75, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(), list("gray70", 0.5, NULL, NULL, 
            FALSE, "gray70", FALSE), NULL, NULL, list("gray70", 0.5, NULL, NULL, FALSE, "gray70", FALSE), NULL, NULL, NULL, NULL, list(NULL, NA, NULL, NULL, TRUE), NULL, 2, NULL, NULL, NULL, 1.2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0.2, NULL, list(NULL, NULL, NULL, 0.8, NULL, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, list(NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, TRUE), NULL, "right", NULL, NULL, NULL, "center", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, list(), 2, list("white", 
            NA, NULL, NULL, TRUE), list(), NULL, NULL, NULL, list("grey87", NULL, NULL, NULL, FALSE, "grey87", TRUE), list(), list(), NULL, NULL, NULL, NULL, FALSE, list(NULL, "white", NULL, NULL, TRUE), list(NULL, NULL, NULL, 1.2, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, NULL, 0, 1, NULL, NULL, c(0, 0, 5.5, 0), NULL, TRUE), list(NULL, NULL, NULL, 0.8, 1, 1, NULL, NULL, c(5.5, 0, 0, 0), NULL, TRUE), "panel", list(NULL, NULL, NULL, 1.2, 0.5, 0.5, NULL, NULL, NULL, NULL, 
            TRUE), "topleft", NULL, NULL, list("gray90", NA, NULL, NULL, FALSE), NULL, NULL, "on", "inside", list(NULL, NULL, "black", 0.8, NULL, NULL, NULL, NULL, c(6, 6, 6, 6), NULL, FALSE), NULL, NULL, NULL, list(NULL, NULL, NULL, NULL, NULL, NULL, -90, NULL, NULL, NULL, TRUE), list(NULL, NULL, NULL, NULL, NULL, NULL, 90, NULL, NULL, NULL, TRUE), NULL, 2.75, 2.75))
      
      [ FAIL 1 | WARN 14 | SKIP 0 | PASS 108 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 163.4Mb
      sub-directories of 1Mb or more:
        libs  162.3Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# UBayFS

<details>

* Version: 1.0
* GitHub: https://github.com/annajenul/UBayFS
* Source code: https://github.com/cran/UBayFS
* Date/Publication: 2023-03-07 10:50:02 UTC
* Number of recursive dependencies: 188

Run `revdepcheck::cloud_details(, "UBayFS")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘BFS_UBayFS.Rmd’
      ...
     ( 21,23,28 )
     ( 21,27,28 )
     ( 21,23,24 )  
    
    > plot(model)
    [1] "Warning: multiple optimal feature sets, plotting first feature set."
    
    ...
      ( 2,3,7,8,14,22,23,26,27,28 )  
    
    > plot(model)
    
      When sourcing ‘UBayFS.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘BFS_UBayFS.Rmd’ using ‘UTF-8’... failed
      ‘UBayFS.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘BFS_UBayFS.Rmd’ using rmarkdown
    
    Quitting from lines 147-150 [unnamed-chunk-8] (BFS_UBayFS.Rmd)
    Error: processing vignette 'BFS_UBayFS.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘BFS_UBayFS.Rmd’
    
    --- re-building ‘UBayFS.Rmd’ using rmarkdown
    ...
    Quitting from lines 306-309 [unnamed-chunk-12] (UBayFS.Rmd)
    Error: processing vignette 'UBayFS.Rmd' failed with diagnostics:
    invalid line type: must be length 2, 4, 6 or 8
    --- failed re-building ‘UBayFS.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘BFS_UBayFS.Rmd’ ‘UBayFS.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# Umatrix

<details>

* Version: 4.0.1
* GitHub: NA
* Source code: https://github.com/cran/Umatrix
* Date/Publication: 2024-08-17 06:30:17 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "Umatrix")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Umatrix-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotMatrix
    > ### Title: plotMatrix
    > ### Aliases: plotMatrix
    > 
    > ### ** Examples
    > 
    > data("Hepta")
    ...
      4.   └─ggplot2:::ggplot_build.ggplot(x)
      5.     └─layout$setup(data, plot$data, plot$plot_env)
      6.       └─ggplot2 (local) setup(..., self = self)
      7.         └─self$coord$setup_params(data)
      8.           └─ggplot2 (local) setup_params(..., self = self)
      9.             └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
     10.               └─ggplot2:::check_logical(expand)
     11.                 └─ggplot2:::stop_input_type(...)
     12.                   └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

# umiAnalyzer

<details>

* Version: 1.0.0
* GitHub: https://github.com/sfilges/umiAnalyzer
* Source code: https://github.com/cran/umiAnalyzer
* Date/Publication: 2021-11-25 08:40:02 UTC
* Number of recursive dependencies: 116

Run `revdepcheck::cloud_details(, "umiAnalyzer")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘umiAnalyzer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: AmpliconPlot
    > ### Title: Generate Amplicon plots
    > ### Aliases: AmpliconPlot
    > 
    > ### ** Examples
    > 
    > library(umiAnalyzer)
    ...
    > 
    > main = system.file('extdata', package = 'umiAnalyzer')
    > samples <- list.dirs(path = main, full.names = FALSE, recursive = FALSE)
    > simsen <- createUmiExperiment(experimentName = 'example',mainDir = main,sampleNames = samples)
    > simsen <- filterUmiObject(simsen)
    > 
    > amplicon_plot <- AmpliconPlot(simsen)
    Error in pm[[2]] : subscript out of bounds
    Calls: AmpliconPlot -> <Anonymous> -> ggplotly.ggplot -> gg2list
    Execution halted
    ```

# UnalR

<details>

* Version: 1.0.0
* GitHub: https://github.com/estadisticaun/UnalR
* Source code: https://github.com/cran/UnalR
* Date/Publication: 2024-05-25 17:20:05 UTC
* Number of recursive dependencies: 168

Run `revdepcheck::cloud_details(, "UnalR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘UnalR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Plot.Apiladas
    > ### Title: Cree un gráfico de barras apiladas dinámico/estático y flexible
    > ### Aliases: Plot.Apiladas
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    ...
    +         legend.direction = "vertical"), gg.Bar = list(width = 0.6, color = "#000000"), 
    +         gg.Texto = list(subtitle = "»»»", tag = "®", caption = "Información Disponible desde 2009-1")))
    Warning: 
        ¡Ha ingresado un dataframe que no está de forma condensada, es decir,
        para cada categoría existe más de un valor para un mismo punto del eje X!
        Se sumará los valores por defectos para dichos puntos que gocen de +1 valor
               
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(UnalR)
      > 
      > test_check("UnalR")
      Starting 2 test processes
      [ FAIL 2 | WARN 4 | SKIP 1 | PASS 50 ]
      
    ...
        7. └─UnalR::Plot.Series(...)
        8.   └─ggplot2:::`+.gg`(...)
        9.     └─ggplot2:::add_ggplot(e1, e2, e2name)
       10.       ├─ggplot2::ggplot_add(object, p, objectname)
       11.       └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       12.         └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 2 | WARN 4 | SKIP 1 | PASS 50 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        R      2.3Mb
        data   2.0Mb
        help   2.6Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 312859 marked UTF-8 strings
    ```

# unmconf

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/unmconf
* Date/Publication: 2024-09-09 22:00:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "unmconf")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘unmconf.Rmd’
      ...
     9 -1.05      1  5.63   -0.447      1     1  1.55  TRUE        TRUE       
    10  0.620     0  1.66   -1.71       1     1  1.37  TRUE        TRUE       
    
    > bayesplot::mcmc_intervals(unm_mod, prob_outer = 0.95, 
    +     regex_pars = "(beta|lambda|gamma|delta|zeta).+") + geom_point(aes(value, 
    +     name),  .... [TRUNCATED] 
    
      When sourcing ‘unmconf.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘unmconf.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘unmconf.Rmd’ using rmarkdown
    ```

# usmap

<details>

* Version: 0.7.1
* GitHub: https://github.com/pdil/usmap
* Source code: https://github.com/cran/usmap
* Date/Publication: 2024-03-21 04:20:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "usmap")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘usmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: .east_north_central
    > ### Title: East North Central census division
    > ### Aliases: .east_north_central
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > plot_usmap(include = .east_north_central, labels = TRUE)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: plot_usmap ... ggplot_add.list -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

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
      • data/statepov.svg
      • plot/arizona-county-map-with-labels-and-fill.svg
      • plot/example-data-state-map-with-custom-linewidth.svg
      • plot/new-england-state-map-with-labels-excluding-maine.svg
      • plot/southeastern-states-map-with-labels.svg
      • plot/state-map-with-labels.svg
      • plot/state-map-with-major-rivers.svg
      • plot/state-population-map-with-blue-outlines.svg
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘usmap1.Rmd’
      ...
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
    
    > usmap::plot_usmap()
    
      When sourcing ‘usmap1.R’:
    Error: argument is of length zero
    ...
    
    > usmap::plot_usmap("states", labels = TRUE)
    
      When sourcing ‘usmap3.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘usmap1.Rmd’ using ‘UTF-8’... failed
      ‘usmap2.Rmd’ using ‘UTF-8’... failed
      ‘usmap3.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘usmap1.Rmd’ using rmarkdown
    
    Quitting from lines 26-27 [unnamed-chunk-1] (usmap1.Rmd)
    Error: processing vignette 'usmap1.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘usmap1.Rmd’
    
    --- re-building ‘usmap2.Rmd’ using rmarkdown
    
    ...
    Quitting from lines 26-27 [unnamed-chunk-1] (usmap3.Rmd)
    Error: processing vignette 'usmap3.Rmd' failed with diagnostics:
    argument is of length zero
    --- failed re-building ‘usmap3.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘usmap1.Rmd’ ‘usmap2.Rmd’ ‘usmap3.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 17 marked UTF-8 strings
    ```

# vannstats

<details>

* Version: 1.3.4.14
* GitHub: NA
* Source code: https://github.com/cran/vannstats
* Date/Publication: 2023-04-15 04:30:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "vannstats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vannstats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qq
    > ### Title: Simplified Normal (Q-Q) Plot
    > ### Aliases: qq
    > 
    > ### ** Examples
    > 
    > data <- mtcars
    > 
    > qq(data,mpg,cyl)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: qq ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# vDiveR

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/vDiveR
* Date/Publication: 2024-01-09 20:20:02 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "vDiveR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vDiveR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_entropy
    > ### Title: Entropy plot
    > ### Aliases: plot_entropy
    > 
    > ### ** Examples
    > 
    > plot_entropy(proteins_1host)
    Scale for colour is already present.
    Adding another scale for colour, which will replace the existing scale.
    Scale for linetype is already present.
    Adding another scale for linetype, which will replace the existing scale.
    Error in grid.Call.graphics(C_lines, x$x, x$y, index, x$arrow) : 
      invalid line type: must be length 2, 4, 6 or 8
    Calls: <Anonymous> ... drawDetails -> drawDetails.polyline -> grid.Call.graphics
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘maps’ ‘readr’
      All declared Imports should be used.
    ```

# venn

<details>

* Version: 1.12
* GitHub: https://github.com/dusadrian/venn
* Source code: https://github.com/cran/venn
* Date/Publication: 2024-01-08 11:40:05 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "venn")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘venn-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: venn
    > ### Title: Draw and display a Venn diagram
    > ### Aliases: venn
    > ### Keywords: functions
    > 
    > ### ** Examples
    > 
    ...
    > 
    > 
    > # producing a ggplot2 graphics
    > venn(x, ilabels = "counts", ggplot = TRUE)
    > 
    > # increasing the border size
    > venn(x, ilabels = "counts", ggplot = TRUE, size = 1.5)
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: venn ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# vimpclust

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/vimpclust
* Date/Publication: 2021-01-08 09:30:03 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "vimpclust")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘sparsewkm.Rmd’
      ...
    restecg   3.1e-02    0.000    0.000    0.000    0.000
    exang     2.3e-01    0.182    0.138    0.106    0.046
    slope     3.2e-01    0.231    0.188    0.151    0.077
    thal      1.2e-01    0.058    0.023    0.000    0.000
    
    > plot(res, what = "weights.features")
    
      When sourcing ‘sparsewkm.R’:
    Error: invalid line type: must be length 2, 4, 6 or 8
    Execution halted
    
      ‘groupsparsewkm.Rmd’ using ‘UTF-8’... OK
      ‘sparsewkm.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘groupsparsewkm.Rmd’ using rmarkdown
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘mclust’ ‘rlang’
      All declared Imports should be used.
    ```

# vip

<details>

* Version: 0.4.1
* GitHub: https://github.com/koalaverse/vip
* Source code: https://github.com/cran/vip
* Date/Publication: 2023-08-21 09:20:02 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "vip")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vip-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vi
    > ### Title: Variable importance
    > ### Aliases: vi vi.default
    > 
    > ### ** Examples
    > 
    > #
    ...
     8 drat       0.0265   0.0564 
     9 carb       0.00898  0.00885
    10 disp      -0.000824 0.00744
    > 
    > # Plot variable importance scores
    > vip(vis, include_type = TRUE, all_permutations = TRUE,
    +     geom = "point", aesthetics = list(color = "forestgreen", size = 3))
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: vip ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > if (requireNamespace("tinytest", quietly = TRUE)) {
      +   home <- length(unclass(packageVersion("vip"))[[1L]]) == 4
      +   tinytest::test_package("vip", at_home = home)
      + }
      
      Attaching package: 'vip'
      
    ...
      test_pkg_nnet.R...............    0 tests    
      test_pkg_nnet.R...............    0 tests    
      test_pkg_nnet.R...............    0 tests    
      test_pkg_nnet.R...............    0 tests    
      test_pkg_nnet.R...............    0 tests    
      test_pkg_nnet.R...............    1 tests [0;32mOK[0m 
      test_pkg_nnet.R...............    2 tests [0;32mOK[0m 
      test_pkg_nnet.R...............    3 tests [0;32mOK[0m Error in if (new_name %in% existing) { : argument is of length zero
      Calls: <Anonymous> ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'C50', 'caret', 'Cubist', 'earth', 'gbm', 'glmnet', 'h2o',
      'lightgbm', 'mixOmics', 'mlr', 'mlr3', 'neuralnet', 'parsnip',
      'partykit', 'pls', 'randomForest', 'ranger', 'RSNNS', 'sparklyr',
      'tidymodels', 'workflows', 'xgboost'
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘randomForest’, ‘glmnet’, ‘C50’, ‘Cubist’, ‘caret’, ‘partykit’, ‘earth’, ‘gbm’, ‘h2o’, ‘sparklyr’, ‘ranger’, ‘xgboost’, ‘lightgbm’
    ```

# VirtualPop

<details>

* Version: 2.0.2
* GitHub: https://github.com/willekens/VirtualPop
* Source code: https://github.com/cran/VirtualPop
* Date/Publication: 2024-03-18 10:30:02 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "VirtualPop")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Piecewise_exponential.Rmd’
      ...
    > H <- VirtualPop::H_pw(x, breakpoints, rates)
    
    > dd <- data.frame(x = x, y = exp(-H))
    
    > p <- survminer::ggsurvplot(KM, data = data.frame(pw_sample), 
    +     conf.int = TRUE, ggtheme = theme_bw())
    
      When sourcing ‘Piecewise_exponential.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘MultistateLH.Rmd’ using ‘UTF-8’... OK
      ‘Piecewise_exponential.Rmd’ using ‘UTF-8’... failed
      ‘Tutorial.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MultistateLH.Rmd’ using knitr
    --- finished re-building ‘MultistateLH.Rmd’
    
    --- re-building ‘Piecewise_exponential.Rmd’ using knitr
    ```

# viscomp

<details>

* Version: 1.0.0
* GitHub: https://github.com/georgiosseitidis/viscomp
* Source code: https://github.com/cran/viscomp
* Date/Publication: 2023-01-16 09:50:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "viscomp")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘viscomp-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: loccos
    > ### Title: Leaving One Component Combination Out Scatter plot
    > ### Aliases: loccos
    > 
    > ### ** Examples
    > 
    > data(nmaMACE)
    > loccos(model = nmaMACE, combination = c("B"))
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: loccos ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

# vivaldi

<details>

* Version: 1.0.1
* GitHub: https://github.com/GreshamLab/vivaldi
* Source code: https://github.com/cran/vivaldi
* Date/Publication: 2023-03-21 20:10:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "vivaldi")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vivaldi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: snv_location
    > ### Title: snv_location
    > ### Aliases: snv_location
    > 
    > ### ** Examples
    > 
    > # Example 1:
    ...
    6      m2   PB1 234     G     A    minor     0.010     0.990
    7      m2   PB1 266     G     A    minor     0.022     0.978
    8      m2   PB2 199     A     G    minor     0.043     0.957
    9      m2   PB2  88     G     A    major     0.055     0.945
    10     m2   PB2 180     C     T    minor     0.011     0.989
    > 
    > snv_location(df)
    Error in pm[[2]] : subscript out of bounds
    Calls: snv_location -> <Anonymous> -> ggplotly.ggplot -> gg2list
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
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-snv_location.R:13:3'): expect output ─────────────────────────
      Expected `snv_location(df)` to run without any errors.
      i Actually got a <subscriptOutOfBoundsError> with text:
        subscript out of bounds
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 29 ]
      Error: Test failures
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘vignette.Rmd’
      ...
    |a_3_fb |        96|
    |a_3_iv |        94|
    |b_1_fb |        82|
    |b_1_iv |        91|
    
    > snv_location(DF_filt_SNVs)
    
      When sourcing ‘vignette.R’:
    Error: subscript out of bounds
    Execution halted
    
      ‘vignette.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘vignette.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        doc       5.4Mb
        extdata   1.1Mb
    ```

# voiceR

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/voiceR
* Date/Publication: 2023-09-12 20:30:02 UTC
* Number of recursive dependencies: 179

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
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: comparisonPlots ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
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
       4.   └─(function() {...
       5.     └─ggplot2:::`+.gg`(...)
       6.       └─ggplot2:::add_ggplot(e1, e2, e2name)
       7.         ├─ggplot2::ggplot_add(object, p, objectname)
       8.         └─ggplot2:::ggplot_add.Layer(object, p, objectname)
       9.           └─ggplot2:::new_layer_names(object, names(plot$layers))
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 30 ]
      Error: Test failures
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
* Number of recursive dependencies: 166

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
     11. │           └─ggplot2:::`+.gg`(...)
     12. │             └─ggplot2:::add_ggplot(e1, e2, e2name)
     13. │               ├─ggplot2::ggplot_add(object, p, objectname)
     14. │               └─ggplot2:::ggplot_add.Layer(object, p, objectname)
     15. │                 └─ggplot2:::new_layer_names(object, names(plot$layers))
     16. └─base::.handleSimpleError(...)
     17.   └─purrr (local) h(simpleError(msg, call))
     18.     └─cli::cli_abort(...)
     19.       └─rlang::abort(...)
    Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘Vignette.rmd’
      ...
    > plot1 <- boxplot_trio(syn_polar, value = "COBL", text_size = 7, 
    +     test = "polar_padj", my_comparisons = list(c("Lymphoid", 
    +         "Myeloid" .... [TRUNCATED] 
    
      When sourcing ‘Vignette.R’:
    Error: ℹ In index: 1.
    ℹ With name: row.
    Caused by error in `if (new_name %in% existing) ...`:
    ! argument is of length zero
    Execution halted
    
      ‘Vignette.rmd’ using ‘UTF-8’... failed
      ‘Vignette_2x3.Rmd’ using ‘UTF-8’... OK
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Vignette.rmd’ using rmarkdown
    ```

# voluModel

<details>

* Version: 0.2.2
* GitHub: https://github.com/hannahlowens/voluModel
* Source code: https://github.com/cran/voluModel
* Date/Publication: 2024-08-20 22:50:01 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "voluModel")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘voluModel-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: pointCompMap
    > ### Title: Comparative point mapping
    > ### Aliases: pointCompMap
    > ### Keywords: plotting
    > 
    > ### ** Examples
    > 
    ...
      6.       └─ggplot2 (local) setup(..., self = self)
      7.         └─self$coord$setup_params(data)
      8.           └─ggplot2 (local) setup_params(..., self = self)
      9.             └─ggproto_parent(Coord, self)$setup_params(data)
     10.               └─ggplot2 (local) setup_params(..., self = self)
     11.                 └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
     12.                   └─ggplot2:::check_logical(expand)
     13.                     └─ggplot2:::stop_input_type(...)
     14.                       └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘a_Introduction.Rmd’ using rmarkdown
    
    Quitting from lines 44-58 [show points] (a_Introduction.Rmd)
    Error: processing vignette 'a_Introduction.Rmd' failed with diagnostics:
    `expand` must be a logical vector, not the number 0.05.
    --- failed re-building ‘a_Introduction.Rmd’
    
    --- re-building ‘b_RasterProcessing.Rmd’ using rmarkdown
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘a_Introduction.Rmd’
      ...
    +     returnclass = "sf")[1]
    
    > pointMap(occs = occurrences, ptCol = "orange", landCol = "black", 
    +     spName = "Steindachneria argentea", ptSize = 3, land = land)
    Using decimalLongitude and decimalLatitude
     as x and y coordinates, respectively.
    
    ...
    
      When sourcing ‘e_GLMWorkflow.R’:
    Error: invalid font type
    Execution halted
    
      ‘a_Introduction.Rmd’ using ‘UTF-8’... failed
      ‘b_RasterProcessing.Rmd’ using ‘UTF-8’... OK
      ‘c_DataSampling.Rmd’ using ‘UTF-8’... failed
      ‘d_Visualization.Rmd’ using ‘UTF-8’... failed
      ‘e_GLMWorkflow.Rmd’ using ‘UTF-8’... failed
    ```

# vsd

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/vsd
* Date/Publication: 2021-05-11 09:40:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "vsd")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vsd-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vsd
    > ### Title: Visualizing Survival Data
    > ### Aliases: vsd vsd.formula vsd.Surv vsd.coxph vsd.survfit vsd.survfitcox
    > ###   vsd.flexsurvreg
    > 
    > ### ** Examples
    > 
    ...
    • `linetype = "Strata"`
    • `shape = "Strata"`
    
    > 
    > # parametric models are also supported with flexsurv
    > vsd(flexsurv::flexsurvreg(Surv(rectime, censrec) ~ group, data = flexsurv::bc, dist = 'gengamma'),
    +     .include = c("par"))
    Error in if (new_name %in% existing) { : argument is of length zero
    Calls: vsd ... add_ggplot -> ggplot_add -> ggplot_add.Layer -> new_layer_names
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘flexsurv’
      All declared Imports should be used.
    ```

# vvshiny

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/vvshiny
* Date/Publication: 2023-07-19 15:30:02 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "vvshiny")` for more info

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
       1. ├─vvshiny::ggplotly_with_legend(p, color = "grp", mapping_table = list(grp = "Group")) at test-ggplotly_with_legend.R:15:3
       2. │ ├─plotly::ggplotly(plot) %>% ...
       3. │ ├─plotly::ggplotly(plot)
       4. │ └─plotly:::ggplotly.ggplot(plot)
       5. │   └─plotly::gg2list(...)
       6. └─plotly::layout(...)
      
      [ FAIL 1 | WARN 2 | SKIP 0 | PASS 60 ]
      Error: Test failures
      Execution halted
    ```

# walker

<details>

* Version: 1.0.10
* GitHub: https://github.com/helske/walker
* Source code: https://github.com/cran/walker
* Date/Publication: 2024-08-30 06:40:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "walker")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘walker.Rmd’
      ...
    For each parameter, n_eff is a crude measure of effective sample size,
    and Rhat is the potential scale reduction factor on split chains (at 
    convergence, Rhat=1).
    
    > mcmc_areas(as.matrix(fit$stanfit), regex_pars = c("sigma_y", 
    +     "sigma_rw1"))
    
      When sourcing ‘walker.R’:
    Error: argument is of length zero
    Execution halted
    
      ‘walker.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘walker.Rmd’ using rmarkdown
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 161.5Mb
      sub-directories of 1Mb or more:
        libs  160.1Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# WVPlots

<details>

* Version: 1.3.8
* GitHub: https://github.com/WinVector/WVPlots
* Source code: https://github.com/cran/WVPlots
* Date/Publication: 2024-04-22 20:40:07 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "WVPlots")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘WVPlots-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ScatterHist
    > ### Title: Plot a scatter plot with marginals.
    > ### Aliases: ScatterHist
    > 
    > ### ** Examples
    > 
    > 
    ...
     10.           └─ggplot2:::ggplot_build.ggplot(x)
     11.             └─layout$setup(data, plot$data, plot$plot_env)
     12.               └─ggplot2 (local) setup(..., self = self)
     13.                 └─self$coord$setup_params(data)
     14.                   └─ggplot2 (local) setup_params(..., self = self)
     15.                     └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
     16.                       └─ggplot2:::check_logical(expand)
     17.                         └─ggplot2:::stop_input_type(...)
     18.                           └─rlang::abort(message, ..., call = call, arg = arg)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘tinytest.R’
    Running the tests in ‘tests/tinytest.R’ failed.
    Complete output:
      > 
      > if (requireNamespace("tinytest", quietly=TRUE) ) {
      +   if (requireNamespace('data.table', quietly = TRUE)) {
      +     # don't multi-thread during CRAN checks
      +     data.table::setDTthreads(1)
      +   }
      +   tinytest::test_package("WVPlots")
    ...
       18.                           └─ggplot2:::ggplot_build.ggplot(x)
       19.                             └─layout$setup(data, plot$data, plot$plot_env)
       20.                               └─ggplot2 (local) setup(..., self = self)
       21.                                 └─self$coord$setup_params(data)
       22.                                   └─ggplot2 (local) setup_params(..., self = self)
       23.                                     └─ggplot2:::parse_coord_expand(expand = self$expand %||% TRUE)
       24.                                       └─ggplot2:::check_logical(expand)
       25.                                         └─ggplot2:::stop_input_type(...)
       26.                                           └─rlang::abort(message, ..., call = call, arg = arg)
      Execution halted
    ```

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘WVPlots_concept.Rmd’
      ...
    > frm$absY <- abs(frm$y)
    
    > frm$posY = frm$y > 0
    
    > WVPlots::ScatterHist(frm, "x", "y", smoothmethod = "lm", 
    +     title = "Example Linear Fit")
    
    ...
    > frm$posY = frm$y > 0
    
    > WVPlots::ScatterHist(frm, "x", "y", title = "Example Fit")
    
      When sourcing ‘WVPlots_examples.R’:
    Error: `expand` must be a logical vector, not the number 0.
    Execution halted
    
      ‘WVPlots_concept.Rmd’ using ‘UTF-8’... failed
      ‘WVPlots_examples.Rmd’ using ‘UTF-8’... failed
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘WVPlots_concept.Rmd’ using rmarkdown
    ```

# xaringanthemer

<details>

* Version: 0.4.2
* GitHub: https://github.com/gadenbuie/xaringanthemer
* Source code: https://github.com/cran/xaringanthemer
* Date/Publication: 2022-08-20 18:40:02 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "xaringanthemer")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(xaringanthemer)
      > 
      > test_check("xaringanthemer")
      [ FAIL 1 | WARN 18 | SKIP 1 | PASS 308 ]
      
      ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-ggplot2.R:267:3'): theme_xaringan_restore_defaults() restores defaults ──
      res$after_restore$line_colour (`actual`) not equal to res$original$colour (`expected`).
      
      `actual` is a character vector ('#0088ff')
      `expected` is an S3 object of class <quosure/formula>, a call
      
      [ FAIL 1 | WARN 18 | SKIP 1 | PASS 308 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘xaringanthemer.Rmd’
      ...
    Warning in file(con, "r") :
      cannot open file './../man/fragments/_quick-intro.Rmd': No such file or directory
    
    Quitting from lines 43-43 [unnamed-chunk-2] (xaringanthemer.Rmd)
    
      When tangling ‘xaringanthemer.Rmd’:
    Error: cannot open the connection
    Execution halted
    
      ‘ggplot2-themes.Rmd’ using ‘UTF-8’... OK
      ‘template-variables.Rmd’ using ‘UTF-8’... OK
      ‘xaringanthemer.Rmd’ using ‘UTF-8’... failed
    ```

# yamlet

<details>

* Version: 1.0.3
* GitHub: https://github.com/bergsmat/yamlet
* Source code: https://github.com/cran/yamlet
* Date/Publication: 2024-03-29 13:30:02 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::cloud_details(, "yamlet")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘yamlet-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggplot_add.ggplot_isometric
    > ### Title: Add Isometry to Plot Object
    > ### Aliases: ggplot_add.ggplot_isometric
    > ### Keywords: internal
    > 
    > ### ** Examples
    > 
    ...
    ismtrc> library(magrittr)
    
    ismtrc> library(ggplot2)
    
    ismtrc> data.frame(x = 1:5, y = 3:7) %>%
    ismtrc+ ggplot(aes(x, y)) + geom_point() + isometric()
    Error in ggplot_add.ggplot_isometric(object, p, objectname) : 
      "x" %in% names(plot$labels) is not TRUE
    Calls: example ... ggplot_add -> ggplot_add.ggplot_isometric -> stopifnot
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(yamlet)
      
      Attaching package: 'yamlet'
      
      The following object is masked from 'package:stats':
      
    ...
      ══ Skipped tests (2) ═══════════════════════════════════════════════════════════
      • empty test (2): 'test-yamlet.R:1346:1', 'test-yamlet.R:1351:1'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-yamlet.R:843:1'): ggplot.resolved is stable ──────────────────
      `print(x %>% ggplot(map) + geom_point())` did not produce any warnings.
      
      [ FAIL 1 | WARN 0 | SKIP 2 | PASS 516 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘scripted-html.Rmd’ using rmarkdown
    ```

