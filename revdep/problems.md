# apyramid

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/apyramid
* URL: https://github.com/R4EPI/apyramid, https://r4epis.netlify.com
* BugReports: https://github.com/R4EPI/apyramid/issues
* Date/Publication: 2020-01-13 15:50:06 UTC
* Number of recursive dependencies: 120

Run `revdep_details(,"apyramid")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(apyramid)
      > 
      > test_check("apyramid")
      [31m──[39m [31m1. Failure: missing split data are removed before plotting (@test-age-pyramid.R#171) [39m [31m──────────────────────────────────────────────────[39m
      `age_pyramid(dat, age_group = "AGE", na.rm = FALSE)` produced warnings.
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 72 | SKIPPED: 11 | WARNINGS: 11 | FAILED: 1 ]
      1. Failure: missing split data are removed before plotting (@test-age-pyramid.R#171) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# auditor

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/auditor
* URL: https://github.com/ModelOriented/auditor
* BugReports: https://github.com/ModelOriented/auditor/issues
* Date/Publication: 2019-09-24 07:20:06 UTC
* Number of recursive dependencies: 71

Run `revdep_details(,"auditor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
      -> residuals         :  numerical, min =  -0.9614217 , mean =  -1.682008e-09 , max =  0.9666502  
      -> model_info        :  package stats , ver. 3.6.1 , task regression ( [33m default [39m ) 
     [32m A new explainer has been created! [39m 
    > 
    > # validate a model with auditor
    > library(auditor)
    > eva_glm <- model_evaluation(exp_glm)
    > 
    > # plot results
    > plot_lift(eva_glm)
    Error:   Differing number of values and breaks in manual scale.
      3 values provided compared to 1 breaks.
    [1m<error/rlang_error>[22m
      Differing number of values and breaks in manual scale.
      3 values provided compared to 1 breaks.
    [1mBacktrace:[22m
    [90m    [39m█
    [90m 1. [39m└─auditor::plot_lift(eva_glm)
    [90m 2. [39m  └─ggplot2::scale_color_manual(...)
    [90m 3. [39m    └─ggplot2:::manual_scale(aesthetics, values, breaks, ...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      95 out of 100 
      96 out of 100 
      97 out of 100 
      98 out of 100 
      99 out of 100 
      100 out of 100 
      Gaussian model (lm object) 
      Gaussian model (lm object) 
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 154 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: plot_lift (@test_plotsR.R#78) 
      2. Error: plot (@test_plotsR.R#113) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# autocogs

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/autocogs
* URL: https://github.com/schloerke/autocogs
* BugReports: https://github.com/schloerke/autocogs/issues
* Date/Publication: 2019-02-12 00:03:28 UTC
* Number of recursive dependencies: 76

Run `revdep_details(,"autocogs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    [1] FALSE
    
    $remove
    character(0)
    
    attr(,"class")
    [1] "cog_spec"
    [1] FALSE
    > 
    > # set up data
    > p <- ggplot2::qplot(Sepal.Length, Sepal.Width, data = iris, geom = c("point", "smooth"))
    > dt <- tibble::data_frame(panel = list(p))
    Warning: `data_frame()` is deprecated, use `tibble()`.
    [90mThis warning is displayed once per session.[39m
    > 
    > # compute cognostics like normal
    > add_panel_cogs(dt)
    Error in switch(as.character(layer$stat_params$method), loess = "geom_smooth_loess",  : 
      EXPR must be a length 1 vector
    Calls: add_panel_cogs ... get_layer_info -> layer_info -> layer_info.ggplot -> lapply -> FUN
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m  1. [39mggplot2::qplot(cty, hwy, data = mpg, geom = "smooth")
      [90m  9. [39mautocogs:::expect_auto_cogs(...)
      [90m 10. [39mautocogs:::plot_cogs(.)
      [90m 18. [39mautocogs:::get_layer_info(p, keep = keep_layers, ...)
      [90m 19. [39mautocogs:::layer_info.ggplot(p, keep = keep, ...)
      [90m 21. [39mbase::lapply(...)
      [90m 22. [39mautocogs:::FUN(X[[i]], ...)
      [90m 23. [39mggplot2::qplot(cty, hwy, data = mpg, geom = "smooth")
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 242 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: ggplot2::geom_smooth (@test-plot_cogs.R#246) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘broom’ ‘diptest’ ‘ggplot2’ ‘hexbin’ ‘moments’
      All declared Imports should be used.
    ```

# bayesdfa

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/bayesdfa
* URL: https://github.com/fate-ewi/bayesdfa
* BugReports: https://github.com/fate-ewi/bayesdfa/issues
* Date/Publication: 2019-05-22 13:40:05 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"bayesdfa")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        libs   4.6Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Newly fixed

*   checking whether package ‘bayesdfa’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/bayesdfa/old/bayesdfa.Rcheck/00install.out’ for details.
    ```

# benchr

<details>

* Version: 0.2.3-1
* Source code: https://github.com/cran/benchr
* URL: https://gitlab.com/artemklevtsov/benchr
* BugReports: https://gitlab.com/artemklevtsov/benchr/issues
* Date/Publication: 2019-07-01 12:50:07 UTC
* Number of recursive dependencies: 41

Run `revdep_details(,"benchr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
       diff| Lengths: 2, 0
       diff| target is character, current is NULL
      ----- FAILED[data]: test-plot.R<28--28>
       call| expect_equal(bphp$layout$panel_params[[1]]$y.labels, c("1 + 1", 
       call| "2 + 2"))
       diff| Modes: character, NULL
       diff| Lengths: 2, 0
       diff| target is character, current is NULL
      ----- FAILED[data]: test-plot.R<40--40>
       call| expect_equal(bpvp$layout$panel_params[[1]]$x.labels, c("1 + 1", 
       call| "2 + 2"))
       diff| Modes: character, NULL
       diff| Lengths: 2, 0
       diff| target is character, current is NULL 
      Execution halted
    ```

# biclustermd

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/biclustermd
* URL: http://github.com/jreisner/biclustermd
* BugReports: http://github.com/jreisner/biclustermd/issues
* Date/Publication: 2019-12-07 05:20:02 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"biclustermd")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1mBacktrace:[22m
      [90m 1. [39mbase::data.frame(...)
      
      [31m──[39m [31m2. Error: autoplot_biclustermd() plots row clusters in correct clusters (@test-autoplot_biclustermd.R#38) [39m [31m─────────────────────────────[39m
      arguments imply differing number of rows: 6, 0
      [1mBacktrace:[22m
      [90m 1. [39mbase::data.frame(...)
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 66 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: autoplot_biclustermd() plots column clusters in correct clusters (@test-autoplot_biclustermd.R#16) 
      2. Error: autoplot_biclustermd() plots row clusters in correct clusters (@test-autoplot_biclustermd.R#38) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘nycflights13’
      All declared Imports should be used.
    ```

# BMSC

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/BMSC
* Date/Publication: 2019-04-16 15:25:42 UTC
* Number of recursive dependencies: 88

Run `revdep_details(,"BMSC")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        libs   6.1Mb
    ```

## Newly fixed

*   checking whether package ‘BMSC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/BMSC/old/BMSC.Rcheck/00install.out’ for details.
    ```

# ChIPQC

<details>

* Version: 1.20.0
* Source code: https://github.com/cran/ChIPQC
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 166

Run `revdep_details(,"ChIPQC")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    BT4741  776353  100  15.8  8.42    28   153 2.110 1.88 14.90  1.75
    BT4742  782419  100  15.1 10.30    28   147 2.010 1.63 13.80  1.68
    MCF71   438994  100  20.9 21.30    28   134 2.300 2.52 26.50  1.70
    MCF72   465700  100  20.8  4.84    28   155 2.090 1.65 16.10  2.17
    MCF73   577273  100  19.1 10.30    28   153 2.460 2.18 21.80  1.83
    T47D1   507492  100  21.1  7.86    28   153 1.520 1.59  9.62  2.24
    T47D2  1831766  100  19.3  9.56    28   162 1.530 2.30  5.70  2.26
    TAMR1   747610  100  17.4 15.50    28   151 2.490 2.15 19.30  2.05
    TAMR2   728601  100  18.4  5.84    28   149 1.850 1.45 12.10  1.56
    ZR751   804427  100  12.0 15.80    28   158 2.950 3.52 30.70  1.39
    ZR752  2918549  100  11.6 23.80    28   160 3.140 4.45 20.70  1.22
    BT474c  598010  100  18.1  3.30    28   105 0.202 1.14  2.98  1.72
    MCF7c   485192  100  26.3  1.70    28   109 0.108 1.40  2.65  2.47
    T47Dc   400396  100  44.2 31.60    36   196 0.268 6.02  1.18  8.56
    TAMRc   779102  100  19.3  6.16    28   110 0.267 1.38  2.21  2.06
    ZR75c  1023987  100  26.2 20.10    36   220 0.573 5.36  1.50  5.35
    > plotRegi(tamoxifen)
    Error in gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t,  : 
      Not all inputs have either length 1 or same length same as 'grobs'
    Calls: <Anonymous> ... <Anonymous> -> f -> <Anonymous> -> f -> gtable_add_grob
    Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘ChIPQC.Rnw’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘ChIPQC.Rnw’
      ...
    
    > plotCC(tamoxifen, facetBy = c("Tissue", "Condition"))
    Warning in strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]] :
      number of items to replace is not a multiple of replacement length
    
    > plotRegi(tamoxifen, facetBy = c("Tissue", "Condition"))
    
      When sourcing ‘ChIPQC.R’:
    Error: Not all inputs have either length 1 or same length same as 'grobs'
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... WARNING
    ```
      Error loading dataset 'exampleExp':
       Error in .requirePackage(package) : 
        unable to find required package 'ChIPQC'
      
      Error loading dataset 'tamoxifen':
       Error in .requirePackage(package) : 
        unable to find required package 'ChIPQC'
      
      The dataset(s) may use package(s) not declared in the DESCRIPTION file.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘S4Vectors:::tabulate2’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking foreign function calls ... NOTE
    ```
    Foreign function call to a different package:
      .Call("rle_sum_any", ..., PACKAGE = "chipseq")
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    makeRegiPlot: no visible binding for global variable ‘Sample’
    makeRegiPlot: no visible binding for global variable ‘GenomicIntervals’
    makeRegiPlot: no visible binding for global variable ‘log2_Enrichment’
    makeSSDPlot: no visible binding for global variable ‘Sample’
    makeSSDPlot: no visible binding for global variable ‘SSD’
    makeSSDPlot: no visible global function definition for ‘geom_point’
    sampleQC: no visible global function definition for ‘seqlevels<-’
    plotCC,ChIPQCexperiment: no visible binding for global variable
      ‘Sample’
    plotCC,list: no visible binding for global variable ‘Sample’
    plotPeakProfile,ChIPQCexperiment: no visible binding for global
      variable ‘Sample’
    plotPeakProfile,list: no visible binding for global variable ‘Sample’
    Undefined global functions or variables:
      CC_Score CountsInPeaks Depth Distance FRIBL FRIP GenomicIntervals
      Reads SSD Sample Shift_Size Signal TxDb.Hsapiens.UCSC.hg38.knownGene
      geom_point log10_bp log2_Enrichment seqlengths seqlengths<-
      seqlevels<-
    Consider adding
      importFrom("stats", "SSD")
    to your NAMESPACE file.
    ```

*   checking contents of ‘data’ directory ... NOTE
    ```
    Output for data("tamoxifen_QC", package = "ChIPQC"):
      
    ```

# dabestr

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/dabestr
* URL: https://github.com/ACCLAB/dabestr
* BugReports: https://github.com/ACCLAB/dabestr/issues
* Date/Publication: 2019-07-04 16:20:05 UTC
* Number of recursive dependencies: 124

Run `revdep_details(,"dabestr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > # Performing unpaired (two independent groups) analysis.
    > unpaired_mean_diff <- dabest(iris, Species, Petal.Width,
    +                              idx = c("setosa", "versicolor"),
    +                              paired = FALSE)
    > 
    > # Create a Gardner-Altman estimation plot.
    > plot(unpaired_mean_diff)
    > 
    > 
    > 
    > # Comparing versicolor and virginica petal width to setosa petal width.
    > shared_control_data <- dabest(iris, Species, Petal.Width,
    +                               idx = c("setosa", "versicolor", "virginica")
    +                               )
    > 
    > # Create a Cumming estimation plot.
    > plot(shared_control_data)
    Error in max(tick_nchars) : invalid 'type' (list) of argument
    Calls: plot -> plot.dabest -> max_nchar_ticks
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m 2. [39mdabestr:::plot.dabest(multi.group.shared.control, color.column = Gender)
      [90m 3. [39mdabestr:::max_nchar_ticks(rawplot.yticks.labels)
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 22 | SKIPPED: 14 | WARNINGS: 0 | FAILED: 7 ]
      1. Error: Cumming custom aesthetics (@test-aesthetics.R#135) 
      2. Error: Cumming two-groups unpaired (@test-cumming-plots.R#14) 
      3. Error: Cumming two-groups paired (@test-cumming-plots.R#33) 
      4. Error: Cumming multi two-groups unpaired (@test-cumming-plots.R#53) 
      5. Error: Cumming multi two-groups paired (@test-cumming-plots.R#73) 
      6. Error: Cumming shared control (@test-cumming-plots.R#102) 
      7. Error: Cumming multi-group shared control (@test-cumming-plots.R#123) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   5.7Mb
    ```

# DepecheR

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/DepecheR
* Date/Publication: 2019-06-28
* Number of recursive dependencies: 100

Run `revdep_details(,"DepecheR")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        |                                                                            
        |=================================================                     |  70%
        |                                                                            
        |========================================================              |  80%
        |                                                                            
        |===============================================================       |  90%
        |                                                                            
        |======================================================================| 100%
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 21 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: depeche expected output (@test_depeche.R#7) 
      2. Failure: depeche expected output (@test_depeche.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘BiocParallel’
      All declared Imports should be used.
    ```

*   checking compiled code ... NOTE
    ```
    File ‘DepecheR/libs/DepecheR.so’:
      Found ‘_rand’, possibly from ‘rand’ (C)
        Object: ‘Clusterer.o’
      Found ‘_srand’, possibly from ‘srand’ (C)
        Objects: ‘Clusterer.o’, ‘InterfaceUtils.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# drake

<details>

* Version: 7.9.0
* Source code: https://github.com/cran/drake
* URL: https://github.com/ropensci/drake, https://docs.ropensci.org/drake, https://books.ropensci.org/drake/
* BugReports: https://github.com/ropensci/drake/issues
* Date/Publication: 2020-01-08 09:00:12 UTC
* Number of recursive dependencies: 137

Run `revdep_details(,"drake")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m  7. [39mfuture::makeClusterPSOCK(workers, ...)
      [90m  8. [39mfuture:::makeNode(...)
      [90m  9. [39m(function() {...
      [90m 10. [39mbase::tryCatch(...)
      [90m 11. [39mbase:::tryCatchList(expr, classes, parentenv, handlers)
      [90m 12. [39mbase:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
      [90m 13. [39mvalue[[3L]](cond)
      [90m 14. [39m(function() {...
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 5042 | SKIPPED: 264 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: future package functionality (@test-9-future.R#87) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# emmeans

<details>

* Version: 1.4.3.01
* Source code: https://github.com/cran/emmeans
* URL: https://github.com/rvlenth/emmeans
* BugReports: https://github.com/rvlenth/emmeans/issues
* Date/Publication: 2019-11-28 11:50:05 UTC
* Number of recursive dependencies: 171

Run `revdep_details(,"emmeans")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘emmeans-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: auto.noise
    > ### Title: Auto Pollution Filter Noise
    > ### Aliases: auto.noise
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > noise.lm <- lm(noise ~ size * type * side, data = auto.noise)
    > 
    > # Interaction plot of predictions
    > emmip(noise.lm, type ~ size | side)
    > 
    > # Confidence intervals
    > plot(emmeans(noise.lm, ~ size | side*type))
    Error in gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t,  : 
      Not all inputs have either length 1 or same length same as 'grobs'
    Calls: <Anonymous> ... <Anonymous> -> f -> <Anonymous> -> f -> gtable_add_grob
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'CARBayes', 'sommer'
    ```

# ezplot

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2019-07-20 21:20:03 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"ezplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > library(lubridate)
    
    Attaching package: ‘lubridate’
    
    The following object is masked from ‘package:base’:
    
        date
    
    > bar_plot(ansett, "year(Week)", "Passengers")
    > bar_plot(ansett, "year(Week)", "Passengers", "Class")
    Error:   Differing number of values and breaks in manual scale.
      3 values provided compared to 1 breaks.
    [1m<error/rlang_error>[22m
      Differing number of values and breaks in manual scale.
      3 values provided compared to 1 breaks.
    [1mBacktrace:[22m
    [90m    [39m█
    [90m 1. [39m└─ezplot::bar_plot(ansett, "year(Week)", "Passengers", "Class")
    [90m 2. [39m  └─ggplot2::scale_fill_manual(...)
    [90m 3. [39m    └─ggplot2:::manual_scale(aesthetics, values, breaks, ...)
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [31m──[39m [31m2. Error: bar_plot works (@test-bar_plot.R#10) [39m [31m────────────────────────────────────────────────────────────────────────────────────────[39m
        Differing number of values and breaks in manual scale.
        2 values provided compared to 1 breaks.
      [1mBacktrace:[22m
      [90m 1. [39mezplot::bar_plot(mtcars, "cyl", "1", "am", position = "fill")
      [90m 2. [39mggplot2::scale_fill_manual(...)
      [90m 3. [39mggplot2:::manual_scale(aesthetics, values, breaks, ...)
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 57 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: area_plot works (@test-area_plot.R#9) 
      2. Error: bar_plot works (@test-bar_plot.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# FLightR

<details>

* Version: 0.4.9
* Source code: https://github.com/cran/FLightR
* URL: https://CRAN.R-project.org/package=FLightR
* BugReports: http://github.com/eldarrak/FLightR/issues
* Date/Publication: 2019-06-14 16:30:24 UTC
* Number of recursive dependencies: 109

Run `revdep_details(,"FLightR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     checking dusk 13
     checking dusk 14
     checking dusk 15
    
    
    calibration method used: parametric.slope 
    > 
    > Grid<-make.grid(left=0, bottom=50, right=10, top=56,
    +   distance.from.land.allowed.to.use=c(-Inf, Inf),
    +   distance.from.land.allowed.to.stay=c(-Inf, Inf))
    > 
    > all.in<-make.prerun.object(Proc.data, Grid, start=c(5.43, 52.93),
    +                              Calibration=Calibration, threads=2)
    likelihood correction turned off as no correction found in the calibration
    making cluster
    Warning in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  :
      port 11227 cannot be opened
    Error in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  : 
      cannot open the connection
    Calls: make.prerun.object ... makePSOCKcluster -> newPSOCKnode -> socketConnection
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rgdal’
      All declared Imports should be used.
    ```

# gastempt

<details>

* Version: 0.4.4
* Source code: https://github.com/cran/gastempt
* URL: http://github.com/dmenne/gastempt
* BugReports: http://github.com/dmenne/gastempt/issues
* Date/Publication: 2019-03-06 16:32:41 UTC
* Number of recursive dependencies: 90

Run `revdep_details(,"gastempt")` for more info

</details>

## Newly broken

*   checking whether package ‘gastempt’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/gastempt/new/gastempt.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        libs   6.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘methods’ ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Installation

### Devel

```
* installing *source* package ‘gastempt’ ...
** package ‘gastempt’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1b.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1c.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1d.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_2b.stan
Wrote C++ file "stan_files/linexp_gastro_1b.cc"
Wrote C++ file "stan_files/linexp_gastro_1c.cc"
Wrote C++ file "stan_files/linexp_gastro_1d.cc"
Wrote C++ file "stan_files/linexp_gastro_2b.cc"
Error in readRDS("/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpyZC0Ey/filefb6a4c583a4") : 
  error reading from connection
Calls: .Last -> readRDS
Execution halted
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_2c.stan
make: *** [stan_files/linexp_gastro_1d.cc] Error 1
make: *** Waiting for unfinished jobs....
Wrote C++ file "stan_files/linexp_gastro_2c.cc"
rm stan_files/linexp_gastro_2c.cc stan_files/linexp_gastro_1d.cc stan_files/linexp_gastro_1b.cc stan_files/linexp_gastro_2b.cc stan_files/linexp_gastro_1c.cc
ERROR: compilation failed for package ‘gastempt’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/gastempt/new/gastempt.Rcheck/gastempt’

```
### CRAN

```
* installing *source* package ‘gastempt’ ...
** package ‘gastempt’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1b.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1c.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1d.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_2b.stan
Wrote C++ file "stan_files/linexp_gastro_1c.cc"
Wrote C++ file "stan_files/linexp_gastro_1d.cc"
Wrote C++ file "stan_files/linexp_gastro_1b.cc"
Wrote C++ file "stan_files/linexp_gastro_2b.cc"
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_2c.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/powexp_gastro_1b.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/powexp_gastro_2c.stan


/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.cpp -o init.o


/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/linexp_gastro_1b.cc -o stan_files/linexp_gastro_1b.o


Wrote C++ file "stan_files/powexp_gastro_1b.cc"
Wrote C++ file "stan_files/powexp_gastro_2c.cc"
Wrote C++ file "stan_files/linexp_gastro_2c.cc"
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/linexp_gastro_1c.cc -o stan_files/linexp_gastro_1c.o


/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/linexp_gastro_1d.cc -o stan_files/linexp_gastro_1d.o


/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/linexp_gastro_2b.cc -o stan_files/linexp_gastro_2b.o


In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/linexp_gastro_1b.cc:3:
stan_files/linexp_gastro_1b.hpp:176:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/linexp_gastro_1c.cc:3:
stan_files/linexp_gastro_1c.hpp:176:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/linexp_gastro_1d.cc:3:
stan_files/linexp_gastro_1d.hpp:176:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/linexp_gastro_2b.cc:3:
stan_files/linexp_gastro_2b.hpp:241:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
21 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/linexp_gastro_2c.cc -o stan_files/linexp_gastro_2c.o


21 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/powexp_gastro_1b.cc -o stan_files/powexp_gastro_1b.o


21 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/powexp_gastro_2c.cc -o stan_files/powexp_gastro_2c.o
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
21 warnings generated.
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/linexp_gastro_2c.cc:3:
stan_files/linexp_gastro_2c.hpp:225:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/powexp_gastro_1b.cc:3:
stan_files/powexp_gastro_1b.hpp:176:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/powexp_gastro_2c.cc:3:
stan_files/powexp_gastro_2c.hpp:241:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
21 warnings generated.
21 warnings generated.
21 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++14 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o gastempt.so stan_files/linexp_gastro_1b.o stan_files/linexp_gastro_1c.o stan_files/linexp_gastro_1d.o stan_files/linexp_gastro_2b.o stan_files/linexp_gastro_2c.o stan_files/powexp_gastro_1b.o stan_files/powexp_gastro_2c.o init.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
rm stan_files/linexp_gastro_2c.cc stan_files/linexp_gastro_1d.cc stan_files/powexp_gastro_2c.cc stan_files/linexp_gastro_1b.cc stan_files/linexp_gastro_2b.cc stan_files/linexp_gastro_1c.cc stan_files/powexp_gastro_1b.cc
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/gastempt/old/gastempt.Rcheck/00LOCK-gastempt/00new/gastempt/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (gastempt)

```
# GDCRNATools

<details>

* Version: 1.4.1
* Source code: https://github.com/cran/GDCRNATools
* Date/Publication: 2019-09-16
* Number of recursive dependencies: 196

Run `revdep_details(,"GDCRNATools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                     2.1,2.9,5.9,5.7,4.5,3.5,
    +                     2.7,5.9,4.5,5.8,5.2,3.0,
    +                     2.5,2.2,5.3,4.4,4.4,2.9,
    +                     2.4,3.8,6.2,3.8,3.8,4.2),6,6)
    > rownames(rnaExpr) <- genes
    > colnames(rnaExpr) <- samples
    > gdcCorPlot(gene1 = 'ENSG00000000938', 
    +         gene2    = 'ENSG00000001084',
    +         rna.expr = rnaExpr,
    +         metadata = metaMatrix)
    Error:   Differing number of values and breaks in manual scale.
      2 values provided compared to 6 breaks.
    [1m<error/rlang_error>[22m
      Differing number of values and breaks in manual scale.
      2 values provided compared to 6 breaks.
    [1mBacktrace:[22m
    [90m    [39m█
    [90m 1. [39m└─GDCRNATools::gdcCorPlot(...)
    [90m 2. [39m  └─ggplot2::scale_colour_manual(...)
    [90m 3. [39m    └─ggplot2:::manual_scale(aesthetics, values, breaks, ...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        R      3.2Mb
        data   1.1Mb
        doc    2.6Mb
    ```

*   checking top-level files ... NOTE
    ```
    File
      LICENSE
    is not mentioned in the DESCRIPTION file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    gdcRNAMerge: no visible global function definition for ‘read.table’
    gdcRNAMerge : <anonymous>: no visible global function definition for
      ‘read.delim’
    gdcRNAMerge: no visible global function definition for ‘read.delim’
    hyperTestFun: no visible global function definition for ‘phyper’
    kmTestFun: no visible global function definition for ‘pchisq’
    kmTestFun: no visible global function definition for ‘qnorm’
    manifestDownloadFun: no visible global function definition for
      ‘read.table’
    mirCorTestFun: no visible global function definition for ‘cor.test’
    multiRegFun: no visible global function definition for ‘cor.test’
    Undefined global functions or variables:
      Category Counts FDR Regulation Terms URLencode aggregate cor.test
      download.file foldEnrichment model.matrix p.adjust pchisq phyper
      qnorm read.delim read.table unzip write.table
    Consider adding
      importFrom("stats", "aggregate", "cor.test", "model.matrix",
                 "p.adjust", "pchisq", "phyper", "qnorm")
      importFrom("utils", "URLencode", "download.file", "read.delim",
                 "read.table", "unzip", "write.table")
    to your NAMESPACE file.
    ```

# GGally

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/GGally
* URL: https://ggobi.github.io/ggally, https://github.com/ggobi/ggally
* BugReports: https://github.com/ggobi/ggally/issues
* Date/Publication: 2018-05-17 23:31:19 UTC
* Number of recursive dependencies: 138

Run `revdep_details(,"GGally")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 699 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 11 ]
      1. Error: diagAxis (@test-gg-plots.R#102) 
      2. Failure: shrink (@test-gg-plots.R#173) 
      3. Failure: shrink (@test-gg-plots.R#177) 
      4. Failure: smooth_se (@test-gg-plots.R#183) 
      5. Failure: smooth_se (@test-gg-plots.R#187) 
      6. Failure: generally works (@test-ggfacet.R#24) 
      7. Failure: generally works (@test-ggfacet.R#36) 
      8. Failure: generally works (@test-ggfacet.R#45) 
      9. Failure: ggnostic mtcars (@test-ggnostic.R#71) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘GGally-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggally_diagAxis
    > ### Title: Internal Axis Labeling Plot for ggpairs
    > ### Aliases: ggally_diagAxis
    > 
    > ### ** Examples
    > 
    >  data(tips, package = "reshape")
    >  ggally_diagAxis(tips, ggplot2::aes(x=tip))
    Error in `$<-.data.frame`(`*tmp*`, "hjust", value = 0.5) : 
      replacement has 1 row, data has 0
    Calls: ggally_diagAxis -> get_x_axis_labels -> $<- -> $<-.data.frame
    Execution halted
    ```

# ggcyto

<details>

* Version: 1.12.0
* Source code: https://github.com/cran/ggcyto
* URL: https://github.com/RGLab/ggcyto/issues
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 160

Run `revdep_details(,"ggcyto")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Plot cytometry data in one or two dimension with the ggcyto
    > ###   package.
    > ### Aliases: autoplot.flowSet autoplot autoplot.ncdfFlowList
    > ###   autoplot.flowFrame autoplot.GatingSetList autoplot.GatingSet
    > ###   autoplot.GatingHierarchy
    > 
    > ### ** Examples
    > 
    > library(flowCore)
    > data(GvHD)
    > fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
    > 
    > #1d- density plot
    > autoplot(fs, x = "SSC-H")
    > 
    > #1d- density plot on all channels
    > autoplot(fs[[1]])
    Error in get(name, envir = asNamespace(pkg), inherits = FALSE) : 
      object 'update_theme' not found
    Calls: autoplot ... lapply -> FUN -> + -> + -> add_ggcyto -> ::: -> get
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m 5. [39mggcyto:::add_ggcyto_gs(e1, e2)
      [90m 6. [39mggcyto:::`+.ggcyto_flowSet`(e1, e2)
      [90m 7. [39mggcyto:::add_ggcyto(e1, e2, e2name)
      [90m 8. [39mggplot2:::update_theme
      [90m 9. [39mbase::get(name, envir = asNamespace(pkg), inherits = FALSE)
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 1 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: autoplot -- flowset (@test-autoplot.R#8) 
      2. Error: autoplot -- gatingset (@test-autoplot.R#19) 
      3. Error: fs (@test-ggcyto-fs.R#16) 
      4. Error: gs (@test-ggcyto-gs.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented S4 methods:
      generic '%+%' and siglist 'ggcyto'
    All user-level objects in a package (including S4 classes and methods)
    should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in documentation object 'fortify.filterList'
      ‘data’ ‘nPoints’
    
    Undocumented arguments in documentation object 'fortify.polygonGate'
      ‘nPoints’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        doc   5.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ':::' call which should be '::': ‘flowWorkspace:::isNegated’
      See the note in ?`:::` about the use of this operator.
    Missing object imported by a ':::' call: ‘ggplot2:::update_theme’
    Unexported objects imported by ':::' calls:
      ‘flowWorkspace:::.mergeGates’ ‘flowWorkspace:::compact’
      ‘flowWorkspace:::fix_y_axis’ ‘ggplot2:::+.gg’ ‘ggplot2:::add_group’
      ‘ggplot2:::as_gg_data_frame’ ‘ggplot2:::check_aesthetics’
      ‘ggplot2:::hex_binwidth’ ‘ggplot2:::is.waive’
      ‘ggplot2:::is_calculated_aes’ ‘ggplot2:::make_labels’
      ‘ggplot2:::make_scale’ ‘ggplot2:::plot_clone’
      ‘ggplot2:::print.ggplot’ ‘ggplot2:::scales_add_defaults’
      ‘ggplot2:::scales_list’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    fortify_fs.GatingSetList: no visible global function definition for
      ‘getS3method’
    getFlowFrame.GatingSetList: no visible global function definition for
      ‘getS3method’
    getFlowFrame.ncdfFlowList: no visible global function definition for
      ‘getS3method’
    ggcyto.GatingSetList: no visible global function definition for
      ‘getS3method’
    ggcyto.flowSet: no visible binding for global variable ‘name’
    ggcyto.flowSet: no visible binding for global variable ‘axis’
    ggcyto.ncdfFlowList: no visible global function definition for
      ‘getS3method’
    ggcyto_arrange: no visible binding for global variable ‘name’
    Undefined global functions or variables:
      axis density desc getS3method gray modifyList name
    Consider adding
      importFrom("grDevices", "gray")
      importFrom("graphics", "axis")
      importFrom("stats", "density")
      importFrom("utils", "getS3method", "modifyList")
    to your NAMESPACE file.
    ```

# ggdag

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/ggdag
* URL: https://github.com/malcolmbarrett/ggdag
* BugReports: https://github.com/malcolmbarrett/ggdag/issues
* Date/Publication: 2019-12-06 05:50:02 UTC
* Number of recursive dependencies: 95

Run `revdep_details(,"ggdag")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    [39m[90m# Outcome: y
    [39m[90m#
    [39m[90m# A tibble: 36 x 10[39m
       name      x     y direction to     xend  yend circular adjusted   set        
       [3m[90m<chr>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<fct>[39m[23m     [3m[90m<chr>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m [3m[90m<lgl>[39m[23m    [3m[90m<chr>[39m[23m      [3m[90m<chr>[39m[23m      
    [90m 1[39m v      23.5  16.9 ->        z1     22.6  16.0 FALSE    unadjusted {w1, w2, z…
    [90m 2[39m v      23.5  16.9 ->        z2     24.5  15.9 FALSE    unadjusted {w1, w2, z…
    [90m 3[39m w1     23.4  15.1 ->        x      22.5  14.7 FALSE    adjusted   {w1, w2, z…
    [90m 4[39m w1     23.4  15.1 ->        y      23.9  14.6 FALSE    adjusted   {w1, w2, z…
    [90m 5[39m w1     23.4  15.1 ->        z1     22.6  16.0 FALSE    adjusted   {w1, w2, z…
    [90m 6[39m w1     23.4  15.1 <->       w2     24.8  14.9 FALSE    adjusted   {w1, w2, z…
    [90m 7[39m w2     24.8  14.9 ->        y      23.9  14.6 FALSE    adjusted   {w1, w2, z…
    [90m 8[39m w2     24.8  14.9 ->        z2     24.5  15.9 FALSE    adjusted   {w1, w2, z…
    [90m 9[39m x      22.5  14.7 ->        y      23.9  14.6 FALSE    unadjusted {w1, w2, z…
    [90m10[39m z1     22.6  16.0 ->        x      22.5  14.7 FALSE    unadjusted {w1, w2, z…
    [90m# … with 26 more rows[39m
    > 
    > ggdag_adjustment_set(dag)
    Error in f(..., self = self) : unused argument (modifiers)
    Calls: <Anonymous> ... ggplot_build.ggplot -> by_layer -> f -> <Anonymous> -> f -> <Anonymous>
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggforce’ ‘plyr’
      All declared Imports should be used.
    ```

# ggfortify

<details>

* Version: 0.4.8
* Source code: https://github.com/cran/ggfortify
* URL: https://github.com/sinhrks/ggfortify
* BugReports: https://github.com/sinhrks/ggfortify/issues
* Date/Publication: 2019-11-10 21:40:02 UTC
* Number of recursive dependencies: 127

Run `revdep_details(,"ggfortify")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      
      x[8]: "#00BFC4"
      y[8]: "#F8766D"
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 731 | SKIPPED: 47 | WARNINGS: 0 | FAILED: 6 ]
      1. Failure: autoplot ts works for multivariate timeseries (@test-ts.R#139) 
      2. Failure: autoplot ts works for multivariate timeseries (@test-ts.R#140) 
      3. Failure: autoplot ts works for multivariate timeseries (@test-ts.R#141) 
      4. Failure: autoplot ts works for multivariate timeseries (@test-ts.R#148) 
      5. Failure: autoplot ts works for multivariate timeseries (@test-ts.R#149) 
      6. Failure: autoplot ts works for multivariate timeseries (@test-ts.R#150) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggmap

<details>

* Version: 3.0.0
* Source code: https://github.com/cran/ggmap
* URL: https://github.com/dkahle/ggmap
* BugReports: https://github.com/dkahle/ggmap/issues
* Date/Publication: 2019-02-05 10:19:04
* Number of recursive dependencies: 70

Run `revdep_details(,"ggmap")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   4.8Mb
    ```

# ggmuller

<details>

* Version: 0.5.4
* Source code: https://github.com/cran/ggmuller
* Date/Publication: 2019-09-05 02:10:17 UTC
* Number of recursive dependencies: 57

Run `revdep_details(,"ggmuller")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: Muller_plot
    > ### Title: Draw a Muller plot of frequencies using ggplot2
    > ### Aliases: Muller_plot
    > 
    > ### ** Examples
    > 
    > # include all genotypes:
    > Muller_df1 <- get_Muller_df(example_edges, example_pop_df)
    > Muller_plot(Muller_df1)
    Error:   Differing number of values and breaks in manual scale.
      26 values provided compared to 7 breaks.
    [1m<error/rlang_error>[22m
      Differing number of values and breaks in manual scale.
      26 values provided compared to 7 breaks.
    [1mBacktrace:[22m
    [90m    [39m█
    [90m 1. [39m└─ggmuller::Muller_plot(Muller_df1)
    [90m 2. [39m  └─ggplot2::scale_fill_manual(...)
    [90m 3. [39m    └─ggplot2:::manual_scale(aesthetics, values, breaks, ...)
    Execution halted
    ```

# ggpol

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/ggpol
* URL: https://github.com/erocoar/ggpol
* Date/Publication: 2019-03-14 13:40:02 UTC
* Number of recursive dependencies: 56

Run `revdep_details(,"ggpol")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # Get the count per age and sex
    > df$count <- 1
    > df <- aggregate(count ~ gender + age, data = df, length)
    > 
    > # For the horizontally shared axis, if we want to mirror the axes,
    > # we have to multiply the first panel by -1, and use coord_flip().
    > df_h <- df 
    > df_h$count = ifelse(df_h$gender == "F", df_h$count * -1, df_h$count)
    > 
    > p <- ggplot(df_h, aes(x = factor(age), y = count, fill = gender)) + 
    +   geom_bar(stat = "identity") +
    +   facet_share(~gender, dir = "h", scales = "free", reverse_num = TRUE) + 
    +   coord_flip() +
    +   labs(x = "Age", y = "Count") + 
    +   theme(legend.position = "bottom")
    > 
    > p
    Error in axes$y$left[[1]]$children$axis$grobs[[lab_idx]] : 
      attempt to select less than one element in get1index
    Calls: <Anonymous> ... ggplot_gtable.ggplot_built -> <Anonymous> -> f -> <Anonymous> -> f
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘grDevices’
      All declared Imports should be used.
    ```

# ggspectra

<details>

* Version: 0.3.4
* Source code: https://github.com/cran/ggspectra
* URL: https://www.r4photobiology.info, https://bitbucket.org/aphalo/ggspectra
* BugReports: https://bitbucket.org/aphalo/ggspectra
* Date/Publication: 2019-09-12 16:50:02 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"ggspectra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggspectra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.filter_spct
    > ### Title: Create a complete ggplot for a filter spectrum.
    > ### Aliases: autoplot.filter_spct autoplot.filter_mspct
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    > 
    > autoplot(yellow_gel.spct)
    Error in FUN(X[[i]], ...) : object 'wl.colour' not found
    Calls: <Anonymous> ... ggplot_build.ggplot -> by_layer -> f -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

# ggstance

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/ggstance
* Date/Publication: 2019-08-19 13:30:03 UTC
* Number of recursive dependencies: 122

Run `revdep_details(,"ggstance")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 1 | SKIPPED: 28 | WARNINGS: 3 | FAILED: 25 ]
      1. Failure: flipped geoms have correct `required_aes` failure messages (@test-flip.R#6) 
      2. Failure: geom_linerangeh() flips (@test-geoms.R#7) 
      3. Failure: geom_pointangeh() flips (@test-geoms.R#13) 
      4. Failure: geom_pointangeh() flips (@test-geoms.R#19) 
      5. Failure: geom_pointangeh() flips (@test-geoms.R#25) 
      6. Failure: geom_crossbarh() flips (@test-geoms.R#31) 
      7. Failure: geom_errorbarh() flips (@test-geoms.R#37) 
      8. Failure: geom_barh() flips (@test-geoms.R#45) 
      9. Failure: geom_barh() flips (@test-geoms.R#51) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggtern

<details>

* Version: 3.1.0
* Source code: https://github.com/cran/ggtern
* URL: http://www.ggtern.com
* Date/Publication: 2018-12-19 11:20:03 UTC
* Number of recursive dependencies: 48

Run `revdep_details(,"ggtern")` for more info

</details>

## Newly broken

*   checking whether package ‘ggtern’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggtern/new/ggtern.Rcheck/00install.out’ for details.
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
  object 'expand_default' not found
Error: unable to load R code in package ‘ggtern’
Execution halted
ERROR: lazy loading failed for package ‘ggtern’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggtern/new/ggtern.Rcheck/ggtern’

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
# interactions

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/interactions
* URL: https://interactions.jacob-long.com
* BugReports: https://github.com/jacob-long/interactions/issues
* Date/Publication: 2019-07-05 07:30:23 UTC
* Number of recursive dependencies: 91

Run `revdep_details(,"interactions")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("interactions")
      [31m──[39m [31m1. Failure: interact_plot linearity.check works (@test_interact_plot.R#157) [39m [31m───────────────────────────────────────────────────────────[39m
      `print(p)` produced messages.
      
      [31m──[39m [31m2. Failure: interact_plot linearity.check works (@test_interact_plot.R#162) [39m [31m───────────────────────────────────────────────────────────[39m
      `print(p)` produced messages.
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 165 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: interact_plot linearity.check works (@test_interact_plot.R#157) 
      2. Failure: interact_plot linearity.check works (@test_interact_plot.R#162) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘pequod’
    ```

# lemon

<details>

* Version: 0.4.3
* Source code: https://github.com/cran/lemon
* URL: https://github.com/stefanedwards/lemon
* BugReports: https://github.com/stefanedwards/lemon/issues
* Date/Publication: 2019-01-08 15:50:03 UTC
* Number of recursive dependencies: 68

Run `revdep_details(,"lemon")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `l + 1` threw an error with unexpected message.
      Expected match: "Don't know how to add 1 to a plot"
      Actual message: "Can't add `1` to a ggplot object."
      [1mBacktrace:[22m
      [90m 1. [39mtestthat::expect_error(...)
      [90m 6. [39mggplot2:::`+.gg`(l, 1)
      [90m 7. [39mggplot2:::add_ggplot(e1, e2, e2name)
      [90m 9. [39mggplot2:::ggplot_add.default(object, p, objectname)
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 137 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: ggplot2 does not break lemon_plot by altering class (@test_lemon_plot.r#12) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# mcStats

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/mcStats
* Date/Publication: 2019-12-03 17:00:02 UTC
* Number of recursive dependencies: 59

Run `revdep_details(,"mcStats")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      * `mapping` is not used by stat_function()
      * `data` is not used by stat_function()
      * `mapping` is not used by stat_function()
      * `data` is not used by stat_function()
      * `mapping` is not used by stat_function()
      * `data` is not used by stat_function()
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 5 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: showT.Test runs error-free (@testGraphs.R#10) 
      2. Failure: showANOVA runs error-free (@testGraphs.R#26) 
      3. Failure: showOLS runs error-free (@testGraphs.R#30) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# obAnalytics

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/obAnalytics
* URL: https://github.com/phil8192/ob-analytics
* BugReports: https://github.com/phil8192/ob-analytics/issues
* Date/Publication: 2016-11-11 17:26:37
* Number of recursive dependencies: 63

Run `revdep_details(,"obAnalytics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > 
    > # visualise 2 hours of order book liquidity.
    > # data will be aggregated to minute-by-minute resolution.
    > plotVolumePercentiles(lob.data$depth.summary,
    +     start.time=as.POSIXct("2015-05-01 02:30:00.000", tz="UTC"),
    +     end.time=as.POSIXct("2015-05-01 04:30:00.000", tz="UTC"),
    +     volume.scale=10^-8)
    Error:   Differing number of values and breaks in manual scale.
      40 values provided compared to 20 breaks.
    [1m<error/rlang_error>[22m
      Differing number of values and breaks in manual scale.
      40 values provided compared to 20 breaks.
    [1mBacktrace:[22m
    [90m    [39m█
    [90m 1. [39m└─obAnalytics::plotVolumePercentiles(...)
    [90m 2. [39m  └─ggplot2::scale_fill_manual(...)
    [90m 3. [39m    └─ggplot2:::manual_scale(aesthetics, values, breaks, ...)
    Execution halted
    ```

# ormPlot

<details>

* Version: 0.3.2
* Source code: https://github.com/cran/ormPlot
* Date/Publication: 2019-06-21 16:30:08 UTC
* Number of recursive dependencies: 136

Run `revdep_details(,"ormPlot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > cran_model <- orm(educ_3 ~ Rural + sex + max_SEP_3 + cran_rzs, data = educ_data)
    > 
    > #plot the predictions of the model for varying one variable only
    > plot(cran_model, cran_rzs)
    > 
    > #customize the plotting varying all variables
    > plot(cran_model, cran_rzs,
    +       plot_cols = max_SEP_3,
    +       plot_rows = c(Rural, sex),
    + 
    +       #setting new x-label (optional)
    +      xlab = "Cranial volume (residuals to age an birth date)",
    + 
    +      #setting new facet labels (optional)
    +      facet_labels = list(Rural = c("Urban", "Rural"),
    +                           sex = c("Boys","Girls"))
    +      )
    Error in gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t,  : 
      Not all inputs have either length 1 or same length same as 'grobs'
    Calls: <Anonymous> ... <Anonymous> -> f -> <Anonymous> -> f -> gtable_add_grob
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m  9. [39mlayout$render(geom_grobs, data, theme, plot$labels)
      [90m 10. [39mggplot2:::f(..., self = self)
      [90m 11. [39mself$facet$draw_panels(...)
      [90m 12. [39mggplot2:::f(...)
      [90m 13. [39mgtable::gtable_add_grob(...)
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 391 | SKIPPED: 14 | WARNINGS: 1 | FAILED: 4 ]
      1. Error: plotting test data generates the expected image (@test-plot_orm_predictWithCI.R#21) 
      2. Error: plotting test data changes element names and order (@test-plot_orm_predictWithCI.R#43) 
      3. Error: plotting test data accepts no vectors (@test-plot_orm_predictWithCI.R#57) 
      4. Error: plotting test data accepts no quotes (@test-plot_orm_predictWithCI.R#71) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# photobiologyFilters

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/photobiologyFilters
* URL: http://www.r4photobiology.info/
* BugReports: https://bitbucket.org/aphalo/photobiologyfilters
* Date/Publication: 2019-06-26 10:10:51 UTC
* Number of recursive dependencies: 65

Run `revdep_details(,"photobiologyFilters")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    [90m# … with 991 more rows[39m
    > getWhatMeasured(filters.mspct$Schott_OG530)
    [1] "SCHOTT OG530, thickness (m): 0.003"
    > 
    > # combining name (index) vectors
    > # blue glass filters
    > intersect(optical_glass_filters, blue_filters)
    [1] "Schott_GG395" "Schott_GG400" "Schott_GG420" "Schott_GG435" "Schott_GG455"
    [6] "Schott_GG475" "Schott_GG495"
    > # green plastic films
    > intersect(plastic_films, green_filters)
    [1] "Rosco_Moss_Green_EColour_no89"      "Rosco_Moss_Green_EColour_no89_used"
    [3] "XL_Horticulture_Supergreen"        
    > 
    > # A Plexiglas sheet
    > getWhatMeasured(filters.mspct$Evonik_Sky_Blue_5C01_GT)
    [1] "Poly(methyl methacrylate) (PMMA) 'acrylic' sheet; Plexiglas 'Sky Blue 5C01 GT'; 0.002 m thick; new; from Evonik Industries, Germany"
    > plot(filters.mspct$Evonik_Sky_Blue_5C01_GT)
    Error in FUN(X[[i]], ...) : object 'wl.colour' not found
    Calls: <Anonymous> ... ggplot_build.ggplot -> by_layer -> f -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

# photobiologyLEDs

<details>

* Version: 0.4.3-1
* Source code: https://github.com/cran/photobiologyLEDs
* URL: http://www.r4photobiology.info, https://bitbucket.org/aphalo/photobiologyleds
* BugReports: https://bitbucket.org/aphalo/photobiologyleds/issues
* Date/Publication: 2018-01-14 15:47:06 UTC
* Number of recursive dependencies: 60

Run `revdep_details(,"photobiologyLEDs")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Label: White LED from hardware store
    supplier Clas Ohlsson, Finland 
    Time unit 1s
    Spectral data normalized to 1 at 453.47 nm 
    
    [90m# A tibble: 2 x 2[39m
      w.length s.e.irrad
    [90m*[39m    [3m[90m<dbl>[39m[23m     [3m[90m<dbl>[39m[23m
    [90m1[39m     453.     1    
    [90m2[39m     547.     0.253
    > 
    > range(leds.mspct$white)
    [1] 250.05 899.78
    > 
    > stepsize(leds.mspct$white)
    [1] 0.43 0.48
    > 
    > plot(leds.mspct$white)
    Error in FUN(X[[i]], ...) : object 'wl.colour' not found
    Calls: <Anonymous> ... ggplot_build.ggplot -> by_layer -> f -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

# photobiologySensors

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/photobiologySensors
* URL: http://www.r4photobiology.info, https://bitbucket.org/aphalo/photobiologysensors
* BugReports: https://bitbucket.org/aphalo/photobiologysensors/issues
* Date/Publication: 2018-02-26 19:22:32 UTC
* Number of recursive dependencies: 60

Run `revdep_details(,"photobiologySensors")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > licor_sensors
    [1] "LI_190" "LI_200" "LI_210"
    > par_sensors
    [1] "SKP215" "SKE510" "SKP210" "PQS1"   "LI_190" "BF5"   
    > intersect(par_sensors, licor_sensors)
    [1] "LI_190"
    > 
    > photon_as_default()
    > 
    > response(sensors.mspct$LI_190, w.band = PAR(), quantity = "contribution.pc")
     Total PAR 
      99.18297 
    attr(,"time.unit")
    [1] "second"
    attr(,"radiation.unit")
    [1] "photon response contribution.pc"
    > 
    > plot(sensors.mspct$LI_190, w.band = PAR(), label.qty = "contribution.pc")
    Error in FUN(X[[i]], ...) : object 'wl.colour' not found
    Calls: <Anonymous> ... ggplot_build.ggplot -> by_layer -> f -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

# plot3logit

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/plot3logit
* URL: https://www.flaviosanti.it/software/plot3logit
* BugReports: https://github.com/f-santi/plot3logit
* Date/Publication: 2019-09-08 15:10:02 UTC
* Number of recursive dependencies: 72

Run `revdep_details(,"plot3logit")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > mod0
    Call:
    nnet::multinom(formula = employment_sit ~ finalgrade + irregularity + 
        hsscore, data = cross_1year)
    
    Coefficients:
               (Intercept) finalgradeLow finalgradeHigh irregularityLow
    Unemployed  -0.4481761    0.05551765    -0.07810893     -0.01874354
    Trainee     -1.3751140    0.14456683    -0.26849829      0.05764144
               irregularityHigh      hsscore
    Unemployed       0.15691595 -0.016619227
    Trainee         -0.03477569 -0.009964381
    
    Residual Deviance: 4314.176 
    AIC: 4338.176 
    > 
    > # Assessing the effect of "finalgradeHigh" (explicit notation)
    > field0 <- field3logit(mod0, c(0, 0, 1, 0, 0, 0))
    > gg3logit(field0) + stat_3logit()
    Error: $ operator is invalid for atomic vectors
    Execution halted
    ```

# plotly

<details>

* Version: 4.9.1
* Source code: https://github.com/cran/plotly
* URL: https://plotly-r.com, https://github.com/ropensci/plotly#readme, https://plot.ly/r
* BugReports: https://github.com/ropensci/plotly/issues
* Date/Publication: 2019-11-07 19:00:02 UTC
* Number of recursive dependencies: 152

Run `revdep_details(,"plotly")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [31m──[39m [31m5. Failure: ylim is respected for 1 trace (@test-ggplot-ylim.R#31) [39m [31m────────────────────────────────────────────────────────────────────[39m
      min(info$layout$yaxis$tickvals) not equivalent to 0.
      1/1 mismatches
      [1] NA - 0 == NA
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 1395 | SKIPPED: 43 | WARNINGS: 69 | FAILED: 5 ]
      1. Failure: geom_col is supported (@test-ggplot-col.R#20) 
      2. Failure: geom_col is supported (@test-ggplot-col.R#24) 
      3. Failure: StatDensity2d with GeomPolygon translates to filled path(s) (@test-ggplot-density2d.R#66) 
      4. Failure: tooltip argument respects ordering (@test-ggplot-tooltip.R#39) 
      5. Failure: ylim is respected for 1 trace (@test-ggplot-ylim.R#31) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.6Mb
    ```

# qgcomp

<details>

* Version: 1.3.0
* Source code: https://github.com/cran/qgcomp
* Date/Publication: 2019-12-11 18:20:03 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"qgcomp")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test_bootchooser.R’ failed.
    Last 13 lines of output:
      > res = try(fit1 <- qgcomp(f, expnms=Xnm, data = dat), silent=TRUE)
      > stopifnot(class(res)=="qgcompfit")
      > 
      > fs = Surv(time,d)~. + .^2
      > res = try(fit1 <- qgcomp(fs, expnms=Xnm, data = dat, B=5, MCsize=100), silent=TRUE)
      > stopifnot(class(res)=="qgcompfit")
      > 
      > fs = Surv(time,d)~. + .^2
      > res = try(fit1 <- qgcomp(fs, expnms=Xnm, data = dat, B=5, MCsize=100, parallel=TRUE), silent=TRUE)
      Warning message:
      In socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  :
        port 31996 cannot be opened
      > stopifnot(class(res)=="qgcompfit")
      Error: class(res) == "qgcompfit" is not TRUE
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘parallel’
      All declared Imports should be used.
    ```

# RAM

<details>

* Version: 1.2.1.7
* Source code: https://github.com/cran/RAM
* URL: https://cran.r-project.org/package=RAM, https://bitbucket.org/Wen_Chen/ram_releases/src/
* BugReports: https://bitbucket.org/Wen_Chen/ram_releases/issues/
* Date/Publication: 2018-05-15 15:38:34 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"RAM")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > data(ITS1, ITS2)
    > ranks <- c("p", "c", "o", "f", "g")
    > df <- OTU.recap(data=list(ITS1=ITS1, ITS2=ITS2), ranks=ranks)
    Using ind as id variables
    Using ind as id variables
    Warning in brewer.pal(len, brewer.pal) :
      n too large, allowed maximum for palette Pastel1 is 9
    Returning the palette you asked for with that many colors
    
    Error:   Differing number of values and breaks in manual scale.
      9 values provided compared to 4 breaks.
    [1m<error/rlang_error>[22m
      Differing number of values and breaks in manual scale.
      9 values provided compared to 4 breaks.
    [1mBacktrace:[22m
    [90m    [39m█
    [90m 1. [39m└─RAM::OTU.recap(data = list(ITS1 = ITS1, ITS2 = ITS2), ranks = ranks)
    [90m 2. [39m  └─ggplot2::scale_fill_manual(...)
    [90m 3. [39m    └─ggplot2:::manual_scale(aesthetics, values, breaks, ...)
    Execution halted
    ```

# RGraphics

<details>

* Version: 2.0-14
* Source code: https://github.com/cran/RGraphics
* URL: https://www.stat.auckland.ac.nz/~paul/RG2e/index.html
* Date/Publication: 2016-03-03 05:49:58
* Number of recursive dependencies: 223

Run `revdep_details(,"RGraphics")` for more info

</details>

## Newly broken

*   checking whether package ‘RGraphics’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'coord_trans(xtrans = "exp", ': unused arguments (xtrans = "exp", ytrans = "exp") 
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RGraphics/new/RGraphics.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

*   checking R code for possible problems ... NOTE
    ```
    figure5.11: possible error in coord_trans(xtrans = "exp", ytrans =
      "exp"): unused arguments (xtrans = "exp", ytrans = "exp")
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'playwith', 'pmg', 'rggobi', 'RGraphics', 'SVGAnnotation'
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.9Mb
      sub-directories of 1Mb or more:
        extra   9.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/RGraphics/RGtk2/libs/RGtk2.so':
      dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/RGraphics/RGtk2/libs/RGtk2.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/RGraphics/RGtk2/libs/RGtk2.so
      Reason: image not found
    Error in dyn.load(file, DLLpath = DLLpath, ...) : 
      unable to load shared object '/Users/max/github/forks/ggplot2/revdep/library.noindex/RGraphics/cairoDevice/libs/cairoDevice.so':
      dlopen(/Users/max/github/forks/ggplot2/revdep/library.noindex/RGraphics/cairoDevice/libs/cairoDevice.so, 6): Library not loaded: /Library/Frameworks/GTK+.framework/Versions/2.24.X11/Resources/lib/libgtk-x11-2.0.0.dylib
      Referenced from: /Users/max/github/forks/ggplot2/revdep/library.noindex/RGraphics/cairoDevice/libs/cairoDevice.so
      Reason: image not found
    ```

# rrecsys

<details>

* Version: 0.9.7.3.1
* Source code: https://github.com/cran/rrecsys
* URL: https://rrecsys.inf.unibz.it/
* BugReports: https://github.com/ludovikcoba/rrecsys/issues
* Date/Publication: 2019-06-09 18:45:49 UTC
* Number of recursive dependencies: 47

Run `revdep_details(,"rrecsys")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rrecsys-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dataChart
    > ### Title: Visualization of data characteristics.
    > ### Aliases: dataChart
    > 
    > ### ** Examples
    > 
    > data(mlLatest100k)
    > 
    > a <- defineData(mlLatest100k)
    There are values smaller that the defined threshold. If you want to keep using them please redefine the "minimum" argument.
    > 
    > dataChart(a, x = "items", y = "num_of_ratings")
    Error: Unknown colour name: 0.3
    Execution halted
    ```

# scales

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/scales
* URL: https://scales.r-lib.org, https://github.com/r-lib/scales
* BugReports: https://github.com/r-lib/scales/issues
* Date/Publication: 2019-11-18 16:10:05 UTC
* Number of recursive dependencies: 64

Run `revdep_details(,"scales")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > demo_continuous(c(0, 1e9), labels = label_number_auto())
    scale_x_continuous(labels = label_number_auto())
    > 
    > # Other ranges get the numbers printed in full
    > demo_continuous(c(0, 1e-3), labels = label_number_auto())
    scale_x_continuous(labels = label_number_auto())
    > demo_continuous(c(0, 1), labels = label_number_auto())
    scale_x_continuous(labels = label_number_auto())
    > demo_continuous(c(0, 1e3), labels = label_number_auto())
    scale_x_continuous(labels = label_number_auto())
    > demo_continuous(c(0, 1e6), labels = label_number_auto())
    scale_x_continuous(labels = label_number_auto())
    > 
    > # Transformation is applied individually so you get as little
    > # scientific notation as possible
    > demo_log10(c(1, 1e7), labels = label_number_auto())
    scale_x_log10(labels = label_number_auto())
    Error in `$<-.data.frame`(`*tmp*`, ".label", value = c("10", "1 000",  : 
      replacement has 4 rows, data has 5
    Calls: <Anonymous> ... guide_train -> guide_train.axis -> $<- -> $<-.data.frame
    Execution halted
    ```

# sentometrics

<details>

* Version: 0.8.0
* Source code: https://github.com/cran/sentometrics
* URL: https://github.com/sborms/sentometrics
* BugReports: https://github.com/sborms/sentometrics/issues
* Date/Publication: 2020-01-13 21:10:03 UTC
* Number of recursive dependencies: 121

Run `revdep_details(,"sentometrics")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      cannot open the connection
      [1mBacktrace:[22m
      [90m 1. [39msentometrics::sento_model(...)
      [90m 2. [39msentometrics:::run_sento_modelIter(...)
      [90m 3. [39mparallel::makeCluster(min(parallel::detectCores(), nCore))
      [90m 4. [39mparallel::makePSOCKcluster(names = spec, ...)
      [90m 5. [39mparallel:::newPSOCKnode(names[[i]], options = options, rank = i)
      [90m 6. [39mbase::socketConnection(...)
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 176 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 1 ]
      1. Error: (unknown) (@test_modelling.R#69) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘MCS’, ‘lexicon’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4436 marked UTF-8 strings
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# seqCAT

<details>

* Version: 1.6.3
* Source code: https://github.com/cran/seqCAT
* Date/Publication: 2019-08-14
* Number of recursive dependencies: 115

Run `revdep_details(,"seqCAT")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 101 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 16 ]
      1. Failure: correct number of variants per impact are plotted (@test_06_plot_impacts.R#20) 
      2. Failure: correct number of variants per impact are plotted (@test_06_plot_impacts.R#21) 
      3. Failure: correct number of variants per impact are plotted (@test_06_plot_impacts.R#22) 
      4. Failure: correct number of variants per impact are plotted (@test_06_plot_impacts.R#23) 
      5. Failure: correct impact distributions are calculated (@test_06_plot_impacts.R#27) 
      6. Failure: correct impact distributions are calculated (@test_06_plot_impacts.R#28) 
      7. Failure: correct impact distributions are calculated (@test_06_plot_impacts.R#29) 
      8. Failure: correct impact distributions are calculated (@test_06_plot_impacts.R#30) 
      9. Failure: correct impact distributions are calculated (@test_06_plot_impacts.R#31) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# Seurat

<details>

* Version: 3.1.2
* Source code: https://github.com/cran/Seurat
* URL: http://www.satijalab.org/seurat, https://github.com/satijalab/seurat
* BugReports: https://github.com/satijalab/seurat/issues
* Date/Publication: 2019-12-12 22:20:06 UTC
* Number of recursive dependencies: 228

Run `revdep_details(,"Seurat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Seurat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DoHeatmap
    > ### Title: Feature expression heatmap
    > ### Aliases: DoHeatmap
    > 
    > ### ** Examples
    > 
    > DoHeatmap(object = pbmc_small)
    Error in data.frame(group = sort(x = group.use), x = x.divs) : 
      arguments imply differing number of rows: 83, 0
    Calls: DoHeatmap -> data.frame
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(Seurat)
      > 
      > test_check("Seurat")
      OMP: Error #15: Initializing libomp.dylib, but found libomp.dylib already initialized.
      OMP: Hint This means that multiple copies of the OpenMP runtime have been linked into the program. That is dangerous, since it can degrade performance or cause incorrect results. The best thing to do is to ensure that only a single OpenMP runtime is linked into the process, e.g. by avoiding static linking of the OpenMP runtime in any library. As an unsafe, unsupported, undocumented workaround you can set the environment variable KMP_DUPLICATE_LIB_OK=TRUE to allow the program to continue to execute, but that may cause crashes or silently produce incorrect results. For more information, please see http://openmp.llvm.org/
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘loomR’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘loomR’
    ```

# spartan

<details>

* Version: 3.0.2
* Source code: https://github.com/cran/spartan
* URL: http://www.york.ac.uk/ycil/software/spartan
* BugReports: http://github.com/kalden/spartan/issues
* Date/Publication: 2018-11-19 18:20:03 UTC
* Number of recursive dependencies: 80

Run `revdep_details(,"spartan")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [31m──[39m [31m7. Error: oat_plotResultDistribution_overTime (@test_robustness_analysis.R#376) [39m [31m───────────────────────────────────────────────────────[39m
      (converted from warning) `fun.y` is deprecated. Use `fun` instead.
      [1mBacktrace:[22m
      [90m 1. [39mspartan::oat_plotResultDistribution(...) [90mtestthat/test_robustness_analysis.R:376:2[39m
      [90m 2. [39mspartan::oat_plotResultDistribution(...)
      [90m 3. [39mggplot2::stat_summary(...)
      [90m 4. [39mrlang::warn("`fun.y` is deprecated. Use `fun` instead.")
      [90m 5. [39mbase::warning(cnd)
      [90m 6. [39mbase::withRestarts(...)
      [90m 7. [39mbase:::withOneRestart(expr, restarts[[1L]])
      [90m 8. [39mbase:::doWithOneRestart(return(expr), restart)
      
      ══ DONE ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gplots’
      All declared Imports should be used.
    ```

# survsup

<details>

* Version: 0.0.2
* Source code: https://github.com/cran/survsup
* URL: http://github.com/dlindholm/survsup
* BugReports: http://github.com/dlindholm/survsup/issues
* Date/Publication: 2019-05-07 07:40:03 UTC
* Number of recursive dependencies: 61

Run `revdep_details(,"survsup")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > library(survsup); library(ggplot2); library(dplyr); library(survival)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > retinopathy %>%
    +     survfit(Surv(futime, status) ~ trt, data = .) %>%
    +    plot_survfit() %>%
    + 	   nar()
    Error in `$<-.data.frame`(`*tmp*`, "y", value = NA_real_) : 
      replacement has 1 row, data has 0
    Calls: %>% ... withVisible -> <Anonymous> -> nar -> $<- -> $<-.data.frame
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘stats’ ‘survival’ ‘utils’
      All declared Imports should be used.
    ```

# TPP

<details>

* Version: 3.12.0
* Source code: https://github.com/cran/TPP
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 97

Run `revdep_details(,"TPP")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      [[1]]
      
      [[2]]
      
      [[3]]
      
      [[1]]
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 271 | SKIPPED: 1 | WARNINGS: 13281 | FAILED: 1 ]
      1. Error: NPARC_allok_plot (@test_analyzeTPPTR.R#61) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.6Mb
      sub-directories of 1Mb or more:
        data           1.9Mb
        example_data   8.0Mb
        test_data      1.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘broom’
      All declared Imports should be used.
    Unexported objects imported by ':::' calls:
      ‘doParallel:::.options’ ‘mefa:::rep.data.frame’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    File ‘TPP/R/TPP.R’:
      .onLoad calls:
        packageStartupMessage(msgText, "\n")
    
    See section ‘Good practice’ in '?.onAttach'.
    
    plot_fSta_distribution: no visible binding for global variable
      ‘..density..’
    plot_pVal_distribution: no visible binding for global variable
      ‘..density..’
    Undefined global functions or variables:
      ..density..
    ```

# trialr

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/trialr
* URL: https://github.com/brockk/trialr
* BugReports: https://github.com/brockk/trialr/issues
* Date/Publication: 2020-01-08 22:30:10 UTC
* Number of recursive dependencies: 108

Run `revdep_details(,"trialr")` for more info

</details>

## Newly broken

*   checking whether package ‘trialr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/trialr/new/trialr.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 12.6Mb
      sub-directories of 1Mb or more:
        doc    4.1Mb
        libs   7.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Installation

### Devel

```
* installing *source* package ‘trialr’ ...
** package ‘trialr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/AugBin2T1A.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/BebopInPeps2.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmEmpiricNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmOneParamLogisticGammaPrior.stan
Wrote C++ file "stan_files/CrmEmpiricNormalPrior.cc"
Wrote C++ file "stan_files/CrmOneParamLogisticGammaPrior.cc"
Wrote C++ file "stan_files/AugBin2T1A.cc"
Wrote C++ file "stan_files/BebopInPeps2.cc"
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmOneParamLogisticNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmTwoParamLogisticNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/EffTox.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/ThallHierarchicalBinary.stan


Wrote C++ file "stan_files/ThallHierarchicalBinary.cc"
Wrote C++ file "stan_files/CrmTwoParamLogisticNormalPrior.cc"
Wrote C++ file "stan_files/CrmOneParamLogisticNormalPrior.cc"
Wrote C++ file "stan_files/EffTox.cc"
Error in readRDS("/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpyZC0Ey/filefb6a2d8d894a") : 
  error reading from connection
Calls: .Last -> readRDS
Execution halted
make: *** [stan_files/CrmOneParamLogisticNormalPrior.cc] Error 1
make: *** Waiting for unfinished jobs....
rm stan_files/EffTox.cc stan_files/BebopInPeps2.cc stan_files/CrmEmpiricNormalPrior.cc stan_files/ThallHierarchicalBinary.cc stan_files/CrmTwoParamLogisticNormalPrior.cc stan_files/CrmOneParamLogisticNormalPrior.cc stan_files/CrmOneParamLogisticGammaPrior.cc stan_files/AugBin2T1A.cc
ERROR: compilation failed for package ‘trialr’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/trialr/new/trialr.Rcheck/trialr’

```
### CRAN

```
* installing *source* package ‘trialr’ ...
** package ‘trialr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/AugBin2T1A.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/BebopInPeps2.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmEmpiricNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmOneParamLogisticGammaPrior.stan
Wrote C++ file "stan_files/CrmEmpiricNormalPrior.cc"
Wrote C++ file "stan_files/CrmOneParamLogisticGammaPrior.cc"
Wrote C++ file "stan_files/BebopInPeps2.cc"
Wrote C++ file "stan_files/AugBin2T1A.cc"
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmOneParamLogisticNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/CrmTwoParamLogisticNormalPrior.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/EffTox.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/ThallHierarchicalBinary.stan


Wrote C++ file "stan_files/CrmOneParamLogisticNormalPrior.cc"
Wrote C++ file "stan_files/CrmTwoParamLogisticNormalPrior.cc"
Wrote C++ file "stan_files/ThallHierarchicalBinary.cc"
Wrote C++ file "stan_files/EffTox.cc"
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.cpp -o init.o


/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/AugBin2T1A.cc -o stan_files/AugBin2T1A.o


/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/BebopInPeps2.cc -o stan_files/BebopInPeps2.o


/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/CrmEmpiricNormalPrior.cc -o stan_files/CrmEmpiricNormalPrior.o


/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/CrmOneParamLogisticGammaPrior.cc -o stan_files/CrmOneParamLogisticGammaPrior.o


In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/BebopInPeps2.cc:3:
In file included from stan_files/BebopInPeps2.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/BebopInPeps2.cc:3:
stan_files/BebopInPeps2.hpp:408:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
In file included from stan_files/CrmEmpiricNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/CrmEmpiricNormalPrior.cc:3:
stan_files/CrmEmpiricNormalPrior.hpp:272:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticGammaPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/CrmOneParamLogisticGammaPrior.cc:3:
stan_files/CrmOneParamLogisticGammaPrior.hpp:306:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/AugBin2T1A.cc:3:
In file included from stan_files/AugBin2T1A.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/AugBin2T1A.cc:3:
stan_files/AugBin2T1A.hpp:370:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
21 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/CrmOneParamLogisticNormalPrior.cc -o stan_files/CrmOneParamLogisticNormalPrior.o


21 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/CrmTwoParamLogisticNormalPrior.cc -o stan_files/CrmTwoParamLogisticNormalPrior.o


21 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/EffTox.cc -o stan_files/EffTox.o


In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
21 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/ThallHierarchicalBinary.cc -o stan_files/ThallHierarchicalBinary.o
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmOneParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/CrmOneParamLogisticNormalPrior.cc:3:
stan_files/CrmOneParamLogisticNormalPrior.hpp:305:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
In file included from stan_files/CrmTwoParamLogisticNormalPrior.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/CrmTwoParamLogisticNormalPrior.cc:3:
stan_files/CrmTwoParamLogisticNormalPrior.hpp:316:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/EffTox.cc:3:
In file included from stan_files/EffTox.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/EffTox.cc:3:
stan_files/EffTox.hpp:479:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:31:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/CholmodSupport:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/KroneckerProduct:34:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:39:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/Polynomials:135:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:22:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigen.h:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/RcppEigenForward.h:40:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/SparseExtra:51:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/RcppEigen/include/unsupported/Eigen/../../Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:45:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core.hpp:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints_nested.hpp:17:13: warning: 'static' function 'set_zero_all_adjoints_nested' declared in header file should be declared 'static inline' [-Wunneeded-internal-declaration]
static void set_zero_all_adjoints_nested() {
            ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
In file included from stan_files/ThallHierarchicalBinary.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/rstan/include/rstan/stan_fit.hpp:35:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat.hpp:336:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_log.hpp:5:
/Users/max/github/forks/ggplot2/revdep/library.noindex/trialr/StanHeaders/include/stan/math/prim/mat/prob/poisson_log_glm_lpmf.hpp:64:59: warning: unused typedef 'T_alpha_val' [-Wunused-local-typedef]
      typename partials_return_type<T_alpha>::type>::type T_alpha_val;
                                                          ^
In file included from stan_files/ThallHierarchicalBinary.cc:3:
stan_files/ThallHierarchicalBinary.hpp:188:24: warning: unused typedef 'local_scalar_t__' [-Wunused-local-typedef]
        typedef double local_scalar_t__;
                       ^
21 warnings generated.
21 warnings generated.
21 warnings generated.
21 warnings generated.
/usr/local/clang8/bin/clang++ -std=gnu++14 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/clang8/lib -o trialr.so stan_files/AugBin2T1A.o stan_files/BebopInPeps2.o stan_files/CrmEmpiricNormalPrior.o stan_files/CrmOneParamLogisticGammaPrior.o stan_files/CrmOneParamLogisticNormalPrior.o stan_files/CrmTwoParamLogisticNormalPrior.o stan_files/EffTox.o stan_files/ThallHierarchicalBinary.o init.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
rm stan_files/EffTox.cc stan_files/BebopInPeps2.cc stan_files/CrmEmpiricNormalPrior.cc stan_files/ThallHierarchicalBinary.cc stan_files/CrmTwoParamLogisticNormalPrior.cc stan_files/CrmOneParamLogisticNormalPrior.cc stan_files/CrmOneParamLogisticGammaPrior.cc stan_files/AugBin2T1A.cc
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/trialr/old/trialr.Rcheck/00LOCK-trialr/00new/trialr/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (trialr)

```
# tricolore

<details>

* Version: 1.2.1
* Source code: https://github.com/cran/tricolore
* Date/Publication: 2019-07-29 11:00:02 UTC
* Number of recursive dependencies: 99

Run `revdep_details(,"tricolore")` for more info

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
    > tricolore:::ColorKeySextant(center = prop.table(runif(3)),
    +                            values = c('#01A0C6', '#B8B3D8', '#F11D8C',
    +                                       '#FFB3B3', '#FFFF00', '#B3DCC3'),
    +                            label_as = 'pct_diff', show_center = TRUE)
    Error in (function (el, elname)  : 
      "plot.title.position" is not a valid theme element name.
    Calls: <Anonymous> ... update_theme -> do.call -> <Anonymous> -> mapply -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1mBacktrace:[22m
      [90m 1. [39mtricolore::Tricolore(P, "a", "b", "c", breaks = Inf)
      [90m 2. [39mtricolore:::ColorKeyTricolore(...)
      [90m 3. [39mtricolore:::BasicKey(...)
      [90m 4. [39mggtern:::`+.gg`(...)
      [90m 5. [39mggtern:::add_ggplot(e1, e2, e2name)
      [90m 6. [39mggtern:::update_theme(p$theme, object)
      [90m 9. [39mbase::mapply(validate_element, elements, names(elements))
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 33 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: NA, Inf, NaNs in input return NA in output (@test-global.R#82) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# xpose

<details>

* Version: 0.4.6
* Source code: https://github.com/cran/xpose
* URL: https://github.com/UUPharmacometrics/xpose
* BugReports: https://github.com/UUPharmacometrics/xpose/issues
* Date/Publication: 2020-01-12 21:50:02 UTC
* Number of recursive dependencies: 102

Run `revdep_details(,"xpose")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
          filter
      
      > 
      > test_check("xpose")
      [31m──[39m [31m1. Failure: parial xp_themes are properly added (@test-update_themes.R#39) [39m [31m────────────────────────────────────────────────────────────[39m
      update_themes(xpdb = xpdb_ex_pk, xp_theme = c(point_color = "green"))$xp_theme not equivalent to `theme_xp_custom`.
      Component "labeller": target, current do not match when deparsed
      
      ══ testthat results  ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 522 | SKIPPED: 6 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: parial xp_themes are properly added (@test-update_themes.R#39) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc   3.4Mb
    ```

