# apyramid

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/apyramid
* URL: https://github.com/R4EPI/apyramid, https://r4epis.netlify.com
* BugReports: https://github.com/R4EPI/apyramid/issues
* Date/Publication: 2020-01-13 15:50:06 UTC
* Number of recursive dependencies: 119

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
      [31m──[39m [31m1. Failure: missing split data are removed before plotting (@test-age-pyramid.R#171) [39m [31m───────────────────────────────────────────────────[39m
      `age_pyramid(dat, age_group = "AGE", na.rm = FALSE)` produced warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 72 | SKIPPED: 11 | WARNINGS: 11 | FAILED: 1 ]
      1. Failure: missing split data are removed before plotting (@test-age-pyramid.R#171) 
      
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
* Number of recursive dependencies: 75

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
      [1mBacktrace:[22m
      [90m  1. [39mggplot2::qplot(cty, hwy, data = mpg, geom = "smooth")
      [90m  9. [39mautocogs:::expect_auto_cogs(...)
      [90m 11. [39mautocogs:::plot_cogs(.)
      [90m 19. [39mautocogs:::get_layer_info(p, keep = keep_layers, ...)
      [90m 21. [39mautocogs:::layer_info.ggplot(p, keep = keep, ...)
      [90m 22. [39mbase::lapply(...)
      [90m 23. [39mautocogs:::FUN(X[[i]], ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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
* Number of recursive dependencies: 78

Run `revdep_details(,"bayesdfa")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        libs   4.8Mb
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
* Number of recursive dependencies: 39

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
* Number of recursive dependencies: 78

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
      
      [31m──[39m [31m2. Error: autoplot_biclustermd() plots row clusters in correct clusters (@test-autoplot_biclustermd.R#38) [39m [31m──────────────────────────────[39m
      arguments imply differing number of rows: 6, 0
      [1mBacktrace:[22m
      [90m 1. [39mbase::data.frame(...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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

# CNPBayes

<details>

* Version: 1.13.5
* Source code: https://github.com/cran/CNPBayes
* URL: https://github.com/scristia/CNPBayes
* BugReports: https://github.com/scristia/CNPBayes/issues
* Date/Publication: 2019-01-05
* Number of recursive dependencies: 162

Run `revdep_details(,"CNPBayes")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## In both

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘CNPBayes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggChains
    > ### Title: Trace plots of MCMC chains and mixture model densities
    > ### Aliases: ggChains ggMixture ggMixture,MultiBatchCopyNumber-method
    > ###   ggMixture,MultiBatchCopyNumberPooled-method
    > ###   ggMixture,MultiBatchModel-method ggMixture,MultiBatch-method
    > ###   ggMixture,MultiBatchPooled-method ggChains,MultiBatchModel-method
    > ###   ggChains,MultiBatchPooled-method
    > 
    > ### ** Examples
    > 
    >   sb <- SingleBatchModelExample
    >   iter(sb) <- 1000
    >   burnin(sb) <- 100
    >   sb <- posteriorSimulation(sb)
    >   fig.chains <- ggChains(sb)
    Error: 1 components of `...` had unexpected names.
    
    We detected these problematic arguments:
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      [31m──[39m [31m4. Failure: easy mendelian example (@test_trios.R#524) [39m [31m─────────────────────────────────────────────────────────────────────────────────[39m
      `z.m` not identical to 2L.
      1/1 mismatches
      [1] 3 - 2 == 1
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 305 | SKIPPED: 3 | WARNINGS: 3 | FAILED: 4 ]
      1. Failure: sigma2_pooled (@test_SingleBatchPooled.R#24) 
      2. Failure: ggfun (@test_ggfuns.R#9) 
      3. Failure: kbatch (@test_multibatch.R#271) 
      4. Failure: easy mendelian example (@test_trios.R#524) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking for missing documentation entries ... WARNING
    ```
    ...
      generic 'sigma' and siglist 'MultiBatchPooled'
      generic 'sigma<-' and siglist 'MixtureModel'
      generic 'sigma<-' and siglist 'MultiBatchPooled'
      generic 'tau2' and siglist 'MultiBatch'
      generic 'theta' and siglist 'MultiBatch'
      generic 'theta<-' and siglist 'McmcChains,ANY'
      generic 'theta<-' and siglist 'MixtureModel,ANY'
      generic 'theta<-' and siglist 'MultiBatch,matrix'
      generic 'theta<-' and siglist 'MultiBatchModel,ANY'
      generic 'thin' and siglist 'MultiBatch'
      generic 'thin' and siglist 'MultiBatchList'
      generic 'thin<-' and siglist 'McmcParams,numeric'
      generic 'thin<-' and siglist 'MultiBatch,numeric'
      generic 'thin<-' and siglist 'MultiBatchList,numeric'
      generic 'triodata_lrr' and siglist 'TrioBatchModel'
      generic 'z' and siglist 'MultiBatch'
      generic 'zFreq' and siglist 'MultiBatch'
    All user-level objects in a package (including S4 classes and methods)
    should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
    Slots for class 'MultiBatch'
      Code: chains current_values data down_sample flags parameters specs
            summaries
      Docs: chains current_values data down_sample flags parameters
            summaries
    
    S4 class codoc mismatches from documentation object 'MultiBatchModel-class':
    Slots for class 'MultiBatchModel'
      Code: .internal.constraint .internal.counter batch batchElements data
            data.mean data.prec hyperparams k label_switch loglik logprior
            marginal_lik mcmc.chains mcmc.params modes mu nu.0 pi
            predictive probz sigma2 sigma2.0 tau2 theta u z zfreq zstar
      Inherited: k hyperparams theta sigma2 nu.0 sigma2.0 pi mu tau2
            predictive zstar data data.mean data.prec z zfreq probz u
            logprior loglik mcmc.chains batch batchElements modes
            mcmc.params label_switch marginal_lik .internal.constraint
            .internal.counter
      Docs: .internal.constraint batch batchElements data data.mean
            data.prec hyperparams is_mendelian k label_switch loglik
            logprior mcmc.chains mcmc.params modes mu nu.0 pi probz sigma2
            sigma2.0 tau2 theta z zfreq
    ```

*   checking Rd \usage sections ... WARNING
    ```
    ...
    
    Documented arguments not in \usage in documentation object 'iter<-':
      ‘force’
    
    Documented arguments not in \usage in documentation object 'mcmcParams':
      ‘force’
    
    Undocumented arguments in documentation object 'sigma<-'
      ‘value’
    
    Undocumented arguments in documentation object 'singleBatchGuided,MultiBatchList,MultiBatch-method'
      ‘x’ ‘guide’
    
    Undocumented arguments in documentation object 'theta'
      ‘value’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        R     2.1Mb
        doc   3.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppArmadillo’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    batch<-,MultiBatch-numeric: no visible global function definition for
      ‘spec’
    batch<-,MultiBatch-numeric: no visible global function definition for
      ‘spec<-’
    coerce,McmcChains-list: no visible binding for global variable ‘s’
    computePrec,MultiBatch: no visible binding for global variable ‘prec’
    findSurrogates,MultiBatch: no visible binding for global variable ‘id’
    findSurrogates,MultiBatch: no visible binding for global variable
      ‘provisional_batch’
    findSurrogates,MultiBatch: no visible binding for global variable
      ‘batch_labels’
    sigma,MultiBatchCopyNumberPooled: no visible binding for global
      variable ‘s2’
    Undefined global functions or variables:
      . .gibbs_trios_mcmc2 .gibbs_trios_mcmc3 := batch_index batch_labels
      batches bk copy_number father id log_ratio maplabel medians model
      mother mprob nhom parents prec provisional_batch s s2 snpdat spec
      spec<- t.test value
    Consider adding
      importFrom("stats", "t.test")
    to your NAMESPACE file.
    ```

# dabestr

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/dabestr
* URL: https://github.com/ACCLAB/dabestr
* BugReports: https://github.com/ACCLAB/dabestr/issues
* Date/Publication: 2019-07-04 16:20:05 UTC
* Number of recursive dependencies: 123

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
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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

# DeLorean

<details>

* Version: 1.5.0
* Source code: https://github.com/cran/DeLorean
* Date/Publication: 2018-10-17 22:30:16 UTC
* Number of recursive dependencies: 118

Run `revdep_details(,"DeLorean")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        libs   4.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lattice’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Newly fixed

*   checking whether package ‘DeLorean’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DeLorean/old/DeLorean.Rcheck/00install.out’ for details.
    ```

# describedata

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/describedata
* Date/Publication: 2019-08-02 11:50:02 UTC
* Number of recursive dependencies: 65

Run `revdep_details(,"describedata")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘describedata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gladder
    > ### Title: Replica of Stata's gladder function
    > ### Aliases: gladder
    > 
    > ### ** Examples
    > 
    > gladder(iris$Sepal.Length)
    Error in FUN(X[[i]], ...) : object '..density..' not found
    Calls: <Anonymous> ... ggplot_build.ggplot -> by_layer -> f -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

# ezplot

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2019-07-20 21:20:03 UTC
* Number of recursive dependencies: 88

Run `revdep_details(,"ezplot")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: bar_plot
    > ### Title: bar_plot
    > ### Aliases: bar_plot
    > 
    > ### ** Examples
    > 
    > library(tsibbledata)
    > library(lubridate)
    
    Attaching package: ‘lubridate’
    
    The following object is masked from ‘package:base’:
    
        date
    
    > bar_plot(ansett, "year(Week)", "Passengers")
    > bar_plot(ansett, "year(Week)", "Passengers", "Class")
    Error in as.character(function (x)  : 
      cannot coerce type 'closure' to vector of type 'character'
    Calls: bar_plot -> scale_fill_manual -> manual_scale
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      [31m──[39m [31m2. Error: bar_plot works (@test-bar_plot.R#10) [39m [31m─────────────────────────────────────────────────────────────────────────────────────────[39m
      cannot coerce type 'closure' to vector of type 'character'
      [1mBacktrace:[22m
      [90m 1. [39mezplot::bar_plot(mtcars, "cyl", "1", "am", position = "fill")
      [90m 2. [39mggplot2::scale_fill_manual(...)
      [90m 3. [39mggplot2:::manual_scale(aesthetics, values, breaks, ...)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 57 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: area_plot works (@test-area_plot.R#9) 
      2. Error: bar_plot works (@test-bar_plot.R#10) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# GGally

<details>

* Version: 1.4.0
* Source code: https://github.com/cran/GGally
* URL: https://ggobi.github.io/ggally, https://github.com/ggobi/ggally
* BugReports: https://github.com/ggobi/ggally/issues
* Date/Publication: 2018-05-17 23:31:19 UTC
* Number of recursive dependencies: 137

Run `revdep_details(,"GGally")` for more info

</details>

## Newly broken

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

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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

# ggcyto

<details>

* Version: 1.12.0
* Source code: https://github.com/cran/ggcyto
* URL: https://github.com/RGLab/ggcyto/issues
* Date/Publication: 2019-05-02
* Number of recursive dependencies: 159

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
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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
* Number of recursive dependencies: 94

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

# ggeasy

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/ggeasy
* URL: https://github.com/jonocarroll/ggeasy
* BugReports: https://github.com/jonocarroll/ggeasy/issues
* Date/Publication: 2020-01-31 16:00:02 UTC
* Number of recursive dependencies: 116

Run `revdep_details(,"ggeasy")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 185 | SKIPPED: 4 | WARNINGS: 0 | FAILED: 11 ]
      1. Failure: set text sizes (@test-text.R#14) 
      2. Failure: set text sizes (@test-text.R#26) 
      3. Failure: set text sizes (@test-text.R#38) 
      4. Failure: set text sizes (@test-text.R#50) 
      5. Failure: set text colors (@test-text.R#135) 
      6. Failure: set text colors (@test-text.R#147) 
      7. Failure: set text colors (@test-text.R#154) 
      8. Failure: set text colors (@test-text.R#161) 
      9. Failure: set text colors (@test-text.R#168) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggfortify

<details>

* Version: 0.4.8
* Source code: https://github.com/cran/ggfortify
* URL: https://github.com/sinhrks/ggfortify
* BugReports: https://github.com/sinhrks/ggfortify/issues
* Date/Publication: 2019-11-10 21:40:02 UTC
* Number of recursive dependencies: 126

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
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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

# ggpol

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/ggpol
* URL: https://github.com/erocoar/ggpol
* Date/Publication: 2019-03-14 13:40:02 UTC
* Number of recursive dependencies: 54

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

* Version: 0.3.5
* Source code: https://github.com/cran/ggspectra
* URL: https://www.r4photobiology.info, https://bitbucket.org/aphalo/ggspectra
* BugReports: https://bitbucket.org/aphalo/ggspectra
* Date/Publication: 2020-01-16 16:30:02 UTC
* Number of recursive dependencies: 62

Run `revdep_details(,"ggspectra")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggspectra-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggspectra-package
    > ### Title: ggspectra: Extensions to 'ggplot2' for Radiation Spectra
    > ### Aliases: ggspectra ggspectra-package
    > 
    > ### ** Examples
    > 
    > 
    > library(photobiologyWavebands)
    > 
    > ggplot(sun.spct) + geom_line() + stat_peaks(span = NULL)
    Error in FUN(X[[i]], ...) : object 'wl.color' not found
    Calls: <Anonymous> ... <Anonymous> -> f -> scales_add_defaults -> lapply -> FUN
    Execution halted
    ```

# ggstance

<details>

* Version: 0.3.3
* Source code: https://github.com/cran/ggstance
* Date/Publication: 2019-08-19 13:30:03 UTC
* Number of recursive dependencies: 121

Run `revdep_details(,"ggstance")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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
* Number of recursive dependencies: 46

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
# gWQS

<details>

* Version: 2.0.0
* Source code: https://github.com/cran/gWQS
* Date/Publication: 2019-08-27 12:40:02 UTC
* Number of recursive dependencies: 130

Run `revdep_details(,"gWQS")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
      ‘gwqs-vignette.Rnw’using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘gwqs-vignette.Rnw’
      ...
      port 11099 cannot be opened
    
      When sourcing 'gwqs-vignette.R':
    Error: Failed to launch and connect to R worker on local machine 'localhost' from local machine 'imp.atlanticbb.net'.
     * The error produced by socketConnection() was: 'cannot open the connection'
     * In addition, socketConnection() produced 1 warning(s):
       - Warning #1: 'port 11099 cannot be opened' (which suggests that this port is either already occupied by another process or blocked by the firewall on your local machine)
     * The localhost socket connection that failed to connect to the R worker used port 11099 using a communication timeout of 2592000 seconds and a connection timeout of 120 seconds.
     * Worker launch call: '/Library/Frameworks/R.framework/Resources/bin/Rscript' --default-packages=datasets,utils,grDevices,graphics,stats,methods -e 'try(suppressWarnings(cat(Sys.getpid(),file="/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpdLwHk3/future.parent=27319.6ab7238cfd35.pid")), silent = TRUE)' -e 'parallel:::.slaveRSOCK()' MASTER=localhost PORT=11099 OUT=/dev/null TIMEOUT
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# helda

<details>

* Version: 0.9.1
* Source code: https://github.com/cran/helda
* URL: https://www.github.com/Redcart/helda
* BugReports: https://github.com/Redcart/helda/issues
* Date/Publication: 2020-01-31 15:20:02 UTC
* Number of recursive dependencies: 104

Run `revdep_details(,"helda")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component "layers": Component 1: Component 3: Component 1: Lengths: 2, 1
      Component "layers": Component 1: Component 3: Component 1: target is character, current is function
      Component "layers": Component 1: Component 3: Component 2: target, current do not match when deparsed
      Component "layers": Component 1: Component 4: Length mismatch: comparison on first 1 components
      Component "theme": Names: 62 string mismatches
      Component "theme": Length mismatch: comparison on first 65 components
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 4 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Failure: lift curve for titanic data set (@test-lift_curve.R#14) 
      2. Failure: lift effect for titanic data set (@test-lift_effect.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# interactions

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/interactions
* URL: https://interactions.jacob-long.com
* BugReports: https://github.com/jacob-long/interactions/issues
* Date/Publication: 2019-07-05 07:30:23 UTC
* Number of recursive dependencies: 90

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
      [31m──[39m [31m1. Failure: interact_plot linearity.check works (@test_interact_plot.R#157) [39m [31m────────────────────────────────────────────────────────────[39m
      `print(p)` produced messages.
      
      [31m──[39m [31m2. Failure: interact_plot linearity.check works (@test_interact_plot.R#162) [39m [31m────────────────────────────────────────────────────────────[39m
      `print(p)` produced messages.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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
* Number of recursive dependencies: 67

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
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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
* Number of recursive dependencies: 58

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
      
      ══ testthat results  ═══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 5 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Failure: showT.Test runs error-free (@testGraphs.R#10) 
      2. Failure: showANOVA runs error-free (@testGraphs.R#26) 
      3. Failure: showOLS runs error-free (@testGraphs.R#30) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# plot3logit

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/plot3logit
* URL: https://www.flaviosanti.it/software/plot3logit
* BugReports: https://github.com/f-santi/plot3logit
* Date/Publication: 2019-09-08 15:10:02 UTC
* Number of recursive dependencies: 70

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

# PSCBS

<details>

* Version: 0.65.0
* Source code: https://github.com/cran/PSCBS
* URL: https://github.com/HenrikBengtsson/PSCBS
* BugReports: https://github.com/HenrikBengtsson/PSCBS/issues
* Date/Publication: 2019-05-05 22:40:09 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"PSCBS")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/segmentByCBS,futures.R’ failed.
    Last 13 lines of output:
       * The error produced by socketConnection() was: 'cannot open the connection'
       * In addition, socketConnection() produced 1 warning(s):
         - Warning #1: 'port 22510 cannot be opened' (which suggests that this port is either already occupied by another process or blocked by the firewall on your local machine)
       * The localhost socket connection that failed to connect to the R worker used port 22510 using a communication timeout of 120 seconds and a connection timeout of 120 seconds.
       * Worker launch call: '/Library/Frameworks/R.framework/Resources/bin/Rscript' --default-packages=datasets,utils,grDevices,graphics,stats,methods -e '#label=segmentByCBS,futures.R:17494:imp.atlanticbb.net:max' -e 'try(suppressWarnings(cat(Sys.getpid(),file="/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//Rtmp1AIg4a/future.parent=17494.44566eeaca94.pid")), silent = TRUE)' -e 'parallel:::.slaveRSOCK()' MASTER=localhost PORT=22510 OUT=/dev/null TIMEOUT=120 XDR=TRUE.
       * Worker (PID 17610) was successfully killed: TRUE
       * Troubleshooting suggestions:
         - Suggestion #1: Set 'verbose=TRUE' to see more details.
         - Suggestion #2: Set 'outfile=NULL' to see output from worker.
      Calls: plan ... tryCatchList -> tryCatchOne -> <Anonymous> -> <Anonymous>
      In addition: Warning messages:
      1: [ONE-TIME WARNING] Forked processing ('multicore') is disabled in future (>= 1.13.0) when running R from RStudio, because it is considered unstable. Because of this, plan("multicore") will fall back to plan("sequential"), and plan("multiprocess") will fall back to plan("multisession") - not plan("multicore") as in the past. For more details, how to control forked processing or not, and how to silence this warning in future R sessions, see ?future::supportsMulticore 
      2: In socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  :
        port 22510 cannot be opened
      Execution halted
    ```

# RGraphics

<details>

* Version: 2.0-14
* Source code: https://github.com/cran/RGraphics
* URL: https://www.stat.auckland.ac.nz/~paul/RG2e/index.html
* Date/Publication: 2016-03-03 05:49:58
* Number of recursive dependencies: 213

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

# riskRegression

<details>

* Version: 2019.11.03
* Source code: https://github.com/cran/riskRegression
* Date/Publication: 2019-11-04 17:20:02 UTC
* Number of recursive dependencies: 152

Run `revdep_details(,"riskRegression")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-ate.R’ failed.
    Last 13 lines of output:
      +                   B = 5, cpus = 1
      +                   )
      +     yy <- capture.output(print(e.ate2))
      + 
      + })
      Error: Test failed: '[ate] check that bootstrap returns a result'
      * cannot open the connection
      [1mBacktrace:[22m
      [90m 1. [39mriskRegression::ate(...)
      [90m 2. [39mriskRegression:::calcBootATE(...)
      [90m 3. [39mparallel::makeCluster(mc.cores)
      [90m 4. [39mparallel::makePSOCKcluster(names = spec, ...)
      [90m 5. [39mparallel:::newPSOCKnode(names[[i]], options = options, rank = i)
      [90m 6. [39mbase::socketConnection(...)
      Execution halted
    ```

# seqCAT

<details>

* Version: 1.6.3
* Source code: https://github.com/cran/seqCAT
* Date/Publication: 2019-08-14
* Number of recursive dependencies: 114

Run `revdep_details(,"seqCAT")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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
* Number of recursive dependencies: 227

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
* Number of recursive dependencies: 79

Run `revdep_details(,"spartan")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [31m──[39m [31m7. Error: oat_plotResultDistribution_overTime (@test_robustness_analysis.R#376) [39m [31m─────────────────────────────────────────────────[39m
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
      
      ══ DONE ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
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
* Number of recursive dependencies: 60

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

# TFEA.ChIP

<details>

* Version: 1.4.2
* Source code: https://github.com/cran/TFEA.ChIP
* Date/Publication: 2019-07-18
* Number of recursive dependencies: 158

Run `revdep_details(,"TFEA.ChIP")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TFEA.ChIP-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: GeneID2entrez
    > ### Title: Translates gene IDs from Gene Symbol or Ensemble ID to Entrez
    > ###   ID.
    > ### Aliases: GeneID2entrez
    > 
    > ### ** Examples
    > 
    > GeneID2entrez(c('TNMD','DPM1','SCYL3','FGR','CFH','FUCA2','GCLC'))
    Done! 7 genes of 7 successfully converted.
    
    [1] "64102" "8813"  "57147" "2268"  "3075"  "2519"  "2729" 
    > GeneID2entrez(c('Mcm6', 'Rpl7', 'Itch' ), mode ="m2m")
    Error in biomaRt::getBM(attributes = c("ensembl_gene_id", "mgi_symbol",  : 
      The query to the BioMart webservice returned an invalid result: biomaRt expected a character string of length 1. 
    Please report this on the support site at http://support.bioconductor.org
    Calls: GeneID2entrez -> <Anonymous>
    Execution halted
    ```

# tricolore

<details>

* Version: 1.2.1
* Source code: https://github.com/cran/tricolore
* Date/Publication: 2019-07-29 11:00:02 UTC
* Number of recursive dependencies: 98

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
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 33 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: NA, Inf, NaNs in input return NA in output (@test-global.R#82) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# xpose

<details>

* Version: 0.4.7
* Source code: https://github.com/cran/xpose
* URL: https://github.com/UUPharmacometrics/xpose
* BugReports: https://github.com/UUPharmacometrics/xpose/issues
* Date/Publication: 2020-02-04 20:30:02 UTC
* Number of recursive dependencies: 101

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
      [31m──[39m [31m1. Failure: parial xp_themes are properly added (@test-update_themes.R#39) [39m [31m───────────────────────────────────────────[39m
      update_themes(xpdb = xpdb_ex_pk, xp_theme = c(point_color = "green"))$xp_theme not equivalent to `theme_xp_custom`.
      Component "labeller": target, current do not match when deparsed
      
      ══ testthat results  ═════════════════════════════════════════════════════════════════════════════════════════════════════
      [ OK: 521 | SKIPPED: 7 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: parial xp_themes are properly added (@test-update_themes.R#39) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

