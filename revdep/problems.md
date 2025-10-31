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
    Quitting
    Bye Bye: Did you know that Jupiter is biggest planet in our solar system :)?
    Warning in ggplot2::scale_y_continuous(trans = "log10", breaks = c(0, 0.5,  :
      log-10 transformation introduced infinite values.
    Warning in ggplot2::scale_y_continuous(trans = "log10", breaks = c(0, 0.5,  :
      log-10 transformation introduced infinite values.
    Error in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid mathematical annotation
    Calls: <Anonymous> ... <Anonymous> -> widthDetails -> widthDetails.text -> grid.Call
    Execution halted
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) compare_cohorts.Rd:35: Lost braces in \itemize; meant \describe ?
    checkRd: (-1) compare_cohorts.Rd:36: Lost braces in \itemize; meant \describe ?
    ```

# APCI

<details>

* Version: 1.0.8
* GitHub: NA
* Source code: https://github.com/cran/APCI
* Date/Publication: 2024-09-02 20:20:06 UTC
* Number of recursive dependencies: 75

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
       -0.355316789   0.089582104   0.192153615  -0.331514454   0.179289902 
          acc8:pcc3     acc9:pcc3     acc1:pcc4     acc2:pcc4     acc3:pcc4 
       -0.119021995   0.149440647  -0.484785372  -0.013962924   0.292056275 
          acc4:pcc4     acc5:pcc4     acc6:pcc4     acc7:pcc4     acc8:pcc4 
        0.302556444   0.215656449  -0.171245134  -0.083395132  -0.217705607 
          acc9:pcc4     acc1:pcc5     acc2:pcc5     acc3:pcc5     acc4:pcc5 
        0.125040240   0.105797087   0.356674102  -0.086783879  -0.100255900 
          acc5:pcc5     acc6:pcc5     acc7:pcc5     acc8:pcc5     acc9:pcc5 
       -0.587743743  -0.362151437   0.333929747   0.603889317  -0.348507261 
      Killed
    ```

# bmgarch

<details>

* Version: 2.0.0
* GitHub: https://github.com/ph-rast/bmgarch
* Source code: https://github.com/cran/bmgarch
* Date/Publication: 2023-09-12 00:40:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "bmgarch")` for more info

</details>

## Newly broken

*   checking whether package ‘bmgarch’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bmgarch/new/bmgarch.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 303.9Mb
      sub-directories of 1Mb or more:
        libs  303.0Mb
    ```

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) dot-pred_array_to_df.Rd:20: Lost braces; missing escapes or markup?
        20 | Helper function for as.data.frame.{fitted, forecast}. Converts predictive array to data.frame.
           |                                   ^
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Installation

### Devel

```
* installing *source* package ‘bmgarch’ ...
** package ‘bmgarch’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_DCCMGARCH_namespace::model_DCCMGARCH; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.4.0/lib/R/etc/Makeconf:202: stanExports_DCCMGARCH.o] Error 1
ERROR: compilation failed for package ‘bmgarch’
* removing ‘/tmp/workdir/bmgarch/new/bmgarch.Rcheck/bmgarch’


```
### CRAN

```
* installing *source* package ‘bmgarch’ ...
** package ‘bmgarch’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.4.0/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (bmgarch)


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
       diff| Mean relative difference: 1
       info| legend labels
      ----- FAILED[data]: test-guide_legend_interactive.R<64--109>
       call| expect_equal(tooltips, c("Male", "Female"), info = "legend labels tooltips")
       diff| Modes: character, list
       diff| Lengths: 2, 0
       diff| target is character, current is list
       info| legend labels tooltips
      Error: 4 out of 1286 tests failed
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

# MetAlyzer

<details>

* Version: 1.1.0
* GitHub: https://github.com/nilsmechtel/MetAlyzer
* Source code: https://github.com/cran/MetAlyzer
* Date/Publication: 2024-12-06 14:00:02 UTC
* Number of recursive dependencies: 92

Run `revdepcheck::cloud_details(, "MetAlyzer")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘MetAlyzer_User_Guide.Rmd’ using rmarkdown
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
    Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
    ℹ Please use tidy evaluation idioms with `aes()`.
    ℹ See also `vignette("ggplot2-in-packages")` for more information.
    ℹ The deprecated feature was likely used in the NMF package.
      Please report the issue to the authors.
    `geom_smooth()` using formula = 'y ~ x'
    Error in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid mathematical annotation
    Calls: <Anonymous> ... <Anonymous> -> widthDetails -> widthDetails.text -> grid.Call
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
    > TestFit <- flFitSpline(time = time,
    +                        fl_data = data,
    +                        ID = 'TestFit',
    +                        control = fl.control(fit.opt = 's', x_type = 'time'))
    > 
    > plot(TestFit)
    Error in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid mathematical annotation
    Calls: plot ... drawDetails -> drawDetails.text -> grid.Call.graphics
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
    Error in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid mathematical annotation
    Calls: iid.test ... <Anonymous> -> widthDetails -> widthDetails.text -> grid.Call
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
    190    s36     10 0.28311741   0.075931996 4.479054e-03 0.694
    191    s36     11 0.26103896   0.099035285 1.276679e-04 0.633
    192    s36     12 0.22566667   0.082398303 1.734336e-02 0.796
    > seasonTrend(x, plot = TRUE, ncol = 4)
    Warning: Removed 58 rows containing missing values or values outside the scale range
    (`geom_bar()`).
    Error in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid mathematical annotation
    Calls: <Anonymous> ... <Anonymous> -> widthDetails -> widthDetails.text -> grid.Call
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
    ...
    > data(tidfit)
    > 
    > # plot using defaults
    > fitmoplot(tidfit)
    Warning: Removed 8 rows containing missing values or values outside the scale range
    (`geom_line()`).
    Error in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : 
      invalid mathematical annotation
    Calls: <Anonymous> ... <Anonymous> -> widthDetails -> widthDetails.text -> grid.Call
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

