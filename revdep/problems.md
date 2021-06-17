# bayesAB

<details>

* Version: 1.1.2
* GitHub: https://github.com/FrankPortman/bayesAB
* Source code: https://github.com/cran/bayesAB
* Date/Publication: 2019-07-02 23:11:08 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "bayesAB")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("bayesAB")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-generics.R:41:3): Success ─────────────────────────────────────
      `plot(x)` produced warnings.
      ── Failure (test-generics.R:42:3): Success ─────────────────────────────────────
      `plot(x, rep(0.5, 4))` produced warnings.
      ── Failure (test-generics.R:46:3): Success ─────────────────────────────────────
      `print(plot(x))` produced warnings.
      ── Failure (test-generics.R:47:3): Success ─────────────────────────────────────
      `print(plot(x, rep(0.5, 4)))` produced warnings.
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 137 ]
      Error: Test failures
      Execution halted
    ```

# BayesianReasoning

<details>

* Version: 0.3.2
* GitHub: https://github.com/gorkang/BayesianReasoning
* Source code: https://github.com/cran/BayesianReasoning
* Date/Publication: 2020-07-03 16:40:16 UTC
* Number of recursive dependencies: 80

Run `cloud_details(, "BayesianReasoning")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(BayesianReasoning)
      > 
      > test_check("BayesianReasoning")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-PPV_heatmap.R:117:3): PPV calculation with area overlay, low uncertainty, and decimals in y axis ──
      p$layers[[3]]$geom_params$description not identical to "40 y.o.\n2 out of 8\nFP = 4.8%\nPPV = 85%".
      target is NULL, current is character
      ── Failure (test-PPV_heatmap.R:146:3): NPV calculation with area overlay and low uncertainty ──
      p$layers[[3]]$geom_params$description not identical to "40 y.o.\n67 out of 68\nFN = 1%\nNPV = 59%".
      target is NULL, current is character
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 27 ]
      Error: Test failures
      Execution halted
    ```

# cvms

<details>

* Version: 1.3.0
* GitHub: https://github.com/ludvigolsen/cvms
* Source code: https://github.com/cran/cvms
* Date/Publication: 2021-06-07 14:40:02 UTC
* Number of recursive dependencies: 124

Run `cloud_details(, "cvms")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          █
       1. └─testthat::expect_true(!p1$guides$fill[[1]]) test_plotting_functions.R:183:2
       2.   └─testthat::quasi_label(enquo(object), label, arg = "object")
       3.     └─rlang::eval_bare(expr, quo_get_env(quo))
      ── Error (test_plotting_functions.R:340:3): plot_confusion_matrix() with sum tiles, class order, and intensity_by percentage ──
      Error: invalid argument type
      Backtrace:
          █
       1. └─testthat::expect_true(!p1$guides$fill[[1]]) test_plotting_functions.R:340:2
       2.   └─testthat::quasi_label(enquo(object), label, arg = "object")
       3.     └─rlang::eval_bare(expr, quo_get_env(quo))
      
      [ FAIL 3 | WARN 6 | SKIP 68 | PASS 3743 ]
      Error: Test failures
      Execution halted
    ```

# ezEDA

<details>

* Version: 0.1.0
* GitHub: https://github.com/kviswana/ezEDA
* Source code: https://github.com/cran/ezEDA
* Date/Publication: 2020-06-25 09:20:06 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "ezEDA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      p$labels$y not identical to "count".
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 1, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      ── Failure (test_two_category_tally.R:19:3): y axis is labeled 'count' ─────────
      p$labels$y not identical to "count".
      Attributes: < Modes: list, NULL >
      Attributes: < Lengths: 1, 0 >
      Attributes: < names for target but not for current >
      Attributes: < current is not list-like >
      
      [ FAIL 8 | WARN 1 | SKIP 0 | PASS 79 ]
      Error: Test failures
      Execution halted
    ```

# ggseg

<details>

* Version: 1.6.3
* GitHub: https://github.com/LCBC-UiO/ggseg
* Source code: https://github.com/cran/ggseg
* Date/Publication: 2021-05-19 11:00:05 UTC
* Number of recursive dependencies: 125

Run `cloud_details(, "ggseg")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggseg-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_brain
    > ### Title: Brain geom
    > ### Aliases: geom_brain GeomBrain
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    ...
      5.     └─ggplot2:::by_layer(function(l, d) l$compute_statistic(d, layout))
      6.       └─ggplot2:::f(l = layers[[i]], d = data[[i]])
      7.         └─l$compute_statistic(d, layout)
      8.           └─ggplot2:::f(..., self = self)
      9.             └─self$stat$compute_layer(data, self$computed_stat_params, layout)
     10.               └─ggplot2:::f(..., self = self)
     11.                 └─ggproto_parent(Stat, self)$compute_layer(data, params, layout)
     12.                   └─ggplot2:::f(..., self = self)
     13.                     └─ggplot2:::check_required_aesthetics(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7.         ├─ggplot2::ggplot_build(x)
        8.         └─ggplot2:::ggplot_build.ggplot(x)
        9.           └─ggplot2:::by_layer(function(l, d) l$compute_statistic(d, layout))
       10.             └─ggplot2:::f(l = layers[[i]], d = data[[i]])
       11.               └─l$compute_statistic(d, layout)
       12.                 └─ggplot2:::f(..., self = self)
       13.                   └─self$stat$compute_layer(data, self$computed_stat_params, layout)
       14.                     └─ggplot2:::f(..., self = self)
       15.                       └─ggproto_parent(Stat, self)$compute_layer(data, params, layout)
       16.                         └─ggplot2:::f(..., self = self)
       17.                           └─ggplot2:::check_required_aesthetics(...)
      
      [ FAIL 1 | WARN 0 | SKIP 7 | PASS 108 ]
      Error: Test failures
      Execution halted
    ```

# ggtern

<details>

* Version: 3.3.0
* GitHub: NA
* Source code: https://github.com/cran/ggtern
* Date/Publication: 2020-04-11 14:50:05 UTC
* Number of recursive dependencies: 45

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
  object 'try_require' not found
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
# HRM

<details>

* Version: 1.2.1
* GitHub: https://github.com/happma/HRM
* Source code: https://github.com/cran/HRM
* Date/Publication: 2020-02-06 14:50:02 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "HRM")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    
    (R:42550): Gtk-WARNING **: 09:01:27.551: gtk_disable_setlocale() must be called before gtk_init()
    ```

# OncoBayes2

<details>

* Version: 0.7-0
* GitHub: NA
* Source code: https://github.com/cran/OncoBayes2
* Date/Publication: 2021-05-07 19:30:02 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "OncoBayes2")` for more info

</details>

## Newly broken

*   checking whether package ‘OncoBayes2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/OncoBayes2/new/OncoBayes2.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 59.6Mb
      sub-directories of 1Mb or more:
        libs  58.4Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Installation

### Devel

```
* installing *source* package ‘OncoBayes2’ ...
** package ‘OncoBayes2’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: current / base
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
Info: integer division implicitly rounds to integer. Found int division: left_ind + right_ind / 2
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

** libs
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_blrm_exnex_namespace::model_blrm_exnex; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_blrm_exnex.o] Error 1
ERROR: compilation failed for package ‘OncoBayes2’
* removing ‘/tmp/workdir/OncoBayes2/new/OncoBayes2.Rcheck/OncoBayes2’


```
### CRAN

```
* installing *source* package ‘OncoBayes2’ ...
** package ‘OncoBayes2’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: current / base
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
Info: integer division implicitly rounds to integer. Found int division: left_ind + right_ind / 2
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

** libs
...

** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (OncoBayes2)


```
# plotly

<details>

* Version: 4.9.4
* GitHub: https://github.com/ropensci/plotly
* Source code: https://github.com/cran/plotly
* Date/Publication: 2021-06-08 17:40:02 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `expected` is a logical vector (TRUE)
      ── Failure (test-animate-highlight.R:119:7): When key is equivalent to group, produce simple keys ──
      tr$key == tr$name is not TRUE
      
      `actual`:       
      `expected`: TRUE
      ── Failure (test-animate-highlight.R:120:7): When key is equivalent to group, produce simple keys ──
      tr$`_isSimpleKey` is not TRUE
      
      `actual` is NULL
      `expected` is a logical vector (TRUE)
      
      [ FAIL 4 | WARN 36 | SKIP 56 | PASS 1404 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        examples      1.1Mb
        htmlwidgets   3.8Mb
    ```

# ratPASTA

<details>

* Version: 0.2.1
* GitHub: https://github.com/ikodvanj/ratPASTA
* Source code: https://github.com/cran/ratPASTA
* Date/Publication: 2021-06-02 08:00:02 UTC
* Number of recursive dependencies: 127

Run `cloud_details(, "ratPASTA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-latency.R:20:3): Testing output of latency plot ───────────────
      `l` not equal to readRDS("lp.rds").
      Component "LatencyVsCycle": Component "y": Mean relative difference: 0.009602421
      Component "LatencyVsCycle": Component "x": Mean relative difference: 0.1132548
      Component "LatencyVsGroup": Component "y": Mean relative difference: 0.006664163
      Component "LatencyVsGroup": Component "x": Mean relative difference: 0.026796
      ── Failure (test-startleplot.r:26:3): Testing output of startle plot ───────────
      `pld` not equal to `plde`.
      Component "x": Mean relative difference: 0.1892022
      Component "y": Mean relative difference: 7.335276e-05
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 21 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hms’
      All declared Imports should be used.
    ```

# rBiasCorrection

<details>

* Version: 0.3.0
* GitHub: https://github.com/kapsner/rBiasCorrection
* Source code: https://github.com/cran/rBiasCorrection
* Date/Publication: 2021-05-17 17:20:02 UTC
* Number of recursive dependencies: 132

Run `cloud_details(, "rBiasCorrection")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error in (function (formula, data = parent.frame(), start, control = nls.control(),  : 
        singular gradient
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (9)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-plotting.R:236:5): create_exampleplot ─────────────────────────
      `virtual_list` is not NULL
      
      `actual` is a character vector ('/tmp/RtmpcmmfYc/plotdir//exampleplot.png')
      `expected` is NULL
      
      [ FAIL 1 | WARN 31 | SKIP 9 | PASS 50 ]
      Error: Test failures
      Execution halted
    ```

# tricolore

<details>

* Version: 1.2.2
* GitHub: NA
* Source code: https://github.com/cran/tricolore
* Date/Publication: 2020-04-28 13:10:02 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "tricolore")` for more info

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
     1. ├─(function (x, ...) ...
     2. └─ggtern:::print.ggplot(x)
     3.   ├─ggtern::ggplot_build(x)
     4.   └─ggtern:::ggplot_build.ggplot(x)
     5.     └─ggtern:::by_layer(function(l, d) l$compute_geom_1(d))
     6.       └─ggtern:::f(l = layers[[i]], d = data[[i]])
     7.         └─l$compute_geom_1(d)
     8.           └─ggplot2:::f(..., self = self)
     9.             └─ggplot2:::check_required_aesthetics(...)
    Execution halted
    ```

# xpose

<details>

* Version: 0.4.12
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2021-01-12 16:50:02 UTC
* Number of recursive dependencies: 96

Run `cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       2.   └─ggplot2::ggsave(...)
       3.     └─ggplot2:::plot_dim(...)
       4.       └─base::match.arg(units)
      ── Error (test-xpose_save.R:80:3): mutlitple pages are properly saved ──────────
      Error: 'arg' must be of length 1
      Backtrace:
          █
       1. └─xpose::xpose_save(...) test-xpose_save.R:80:2
       2.   └─ggplot2::ggsave(...)
       3.     └─ggplot2:::plot_dim(...)
       4.       └─base::match.arg(units)
      
      [ FAIL 4 | WARN 0 | SKIP 7 | PASS 516 ]
      Error: Test failures
      Execution halted
    ```

