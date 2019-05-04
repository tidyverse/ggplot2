# bayesAB

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/bayesAB
* URL: https://github.com/FrankPortman/bayesAB
* BugReports: https://github.com/FrankPortman/bayesAB/issues
* Date/Publication: 2018-07-14 21:40:03 UTC
* Number of recursive dependencies: 52

Run `revdep_details(,"bayesAB")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    $Probability
    [1] 0.04541
    
    --------------------------------------------
    
    Credible Interval on (A - B) / B for interval length(s) (0.9) : 
    
    $Probability
              5%          95% 
    -0.355943366 -0.006198834 
    
    --------------------------------------------
    
    Posterior Expected Loss for choosing B over A:
    
    $Probability
    [1] 0.2601664
    
    > plot(AB1)
    Error: Either ymin or ymax must be given as an aesthetic.
    Execution halted
    ```

# breathteststan

<details>

* Version: 0.4.7
* Source code: https://github.com/cran/breathteststan
* URL: https://github.com/dmenne/breathteststan
* BugReports: https://github.com/dmenne/breathteststan/issues
* Date/Publication: 2018-11-07 08:50:03 UTC
* Number of recursive dependencies: 123

Run `revdep_details(,"breathteststan")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +   inner_join(d$record, by = "patient_id") %>%
    +   select(patient_id, m_in = m.y, m_out = m.x,
    +          beta_in = beta.y, beta_out = beta.x,
    +          k_in = k.y, k_out = k.x)
    # A tibble: 3 x 7
      patient_id  m_in m_out beta_in beta_out    k_in   k_out
      <chr>      <dbl> <dbl>   <dbl>    <dbl>   <dbl>   <dbl>
    1 rec_01        34  36.0    2.19     2.00 0.0140  0.0124 
    2 rec_02        42  44.5    2.30     2.12 0.0108  0.00976
    3 rec_03        32  36.8    2.23     2.14 0.00795 0.00687
    > # For a detailed analysis of the fit, use the shinystan library
    > # The following plots are somewhat degenerate because
    > # of the few iterations in stan_fit
    > suppressPackageStartupMessages(library(rstan))
    > stan_plot(fit$stan_fit, pars = c("beta[1]","beta[2]","beta[3]"))
    ci_level: 0.8 (80% intervals)
    outer_level: 0.95 (95% intervals)
    Error in axis.ticks.length.x.bottom %||% axis.ticks.length.x : 
      object 'axis.ticks.length.x.bottom' not found
    Calls: <Anonymous> ... with -> with.default -> eval -> eval -> %||% -> %||%
    Execution halted
    ```

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# CSTools

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/CSTools
* Date/Publication: 2019-04-24 14:20:02 UTC
* Number of recursive dependencies: 75

Run `revdep_details(,"CSTools")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(CSTools)
      Loading required package: maps
      > 
      > test_check("CSTools")
      ── 1. Failure: some checks (@test-PlotForecastPDF.R#13)  ───────────────────────
      length(...) not equal to 61.
      1/1 mismatches
      [1] 67 - 61 == 6
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 40 SKIPPED: 0 WARNINGS: 45 FAILED: 1
      1. Failure: some checks (@test-PlotForecastPDF.R#13) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# esmisc

<details>

* Version: 0.0.3
* Source code: https://github.com/cran/esmisc
* URL: https://github.com/EDiLD/esmisc
* BugReports: https://github.com/EDiLD/esmisc/issues
* Date/Publication: 2017-01-11 10:34:49
* Number of recursive dependencies: 41

Run `revdep_details(,"esmisc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘esmisc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_edi
    > ### Title: Custom ggplot2 theme
    > ### Aliases: theme_edi
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > p <- ggplot(mtcars) + 
    +  geom_point(aes(x = wt, y = mpg, 
    + colour=factor(gear))) + facet_wrap(~am)
    > p
    > p + theme_edi()
    Error in axis.ticks.length.x.bottom %||% axis.ticks.length.x : 
      object 'axis.ticks.length.x.bottom' not found
    Calls: <Anonymous> ... with -> with.default -> eval -> eval -> %||% -> %||%
    Execution halted
    ```

# fergm

<details>

* Version: 1.1.4
* Source code: https://github.com/cran/fergm
* URL: http://github.com/benjamin-w-campbell/fergm
* Date/Publication: 2018-10-17 22:20:11 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"fergm")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: coef_posterior_density
    > ### Title: Plots the posterior density for FERGM model terms.
    > ### Aliases: coef_posterior_density
    > ### Keywords: FERGM interpret summary
    > 
    > ### ** Examples
    > 
    > # load example data
    > data("ergm.fit")
    > data("fergm.fit")
    > data("mesa")
    > 
    > # rstan functions
    > # Histogram of the posterior
    > rstan::stan_hist(fergm.fit$stan.fit, par = "beta")
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    Error in axis.ticks.length.x.bottom %||% axis.ticks.length.x : 
      object 'axis.ticks.length.x.bottom' not found
    Calls: <Anonymous> ... with -> with.default -> eval -> eval -> %||% -> %||%
    Execution halted
    ```

# fingertipscharts

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/fingertipscharts
* BugReports: https://github.com/PublicHealthEngland/fingertipscharts/issues
* Date/Publication: 2019-04-08 21:23:17 UTC
* Number of recursive dependencies: 129

Run `revdep_details(,"fingertipscharts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > df <- fingertips_data(90316) %>%
    +         filter(is.na(CategoryType) &
    +                        Timeperiod == "2016/17" &
    +                        (AreaName %in% top_names |
    +                                 ParentName == region) &
    +                        Sex == "Persons") %>%
    +         mutate(ComparedtoEnglandvalueorpercentiles =
    +                        factor(ComparedtoEnglandvalueorpercentiles,
    +                               levels = ordered_levels))
    > p <- compare_areas(df, AreaName, Value,
    +                    fill = ComparedtoEnglandvalueorpercentiles,
    +                    lowerci = LowerCI95.0limit,
    +                    upperci = UpperCI95.0limit,
    +                    order = "desc",
    +                    top_areas = top_names,
    +                    title = unique(df$IndicatorName))
    > p
    Error in axis.ticks.length.x.bottom %||% axis.ticks.length.x : 
      object 'axis.ticks.length.x.bottom' not found
    Calls: <Anonymous> ... with -> with.default -> eval -> eval -> %||% -> %||%
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 9 SKIPPED: 20 WARNINGS: 12 FAILED: 10
      1.  Error: box plots draws correctly (@test-box-plots.R#20) 
      2.  Error: desc compare areas draws correctly (@test-compare-areas.R#55) 
      3.  Error: no top desc compare areas draws correctly (@test-compare-areas.R#61) 
      4.  Error: asc compare areas draws correctly (@test-compare-areas.R#67) 
      5.  Error: no top asc compare areas draws correctly (@test-compare-areas.R#73) 
      6.  Error: desc compare areas no fill draws correctly (@test-compare-areas.R#79) 
      7.  Error: trends example draws correctly (@test-examples.R#52) 
      8.  Error: box plot example draws correctly (@test-examples.R#103) 
      9.  Error: trends with fill draws correctly (@test-trends.R#41) 
      10. Error: trends without fill draws correctly (@test-trends.R#47) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘mapproj’
      All declared Imports should be used.
    ```

# gastempt

<details>

* Version: 0.4.4
* Source code: https://github.com/cran/gastempt
* URL: http://github.com/dmenne/gastempt
* BugReports: http://github.com/dmenne/gastempt/issues
* Date/Publication: 2019-03-06 16:32:41 UTC
* Number of recursive dependencies: 82

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
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        libs   7.5Mb
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
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1b.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1c.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1d.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_2b.stan
Wrote C++ file "stan_files/linexp_gastro_1c.cc"
Wrote C++ file "stan_files/linexp_gastro_1b.cc"
Wrote C++ file "stan_files/linexp_gastro_1d.cc"
Wrote C++ file "stan_files/linexp_gastro_2b.cc"
Error in readRDS("/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpNeTeBk/file30e631b2ee0") : 
  error reading from connection
Calls: .Last -> readRDS
Execution halted
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_2c.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/powexp_gastro_1b.stan
make: *** [stan_files/linexp_gastro_1d.cc] Error 1
make: *** Waiting for unfinished jobs....
Wrote C++ file "stan_files/linexp_gastro_2c.cc"
Wrote C++ file "stan_files/powexp_gastro_1b.cc"
rm stan_files/linexp_gastro_2c.cc stan_files/linexp_gastro_1d.cc stan_files/linexp_gastro_1b.cc stan_files/linexp_gastro_2b.cc stan_files/linexp_gastro_1c.cc stan_files/powexp_gastro_1b.cc
ERROR: compilation failed for package ‘gastempt’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/gastempt/new/gastempt.Rcheck/gastempt’

```
### CRAN

```
* installing *source* package ‘gastempt’ ...
** package ‘gastempt’ successfully unpacked and MD5 sums checked
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1b.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1c.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_1d.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_2b.stan
Wrote C++ file "stan_files/linexp_gastro_1d.cc"
Wrote C++ file "stan_files/linexp_gastro_1c.cc"
Wrote C++ file "stan_files/linexp_gastro_1b.cc"
Wrote C++ file "stan_files/linexp_gastro_2b.cc"
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linexp_gastro_2c.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/powexp_gastro_1b.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/powexp_gastro_2c.stan


/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c init.cpp -o init.o


/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c stan_files/linexp_gastro_1b.cc -o stan_files/linexp_gastro_1b.o
Wrote C++ file "stan_files/powexp_gastro_1b.cc"
Wrote C++ file "stan_files/linexp_gastro_2c.cc"
Wrote C++ file "stan_files/powexp_gastro_2c.cc"


/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c stan_files/linexp_gastro_1c.cc -o stan_files/linexp_gastro_1c.o


/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c stan_files/linexp_gastro_1d.cc -o stan_files/linexp_gastro_1d.o


/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c stan_files/linexp_gastro_2b.cc -o stan_files/linexp_gastro_2b.o


In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:32:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:32:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:32:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:32:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_1b.cc:3:
In file included from stan_files/linexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_1c.cc:3:
In file included from stan_files/linexp_gastro_1c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_1d.cc:3:
In file included from stan_files/linexp_gastro_1d.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_2b.cc:3:
In file included from stan_files/linexp_gastro_2b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
16 warnings generated.
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c stan_files/linexp_gastro_2c.cc -o stan_files/linexp_gastro_2c.o


16 warnings generated.
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c stan_files/powexp_gastro_1b.cc -o stan_files/powexp_gastro_1b.o


16 warnings generated.
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include" -I/usr/local/include   -fPIC  -Wall -g -O2 -c stan_files/powexp_gastro_2c.cc -o stan_files/powexp_gastro_2c.o
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:32:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:32:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:32:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
16 warnings generated.
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/linexp_gastro_2c.cc:3:
In file included from stan_files/linexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/powexp_gastro_1b.cc:3:
In file included from stan_files/powexp_gastro_1b.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/powexp_gastro_2c.cc:3:
In file included from stan_files/powexp_gastro_2c.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/gastempt/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
16 warnings generated.
16 warnings generated.
16 warnings generated.
/usr/local/clang6/bin/clang++ -std=gnu++14 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o gastempt.so stan_files/linexp_gastro_1b.o stan_files/linexp_gastro_1c.o stan_files/linexp_gastro_1d.o stan_files/linexp_gastro_2b.o stan_files/linexp_gastro_2c.o stan_files/powexp_gastro_1b.o stan_files/powexp_gastro_2c.o init.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: text-based stub file /System/Library/Frameworks//CoreFoundation.framework/CoreFoundation.tbd and library file /System/Library/Frameworks//CoreFoundation.framework/CoreFoundation are out of sync. Falling back to library file for linking.
rm stan_files/linexp_gastro_2c.cc stan_files/linexp_gastro_1d.cc stan_files/powexp_gastro_2c.cc stan_files/linexp_gastro_1b.cc stan_files/linexp_gastro_2b.cc stan_files/linexp_gastro_1c.cc stan_files/powexp_gastro_1b.cc
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/gastempt/old/gastempt.Rcheck/gastempt/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
* DONE (gastempt)

```
# ggalt

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/ggalt
* URL: https://github.com/hrbrmstr/ggalt
* BugReports: https://github.com/hrbrmstr/ggalt/issues
* Date/Publication: 2017-02-15 18:16:00
* Number of recursive dependencies: 78

Run `revdep_details(,"ggalt")` for more info

</details>

## Newly broken

*   checking whether package ‘ggalt’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'pathGrob(munched$x, munched$y, ': unused argument (pathId = munched$group) 
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggalt/new/ggalt.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plotly’
      All declared Imports should be used.
    ```

# ggforce

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/ggforce
* URL: https://ggforce.data-imaginist.com
* BugReports: https://github.com/thomasp85/ggforce/issues
* Date/Publication: 2019-04-23 13:20:03 UTC
* Number of recursive dependencies: 66

Run `revdep_details(,"ggforce")` for more info

</details>

## Newly broken

*   checking whether package ‘ggforce’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'pathGrob(munched$x, munched$y, ': unused argument (pathId = munched$group) 
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggforce/new/ggforce.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

# ggpol

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/ggpol
* URL: https://github.com/erocoar/ggpol
* Date/Publication: 2019-03-14 13:40:02 UTC
* Number of recursive dependencies: 50

Run `revdep_details(,"ggpol")` for more info

</details>

## Newly broken

*   checking whether package ‘ggpol’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'pathGrob(munched$x, munched$y, ': unused argument (pathId = munched$group) 
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggpol/new/ggpol.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘grDevices’
      All declared Imports should be used.
    ```

# ggpolypath

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/ggpolypath
* URL: https://github.com/mdsumner/ggpolypath, http://rpubs.com/kohske/3522
* BugReports: https://github.com/mdsumner/ggpolypath/issues
* Date/Publication: 2016-08-10 02:53:58
* Number of recursive dependencies: 45

Run `revdep_details(,"ggpolypath")` for more info

</details>

## Newly broken

*   checking whether package ‘ggpolypath’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'pathGrob(munched$x, munched$y, ': unused argument (pathId = munched$group) 
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggpolypath/new/ggpolypath.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

# ggsolvencyii

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/ggsolvencyii
* URL: https://github.com/vanzanden/ggsolvencyii
* BugReports: https://github.com/vanzanden/ggsolvencyii/issues
* Date/Publication: 2019-01-04 12:10:03 UTC
* Number of recursive dependencies: 66

Run `revdep_details(,"ggsolvencyii")` for more info

</details>

## Newly broken

*   checking whether package ‘ggsolvencyii’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'pathGrob(munched$x, munched$y, ': unused argument (pathId = munched$group) 
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggsolvencyii/new/ggsolvencyii.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

# ggspatial

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/ggspatial
* URL: https://github.com/paleolimbot/ggspatial
* BugReports: https://github.com/paleolimbot/ggspatial/issues
* Date/Publication: 2018-12-14 21:10:04 UTC
* Number of recursive dependencies: 80

Run `revdep_details(,"ggspatial")` for more info

</details>

## Newly broken

*   checking whether package ‘ggspatial’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'pathGrob(munched$x, munched$y, ': unused argument (pathId = munched$group) 
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggspatial/new/ggspatial.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘reshape2’ ‘rosm’
      All declared Imports should be used.
    ```

# ggstance

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/ggstance
* Date/Publication: 2018-07-20 10:10:03 UTC
* Number of recursive dependencies: 105

Run `revdep_details(,"ggstance")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library("testthat")
      > library("ggstance")
      > 
      > test_check("ggstance")
      ── 1. Failure: position_jitterdodge() flips (@test-positions.R#20)  ────────────
      `h_data` not identical to `v_data`.
      Component 1: Component 11: Mean relative difference: 0.006782654
      Component 1: Component 13: Mean relative difference: 0.006916903
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 29 SKIPPED: 28 WARNINGS: 0 FAILED: 1
      1. Failure: position_jitterdodge() flips (@test-positions.R#20) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggstatsplot

<details>

* Version: 0.0.10
* Source code: https://github.com/cran/ggstatsplot
* URL: https://indrajeetpatil.github.io/ggstatsplot/, https://github.com/IndrajeetPatil/ggstatsplot
* BugReports: https://github.com/IndrajeetPatil/ggstatsplot/issues
* Date/Publication: 2019-03-17 17:50:02 UTC
* Number of recursive dependencies: 217

Run `revdep_details(,"ggstatsplot")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      as.character(df2$value[[35]][1]) not identical to "5pt".
      1/1 mismatches
      x[1]: "NULL"
      y[1]: "5pt"
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 21 SKIPPED: 184 WARNINGS: 0 FAILED: 5
      1. Failure: `theme_pie()` works (@test_theme_ggstatsplot.R#65) 
      2. Failure: `theme_pie()` works (@test_theme_ggstatsplot.R#66) 
      3. Failure: `theme_pie()` works (@test_theme_ggstatsplot.R#67) 
      4. Failure: `theme_pie()` works (@test_theme_ggstatsplot.R#68) 
      5. Failure: `theme_pie()` works (@test_theme_ggstatsplot.R#69) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# ggtern

<details>

* Version: 3.1.0
* Source code: https://github.com/cran/ggtern
* URL: http://www.ggtern.com
* Date/Publication: 2018-12-19 11:20:03 UTC
* Number of recursive dependencies: 44

Run `revdep_details(,"ggtern")` for more info

</details>

## Newly broken

*   checking whether package ‘ggtern’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Note: possible error in 'pathGrob(munched$x, munched$y, ': unused argument (pathId = munched$group) 
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ggtern/new/ggtern.Rcheck/00install.out’ for details.
    Information on the location(s) of code generating the ‘Note’s can be
    obtained by re-running with environment variable R_KEEP_PKG_SOURCE set
    to ‘yes’.
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘chemometrics’
    ```

# heatmaply

<details>

* Version: 0.15.2
* Source code: https://github.com/cran/heatmaply
* URL: https://cran.r-project.org/package=heatmaply, https://github.com/talgalili/heatmaply/, https://www.r-statistics.com/tag/heatmaply/
* BugReports: https://github.com/talgalili/heatmaply/issues
* Date/Publication: 2018-07-06 13:00:03 UTC
* Number of recursive dependencies: 107

Run `revdep_details(,"heatmaply")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   4.6Mb
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘d3heatmap’
    ```

# HistDAWass

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/HistDAWass
* Date/Publication: 2018-03-20 16:23:27 UTC
* Number of recursive dependencies: 76

Run `revdep_details(,"HistDAWass")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘HistDAWass-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot-distributionH
    > ### Title: plot for a distributionH object
    > ### Aliases: plot-distributionH plot,distributionH-method
    > 
    > ### ** Examples
    > 
    > ##---- initialize a distributionH
    >  mydist<-distributionH(x=c(7,8,10,15),p=c(0, 0.2, 0.7, 1))
    >  # show the histogram
    >  plot(mydist) #plots mydist
    >  plot(mydist, type="HISTO", col="red", border="blue") #plots mydist
    >  plot(mydist, type="DENS", col="red", border="blue") #plots a density approximation for mydist
    >  plot(mydist, type="HBOXPLOT") #plots a horizontal boxplot for mydist
    Error: Elements must equal the number of rows or 1
    Execution halted
    ```

# incR

<details>

* Version: 1.1.0
* Source code: https://github.com/cran/incR
* Date/Publication: 2018-03-21 15:25:24 UTC
* Number of recursive dependencies: 56

Run `revdep_details(,"incR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: incRplot
    > ### Title: Quick visualisation of incubation temperatures, on-bouts and
    > ###   off-bouts
    > ### Aliases: incRplot
    > 
    > ### ** Examples
    > 
    > # loading example data
    > data(incR_procdata)
    > my_plot <- incRplot(data = incR_procdata[complete.cases(incR_procdata$temperature),], 
    +                     time.var = "dec_time", 
    +                     day.var = "date", 
    +                     inc.temperature.var = "temperature", 
    +                     env.temperature.var = "env_temp",
    +                     vector.incubation = "incR_score")
    >                     
    > # see your plot
    > my_plot
    Error in scale_apply(layer_data, x_vars, "train", SCALE_X, x_scales) : 
    Calls: <Anonymous> ... <Anonymous> -> f -> <Anonymous> -> f -> scale_apply
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘rgeos’
      All declared Imports should be used.
    ```

# interflex

<details>

* Version: 1.0.4
* Source code: https://github.com/cran/interflex
* Date/Publication: 2018-04-20 09:50:27 UTC
* Number of recursive dependencies: 46

Run `revdep_details(,"interflex")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘interflex-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: inter.binning
    > ### Title: The Binning Estimator
    > ### Aliases: inter.binning
    > ### Keywords: graphics
    > 
    > ### ** Examples
    > 
    > library(interflex)
    > data(interflex)
    > inter.binning(Y = "Y", D = "D", X = "X", Z = "Z1",
    +                   data = s1, nbins = 3, vartype = "homoscedastic",
    +                   Ylabel = "Y", Dlabel = "Tr", Xlabel="X")
    Error: Elements must equal the number of rows or 1
    Execution halted
    ```

# jcolors

<details>

* Version: 0.0.3
* Source code: https://github.com/cran/jcolors
* URL: https://jaredhuling.github.io/jcolors/
* BugReports: https://github.com/jaredhuling/jcolors/issues
* Date/Publication: 2018-08-09 10:20:10 UTC
* Number of recursive dependencies: 46

Run `revdep_details(,"jcolors")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘jcolors-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_dark_bg
    > ### Title: minimal theme for dark backgrounds
    > ### Aliases: theme_dark_bg theme_light_bg
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > 
    > p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
    +          colour = factor(gear))) + facet_grid(vs~am)
    > p + theme_dark_bg()
    Error in axis.ticks.length.x.bottom %||% axis.ticks.length.x : 
      object 'axis.ticks.length.x.bottom' not found
    Calls: <Anonymous> ... with -> with.default -> eval -> eval -> %||% -> %||%
    Execution halted
    ```

# lime

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/lime
* URL: https://github.com/thomasp85/lime
* BugReports: https://github.com/thomasp85/lime/issues
* Date/Publication: 2018-11-21 12:50:02 UTC
* Number of recursive dependencies: 126

Run `revdep_details(,"lime")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1: h2o.importFile(path) at testthat/test-h2o.R:12
      2: h2o.importFolder(path, pattern = "", destination_frame = destination_frame, parse, 
             header, sep, col.names, col.types, na.strings = na.strings, decrypt_tool = decrypt_tool, 
             skipped_columns = skipped_columns)
      3: .h2o.__remoteSend(.h2o.__IMPORT, path = path, pattern = pattern)
      4: .h2o.doSafeREST(h2oRestApiVersion = h2oRestApiVersion, urlSuffix = page, parms = .params, 
             method = method)
      5: stop(msg)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 28 SKIPPED: 0 WARNINGS: 1 FAILED: 1
      1. Error: H2OBinomialClassification: lime explanation only produces one entry per case and feature (@test-h2o.R#12) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# mlr

<details>

* Version: 2.14.0
* Source code: https://github.com/cran/mlr
* URL: https://github.com/mlr-org/mlr
* BugReports: https://github.com/mlr-org/mlr/issues
* Date/Publication: 2019-04-25 22:00:04 UTC
* Number of recursive dependencies: 359

Run `revdep_details(,"mlr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Task: Sonar-example, Learner: classif.rpart
    Resampling: cross-validation
    Measures:             acc       ber       
    [Resample] iter 1:    0.6153846 0.4109375 
    [Resample] iter 2:    0.7115385 0.2892870 
    
    
    Aggregated Result: acc.test.mean=0.6634615,ber.test.mean=0.3501123
    
    
    > rmat = convertBMRToRankMatrix(bmr)
    > print(rmat)
                  Sonar-example iris-example
    classif.lda               1            1
    classif.rpart             2            2
    > plotBMRSummary(bmr)
    > plotBMRBoxplots(bmr, ber, style = "violin")
    Warning in max(data$density) :
      no non-missing arguments to max; returning -Inf
    Error: Elements must equal the number of rows or 1
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   2.3Mb
        help   1.4Mb
    ```

# MTLR

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/MTLR
* URL: https://github.com/haiderstats/MTLR
* BugReports: https://github.com/haiderstats/MTLR/issues
* Date/Publication: 2019-03-09 17:22:41 UTC
* Number of recursive dependencies: 56

Run `revdep_details(,"MTLR")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component "theme": Component 20: target is NULL, current is margin
      Component "theme": Component 21: target is NULL, current is unit
      Component "theme": Component 22: Modes: list, NULL
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 84 SKIPPED: 2 WARNINGS: 35 FAILED: 5
      1. Failure: print and plot functions are consistent for basic survival dataset (@test-method.R#15) 
      2. Failure: plotcurves function is consistent (@test-plotcurves.R#13) 
      3. Failure: plotcurves function is consistent (@test-plotcurves.R#14) 
      4. Failure: plotcurves function is consistent (@test-plotcurves.R#15) 
      5. Failure: plotcurves function is consistent (@test-plotcurves.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# OUTRIDER

<details>

* Version: 1.0.4
* Source code: https://github.com/cran/OUTRIDER
* URL: https://github.com/gagneurlab/OUTRIDER
* Date/Publication: 2019-03-19
* Number of recursive dependencies: 151

Run `revdep_details(,"OUTRIDER")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: aberrant
    > ### Title: Number of aberrant events
    > ### Aliases: aberrant
    > 
    > ### ** Examples
    > 
    > ods <- makeExampleOutriderDataSet()
    > ods <- OUTRIDER(ods, implementation='pca')
    Fri May  3 17:36:07 2019: SizeFactor estimation ...
    Fri May  3 17:36:07 2019: Controlling for confounders ...
    Using provided q with: 10
    Fri May  3 17:36:07 2019: Using the pca implementation for controlling.
    Fri May  3 17:36:07 2019: Used the pca implementation for controlling.
    Fri May  3 17:36:07 2019: Fitting the data ...
    Warning in socketConnection(host, port, TRUE, TRUE, "a+b", timeout = timeout) :
      port 11896 cannot be opened
    Error in socketConnection(host, port, TRUE, TRUE, "a+b", timeout = timeout) : 
      cannot open the connection
    Calls: OUTRIDER ... .local -> .bpfork -> .bpforkConnect -> socketConnection
    Execution halted
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1] "Fri May  3 17:36:41 2019: Iteration: 15 loss: 3.78202399468212"
      Time difference of 4.905578 secs
      [1] "15 Final nb-AE loss: 3.78202399468212"
      [1] "Evaluation loss: 0.291702213587694"
      [1] "Initial PCA loss: 6.46616282459584"
      [1] "Fri May  3 17:36:48 2019: Iteration: 1 loss: 4.81536028479294"
      [1] "Fri May  3 17:36:48 2019: Iteration: 2 loss: 4.78999714768799"
      Time difference of 1.036859 secs
      [1] "2 Final nb-AE loss: 4.78999714768799"
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 82 SKIPPED: 0 WARNINGS: 24 FAILED: 1
      1. Error: Expression filtering (@test_filtering.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Authors@R field gives more than one person with maintainer role:
      Felix Brechtmann <brechtma@in.tum.de> [aut, cre]
      Christian Mertes <mertes@in.tum.de> [aut, cre]
    ```

# PathoStat

<details>

* Version: 1.8.4
* Source code: https://github.com/cran/PathoStat
* URL: https://github.com/mani2012/PathoStat
* BugReports: https://github.com/mani2012/PathoStat/issues
* Date/Publication: 2018-12-02
* Number of recursive dependencies: 177

Run `revdep_details(,"PathoStat")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PathoStat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: findTaxonomy
    > ### Title: Find the taxonomy for unlimited tids
    > ### Aliases: findTaxonomy
    > 
    > ### ** Examples
    > 
    > example_data_dir <- system.file("example/data", package = "PathoStat")
    > pathoreport_file_suffix <- "-sam-report.tsv"
    > datlist <- readPathoscopeData(example_data_dir, pathoreport_file_suffix,
    + input.files.name.vec = as.character(1:6))
    > dat <- datlist$data
    > ids <- rownames(dat)
    > tids <- unlist(lapply(ids, FUN = grepTid))
    > taxonLevels <- findTaxonomy(tids[1:5])
    Error: HTTP failure: 429
    {"error":"API rate limit exceeded","api-key":"208.103.64.29","count":"4","limit":"3"}
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ComplexHeatmap’ ‘RColorBrewer’
      All declared Imports should be used.
    ```

# PepsNMR

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/PepsNMR
* URL: https://github.com/ManonMartin/PepsNMR
* BugReports: https://github.com/ManonMartin/PepsNMR/issues
* Date/Publication: 2019-03-08
* Number of recursive dependencies: 53

Run `revdep_details(,"PepsNMR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PepsNMR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DrawPCA
    > ### Title: Draw the PCA scores or loadings of the signals
    > ### Aliases: DrawPCA
    > ### Keywords: hplot
    > 
    > ### ** Examples
    > 
    > require(PepsNMRData)
    Loading required package: PepsNMRData
    > # Draw loadings
    > DrawPCA(FinalSpectra_HS, main = "PCA loadings plot", 
    +       Class = NULL, axes =c(1,3, 5), type ="loadings", loadingstype="l", 
    +       num.stacked=4, xlab="ppm", createWindow = TRUE)
    dev.new(): using pdf(file="Rplots2.pdf")
    Error: Aesthetics must be either length 1 or the same as the data (9): label
    Execution halted
    ```

# SCORPIUS

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/SCORPIUS
* URL: http://github.com/rcannood/SCORPIUS
* BugReports: https://github.com/rcannood/SCORPIUS/issues
* Date/Publication: 2018-06-29 14:55:11 UTC
* Number of recursive dependencies: 71

Run `revdep_details(,"SCORPIUS")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      TSP requires a symmetric matrix
      1: infer_trajectory(space) at testthat/test-trajectory_inference.R:9
      2: infer_initial_trajectory(space, k)
      3: TSP::insert_dummy(TSP::TSP(cluster_distances))
      4: TSP::TSP(cluster_distances)
      5: as.TSP(x)
      6: as.TSP.matrix(x)
      7: stop("TSP requires a symmetric matrix")
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 189 SKIPPED: 0 WARNINGS: 0 FAILED: 1
      1. Error: With generated data (@test-trajectory_inference.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# shiny

<details>

* Version: 1.3.2
* Source code: https://github.com/cran/shiny
* URL: http://shiny.rstudio.com
* BugReports: https://github.com/rstudio/shiny/issues
* Date/Publication: 2019-04-22 16:00:09 UTC
* Number of recursive dependencies: 55

Run `revdep_details(,"shiny")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
         3: test_dir
         2: test_package_dir
         1: test_check
        78: <Anonymous>
      ── 1. Failure: debounce/throttle work properly (with priming) (@test-reactivity.
      isolate(tr()) not identical to 10.
      1/1 mismatches
      [1] 9 - 10 == -1
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 760 SKIPPED: 0 WARNINGS: 0 FAILED: 1
      1. Failure: debounce/throttle work properly (with priming) (@test-reactivity.r#1125) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.9Mb
      sub-directories of 1Mb or more:
        R     2.0Mb
        www   7.9Mb
    ```

# ssMousetrack

<details>

* Version: 1.1.5
* Source code: https://github.com/cran/ssMousetrack
* BugReports: https://github.com/antcalcagni/ssMousetrack/issues
* Date/Publication: 2019-01-16 17:00:03 UTC
* Number of recursive dependencies: 56

Run `revdep_details(,"ssMousetrack")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        libs   5.3Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Newly fixed

*   checking whether package ‘ssMousetrack’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/ssMousetrack/old/ssMousetrack.Rcheck/00install.out’ for details.
    ```

# stats19

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/stats19
* URL: https://itsleeds.github.io/stats19/
* BugReports: https://github.com/ropensci/stats19/issues
* Date/Publication: 2019-04-03 08:40:03 UTC
* Number of recursive dependencies: 114

Run `revdep_details(,"stats19")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Content type 'application/x-zip-compressed' length 1236846 bytes (1.2 MB)
      ==================================================
      downloaded 1.2 MB
      
      trying URL 'http://data.dft.gov.uk.s3.amazonaws.com/road-accidents-safety-data/dftRoadSafetyData_Casualties_2017.zip'
      Content type 'application/x-zip-compressed' length 1169376 bytes (1.1 MB)
      ==================================================
      downloaded 1.1 MB
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 44 SKIPPED: 4 WARNINGS: 7 FAILED: 1
      1. Error: get_stats19 works (@test-get.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# trialr

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/trialr
* URL: https://github.com/brockk/trialr
* BugReports: https://github.com/brockk/trialr/issues
* Date/Publication: 2019-04-21 21:00:03 UTC
* Number of recursive dependencies: 79

Run `revdep_details(,"trialr")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  9.5Mb
      sub-directories of 1Mb or more:
        doc    1.6Mb
        libs   7.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘magrittr’ ‘tidyr’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Newly fixed

*   checking whether package ‘trialr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/trialr/old/trialr.Rcheck/00install.out’ for details.
    ```

# vdiffr

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/vdiffr
* URL: https://github.com/r-lib/vdiffr
* BugReports: https://github.com/r-lib/vdiffr/issues
* Date/Publication: 2019-01-02 19:30:02 UTC
* Number of recursive dependencies: 87

Run `revdep_details(,"vdiffr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
    ...
      : 56' style='font-size: 13.20px; font-family: Liberation Sans;' textLength='38.8
      : 3px' lengthAdjust='spacingAndGlyphs'>myplot</text></g>                        
        </svg>                                                                        
      
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library("vdiffr")
      > test_check("vdiffr")
      ── 1. Failure: Doppelgangers pass (@test-expectations.R#25)  ───────────────────
      `ggplot_result` inherits from `expectation_failure/expectation/error/condition/vdiffr_mismatch` not `expectation_success`.
      
      ── 2. Failure: ggtitle is set correctly (@test-ggplot.R#6)  ────────────────────
      purrr::every(ggplot_results, inherits, "expectation_success") isn't true.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 29 SKIPPED: 0 WARNINGS: 0 FAILED: 2
      1. Failure: Doppelgangers pass (@test-expectations.R#25) 
      2. Failure: ggtitle is set correctly (@test-ggplot.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘freetypeharfbuzz’
      All declared Imports should be used.
    ```

# vidger

<details>

* Version: 1.2.1
* Source code: https://github.com/cran/vidger
* URL: https://github.com/btmonier/vidger, https://bioconductor.org/packages/release/bioc/html/vidger.html
* BugReports: https://github.com/btmonier/vidger/issues
* Date/Publication: 2019-01-04
* Number of recursive dependencies: 116

Run `revdep_details(,"vidger")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vidger-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vsMAMatrix
    > ### Title: MA plot matrix from log_{2} fold changes and -log_{10}(p-values)
    > ### Aliases: vsMAMatrix
    > 
    > ### ** Examples
    > 
    > # Cuffdiff example
    > data("df.cuff")
    > vsMAMatrix(
    +  	data = df.cuff, d.factor = NULL, type = "cuffdiff",
    +  	padj = 0.05, y.lim = NULL, lfc = 1, title = TRUE,
    +  	grid = TRUE, counts = TRUE, data.return = FALSE
    + )
    Error: Aesthetics must be either length 1 or the same as the data (36): label
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.5Mb
      sub-directories of 1Mb or more:
        data   4.0Mb
        doc    6.1Mb
    ```

# xpose

<details>

* Version: 0.4.4
* Source code: https://github.com/cran/xpose
* URL: https://github.com/UUPharmacometrics/xpose
* BugReports: https://github.com/UUPharmacometrics/xpose/issues
* Date/Publication: 2019-03-21 17:10:03 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"xpose")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xpose-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: amt_vs_idv
    > ### Title: Compartment kinetics
    > ### Aliases: amt_vs_idv
    > 
    > ### ** Examples
    > 
    > amt_vs_idv(xpdb_ex_pk)
    Using data from $prob no.1
    Tidying data by ID, SEX, MED1, MED2, AMT ... and 20 more variables
    Error in axis.ticks.length.x.bottom %||% axis.ticks.length.x : 
      object 'axis.ticks.length.x.bottom' not found
    Calls: <Anonymous> ... with -> with.default -> eval -> eval -> %||% -> %||%
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      31: with.default(theme, axis.ticks.length.x.bottom %||% axis.ticks.length.x %||% axis.ticks.length)
      32: eval(substitute(expr), data, enclos = parent.frame())
      33: eval(substitute(expr), data, enclos = parent.frame())
      34: axis.ticks.length.x.bottom %||% axis.ticks.length.x %||% axis.ticks.length
      35: axis.ticks.length.x.bottom %||% axis.ticks.length.x
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 515 SKIPPED: 6 WARNINGS: 0 FAILED: 4
      1. Error: warnings and errors are properly returned (@test-print_xpose_plots.R#19) 
      2. Error: common graphical device work properly (@test-xpose_save.R#45) 
      3. Error: template filenames and auto file extension work properly (@test-xpose_save.R#67) 
      4. Error: mutlitple pages are properly saved (@test-xpose_save.R#80) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

