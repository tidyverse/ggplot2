# bayesAB

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/bayesAB
* URL: https://github.com/FrankPortman/bayesAB
* BugReports: https://github.com/FrankPortman/bayesAB/issues
* Date/Publication: 2018-07-14 21:40:03 UTC
* Number of recursive dependencies: 55

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

# bayesdfa

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/bayesdfa
* URL: https://github.com/fate-ewi/bayesdfa
* BugReports: https://github.com/fate-ewi/bayesdfa/issues
* Date/Publication: 2019-05-22 13:40:05 UTC
* Number of recursive dependencies: 74

Run `revdep_details(,"bayesdfa")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        libs   4.9Mb
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

# breathteststan

<details>

* Version: 0.4.7
* Source code: https://github.com/cran/breathteststan
* URL: https://github.com/dmenne/breathteststan
* BugReports: https://github.com/dmenne/breathteststan/issues
* Date/Publication: 2018-11-07 08:50:03 UTC
* Number of recursive dependencies: 125

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
    1 rec_01        34  35.9    2.19     2.00 0.0140  0.0125 
    2 rec_02        42  44.7    2.30     2.09 0.0108  0.00964
    3 rec_03        32  38.7    2.23     2.06 0.00795 0.00639
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

*   checking line endings in C/C++/Fortran sources/headers ... NOTE
    ```
    Found the following sources/headers with CR or CRLF line endings:
      inst/include/meta_header.hpp
    Some Unix compilers require LF line endings.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# CSTools

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/CSTools
* Date/Publication: 2019-04-24 14:20:02 UTC
* Number of recursive dependencies: 77

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

# DeLorean

<details>

* Version: 1.5.0
* Source code: https://github.com/cran/DeLorean
* Date/Publication: 2018-10-17 22:30:16 UTC
* Number of recursive dependencies: 113

Run `revdep_details(,"DeLorean")` for more info

</details>

## Newly broken

*   checking whether package ‘DeLorean’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DeLorean/new/DeLorean.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        libs   5.1Mb
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

## Installation

### Devel

```
* installing *source* package ‘DeLorean’ ...
** package ‘DeLorean’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/exact.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/exactsizes.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/lowrank.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/lowranksizes.stan
Wrote C++ file "stan_files/exact.cc"
Wrote C++ file "stan_files/lowrank.cc"
Wrote C++ file "stan_files/exactsizes.cc"
Wrote C++ file "stan_files/lowranksizes.cc"
Error in readRDS("/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpTnxXE5/filea2b8554ba055") : 
  error reading from connection
Calls: .Last -> readRDS
Execution halted
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.cpp -o init.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/exact.cc -o stan_files/exact.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/lowrank.cc -o stan_files/lowrank.o
make: *** [stan_files/lowranksizes.cc] Error 1
make: *** Waiting for unfinished jobs....
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/exact.cc:3:
stan_files/exact.hpp:397:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/lowrank.cc:3:
stan_files/lowrank.hpp:495:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
17 warnings generated.
17 warnings generated.
rm stan_files/exact.cc stan_files/exactsizes.cc stan_files/lowranksizes.cc stan_files/lowrank.cc
ERROR: compilation failed for package ‘DeLorean’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/DeLorean/new/DeLorean.Rcheck/DeLorean’

```
### CRAN

```
* installing *source* package ‘DeLorean’ ...
** package ‘DeLorean’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/exact.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/exactsizes.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/lowrank.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/lowranksizes.stan
Registered S3 methods overwritten by 'ggplot2':
  method         from 
  [.quosures     rlang
  c.quosures     rlang
  print.quosures rlang
Registered S3 methods overwritten by 'ggplot2':
  method         from 
  [.quosures     rlang
  c.quosures     rlang
  print.quosures rlang
Registered S3 methods overwritten by 'ggplot2':
  method         from 
  [.quosures     rlang
  c.quosures     rlang
  print.quosures rlang
Registered S3 methods overwritten by 'ggplot2':
  method         from 
  [.quosures     rlang
  c.quosures     rlang
  print.quosures rlang
Wrote C++ file "stan_files/exact.cc"
Wrote C++ file "stan_files/lowrank.cc"
Wrote C++ file "stan_files/exactsizes.cc"
Wrote C++ file "stan_files/lowranksizes.cc"
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.cpp -o init.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/exact.cc -o stan_files/exact.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/lowrank.cc -o stan_files/lowrank.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/exactsizes.cc -o stan_files/exactsizes.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/lowranksizes.cc -o stan_files/lowranksizes.o
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
25:
In file included from     #pragma clang diagnostic pop
                             ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/exact.cc:3:
In file included from stan_files/exact.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/exact.cc:3:
stan_files/exact.hpp:397:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/exactsizes.cc:3:
In file included from stan_files/exactsizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/exactsizes.cc:3:
stan_files/exactsizes.hpp:397:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/lowrank.cc:3:
In file included from stan_files/lowrank.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/lowrank.cc:3:
stan_files/lowrank.hpp:495:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/lowranksizes.cc:3:
In file included from stan_files/lowranksizes.hpp:25:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/DeLorean/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/lowranksizes.cc:3:
stan_files/lowranksizes.hpp:495:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
17 warnings generated.
17 warnings generated.
17 warnings generated.
17 warnings generated.
/usr/local/clang6/bin/clang++ -std=gnu++14 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o DeLorean.so stan_files/exact.o stan_files/exactsizes.o stan_files/lowrank.o stan_files/lowranksizes.o init.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: text-based stub file /System/Library/Frameworks//CoreFoundation.framework/CoreFoundation.tbd and library file /System/Library/Frameworks//CoreFoundation.framework/CoreFoundation are out of sync. Falling back to library file for linking.
rm stan_files/exact.cc stan_files/exactsizes.cc stan_files/lowranksizes.cc stan_files/lowrank.cc
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/DeLorean/old/DeLorean.Rcheck/00LOCK-DeLorean/00new/DeLorean/libs
** R
** data
*** moving datasets to lazyload DB
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
* DONE (DeLorean)

```
# esmisc

<details>

* Version: 0.0.3
* Source code: https://github.com/cran/esmisc
* URL: https://github.com/EDiLD/esmisc
* BugReports: https://github.com/EDiLD/esmisc/issues
* Date/Publication: 2017-01-11 10:34:49
* Number of recursive dependencies: 44

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
* Number of recursive dependencies: 81

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

# flowAI

<details>

* Version: 1.12.7
* Source code: https://github.com/cran/flowAI
* Date/Publication: 2019-03-04
* Number of recursive dependencies: 71

Run `revdep_details(,"flowAI")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   5.1Mb
    ```

# ggformula

<details>

* Version: 0.9.1
* Source code: https://github.com/cran/ggformula
* URL: https://github.com/ProjectMOSAIC/ggformula
* BugReports: https://github.com/ProjectMOSAIC/ggformula/issues
* Date/Publication: 2019-01-12 19:00:04 UTC
* Number of recursive dependencies: 162

Run `revdep_details(,"ggformula")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        R     1.0Mb
        doc   2.7Mb
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# ggstatsplot

<details>

* Version: 0.0.10
* Source code: https://github.com/cran/ggstatsplot
* URL: https://indrajeetpatil.github.io/ggstatsplot/, https://github.com/IndrajeetPatil/ggstatsplot
* BugReports: https://github.com/IndrajeetPatil/ggstatsplot/issues
* Date/Publication: 2019-03-17 17:50:02 UTC
* Number of recursive dependencies: 219

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

# HistDAWass

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/HistDAWass
* Date/Publication: 2018-03-20 16:23:27 UTC
* Number of recursive dependencies: 79

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
    Error: Can't draw more than one boxplot per group. Did you forget aes(group = ...)?
    Execution halted
    ```

# hybridModels

<details>

* Version: 0.3.5
* Source code: https://github.com/cran/hybridModels
* URL: https://github.com/fernandosm/hybridModels
* BugReports: https://github.com/fernandosm/hybridModels/issues
* Date/Publication: 2018-06-15 18:59:34 UTC
* Number of recursive dependencies: 41

Run `revdep_details(,"hybridModels")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: findContactChain
    > ### Title: Finding elements in contact chains of a dynamic network.
    > ### Aliases: findContactChain
    > 
    > ### ** Examples
    > 
    > # Loading data
    > data(networkSample) # help("networkSample"), for more info.
    >  
    > # contact chain function
    > selected.nodes <- c(37501, 36811, 36812)
    > contact.chain <- findContactChain(Data = networkSample, from = 'originID',
    +                                   to = 'destinationID', Time = 'Day', selected.nodes,
    +                                   type = 'chain', numberOfcores = 2)
    Warning in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  :
      port 11101 cannot be opened
    Error in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  : 
      cannot open the connection
    Calls: findContactChain ... makePSOCKcluster -> newPSOCKnode -> socketConnection
    Execution halted
    ```

# lime

<details>

* Version: 0.4.1
* Source code: https://github.com/cran/lime
* URL: https://github.com/thomasp85/lime
* BugReports: https://github.com/thomasp85/lime/issues
* Date/Publication: 2018-11-21 12:50:02 UTC
* Number of recursive dependencies: 127

Run `revdep_details(,"lime")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## Newly fixed

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

# MTLR

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/MTLR
* URL: https://github.com/haiderstats/MTLR
* BugReports: https://github.com/haiderstats/MTLR/issues
* Date/Publication: 2019-03-09 17:22:41 UTC
* Number of recursive dependencies: 59

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

# PepsNMR

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/PepsNMR
* URL: https://github.com/ManonMartin/PepsNMR
* BugReports: https://github.com/ManonMartin/PepsNMR/issues
* Date/Publication: 2019-03-08
* Number of recursive dependencies: 56

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

# RITAN

<details>

* Version: 1.6.0
* Source code: https://github.com/cran/RITAN
* Date/Publication: 2018-10-30
* Number of recursive dependencies: 95

Run `revdep_details(,"RITAN")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## Newly fixed

*   checking examples ... ERROR
    ```
    ...
    > require(RITANdata)
    Loading required package: RITANdata
    > myGeneSet <- c('BRCA1','RAD51C','VAV1','HRAS','ABCC1','CYP1B1','CYP3A5')
    > 
    > ## Not run: 
    > ##D ## We suggest using term_enrichment() instead. E.g.:
    > ##D e <- enrichment_symbols(myGeneSet, 'GO')
    > ## End(Not run)
    > 
    > ## But, you may use enrichment_symbols() directly for an individual term:
    > load_geneset_symbols('GO')
    Loading the requested genesets of "GO"...
    
    	Loaded 15831 genesets.
    > e <- enrichment_symbols(myGeneSet, 'DNA_repair')
    Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
      URL 'ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/locus_groups/protein-coding_gene.txt': status was 'Transferred a partial file'
    Error in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  : 
      cannot read from connection
    Calls: enrichment_symbols ... load_all_protein_coding_symbols -> read.table -> scan
    Execution halted
    ```

## In both

*   checking whether package ‘RITAN’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘gplots::plotCI’ by ‘plotrix::plotCI’ when loading ‘RITAN’
      Warning: replacing previous import ‘igraph::decompose’ by ‘stats::decompose’ when loading ‘RITAN’
      Warning: replacing previous import ‘gplots::lowess’ by ‘stats::lowess’ when loading ‘RITAN’
      Warning: replacing previous import ‘igraph::spectrum’ by ‘stats::spectrum’ when loading ‘RITAN’
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/RITAN/new/RITAN.Rcheck/00install.out’ for details.
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Documented arguments not in \usage in documentation object 'resource_reduce':
      ‘mutual_overlap’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘sqldf’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BgeeDB’ ‘knitr’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    load_geneset_symbols: no visible binding for global variable
      ‘geneset_list’
    load_geneset_symbols: no visible binding for global variable ‘f’
    load_geneset_symbols: no visible binding for global variable
      ‘active_genesets’
    network_overlap : <anonymous>: no visible binding for global variable
      ‘network_list’
    network_overlap: no visible binding for global variable ‘network_list’
    network_overlap : select_edges: no visible binding for global variable
      ‘network_list’
    plot.term_enrichment_by_subset: no visible binding for global variable
      ‘Var2’
    plot.term_enrichment_by_subset: no visible binding for global variable
      ‘Var1’
    show_active_genesets_hist: no visible binding for global variable
      ‘active_genesets’
    term_enrichment : process_source: no visible binding for global
      variable ‘active_genesets’
    Undefined global functions or variables:
      Var1 Var2 active_genesets all_net all_symbols f geneset_list
      network_list ss2
    ```

# Roleswitch

<details>

* Version: 1.20.0
* Source code: https://github.com/cran/Roleswitch
* URL: http://www.cs.utoronto.ca/~yueli/roleswitch.html
* Date/Publication: 2018-10-30
* Number of recursive dependencies: 70

Run `revdep_details(,"Roleswitch")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ...
    ```
       ‘Roleswitch.Rnw’ ... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘Roleswitch.Rnw’
      ...
    > if (any(z < 0)) z <- rescale(as.matrix(z), to = c(0, 
    +     max(z)))
    
    > seedMatrix <- getSeedMatrix(species = "human")
    
      When sourcing ‘Roleswitch.R’:
    Error: Unexpected format to the list of available marts.
    Please check the following URL manually, and try ?listMarts for advice.
    http://www.ensembl.org:80/biomart/martservice?type=registry&requestid=biomaRt
    Execution halted
    ```

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘Roleswitch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getSeedMatrix
    > ### Title: Get seed-match matrix between defined mRNA and miRNA in an
    > ###   organism.
    > ### Aliases: getSeedMatrix
    > ### Keywords: miRNA seedmatch
    > 
    > ### ** Examples
    > 
    > 
    > seedMatrix.human <- getSeedMatrix()
    Error in getBM(mart = mart, attributes = c(mRNA_id_type, "ensembl_gene_id",  : 
      The query to the BioMart webservice returned an invalid result: biomaRt expected a character string of length 1. 
    Please report this on the support site at http://support.bioconductor.org
    Calls: getSeedMatrix -> getTranscriptIDwithLongest3UTR -> getBM
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      'pracma', 'reshape', 'plotrix', 'microRNA', 'biomaRt', 'Biostrings',
      'Biobase', 'DBI'
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking R code for possible problems ... NOTE
    ```
    diagnosticPlot: no visible global function definition for ‘par’
    diagnosticPlot: no visible global function definition for ‘axis’
    diagnosticPlot: no visible global function definition for ‘plot’
    getSeedMatrix: no visible global function definition for ‘data’
    getTranscriptIDwithLongest3UTR: no visible global function definition
      for ‘aggregate’
    roleswitch: no visible global function definition for ‘aggregate’
    Undefined global functions or variables:
      aggregate axis data par plot
    Consider adding
      importFrom("graphics", "axis", "par", "plot")
      importFrom("stats", "aggregate")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# rstap

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/rstap
* URL: https://biostatistics4socialimpact.github.io/rstap
* BugReports: https://github.com/biostatistics4socialimpact/rstap/issues
* Date/Publication: 2019-02-06 20:30:03 UTC
* Number of recursive dependencies: 104

Run `revdep_details(,"rstap")` for more info

</details>

## Newly broken

*   checking whether package ‘rstap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rstap/new/rstap.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        libs   7.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘loo’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Installation

### Devel

```
* installing *source* package ‘rstap’ ...
** package ‘rstap’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/stap_bernoulli.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/stap_binomial.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/stap_continuous.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/stap_count.stan
Wrote C++ file "stan_files/stap_binomial.cc"
Wrote C++ file "stan_files/stap_count.cc"
DIAGNOSTIC(S) FROM PARSER:
Info (non-fatal): Comments beginning with # are deprecated.  Please use // in place of # for line comments.

Wrote C++ file "stan_files/stap_continuous.cc"
Wrote C++ file "stan_files/stap_bernoulli.cc"
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.cpp -o init.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/stap_binomial.cc -o stan_files/stap_binomial.o
Error in readRDS("/var/folders/lb/xhxqmcrd7gv302_b1pdfykh80000gn/T//RtmpTnxXE5/filea2b82ff372a8") : 
  error reading from connection
Calls: .Last -> readRDS
Execution halted
make: *** [stan_files/stap_bernoulli.cc] Error 1
make: *** Waiting for unfinished jobs....
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/stap_binomial.cc:3:
stan_files/stap_binomial.hpp:661:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
17 warnings generated.
rm stan_files/stap_continuous.cc stan_files/stap_bernoulli.cc stan_files/stap_count.cc stan_files/stap_binomial.cc
ERROR: compilation failed for package ‘rstap’
* removing ‘/Users/max/github/forks/ggplot2/revdep/checks.noindex/rstap/new/rstap.Rcheck/rstap’

```
### CRAN

```
* installing *source* package ‘rstap’ ...
** package ‘rstap’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/stap_bernoulli.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/stap_binomial.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/stap_continuous.stan
"/Library/Frameworks/R.framework/Resources/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/stap_count.stan
Registered S3 methods overwritten by 'ggplot2':
  method         from 
  [.quosures     rlang
  c.quosures     rlang
  print.quosures rlang
Registered S3 methods overwritten by 'ggplot2':
  method         from 
  [.quosures     rlang
  c.quosures     rlang
  print.quosures rlang
Registered S3 methods overwritten by 'ggplot2':
  method         from 
  [.quosures     rlang
  c.quosures     rlang
  print.quosures rlang
Registered S3 methods overwritten by 'ggplot2':
  method         from 
  [.quosures     rlang
  c.quosures     rlang
  print.quosures rlang
Wrote C++ file "stan_files/stap_binomial.cc"
Wrote C++ file "stan_files/stap_count.cc"
Wrote C++ file "stan_files/stap_bernoulli.cc"
DIAGNOSTIC(S) FROM PARSER:
Info (non-fatal): Comments beginning with # are deprecated.  Please use // in place of # for line comments.

Wrote C++ file "stan_files/stap_continuous.cc"
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.cpp -o init.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/stap_binomial.cc -o stan_files/stap_binomial.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/stap_count.cc -o stan_files/stap_count.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/stap_bernoulli.cc -o stan_files/stap_bernoulli.o
/usr/local/clang6/bin/clang++ -std=gnu++14 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -I"../inst/include" -I"`"/Library/Frameworks/R.framework/Resources/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error  -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/Rcpp/include" -I"/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c stan_files/stap_continuous.cc -o stan_files/stap_continuous.o
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:15:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/additive_combine.hpp:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/linear_congruential.hpp:30:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/const_mod.hpp:23:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/large_arithmetic.hpp:19:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/random/detail/integer_log2.hpp:19:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/pending/integer_log2.hpp:7:1: warning: This header is deprecated. Use <boost/integer/integer_log2.hpp> instead. [-W#pragma-messages]
BOOST_HEADER_DEPRECATED("<boost/integer/integer_log2.hpp>");
^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/header_deprecated.hpp:23:37: note: expanded from macro 'BOOST_HEADER_DEPRECATED'
# define BOOST_HEADER_DEPRECATED(a) BOOST_PRAGMA_MESSAGE("This header is deprecated. Use " a " instead.")
                                    ^
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/BH/include/boost/config/pragma_message.hpp:24:34: note: expanded from macro 'BOOST_PRAGMA_MESSAGE'
# define BOOST_PRAGMA_MESSAGE(x) _Pragma(BOOST_STRINGIZE(message(x)))
                                 ^
<scratch space>:73:2: note: expanded from here
 message("This header is deprecated. Use " "<boost/integer/integer_log2.hpp>" " instead.")
 ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:1:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Core:535:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:2:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/LU:47:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Jacobi:29:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Cholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/QR:17:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Householder:27:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:5:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SVD:48:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Geometry:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:14:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/matrix_vari.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat/fun/Eigen_NumTraits.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Dense:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Eigenvalues:58:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:26:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCore:66:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:27:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/OrderingMethods:71:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:29:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseCholesky:43:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:32:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/SparseQR:35:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:96:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/csr_extract_u.hpp:6:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/Sparse:33:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/IterativeLinearSolvers:46:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/RcppEigen/include/Eigen/src/Core/util/ReenableStupidWarnings.h:10:30: warning: pragma diagnostic pop could not pop, no matching push [-Wunknown-pragmas]
    #pragma clang diagnostic pop
                             ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/stap_binomial.cc:3:
In file included from stan_files/stap_binomial.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/stap_binomial.cc:3:
stan_files/stap_binomial.hpp:661:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/stap_bernoulli.cc:3:
In file included from stan_files/stap_bernoulli.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/stap_bernoulli.cc:3:
stan_files/stap_bernoulli.hpp:633:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/stap_count.cc:3:
In file included from stan_files/stap_count.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/stap_count.cc:3:
stan_files/stap_count.hpp:661:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core.hpp:44:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/core/set_zero_all_adjoints.hpp:14:13: warning: unused function 'set_zero_all_adjoints' [-Wunused-function]
static void set_zero_all_adjoints() {
            ^
In file included from stan_files/stap_continuous.cc:3:
In file included from stan_files/stap_continuous.hpp:18:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/rstaninc.hpp:3:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/rstan/include/rstan/stan_fit.hpp:34:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/test_gradients.hpp:7:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/rev/mat.hpp:12:
In file included from /Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat.hpp:70:
/Users/max/github/forks/ggplot2/revdep/library.noindex/rstap/StanHeaders/include/stan/math/prim/mat/fun/autocorrelation.hpp:18:8: warning: function 'fft_next_good_size' is not needed and will not be emitted [-Wunneeded-internal-declaration]
size_t fft_next_good_size(size_t N) {
       ^
In file included from stan_files/stap_continuous.cc:3:
stan_files/stap_continuous.hpp:657:30: warning: unused typedef 'fun_return_scalar_t__' [-Wunused-local-typedef]
    typedef local_scalar_t__ fun_return_scalar_t__;
                             ^
17 warnings generated.
17 warnings generated.
17 warnings generated.
17 warnings generated.
/usr/local/clang6/bin/clang++ -std=gnu++14 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o rstap.so stan_files/stap_bernoulli.o stan_files/stap_binomial.o stan_files/stap_continuous.o stan_files/stap_count.o init.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: text-based stub file /System/Library/Frameworks//CoreFoundation.framework/CoreFoundation.tbd and library file /System/Library/Frameworks//CoreFoundation.framework/CoreFoundation are out of sync. Falling back to library file for linking.
rm stan_files/stap_continuous.cc stan_files/stap_bernoulli.cc stan_files/stap_count.cc stan_files/stap_binomial.cc
installing to /Users/max/github/forks/ggplot2/revdep/checks.noindex/rstap/old/rstap.Rcheck/00LOCK-rstap/00new/rstap/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
DIAGNOSTIC(S) FROM PARSER:
Info (non-fatal): Comments beginning with # are deprecated.  Please use // in place of # for line comments.

** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (rstap)

```
# trialr

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/trialr
* URL: https://github.com/brockk/trialr
* BugReports: https://github.com/brockk/trialr/issues
* Date/Publication: 2019-04-21 21:00:03 UTC
* Number of recursive dependencies: 81

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
* Number of recursive dependencies: 89

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
* Number of recursive dependencies: 118

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
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        data   4.7Mb
        doc    6.1Mb
    ```

# xpose

<details>

* Version: 0.4.4
* Source code: https://github.com/cran/xpose
* URL: https://github.com/UUPharmacometrics/xpose
* BugReports: https://github.com/UUPharmacometrics/xpose/issues
* Date/Publication: 2019-03-21 17:10:03 UTC
* Number of recursive dependencies: 92

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

