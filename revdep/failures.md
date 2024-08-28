# abctools

<details>

* Version: 1.1.7
* GitHub: https://github.com/dennisprangle/abctools
* Source code: https://github.com/cran/abctools
* Date/Publication: 2023-09-18 10:40:02 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "abctools")` for more info

</details>

## In both

*   checking whether package ‘abctools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/abctools/new/abctools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘abctools’ ...
** package ‘abctools’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c abctools.c -o abctools.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c init.c -o init.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o abctools.so abctools.o init.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/abctools/new/abctools.Rcheck/00LOCK-abctools/00new/abctools/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘quantreg’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘abctools’
* removing ‘/tmp/workdir/abctools/new/abctools.Rcheck/abctools’


```
### CRAN

```
* installing *source* package ‘abctools’ ...
** package ‘abctools’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c abctools.c -o abctools.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c init.c -o init.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o abctools.so abctools.o init.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/abctools/old/abctools.Rcheck/00LOCK-abctools/00new/abctools/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘quantreg’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘abctools’
* removing ‘/tmp/workdir/abctools/old/abctools.Rcheck/abctools’


```
# animalEKF

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/animalEKF
* Date/Publication: 2023-09-29 15:32:41 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "animalEKF")` for more info

</details>

## In both

*   checking whether package ‘animalEKF’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/animalEKF/new/animalEKF.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘animalEKF’ ...
** package ‘animalEKF’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘animalEKF’
* removing ‘/tmp/workdir/animalEKF/new/animalEKF.Rcheck/animalEKF’


```
### CRAN

```
* installing *source* package ‘animalEKF’ ...
** package ‘animalEKF’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘animalEKF’
* removing ‘/tmp/workdir/animalEKF/old/animalEKF.Rcheck/animalEKF’


```
# ANOM

<details>

* Version: 0.5
* GitHub: https://github.com/PhilipPallmann/ANOM
* Source code: https://github.com/cran/ANOM
* Date/Publication: 2017-04-12 13:32:33 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "ANOM")` for more info

</details>

## In both

*   checking whether package ‘ANOM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ANOM/new/ANOM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ANOM’ ...
** package ‘ANOM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ANOM’
* removing ‘/tmp/workdir/ANOM/new/ANOM.Rcheck/ANOM’


```
### CRAN

```
* installing *source* package ‘ANOM’ ...
** package ‘ANOM’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ANOM’
* removing ‘/tmp/workdir/ANOM/old/ANOM.Rcheck/ANOM’


```
# atRisk

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/atRisk
* Date/Publication: 2023-08-08 14:50:05 UTC
* Number of recursive dependencies: 37

Run `revdepcheck::cloud_details(, "atRisk")` for more info

</details>

## In both

*   checking whether package ‘atRisk’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/atRisk/new/atRisk.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘atRisk’ ...
** package ‘atRisk’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘atRisk’
* removing ‘/tmp/workdir/atRisk/new/atRisk.Rcheck/atRisk’


```
### CRAN

```
* installing *source* package ‘atRisk’ ...
** package ‘atRisk’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘atRisk’
* removing ‘/tmp/workdir/atRisk/old/atRisk.Rcheck/atRisk’


```
# AutoScore

<details>

* Version: 1.0.0
* GitHub: https://github.com/nliulab/AutoScore
* Source code: https://github.com/cran/AutoScore
* Date/Publication: 2022-10-15 22:15:26 UTC
* Number of recursive dependencies: 170

Run `revdepcheck::cloud_details(, "AutoScore")` for more info

</details>

## In both

*   checking whether package ‘AutoScore’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/AutoScore/new/AutoScore.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘AutoScore’ ...
** package ‘AutoScore’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘AutoScore’
* removing ‘/tmp/workdir/AutoScore/new/AutoScore.Rcheck/AutoScore’


```
### CRAN

```
* installing *source* package ‘AutoScore’ ...
** package ‘AutoScore’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘AutoScore’
* removing ‘/tmp/workdir/AutoScore/old/AutoScore.Rcheck/AutoScore’


```
# bayesdfa

<details>

* Version: 1.3.3
* GitHub: https://github.com/fate-ewi/bayesdfa
* Source code: https://github.com/cran/bayesdfa
* Date/Publication: 2024-02-26 20:50:06 UTC
* Number of recursive dependencies: 89

Run `revdepcheck::cloud_details(, "bayesdfa")` for more info

</details>

## In both

*   checking whether package ‘bayesdfa’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bayesdfa’ ...
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_dfa_namespace::model_dfa; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:10:   required from here
/opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_dfa.o] Error 1
ERROR: compilation failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/bayesdfa’


```
### CRAN

```
* installing *source* package ‘bayesdfa’ ...
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_dfa_namespace::model_dfa; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:10:   required from here
/opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_dfa.o] Error 1
ERROR: compilation failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/old/bayesdfa.Rcheck/bayesdfa’


```
# bayesDP

<details>

* Version: 1.3.6
* GitHub: https://github.com/graemeleehickey/bayesDP
* Source code: https://github.com/cran/bayesDP
* Date/Publication: 2022-01-30 22:20:02 UTC
* Number of recursive dependencies: 80

Run `revdepcheck::cloud_details(, "bayesDP")` for more info

</details>

## In both

*   checking whether package ‘bayesDP’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bayesDP/new/bayesDP.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bayesDP’ ...
** package ‘bayesDP’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c bdplm.cpp -o bdplm.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c ppexp.cpp -o ppexp.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o bayesDP.so RcppExports.o bdplm.o ppexp.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/bayesDP/new/bayesDP.Rcheck/00LOCK-bayesDP/00new/bayesDP/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘bayesDP’
* removing ‘/tmp/workdir/bayesDP/new/bayesDP.Rcheck/bayesDP’


```
### CRAN

```
* installing *source* package ‘bayesDP’ ...
** package ‘bayesDP’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c bdplm.cpp -o bdplm.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c ppexp.cpp -o ppexp.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o bayesDP.so RcppExports.o bdplm.o ppexp.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/bayesDP/old/bayesDP.Rcheck/00LOCK-bayesDP/00new/bayesDP/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘bayesDP’
* removing ‘/tmp/workdir/bayesDP/old/bayesDP.Rcheck/bayesDP’


```
# BayesianFactorZoo

<details>

* Version: 0.0.0.2
* GitHub: NA
* Source code: https://github.com/cran/BayesianFactorZoo
* Date/Publication: 2023-11-14 12:43:44 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "BayesianFactorZoo")` for more info

</details>

## In both

*   checking whether package ‘BayesianFactorZoo’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BayesianFactorZoo/new/BayesianFactorZoo.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BayesianFactorZoo’ ...
** package ‘BayesianFactorZoo’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘BayesianFactorZoo’
* removing ‘/tmp/workdir/BayesianFactorZoo/new/BayesianFactorZoo.Rcheck/BayesianFactorZoo’


```
### CRAN

```
* installing *source* package ‘BayesianFactorZoo’ ...
** package ‘BayesianFactorZoo’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘BayesianFactorZoo’
* removing ‘/tmp/workdir/BayesianFactorZoo/old/BayesianFactorZoo.Rcheck/BayesianFactorZoo’


```
# BayesSurvive

<details>

* Version: 0.0.2
* GitHub: https://github.com/ocbe-uio/BayesSurvive
* Source code: https://github.com/cran/BayesSurvive
* Date/Publication: 2024-06-04 13:20:12 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "BayesSurvive")` for more info

</details>

## In both

*   checking whether package ‘BayesSurvive’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BayesSurvive/new/BayesSurvive.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BayesSurvive’ ...
** package ‘BayesSurvive’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether the compiler supports GNU C++... yes
checking whether g++ -std=gnu++17 accepts -g... yes
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘BayesSurvive’
* removing ‘/tmp/workdir/BayesSurvive/new/BayesSurvive.Rcheck/BayesSurvive’


```
### CRAN

```
* installing *source* package ‘BayesSurvive’ ...
** package ‘BayesSurvive’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether the compiler supports GNU C++... yes
checking whether g++ -std=gnu++17 accepts -g... yes
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘BayesSurvive’
* removing ‘/tmp/workdir/BayesSurvive/old/BayesSurvive.Rcheck/BayesSurvive’


```
# BCClong

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/BCClong
* Date/Publication: 2024-06-24 00:00:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "BCClong")` for more info

</details>

## In both

*   checking whether package ‘BCClong’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BCClong/new/BCClong.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BCClong’ ...
** package ‘BCClong’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c BCC.cpp -o BCC.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c Likelihood.cpp -o Likelihood.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c c_which.cpp -o c_which.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o BCClong.so BCC.o Likelihood.o RcppExports.o c_which.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘BCClong’
* removing ‘/tmp/workdir/BCClong/new/BCClong.Rcheck/BCClong’


```
### CRAN

```
* installing *source* package ‘BCClong’ ...
** package ‘BCClong’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c BCC.cpp -o BCC.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c Likelihood.cpp -o Likelihood.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c c_which.cpp -o c_which.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o BCClong.so BCC.o Likelihood.o RcppExports.o c_which.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘BCClong’
* removing ‘/tmp/workdir/BCClong/old/BCClong.Rcheck/BCClong’


```
# BGGM

<details>

* Version: 2.1.3
* GitHub: https://github.com/donaldRwilliams/BGGM
* Source code: https://github.com/cran/BGGM
* Date/Publication: 2024-07-05 20:30:02 UTC
* Number of recursive dependencies: 208

Run `revdepcheck::cloud_details(, "BGGM")` for more info

</details>

## In both

*   checking whether package ‘BGGM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BGGM/new/BGGM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BGGM’ ...
** package ‘BGGM’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++17 accepts -g... yes
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘BGGM’
* removing ‘/tmp/workdir/BGGM/new/BGGM.Rcheck/BGGM’


```
### CRAN

```
* installing *source* package ‘BGGM’ ...
** package ‘BGGM’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C++ compiler... yes
checking whether g++ -std=gnu++17 accepts -g... yes
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘BGGM’
* removing ‘/tmp/workdir/BGGM/old/BGGM.Rcheck/BGGM’


```
# binsreg

<details>

* Version: 1.0
* GitHub: NA
* Source code: https://github.com/cran/binsreg
* Date/Publication: 2023-07-11 12:00:24 UTC
* Number of recursive dependencies: 35

Run `revdepcheck::cloud_details(, "binsreg")` for more info

</details>

## In both

*   checking whether package ‘binsreg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/binsreg/new/binsreg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘binsreg’ ...
** package ‘binsreg’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘binsreg’
* removing ‘/tmp/workdir/binsreg/new/binsreg.Rcheck/binsreg’


```
### CRAN

```
* installing *source* package ‘binsreg’ ...
** package ‘binsreg’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘binsreg’
* removing ‘/tmp/workdir/binsreg/old/binsreg.Rcheck/binsreg’


```
# bmstdr

<details>

* Version: 0.7.9
* GitHub: https://github.com/sujit-sahu/bmstdr
* Source code: https://github.com/cran/bmstdr
* Date/Publication: 2023-12-18 15:00:02 UTC
* Number of recursive dependencies: 215

Run `revdepcheck::cloud_details(, "bmstdr")` for more info

</details>

## In both

*   checking whether package ‘bmstdr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bmstdr/new/bmstdr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bmstdr’ ...
** package ‘bmstdr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘bmstdr’
* removing ‘/tmp/workdir/bmstdr/new/bmstdr.Rcheck/bmstdr’


```
### CRAN

```
* installing *source* package ‘bmstdr’ ...
** package ‘bmstdr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘bmstdr’
* removing ‘/tmp/workdir/bmstdr/old/bmstdr.Rcheck/bmstdr’


```
# bspcov

<details>

* Version: 1.0.0
* GitHub: https://github.com/statjs/bspcov
* Source code: https://github.com/cran/bspcov
* Date/Publication: 2024-02-06 16:50:08 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "bspcov")` for more info

</details>

## In both

*   checking whether package ‘bspcov’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/bspcov/new/bspcov.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘bspcov’ ...
** package ‘bspcov’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘bspcov’
* removing ‘/tmp/workdir/bspcov/new/bspcov.Rcheck/bspcov’


```
### CRAN

```
* installing *source* package ‘bspcov’ ...
** package ‘bspcov’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘bspcov’
* removing ‘/tmp/workdir/bspcov/old/bspcov.Rcheck/bspcov’


```
# BuyseTest

<details>

* Version: 3.0.4
* GitHub: https://github.com/bozenne/BuyseTest
* Source code: https://github.com/cran/BuyseTest
* Date/Publication: 2024-07-01 09:20:02 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "BuyseTest")` for more info

</details>

## In both

*   checking whether package ‘BuyseTest’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BuyseTest/new/BuyseTest.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BuyseTest’ ...
** package ‘BuyseTest’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c FCT_buyseTest.cpp -o FCT_buyseTest.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c FCT_precompute.cpp -o FCT_precompute.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c utils-from-riskRegression.cpp -o utils-from-riskRegression.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o BuyseTest.so FCT_buyseTest.o FCT_precompute.o RcppExports.o utils-from-riskRegression.o -L/opt/R/4.3.1/lib/R/lib -lR
...
installing to /tmp/workdir/BuyseTest/new/BuyseTest.Rcheck/00LOCK-BuyseTest/00new/BuyseTest/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Error: unable to load R code in package ‘BuyseTest’
Execution halted
ERROR: lazy loading failed for package ‘BuyseTest’
* removing ‘/tmp/workdir/BuyseTest/new/BuyseTest.Rcheck/BuyseTest’


```
### CRAN

```
* installing *source* package ‘BuyseTest’ ...
** package ‘BuyseTest’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c FCT_buyseTest.cpp -o FCT_buyseTest.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c FCT_precompute.cpp -o FCT_precompute.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c utils-from-riskRegression.cpp -o utils-from-riskRegression.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o BuyseTest.so FCT_buyseTest.o FCT_precompute.o RcppExports.o utils-from-riskRegression.o -L/opt/R/4.3.1/lib/R/lib -lR
...
installing to /tmp/workdir/BuyseTest/old/BuyseTest.Rcheck/00LOCK-BuyseTest/00new/BuyseTest/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Error: unable to load R code in package ‘BuyseTest’
Execution halted
ERROR: lazy loading failed for package ‘BuyseTest’
* removing ‘/tmp/workdir/BuyseTest/old/BuyseTest.Rcheck/BuyseTest’


```
# CalibrationCurves

<details>

* Version: 2.0.3
* GitHub: NA
* Source code: https://github.com/cran/CalibrationCurves
* Date/Publication: 2024-07-02 08:50:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "CalibrationCurves")` for more info

</details>

## In both

*   checking whether package ‘CalibrationCurves’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/CalibrationCurves/new/CalibrationCurves.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CalibrationCurves’ ...
** package ‘CalibrationCurves’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rms’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘CalibrationCurves’
* removing ‘/tmp/workdir/CalibrationCurves/new/CalibrationCurves.Rcheck/CalibrationCurves’


```
### CRAN

```
* installing *source* package ‘CalibrationCurves’ ...
** package ‘CalibrationCurves’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rms’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘CalibrationCurves’
* removing ‘/tmp/workdir/CalibrationCurves/old/CalibrationCurves.Rcheck/CalibrationCurves’


```
# CARBayesST

<details>

* Version: 4.0
* GitHub: https://github.com/duncanplee/CARBayesST
* Source code: https://github.com/cran/CARBayesST
* Date/Publication: 2023-10-30 16:40:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "CARBayesST")` for more info

</details>

## In both

*   checking whether package ‘CARBayesST’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/CARBayesST/new/CARBayesST.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CARBayesST’ ...
** package ‘CARBayesST’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CARBayesST.cpp -o CARBayesST.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o CARBayesST.so CARBayesST.o RcppExports.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/CARBayesST/new/CARBayesST.Rcheck/00LOCK-CARBayesST/00new/CARBayesST/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘CARBayesST’
* removing ‘/tmp/workdir/CARBayesST/new/CARBayesST.Rcheck/CARBayesST’


```
### CRAN

```
* installing *source* package ‘CARBayesST’ ...
** package ‘CARBayesST’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CARBayesST.cpp -o CARBayesST.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o CARBayesST.so CARBayesST.o RcppExports.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/CARBayesST/old/CARBayesST.Rcheck/00LOCK-CARBayesST/00new/CARBayesST/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘CARBayesST’
* removing ‘/tmp/workdir/CARBayesST/old/CARBayesST.Rcheck/CARBayesST’


```
# CaseBasedReasoning

<details>

* Version: 0.3
* GitHub: https://github.com/sipemu/case-based-reasoning
* Source code: https://github.com/cran/CaseBasedReasoning
* Date/Publication: 2023-05-02 08:40:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "CaseBasedReasoning")` for more info

</details>

## In both

*   checking whether package ‘CaseBasedReasoning’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/CaseBasedReasoning/new/CaseBasedReasoning.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CaseBasedReasoning’ ...
** package ‘CaseBasedReasoning’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c distanceAPI.cpp -o distanceAPI.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c distances.cpp -o distances.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c order.cpp -o order.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c weightedKNN.cpp -o weightedKNN.o
...
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o CaseBasedReasoning.so RcppExports.o distanceAPI.o distances.o order.o weightedKNN.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/CaseBasedReasoning/new/CaseBasedReasoning.Rcheck/00LOCK-CaseBasedReasoning/00new/CaseBasedReasoning/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rms’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘CaseBasedReasoning’
* removing ‘/tmp/workdir/CaseBasedReasoning/new/CaseBasedReasoning.Rcheck/CaseBasedReasoning’


```
### CRAN

```
* installing *source* package ‘CaseBasedReasoning’ ...
** package ‘CaseBasedReasoning’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c distanceAPI.cpp -o distanceAPI.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c distances.cpp -o distances.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c order.cpp -o order.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2  -c weightedKNN.cpp -o weightedKNN.o
...
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o CaseBasedReasoning.so RcppExports.o distanceAPI.o distances.o order.o weightedKNN.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/CaseBasedReasoning/old/CaseBasedReasoning.Rcheck/00LOCK-CaseBasedReasoning/00new/CaseBasedReasoning/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rms’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘CaseBasedReasoning’
* removing ‘/tmp/workdir/CaseBasedReasoning/old/CaseBasedReasoning.Rcheck/CaseBasedReasoning’


```
# CGPfunctions

<details>

* Version: 0.6.3
* GitHub: https://github.com/ibecav/CGPfunctions
* Source code: https://github.com/cran/CGPfunctions
* Date/Publication: 2020-11-12 14:50:09 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "CGPfunctions")` for more info

</details>

## In both

*   checking whether package ‘CGPfunctions’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/CGPfunctions/new/CGPfunctions.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CGPfunctions’ ...
** package ‘CGPfunctions’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘CGPfunctions’
* removing ‘/tmp/workdir/CGPfunctions/new/CGPfunctions.Rcheck/CGPfunctions’


```
### CRAN

```
* installing *source* package ‘CGPfunctions’ ...
** package ‘CGPfunctions’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘CGPfunctions’
* removing ‘/tmp/workdir/CGPfunctions/old/CGPfunctions.Rcheck/CGPfunctions’


```
# cmprskcoxmsm

<details>

* Version: 0.2.1
* GitHub: NA
* Source code: https://github.com/cran/cmprskcoxmsm
* Date/Publication: 2021-09-04 05:50:02 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "cmprskcoxmsm")` for more info

</details>

## In both

*   checking whether package ‘cmprskcoxmsm’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/cmprskcoxmsm/new/cmprskcoxmsm.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘cmprskcoxmsm’ ...
** package ‘cmprskcoxmsm’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘cmprskcoxmsm’
* removing ‘/tmp/workdir/cmprskcoxmsm/new/cmprskcoxmsm.Rcheck/cmprskcoxmsm’


```
### CRAN

```
* installing *source* package ‘cmprskcoxmsm’ ...
** package ‘cmprskcoxmsm’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘cmprskcoxmsm’
* removing ‘/tmp/workdir/cmprskcoxmsm/old/cmprskcoxmsm.Rcheck/cmprskcoxmsm’


```
# contrast

<details>

* Version: 0.24.2
* GitHub: https://github.com/Alanocallaghan/contrast
* Source code: https://github.com/cran/contrast
* Date/Publication: 2022-10-05 17:20:09 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "contrast")` for more info

</details>

## In both

*   checking whether package ‘contrast’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/contrast/new/contrast.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘contrast’ ...
** package ‘contrast’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘contrast’
* removing ‘/tmp/workdir/contrast/new/contrast.Rcheck/contrast’


```
### CRAN

```
* installing *source* package ‘contrast’ ...
** package ‘contrast’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘contrast’
* removing ‘/tmp/workdir/contrast/old/contrast.Rcheck/contrast’


```
# coxed

<details>

* Version: 0.3.3
* GitHub: https://github.com/jkropko/coxed
* Source code: https://github.com/cran/coxed
* Date/Publication: 2020-08-02 01:20:07 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "coxed")` for more info

</details>

## In both

*   checking whether package ‘coxed’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/coxed/new/coxed.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘coxed’ ...
** package ‘coxed’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rms’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘coxed’
* removing ‘/tmp/workdir/coxed/new/coxed.Rcheck/coxed’


```
### CRAN

```
* installing *source* package ‘coxed’ ...
** package ‘coxed’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘rms’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘coxed’
* removing ‘/tmp/workdir/coxed/old/coxed.Rcheck/coxed’


```
# CRMetrics

<details>

* Version: 0.3.0
* GitHub: https://github.com/khodosevichlab/CRMetrics
* Source code: https://github.com/cran/CRMetrics
* Date/Publication: 2023-09-01 09:00:06 UTC
* Number of recursive dependencies: 239

Run `revdepcheck::cloud_details(, "CRMetrics")` for more info

</details>

## In both

*   checking whether package ‘CRMetrics’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/CRMetrics/new/CRMetrics.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CRMetrics’ ...
** package ‘CRMetrics’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘CRMetrics’
* removing ‘/tmp/workdir/CRMetrics/new/CRMetrics.Rcheck/CRMetrics’


```
### CRAN

```
* installing *source* package ‘CRMetrics’ ...
** package ‘CRMetrics’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘CRMetrics’
* removing ‘/tmp/workdir/CRMetrics/old/CRMetrics.Rcheck/CRMetrics’


```
# csmpv

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/csmpv
* Date/Publication: 2024-03-01 18:12:44 UTC
* Number of recursive dependencies: 178

Run `revdepcheck::cloud_details(, "csmpv")` for more info

</details>

## In both

*   checking whether package ‘csmpv’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/csmpv/new/csmpv.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘csmpv’ ...
** package ‘csmpv’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘csmpv’
* removing ‘/tmp/workdir/csmpv/new/csmpv.Rcheck/csmpv’


```
### CRAN

```
* installing *source* package ‘csmpv’ ...
** package ‘csmpv’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘csmpv’
* removing ‘/tmp/workdir/csmpv/old/csmpv.Rcheck/csmpv’


```
# ctsem

<details>

* Version: 3.10.0
* GitHub: https://github.com/cdriveraus/ctsem
* Source code: https://github.com/cran/ctsem
* Date/Publication: 2024-05-09 14:40:03 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "ctsem")` for more info

</details>

## In both

*   checking whether package ‘ctsem’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ctsem/new/ctsem.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ctsem’ ...
** package ‘ctsem’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:10:   required from here
/opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_ctsm.o] Error 1
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/new/ctsem.Rcheck/ctsem’


```
### CRAN

```
* installing *source* package ‘ctsem’ ...
** package ‘ctsem’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:10:   required from here
/opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_ctsm.o] Error 1
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/old/ctsem.Rcheck/ctsem’


```
# DepthProc

<details>

* Version: 2.1.5
* GitHub: https://github.com/zzawadz/DepthProc
* Source code: https://github.com/cran/DepthProc
* Date/Publication: 2022-02-03 20:30:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "DepthProc")` for more info

</details>

## In both

*   checking whether package ‘DepthProc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/DepthProc/new/DepthProc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DepthProc’ ...
** package ‘DepthProc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c Depth.cpp -o Depth.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationEstimators.cpp -o LocationEstimators.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationScaleDepth.cpp -o LocationScaleDepth.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationScaleDepthCPP.cpp -o LocationScaleDepthCPP.o
...
installing to /tmp/workdir/DepthProc/new/DepthProc.Rcheck/00LOCK-DepthProc/00new/DepthProc/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘np’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘DepthProc’
* removing ‘/tmp/workdir/DepthProc/new/DepthProc.Rcheck/DepthProc’


```
### CRAN

```
* installing *source* package ‘DepthProc’ ...
** package ‘DepthProc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c Depth.cpp -o Depth.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationEstimators.cpp -o LocationEstimators.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationScaleDepth.cpp -o LocationScaleDepth.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationScaleDepthCPP.cpp -o LocationScaleDepthCPP.o
...
installing to /tmp/workdir/DepthProc/old/DepthProc.Rcheck/00LOCK-DepthProc/00new/DepthProc/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘np’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘DepthProc’
* removing ‘/tmp/workdir/DepthProc/old/DepthProc.Rcheck/DepthProc’


```
# DR.SC

<details>

* Version: 3.4
* GitHub: https://github.com/feiyoung/DR.SC
* Source code: https://github.com/cran/DR.SC
* Date/Publication: 2024-03-19 08:40:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "DR.SC")` for more info

</details>

## In both

*   checking whether package ‘DR.SC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/DR.SC/new/DR.SC.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DR.SC’ ...
** package ‘DR.SC’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c getNB_fast.cpp -o getNB_fast.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c mt_paral_job.cpp -o mt_paral_job.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c mt_paral_job2.cpp -o mt_paral_job2.o
...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘DR.SC’
* removing ‘/tmp/workdir/DR.SC/new/DR.SC.Rcheck/DR.SC’


```
### CRAN

```
* installing *source* package ‘DR.SC’ ...
** package ‘DR.SC’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c getNB_fast.cpp -o getNB_fast.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c mt_paral_job.cpp -o mt_paral_job.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c mt_paral_job2.cpp -o mt_paral_job2.o
...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘DR.SC’
* removing ‘/tmp/workdir/DR.SC/old/DR.SC.Rcheck/DR.SC’


```
# DynNom

<details>

* Version: 5.1
* GitHub: NA
* Source code: https://github.com/cran/DynNom
* Date/Publication: 2024-06-07 12:20:21 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "DynNom")` for more info

</details>

## In both

*   checking whether package ‘DynNom’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/DynNom/new/DynNom.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘DynNom’ ...
** package ‘DynNom’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘DynNom’
* removing ‘/tmp/workdir/DynNom/new/DynNom.Rcheck/DynNom’


```
### CRAN

```
* installing *source* package ‘DynNom’ ...
** package ‘DynNom’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘DynNom’
* removing ‘/tmp/workdir/DynNom/old/DynNom.Rcheck/DynNom’


```
# easybgm

<details>

* Version: 0.1.2
* GitHub: https://github.com/KarolineHuth/easybgm
* Source code: https://github.com/cran/easybgm
* Date/Publication: 2024-03-13 13:40:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "easybgm")` for more info

</details>

## In both

*   checking whether package ‘easybgm’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/easybgm/new/easybgm.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘easybgm’ ...
** package ‘easybgm’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘easybgm’
* removing ‘/tmp/workdir/easybgm/new/easybgm.Rcheck/easybgm’


```
### CRAN

```
* installing *source* package ‘easybgm’ ...
** package ‘easybgm’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘easybgm’
* removing ‘/tmp/workdir/easybgm/old/easybgm.Rcheck/easybgm’


```
# ecolottery

<details>

* Version: 1.0.0
* GitHub: https://github.com/frmunoz/ecolottery
* Source code: https://github.com/cran/ecolottery
* Date/Publication: 2017-07-03 11:01:29 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "ecolottery")` for more info

</details>

## In both

*   checking whether package ‘ecolottery’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ecolottery/new/ecolottery.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ecolottery’ ...
** package ‘ecolottery’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ecolottery’
* removing ‘/tmp/workdir/ecolottery/new/ecolottery.Rcheck/ecolottery’


```
### CRAN

```
* installing *source* package ‘ecolottery’ ...
** package ‘ecolottery’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ecolottery’
* removing ‘/tmp/workdir/ecolottery/old/ecolottery.Rcheck/ecolottery’


```
# EpiEstim

<details>

* Version: 2.2-4
* GitHub: https://github.com/mrc-ide/EpiEstim
* Source code: https://github.com/cran/EpiEstim
* Date/Publication: 2021-01-07 16:20:10 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "EpiEstim")` for more info

</details>

## In both

*   checking whether package ‘EpiEstim’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/EpiEstim/new/EpiEstim.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘EpiEstim’ ...
** package ‘EpiEstim’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘EpiEstim’
* removing ‘/tmp/workdir/EpiEstim/new/EpiEstim.Rcheck/EpiEstim’


```
### CRAN

```
* installing *source* package ‘EpiEstim’ ...
** package ‘EpiEstim’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘EpiEstim’
* removing ‘/tmp/workdir/EpiEstim/old/EpiEstim.Rcheck/EpiEstim’


```
# evolqg

<details>

* Version: 0.3-4
* GitHub: https://github.com/lem-usp/evolqg
* Source code: https://github.com/cran/evolqg
* Date/Publication: 2023-12-05 15:20:12 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "evolqg")` for more info

</details>

## In both

*   checking whether package ‘evolqg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/evolqg/new/evolqg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘evolqg’ ...
** package ‘evolqg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c fast_RS.cpp -o fast_RS.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o evolqg.so RcppExports.o fast_RS.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/evolqg/new/evolqg.Rcheck/00LOCK-evolqg/00new/evolqg/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘evolqg’
* removing ‘/tmp/workdir/evolqg/new/evolqg.Rcheck/evolqg’


```
### CRAN

```
* installing *source* package ‘evolqg’ ...
** package ‘evolqg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c fast_RS.cpp -o fast_RS.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o evolqg.so RcppExports.o fast_RS.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/evolqg/old/evolqg.Rcheck/00LOCK-evolqg/00new/evolqg/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘evolqg’
* removing ‘/tmp/workdir/evolqg/old/evolqg.Rcheck/evolqg’


```
# ForecastComb

<details>

* Version: 1.3.1
* GitHub: https://github.com/ceweiss/ForecastComb
* Source code: https://github.com/cran/ForecastComb
* Date/Publication: 2018-08-07 13:50:08 UTC
* Number of recursive dependencies: 74

Run `revdepcheck::cloud_details(, "ForecastComb")` for more info

</details>

## In both

*   checking whether package ‘ForecastComb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ForecastComb/new/ForecastComb.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ForecastComb’ ...
** package ‘ForecastComb’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ForecastComb’
* removing ‘/tmp/workdir/ForecastComb/new/ForecastComb.Rcheck/ForecastComb’


```
### CRAN

```
* installing *source* package ‘ForecastComb’ ...
** package ‘ForecastComb’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ForecastComb’
* removing ‘/tmp/workdir/ForecastComb/old/ForecastComb.Rcheck/ForecastComb’


```
# gapfill

<details>

* Version: 0.9.6-1
* GitHub: https://github.com/florafauna/gapfill
* Source code: https://github.com/cran/gapfill
* Date/Publication: 2021-02-12 10:10:05 UTC
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "gapfill")` for more info

</details>

## In both

*   checking whether package ‘gapfill’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/gapfill/new/gapfill.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      'raster', 'doParallel', 'doMPI'
    ```

## Installation

### Devel

```
* installing *source* package ‘gapfill’ ...
** package ‘gapfill’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c gapfill.cpp -o gapfill.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o gapfill.so RcppExports.o gapfill.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/gapfill/new/gapfill.Rcheck/00LOCK-gapfill/00new/gapfill/libs
** R
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘gapfill’
* removing ‘/tmp/workdir/gapfill/new/gapfill.Rcheck/gapfill’


```
### CRAN

```
* installing *source* package ‘gapfill’ ...
** package ‘gapfill’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c gapfill.cpp -o gapfill.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o gapfill.so RcppExports.o gapfill.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/gapfill/old/gapfill.Rcheck/00LOCK-gapfill/00new/gapfill/libs
** R
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘gapfill’
* removing ‘/tmp/workdir/gapfill/old/gapfill.Rcheck/gapfill’


```
# GeomComb

<details>

* Version: 1.0
* GitHub: https://github.com/ceweiss/GeomComb
* Source code: https://github.com/cran/GeomComb
* Date/Publication: 2016-11-27 16:02:26
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "GeomComb")` for more info

</details>

## In both

*   checking whether package ‘GeomComb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/GeomComb/new/GeomComb.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘GeomComb’ ...
** package ‘GeomComb’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘GeomComb’
* removing ‘/tmp/workdir/GeomComb/new/GeomComb.Rcheck/GeomComb’


```
### CRAN

```
* installing *source* package ‘GeomComb’ ...
** package ‘GeomComb’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘GeomComb’
* removing ‘/tmp/workdir/GeomComb/old/GeomComb.Rcheck/GeomComb’


```
# ggrcs

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/ggrcs
* Date/Publication: 2024-06-29 02:40:02 UTC
* Number of recursive dependencies: 78

Run `revdepcheck::cloud_details(, "ggrcs")` for more info

</details>

## In both

*   checking whether package ‘ggrcs’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggrcs/new/ggrcs.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggrcs’ ...
** package ‘ggrcs’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ggrcs’
* removing ‘/tmp/workdir/ggrcs/new/ggrcs.Rcheck/ggrcs’


```
### CRAN

```
* installing *source* package ‘ggrcs’ ...
** package ‘ggrcs’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ggrcs’
* removing ‘/tmp/workdir/ggrcs/old/ggrcs.Rcheck/ggrcs’


```
# ggrisk

<details>

* Version: 1.3
* GitHub: https://github.com/yikeshu0611/ggrisk
* Source code: https://github.com/cran/ggrisk
* Date/Publication: 2021-08-09 07:40:06 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "ggrisk")` for more info

</details>

## In both

*   checking whether package ‘ggrisk’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggrisk/new/ggrisk.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggrisk’ ...
** package ‘ggrisk’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ggrisk’
* removing ‘/tmp/workdir/ggrisk/new/ggrisk.Rcheck/ggrisk’


```
### CRAN

```
* installing *source* package ‘ggrisk’ ...
** package ‘ggrisk’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ggrisk’
* removing ‘/tmp/workdir/ggrisk/old/ggrisk.Rcheck/ggrisk’


```
# gJLS2

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/gJLS2
* Date/Publication: 2021-09-30 09:00:05 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "gJLS2")` for more info

</details>

## In both

*   checking whether package ‘gJLS2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/gJLS2/new/gJLS2.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘gJLS2’ ...
** package ‘gJLS2’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘gJLS2’
* removing ‘/tmp/workdir/gJLS2/new/gJLS2.Rcheck/gJLS2’


```
### CRAN

```
* installing *source* package ‘gJLS2’ ...
** package ‘gJLS2’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘gJLS2’
* removing ‘/tmp/workdir/gJLS2/old/gJLS2.Rcheck/gJLS2’


```
# Greg

<details>

* Version: 2.0.2
* GitHub: https://github.com/gforge/Greg
* Source code: https://github.com/cran/Greg
* Date/Publication: 2024-01-29 13:30:21 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "Greg")` for more info

</details>

## In both

*   checking whether package ‘Greg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/Greg/new/Greg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Greg’ ...
** package ‘Greg’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Greg’
* removing ‘/tmp/workdir/Greg/new/Greg.Rcheck/Greg’


```
### CRAN

```
* installing *source* package ‘Greg’ ...
** package ‘Greg’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Greg’
* removing ‘/tmp/workdir/Greg/old/Greg.Rcheck/Greg’


```
# greport

<details>

* Version: 0.7-4
* GitHub: https://github.com/harrelfe/greport
* Source code: https://github.com/cran/greport
* Date/Publication: 2023-09-02 22:20:02 UTC
* Number of recursive dependencies: 84

Run `revdepcheck::cloud_details(, "greport")` for more info

</details>

## In both

*   checking whether package ‘greport’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/greport/new/greport.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘greport’ ...
** package ‘greport’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘greport’
* removing ‘/tmp/workdir/greport/new/greport.Rcheck/greport’


```
### CRAN

```
* installing *source* package ‘greport’ ...
** package ‘greport’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘greport’
* removing ‘/tmp/workdir/greport/old/greport.Rcheck/greport’


```
# hettx

<details>

* Version: 0.1.3
* GitHub: https://github.com/bfifield/hettx
* Source code: https://github.com/cran/hettx
* Date/Publication: 2023-08-19 22:22:34 UTC
* Number of recursive dependencies: 85

Run `revdepcheck::cloud_details(, "hettx")` for more info

</details>

## In both

*   checking whether package ‘hettx’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/hettx/new/hettx.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘hettx’ ...
** package ‘hettx’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘hettx’
* removing ‘/tmp/workdir/hettx/new/hettx.Rcheck/hettx’


```
### CRAN

```
* installing *source* package ‘hettx’ ...
** package ‘hettx’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘hettx’
* removing ‘/tmp/workdir/hettx/old/hettx.Rcheck/hettx’


```
# hIRT

<details>

* Version: 0.3.0
* GitHub: https://github.com/xiangzhou09/hIRT
* Source code: https://github.com/cran/hIRT
* Date/Publication: 2020-03-26 17:10:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "hIRT")` for more info

</details>

## In both

*   checking whether package ‘hIRT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/hIRT/new/hIRT.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘hIRT’ ...
** package ‘hIRT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘hIRT’
* removing ‘/tmp/workdir/hIRT/new/hIRT.Rcheck/hIRT’


```
### CRAN

```
* installing *source* package ‘hIRT’ ...
** package ‘hIRT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘hIRT’
* removing ‘/tmp/workdir/hIRT/old/hIRT.Rcheck/hIRT’


```
# Hmsc

<details>

* Version: 3.0-13
* GitHub: https://github.com/hmsc-r/HMSC
* Source code: https://github.com/cran/Hmsc
* Date/Publication: 2022-08-11 14:10:14 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "Hmsc")` for more info

</details>

## In both

*   checking whether package ‘Hmsc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/Hmsc/new/Hmsc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Hmsc’ ...
** package ‘Hmsc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Hmsc’
* removing ‘/tmp/workdir/Hmsc/new/Hmsc.Rcheck/Hmsc’


```
### CRAN

```
* installing *source* package ‘Hmsc’ ...
** package ‘Hmsc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Hmsc’
* removing ‘/tmp/workdir/Hmsc/old/Hmsc.Rcheck/Hmsc’


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
Warning in qgamma(service_level, alpha, beta) : NaNs produced
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (inventorize)


```
# iNZightPlots

<details>

* Version: 2.15.3
* GitHub: https://github.com/iNZightVIT/iNZightPlots
* Source code: https://github.com/cran/iNZightPlots
* Date/Publication: 2023-10-14 05:00:02 UTC
* Number of recursive dependencies: 162

Run `revdepcheck::cloud_details(, "iNZightPlots")` for more info

</details>

## In both

*   checking whether package ‘iNZightPlots’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/iNZightPlots/new/iNZightPlots.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iNZightPlots’ ...
** package ‘iNZightPlots’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘iNZightPlots’
* removing ‘/tmp/workdir/iNZightPlots/new/iNZightPlots.Rcheck/iNZightPlots’


```
### CRAN

```
* installing *source* package ‘iNZightPlots’ ...
** package ‘iNZightPlots’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘iNZightPlots’
* removing ‘/tmp/workdir/iNZightPlots/old/iNZightPlots.Rcheck/iNZightPlots’


```
# iNZightRegression

<details>

* Version: 1.3.4
* GitHub: https://github.com/iNZightVIT/iNZightRegression
* Source code: https://github.com/cran/iNZightRegression
* Date/Publication: 2024-04-05 02:32:59 UTC
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "iNZightRegression")` for more info

</details>

## In both

*   checking whether package ‘iNZightRegression’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/iNZightRegression/new/iNZightRegression.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iNZightRegression’ ...
** package ‘iNZightRegression’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘iNZightRegression’
* removing ‘/tmp/workdir/iNZightRegression/new/iNZightRegression.Rcheck/iNZightRegression’


```
### CRAN

```
* installing *source* package ‘iNZightRegression’ ...
** package ‘iNZightRegression’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘iNZightRegression’
* removing ‘/tmp/workdir/iNZightRegression/old/iNZightRegression.Rcheck/iNZightRegression’


```
# IRexamples

<details>

* Version: 0.0.4
* GitHub: https://github.com/vinhdizzo/IRexamples
* Source code: https://github.com/cran/IRexamples
* Date/Publication: 2023-10-06 06:40:02 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::cloud_details(, "IRexamples")` for more info

</details>

## In both

*   checking whether package ‘IRexamples’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/IRexamples/new/IRexamples.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘IRexamples’ ...
** package ‘IRexamples’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘IRexamples’
* removing ‘/tmp/workdir/IRexamples/new/IRexamples.Rcheck/IRexamples’


```
### CRAN

```
* installing *source* package ‘IRexamples’ ...
** package ‘IRexamples’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘IRexamples’
* removing ‘/tmp/workdir/IRexamples/old/IRexamples.Rcheck/IRexamples’


```
# jmBIG

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/jmBIG
* Date/Publication: 2024-03-20 23:40:02 UTC
* Number of recursive dependencies: 184

Run `revdepcheck::cloud_details(, "jmBIG")` for more info

</details>

## In both

*   checking whether package ‘jmBIG’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/jmBIG/new/jmBIG.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘jmBIG’ ...
** package ‘jmBIG’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘jmBIG’
* removing ‘/tmp/workdir/jmBIG/new/jmBIG.Rcheck/jmBIG’


```
### CRAN

```
* installing *source* package ‘jmBIG’ ...
** package ‘jmBIG’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘jmBIG’
* removing ‘/tmp/workdir/jmBIG/old/jmBIG.Rcheck/jmBIG’


```
# joineRML

<details>

* Version: 0.4.6
* GitHub: https://github.com/graemeleehickey/joineRML
* Source code: https://github.com/cran/joineRML
* Date/Publication: 2023-01-20 04:50:02 UTC
* Number of recursive dependencies: 91

Run `revdepcheck::cloud_details(, "joineRML")` for more info

</details>

## In both

*   checking whether package ‘joineRML’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/joineRML/new/joineRML.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘joineRML’ ...
** package ‘joineRML’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c expW.cpp -o expW.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c gammaUpdate.cpp -o gammaUpdate.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘joineRML’
* removing ‘/tmp/workdir/joineRML/new/joineRML.Rcheck/joineRML’


```
### CRAN

```
* installing *source* package ‘joineRML’ ...
** package ‘joineRML’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c expW.cpp -o expW.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c gammaUpdate.cpp -o gammaUpdate.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘joineRML’
* removing ‘/tmp/workdir/joineRML/old/joineRML.Rcheck/joineRML’


```
# JWileymisc

<details>

* Version: 1.4.1
* GitHub: https://github.com/JWiley/JWileymisc
* Source code: https://github.com/cran/JWileymisc
* Date/Publication: 2023-10-05 04:50:02 UTC
* Number of recursive dependencies: 167

Run `revdepcheck::cloud_details(, "JWileymisc")` for more info

</details>

## In both

*   checking whether package ‘JWileymisc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/JWileymisc/new/JWileymisc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘JWileymisc’ ...
** package ‘JWileymisc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘JWileymisc’
* removing ‘/tmp/workdir/JWileymisc/new/JWileymisc.Rcheck/JWileymisc’


```
### CRAN

```
* installing *source* package ‘JWileymisc’ ...
** package ‘JWileymisc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘JWileymisc’
* removing ‘/tmp/workdir/JWileymisc/old/JWileymisc.Rcheck/JWileymisc’


```
# kmc

<details>

* Version: 0.4-2
* GitHub: https://github.com/yfyang86/kmc
* Source code: https://github.com/cran/kmc
* Date/Publication: 2022-11-22 08:30:02 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "kmc")` for more info

</details>

## In both

*   checking whether package ‘kmc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/kmc/new/kmc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘kmc’ ...
** package ‘kmc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExport.cpp -o RcppExport.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c kmc.cpp -o kmc.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c kmc_init.c -o kmc_init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c surv2.c -o surv2.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o kmc.so RcppExport.o kmc.o kmc_init.o surv2.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/kmc/new/kmc.Rcheck/00LOCK-kmc/00new/kmc/libs
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘emplik’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘kmc’
* removing ‘/tmp/workdir/kmc/new/kmc.Rcheck/kmc’


```
### CRAN

```
* installing *source* package ‘kmc’ ...
** package ‘kmc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExport.cpp -o RcppExport.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c kmc.cpp -o kmc.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c kmc_init.c -o kmc_init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c surv2.c -o surv2.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o kmc.so RcppExport.o kmc.o kmc_init.o surv2.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/kmc/old/kmc.Rcheck/00LOCK-kmc/00new/kmc/libs
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘emplik’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘kmc’
* removing ‘/tmp/workdir/kmc/old/kmc.Rcheck/kmc’


```
# L2E

<details>

* Version: 2.0
* GitHub: NA
* Source code: https://github.com/cran/L2E
* Date/Publication: 2022-09-08 21:13:00 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::cloud_details(, "L2E")` for more info

</details>

## In both

*   checking whether package ‘L2E’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/L2E/new/L2E.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘L2E’ ...
** package ‘L2E’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘osqp’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.1 is required
Execution halted
ERROR: lazy loading failed for package ‘L2E’
* removing ‘/tmp/workdir/L2E/new/L2E.Rcheck/L2E’


```
### CRAN

```
* installing *source* package ‘L2E’ ...
** package ‘L2E’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘osqp’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.1 is required
Execution halted
ERROR: lazy loading failed for package ‘L2E’
* removing ‘/tmp/workdir/L2E/old/L2E.Rcheck/L2E’


```
# llbayesireg

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/llbayesireg
* Date/Publication: 2019-04-04 16:20:03 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "llbayesireg")` for more info

</details>

## In both

*   checking whether package ‘llbayesireg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/llbayesireg/new/llbayesireg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘llbayesireg’ ...
** package ‘llbayesireg’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘llbayesireg’
* removing ‘/tmp/workdir/llbayesireg/new/llbayesireg.Rcheck/llbayesireg’


```
### CRAN

```
* installing *source* package ‘llbayesireg’ ...
** package ‘llbayesireg’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘llbayesireg’
* removing ‘/tmp/workdir/llbayesireg/old/llbayesireg.Rcheck/llbayesireg’


```
# LorenzRegression

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/LorenzRegression
* Date/Publication: 2023-02-28 17:32:34 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "LorenzRegression")` for more info

</details>

## In both

*   checking whether package ‘LorenzRegression’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/LorenzRegression/new/LorenzRegression.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘LorenzRegression’ ...
** package ‘LorenzRegression’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c GA_fitness.cpp -o GA_fitness.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c GA_meanrank.cpp -o GA_meanrank.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c PLR_derivative.cpp -o PLR_derivative.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c PLR_loss.cpp -o PLR_loss.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘LorenzRegression’
* removing ‘/tmp/workdir/LorenzRegression/new/LorenzRegression.Rcheck/LorenzRegression’


```
### CRAN

```
* installing *source* package ‘LorenzRegression’ ...
** package ‘LorenzRegression’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c GA_fitness.cpp -o GA_fitness.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c GA_meanrank.cpp -o GA_meanrank.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c PLR_derivative.cpp -o PLR_derivative.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c PLR_loss.cpp -o PLR_loss.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘LorenzRegression’
* removing ‘/tmp/workdir/LorenzRegression/old/LorenzRegression.Rcheck/LorenzRegression’


```
# lsirm12pl

<details>

* Version: 1.3.1
* GitHub: NA
* Source code: https://github.com/cran/lsirm12pl
* Date/Publication: 2023-06-22 14:12:35 UTC
* Number of recursive dependencies: 124

Run `revdepcheck::cloud_details(, "lsirm12pl")` for more info

</details>

## In both

*   checking whether package ‘lsirm12pl’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/lsirm12pl/new/lsirm12pl.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘lsirm12pl’ ...
** package ‘lsirm12pl’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c log_likelihood.cpp -o log_likelihood.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm1pl.cpp -o lsirm1pl.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm1pl_fixed_gamma.cpp -o lsirm1pl_fixed_gamma.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm1pl_fixed_gamma_mar.cpp -o lsirm1pl_fixed_gamma_mar.o
...
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘lsirm12pl’
* removing ‘/tmp/workdir/lsirm12pl/new/lsirm12pl.Rcheck/lsirm12pl’


```
### CRAN

```
* installing *source* package ‘lsirm12pl’ ...
** package ‘lsirm12pl’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c log_likelihood.cpp -o log_likelihood.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm1pl.cpp -o lsirm1pl.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm1pl_fixed_gamma.cpp -o lsirm1pl_fixed_gamma.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm1pl_fixed_gamma_mar.cpp -o lsirm1pl_fixed_gamma_mar.o
...
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘lsirm12pl’
* removing ‘/tmp/workdir/lsirm12pl/old/lsirm12pl.Rcheck/lsirm12pl’


```
# mbsts

<details>

* Version: 3.0
* GitHub: NA
* Source code: https://github.com/cran/mbsts
* Date/Publication: 2023-01-07 01:10:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "mbsts")` for more info

</details>

## In both

*   checking whether package ‘mbsts’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mbsts/new/mbsts.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mbsts’ ...
** package ‘mbsts’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘mbsts’
* removing ‘/tmp/workdir/mbsts/new/mbsts.Rcheck/mbsts’


```
### CRAN

```
* installing *source* package ‘mbsts’ ...
** package ‘mbsts’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘mbsts’
* removing ‘/tmp/workdir/mbsts/old/mbsts.Rcheck/mbsts’


```
# MendelianRandomization

<details>

* Version: 0.10.0
* GitHub: NA
* Source code: https://github.com/cran/MendelianRandomization
* Date/Publication: 2024-04-12 10:10:02 UTC
* Number of recursive dependencies: 88

Run `revdepcheck::cloud_details(, "MendelianRandomization")` for more info

</details>

## In both

*   checking whether package ‘MendelianRandomization’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/MendelianRandomization/new/MendelianRandomization.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MendelianRandomization’ ...
** package ‘MendelianRandomization’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c mvmrcML.cpp -o mvmrcML.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o MendelianRandomization.so RcppExports.o mvmrcML.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/MendelianRandomization/new/MendelianRandomization.Rcheck/00LOCK-MendelianRandomization/00new/MendelianRandomization/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘MendelianRandomization’
* removing ‘/tmp/workdir/MendelianRandomization/new/MendelianRandomization.Rcheck/MendelianRandomization’


```
### CRAN

```
* installing *source* package ‘MendelianRandomization’ ...
** package ‘MendelianRandomization’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c mvmrcML.cpp -o mvmrcML.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o MendelianRandomization.so RcppExports.o mvmrcML.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/MendelianRandomization/old/MendelianRandomization.Rcheck/00LOCK-MendelianRandomization/00new/MendelianRandomization/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘MendelianRandomization’
* removing ‘/tmp/workdir/MendelianRandomization/old/MendelianRandomization.Rcheck/MendelianRandomization’


```
# MetabolicSurv

<details>

* Version: 1.1.2
* GitHub: https://github.com/OlajumokeEvangelina/MetabolicSurv
* Source code: https://github.com/cran/MetabolicSurv
* Date/Publication: 2021-06-11 08:30:02 UTC
* Number of recursive dependencies: 131

Run `revdepcheck::cloud_details(, "MetabolicSurv")` for more info

</details>

## In both

*   checking whether package ‘MetabolicSurv’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/MetabolicSurv/new/MetabolicSurv.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MetabolicSurv’ ...
** package ‘MetabolicSurv’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘MetabolicSurv’
* removing ‘/tmp/workdir/MetabolicSurv/new/MetabolicSurv.Rcheck/MetabolicSurv’


```
### CRAN

```
* installing *source* package ‘MetabolicSurv’ ...
** package ‘MetabolicSurv’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘MetabolicSurv’
* removing ‘/tmp/workdir/MetabolicSurv/old/MetabolicSurv.Rcheck/MetabolicSurv’


```
# miWQS

<details>

* Version: 0.4.4
* GitHub: https://github.com/phargarten2/miWQS
* Source code: https://github.com/cran/miWQS
* Date/Publication: 2021-04-02 21:50:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "miWQS")` for more info

</details>

## In both

*   checking whether package ‘miWQS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/miWQS/new/miWQS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘miWQS’ ...
** package ‘miWQS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘miWQS’
* removing ‘/tmp/workdir/miWQS/new/miWQS.Rcheck/miWQS’


```
### CRAN

```
* installing *source* package ‘miWQS’ ...
** package ‘miWQS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘miWQS’
* removing ‘/tmp/workdir/miWQS/old/miWQS.Rcheck/miWQS’


```
# MRZero

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/MRZero
* Date/Publication: 2024-04-14 09:30:03 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "MRZero")` for more info

</details>

## In both

*   checking whether package ‘MRZero’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/MRZero/new/MRZero.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MRZero’ ...
** package ‘MRZero’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘MRZero’
* removing ‘/tmp/workdir/MRZero/new/MRZero.Rcheck/MRZero’


```
### CRAN

```
* installing *source* package ‘MRZero’ ...
** package ‘MRZero’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘MRZero’
* removing ‘/tmp/workdir/MRZero/old/MRZero.Rcheck/MRZero’


```
# Multiaovbay

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/Multiaovbay
* Date/Publication: 2023-03-17 17:20:02 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "Multiaovbay")` for more info

</details>

## In both

*   checking whether package ‘Multiaovbay’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/Multiaovbay/new/Multiaovbay.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Multiaovbay’ ...
** package ‘Multiaovbay’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Multiaovbay’
* removing ‘/tmp/workdir/Multiaovbay/new/Multiaovbay.Rcheck/Multiaovbay’


```
### CRAN

```
* installing *source* package ‘Multiaovbay’ ...
** package ‘Multiaovbay’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Multiaovbay’
* removing ‘/tmp/workdir/Multiaovbay/old/Multiaovbay.Rcheck/Multiaovbay’


```
# multilevelTools

<details>

* Version: 0.1.1
* GitHub: https://github.com/JWiley/multilevelTools
* Source code: https://github.com/cran/multilevelTools
* Date/Publication: 2020-03-04 09:50:02 UTC
* Number of recursive dependencies: 168

Run `revdepcheck::cloud_details(, "multilevelTools")` for more info

</details>

## In both

*   checking whether package ‘multilevelTools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/multilevelTools/new/multilevelTools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘multilevelTools’ ...
** package ‘multilevelTools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘multilevelTools’
* removing ‘/tmp/workdir/multilevelTools/new/multilevelTools.Rcheck/multilevelTools’


```
### CRAN

```
* installing *source* package ‘multilevelTools’ ...
** package ‘multilevelTools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘multilevelTools’
* removing ‘/tmp/workdir/multilevelTools/old/multilevelTools.Rcheck/multilevelTools’


```
# multinma

<details>

* Version: 0.7.1
* GitHub: https://github.com/dmphillippo/multinma
* Source code: https://github.com/cran/multinma
* Date/Publication: 2024-06-11 12:20:06 UTC
* Number of recursive dependencies: 152

Run `revdepcheck::cloud_details(, "multinma")` for more info

</details>

## In both

*   checking whether package ‘multinma’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/multinma/new/multinma.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘multinma’ ...
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_survival_mspline_namespace::model_survival_mspline; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:10:   required from here
/opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_survival_mspline.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/new/multinma.Rcheck/multinma’


```
### CRAN

```
* installing *source* package ‘multinma’ ...
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/opt/R/4.3.1/lib/R/site-library/BH/include' -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -I'/opt/R/4.3.1/lib/R/site-library/rstan/include' -I'/opt/R/4.3.1/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/opt/R/4.3.1/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_survival_mspline_namespace::model_survival_mspline; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.3.1/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:10:   required from here
/opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_survival_mspline.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/old/multinma.Rcheck/multinma’


```
# NCA

<details>

* Version: 4.0.1
* GitHub: NA
* Source code: https://github.com/cran/NCA
* Date/Publication: 2024-02-23 09:30:15 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "NCA")` for more info

</details>

## In both

*   checking whether package ‘NCA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/NCA/new/NCA.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘NCA’ ...
** package ‘NCA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘NCA’
* removing ‘/tmp/workdir/NCA/new/NCA.Rcheck/NCA’


```
### CRAN

```
* installing *source* package ‘NCA’ ...
** package ‘NCA’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘NCA’
* removing ‘/tmp/workdir/NCA/old/NCA.Rcheck/NCA’


```
# netcmc

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/netcmc
* Date/Publication: 2022-11-08 22:30:15 UTC
* Number of recursive dependencies: 61

Run `revdepcheck::cloud_details(, "netcmc")` for more info

</details>

## In both

*   checking whether package ‘netcmc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/netcmc/new/netcmc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘netcmc’ ...
** package ‘netcmc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c choleskyDecompositionRcppConversion.cpp -o choleskyDecompositionRcppConversion.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c doubleMatrixMultiplicationRcpp.cpp -o doubleMatrixMultiplicationRcpp.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c doubleVectorMultiplicationRcpp.cpp -o doubleVectorMultiplicationRcpp.o
...
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c vectorVectorTransposeMultiplicationRcpp.cpp -o vectorVectorTransposeMultiplicationRcpp.o
g++ -std=gnu++11 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o netcmc.so RcppExports.o choleskyDecompositionRcppConversion.o doubleMatrixMultiplicationRcpp.o doubleVectorMultiplicationRcpp.o eigenValuesRcppConversion.o getDiagonalMatrix.o getExp.o getExpDividedByOnePlusExp.o getMeanCenteredRandomEffects.o getMultivariateBinomialNetworkLerouxDIC.o getMultivariateBinomialNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getMultivariateGaussianNetworkLerouxDIC.o getMultivariateGaussianNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getMultivariatePoissonNetworkLerouxDIC.o getMultivariatePoissonNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getNonZeroEntries.o getSubvector.o getSubvectorIndecies.o getSumExpNetwork.o getSumExpNetworkIndecies.o getSumExpNetworkLeroux.o getSumExpNetworkLerouxIndecies.o getSumLogExp.o getSumLogExpIndecies.o getSumVector.o getTripletForm.o getUnivariateBinomialNetworkLerouxDIC.o getUnivariateBinomialNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getUnivariateGaussianNetworkLerouxDIC.o getUnivariateGaussianNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getUnivariatePoissonNetworkDIC.o getUnivariatePoissonNetworkFittedValuesAndLikelihoodForDICEveryIteration.o getUnivariatePoissonNetworkLerouxDIC.o getUnivariatePoissonNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getVectorMean.o matrixInverseRcppConversion.o matrixMatrixAdditionRcpp.o matrixMatrixSubtractionRcpp.o matrixVectorMultiplicationRcpp.o multivariateBinomialNetworkLerouxAllUpdate.o multivariateBinomialNetworkLerouxBetaUpdate.o multivariateBinomialNetworkLerouxRhoUpdate.o multivariateBinomialNetworkLerouxSingleUpdate.o multivariateBinomialNetworkLerouxSpatialRandomEffectsUpdate.o multivariateBinomialNetworkLerouxTauSquaredUpdate.o multivariateBinomialNetworkLerouxURandomEffectsUpdate.o multivariateBinomialNetworkLerouxVRandomEffectsUpdate.o multivariateBinomialNetworkLerouxVarianceCovarianceUUpdate.o multivariateBinomialNetworkRandAllUpdate.o multivariateBinomialNetworkRandSingleUpdate.o multivariateGaussianNetworkLerouxAllMHUpdate.o multivariateGaussianNetworkLerouxBetaUpdate.o multivariateGaussianNetworkLerouxRhoUpdate.o multivariateGaussianNetworkLerouxSigmaSquaredEUpdate.o multivariateGaussianNetworkLerouxSingleMHUpdate.o multivariateGaussianNetworkLerouxSpatialRandomEffectsMHUpdate.o multivariateGaussianNetworkLerouxTauSquaredUpdate.o multivariateGaussianNetworkLerouxURandomEffectsUpdate.o multivariateGaussianNetworkLerouxVarianceCovarianceUUpdate.o multivariateGaussianNetworkRandAllUpdate.o multivariateGaussianNetworkRandSingleUpdate.o multivariateGaussianNetworkRandVRandomEffectsUpdate.o multivariatePoissonNetworkLerouxAllUpdate.o multivariatePoissonNetworkLerouxBetaUpdate.o multivariatePoissonNetworkLerouxRhoUpdate.o multivariatePoissonNetworkLerouxSingleUpdate.o multivariatePoissonNetworkLerouxSpatialRandomEffectsUpdate.o multivariatePoissonNetworkLerouxTauSquaredUpdate.o multivariatePoissonNetworkLerouxURandomEffectsUpdate.o multivariatePoissonNetworkLerouxVRandomEffectsUpdate.o multivariatePoissonNetworkLerouxVarianceCovarianceUUpdate.o multivariatePoissonNetworkRandAllUpdate.o multivariatePoissonNetworkRandSingleUpdate.o sumMatrix.o univariateBinomialNetworkLerouxAllUpdate.o univariateBinomialNetworkLerouxBetaUpdate.o univariateBinomialNetworkLerouxRhoUpdate.o univariateBinomialNetworkLerouxSigmaSquaredUpdate.o univariateBinomialNetworkLerouxSingleUpdate.o univariateBinomialNetworkLerouxSpatialRandomEffectsUpdate.o univariateBinomialNetworkLerouxTauSquaredUpdate.o univariateBinomialNetworkLerouxURandomEffectsUpdate.o univariateGaussianNetworkLerouxAllMHUpdate.o univariateGaussianNetworkLerouxBetaUpdate.o univariateGaussianNetworkLerouxRhoUpdate.o univariateGaussianNetworkLerouxSigmaSquaredEUpdate.o univariateGaussianNetworkLerouxSigmaSquaredUUpdate.o univariateGaussianNetworkLerouxSingleMHUpdate.o univariateGaussianNetworkLerouxSpatialRandomEffectsMHUpdate.o univariateGaussianNetworkLerouxTauSquaredUpdate.o univariateGaussianNetworkLerouxURandomEffectsUpdate.o univariatePoissonNetworkLerouxAllUpdate.o univariatePoissonNetworkLerouxBetaUpdate.o univariatePoissonNetworkLerouxRhoUpdate.o univariatePoissonNetworkLerouxSigmaSquaredUpdate.o univariatePoissonNetworkLerouxSingleUpdate.o univariatePoissonNetworkLerouxSpatialRandomEffectsUpdate.o univariatePoissonNetworkLerouxTauSquaredUpdate.o univariatePoissonNetworkLerouxURandomEffectsUpdate.o vectorTransposeVectorMultiplicationRcpp.o vectorVectorTransposeMultiplicationRcpp.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/netcmc/new/netcmc.Rcheck/00LOCK-netcmc/00new/netcmc/libs
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘MCMCpack’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘netcmc’
* removing ‘/tmp/workdir/netcmc/new/netcmc.Rcheck/netcmc’


```
### CRAN

```
* installing *source* package ‘netcmc’ ...
** package ‘netcmc’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c choleskyDecompositionRcppConversion.cpp -o choleskyDecompositionRcppConversion.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c doubleMatrixMultiplicationRcpp.cpp -o doubleMatrixMultiplicationRcpp.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c doubleVectorMultiplicationRcpp.cpp -o doubleVectorMultiplicationRcpp.o
...
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c vectorVectorTransposeMultiplicationRcpp.cpp -o vectorVectorTransposeMultiplicationRcpp.o
g++ -std=gnu++11 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o netcmc.so RcppExports.o choleskyDecompositionRcppConversion.o doubleMatrixMultiplicationRcpp.o doubleVectorMultiplicationRcpp.o eigenValuesRcppConversion.o getDiagonalMatrix.o getExp.o getExpDividedByOnePlusExp.o getMeanCenteredRandomEffects.o getMultivariateBinomialNetworkLerouxDIC.o getMultivariateBinomialNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getMultivariateGaussianNetworkLerouxDIC.o getMultivariateGaussianNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getMultivariatePoissonNetworkLerouxDIC.o getMultivariatePoissonNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getNonZeroEntries.o getSubvector.o getSubvectorIndecies.o getSumExpNetwork.o getSumExpNetworkIndecies.o getSumExpNetworkLeroux.o getSumExpNetworkLerouxIndecies.o getSumLogExp.o getSumLogExpIndecies.o getSumVector.o getTripletForm.o getUnivariateBinomialNetworkLerouxDIC.o getUnivariateBinomialNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getUnivariateGaussianNetworkLerouxDIC.o getUnivariateGaussianNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getUnivariatePoissonNetworkDIC.o getUnivariatePoissonNetworkFittedValuesAndLikelihoodForDICEveryIteration.o getUnivariatePoissonNetworkLerouxDIC.o getUnivariatePoissonNetworkLerouxFittedValuesAndLikelihoodForDICEveryIteration.o getVectorMean.o matrixInverseRcppConversion.o matrixMatrixAdditionRcpp.o matrixMatrixSubtractionRcpp.o matrixVectorMultiplicationRcpp.o multivariateBinomialNetworkLerouxAllUpdate.o multivariateBinomialNetworkLerouxBetaUpdate.o multivariateBinomialNetworkLerouxRhoUpdate.o multivariateBinomialNetworkLerouxSingleUpdate.o multivariateBinomialNetworkLerouxSpatialRandomEffectsUpdate.o multivariateBinomialNetworkLerouxTauSquaredUpdate.o multivariateBinomialNetworkLerouxURandomEffectsUpdate.o multivariateBinomialNetworkLerouxVRandomEffectsUpdate.o multivariateBinomialNetworkLerouxVarianceCovarianceUUpdate.o multivariateBinomialNetworkRandAllUpdate.o multivariateBinomialNetworkRandSingleUpdate.o multivariateGaussianNetworkLerouxAllMHUpdate.o multivariateGaussianNetworkLerouxBetaUpdate.o multivariateGaussianNetworkLerouxRhoUpdate.o multivariateGaussianNetworkLerouxSigmaSquaredEUpdate.o multivariateGaussianNetworkLerouxSingleMHUpdate.o multivariateGaussianNetworkLerouxSpatialRandomEffectsMHUpdate.o multivariateGaussianNetworkLerouxTauSquaredUpdate.o multivariateGaussianNetworkLerouxURandomEffectsUpdate.o multivariateGaussianNetworkLerouxVarianceCovarianceUUpdate.o multivariateGaussianNetworkRandAllUpdate.o multivariateGaussianNetworkRandSingleUpdate.o multivariateGaussianNetworkRandVRandomEffectsUpdate.o multivariatePoissonNetworkLerouxAllUpdate.o multivariatePoissonNetworkLerouxBetaUpdate.o multivariatePoissonNetworkLerouxRhoUpdate.o multivariatePoissonNetworkLerouxSingleUpdate.o multivariatePoissonNetworkLerouxSpatialRandomEffectsUpdate.o multivariatePoissonNetworkLerouxTauSquaredUpdate.o multivariatePoissonNetworkLerouxURandomEffectsUpdate.o multivariatePoissonNetworkLerouxVRandomEffectsUpdate.o multivariatePoissonNetworkLerouxVarianceCovarianceUUpdate.o multivariatePoissonNetworkRandAllUpdate.o multivariatePoissonNetworkRandSingleUpdate.o sumMatrix.o univariateBinomialNetworkLerouxAllUpdate.o univariateBinomialNetworkLerouxBetaUpdate.o univariateBinomialNetworkLerouxRhoUpdate.o univariateBinomialNetworkLerouxSigmaSquaredUpdate.o univariateBinomialNetworkLerouxSingleUpdate.o univariateBinomialNetworkLerouxSpatialRandomEffectsUpdate.o univariateBinomialNetworkLerouxTauSquaredUpdate.o univariateBinomialNetworkLerouxURandomEffectsUpdate.o univariateGaussianNetworkLerouxAllMHUpdate.o univariateGaussianNetworkLerouxBetaUpdate.o univariateGaussianNetworkLerouxRhoUpdate.o univariateGaussianNetworkLerouxSigmaSquaredEUpdate.o univariateGaussianNetworkLerouxSigmaSquaredUUpdate.o univariateGaussianNetworkLerouxSingleMHUpdate.o univariateGaussianNetworkLerouxSpatialRandomEffectsMHUpdate.o univariateGaussianNetworkLerouxTauSquaredUpdate.o univariateGaussianNetworkLerouxURandomEffectsUpdate.o univariatePoissonNetworkLerouxAllUpdate.o univariatePoissonNetworkLerouxBetaUpdate.o univariatePoissonNetworkLerouxRhoUpdate.o univariatePoissonNetworkLerouxSigmaSquaredUpdate.o univariatePoissonNetworkLerouxSingleUpdate.o univariatePoissonNetworkLerouxSpatialRandomEffectsUpdate.o univariatePoissonNetworkLerouxTauSquaredUpdate.o univariatePoissonNetworkLerouxURandomEffectsUpdate.o vectorTransposeVectorMultiplicationRcpp.o vectorVectorTransposeMultiplicationRcpp.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/netcmc/old/netcmc.Rcheck/00LOCK-netcmc/00new/netcmc/libs
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘MCMCpack’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘netcmc’
* removing ‘/tmp/workdir/netcmc/old/netcmc.Rcheck/netcmc’


```
# NetworkChange

<details>

* Version: 0.8
* GitHub: https://github.com/jongheepark/NetworkChange
* Source code: https://github.com/cran/NetworkChange
* Date/Publication: 2022-03-04 07:30:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "NetworkChange")` for more info

</details>

## In both

*   checking whether package ‘NetworkChange’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/NetworkChange/new/NetworkChange.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘NetworkChange’ ...
** package ‘NetworkChange’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘MCMCpack’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘NetworkChange’
* removing ‘/tmp/workdir/NetworkChange/new/NetworkChange.Rcheck/NetworkChange’


```
### CRAN

```
* installing *source* package ‘NetworkChange’ ...
** package ‘NetworkChange’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘MCMCpack’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘NetworkChange’
* removing ‘/tmp/workdir/NetworkChange/old/NetworkChange.Rcheck/NetworkChange’


```
# nlmeVPC

<details>

* Version: 2.6
* GitHub: NA
* Source code: https://github.com/cran/nlmeVPC
* Date/Publication: 2022-12-22 05:20:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "nlmeVPC")` for more info

</details>

## In both

*   checking whether package ‘nlmeVPC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/nlmeVPC/new/nlmeVPC.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘nlmeVPC’ ...
** package ‘nlmeVPC’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c Misc.cpp -o Misc.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o nlmeVPC.so Misc.o RcppExports.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/nlmeVPC/new/nlmeVPC.Rcheck/00LOCK-nlmeVPC/00new/nlmeVPC/libs
** R
** data
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘nlmeVPC’
* removing ‘/tmp/workdir/nlmeVPC/new/nlmeVPC.Rcheck/nlmeVPC’


```
### CRAN

```
* installing *source* package ‘nlmeVPC’ ...
** package ‘nlmeVPC’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c Misc.cpp -o Misc.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o nlmeVPC.so Misc.o RcppExports.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/nlmeVPC/old/nlmeVPC.Rcheck/00LOCK-nlmeVPC/00new/nlmeVPC/libs
** R
** data
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘nlmeVPC’
* removing ‘/tmp/workdir/nlmeVPC/old/nlmeVPC.Rcheck/nlmeVPC’


```
# NMADiagT

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/NMADiagT
* Date/Publication: 2020-02-26 07:00:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "NMADiagT")` for more info

</details>

## In both

*   checking whether package ‘NMADiagT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/NMADiagT/new/NMADiagT.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘NMADiagT’ ...
** package ‘NMADiagT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘NMADiagT’
* removing ‘/tmp/workdir/NMADiagT/new/NMADiagT.Rcheck/NMADiagT’


```
### CRAN

```
* installing *source* package ‘NMADiagT’ ...
** package ‘NMADiagT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘NMADiagT’
* removing ‘/tmp/workdir/NMADiagT/old/NMADiagT.Rcheck/NMADiagT’


```
# optweight

<details>

* Version: 0.2.5
* GitHub: NA
* Source code: https://github.com/cran/optweight
* Date/Publication: 2019-09-16 15:40:02 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::cloud_details(, "optweight")` for more info

</details>

## In both

*   checking whether package ‘optweight’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/optweight/new/optweight.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘optweight’ ...
** package ‘optweight’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.1 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘optweight’
* removing ‘/tmp/workdir/optweight/new/optweight.Rcheck/optweight’


```
### CRAN

```
* installing *source* package ‘optweight’ ...
** package ‘optweight’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.1 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘optweight’
* removing ‘/tmp/workdir/optweight/old/optweight.Rcheck/optweight’


```
# OVtool

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/OVtool
* Date/Publication: 2021-11-02 08:10:07 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "OVtool")` for more info

</details>

## In both

*   checking whether package ‘OVtool’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/OVtool/new/OVtool.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘OVtool’ ...
** package ‘OVtool’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘twang’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘OVtool’
* removing ‘/tmp/workdir/OVtool/new/OVtool.Rcheck/OVtool’


```
### CRAN

```
* installing *source* package ‘OVtool’ ...
** package ‘OVtool’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘twang’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘OVtool’
* removing ‘/tmp/workdir/OVtool/old/OVtool.Rcheck/OVtool’


```
# paths

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/paths
* Date/Publication: 2021-06-18 08:40:02 UTC
* Number of recursive dependencies: 102

Run `revdepcheck::cloud_details(, "paths")` for more info

</details>

## In both

*   checking whether package ‘paths’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/paths/new/paths.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘paths’ ...
** package ‘paths’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘paths’
* removing ‘/tmp/workdir/paths/new/paths.Rcheck/paths’


```
### CRAN

```
* installing *source* package ‘paths’ ...
** package ‘paths’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘paths’
* removing ‘/tmp/workdir/paths/old/paths.Rcheck/paths’


```
# PLMIX

<details>

* Version: 2.1.1
* GitHub: NA
* Source code: https://github.com/cran/PLMIX
* Date/Publication: 2019-09-04 11:50:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "PLMIX")` for more info

</details>

## In both

*   checking whether package ‘PLMIX’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/PLMIX/new/PLMIX.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘PLMIX’ ...
** package ‘PLMIX’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompProbZpartial.cpp -o CompProbZpartial.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompRateP.cpp -o CompRateP.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompRateYpartial.cpp -o CompRateYpartial.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c Estep.cpp -o Estep.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c PLMIXsim.cpp -o PLMIXsim.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘PLMIX’
* removing ‘/tmp/workdir/PLMIX/new/PLMIX.Rcheck/PLMIX’


```
### CRAN

```
* installing *source* package ‘PLMIX’ ...
** package ‘PLMIX’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompProbZpartial.cpp -o CompProbZpartial.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompRateP.cpp -o CompRateP.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompRateYpartial.cpp -o CompRateYpartial.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c Estep.cpp -o Estep.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c PLMIXsim.cpp -o PLMIXsim.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘PLMIX’
* removing ‘/tmp/workdir/PLMIX/old/PLMIX.Rcheck/PLMIX’


```
# popstudy

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/popstudy
* Date/Publication: 2023-10-17 23:50:02 UTC
* Number of recursive dependencies: 240

Run `revdepcheck::cloud_details(, "popstudy")` for more info

</details>

## In both

*   checking whether package ‘popstudy’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/popstudy/new/popstudy.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘popstudy’ ...
** package ‘popstudy’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘popstudy’
* removing ‘/tmp/workdir/popstudy/new/popstudy.Rcheck/popstudy’


```
### CRAN

```
* installing *source* package ‘popstudy’ ...
** package ‘popstudy’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘popstudy’
* removing ‘/tmp/workdir/popstudy/old/popstudy.Rcheck/popstudy’


```
# pould

<details>

* Version: 1.0.1
* GitHub: NA
* Source code: https://github.com/cran/pould
* Date/Publication: 2020-10-16 13:50:03 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "pould")` for more info

</details>

## In both

*   checking whether package ‘pould’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/pould/new/pould.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘pould’ ...
** package ‘pould’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘pould’
* removing ‘/tmp/workdir/pould/new/pould.Rcheck/pould’


```
### CRAN

```
* installing *source* package ‘pould’ ...
** package ‘pould’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘pould’
* removing ‘/tmp/workdir/pould/old/pould.Rcheck/pould’


```
# powerly

<details>

* Version: 1.8.6
* GitHub: https://github.com/mihaiconstantin/powerly
* Source code: https://github.com/cran/powerly
* Date/Publication: 2022-09-09 14:10:01 UTC
* Number of recursive dependencies: 181

Run `revdepcheck::cloud_details(, "powerly")` for more info

</details>

## In both

*   checking whether package ‘powerly’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/powerly/new/powerly.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘powerly’ ...
** package ‘powerly’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.1 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘powerly’
* removing ‘/tmp/workdir/powerly/new/powerly.Rcheck/powerly’


```
### CRAN

```
* installing *source* package ‘powerly’ ...
** package ‘powerly’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.1 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘powerly’
* removing ‘/tmp/workdir/powerly/old/powerly.Rcheck/powerly’


```
# pre

<details>

* Version: 1.0.7
* GitHub: https://github.com/marjoleinF/pre
* Source code: https://github.com/cran/pre
* Date/Publication: 2024-01-12 19:30:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "pre")` for more info

</details>

## In both

*   checking whether package ‘pre’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/pre/new/pre.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘pre’ ...
** package ‘pre’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘pre’
* removing ‘/tmp/workdir/pre/new/pre.Rcheck/pre’


```
### CRAN

```
* installing *source* package ‘pre’ ...
** package ‘pre’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘pre’
* removing ‘/tmp/workdir/pre/old/pre.Rcheck/pre’


```
# ProFAST

<details>

* Version: 1.4
* GitHub: https://github.com/feiyoung/ProFAST
* Source code: https://github.com/cran/ProFAST
* Date/Publication: 2024-03-18 08:10:06 UTC
* Number of recursive dependencies: 245

Run `revdepcheck::cloud_details(, "ProFAST")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ProFAST/new/ProFAST.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
* running under: Ubuntu 22.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ProFAST/DESCRIPTION’ ... OK
...
* this is package ‘ProFAST’ version ‘1.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'DR.SC', 'PRECAST'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/ProFAST/old/ProFAST.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
* running under: Ubuntu 22.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ProFAST/DESCRIPTION’ ... OK
...
* this is package ‘ProFAST’ version ‘1.4’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'DR.SC', 'PRECAST'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# psbcSpeedUp

<details>

* Version: 2.0.7
* GitHub: https://github.com/ocbe-uio/psbcSpeedUp
* Source code: https://github.com/cran/psbcSpeedUp
* Date/Publication: 2024-07-01 09:00:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "psbcSpeedUp")` for more info

</details>

## In both

*   checking whether package ‘psbcSpeedUp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/psbcSpeedUp/new/psbcSpeedUp.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘psbcSpeedUp’ ...
** package ‘psbcSpeedUp’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether the compiler supports GNU C++... yes
checking whether g++ -std=gnu++17 accepts -g... yes
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘psbcSpeedUp’
* removing ‘/tmp/workdir/psbcSpeedUp/new/psbcSpeedUp.Rcheck/psbcSpeedUp’


```
### CRAN

```
* installing *source* package ‘psbcSpeedUp’ ...
** package ‘psbcSpeedUp’ successfully unpacked and MD5 sums checked
** using staged installation
checking whether the C++ compiler works... yes
checking for C++ compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether the compiler supports GNU C++... yes
checking whether g++ -std=gnu++17 accepts -g... yes
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘psbcSpeedUp’
* removing ‘/tmp/workdir/psbcSpeedUp/old/psbcSpeedUp.Rcheck/psbcSpeedUp’


```
# pscore

<details>

* Version: 0.4.0
* GitHub: https://github.com/JWiley/score-project
* Source code: https://github.com/cran/pscore
* Date/Publication: 2022-05-13 22:30:02 UTC
* Number of recursive dependencies: 169

Run `revdepcheck::cloud_details(, "pscore")` for more info

</details>

## In both

*   checking whether package ‘pscore’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/pscore/new/pscore.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘pscore’ ...
** package ‘pscore’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘pscore’
* removing ‘/tmp/workdir/pscore/new/pscore.Rcheck/pscore’


```
### CRAN

```
* installing *source* package ‘pscore’ ...
** package ‘pscore’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘pscore’
* removing ‘/tmp/workdir/pscore/old/pscore.Rcheck/pscore’


```
# psfmi

<details>

* Version: 1.4.0
* GitHub: https://github.com/mwheymans/psfmi
* Source code: https://github.com/cran/psfmi
* Date/Publication: 2023-06-17 22:40:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "psfmi")` for more info

</details>

## In both

*   checking whether package ‘psfmi’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/psfmi/new/psfmi.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘psfmi’ ...
** package ‘psfmi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘psfmi’
* removing ‘/tmp/workdir/psfmi/new/psfmi.Rcheck/psfmi’


```
### CRAN

```
* installing *source* package ‘psfmi’ ...
** package ‘psfmi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘psfmi’
* removing ‘/tmp/workdir/psfmi/old/psfmi.Rcheck/psfmi’


```
# qPCRtools

<details>

* Version: 1.0.1
* GitHub: https://github.com/lixiang117423/qPCRtools
* Source code: https://github.com/cran/qPCRtools
* Date/Publication: 2023-11-02 13:10:05 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "qPCRtools")` for more info

</details>

## In both

*   checking whether package ‘qPCRtools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/qPCRtools/new/qPCRtools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘qPCRtools’ ...
** package ‘qPCRtools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘qPCRtools’
* removing ‘/tmp/workdir/qPCRtools/new/qPCRtools.Rcheck/qPCRtools’


```
### CRAN

```
* installing *source* package ‘qPCRtools’ ...
** package ‘qPCRtools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘qPCRtools’
* removing ‘/tmp/workdir/qPCRtools/old/qPCRtools.Rcheck/qPCRtools’


```
# qreport

<details>

* Version: 1.0-1
* GitHub: NA
* Source code: https://github.com/cran/qreport
* Date/Publication: 2024-05-26 21:50:03 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "qreport")` for more info

</details>

## In both

*   checking whether package ‘qreport’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/qreport/new/qreport.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘qreport’ ...
** package ‘qreport’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘qreport’
* removing ‘/tmp/workdir/qreport/new/qreport.Rcheck/qreport’


```
### CRAN

```
* installing *source* package ‘qreport’ ...
** package ‘qreport’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘qreport’
* removing ‘/tmp/workdir/qreport/old/qreport.Rcheck/qreport’


```
# qris

<details>

* Version: 1.1.1
* GitHub: https://github.com/Kyuhyun07/qris
* Source code: https://github.com/cran/qris
* Date/Publication: 2024-03-05 14:40:03 UTC
* Number of recursive dependencies: 55

Run `revdepcheck::cloud_details(, "qris")` for more info

</details>

## In both

*   checking whether package ‘qris’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/qris/new/qris.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘qris’ ...
** package ‘qris’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c Amat.cpp -o Amat.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c ghat.cpp -o ghat.o
...
installing to /tmp/workdir/qris/new/qris.Rcheck/00LOCK-qris/00new/qris/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘qris’
* removing ‘/tmp/workdir/qris/new/qris.Rcheck/qris’


```
### CRAN

```
* installing *source* package ‘qris’ ...
** package ‘qris’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c Amat.cpp -o Amat.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c ghat.cpp -o ghat.o
...
installing to /tmp/workdir/qris/old/qris.Rcheck/00LOCK-qris/00new/qris/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘qris’
* removing ‘/tmp/workdir/qris/old/qris.Rcheck/qris’


```
# qte

<details>

* Version: 1.3.1
* GitHub: NA
* Source code: https://github.com/cran/qte
* Date/Publication: 2022-09-01 14:30:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "qte")` for more info

</details>

## In both

*   checking whether package ‘qte’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/qte/new/qte.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘qte’ ...
** package ‘qte’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘qte’
* removing ‘/tmp/workdir/qte/new/qte.Rcheck/qte’


```
### CRAN

```
* installing *source* package ‘qte’ ...
** package ‘qte’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘qte’
* removing ‘/tmp/workdir/qte/old/qte.Rcheck/qte’


```
# quid

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/quid
* Date/Publication: 2021-12-09 09:00:02 UTC
* Number of recursive dependencies: 95

Run `revdepcheck::cloud_details(, "quid")` for more info

</details>

## In both

*   checking whether package ‘quid’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/quid/new/quid.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘quid’ ...
** package ‘quid’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘quid’
* removing ‘/tmp/workdir/quid/new/quid.Rcheck/quid’


```
### CRAN

```
* installing *source* package ‘quid’ ...
** package ‘quid’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘quid’
* removing ‘/tmp/workdir/quid/old/quid.Rcheck/quid’


```
# RATest

<details>

* Version: 0.1.10
* GitHub: https://github.com/ignaciomsarmiento/RATest
* Source code: https://github.com/cran/RATest
* Date/Publication: 2022-09-29 04:30:02 UTC
* Number of recursive dependencies: 54

Run `revdepcheck::cloud_details(, "RATest")` for more info

</details>

## In both

*   checking whether package ‘RATest’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RATest/new/RATest.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RATest’ ...
** package ‘RATest’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘RATest’
* removing ‘/tmp/workdir/RATest/new/RATest.Rcheck/RATest’


```
### CRAN

```
* installing *source* package ‘RATest’ ...
** package ‘RATest’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘RATest’
* removing ‘/tmp/workdir/RATest/old/RATest.Rcheck/RATest’


```
# RcmdrPlugin.RiskDemo

<details>

* Version: 3.2
* GitHub: NA
* Source code: https://github.com/cran/RcmdrPlugin.RiskDemo
* Date/Publication: 2024-02-06 09:20:02 UTC
* Number of recursive dependencies: 200

Run `revdepcheck::cloud_details(, "RcmdrPlugin.RiskDemo")` for more info

</details>

## In both

*   checking whether package ‘RcmdrPlugin.RiskDemo’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RcmdrPlugin.RiskDemo/new/RcmdrPlugin.RiskDemo.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RcmdrPlugin.RiskDemo’ ...
** package ‘RcmdrPlugin.RiskDemo’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘RcmdrPlugin.RiskDemo’
* removing ‘/tmp/workdir/RcmdrPlugin.RiskDemo/new/RcmdrPlugin.RiskDemo.Rcheck/RcmdrPlugin.RiskDemo’


```
### CRAN

```
* installing *source* package ‘RcmdrPlugin.RiskDemo’ ...
** package ‘RcmdrPlugin.RiskDemo’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘RcmdrPlugin.RiskDemo’
* removing ‘/tmp/workdir/RcmdrPlugin.RiskDemo/old/RcmdrPlugin.RiskDemo.Rcheck/RcmdrPlugin.RiskDemo’


```
# rddtools

<details>

* Version: 1.6.0
* GitHub: https://github.com/bquast/rddtools
* Source code: https://github.com/cran/rddtools
* Date/Publication: 2022-01-10 12:42:49 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "rddtools")` for more info

</details>

## In both

*   checking whether package ‘rddtools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rddtools/new/rddtools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rddtools’ ...
** package ‘rddtools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘np’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘rddtools’
* removing ‘/tmp/workdir/rddtools/new/rddtools.Rcheck/rddtools’


```
### CRAN

```
* installing *source* package ‘rddtools’ ...
** package ‘rddtools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘np’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘rddtools’
* removing ‘/tmp/workdir/rddtools/old/rddtools.Rcheck/rddtools’


```
# riskRegression

<details>

* Version: 2023.12.21
* GitHub: https://github.com/tagteam/riskRegression
* Source code: https://github.com/cran/riskRegression
* Date/Publication: 2023-12-19 17:00:02 UTC
* Number of recursive dependencies: 186

Run `revdepcheck::cloud_details(, "riskRegression")` for more info

</details>

## In both

*   checking whether package ‘riskRegression’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/riskRegression/new/riskRegression.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘riskRegression’ ...
** package ‘riskRegression’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c IC-Nelson-Aalen-cens-time.cpp -o IC-Nelson-Aalen-cens-time.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c aucCVFun.cpp -o aucCVFun.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c baseHaz.cpp -o baseHaz.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c calcSeCSC.cpp -o calcSeCSC.o
...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘riskRegression’
* removing ‘/tmp/workdir/riskRegression/new/riskRegression.Rcheck/riskRegression’


```
### CRAN

```
* installing *source* package ‘riskRegression’ ...
** package ‘riskRegression’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c IC-Nelson-Aalen-cens-time.cpp -o IC-Nelson-Aalen-cens-time.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c aucCVFun.cpp -o aucCVFun.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c baseHaz.cpp -o baseHaz.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c calcSeCSC.cpp -o calcSeCSC.o
...
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘riskRegression’
* removing ‘/tmp/workdir/riskRegression/old/riskRegression.Rcheck/riskRegression’


```
# rms

<details>

* Version: 6.8-1
* GitHub: https://github.com/harrelfe/rms
* Source code: https://github.com/cran/rms
* Date/Publication: 2024-05-27 12:00:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "rms")` for more info

</details>

## In both

*   checking whether package ‘rms’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rms/new/rms.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rmsb’
    ```

## Installation

### Devel

```
* installing *source* package ‘rms’ ...
** package ‘rms’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using Fortran compiler: ‘GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c init.c -o init.o
gfortran  -fpic  -g -O2  -c lrmfit.f -o lrmfit.o
gfortran  -fpic  -g -O2  -c mlmats.f -o mlmats.o
gfortran  -fpic  -g -O2  -c ormuv.f -o ormuv.o
...
** R
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘rms’
* removing ‘/tmp/workdir/rms/new/rms.Rcheck/rms’


```
### CRAN

```
* installing *source* package ‘rms’ ...
** package ‘rms’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using Fortran compiler: ‘GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c init.c -o init.o
gfortran  -fpic  -g -O2  -c lrmfit.f -o lrmfit.o
gfortran  -fpic  -g -O2  -c mlmats.f -o mlmats.o
gfortran  -fpic  -g -O2  -c ormuv.f -o ormuv.o
...
** R
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘rms’
* removing ‘/tmp/workdir/rms/old/rms.Rcheck/rms’


```
# rmsb

<details>

* Version: 1.1-1
* GitHub: NA
* Source code: https://github.com/cran/rmsb
* Date/Publication: 2024-07-08 11:10:03 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "rmsb")` for more info

</details>

## In both

*   checking whether package ‘rmsb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rmsb/new/rmsb.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rmsb’ ...
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/tmp/workdir/rmsb/new/rmsb.Rcheck/rmsb’


```
### CRAN

```
* installing *source* package ‘rmsb’ ...
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/tmp/workdir/rmsb/old/rmsb.Rcheck/rmsb’


```
# robmed

<details>

* Version: 1.0.2
* GitHub: https://github.com/aalfons/robmed
* Source code: https://github.com/cran/robmed
* Date/Publication: 2023-06-16 23:00:02 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "robmed")` for more info

</details>

## In both

*   checking whether package ‘robmed’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/robmed/new/robmed.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘robmed’ ...
** package ‘robmed’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘robmed’
* removing ‘/tmp/workdir/robmed/new/robmed.Rcheck/robmed’


```
### CRAN

```
* installing *source* package ‘robmed’ ...
** package ‘robmed’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘robmed’
* removing ‘/tmp/workdir/robmed/old/robmed.Rcheck/robmed’


```
# robmedExtra

<details>

* Version: 0.1.0
* GitHub: https://github.com/aalfons/robmedExtra
* Source code: https://github.com/cran/robmedExtra
* Date/Publication: 2023-06-02 14:40:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "robmedExtra")` for more info

</details>

## In both

*   checking whether package ‘robmedExtra’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/robmedExtra/new/robmedExtra.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘robmedExtra’ ...
** package ‘robmedExtra’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘robmed’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘robmedExtra’
* removing ‘/tmp/workdir/robmedExtra/new/robmedExtra.Rcheck/robmedExtra’


```
### CRAN

```
* installing *source* package ‘robmedExtra’ ...
** package ‘robmedExtra’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘robmed’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘robmedExtra’
* removing ‘/tmp/workdir/robmedExtra/old/robmedExtra.Rcheck/robmedExtra’


```
# RPPanalyzer

<details>

* Version: 1.4.9
* GitHub: NA
* Source code: https://github.com/cran/RPPanalyzer
* Date/Publication: 2024-01-25 11:00:02 UTC
* Number of recursive dependencies: 82

Run `revdepcheck::cloud_details(, "RPPanalyzer")` for more info

</details>

## In both

*   checking whether package ‘RPPanalyzer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RPPanalyzer/new/RPPanalyzer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RPPanalyzer’ ...
** package ‘RPPanalyzer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘RPPanalyzer’
* removing ‘/tmp/workdir/RPPanalyzer/new/RPPanalyzer.Rcheck/RPPanalyzer’


```
### CRAN

```
* installing *source* package ‘RPPanalyzer’ ...
** package ‘RPPanalyzer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘RPPanalyzer’
* removing ‘/tmp/workdir/RPPanalyzer/old/RPPanalyzer.Rcheck/RPPanalyzer’


```
# RQdeltaCT

<details>

* Version: 1.3.0
* GitHub: NA
* Source code: https://github.com/cran/RQdeltaCT
* Date/Publication: 2024-04-17 15:50:02 UTC
* Number of recursive dependencies: 165

Run `revdepcheck::cloud_details(, "RQdeltaCT")` for more info

</details>

## In both

*   checking whether package ‘RQdeltaCT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RQdeltaCT/new/RQdeltaCT.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RQdeltaCT’ ...
** package ‘RQdeltaCT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘RQdeltaCT’
* removing ‘/tmp/workdir/RQdeltaCT/new/RQdeltaCT.Rcheck/RQdeltaCT’


```
### CRAN

```
* installing *source* package ‘RQdeltaCT’ ...
** package ‘RQdeltaCT’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘RQdeltaCT’
* removing ‘/tmp/workdir/RQdeltaCT/old/RQdeltaCT.Rcheck/RQdeltaCT’


```
# scCustomize

<details>

* Version: 2.1.2
* GitHub: https://github.com/samuel-marsh/scCustomize
* Source code: https://github.com/cran/scCustomize
* Date/Publication: 2024-02-28 19:40:02 UTC
* Number of recursive dependencies: 267

Run `revdepcheck::cloud_details(, "scCustomize")` for more info

</details>

## In both

*   checking whether package ‘scCustomize’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/scCustomize/new/scCustomize.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Nebulosa’
    ```

## Installation

### Devel

```
* installing *source* package ‘scCustomize’ ...
** package ‘scCustomize’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘SeuratObject’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.4 is required
Execution halted
ERROR: lazy loading failed for package ‘scCustomize’
* removing ‘/tmp/workdir/scCustomize/new/scCustomize.Rcheck/scCustomize’


```
### CRAN

```
* installing *source* package ‘scCustomize’ ...
** package ‘scCustomize’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘SeuratObject’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.4 is required
Execution halted
ERROR: lazy loading failed for package ‘scCustomize’
* removing ‘/tmp/workdir/scCustomize/old/scCustomize.Rcheck/scCustomize’


```
# SCdeconR

<details>

* Version: 1.0.0
* GitHub: https://github.com/Liuy12/SCdeconR
* Source code: https://github.com/cran/SCdeconR
* Date/Publication: 2024-03-22 19:20:02 UTC
* Number of recursive dependencies: 236

Run `revdepcheck::cloud_details(, "SCdeconR")` for more info

</details>

## In both

*   checking whether package ‘SCdeconR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SCdeconR/new/SCdeconR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SCdeconR’ ...
** package ‘SCdeconR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘SeuratObject’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.4 is required
Execution halted
ERROR: lazy loading failed for package ‘SCdeconR’
* removing ‘/tmp/workdir/SCdeconR/new/SCdeconR.Rcheck/SCdeconR’


```
### CRAN

```
* installing *source* package ‘SCdeconR’ ...
** package ‘SCdeconR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘SeuratObject’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.4 is required
Execution halted
ERROR: lazy loading failed for package ‘SCdeconR’
* removing ‘/tmp/workdir/SCdeconR/old/SCdeconR.Rcheck/SCdeconR’


```
# scGate

<details>

* Version: 1.6.2
* GitHub: https://github.com/carmonalab/scGate
* Source code: https://github.com/cran/scGate
* Date/Publication: 2024-04-23 08:50:02 UTC
* Number of recursive dependencies: 179

Run `revdepcheck::cloud_details(, "scGate")` for more info

</details>

## In both

*   checking whether package ‘scGate’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/scGate/new/scGate.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘scGate’ ...
** package ‘scGate’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘Seurat’ is not available and has been replaced
by .GlobalEnv when processing object ‘query.seurat’
Warning: namespace ‘Seurat’ is not available and has been replaced
by .GlobalEnv when processing object ‘query.seurat’
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘scGate’
* removing ‘/tmp/workdir/scGate/new/scGate.Rcheck/scGate’


```
### CRAN

```
* installing *source* package ‘scGate’ ...
** package ‘scGate’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘Seurat’ is not available and has been replaced
by .GlobalEnv when processing object ‘query.seurat’
Warning: namespace ‘Seurat’ is not available and has been replaced
by .GlobalEnv when processing object ‘query.seurat’
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘scGate’
* removing ‘/tmp/workdir/scGate/old/scGate.Rcheck/scGate’


```
# SCIntRuler

<details>

* Version: 0.99.6
* GitHub: https://github.com/yuelyu21/SCIntRuler
* Source code: https://github.com/cran/SCIntRuler
* Date/Publication: 2024-07-12 15:20:08 UTC
* Number of recursive dependencies: 202

Run `revdepcheck::cloud_details(, "SCIntRuler")` for more info

</details>

## In both

*   checking whether package ‘SCIntRuler’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SCIntRuler/new/SCIntRuler.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SCIntRuler’ ...
** package ‘SCIntRuler’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c crossdist.cpp -o crossdist.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o SCIntRuler.so RcppExports.o crossdist.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/SCIntRuler/new/SCIntRuler.Rcheck/00LOCK-SCIntRuler/00new/SCIntRuler/libs
** R
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SCIntRuler’
* removing ‘/tmp/workdir/SCIntRuler/new/SCIntRuler.Rcheck/SCIntRuler’


```
### CRAN

```
* installing *source* package ‘SCIntRuler’ ...
** package ‘SCIntRuler’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c crossdist.cpp -o crossdist.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o SCIntRuler.so RcppExports.o crossdist.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/SCIntRuler/old/SCIntRuler.Rcheck/00LOCK-SCIntRuler/00new/SCIntRuler/libs
** R
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SCIntRuler’
* removing ‘/tmp/workdir/SCIntRuler/old/SCIntRuler.Rcheck/SCIntRuler’


```
# scMappR

<details>

* Version: 1.0.11
* GitHub: NA
* Source code: https://github.com/cran/scMappR
* Date/Publication: 2023-06-30 08:40:08 UTC
* Number of recursive dependencies: 234

Run `revdepcheck::cloud_details(, "scMappR")` for more info

</details>

## In both

*   checking whether package ‘scMappR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/scMappR/new/scMappR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘scMappR’ ...
** package ‘scMappR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘scMappR’
* removing ‘/tmp/workdir/scMappR/new/scMappR.Rcheck/scMappR’


```
### CRAN

```
* installing *source* package ‘scMappR’ ...
** package ‘scMappR’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘scMappR’
* removing ‘/tmp/workdir/scMappR/old/scMappR.Rcheck/scMappR’


```
# scpi

<details>

* Version: 2.2.5
* GitHub: NA
* Source code: https://github.com/cran/scpi
* Date/Publication: 2023-11-01 06:10:07 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "scpi")` for more info

</details>

## In both

*   checking whether package ‘scpi’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/scpi/new/scpi.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘scpi’ ...
** package ‘scpi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Warning in .recacheSubclasses(def@className, def, env) :
  undefined subclass "pcorMatrix" of class "ConstVal"; definition not updated
Warning in .recacheSubclasses(def@className, def, env) :
...
Warning in .recacheSubclasses(def@className, def, env) :
  undefined subclass "pcorMatrix" of class "ConstValORExpr"; definition not updated
Warning in .recacheSubclasses(def@className, def, env) :
  undefined subclass "pcorMatrix" of class "ConstValORNULL"; definition not updated
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘scpi’
* removing ‘/tmp/workdir/scpi/new/scpi.Rcheck/scpi’


```
### CRAN

```
* installing *source* package ‘scpi’ ...
** package ‘scpi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Warning in .recacheSubclasses(def@className, def, env) :
  undefined subclass "pcorMatrix" of class "ConstVal"; definition not updated
Warning in .recacheSubclasses(def@className, def, env) :
...
Warning in .recacheSubclasses(def@className, def, env) :
  undefined subclass "pcorMatrix" of class "ConstValORExpr"; definition not updated
Warning in .recacheSubclasses(def@className, def, env) :
  undefined subclass "pcorMatrix" of class "ConstValORNULL"; definition not updated
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘scpi’
* removing ‘/tmp/workdir/scpi/old/scpi.Rcheck/scpi’


```
# scRNAstat

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/scRNAstat
* Date/Publication: 2021-09-22 08:10:02 UTC
* Number of recursive dependencies: 156

Run `revdepcheck::cloud_details(, "scRNAstat")` for more info

</details>

## In both

*   checking whether package ‘scRNAstat’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/scRNAstat/new/scRNAstat.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘scRNAstat’ ...
** package ‘scRNAstat’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘Seurat’ is not available and has been replaced
by .GlobalEnv when processing object ‘AJ064_small_last_sce’
Warning: namespace ‘SeuratObject’ is not available and has been replaced
by .GlobalEnv when processing object ‘AJ064_small_last_sce’
...
by .GlobalEnv when processing object ‘AJ064_small_last_sce’
Warning: namespace ‘DBI’ is not available and has been replaced
by .GlobalEnv when processing object ‘AJ064_small_last_sce’
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘scRNAstat’
* removing ‘/tmp/workdir/scRNAstat/new/scRNAstat.Rcheck/scRNAstat’


```
### CRAN

```
* installing *source* package ‘scRNAstat’ ...
** package ‘scRNAstat’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
Warning: namespace ‘Seurat’ is not available and has been replaced
by .GlobalEnv when processing object ‘AJ064_small_last_sce’
Warning: namespace ‘SeuratObject’ is not available and has been replaced
by .GlobalEnv when processing object ‘AJ064_small_last_sce’
...
by .GlobalEnv when processing object ‘AJ064_small_last_sce’
Warning: namespace ‘DBI’ is not available and has been replaced
by .GlobalEnv when processing object ‘AJ064_small_last_sce’
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘scRNAstat’
* removing ‘/tmp/workdir/scRNAstat/old/scRNAstat.Rcheck/scRNAstat’


```
# sectorgap

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/sectorgap
* Date/Publication: 2024-01-22 17:40:02 UTC
* Number of recursive dependencies: 46

Run `revdepcheck::cloud_details(, "sectorgap")` for more info

</details>

## In both

*   checking whether package ‘sectorgap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sectorgap/new/sectorgap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sectorgap’ ...
** package ‘sectorgap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘sectorgap’
* removing ‘/tmp/workdir/sectorgap/new/sectorgap.Rcheck/sectorgap’


```
### CRAN

```
* installing *source* package ‘sectorgap’ ...
** package ‘sectorgap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘sectorgap’
* removing ‘/tmp/workdir/sectorgap/old/sectorgap.Rcheck/sectorgap’


```
# SEERaBomb

<details>

* Version: 2019.2
* GitHub: NA
* Source code: https://github.com/cran/SEERaBomb
* Date/Publication: 2019-12-12 18:50:03 UTC
* Number of recursive dependencies: 185

Run `revdepcheck::cloud_details(, "SEERaBomb")` for more info

</details>

## In both

*   checking whether package ‘SEERaBomb’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SEERaBomb/new/SEERaBomb.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SEERaBomb’ ...
** package ‘SEERaBomb’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c SEERaBomb_init.c -o SEERaBomb_init.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c fillPYM.cpp -o fillPYM.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o SEERaBomb.so RcppExports.o SEERaBomb_init.o fillPYM.o -L/opt/R/4.3.1/lib/R/lib -lR
...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘demography’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘SEERaBomb’
* removing ‘/tmp/workdir/SEERaBomb/new/SEERaBomb.Rcheck/SEERaBomb’


```
### CRAN

```
* installing *source* package ‘SEERaBomb’ ...
** package ‘SEERaBomb’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c SEERaBomb_init.c -o SEERaBomb_init.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c fillPYM.cpp -o fillPYM.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o SEERaBomb.so RcppExports.o SEERaBomb_init.o fillPYM.o -L/opt/R/4.3.1/lib/R/lib -lR
...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘demography’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘SEERaBomb’
* removing ‘/tmp/workdir/SEERaBomb/old/SEERaBomb.Rcheck/SEERaBomb’


```
# semicmprskcoxmsm

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/semicmprskcoxmsm
* Date/Publication: 2022-04-29 23:40:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "semicmprskcoxmsm")` for more info

</details>

## In both

*   checking whether package ‘semicmprskcoxmsm’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/semicmprskcoxmsm/new/semicmprskcoxmsm.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘semicmprskcoxmsm’ ...
** package ‘semicmprskcoxmsm’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘semicmprskcoxmsm’
* removing ‘/tmp/workdir/semicmprskcoxmsm/new/semicmprskcoxmsm.Rcheck/semicmprskcoxmsm’


```
### CRAN

```
* installing *source* package ‘semicmprskcoxmsm’ ...
** package ‘semicmprskcoxmsm’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘semicmprskcoxmsm’
* removing ‘/tmp/workdir/semicmprskcoxmsm/old/semicmprskcoxmsm.Rcheck/semicmprskcoxmsm’


```
# SensMap

<details>

* Version: 0.7
* GitHub: https://github.com/IbtihelRebhi/SensMap
* Source code: https://github.com/cran/SensMap
* Date/Publication: 2022-07-04 19:00:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "SensMap")` for more info

</details>

## In both

*   checking whether package ‘SensMap’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SensMap/new/SensMap.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SensMap’ ...
** package ‘SensMap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SensMap’
* removing ‘/tmp/workdir/SensMap/new/SensMap.Rcheck/SensMap’


```
### CRAN

```
* installing *source* package ‘SensMap’ ...
** package ‘SensMap’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SensMap’
* removing ‘/tmp/workdir/SensMap/old/SensMap.Rcheck/SensMap’


```
# Seurat

<details>

* Version: 5.1.0
* GitHub: https://github.com/satijalab/seurat
* Source code: https://github.com/cran/Seurat
* Date/Publication: 2024-05-10 17:23:17 UTC
* Number of recursive dependencies: 266

Run `revdepcheck::cloud_details(, "Seurat")` for more info

</details>

## In both

*   checking whether package ‘Seurat’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/Seurat/new/Seurat.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Seurat’ ...
** package ‘Seurat’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c ModularityOptimizer.cpp -o ModularityOptimizer.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c RModularityOptimizer.cpp -o RModularityOptimizer.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘SeuratObject’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.4 is required
Execution halted
ERROR: lazy loading failed for package ‘Seurat’
* removing ‘/tmp/workdir/Seurat/new/Seurat.Rcheck/Seurat’


```
### CRAN

```
* installing *source* package ‘Seurat’ ...
** package ‘Seurat’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c ModularityOptimizer.cpp -o ModularityOptimizer.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppEigen/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c RModularityOptimizer.cpp -o RModularityOptimizer.o
In file included from /opt/R/4.3.1/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘SeuratObject’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is being loaded, but >= 1.6.4 is required
Execution halted
ERROR: lazy loading failed for package ‘Seurat’
* removing ‘/tmp/workdir/Seurat/old/Seurat.Rcheck/Seurat’


```
# shinyTempSignal

<details>

* Version: 0.0.8
* GitHub: https://github.com/YuLab-SMU/shinyTempSignal
* Source code: https://github.com/cran/shinyTempSignal
* Date/Publication: 2024-03-06 08:00:02 UTC
* Number of recursive dependencies: 137

Run `revdepcheck::cloud_details(, "shinyTempSignal")` for more info

</details>

## In both

*   checking whether package ‘shinyTempSignal’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/shinyTempSignal/new/shinyTempSignal.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘shinyTempSignal’ ...
** package ‘shinyTempSignal’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘shinyTempSignal’
* removing ‘/tmp/workdir/shinyTempSignal/new/shinyTempSignal.Rcheck/shinyTempSignal’


```
### CRAN

```
* installing *source* package ‘shinyTempSignal’ ...
** package ‘shinyTempSignal’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘shinyTempSignal’
* removing ‘/tmp/workdir/shinyTempSignal/old/shinyTempSignal.Rcheck/shinyTempSignal’


```
# sievePH

<details>

* Version: 1.1
* GitHub: https://github.com/mjuraska/sievePH
* Source code: https://github.com/cran/sievePH
* Date/Publication: 2024-05-17 23:40:02 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "sievePH")` for more info

</details>

## In both

*   checking whether package ‘sievePH’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sievePH/new/sievePH.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sievePH’ ...
** package ‘sievePH’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c kernel_sievePH_utils.cpp -o kernel_sievePH_utils.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o sievePH.so RcppExports.o kernel_sievePH_utils.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/sievePH/new/sievePH.Rcheck/00LOCK-sievePH/00new/sievePH/libs
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘sievePH’
* removing ‘/tmp/workdir/sievePH/new/sievePH.Rcheck/sievePH’


```
### CRAN

```
* installing *source* package ‘sievePH’ ...
** package ‘sievePH’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c kernel_sievePH_utils.cpp -o kernel_sievePH_utils.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o sievePH.so RcppExports.o kernel_sievePH_utils.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/sievePH/old/sievePH.Rcheck/00LOCK-sievePH/00new/sievePH/libs
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘sievePH’
* removing ‘/tmp/workdir/sievePH/old/sievePH.Rcheck/sievePH’


```
# Signac

<details>

* Version: 1.13.0
* GitHub: https://github.com/stuart-lab/signac
* Source code: https://github.com/cran/Signac
* Date/Publication: 2024-04-04 02:42:57 UTC
* Number of recursive dependencies: 250

Run `revdepcheck::cloud_details(, "Signac")` for more info

</details>

## In both

*   checking whether package ‘Signac’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/Signac/new/Signac.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Signac’ ...
** package ‘Signac’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c filter.cpp -o filter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c group.cpp -o group.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c split.cpp -o split.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c validate.cpp -o validate.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Signac’
* removing ‘/tmp/workdir/Signac/new/Signac.Rcheck/Signac’


```
### CRAN

```
* installing *source* package ‘Signac’ ...
** package ‘Signac’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c filter.cpp -o filter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c group.cpp -o group.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c split.cpp -o split.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c validate.cpp -o validate.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Signac’
* removing ‘/tmp/workdir/Signac/old/Signac.Rcheck/Signac’


```
# SimplyAgree

<details>

* Version: 0.2.0
* GitHub: https://github.com/arcaldwell49/SimplyAgree
* Source code: https://github.com/cran/SimplyAgree
* Date/Publication: 2024-03-21 14:20:06 UTC
* Number of recursive dependencies: 115

Run `revdepcheck::cloud_details(, "SimplyAgree")` for more info

</details>

## In both

*   checking whether package ‘SimplyAgree’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SimplyAgree/new/SimplyAgree.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SimplyAgree’ ...
** package ‘SimplyAgree’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SimplyAgree’
* removing ‘/tmp/workdir/SimplyAgree/new/SimplyAgree.Rcheck/SimplyAgree’


```
### CRAN

```
* installing *source* package ‘SimplyAgree’ ...
** package ‘SimplyAgree’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Warning in check_dep_version() :
  ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SimplyAgree’
* removing ‘/tmp/workdir/SimplyAgree/old/SimplyAgree.Rcheck/SimplyAgree’


```
# sMSROC

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/sMSROC
* Date/Publication: 2023-12-07 15:50:02 UTC
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "sMSROC")` for more info

</details>

## In both

*   checking whether package ‘sMSROC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sMSROC/new/sMSROC.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sMSROC’ ...
** package ‘sMSROC’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘sMSROC’
* removing ‘/tmp/workdir/sMSROC/new/sMSROC.Rcheck/sMSROC’


```
### CRAN

```
* installing *source* package ‘sMSROC’ ...
** package ‘sMSROC’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘sMSROC’
* removing ‘/tmp/workdir/sMSROC/old/sMSROC.Rcheck/sMSROC’


```
# SNPassoc

<details>

* Version: 2.1-0
* GitHub: https://github.com/isglobal-brge/SNPassoc
* Source code: https://github.com/cran/SNPassoc
* Date/Publication: 2022-12-14 20:20:02 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::cloud_details(, "SNPassoc")` for more info

</details>

## In both

*   checking whether package ‘SNPassoc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SNPassoc/new/SNPassoc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SNPassoc’ ...
** package ‘SNPassoc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SNPassoc’
* removing ‘/tmp/workdir/SNPassoc/new/SNPassoc.Rcheck/SNPassoc’


```
### CRAN

```
* installing *source* package ‘SNPassoc’ ...
** package ‘SNPassoc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SNPassoc’
* removing ‘/tmp/workdir/SNPassoc/old/SNPassoc.Rcheck/SNPassoc’


```
# snplinkage

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/snplinkage
* Date/Publication: 2023-05-04 08:10:02 UTC
* Number of recursive dependencies: 145

Run `revdepcheck::cloud_details(, "snplinkage")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/snplinkage/new/snplinkage.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
* running under: Ubuntu 22.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘snplinkage/DESCRIPTION’ ... OK
...
* this is package ‘snplinkage’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘GWASTools’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/snplinkage/old/snplinkage.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
* running under: Ubuntu 22.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘snplinkage/DESCRIPTION’ ... OK
...
* this is package ‘snplinkage’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘GWASTools’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# SoupX

<details>

* Version: 1.6.2
* GitHub: https://github.com/constantAmateur/SoupX
* Source code: https://github.com/cran/SoupX
* Date/Publication: 2022-11-01 14:00:03 UTC
* Number of recursive dependencies: 201

Run `revdepcheck::cloud_details(, "SoupX")` for more info

</details>

## In both

*   checking whether package ‘SoupX’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SoupX/new/SoupX.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SoupX’ ...
** package ‘SoupX’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SoupX’
* removing ‘/tmp/workdir/SoupX/new/SoupX.Rcheck/SoupX’


```
### CRAN

```
* installing *source* package ‘SoupX’ ...
** package ‘SoupX’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SoupX’
* removing ‘/tmp/workdir/SoupX/old/SoupX.Rcheck/SoupX’


```
# sparsereg

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/sparsereg
* Date/Publication: 2016-03-10 23:32:18
* Number of recursive dependencies: 49

Run `revdepcheck::cloud_details(, "sparsereg")` for more info

</details>

## In both

*   checking whether package ‘sparsereg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/sparsereg/new/sparsereg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sparsereg’ ...
** package ‘sparsereg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c makeinter.cpp -o makeinter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c makethreeinter.cpp -o makethreeinter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c subgroup.cpp -o subgroup.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o sparsereg.so RcppExports.o makeinter.o makethreeinter.o subgroup.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/sparsereg/new/sparsereg.Rcheck/00LOCK-sparsereg/00new/sparsereg/libs
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘sparsereg’
* removing ‘/tmp/workdir/sparsereg/new/sparsereg.Rcheck/sparsereg’


```
### CRAN

```
* installing *source* package ‘sparsereg’ ...
** package ‘sparsereg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c makeinter.cpp -o makeinter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c makethreeinter.cpp -o makethreeinter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I'/opt/R/4.3.1/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c subgroup.cpp -o subgroup.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o sparsereg.so RcppExports.o makeinter.o makethreeinter.o subgroup.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/sparsereg/old/sparsereg.Rcheck/00LOCK-sparsereg/00new/sparsereg/libs
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘sparsereg’
* removing ‘/tmp/workdir/sparsereg/old/sparsereg.Rcheck/sparsereg’


```
# spikeSlabGAM

<details>

* Version: 1.1-19
* GitHub: https://github.com/fabian-s/spikeSlabGAM
* Source code: https://github.com/cran/spikeSlabGAM
* Date/Publication: 2022-06-10 15:50:07 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::cloud_details(, "spikeSlabGAM")` for more info

</details>

## In both

*   checking whether package ‘spikeSlabGAM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/spikeSlabGAM/new/spikeSlabGAM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘spikeSlabGAM’ ...
** package ‘spikeSlabGAM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c sampler.c -o sampler.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c spikeSlabGAM_init.c -o spikeSlabGAM_init.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o spikeSlabGAM.so sampler.o spikeSlabGAM_init.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/spikeSlabGAM/new/spikeSlabGAM.Rcheck/00LOCK-spikeSlabGAM/00new/spikeSlabGAM/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘spikeSlabGAM’
* removing ‘/tmp/workdir/spikeSlabGAM/new/spikeSlabGAM.Rcheck/spikeSlabGAM’


```
### CRAN

```
* installing *source* package ‘spikeSlabGAM’ ...
** package ‘spikeSlabGAM’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c sampler.c -o sampler.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c spikeSlabGAM_init.c -o spikeSlabGAM_init.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o spikeSlabGAM.so sampler.o spikeSlabGAM_init.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/spikeSlabGAM/old/spikeSlabGAM.Rcheck/00LOCK-spikeSlabGAM/00new/spikeSlabGAM/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘spikeSlabGAM’
* removing ‘/tmp/workdir/spikeSlabGAM/old/spikeSlabGAM.Rcheck/spikeSlabGAM’


```
# statsr

<details>

* Version: 0.3.0
* GitHub: https://github.com/StatsWithR/statsr
* Source code: https://github.com/cran/statsr
* Date/Publication: 2021-01-22 20:40:03 UTC
* Number of recursive dependencies: 97

Run `revdepcheck::cloud_details(, "statsr")` for more info

</details>

## In both

*   checking whether package ‘statsr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/statsr/new/statsr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘statsr’ ...
** package ‘statsr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘BayesFactor’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘statsr’
* removing ‘/tmp/workdir/statsr/new/statsr.Rcheck/statsr’


```
### CRAN

```
* installing *source* package ‘statsr’ ...
** package ‘statsr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘BayesFactor’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘statsr’
* removing ‘/tmp/workdir/statsr/old/statsr.Rcheck/statsr’


```
# streamDAG

<details>

* Version: 1.5
* GitHub: NA
* Source code: https://github.com/cran/streamDAG
* Date/Publication: 2023-10-06 18:50:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "streamDAG")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/streamDAG/new/streamDAG.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
* running under: Ubuntu 22.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘streamDAG/DESCRIPTION’ ... OK
* this is package ‘streamDAG’ version ‘1.5’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘asbio’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/streamDAG/old/streamDAG.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
* running under: Ubuntu 22.04.4 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘streamDAG/DESCRIPTION’ ... OK
* this is package ‘streamDAG’ version ‘1.5’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘asbio’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# survHE

<details>

* Version: 2.0.1
* GitHub: https://github.com/giabaio/survHE
* Source code: https://github.com/cran/survHE
* Date/Publication: 2023-03-19 22:10:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "survHE")` for more info

</details>

## In both

*   checking whether package ‘survHE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/survHE/new/survHE.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'survHEinla', 'survHEhmc'
    ```

## Installation

### Devel

```
* installing *source* package ‘survHE’ ...
** package ‘survHE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘survHE’
* removing ‘/tmp/workdir/survHE/new/survHE.Rcheck/survHE’


```
### CRAN

```
* installing *source* package ‘survHE’ ...
** package ‘survHE’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘survHE’
* removing ‘/tmp/workdir/survHE/old/survHE.Rcheck/survHE’


```
# survidm

<details>

* Version: 1.3.2
* GitHub: NA
* Source code: https://github.com/cran/survidm
* Date/Publication: 2021-06-24 23:20:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "survidm")` for more info

</details>

## In both

*   checking whether package ‘survidm’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/survidm/new/survidm.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘survidm’ ...
** package ‘survidm’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c survidm_init.c -o survidm_init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c survivalBIV.c -o survivalBIV.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o survidm.so survidm_init.o survivalBIV.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/survidm/new/survidm.Rcheck/00LOCK-survidm/00new/survidm/libs
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘survidm’
* removing ‘/tmp/workdir/survidm/new/survidm.Rcheck/survidm’


```
### CRAN

```
* installing *source* package ‘survidm’ ...
** package ‘survidm’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c survidm_init.c -o survidm_init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c survivalBIV.c -o survivalBIV.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o survidm.so survidm_init.o survivalBIV.o -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/survidm/old/survidm.Rcheck/00LOCK-survidm/00new/survidm/libs
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘survidm’
* removing ‘/tmp/workdir/survidm/old/survidm.Rcheck/survidm’


```
# tempted

<details>

* Version: 0.1.1
* GitHub: https://github.com/pixushi/tempted
* Source code: https://github.com/cran/tempted
* Date/Publication: 2024-05-09 02:40:02 UTC
* Number of recursive dependencies: 37

Run `revdepcheck::cloud_details(, "tempted")` for more info

</details>

## In both

*   checking whether package ‘tempted’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tempted/new/tempted.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tempted’ ...
** package ‘tempted’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘np’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘tempted’
* removing ‘/tmp/workdir/tempted/new/tempted.Rcheck/tempted’


```
### CRAN

```
* installing *source* package ‘tempted’ ...
** package ‘tempted’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘np’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘tempted’
* removing ‘/tmp/workdir/tempted/old/tempted.Rcheck/tempted’


```
# tidydr

<details>

* Version: 0.0.5
* GitHub: https://github.com/YuLab-SMU/tidydr
* Source code: https://github.com/cran/tidydr
* Date/Publication: 2023-03-08 09:20:02 UTC
* Number of recursive dependencies: 74

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
# tidyEdSurvey

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/tidyEdSurvey
* Date/Publication: 2024-05-14 20:20:03 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "tidyEdSurvey")` for more info

</details>

## In both

*   checking whether package ‘tidyEdSurvey’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tidyEdSurvey/new/tidyEdSurvey.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tidyEdSurvey’ ...
** package ‘tidyEdSurvey’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘EdSurvey’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
In addition: Warning message:
In check_dep_version() : ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Execution halted
ERROR: lazy loading failed for package ‘tidyEdSurvey’
* removing ‘/tmp/workdir/tidyEdSurvey/new/tidyEdSurvey.Rcheck/tidyEdSurvey’


```
### CRAN

```
* installing *source* package ‘tidyEdSurvey’ ...
** package ‘tidyEdSurvey’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘EdSurvey’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
In addition: Warning message:
In check_dep_version() : ABI version mismatch: 
lme4 was built with Matrix ABI version 1
Current Matrix ABI version is 0
Please re-install lme4 from source or restore original ‘Matrix’ package
Execution halted
ERROR: lazy loading failed for package ‘tidyEdSurvey’
* removing ‘/tmp/workdir/tidyEdSurvey/old/tidyEdSurvey.Rcheck/tidyEdSurvey’


```
# tidyseurat

<details>

* Version: 0.8.0
* GitHub: https://github.com/stemangiola/tidyseurat
* Source code: https://github.com/cran/tidyseurat
* Date/Publication: 2024-01-10 04:50:02 UTC
* Number of recursive dependencies: 207

Run `revdepcheck::cloud_details(, "tidyseurat")` for more info

</details>

## In both

*   checking whether package ‘tidyseurat’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tidyseurat/new/tidyseurat.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tidyseurat’ ...
** package ‘tidyseurat’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘SeuratObject’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Execution halted
ERROR: lazy loading failed for package ‘tidyseurat’
* removing ‘/tmp/workdir/tidyseurat/new/tidyseurat.Rcheck/tidyseurat’


```
### CRAN

```
* installing *source* package ‘tidyseurat’ ...
** package ‘tidyseurat’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘SeuratObject’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Execution halted
ERROR: lazy loading failed for package ‘tidyseurat’
* removing ‘/tmp/workdir/tidyseurat/old/tidyseurat.Rcheck/tidyseurat’


```
# tidyvpc

<details>

* Version: 1.5.1
* GitHub: https://github.com/certara/tidyvpc
* Source code: https://github.com/cran/tidyvpc
* Date/Publication: 2024-01-18 13:10:02 UTC
* Number of recursive dependencies: 176

Run `revdepcheck::cloud_details(, "tidyvpc")` for more info

</details>

## In both

*   checking whether package ‘tidyvpc’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/tidyvpc/new/tidyvpc.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘tidyvpc’ ...
** package ‘tidyvpc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘tidyvpc’
* removing ‘/tmp/workdir/tidyvpc/new/tidyvpc.Rcheck/tidyvpc’


```
### CRAN

```
* installing *source* package ‘tidyvpc’ ...
** package ‘tidyvpc’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘tidyvpc’
* removing ‘/tmp/workdir/tidyvpc/old/tidyvpc.Rcheck/tidyvpc’


```
# TriDimRegression

<details>

* Version: 1.0.2
* GitHub: https://github.com/alexander-pastukhov/tridim-regression
* Source code: https://github.com/cran/TriDimRegression
* Date/Publication: 2023-09-13 14:10:03 UTC
* Number of recursive dependencies: 99

Run `revdepcheck::cloud_details(, "TriDimRegression")` for more info

</details>

## In both

*   checking whether package ‘TriDimRegression’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/TriDimRegression/new/TriDimRegression.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘TriDimRegression’ ...
** package ‘TriDimRegression’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘TriDimRegression’
* removing ‘/tmp/workdir/TriDimRegression/new/TriDimRegression.Rcheck/TriDimRegression’


```
### CRAN

```
* installing *source* package ‘TriDimRegression’ ...
** package ‘TriDimRegression’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘TriDimRegression’
* removing ‘/tmp/workdir/TriDimRegression/old/TriDimRegression.Rcheck/TriDimRegression’


```
# TSrepr

<details>

* Version: 1.1.0
* GitHub: https://github.com/PetoLau/TSrepr
* Source code: https://github.com/cran/TSrepr
* Date/Publication: 2020-07-13 06:50:15 UTC
* Number of recursive dependencies: 72

Run `revdepcheck::cloud_details(, "TSrepr")` for more info

</details>

## In both

*   checking whether package ‘TSrepr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/TSrepr/new/TSrepr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘TSrepr’ ...
** package ‘TSrepr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c FeatureClippingTrending.cpp -o FeatureClippingTrending.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c helpers.cpp -o helpers.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c measures.cpp -o measures.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c normalizations.cpp -o normalizations.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘TSrepr’
* removing ‘/tmp/workdir/TSrepr/new/TSrepr.Rcheck/TSrepr’


```
### CRAN

```
* installing *source* package ‘TSrepr’ ...
** package ‘TSrepr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c FeatureClippingTrending.cpp -o FeatureClippingTrending.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c helpers.cpp -o helpers.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c measures.cpp -o measures.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/opt/R/4.3.1/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c normalizations.cpp -o normalizations.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘TSrepr’
* removing ‘/tmp/workdir/TSrepr/old/TSrepr.Rcheck/TSrepr’


```
# twang

<details>

* Version: 2.6
* GitHub: NA
* Source code: https://github.com/cran/twang
* Date/Publication: 2023-12-06 00:30:02 UTC
* Number of recursive dependencies: 53

Run `revdepcheck::cloud_details(, "twang")` for more info

</details>

## In both

*   checking whether package ‘twang’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/twang/new/twang.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘twang’ ...
** package ‘twang’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c init.c -o init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c ks.c -o ks.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o twang.so init.o ks.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/twang/new/twang.Rcheck/00LOCK-twang/00new/twang/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘twang’
* removing ‘/tmp/workdir/twang/new/twang.Rcheck/twang’


```
### CRAN

```
* installing *source* package ‘twang’ ...
** package ‘twang’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c init.c -o init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c ks.c -o ks.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o twang.so init.o ks.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/twang/old/twang.Rcheck/00LOCK-twang/00new/twang/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘twang’
* removing ‘/tmp/workdir/twang/old/twang.Rcheck/twang’


```
# vdg

<details>

* Version: 1.2.3
* GitHub: NA
* Source code: https://github.com/cran/vdg
* Date/Publication: 2024-04-23 13:00:02 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::cloud_details(, "vdg")` for more info

</details>

## In both

*   checking whether package ‘vdg’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/vdg/new/vdg.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘vdg’ ...
** package ‘vdg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using Fortran compiler: ‘GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gfortran  -fpic  -g -O2  -c FDS.f -o FDS.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o vdg.so FDS.o -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/vdg/new/vdg.Rcheck/00LOCK-vdg/00new/vdg/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘quantreg’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘vdg’
* removing ‘/tmp/workdir/vdg/new/vdg.Rcheck/vdg’


```
### CRAN

```
* installing *source* package ‘vdg’ ...
** package ‘vdg’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using Fortran compiler: ‘GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0’
gfortran  -fpic  -g -O2  -c FDS.f -o FDS.o
gcc -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o vdg.so FDS.o -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/vdg/old/vdg.Rcheck/00LOCK-vdg/00new/vdg/libs
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘quantreg’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Execution halted
ERROR: lazy loading failed for package ‘vdg’
* removing ‘/tmp/workdir/vdg/old/vdg.Rcheck/vdg’


```
# visa

<details>

* Version: 0.1.0
* GitHub: https://github.com/kang-yu/visa
* Source code: https://github.com/cran/visa
* Date/Publication: 2021-04-20 07:20:02 UTC
* Number of recursive dependencies: 140

Run `revdepcheck::cloud_details(, "visa")` for more info

</details>

## In both

*   checking whether package ‘visa’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/visa/new/visa.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘visa’ ...
** package ‘visa’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘visa’
* removing ‘/tmp/workdir/visa/new/visa.Rcheck/visa’


```
### CRAN

```
* installing *source* package ‘visa’ ...
** package ‘visa’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘visa’
* removing ‘/tmp/workdir/visa/old/visa.Rcheck/visa’


```
# WRTDStidal

<details>

* Version: 1.1.4
* GitHub: https://github.com/fawda123/WRTDStidal
* Source code: https://github.com/cran/WRTDStidal
* Date/Publication: 2023-10-20 09:00:11 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "WRTDStidal")` for more info

</details>

## In both

*   checking whether package ‘WRTDStidal’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/WRTDStidal/new/WRTDStidal.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘WRTDStidal’ ...
** package ‘WRTDStidal’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘WRTDStidal’
* removing ‘/tmp/workdir/WRTDStidal/new/WRTDStidal.Rcheck/WRTDStidal’


```
### CRAN

```
* installing *source* package ‘WRTDStidal’ ...
** package ‘WRTDStidal’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘WRTDStidal’
* removing ‘/tmp/workdir/WRTDStidal/old/WRTDStidal.Rcheck/WRTDStidal’


```
