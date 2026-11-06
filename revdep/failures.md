# b3gbi (1.1.3)

* GitHub: <https://github.com/b-cubed-eu/b3gbi>
* Email: <mailto:Shawn.Dove@allzool.bio.uni-giessen.de>
* GitHub mirror: <https://github.com/cran/b3gbi>

Run `revdepcheck::cloud_details(, "b3gbi")` for more info

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/b3gbi/new/b3gbi.Rcheck’
* using R version 4.6.0 (2026-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* current time: 2026-11-06 08:25:54 UTC
* using option ‘--no-manual’
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/b3gbi/old/b3gbi.Rcheck’
* using R version 4.6.0 (2026-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* current time: 2026-11-06 08:22:38 UTC
* using option ‘--no-manual’
...
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
# bayesdfa (1.3.4)

* GitHub: <https://github.com/fate-ewi/bayesdfa>
* Email: <mailto:eric.ward@noaa.gov>
* GitHub mirror: <https://github.com/cran/bayesdfa>

Run `revdepcheck::cloud_details(, "bayesdfa")` for more info

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
** this is package ‘bayesdfa’ version ‘1.3.4’
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/bayesdfa’


```
### CRAN

```
* installing *source* package ‘bayesdfa’ ...
** this is package ‘bayesdfa’ version ‘1.3.4’
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/old/bayesdfa.Rcheck/bayesdfa’


```
# ctsem (3.11.1)

* GitHub: <https://github.com/cdriveraus/ctsem>
* Email: <mailto:charles.driver2@uzh.ch>
* GitHub mirror: <https://github.com/cran/ctsem>

Run `revdepcheck::cloud_details(, "ctsem")` for more info

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
** this is package ‘ctsem’ version ‘3.11.1’
** package ‘ctsem’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
specified C++17
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
using C++17


...
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.6.0/lib/R/etc/Makeconf:197: stanExports_ctsm.o] Error 1
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/new/ctsem.Rcheck/ctsem’


```
### CRAN

```
* installing *source* package ‘ctsem’ ...
** this is package ‘ctsem’ version ‘3.11.1’
** package ‘ctsem’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
specified C++17
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
using C++17


...
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.6.0/lib/R/etc/Makeconf:197: stanExports_ctsm.o] Error 1
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/old/ctsem.Rcheck/ctsem’


```
# deepSTRAPP (1.1.1)

* GitHub: <https://github.com/MaelDore/deepSTRAPP>
* Email: <mailto:mael.dore@gmail.com>
* GitHub mirror: <https://github.com/cran/deepSTRAPP>

Run `revdepcheck::cloud_details(, "deepSTRAPP")` for more info

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/deepSTRAPP/new/deepSTRAPP.Rcheck’
* using R version 4.6.0 (2026-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* current time: 2026-11-06 08:26:56 UTC
* using option ‘--no-manual’
...
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/deepSTRAPP/old/deepSTRAPP.Rcheck’
* using R version 4.6.0 (2026-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* current time: 2026-11-06 08:22:05 UTC
* using option ‘--no-manual’
...
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
# GeneralizedUmatrixGPU (0.1.14)

* Email: <mailto:Quirin_Stier@gmx.de>
* GitHub mirror: <https://github.com/cran/GeneralizedUmatrixGPU>

Run `revdepcheck::cloud_details(, "GeneralizedUmatrixGPU")` for more info

## In both

*   checking whether package ‘GeneralizedUmatrixGPU’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/GeneralizedUmatrixGPU/new/GeneralizedUmatrixGPU.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘GeneralizedUmatrixGPU’ ...
** this is package ‘GeneralizedUmatrixGPU’ version ‘0.1.14’
** package ‘GeneralizedUmatrixGPU’ successfully unpacked and MD5 sums checked
** using staged installation
configure: building makevars
System detected: Linux
End of configure
** libs
specified C++17
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...
g++ -std=gnu++17 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2   -c trainSESOM.cpp -o trainSESOM.o
In file included from trainSESOM.cpp:2:
ocl.h:9:10: fatal error: CL/cl.h: No such file or directory
    9 | #include <CL/cl.h>
      |          ^~~~~~~~~
compilation terminated.
make: *** [/opt/R/4.6.0/lib/R/etc/Makeconf:199: trainSESOM.o] Error 1
ERROR: compilation failed for package ‘GeneralizedUmatrixGPU’
* removing ‘/tmp/workdir/GeneralizedUmatrixGPU/new/GeneralizedUmatrixGPU.Rcheck/GeneralizedUmatrixGPU’


```
### CRAN

```
* installing *source* package ‘GeneralizedUmatrixGPU’ ...
** this is package ‘GeneralizedUmatrixGPU’ version ‘0.1.14’
** package ‘GeneralizedUmatrixGPU’ successfully unpacked and MD5 sums checked
** using staged installation
configure: building makevars
System detected: Linux
End of configure
** libs
specified C++17
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
...
g++ -std=gnu++17 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppArmadillo/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -fpic  -g -O2   -c trainSESOM.cpp -o trainSESOM.o
In file included from trainSESOM.cpp:2:
ocl.h:9:10: fatal error: CL/cl.h: No such file or directory
    9 | #include <CL/cl.h>
      |          ^~~~~~~~~
compilation terminated.
make: *** [/opt/R/4.6.0/lib/R/etc/Makeconf:199: trainSESOM.o] Error 1
ERROR: compilation failed for package ‘GeneralizedUmatrixGPU’
* removing ‘/tmp/workdir/GeneralizedUmatrixGPU/old/GeneralizedUmatrixGPU.Rcheck/GeneralizedUmatrixGPU’


```
# multinma (0.9.1)

* GitHub: <https://github.com/dmphillippo/multinma>
* Email: <mailto:david.phillippo@bristol.ac.uk>
* GitHub mirror: <https://github.com/cran/multinma>

Run `revdepcheck::cloud_details(, "multinma")` for more info

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
** this is package ‘multinma’ version ‘0.9.1’
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
specified C++17
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
using C++17


...
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_survival_param_namespace::model_survival_param; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.6.0/lib/R/etc/Makeconf:197: stanExports_survival_param.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/new/multinma.Rcheck/multinma’


```
### CRAN

```
* installing *source* package ‘multinma’ ...
** this is package ‘multinma’ version ‘0.9.1’
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
specified C++17
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
using C++17


...
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_survival_param_namespace::model_survival_param; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/tmp/r-deps/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/tmp/r-deps/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.6.0/lib/R/etc/Makeconf:197: stanExports_survival_param.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/old/multinma.Rcheck/multinma’


```
# NMF (0.28)

* Email: <mailto:nicolas.sauwen@openanalytics.eu>
* GitHub mirror: <https://github.com/cran/NMF>

Run `revdepcheck::cloud_details(, "NMF")` for more info

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/NMF/new/NMF.Rcheck’
* using R version 4.6.0 (2026-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* current time: 2026-11-06 08:26:13 UTC
* using option ‘--no-manual’
...
--- failed re-building ‘heatmaps.Rnw’

SUMMARY: processing the following files failed:
  ‘NMF-vignette.Rnw’ ‘heatmaps.Rnw’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 WARNING





```
### CRAN

```
* using log directory ‘/tmp/workdir/NMF/old/NMF.Rcheck’
* using R version 4.6.0 (2026-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* current time: 2026-11-06 08:21:09 UTC
* using option ‘--no-manual’
...
--- failed re-building ‘heatmaps.Rnw’

SUMMARY: processing the following files failed:
  ‘NMF-vignette.Rnw’ ‘heatmaps.Rnw’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 WARNING





```
# rmsb (1.1-3)

* Email: <mailto:fh@fharrell.com>
* GitHub mirror: <https://github.com/cran/rmsb>

Run `revdepcheck::cloud_details(, "rmsb")` for more info

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
** this is package ‘rmsb’ version ‘1.1-3’
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
** this is package ‘rmsb’ version ‘1.1-3’
** package ‘rmsb’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rmsb’
* removing ‘/tmp/workdir/rmsb/old/rmsb.Rcheck/rmsb’


```
# scoper (1.6.0)

* GitHub: <https://github.com/immcantation/scoper>
* Email: <mailto:susanna.marquez@yale.edu>
* GitHub mirror: <https://github.com/cran/scoper>

Run `revdepcheck::cloud_details(, "scoper")` for more info

## In both

*   checking whether package ‘scoper’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/scoper/new/scoper.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘scoper’ ...
** this is package ‘scoper’ version ‘1.6.0’
** package ‘scoper’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c RcppMutation.cpp -o RcppMutation.o
g++ -std=gnu++20 -shared -L/opt/R/4.6.0/lib/R/lib -L/usr/local/lib -o scoper.so RcppExports.o RcppMutation.o -L/opt/R/4.6.0/lib/R/lib -lR
installing to /tmp/workdir/scoper/new/scoper.Rcheck/00LOCK-scoper/00new/scoper/libs
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘Biostrings’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘scoper’
* removing ‘/tmp/workdir/scoper/new/scoper.Rcheck/scoper’


```
### CRAN

```
* installing *source* package ‘scoper’ ...
** this is package ‘scoper’ version ‘1.6.0’
** package ‘scoper’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG  -I'/tmp/r-deps/Rcpp/include' -I/usr/local/include    -fpic  -g -O2   -c RcppMutation.cpp -o RcppMutation.o
g++ -std=gnu++20 -shared -L/opt/R/4.6.0/lib/R/lib -L/usr/local/lib -o scoper.so RcppExports.o RcppMutation.o -L/opt/R/4.6.0/lib/R/lib -lR
installing to /tmp/workdir/scoper/old/scoper.Rcheck/00LOCK-scoper/00new/scoper/libs
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘Biostrings’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘scoper’
* removing ‘/tmp/workdir/scoper/old/scoper.Rcheck/scoper’


```
# shazam (1.4.0)

* GitHub: <https://github.com/immcantation/shazam>
* Email: <mailto:susanna.marquez@yale.edu>
* GitHub mirror: <https://github.com/cran/shazam>

Run `revdepcheck::cloud_details(, "shazam")` for more info

## In both

*   checking whether package ‘shazam’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/shazam/new/shazam.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘shazam’ ...
** this is package ‘shazam’ version ‘1.4.0’
** package ‘shazam’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘Biostrings’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘shazam’
* removing ‘/tmp/workdir/shazam/new/shazam.Rcheck/shazam’


```
### CRAN

```
* installing *source* package ‘shazam’ ...
** this is package ‘shazam’ version ‘1.4.0’
** package ‘shazam’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘Biostrings’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘shazam’
* removing ‘/tmp/workdir/shazam/old/shazam.Rcheck/shazam’


```
# ss3sim (1.0.3)

* GitHub: <https://github.com/ss3sim/ss3sim>
* Email: <mailto:kelli.johnson@noaa.gov>
* GitHub mirror: <https://github.com/cran/ss3sim>

Run `revdepcheck::cloud_details(, "ss3sim")` for more info

## In both

*   checking whether package ‘ss3sim’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/ss3sim/new/ss3sim.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘ss3sim’ ...
** this is package ‘ss3sim’ version ‘1.0.3’
** package ‘ss3sim’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘SS_changepars’ is not exported by 'namespace:r4ss'
Execution halted
ERROR: lazy loading failed for package ‘ss3sim’
* removing ‘/tmp/workdir/ss3sim/new/ss3sim.Rcheck/ss3sim’


```
### CRAN

```
* installing *source* package ‘ss3sim’ ...
** this is package ‘ss3sim’ version ‘1.0.3’
** package ‘ss3sim’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘SS_changepars’ is not exported by 'namespace:r4ss'
Execution halted
ERROR: lazy loading failed for package ‘ss3sim’
* removing ‘/tmp/workdir/ss3sim/old/ss3sim.Rcheck/ss3sim’


```
# streamDAG (1.6)

* Email: <mailto:ahoken@isu.edu>
* GitHub mirror: <https://github.com/cran/streamDAG>

Run `revdepcheck::cloud_details(, "streamDAG")` for more info

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/streamDAG/new/streamDAG.Rcheck’
* using R version 4.6.0 (2026-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* current time: 2026-11-06 08:30:33 UTC
* using option ‘--no-manual’
...
* checking for file ‘streamDAG/DESCRIPTION’ ... OK
* this is package ‘streamDAG’ version ‘1.6’
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
* using R version 4.6.0 (2026-04-24)
* using platform: x86_64-pc-linux-gnu
* R was compiled by
    gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
    GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* current time: 2026-11-06 08:29:58 UTC
* using option ‘--no-manual’
...
* checking for file ‘streamDAG/DESCRIPTION’ ... OK
* this is package ‘streamDAG’ version ‘1.6’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘asbio’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# tigger (1.2.0)

* GitHub: <https://github.com/immcantation/tigger>
* Email: <mailto:susanna.marquez@yale.edu>
* GitHub mirror: <https://github.com/cran/tigger>

Run `revdepcheck::cloud_details(, "tigger")` for more info

## In both

*   checking whether package ‘tigger’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/tigger/new/tigger.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘tigger’ ...
** this is package ‘tigger’ version ‘1.2.0’
** package ‘tigger’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘Biostrings’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘tigger’
* removing ‘/tmp/workdir/tigger/new/tigger.Rcheck/tigger’


```
### CRAN

```
* installing *source* package ‘tigger’ ...
** this is package ‘tigger’ version ‘1.2.0’
** package ‘tigger’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘Biostrings’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘tigger’
* removing ‘/tmp/workdir/tigger/old/tigger.Rcheck/tigger’


```
# TriDimRegression (1.0.3)

* GitHub: <https://github.com/alexander-pastukhov/tridim-regression>
* Email: <mailto:pastukhov.alexander@gmail.com>
* GitHub mirror: <https://github.com/cran/TriDimRegression>

Run `revdepcheck::cloud_details(, "TriDimRegression")` for more info

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
** this is package ‘TriDimRegression’ version ‘1.0.3’
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
** this is package ‘TriDimRegression’ version ‘1.0.3’
** package ‘TriDimRegression’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘TriDimRegression’
* removing ‘/tmp/workdir/TriDimRegression/old/TriDimRegression.Rcheck/TriDimRegression’


```
# valr (0.10.0)

* GitHub: <https://github.com/rnabioco/valr>
* Email: <mailto:jay.hesselberth@gmail.com>
* GitHub mirror: <https://github.com/cran/valr>

Run `revdepcheck::cloud_details(, "valr")` for more info

## In both

*   checking whether package ‘valr’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/valr/new/valr.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘valr’ ...
** this is package ‘valr’ version ‘0.10.0’
** package ‘valr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -I'/tmp/r-deps/cpp11/include' -I/usr/local/include    -fpic  -g -O2   -c bed12toexons.cpp -o bed12toexons.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -I'/tmp/r-deps/cpp11/include' -I/usr/local/include    -fpic  -g -O2   -c closest.cpp -o closest.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -I'/tmp/r-deps/cpp11/include' -I/usr/local/include    -fpic  -g -O2   -c complement.cpp -o complement.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -I'/tmp/r-deps/cpp11/include' -I/usr/local/include    -fpic  -g -O2   -c coverage.cpp -o coverage.o
...
installing to /tmp/workdir/valr/new/valr.Rcheck/00LOCK-valr/00new/valr/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GenomicRanges’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘valr’
* removing ‘/tmp/workdir/valr/new/valr.Rcheck/valr’


```
### CRAN

```
* installing *source* package ‘valr’ ...
** this is package ‘valr’ version ‘0.10.0’
** package ‘valr’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -I'/tmp/r-deps/cpp11/include' -I/usr/local/include    -fpic  -g -O2   -c bed12toexons.cpp -o bed12toexons.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -I'/tmp/r-deps/cpp11/include' -I/usr/local/include    -fpic  -g -O2   -c closest.cpp -o closest.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -I'/tmp/r-deps/cpp11/include' -I/usr/local/include    -fpic  -g -O2   -c complement.cpp -o complement.o
g++ -std=gnu++20 -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG -I../inst/include -I'/tmp/r-deps/cpp11/include' -I/usr/local/include    -fpic  -g -O2   -c coverage.cpp -o coverage.o
...
installing to /tmp/workdir/valr/old/valr.Rcheck/00LOCK-valr/00new/valr/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GenomicRanges’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘valr’
* removing ‘/tmp/workdir/valr/old/valr.Rcheck/valr’


```
# zenplots (1.0.7)

* GitHub: <https://github.com/great-northern-diver/zenplots>
* Email: <mailto:rwoldford@uwaterloo.ca>
* GitHub mirror: <https://github.com/cran/zenplots>

Run `revdepcheck::cloud_details(, "zenplots")` for more info

## In both

*   checking whether package ‘zenplots’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/zenplots/new/zenplots.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘zenplots’ ...
** this is package ‘zenplots’ version ‘1.0.7’
** package ‘zenplots’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
gcc -std=gnu2x -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c col_split.c -o col_split.o
gcc -std=gnu2x -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c init.c -o init.o
gcc -std=gnu2x -shared -L/opt/R/4.6.0/lib/R/lib -L/usr/local/lib -o zenplots.so col_split.o init.o -L/opt/R/4.6.0/lib/R/lib -lR
installing to /tmp/workdir/zenplots/new/zenplots.Rcheck/00LOCK-zenplots/00new/zenplots/libs
...
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘graph’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘zenplots’
* removing ‘/tmp/workdir/zenplots/new/zenplots.Rcheck/zenplots’


```
### CRAN

```
* installing *source* package ‘zenplots’ ...
** this is package ‘zenplots’ version ‘1.0.7’
** package ‘zenplots’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0’
gcc -std=gnu2x -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c col_split.c -o col_split.o
gcc -std=gnu2x -I"/opt/R/4.6.0/lib/R/include" -DNDEBUG   -I/usr/local/include    -fpic  -g -O2  -c init.c -o init.o
gcc -std=gnu2x -shared -L/opt/R/4.6.0/lib/R/lib -L/usr/local/lib -o zenplots.so col_split.o init.o -L/opt/R/4.6.0/lib/R/lib -lR
installing to /tmp/workdir/zenplots/old/zenplots.Rcheck/00LOCK-zenplots/00new/zenplots/libs
...
** data
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘graph’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘zenplots’
* removing ‘/tmp/workdir/zenplots/old/zenplots.Rcheck/zenplots’


```
