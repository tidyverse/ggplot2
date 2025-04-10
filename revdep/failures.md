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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
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
# adjustedCurves

<details>

* Version: 0.11.2
* GitHub: https://github.com/RobinDenz1/adjustedCurves
* Source code: https://github.com/cran/adjustedCurves
* Date/Publication: 2024-07-29 14:30:02 UTC
* Number of recursive dependencies: 178

Run `revdepcheck::cloud_details(, "adjustedCurves")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/adjustedCurves/new/adjustedCurves.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘adjustedCurves/DESCRIPTION’ ... OK
...
--- finished re-building ‘plot_customization.rmd’

SUMMARY: processing the following file failed:
  ‘introduction.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 2 ERRORs, 1 WARNING, 3 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/adjustedCurves/old/adjustedCurves.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘adjustedCurves/DESCRIPTION’ ... OK
...
--- finished re-building ‘plot_customization.rmd’

SUMMARY: processing the following file failed:
  ‘introduction.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 2 ERRORs, 1 WARNING, 3 NOTEs





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
* Number of recursive dependencies: 63

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
# aorsf

<details>

* Version: 0.1.5
* GitHub: https://github.com/ropensci/aorsf
* Source code: https://github.com/cran/aorsf
* Date/Publication: 2024-05-30 03:40:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "aorsf")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/aorsf/new/aorsf.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘aorsf/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘aorsf.Rmd’ using ‘UTF-8’... OK
  ‘fast.Rmd’ using ‘UTF-8’... OK
  ‘oobag.Rmd’ using ‘UTF-8’... OK
  ‘pd.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/aorsf/old/aorsf.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘aorsf/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘aorsf.Rmd’ using ‘UTF-8’... OK
  ‘fast.Rmd’ using ‘UTF-8’... OK
  ‘oobag.Rmd’ using ‘UTF-8’... OK
  ‘pd.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# atRisk

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/atRisk
* Date/Publication: 2025-01-14 18:50:01 UTC
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
# autoReg

<details>

* Version: 0.3.3
* GitHub: https://github.com/cardiomoon/autoReg
* Source code: https://github.com/cran/autoReg
* Date/Publication: 2023-11-14 05:53:27 UTC
* Number of recursive dependencies: 218

Run `revdepcheck::cloud_details(, "autoReg")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/autoReg/new/autoReg.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘autoReg/DESCRIPTION’ ... OK
...
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘Automatic_Regression_Modeling.Rmd’ using ‘UTF-8’... OK
  ‘Bootstrap_Prediction.Rmd’ using ‘UTF-8’... OK
  ‘Getting_started.Rmd’ using ‘UTF-8’... OK
  ‘Statiastical_test_in_gaze.Rmd’ using ‘UTF-8’... OK
  ‘Survival.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/autoReg/old/autoReg.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘autoReg/DESCRIPTION’ ... OK
...
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘Automatic_Regression_Modeling.Rmd’ using ‘UTF-8’... OK
  ‘Bootstrap_Prediction.Rmd’ using ‘UTF-8’... OK
  ‘Getting_started.Rmd’ using ‘UTF-8’... OK
  ‘Statiastical_test_in_gaze.Rmd’ using ‘UTF-8’... OK
  ‘Survival.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
# AutoScore

<details>

* Version: 1.0.0
* GitHub: https://github.com/nliulab/AutoScore
* Source code: https://github.com/cran/AutoScore
* Date/Publication: 2022-10-15 22:15:26 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "AutoScore")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/AutoScore/new/AutoScore.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AutoScore/DESCRIPTION’ ... OK
...
* this is package ‘AutoScore’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘survAUC’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/AutoScore/old/AutoScore.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘AutoScore/DESCRIPTION’ ... OK
...
* this is package ‘AutoScore’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘survAUC’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# bayesdfa

<details>

* Version: 1.3.4
* GitHub: https://github.com/fate-ewi/bayesdfa
* Source code: https://github.com/cran/bayesdfa
* Date/Publication: 2025-03-22 20:30:21 UTC
* Number of recursive dependencies: 87

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
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/new/bayesdfa.Rcheck/bayesdfa’


```
### CRAN

```
* installing *source* package ‘bayesdfa’ ...
** package ‘bayesdfa’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘bayesdfa’
* removing ‘/tmp/workdir/bayesdfa/old/bayesdfa.Rcheck/bayesdfa’


```
# bayesDP

<details>

* Version: 1.3.7
* GitHub: https://github.com/graemeleehickey/bayesDP
* Source code: https://github.com/cran/bayesDP
* Date/Publication: 2025-01-12 11:40:10 UTC
* Number of recursive dependencies: 79

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c bdplm.cpp -o bdplm.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c ppexp.cpp -o ppexp.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c bdplm.cpp -o bdplm.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c ppexp.cpp -o ppexp.o
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
# BayesGrowth

<details>

* Version: 1.0.0
* GitHub: https://github.com/jonathansmart/BayesGrowth
* Source code: https://github.com/cran/BayesGrowth
* Date/Publication: 2023-11-21 18:10:08 UTC
* Number of recursive dependencies: 109

Run `revdepcheck::cloud_details(, "BayesGrowth")` for more info

</details>

## In both

*   checking whether package ‘BayesGrowth’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BayesGrowth/new/BayesGrowth.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BayesGrowth’ ...
** package ‘BayesGrowth’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
Warning: namespace ‘BayesGrowth’ is not available and has been replaced
by .GlobalEnv when processing object ‘MCMC_example_results’
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘MuMIn’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘BayesGrowth’
* removing ‘/tmp/workdir/BayesGrowth/new/BayesGrowth.Rcheck/BayesGrowth’


```
### CRAN

```
* installing *source* package ‘BayesGrowth’ ...
** package ‘BayesGrowth’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
Warning: namespace ‘BayesGrowth’ is not available and has been replaced
by .GlobalEnv when processing object ‘MCMC_example_results’
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘MuMIn’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘BayesGrowth’
* removing ‘/tmp/workdir/BayesGrowth/old/BayesGrowth.Rcheck/BayesGrowth’


```
# BayesianFactorZoo

<details>

* Version: 0.0.0.3
* GitHub: NA
* Source code: https://github.com/cran/BayesianFactorZoo
* Date/Publication: 2024-10-04 09:30:08 UTC
* Number of recursive dependencies: 74

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

* Version: 0.1.0
* GitHub: https://github.com/ocbe-uio/BayesSurvive
* Source code: https://github.com/cran/BayesSurvive
* Date/Publication: 2025-03-25 22:50:23 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "BayesSurvive")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/BayesSurvive/new/BayesSurvive.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BayesSurvive/DESCRIPTION’ ... OK
...
* this is package ‘BayesSurvive’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/BayesSurvive/old/BayesSurvive.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BayesSurvive/DESCRIPTION’ ... OK
...
* this is package ‘BayesSurvive’ version ‘0.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# BCClong

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/BCClong
* Date/Publication: 2024-06-24 00:00:02 UTC
* Number of recursive dependencies: 149

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c BCC.cpp -o BCC.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c Likelihood.cpp -o Likelihood.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c c_which.cpp -o c_which.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c BCC.cpp -o BCC.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c Likelihood.cpp -o Likelihood.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c c_which.cpp -o c_which.o
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

* Version: 2.1.5
* GitHub: https://github.com/donaldRwilliams/BGGM
* Source code: https://github.com/cran/BGGM
* Date/Publication: 2024-12-22 21:40:02 UTC
* Number of recursive dependencies: 211

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
configure: creating ./config.status
config.status: creating src/Makevars
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_NO_DEBUG -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppDist/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_NO_DEBUG -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppDist/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2  -c bggm_fast.cpp -o bggm_fast.o
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
configure: creating ./config.status
config.status: creating src/Makevars
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_NO_DEBUG -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppDist/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_NO_DEBUG -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppDist/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include   -I../inst/include -fpic  -g -O2  -c bggm_fast.cpp -o bggm_fast.o
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

* Version: 1.1
* GitHub: NA
* Source code: https://github.com/cran/binsreg
* Date/Publication: 2024-07-23 14:30:01 UTC
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
# bspcov

<details>

* Version: 1.0.1
* GitHub: https://github.com/statjs/bspcov
* Source code: https://github.com/cran/bspcov
* Date/Publication: 2024-11-13 20:10:02 UTC
* Number of recursive dependencies: 112

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
# BSTZINB

<details>

* Version: 2.0.0
* GitHub: https://github.com/SumanM47/BSTZINB
* Source code: https://github.com/cran/BSTZINB
* Date/Publication: 2025-01-30 17:50:07 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "BSTZINB")` for more info

</details>

## In both

*   checking whether package ‘BSTZINB’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/BSTZINB/new/BSTZINB.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BSTZINB’ ...
** package ‘BSTZINB’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘BSTZINB’
* removing ‘/tmp/workdir/BSTZINB/new/BSTZINB.Rcheck/BSTZINB’


```
### CRAN

```
* installing *source* package ‘BSTZINB’ ...
** package ‘BSTZINB’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘BSTZINB’
* removing ‘/tmp/workdir/BSTZINB/old/BSTZINB.Rcheck/BSTZINB’


```
# BuyseTest

<details>

* Version: 3.1.0
* GitHub: https://github.com/bozenne/BuyseTest
* Source code: https://github.com/cran/BuyseTest
* Date/Publication: 2025-03-04 14:40:02 UTC
* Number of recursive dependencies: 132

Run `revdepcheck::cloud_details(, "BuyseTest")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/BuyseTest/new/BuyseTest.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BuyseTest/DESCRIPTION’ ... OK
...
* this is package ‘BuyseTest’ version ‘3.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/BuyseTest/old/BuyseTest.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘BuyseTest/DESCRIPTION’ ... OK
...
* this is package ‘BuyseTest’ version ‘3.1.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# CAESAR.Suite

<details>

* Version: 0.2.2
* GitHub: https://github.com/XiaoZhangryy/CAESAR.Suite
* Source code: https://github.com/cran/CAESAR.Suite
* Date/Publication: 2025-04-01 09:00:07 UTC
* Number of recursive dependencies: 255

Run `revdepcheck::cloud_details(, "CAESAR.Suite")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/CAESAR.Suite/new/CAESAR.Suite.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘CAESAR.Suite/DESCRIPTION’ ... OK
...
* this is package ‘CAESAR.Suite’ version ‘0.2.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘ProFAST’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/CAESAR.Suite/old/CAESAR.Suite.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘CAESAR.Suite/DESCRIPTION’ ... OK
...
* this is package ‘CAESAR.Suite’ version ‘0.2.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘ProFAST’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# CARBayesST

<details>

* Version: 4.0
* GitHub: https://github.com/duncanplee/CARBayesST
* Source code: https://github.com/cran/CARBayesST
* Date/Publication: 2023-10-30 16:40:02 UTC
* Number of recursive dependencies: 106

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CARBayesST.cpp -o CARBayesST.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CARBayesST.cpp -o CARBayesST.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
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
# Certara.VPCResults

<details>

* Version: 3.0.2
* GitHub: NA
* Source code: https://github.com/cran/Certara.VPCResults
* Date/Publication: 2024-12-02 15:30:02 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "Certara.VPCResults")` for more info

</details>

## In both

*   checking whether package ‘Certara.VPCResults’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/Certara.VPCResults/new/Certara.VPCResults.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Certara.VPCResults’ ...
** package ‘Certara.VPCResults’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Certara.VPCResults’
* removing ‘/tmp/workdir/Certara.VPCResults/new/Certara.VPCResults.Rcheck/Certara.VPCResults’


```
### CRAN

```
* installing *source* package ‘Certara.VPCResults’ ...
** package ‘Certara.VPCResults’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘Certara.VPCResults’
* removing ‘/tmp/workdir/Certara.VPCResults/old/Certara.VPCResults.Rcheck/Certara.VPCResults’


```
# CGPfunctions

<details>

* Version: 0.6.3
* GitHub: https://github.com/ibecav/CGPfunctions
* Source code: https://github.com/cran/CGPfunctions
* Date/Publication: 2020-11-12 14:50:09 UTC
* Number of recursive dependencies: 155

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
# cia

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/cia
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "cia")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# CIDER

<details>

* Version: 0.99.4
* GitHub: https://github.com/zhiyuan-hu-lab/CIDER
* Source code: https://github.com/cran/CIDER
* Date/Publication: 2025-02-07 09:50:14 UTC
* Number of recursive dependencies: 168

Run `revdepcheck::cloud_details(, "CIDER")` for more info

</details>

## In both

*   checking whether package ‘CIDER’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/CIDER/new/CIDER.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CIDER’ ...
** package ‘CIDER’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘CIDER’
* removing ‘/tmp/workdir/CIDER/new/CIDER.Rcheck/CIDER’


```
### CRAN

```
* installing *source* package ‘CIDER’ ...
** package ‘CIDER’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘CIDER’
* removing ‘/tmp/workdir/CIDER/old/CIDER.Rcheck/CIDER’


```
# cinaR

<details>

* Version: 0.2.3
* GitHub: https://github.com/eonurk/cinaR
* Source code: https://github.com/cran/cinaR
* Date/Publication: 2022-05-18 14:00:09 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::cloud_details(, "cinaR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/cinaR/new/cinaR.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cinaR/DESCRIPTION’ ... OK
...
* this is package ‘cinaR’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'ChIPseeker', 'fgsea'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/cinaR/old/cinaR.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘cinaR/DESCRIPTION’ ... OK
...
* this is package ‘cinaR’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'ChIPseeker', 'fgsea'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# ClusterGVis

<details>

* Version: 0.1.2
* GitHub: https://github.com/junjunlab/ClusterGVis
* Source code: https://github.com/cran/ClusterGVis
* Date/Publication: 2025-02-14 14:30:13 UTC
* Number of recursive dependencies: 297

Run `revdepcheck::cloud_details(, "ClusterGVis")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ClusterGVis/new/ClusterGVis.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClusterGVis/DESCRIPTION’ ... OK
...
* this is package ‘ClusterGVis’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/ClusterGVis/old/ClusterGVis.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ClusterGVis/DESCRIPTION’ ... OK
...
* this is package ‘ClusterGVis’ version ‘0.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





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
# COMMA

<details>

* Version: 1.1.1
* GitHub: NA
* Source code: https://github.com/cran/COMMA
* Date/Publication: 2024-12-13 21:10:02 UTC
* Number of recursive dependencies: 73

Run `revdepcheck::cloud_details(, "COMMA")` for more info

</details>

## In both

*   checking whether package ‘COMMA’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/COMMA/new/COMMA.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘COMMA’ ...
** package ‘COMMA’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘COMMA’
* removing ‘/tmp/workdir/COMMA/new/COMMA.Rcheck/COMMA’


```
### CRAN

```
* installing *source* package ‘COMMA’ ...
** package ‘COMMA’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘COMMA’
* removing ‘/tmp/workdir/COMMA/old/COMMA.Rcheck/COMMA’


```
# contsurvplot

<details>

* Version: 0.2.1
* GitHub: https://github.com/RobinDenz1/contsurvplot
* Source code: https://github.com/cran/contsurvplot
* Date/Publication: 2023-08-15 08:00:03 UTC
* Number of recursive dependencies: 157

Run `revdepcheck::cloud_details(, "contsurvplot")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/contsurvplot/new/contsurvplot.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘contsurvplot/DESCRIPTION’ ... OK
...
* this is package ‘contsurvplot’ version ‘0.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/contsurvplot/old/contsurvplot.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘contsurvplot/DESCRIPTION’ ... OK
...
* this is package ‘contsurvplot’ version ‘0.2.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# counterfactuals

<details>

* Version: 0.1.6
* GitHub: https://github.com/dandls/counterfactuals
* Source code: https://github.com/cran/counterfactuals
* Date/Publication: 2024-10-17 12:00:06 UTC
* Number of recursive dependencies: 218

Run `revdepcheck::cloud_details(, "counterfactuals")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/counterfactuals/new/counterfactuals.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘counterfactuals/DESCRIPTION’ ... OK
...
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘how-to-add-new-cf-methods.html.asis’ using ‘UTF-8’... OK
  ‘introduction.html.asis’ using ‘UTF-8’... OK
  ‘other_models.html.asis’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/counterfactuals/old/counterfactuals.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘counterfactuals/DESCRIPTION’ ... OK
...
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘how-to-add-new-cf-methods.html.asis’ using ‘UTF-8’... OK
  ‘introduction.html.asis’ using ‘UTF-8’... OK
  ‘other_models.html.asis’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# CRMetrics

<details>

* Version: 0.3.2
* GitHub: https://github.com/khodosevichlab/CRMetrics
* Source code: https://github.com/cran/CRMetrics
* Date/Publication: 2024-11-08 00:20:06 UTC
* Number of recursive dependencies: 243

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
# ctsem

<details>

* Version: 3.10.2
* GitHub: https://github.com/cdriveraus/ctsem
* Source code: https://github.com/cran/ctsem
* Date/Publication: 2025-01-13 11:00:08 UTC
* Number of recursive dependencies: 160

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ctsm_namespace::model_ctsm; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_ctsm.o] Error 1
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/old/ctsem.Rcheck/ctsem’


```
# DamageDetective

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/DamageDetective
* Number of recursive dependencies: 177

Run `revdepcheck::cloud_details(, "DamageDetective")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# dartR.base

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/dartR.base
* Date/Publication: 2025-03-04 03:40:02 UTC
* Number of recursive dependencies: 289

Run `revdepcheck::cloud_details(, "dartR.base")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dartR.base/new/dartR.base.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.base/DESCRIPTION’ ... OK
...
* this is package ‘dartR.base’ version ‘1.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘SNPassoc’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/dartR.base/old/dartR.base.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.base/DESCRIPTION’ ... OK
...
* this is package ‘dartR.base’ version ‘1.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘SNPassoc’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# dartR.captive

<details>

* Version: 1.0.2
* GitHub: NA
* Source code: https://github.com/cran/dartR.captive
* Date/Publication: 2025-02-18 08:20:05 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::cloud_details(, "dartR.captive")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dartR.captive/new/dartR.captive.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.captive/DESCRIPTION’ ... OK
...
* this is package ‘dartR.captive’ version ‘1.0.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'dartR.base', 'dartR.sim'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/dartR.captive/old/dartR.captive.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.captive/DESCRIPTION’ ... OK
...
* this is package ‘dartR.captive’ version ‘1.0.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'dartR.base', 'dartR.sim'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# dartR.popgen

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/dartR.popgen
* Date/Publication: 2024-06-27 23:20:04 UTC
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "dartR.popgen")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dartR.popgen/new/dartR.popgen.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.popgen/DESCRIPTION’ ... OK
...
* this is package ‘dartR.popgen’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘dartR.base’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/dartR.popgen/old/dartR.popgen.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.popgen/DESCRIPTION’ ... OK
...
* this is package ‘dartR.popgen’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘dartR.base’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# dartR.sexlinked

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/dartR.sexlinked
* Date/Publication: 2024-06-24 15:40:02 UTC
* Number of recursive dependencies: 125

Run `revdepcheck::cloud_details(, "dartR.sexlinked")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dartR.sexlinked/new/dartR.sexlinked.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.sexlinked/DESCRIPTION’ ... OK
...
* this is package ‘dartR.sexlinked’ version ‘1.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘dartR.base’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/dartR.sexlinked/old/dartR.sexlinked.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.sexlinked/DESCRIPTION’ ... OK
...
* this is package ‘dartR.sexlinked’ version ‘1.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘dartR.base’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# dartR.sim

<details>

* Version: 0.70
* GitHub: https://github.com/green-striped-gecko/dartR.sim
* Source code: https://github.com/cran/dartR.sim
* Date/Publication: 2023-11-20 19:30:02 UTC
* Number of recursive dependencies: 138

Run `revdepcheck::cloud_details(, "dartR.sim")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dartR.sim/new/dartR.sim.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.sim/DESCRIPTION’ ... OK
...
* this is package ‘dartR.sim’ version ‘0.70’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘dartR.base’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/dartR.sim/old/dartR.sim.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.sim/DESCRIPTION’ ... OK
...
* this is package ‘dartR.sim’ version ‘0.70’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘dartR.base’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# dartR.spatial

<details>

* Version: 0.78
* GitHub: NA
* Source code: https://github.com/cran/dartR.spatial
* Date/Publication: 2023-11-15 00:50:02 UTC
* Number of recursive dependencies: 171

Run `revdepcheck::cloud_details(, "dartR.spatial")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dartR.spatial/new/dartR.spatial.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.spatial/DESCRIPTION’ ... OK
...
* this is package ‘dartR.spatial’ version ‘0.78’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘dartR.base’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/dartR.spatial/old/dartR.spatial.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dartR.spatial/DESCRIPTION’ ... OK
...
* this is package ‘dartR.spatial’ version ‘0.78’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘dartR.base’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# DepthProc

<details>

* Version: 2.1.5
* GitHub: https://github.com/zzawadz/DepthProc
* Source code: https://github.com/cran/DepthProc
* Date/Publication: 2022-02-03 20:30:02 UTC
* Number of recursive dependencies: 131

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c Depth.cpp -o Depth.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationEstimators.cpp -o LocationEstimators.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationScaleDepth.cpp -o LocationScaleDepth.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationScaleDepthCPP.cpp -o LocationScaleDepthCPP.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c Depth.cpp -o Depth.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationEstimators.cpp -o LocationEstimators.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationScaleDepth.cpp -o LocationScaleDepth.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c LocationScaleDepthCPP.cpp -o LocationScaleDepthCPP.o
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
# DFD

<details>

* Version: 0.3.0
* GitHub: https://github.com/MohmedSoudy/DFD
* Source code: https://github.com/cran/DFD
* Date/Publication: 2025-02-11 13:50:11 UTC
* Number of recursive dependencies: 207

Run `revdepcheck::cloud_details(, "DFD")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/DFD/new/DFD.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘DFD/DESCRIPTION’ ... OK
...
* this is package ‘DFD’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘signatureSearch’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/DFD/old/DFD.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘DFD/DESCRIPTION’ ... OK
...
* this is package ‘DFD’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘signatureSearch’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# discoveR

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/discoveR
* Number of recursive dependencies: 175

Run `revdepcheck::cloud_details(, "discoveR")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# dMrs

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/dMrs
* Date/Publication: 2025-01-21 15:40:05 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "dMrs")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dMrs/new/dMrs.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dMrs/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... NONE
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘dMrs.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/dMrs/old/dMrs.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dMrs/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... NONE
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘dMrs.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
# DR.SC

<details>

* Version: 3.5
* GitHub: https://github.com/feiyoung/DR.SC
* Source code: https://github.com/cran/DR.SC
* Date/Publication: 2025-03-29 15:10:02 UTC
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c getNB_fast.cpp -o getNB_fast.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c mt_paral_job.cpp -o mt_paral_job.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c mt_paral_job2.cpp -o mt_paral_job2.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c getNB_fast.cpp -o getNB_fast.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c mt_paral_job.cpp -o mt_paral_job.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -DARMA_64BIT_WORD -fpic  -g -O2  -c mt_paral_job2.cpp -o mt_paral_job2.o
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
# dscoreMSM

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/dscoreMSM
* Date/Publication: 2024-12-13 16:40:02 UTC
* Number of recursive dependencies: 121

Run `revdepcheck::cloud_details(, "dscoreMSM")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/dscoreMSM/new/dscoreMSM.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dscoreMSM/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘dscoreMSM.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/dscoreMSM/old/dscoreMSM.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘dscoreMSM/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘dscoreMSM.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
# DynForest

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/DynForest
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "DynForest")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# easybgm

<details>

* Version: 0.2.1
* GitHub: https://github.com/KarolineHuth/easybgm
* Source code: https://github.com/cran/easybgm
* Date/Publication: 2024-10-17 08:30:02 UTC
* Number of recursive dependencies: 180

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
# EcoEnsemble

<details>

* Version: 1.1.2
* GitHub: https://github.com/CefasRepRes/EcoEnsemble
* Source code: https://github.com/cran/EcoEnsemble
* Date/Publication: 2025-03-18 18:20:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "EcoEnsemble")` for more info

</details>

## In both

*   checking whether package ‘EcoEnsemble’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/EcoEnsemble/new/EcoEnsemble.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘EcoEnsemble’ ...
** package ‘EcoEnsemble’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c KF_back.cpp -o KF_back.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ensemble_model_hierarchical_namespace::model_ensemble_model_hierarchical; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_ensemble_model_hierarchical.o] Error 1
ERROR: compilation failed for package ‘EcoEnsemble’
* removing ‘/tmp/workdir/EcoEnsemble/new/EcoEnsemble.Rcheck/EcoEnsemble’


```
### CRAN

```
* installing *source* package ‘EcoEnsemble’ ...
** package ‘EcoEnsemble’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c KF_back.cpp -o KF_back.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_ensemble_model_hierarchical_namespace::model_ensemble_model_hierarchical; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_ensemble_model_hierarchical.o] Error 1
ERROR: compilation failed for package ‘EcoEnsemble’
* removing ‘/tmp/workdir/EcoEnsemble/old/EcoEnsemble.Rcheck/EcoEnsemble’


```
# ecolottery

<details>

* Version: 1.0.0
* GitHub: https://github.com/frmunoz/ecolottery
* Source code: https://github.com/cran/ecolottery
* Date/Publication: 2017-07-03 11:01:29 UTC
* Number of recursive dependencies: 87

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
* Number of recursive dependencies: 90

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
* Number of recursive dependencies: 110

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c fast_RS.cpp -o fast_RS.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c fast_RS.cpp -o fast_RS.o
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
# EWSmethods

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/EWSmethods
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "EWSmethods")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# FARS

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/FARS
* Date/Publication: 2025-04-03 15:10:10 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "FARS")` for more info

</details>

## In both

*   checking whether package ‘FARS’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/FARS/new/FARS.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘FARS’ ...
** package ‘FARS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘FARS’
* removing ‘/tmp/workdir/FARS/new/FARS.Rcheck/FARS’


```
### CRAN

```
* installing *source* package ‘FARS’ ...
** package ‘FARS’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘FARS’
* removing ‘/tmp/workdir/FARS/old/FARS.Rcheck/FARS’


```
# fio

<details>

* Version: 0.1.6
* GitHub: https://github.com/albersonmiranda/fio
* Source code: https://github.com/cran/fio
* Date/Publication: 2025-04-06 07:50:02 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "fio")` for more info

</details>

## In both

*   checking whether package ‘fio’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/fio/new/fio.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fio’ ...
** package ‘fio’ successfully unpacked and MD5 sums checked
** using staged installation
Error in eval(ei, envir) : 
------------------ [UNSUPPORTED RUST VERSION]------------------
- Minimum supported Rust version is 1.77.
- Installed Rust version is 1.75.0.
---------------------------------------------------------------
Calls: source -> withVisible -> eval -> eval
Execution halted
ERROR: configuration failed for package ‘fio’
* removing ‘/tmp/workdir/fio/new/fio.Rcheck/fio’


```
### CRAN

```
* installing *source* package ‘fio’ ...
** package ‘fio’ successfully unpacked and MD5 sums checked
** using staged installation
Error in eval(ei, envir) : 
------------------ [UNSUPPORTED RUST VERSION]------------------
- Minimum supported Rust version is 1.77.
- Installed Rust version is 1.75.0.
---------------------------------------------------------------
Calls: source -> withVisible -> eval -> eval
Execution halted
ERROR: configuration failed for package ‘fio’
* removing ‘/tmp/workdir/fio/old/fio.Rcheck/fio’


```
# flexrsurv

<details>

* Version: 2.0.18
* GitHub: NA
* Source code: https://github.com/cran/flexrsurv
* Date/Publication: 2024-02-09 16:10:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "flexrsurv")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/flexrsurv/new/flexrsurv.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘flexrsurv/DESCRIPTION’ ... OK
...
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/flexrsurv/old/flexrsurv.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘flexrsurv/DESCRIPTION’ ... OK
...
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: OK





```
# fmx

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/fmx
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "fmx")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# ForecastComb

<details>

* Version: 1.3.1
* GitHub: https://github.com/ceweiss/ForecastComb
* Source code: https://github.com/cran/ForecastComb
* Date/Publication: 2018-08-07 13:50:08 UTC
* Number of recursive dependencies: 73

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
# gaawr2

<details>

* Version: 0.0.3
* GitHub: https://github.com/jinghuazhao/gaawr2
* Source code: https://github.com/cran/gaawr2
* Date/Publication: 2025-03-24 15:00:09 UTC
* Number of recursive dependencies: 229

Run `revdepcheck::cloud_details(, "gaawr2")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/gaawr2/new/gaawr2.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gaawr2/DESCRIPTION’ ... OK
...
--- finished re-building ‘web.Rmd’

SUMMARY: processing the following file failed:
  ‘gaawr2.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 WARNING, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/gaawr2/old/gaawr2.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gaawr2/DESCRIPTION’ ... OK
...
--- finished re-building ‘web.Rmd’

SUMMARY: processing the following file failed:
  ‘gaawr2.Rmd’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 WARNING, 2 NOTEs





```
# gap

<details>

* Version: 1.6
* GitHub: https://github.com/jinghuazhao/R
* Source code: https://github.com/cran/gap
* Date/Publication: 2024-08-27 04:40:06 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::cloud_details(, "gap")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/gap/new/gap.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gap/DESCRIPTION’ ... OK
...
--- failed re-building ‘jss.Rnw’

SUMMARY: processing the following file failed:
  ‘jss.Rnw’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 4 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/gap/old/gap.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘gap/DESCRIPTION’ ... OK
...
--- failed re-building ‘jss.Rnw’

SUMMARY: processing the following file failed:
  ‘jss.Rnw’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 4 NOTEs





```
# gapfill

<details>

* Version: 0.9.6-1
* GitHub: https://github.com/florafauna/gapfill
* Source code: https://github.com/cran/gapfill
* Date/Publication: 2021-02-12 10:10:05 UTC
* Number of recursive dependencies: 70

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c gapfill.cpp -o gapfill.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c gapfill.cpp -o gapfill.o
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
# genekitr

<details>

* Version: 1.2.8
* GitHub: https://github.com/GangLiLab/genekitr
* Source code: https://github.com/cran/genekitr
* Date/Publication: 2024-09-06 13:00:06 UTC
* Number of recursive dependencies: 199

Run `revdepcheck::cloud_details(, "genekitr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/genekitr/new/genekitr.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘genekitr/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

Package suggested but not available for checking: ‘fgsea’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/genekitr/old/genekitr.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘genekitr/DESCRIPTION’ ... OK
...
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

Package suggested but not available for checking: ‘fgsea’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# GeneSelectR

<details>

* Version: 1.0.1
* GitHub: https://github.com/dzhakparov/GeneSelectR
* Source code: https://github.com/cran/GeneSelectR
* Date/Publication: 2024-02-03 14:00:05 UTC
* Number of recursive dependencies: 183

Run `revdepcheck::cloud_details(, "GeneSelectR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/GeneSelectR/new/GeneSelectR.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘GeneSelectR/DESCRIPTION’ ... OK
...
+     build_vignettes = FALSE)

  When sourcing ‘example.R’:
Error: there is no package called ‘devtools’
Execution halted

  ‘example.Rmd’ using ‘UTF-8’... failed
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 WARNING, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/GeneSelectR/old/GeneSelectR.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘GeneSelectR/DESCRIPTION’ ... OK
...
+     build_vignettes = FALSE)

  When sourcing ‘example.R’:
Error: there is no package called ‘devtools’
Execution halted

  ‘example.Rmd’ using ‘UTF-8’... failed
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 WARNING, 1 NOTE





```
# GeomComb

<details>

* Version: 1.0
* GitHub: https://github.com/ceweiss/GeomComb
* Source code: https://github.com/cran/GeomComb
* Date/Publication: 2016-11-27 16:02:26
* Number of recursive dependencies: 74

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
# geomorph

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/geomorph
* Number of recursive dependencies: 71

Run `revdepcheck::cloud_details(, "geomorph")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# GeoTox

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/GeoTox
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "GeoTox")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# gJLS2

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/gJLS2
* Date/Publication: 2021-09-30 09:00:05 UTC
* Number of recursive dependencies: 46

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
# grandR

<details>

* Version: 0.2.6
* GitHub: https://github.com/erhard-lab/grandR
* Source code: https://github.com/cran/grandR
* Date/Publication: 2025-01-22 22:10:02 UTC
* Number of recursive dependencies: 266

Run `revdepcheck::cloud_details(, "grandR")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/grandR/new/grandR.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘grandR/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘getting-started.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/grandR/old/grandR.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘grandR/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘getting-started.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
# GseaVis

<details>

* Version: 0.0.5
* GitHub: https://github.com/junjunlab/GseaVis
* Source code: https://github.com/cran/GseaVis
* Date/Publication: 2022-12-20 19:40:07 UTC
* Number of recursive dependencies: 104

Run `revdepcheck::cloud_details(, "GseaVis")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/GseaVis/new/GseaVis.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘GseaVis/DESCRIPTION’ ... OK
...
* this is package ‘GseaVis’ version ‘0.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘DOSE’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/GseaVis/old/GseaVis.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘GseaVis/DESCRIPTION’ ... OK
...
* this is package ‘GseaVis’ version ‘0.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘DOSE’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# HetSeq

<details>

* Version: 0.1.0
* GitHub: https://github.com/erhard-lab/HetSeq
* Source code: https://github.com/cran/HetSeq
* Date/Publication: 2025-02-03 18:00:05 UTC
* Number of recursive dependencies: 185

Run `revdepcheck::cloud_details(, "HetSeq")` for more info

</details>

## In both

*   checking whether package ‘HetSeq’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/HetSeq/new/HetSeq.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘HetSeq’ ...
** package ‘HetSeq’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘HetSeq’
* removing ‘/tmp/workdir/HetSeq/new/HetSeq.Rcheck/HetSeq’


```
### CRAN

```
* installing *source* package ‘HetSeq’ ...
** package ‘HetSeq’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘HetSeq’
* removing ‘/tmp/workdir/HetSeq/old/HetSeq.Rcheck/HetSeq’


```
# hettx

<details>

* Version: 0.1.3
* GitHub: https://github.com/bfifield/hettx
* Source code: https://github.com/cran/hettx
* Date/Publication: 2023-08-19 22:22:34 UTC
* Number of recursive dependencies: 84

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
# Hmisc

<details>

* Version: 5.2-3
* GitHub: NA
* Source code: https://github.com/cran/Hmisc
* Date/Publication: 2025-03-16 15:40:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "Hmisc")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Hmisc/new/Hmisc.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Hmisc/DESCRIPTION’ ... OK
...
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: 4 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/Hmisc/old/Hmisc.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Hmisc/DESCRIPTION’ ... OK
...
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: 4 NOTEs





```
# Hmsc

<details>

* Version: 3.0-13
* GitHub: https://github.com/hmsc-r/HMSC
* Source code: https://github.com/cran/Hmsc
* Date/Publication: 2022-08-11 14:10:14 UTC
* Number of recursive dependencies: 75

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
# iClusterVB

<details>

* Version: 0.1.4
* GitHub: https://github.com/AbdalkarimA/iClusterVB
* Source code: https://github.com/cran/iClusterVB
* Date/Publication: 2024-12-09 19:50:06 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "iClusterVB")` for more info

</details>

## In both

*   checking whether package ‘iClusterVB’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/iClusterVB/new/iClusterVB.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘iClusterVB’ ...
** package ‘iClusterVB’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c CAVI_algorithms.cpp -o CAVI_algorithms.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o iClusterVB.so CAVI_algorithms.o RcppExports.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/iClusterVB/new/iClusterVB.Rcheck/00LOCK-iClusterVB/00new/iClusterVB/libs
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
ERROR: lazy loading failed for package ‘iClusterVB’
* removing ‘/tmp/workdir/iClusterVB/new/iClusterVB.Rcheck/iClusterVB’


```
### CRAN

```
* installing *source* package ‘iClusterVB’ ...
** package ‘iClusterVB’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c CAVI_algorithms.cpp -o CAVI_algorithms.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o iClusterVB.so CAVI_algorithms.o RcppExports.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/iClusterVB/old/iClusterVB.Rcheck/00LOCK-iClusterVB/00new/iClusterVB/libs
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
ERROR: lazy loading failed for package ‘iClusterVB’
* removing ‘/tmp/workdir/iClusterVB/old/iClusterVB.Rcheck/iClusterVB’


```
# immcp

<details>

* Version: 1.0.3
* GitHub: https://github.com/YuanlongHu/immcp
* Source code: https://github.com/cran/immcp
* Date/Publication: 2022-05-12 05:50:02 UTC
* Number of recursive dependencies: 187

Run `revdepcheck::cloud_details(, "immcp")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/immcp/new/immcp.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘immcp/DESCRIPTION’ ... OK
...
* this is package ‘immcp’ version ‘1.0.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'clusterProfiler', 'DOSE'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/immcp/old/immcp.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘immcp/DESCRIPTION’ ... OK
...
* this is package ‘immcp’ version ‘1.0.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'clusterProfiler', 'DOSE'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# invivoPKfit

<details>

* Version: 2.0.1
* GitHub: NA
* Source code: https://github.com/cran/invivoPKfit
* Date/Publication: 2025-03-24 16:00:02 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::cloud_details(, "invivoPKfit")` for more info

</details>

## In both

*   checking whether package ‘invivoPKfit’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/invivoPKfit/new/invivoPKfit.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘invivoPKfit’ ...
** package ‘invivoPKfit’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘expand1’ is not exported by 'namespace:Matrix'
Execution halted
ERROR: lazy loading failed for package ‘invivoPKfit’
* removing ‘/tmp/workdir/invivoPKfit/new/invivoPKfit.Rcheck/invivoPKfit’


```
### CRAN

```
* installing *source* package ‘invivoPKfit’ ...
** package ‘invivoPKfit’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘expand1’ is not exported by 'namespace:Matrix'
Execution halted
ERROR: lazy loading failed for package ‘invivoPKfit’
* removing ‘/tmp/workdir/invivoPKfit/old/invivoPKfit.Rcheck/invivoPKfit’


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
* Number of recursive dependencies: 163

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
* Number of recursive dependencies: 181

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

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/jmBIG
* Date/Publication: 2025-01-19 21:00:02 UTC
* Number of recursive dependencies: 189

Run `revdepcheck::cloud_details(, "jmBIG")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/jmBIG/new/jmBIG.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘jmBIG/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘jmBIG’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/jmBIG/new/jmBIG.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/jmBIG/old/jmBIG.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘jmBIG/DESCRIPTION’ ... OK
...
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking whether package ‘jmBIG’ can be installed ... ERROR
Installation failed.
See ‘/tmp/workdir/jmBIG/old/jmBIG.Rcheck/00install.out’ for details.
* DONE
Status: 1 ERROR





```
# joineRML

<details>

* Version: 0.4.7
* GitHub: https://github.com/graemeleehickey/joineRML
* Source code: https://github.com/cran/joineRML
* Date/Publication: 2025-02-04 16:30:02 UTC
* Number of recursive dependencies: 93

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c expW.cpp -o expW.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c gammaUpdate.cpp -o gammaUpdate.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c gammaUpdate_approx.cpp -o gammaUpdate_approx.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lambdaUpdate.cpp -o lambdaUpdate.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c expW.cpp -o expW.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c gammaUpdate.cpp -o gammaUpdate.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c gammaUpdate_approx.cpp -o gammaUpdate_approx.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lambdaUpdate.cpp -o lambdaUpdate.o
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
# jsmodule

<details>

* Version: 1.6.4
* GitHub: https://github.com/jinseob2kim/jsmodule
* Source code: https://github.com/cran/jsmodule
* Date/Publication: 2025-03-10 06:20:02 UTC
* Number of recursive dependencies: 241

Run `revdepcheck::cloud_details(, "jsmodule")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/jsmodule/new/jsmodule.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘jsmodule/DESCRIPTION’ ... OK
...
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘jsmodule.Rmd’ using ‘UTF-8’... OK
  ‘jsmodule_subgroup_cmprsk.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/jsmodule/old/jsmodule.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘jsmodule/DESCRIPTION’ ... OK
...
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘jsmodule.Rmd’ using ‘UTF-8’... OK
  ‘jsmodule_subgroup_cmprsk.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
# kmc

<details>

* Version: 0.4-2
* GitHub: https://github.com/yfyang86/kmc
* Source code: https://github.com/cran/kmc
* Date/Publication: 2022-11-22 08:30:02 UTC
* Number of recursive dependencies: 60

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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExport.cpp -o RcppExport.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c kmc.cpp -o kmc.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c kmc_init.c -o kmc_init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c surv2.c -o surv2.o
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExport.cpp -o RcppExport.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c kmc.cpp -o kmc.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c kmc_init.c -o kmc_init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c surv2.c -o surv2.o
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
# KMunicate

<details>

* Version: 0.2.5
* GitHub: https://github.com/ellessenne/KMunicate-package
* Source code: https://github.com/cran/KMunicate
* Date/Publication: 2024-05-16 11:50:08 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::cloud_details(, "KMunicate")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/KMunicate/new/KMunicate.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘KMunicate/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘KMunicate.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/KMunicate/old/KMunicate.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘KMunicate/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘KMunicate.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





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
# Landmarking

<details>

* Version: 1.0.0
* GitHub: https://github.com/isobelbarrott/Landmarking
* Source code: https://github.com/cran/Landmarking
* Date/Publication: 2022-02-15 20:00:07 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "Landmarking")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Landmarking/new/Landmarking.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Landmarking/DESCRIPTION’ ... OK
...
* this is package ‘Landmarking’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/Landmarking/old/Landmarking.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Landmarking/DESCRIPTION’ ... OK
...
* this is package ‘Landmarking’ version ‘1.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# lavaSearch2

<details>

* Version: 2.0.3
* GitHub: https://github.com/bozenne/lavaSearch2
* Source code: https://github.com/cran/lavaSearch2
* Date/Publication: 2024-02-23 09:10:02 UTC
* Number of recursive dependencies: 149

Run `revdepcheck::cloud_details(, "lavaSearch2")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/lavaSearch2/new/lavaSearch2.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘lavaSearch2/DESCRIPTION’ ... OK
...
  [ FAIL 1 | WARN 1 | SKIP 0 | PASS 266 ]
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘overview.pdf.asis’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/lavaSearch2/old/lavaSearch2.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘lavaSearch2/DESCRIPTION’ ... OK
...
  [ FAIL 1 | WARN 1 | SKIP 0 | PASS 266 ]
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘overview.pdf.asis’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 1 NOTE





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
# lnmixsurv

<details>

* Version: 3.1.6
* GitHub: NA
* Source code: https://github.com/cran/lnmixsurv
* Date/Publication: 2024-09-03 15:20:08 UTC
* Number of recursive dependencies: 196

Run `revdepcheck::cloud_details(, "lnmixsurv")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/lnmixsurv/new/lnmixsurv.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘lnmixsurv/DESCRIPTION’ ... OK
...
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘compare.Rmd’ using ‘UTF-8’... OK
  ‘expectation_maximization.Rmd’ using ‘UTF-8’... OK
  ‘intercept_only.Rmd’ using ‘UTF-8’... OK
  ‘lnmixsurv.Rmd’ using ‘UTF-8’... OK
  ‘parallel_computation.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 4 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/lnmixsurv/old/lnmixsurv.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘lnmixsurv/DESCRIPTION’ ... OK
...
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘compare.Rmd’ using ‘UTF-8’... OK
  ‘expectation_maximization.Rmd’ using ‘UTF-8’... OK
  ‘intercept_only.Rmd’ using ‘UTF-8’... OK
  ‘lnmixsurv.Rmd’ using ‘UTF-8’... OK
  ‘parallel_computation.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 4 NOTEs





```
# LorenzRegression

<details>

* Version: 2.1.0
* GitHub: https://github.com/AlJacq/LorenzRegression
* Source code: https://github.com/cran/LorenzRegression
* Date/Publication: 2024-10-11 16:50:02 UTC
* Number of recursive dependencies: 84

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c GA_fitness.cpp -o GA_fitness.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c GA_meanrank.cpp -o GA_meanrank.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c PLR_derivative.cpp -o PLR_derivative.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c PLR_loss.cpp -o PLR_loss.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c GA_fitness.cpp -o GA_fitness.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c GA_meanrank.cpp -o GA_meanrank.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c PLR_derivative.cpp -o PLR_derivative.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c PLR_loss.cpp -o PLR_loss.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
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

* Version: 1.3.4
* GitHub: NA
* Source code: https://github.com/cran/lsirm12pl
* Date/Publication: 2025-03-06 10:40:06 UTC
* Number of recursive dependencies: 123

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm1pl.cpp -o lsirm1pl.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm2pl.cpp -o lsirm2pl.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsm.cpp -o lsm.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c utility_cpp.cpp -o utility_cpp.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm1pl.cpp -o lsirm1pl.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsirm2pl.cpp -o lsirm2pl.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c lsm.cpp -o lsm.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c utility_cpp.cpp -o utility_cpp.o
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
# MantaID

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/MantaID
* Number of recursive dependencies: 158

Run `revdepcheck::cloud_details(, "MantaID")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# manymodelr

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/manymodelr
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "manymodelr")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# MARVEL

<details>

* Version: 1.4.0
* GitHub: NA
* Source code: https://github.com/cran/MARVEL
* Date/Publication: 2022-10-31 10:22:50 UTC
* Number of recursive dependencies: 230

Run `revdepcheck::cloud_details(, "MARVEL")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MARVEL/new/MARVEL.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MARVEL/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘MARVEL.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/MARVEL/old/MARVEL.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MARVEL/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘MARVEL.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# MaxWiK

<details>

* Version: 1.0.5
* GitHub: NA
* Source code: https://github.com/cran/MaxWiK
* Date/Publication: 2024-11-25 11:40:13 UTC
* Number of recursive dependencies: 57

Run `revdepcheck::cloud_details(, "MaxWiK")` for more info

</details>

## In both

*   checking whether package ‘MaxWiK’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/MaxWiK/new/MaxWiK.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MaxWiK’ ...
** package ‘MaxWiK’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘MaxWiK’
* removing ‘/tmp/workdir/MaxWiK/new/MaxWiK.Rcheck/MaxWiK’


```
### CRAN

```
* installing *source* package ‘MaxWiK’ ...
** package ‘MaxWiK’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘MaxWiK’
* removing ‘/tmp/workdir/MaxWiK/old/MaxWiK.Rcheck/MaxWiK’


```
# mbsts

<details>

* Version: 3.0
* GitHub: NA
* Source code: https://github.com/cran/mbsts
* Date/Publication: 2023-01-07 01:10:02 UTC
* Number of recursive dependencies: 81

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c mvmrcML.cpp -o mvmrcML.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c mvmrcML.cpp -o mvmrcML.o
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
# mlmts

<details>

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/mlmts
* Date/Publication: 2024-08-18 08:40:06 UTC
* Number of recursive dependencies: 244

Run `revdepcheck::cloud_details(, "mlmts")` for more info

</details>

## In both

*   checking whether package ‘mlmts’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/mlmts/new/mlmts.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘mlmts’ ...
** package ‘mlmts’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : package or namespace load failed for ‘quantspec’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Error: unable to load R code in package ‘mlmts’
Execution halted
ERROR: lazy loading failed for package ‘mlmts’
* removing ‘/tmp/workdir/mlmts/new/mlmts.Rcheck/mlmts’


```
### CRAN

```
* installing *source* package ‘mlmts’ ...
** package ‘mlmts’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error : package or namespace load failed for ‘quantspec’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Error: unable to load R code in package ‘mlmts’
Execution halted
ERROR: lazy loading failed for package ‘mlmts’
* removing ‘/tmp/workdir/mlmts/old/mlmts.Rcheck/mlmts’


```
# mlr

<details>

* Version: 2.19.2
* GitHub: https://github.com/mlr-org/mlr
* Source code: https://github.com/cran/mlr
* Date/Publication: 2024-06-12 10:50:02 UTC
* Number of recursive dependencies: 364

Run `revdepcheck::cloud_details(, "mlr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/mlr/new/mlr.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘mlr/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘mlr.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/mlr/old/mlr.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘mlr/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘mlr.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





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
# mstate

<details>

* Version: 0.3.3
* GitHub: https://github.com/hputter/mstate
* Source code: https://github.com/cran/mstate
* Date/Publication: 2024-07-11 21:30:06 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "mstate")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/mstate/new/mstate.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘mstate/DESCRIPTION’ ... OK
...
--- failed re-building ‘Tutorial.Rnw’

SUMMARY: processing the following file failed:
  ‘Tutorial.Rnw’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/mstate/old/mstate.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘mstate/DESCRIPTION’ ... OK
...
--- failed re-building ‘Tutorial.Rnw’

SUMMARY: processing the following file failed:
  ‘Tutorial.Rnw’

Error: Vignette re-building failed.
Execution halted

* DONE
Status: 1 NOTE





```
# multinma

<details>

* Version: 0.8.0
* GitHub: https://github.com/dmphillippo/multinma
* Source code: https://github.com/cran/multinma
* Date/Publication: 2025-03-25 22:50:16 UTC
* Number of recursive dependencies: 150

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_survival_param_namespace::model_survival_param; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_survival_param.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/new/multinma.Rcheck/multinma’


```
### CRAN

```
* installing *source* package ‘multinma’ ...
** package ‘multinma’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/usr/local/lib/R/site-library/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/usr/local/lib/R/site-library/BH/include' -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppParallel/include' -I'/usr/local/lib/R/site-library/rstan/include' -I'/usr/local/lib/R/site-library/StanHeaders/include' -I/usr/local/include    -I'/usr/local/lib/R/site-library/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
...
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:0:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_survival_param_namespace::model_survival_param; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/usr/local/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:21:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stanExports_survival_param.o] Error 1
ERROR: compilation failed for package ‘multinma’
* removing ‘/tmp/workdir/multinma/old/multinma.Rcheck/multinma’


```
# multipleOutcomes

<details>

* Version: 0.4
* GitHub: NA
* Source code: https://github.com/cran/multipleOutcomes
* Date/Publication: 2024-05-30 15:00:03 UTC
* Number of recursive dependencies: 177

Run `revdepcheck::cloud_details(, "multipleOutcomes")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/multipleOutcomes/new/multipleOutcomes.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘multipleOutcomes/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘test.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/multipleOutcomes/old/multipleOutcomes.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘multipleOutcomes/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘test.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
# MuPETFlow

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/MuPETFlow
* Date/Publication: 2025-01-15 19:40:19 UTC
* Number of recursive dependencies: 86

Run `revdepcheck::cloud_details(, "MuPETFlow")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/MuPETFlow/new/MuPETFlow.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MuPETFlow/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘MuPETFlow.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/MuPETFlow/old/MuPETFlow.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘MuPETFlow/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘MuPETFlow.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# NCA

<details>

* Version: 4.0.2
* GitHub: NA
* Source code: https://github.com/cran/NCA
* Date/Publication: 2024-11-09 18:10:02 UTC
* Number of recursive dependencies: 98

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
* Number of recursive dependencies: 60

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c choleskyDecompositionRcppConversion.cpp -o choleskyDecompositionRcppConversion.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c doubleMatrixMultiplicationRcpp.cpp -o doubleMatrixMultiplicationRcpp.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c doubleVectorMultiplicationRcpp.cpp -o doubleVectorMultiplicationRcpp.o
...
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c vectorVectorTransposeMultiplicationRcpp.cpp -o vectorVectorTransposeMultiplicationRcpp.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c choleskyDecompositionRcppConversion.cpp -o choleskyDecompositionRcppConversion.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c doubleMatrixMultiplicationRcpp.cpp -o doubleMatrixMultiplicationRcpp.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c doubleVectorMultiplicationRcpp.cpp -o doubleVectorMultiplicationRcpp.o
...
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c vectorVectorTransposeMultiplicationRcpp.cpp -o vectorVectorTransposeMultiplicationRcpp.o
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
* Number of recursive dependencies: 122

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c Misc.cpp -o Misc.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c Misc.cpp -o Misc.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
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
* Number of recursive dependencies: 78

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
# obliqueRSF

<details>

* Version: 0.1.2
* GitHub: NA
* Source code: https://github.com/cran/obliqueRSF
* Date/Publication: 2022-08-28 20:50:02 UTC
* Number of recursive dependencies: 117

Run `revdepcheck::cloud_details(, "obliqueRSF")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/obliqueRSF/new/obliqueRSF.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘obliqueRSF/DESCRIPTION’ ... OK
...
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/obliqueRSF/old/obliqueRSF.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘obliqueRSF/DESCRIPTION’ ... OK
...
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: OK





```
# OlinkAnalyze

<details>

* Version: 4.2.0
* GitHub: https://github.com/Olink-Proteomics/OlinkRPackage
* Source code: https://github.com/cran/OlinkAnalyze
* Date/Publication: 2025-02-24 21:40:08 UTC
* Number of recursive dependencies: 211

Run `revdepcheck::cloud_details(, "OlinkAnalyze")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/OlinkAnalyze/new/OlinkAnalyze.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘OlinkAnalyze/DESCRIPTION’ ... OK
...
* checking running R code from vignettes ... OK
  ‘LOD.Rmd’ using ‘UTF-8’... OK
  ‘OutlierExclusion.Rmd’ using ‘UTF-8’... OK
  ‘Vignett.Rmd’ using ‘UTF-8’... OK
  ‘bridging_crossproduct.Rmd’ using ‘UTF-8’... OK
  ‘bridging_introduction.Rmd’ using ‘UTF-8’... OK
  ‘plate_randomizer.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/OlinkAnalyze/old/OlinkAnalyze.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘OlinkAnalyze/DESCRIPTION’ ... OK
...
* checking running R code from vignettes ... OK
  ‘LOD.Rmd’ using ‘UTF-8’... OK
  ‘OutlierExclusion.Rmd’ using ‘UTF-8’... OK
  ‘Vignett.Rmd’ using ‘UTF-8’... OK
  ‘bridging_crossproduct.Rmd’ using ‘UTF-8’... OK
  ‘bridging_introduction.Rmd’ using ‘UTF-8’... OK
  ‘plate_randomizer.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





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
# ordinalsimr

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/ordinalsimr
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "ordinalsimr")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# OVtool

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/OVtool
* Date/Publication: 2021-11-02 08:10:07 UTC
* Number of recursive dependencies: 156

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
# pammtools

<details>

* Version: 0.7.3
* GitHub: https://github.com/adibender/pammtools
* Source code: https://github.com/cran/pammtools
* Date/Publication: 2025-03-24 15:20:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::cloud_details(, "pammtools")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/pammtools/new/pammtools.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘pammtools/DESCRIPTION’ ... OK
...
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking R/sysdata.rda ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/pammtools/old/pammtools.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘pammtools/DESCRIPTION’ ... OK
...
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking R/sysdata.rda ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* DONE
Status: OK





```
# parameters

<details>

* Version: 0.24.2
* GitHub: https://github.com/easystats/parameters
* Source code: https://github.com/cran/parameters
* Date/Publication: 2025-03-04 14:50:06 UTC
* Number of recursive dependencies: 474

Run `revdepcheck::cloud_details(, "parameters")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/parameters/new/parameters.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘parameters/DESCRIPTION’ ... OK
...
  • equivalence_test/equivalence-test-5.svg
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘overview_of_vignettes.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 ERRORs, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/parameters/old/parameters.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘parameters/DESCRIPTION’ ... OK
...
  • equivalence_test/equivalence-test-5.svg
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘overview_of_vignettes.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 ERRORs, 2 NOTEs





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
# PathwaySpace

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/PathwaySpace
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "PathwaySpace")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# pcvr

<details>

* Version: 1.1.1.0
* GitHub: https://github.com/danforthcenter/pcvr
* Source code: https://github.com/cran/pcvr
* Date/Publication: 2024-11-06 20:50:02 UTC
* Number of recursive dependencies: 193

Run `revdepcheck::cloud_details(, "pcvr")` for more info

</details>

## In both

*   checking whether package ‘pcvr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/pcvr/new/pcvr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘pcvr’ ...
** package ‘pcvr’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘pcvr’
* removing ‘/tmp/workdir/pcvr/new/pcvr.Rcheck/pcvr’


```
### CRAN

```
* installing *source* package ‘pcvr’ ...
** package ‘pcvr’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘pcvr’
* removing ‘/tmp/workdir/pcvr/old/pcvr.Rcheck/pcvr’


```
# PLMIX

<details>

* Version: 2.1.1
* GitHub: NA
* Source code: https://github.com/cran/PLMIX
* Date/Publication: 2019-09-04 11:50:02 UTC
* Number of recursive dependencies: 128

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompProbZpartial.cpp -o CompProbZpartial.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompRateP.cpp -o CompRateP.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompRateYpartial.cpp -o CompRateYpartial.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c Estep.cpp -o Estep.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c PLMIXsim.cpp -o PLMIXsim.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompProbZpartial.cpp -o CompProbZpartial.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompRateP.cpp -o CompRateP.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c CompRateYpartial.cpp -o CompRateYpartial.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c Estep.cpp -o Estep.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c PLMIXsim.cpp -o PLMIXsim.o
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
# Polychrome

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/Polychrome
* Number of recursive dependencies: 50

Run `revdepcheck::cloud_details(, "Polychrome")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






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

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/pould/new/pould.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘pould/DESCRIPTION’ ... OK
...
* this is package ‘pould’ version ‘1.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘haplo.stats’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/pould/old/pould.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘pould/DESCRIPTION’ ... OK
...
* this is package ‘pould’ version ‘1.0.1’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘haplo.stats’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# PoweREST

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PoweREST
* Date/Publication: 2024-09-09 09:30:02 UTC
* Number of recursive dependencies: 183

Run `revdepcheck::cloud_details(, "PoweREST")` for more info

</details>

## In both

*   checking whether package ‘PoweREST’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/PoweREST/new/PoweREST.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘PoweREST’ ...
** package ‘PoweREST’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘PoweREST’
* removing ‘/tmp/workdir/PoweREST/new/PoweREST.Rcheck/PoweREST’


```
### CRAN

```
* installing *source* package ‘PoweREST’ ...
** package ‘PoweREST’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘PoweREST’
* removing ‘/tmp/workdir/PoweREST/old/PoweREST.Rcheck/PoweREST’


```
# powerly

<details>

* Version: 1.8.6
* GitHub: https://github.com/mihaiconstantin/powerly
* Source code: https://github.com/cran/powerly
* Date/Publication: 2022-09-09 14:10:01 UTC
* Number of recursive dependencies: 169

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
* Number of recursive dependencies: 154

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

* Version: 1.6
* GitHub: https://github.com/feiyoung/ProFAST
* Source code: https://github.com/cran/ProFAST
* Date/Publication: 2025-03-27 14:40:02 UTC
* Number of recursive dependencies: 246

Run `revdepcheck::cloud_details(, "ProFAST")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ProFAST/new/ProFAST.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ProFAST/DESCRIPTION’ ... OK
...
* this is package ‘ProFAST’ version ‘1.6’
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
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ProFAST/DESCRIPTION’ ... OK
...
* this is package ‘ProFAST’ version ‘1.6’
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
* Number of recursive dependencies: 119

Run `revdepcheck::cloud_details(, "psbcSpeedUp")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/psbcSpeedUp/new/psbcSpeedUp.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘psbcSpeedUp/DESCRIPTION’ ... OK
...
* this is package ‘psbcSpeedUp’ version ‘2.0.7’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/psbcSpeedUp/old/psbcSpeedUp.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘psbcSpeedUp/DESCRIPTION’ ... OK
...
* this is package ‘psbcSpeedUp’ version ‘2.0.7’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘riskRegression’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# pubh

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/pubh
* Number of recursive dependencies: 161

Run `revdepcheck::cloud_details(, "pubh")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# qPCRtools

<details>

* Version: 1.0.1
* GitHub: https://github.com/lixiang117423/qPCRtools
* Source code: https://github.com/cran/qPCRtools
* Date/Publication: 2023-11-02 13:10:05 UTC
* Number of recursive dependencies: 110

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
# qris

<details>

* Version: 1.1.1
* GitHub: https://github.com/Kyuhyun07/qris
* Source code: https://github.com/cran/qris
* Date/Publication: 2024-03-05 14:40:03 UTC
* Number of recursive dependencies: 54

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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c Amat.cpp -o Amat.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c ghat.cpp -o ghat.o
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++11
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c Amat.cpp -o Amat.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c ghat.cpp -o ghat.o
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
* Number of recursive dependencies: 124

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
# quantilogram

<details>

* Version: 3.1.1
* GitHub: NA
* Source code: https://github.com/cran/quantilogram
* Date/Publication: 2024-08-27 12:40:02 UTC
* Number of recursive dependencies: 58

Run `revdepcheck::cloud_details(, "quantilogram")` for more info

</details>

## In both

*   checking whether package ‘quantilogram’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/quantilogram/new/quantilogram.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘quantilogram’ ...
** package ‘quantilogram’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘quantilogram’
* removing ‘/tmp/workdir/quantilogram/new/quantilogram.Rcheck/quantilogram’


```
### CRAN

```
* installing *source* package ‘quantilogram’ ...
** package ‘quantilogram’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘quantilogram’
* removing ‘/tmp/workdir/quantilogram/old/quantilogram.Rcheck/quantilogram’


```
# quid

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/quid
* Date/Publication: 2021-12-09 09:00:02 UTC
* Number of recursive dependencies: 94

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
# RcensusPkg

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/RcensusPkg
* Number of recursive dependencies: 118

Run `revdepcheck::cloud_details(, "RcensusPkg")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# RcmdrPlugin.RiskDemo

<details>

* Version: 3.2
* GitHub: NA
* Source code: https://github.com/cran/RcmdrPlugin.RiskDemo
* Date/Publication: 2024-02-06 09:20:02 UTC
* Number of recursive dependencies: 201

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
* Number of recursive dependencies: 108

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
# relsurv

<details>

* Version: 2.3-2
* GitHub: NA
* Source code: https://github.com/cran/relsurv
* Date/Publication: 2025-01-28 15:50:02 UTC
* Number of recursive dependencies: 113

Run `revdepcheck::cloud_details(, "relsurv")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/relsurv/new/relsurv.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘relsurv/DESCRIPTION’ ... OK
...
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking line endings in Makefiles ... OK
* checking compilation flags in Makevars ... OK
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/relsurv/old/relsurv.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘relsurv/DESCRIPTION’ ... OK
...
* checking line endings in C/C++/Fortran sources/headers ... OK
* checking line endings in Makefiles ... OK
* checking compilation flags in Makevars ... OK
* checking for GNU extensions in Makefiles ... OK
* checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS) ... OK
* checking use of PKG_*FLAGS in Makefiles ... OK
* checking compiled code ... OK
* checking examples ... OK
* DONE
Status: OK





```
# ReporterScore

<details>

* Version: 0.1.9
* GitHub: https://github.com/Asa12138/ReporterScore
* Source code: https://github.com/cran/ReporterScore
* Date/Publication: 2024-11-28 14:10:06 UTC
* Number of recursive dependencies: 250

Run `revdepcheck::cloud_details(, "ReporterScore")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ReporterScore/new/ReporterScore.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ReporterScore/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘ReporterScore.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/ReporterScore/old/ReporterScore.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ReporterScore/DESCRIPTION’ ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘ReporterScore.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# RGraphSpace

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/RGraphSpace
* Number of recursive dependencies: 64

Run `revdepcheck::cloud_details(, "RGraphSpace")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# riskRegression

<details>

* Version: 2023.12.21
* GitHub: https://github.com/tagteam/riskRegression
* Source code: https://github.com/cran/riskRegression
* Date/Publication: 2023-12-19 17:00:02 UTC
* Number of recursive dependencies: 185

Run `revdepcheck::cloud_details(, "riskRegression")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/riskRegression/new/riskRegression.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘riskRegression/DESCRIPTION’ ... OK
...
* this is package ‘riskRegression’ version ‘2023.12.21’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rms’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/riskRegression/old/riskRegression.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘riskRegression/DESCRIPTION’ ... OK
...
* this is package ‘riskRegression’ version ‘2023.12.21’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘rms’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# rmlnomogram

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/rmlnomogram
* Number of recursive dependencies: 182

Run `revdepcheck::cloud_details(, "rmlnomogram")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# rms

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/rms
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "rms")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# robber

<details>

* Version: 0.2.4
* GitHub: https://github.com/Chabert-Liddell/robber
* Source code: https://github.com/cran/robber
* Date/Publication: 2024-02-07 13:50:02 UTC
* Number of recursive dependencies: 144

Run `revdepcheck::cloud_details(, "robber")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/robber/new/robber.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘robber/DESCRIPTION’ ... OK
...
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘topological-analysis.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
### CRAN

```
* using log directory ‘/tmp/workdir/robber/old/robber.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘robber/DESCRIPTION’ ... OK
...
* checking tests ... OK
  Running ‘spelling.R’
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘topological-analysis.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: OK





```
# robmed

<details>

* Version: 1.2.1
* GitHub: https://github.com/aalfons/robmed
* Source code: https://github.com/cran/robmed
* Date/Publication: 2025-02-08 22:50:06 UTC
* Number of recursive dependencies: 59

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

* Version: 0.1.1
* GitHub: https://github.com/aalfons/robmedExtra
* Source code: https://github.com/cran/robmedExtra
* Date/Publication: 2024-11-03 12:10:01 UTC
* Number of recursive dependencies: 91

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
# ROKET

<details>

* Version: 1.0.0
* GitHub: NA
* Source code: https://github.com/cran/ROKET
* Date/Publication: 2025-03-06 17:00:02 UTC
* Number of recursive dependencies: 172

Run `revdepcheck::cloud_details(, "ROKET")` for more info

</details>

## In both

*   checking whether package ‘ROKET’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ROKET/new/ROKET.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ROKET’ ...
** package ‘ROKET’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c ROKET.cpp -o ROKET.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o ROKET.so ROKET.o RcppExports.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/ROKET/new/ROKET.Rcheck/00LOCK-ROKET/00new/ROKET/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ROKET’
* removing ‘/tmp/workdir/ROKET/new/ROKET.Rcheck/ROKET’


```
### CRAN

```
* installing *source* package ‘ROKET’ ...
** package ‘ROKET’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c ROKET.cpp -o ROKET.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o ROKET.so ROKET.o RcppExports.o -fopenmp -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/ROKET/old/ROKET.Rcheck/00LOCK-ROKET/00new/ROKET/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ROKET’
* removing ‘/tmp/workdir/ROKET/old/ROKET.Rcheck/ROKET’


```
# rplec

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/rplec
* Number of recursive dependencies: 122

Run `revdepcheck::cloud_details(, "rplec")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# RplotterPkg

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/RplotterPkg
* Number of recursive dependencies: 123

Run `revdepcheck::cloud_details(, "RplotterPkg")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






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

* Version: 1.3.2
* GitHub: NA
* Source code: https://github.com/cran/RQdeltaCT
* Date/Publication: 2025-02-13 12:02:01 UTC
* Number of recursive dependencies: 163

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
# RRPP

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/RRPP
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "RRPP")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# rshift

<details>

* Version: 3.1.2
* GitHub: https://github.com/alexhroom/rshift
* Source code: https://github.com/cran/rshift
* Date/Publication: 2025-04-06 18:40:02 UTC
* Number of recursive dependencies: 37

Run `revdepcheck::cloud_details(, "rshift")` for more info

</details>

## In both

*   checking whether package ‘rshift’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rshift/new/rshift.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rshift’ ...
** package ‘rshift’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
...
export CARGO_HOME=/tmp/workdir/rshift/new/rshift.Rcheck/00_pkg_src/rshift/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target
error: failed to parse lock file at: /tmp/workdir/rshift/new/rshift.Rcheck/00_pkg_src/rshift/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:28: rust/target/release/librshift.a] Error 101
ERROR: compilation failed for package ‘rshift’
* removing ‘/tmp/workdir/rshift/new/rshift.Rcheck/rshift’


```
### CRAN

```
* installing *source* package ‘rshift’ ...
** package ‘rshift’ successfully unpacked and MD5 sums checked
** using staged installation
Using cargo 1.75.0
Using rustc 1.75.0 (82e1608df 2023-12-21) (built from a source tarball)
Building for CRAN.
Writing `src/Makevars`.
`tools/config.R` has finished.
** libs
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
...
export CARGO_HOME=/tmp/workdir/rshift/old/rshift.Rcheck/00_pkg_src/rshift/src/.cargo && \
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/root/.cargo/bin" && \
RUSTFLAGS=" --print=native-static-libs" cargo build -j 2 --offline --lib --release --manifest-path=./rust/Cargo.toml --target-dir ./rust/target
error: failed to parse lock file at: /tmp/workdir/rshift/old/rshift.Rcheck/00_pkg_src/rshift/src/rust/Cargo.lock

Caused by:
  lock file version 4 requires `-Znext-lockfile-bump`
make: *** [Makevars:28: rust/target/release/librshift.a] Error 101
ERROR: compilation failed for package ‘rshift’
* removing ‘/tmp/workdir/rshift/old/rshift.Rcheck/rshift’


```
# rstanarm

<details>

* Version: 2.32.1
* GitHub: https://github.com/stan-dev/rstanarm
* Source code: https://github.com/cran/rstanarm
* Date/Publication: 2024-01-18 23:00:03 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "rstanarm")` for more info

</details>

## In both

*   checking whether package ‘rstanarm’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rstanarm/new/rstanarm.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rstanarm’ ...
** package ‘rstanarm’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
"/opt/R/4.3.1/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/bernoulli.stan
Wrote C++ file "stan_files/bernoulli.cc"


...
/usr/local/lib/R/site-library/StanHeaders/include/stan/math/rev/fun/quad_form.hpp:88:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stan_files/continuous.o] Error 1
rm stan_files/bernoulli.cc stan_files/binomial.cc stan_files/continuous.cc
ERROR: compilation failed for package ‘rstanarm’
* removing ‘/tmp/workdir/rstanarm/new/rstanarm.Rcheck/rstanarm’


```
### CRAN

```
* installing *source* package ‘rstanarm’ ...
** package ‘rstanarm’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
"/opt/R/4.3.1/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/bernoulli.stan
Wrote C++ file "stan_files/bernoulli.cc"


...
/usr/local/lib/R/site-library/StanHeaders/include/stan/math/rev/fun/quad_form.hpp:88:0:   required from here
/usr/local/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:654:74: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__m128d’} [-Wignored-attributes]
  654 |   return internal::first_aligned<int(unpacket_traits<DefaultPacketType>::alignment),Derived>(m);
      |                                                                          ^~~~~~~~~
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.3.1/lib/R/etc/Makeconf:198: stan_files/continuous.o] Error 1
rm stan_files/bernoulli.cc stan_files/binomial.cc stan_files/continuous.cc
ERROR: compilation failed for package ‘rstanarm’
* removing ‘/tmp/workdir/rstanarm/old/rstanarm.Rcheck/rstanarm’


```
# rTwig

<details>

* Version: 1.4.0
* GitHub: https://github.com/aidanmorales/rTwig
* Source code: https://github.com/cran/rTwig
* Date/Publication: 2025-03-03 17:10:09 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "rTwig")` for more info

</details>

## In both

*   checking whether package ‘rTwig’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rTwig/new/rTwig.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rTwig’ ...
** package ‘rTwig’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c box_counting.cpp -o box_counting.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c cluster_cloud.cpp -o cluster_cloud.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c colors.cpp -o colors.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c convex_hull.cpp -o convex_hull.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘rTwig’
* removing ‘/tmp/workdir/rTwig/new/rTwig.Rcheck/rTwig’


```
### CRAN

```
* installing *source* package ‘rTwig’ ...
** package ‘rTwig’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c box_counting.cpp -o box_counting.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c cluster_cloud.cpp -o cluster_cloud.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c colors.cpp -o colors.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -I../inst/include -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c convex_hull.cpp -o convex_hull.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘rTwig’
* removing ‘/tmp/workdir/rTwig/old/rTwig.Rcheck/rTwig’


```
# RVA

<details>

* Version: 0.0.5
* GitHub: https://github.com/THERMOSTATS/RVA
* Source code: https://github.com/cran/RVA
* Date/Publication: 2021-11-01 21:40:02 UTC
* Number of recursive dependencies: 211

Run `revdepcheck::cloud_details(, "RVA")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RVA/new/RVA.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RVA/DESCRIPTION’ ... OK
...
* this is package ‘RVA’ version ‘0.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/RVA/old/RVA.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RVA/DESCRIPTION’ ... OK
...
* this is package ‘RVA’ version ‘0.0.5’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# S4DM

<details>

* Version: 0.0.1
* GitHub: NA
* Source code: https://github.com/cran/S4DM
* Date/Publication: 2025-01-10 21:00:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "S4DM")` for more info

</details>

## In both

*   checking whether package ‘S4DM’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/S4DM/new/S4DM.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘S4DM’ ...
** package ‘S4DM’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘S4DM’
* removing ‘/tmp/workdir/S4DM/new/S4DM.Rcheck/S4DM’


```
### CRAN

```
* installing *source* package ‘S4DM’ ...
** package ‘S4DM’ successfully unpacked and MD5 sums checked
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
ERROR: lazy loading failed for package ‘S4DM’
* removing ‘/tmp/workdir/S4DM/old/S4DM.Rcheck/S4DM’


```
# scCustomize

<details>

* Version: 3.0.1
* GitHub: https://github.com/samuel-marsh/scCustomize
* Source code: https://github.com/cran/scCustomize
* Date/Publication: 2024-12-18 18:40:02 UTC
* Number of recursive dependencies: 272

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
* Number of recursive dependencies: 237

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
* Number of recursive dependencies: 180

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c crossdist.cpp -o crossdist.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c crossdist.cpp -o crossdist.o
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
* Number of recursive dependencies: 241

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

* Version: 3.0.0
* GitHub: NA
* Source code: https://github.com/cran/scpi
* Date/Publication: 2025-01-31 19:40:02 UTC
* Number of recursive dependencies: 96

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
# SCpubr

<details>

* Version: 2.0.2
* GitHub: https://github.com/enblacar/SCpubr
* Source code: https://github.com/cran/SCpubr
* Date/Publication: 2023-10-11 09:50:02 UTC
* Number of recursive dependencies: 301

Run `revdepcheck::cloud_details(, "SCpubr")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SCpubr/new/SCpubr.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SCpubr/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘reference_manual.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/SCpubr/old/SCpubr.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SCpubr/DESCRIPTION’ ... OK
...
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘reference_manual.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
# scRNAstat

<details>

* Version: 0.1.1.1
* GitHub: NA
* Source code: https://github.com/cran/scRNAstat
* Date/Publication: 2025-03-08 08:58:55 UTC
* Number of recursive dependencies: 157

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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c SEERaBomb_init.c -o SEERaBomb_init.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c fillPYM.cpp -o fillPYM.o
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c SEERaBomb_init.c -o SEERaBomb_init.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c fillPYM.cpp -o fillPYM.o
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
* Number of recursive dependencies: 148

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

* Version: 5.2.1
* GitHub: https://github.com/satijalab/seurat
* Source code: https://github.com/cran/Seurat
* Date/Publication: 2025-01-24 06:50:06 UTC
* Number of recursive dependencies: 281

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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c ModularityOptimizer.cpp -o ModularityOptimizer.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c RModularityOptimizer.cpp -o RModularityOptimizer.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
using C++17
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c ModularityOptimizer.cpp -o ModularityOptimizer.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppEigen/include' -I'/usr/local/lib/R/site-library/RcppProgress/include' -I/usr/local/include    -fpic  -g -O2  -c RModularityOptimizer.cpp -o RModularityOptimizer.o
In file included from /usr/local/lib/R/site-library/RcppEigen/include/Eigen/Core:205,
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
# SeuratExplorer

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/SeuratExplorer
* Date/Publication: 2025-03-13 13:10:05 UTC
* Number of recursive dependencies: 236

Run `revdepcheck::cloud_details(, "SeuratExplorer")` for more info

</details>

## In both

*   checking whether package ‘SeuratExplorer’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SeuratExplorer/new/SeuratExplorer.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SeuratExplorer’ ...
** package ‘SeuratExplorer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SeuratExplorer’
* removing ‘/tmp/workdir/SeuratExplorer/new/SeuratExplorer.Rcheck/SeuratExplorer’


```
### CRAN

```
* installing *source* package ‘SeuratExplorer’ ...
** package ‘SeuratExplorer’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SeuratExplorer’
* removing ‘/tmp/workdir/SeuratExplorer/old/SeuratExplorer.Rcheck/SeuratExplorer’


```
# shinyTempSignal

<details>

* Version: 0.0.8
* GitHub: https://github.com/YuLab-SMU/shinyTempSignal
* Source code: https://github.com/cran/shinyTempSignal
* Date/Publication: 2024-03-06 08:00:02 UTC
* Number of recursive dependencies: 134

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
* Number of recursive dependencies: 76

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c kernel_sievePH_utils.cpp -o kernel_sievePH_utils.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp -fpic  -g -O2  -c kernel_sievePH_utils.cpp -o kernel_sievePH_utils.o
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
# SiFINeT

<details>

* Version: 1.13
* GitHub: NA
* Source code: https://github.com/cran/SiFINeT
* Date/Publication: 2025-01-16 15:10:05 UTC
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "SiFINeT")` for more info

</details>

## In both

*   checking whether package ‘SiFINeT’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SiFINeT/new/SiFINeT.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SiFINeT’ ...
** package ‘SiFINeT’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c EstNull.cpp -o EstNull.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c cal_coexp.cpp -o cal_coexp.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c cal_coexp_sp.cpp -o cal_coexp_sp.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c cal_conn.cpp -o cal_conn.o
...
installing to /tmp/workdir/SiFINeT/new/SiFINeT.Rcheck/00LOCK-SiFINeT/00new/SiFINeT/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SiFINeT’
* removing ‘/tmp/workdir/SiFINeT/new/SiFINeT.Rcheck/SiFINeT’


```
### CRAN

```
* installing *source* package ‘SiFINeT’ ...
** package ‘SiFINeT’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c EstNull.cpp -o EstNull.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c cal_coexp.cpp -o cal_coexp.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c cal_coexp_sp.cpp -o cal_coexp_sp.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG -DARMA_64BIT_WORD=1 -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include   -fopenmp  -fpic  -g -O2  -c cal_conn.cpp -o cal_conn.o
...
installing to /tmp/workdir/SiFINeT/old/SiFINeT.Rcheck/00LOCK-SiFINeT/00new/SiFINeT/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SiFINeT’
* removing ‘/tmp/workdir/SiFINeT/old/SiFINeT.Rcheck/SiFINeT’


```
# Signac

<details>

* Version: 1.14.0
* GitHub: https://github.com/stuart-lab/signac
* Source code: https://github.com/cran/Signac
* Date/Publication: 2024-08-21 07:40:02 UTC
* Number of recursive dependencies: 246

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c filter.cpp -o filter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c group.cpp -o group.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c split.cpp -o split.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c validate.cpp -o validate.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c filter.cpp -o filter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c group.cpp -o group.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c split.cpp -o split.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c validate.cpp -o validate.o
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

* Version: 0.2.1
* GitHub: https://github.com/arcaldwell49/SimplyAgree
* Source code: https://github.com/cran/SimplyAgree
* Date/Publication: 2025-02-24 16:40:11 UTC
* Number of recursive dependencies: 118

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
# smer

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/smer
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "smer")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# SNPassoc

<details>

* Version: 2.1-2
* GitHub: https://github.com/isglobal-brge/SNPassoc
* Source code: https://github.com/cran/SNPassoc
* Date/Publication: 2024-10-28 17:30:02 UTC
* Number of recursive dependencies: 164

Run `revdepcheck::cloud_details(, "SNPassoc")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SNPassoc/new/SNPassoc.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SNPassoc/DESCRIPTION’ ... OK
...
* this is package ‘SNPassoc’ version ‘2.1-2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘haplo.stats’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/SNPassoc/old/SNPassoc.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SNPassoc/DESCRIPTION’ ... OK
...
* this is package ‘SNPassoc’ version ‘2.1-2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘haplo.stats’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# snplinkage

<details>

* Version: 1.2.0
* GitHub: NA
* Source code: https://github.com/cran/snplinkage
* Date/Publication: 2024-09-09 19:10:02 UTC
* Number of recursive dependencies: 155

Run `revdepcheck::cloud_details(, "snplinkage")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/snplinkage/new/snplinkage.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘snplinkage/DESCRIPTION’ ... OK
...
* this is package ‘snplinkage’ version ‘1.2.0’
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
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘snplinkage/DESCRIPTION’ ... OK
...
* this is package ‘snplinkage’ version ‘1.2.0’
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
* Number of recursive dependencies: 202

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
# SpaCCI

<details>

* Version: 1.0.3
* GitHub: NA
* Source code: https://github.com/cran/SpaCCI
* Date/Publication: 2025-01-18 23:10:02 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::cloud_details(, "SpaCCI")` for more info

</details>

## In both

*   checking whether package ‘SpaCCI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/SpaCCI/new/SpaCCI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SpaCCI’ ...
** package ‘SpaCCI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c permutation_func.cpp -o permutation_func.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o SpaCCI.so RcppExports.o permutation_func.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/SpaCCI/new/SpaCCI.Rcheck/00LOCK-SpaCCI/00new/SpaCCI/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SpaCCI’
* removing ‘/tmp/workdir/SpaCCI/new/SpaCCI.Rcheck/SpaCCI’


```
### CRAN

```
* installing *source* package ‘SpaCCI’ ...
** package ‘SpaCCI’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c permutation_func.cpp -o permutation_func.o
g++ -std=gnu++17 -shared -L/opt/R/4.3.1/lib/R/lib -L/usr/local/lib -o SpaCCI.so RcppExports.o permutation_func.o -llapack -lblas -lgfortran -lm -lquadmath -L/opt/R/4.3.1/lib/R/lib -lR
installing to /tmp/workdir/SpaCCI/old/SpaCCI.Rcheck/00LOCK-SpaCCI/00new/SpaCCI/libs
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.4 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘SpaCCI’
* removing ‘/tmp/workdir/SpaCCI/old/SpaCCI.Rcheck/SpaCCI’


```
# sparsereg

<details>

* Version: 1.2
* GitHub: NA
* Source code: https://github.com/cran/sparsereg
* Date/Publication: 2016-03-10 23:32:18
* Number of recursive dependencies: 50

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c makeinter.cpp -o makeinter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c makethreeinter.cpp -o makethreeinter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c subgroup.cpp -o subgroup.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c makeinter.cpp -o makeinter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c makethreeinter.cpp -o makethreeinter.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c subgroup.cpp -o subgroup.o
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

* Version: 1.1-20
* GitHub: https://github.com/fabian-s/spikeSlabGAM
* Source code: https://github.com/cran/spikeSlabGAM
* Date/Publication: 2024-10-22 17:30:02 UTC
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
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
# stabiliser

<details>

* Version: 1.0.6
* GitHub: NA
* Source code: https://github.com/cran/stabiliser
* Date/Publication: 2023-05-17 11:00:05 UTC
* Number of recursive dependencies: 153

Run `revdepcheck::cloud_details(, "stabiliser")` for more info

</details>

## In both

*   checking whether package ‘stabiliser’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/stabiliser/new/stabiliser.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘stabiliser’ ...
** package ‘stabiliser’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘maditr’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘stabiliser’
* removing ‘/tmp/workdir/stabiliser/new/stabiliser.Rcheck/stabiliser’


```
### CRAN

```
* installing *source* package ‘stabiliser’ ...
** package ‘stabiliser’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  there is no package called ‘maditr’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘stabiliser’
* removing ‘/tmp/workdir/stabiliser/old/stabiliser.Rcheck/stabiliser’


```
# statsr

<details>

* Version: 0.3.0
* GitHub: https://github.com/StatsWithR/statsr
* Source code: https://github.com/cran/statsr
* Date/Publication: 2021-01-22 20:40:03 UTC
* Number of recursive dependencies: 96

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

* Version: 1.5-9
* GitHub: NA
* Source code: https://github.com/cran/streamDAG
* Date/Publication: 2025-01-14 21:50:02 UTC
* Number of recursive dependencies: 139

Run `revdepcheck::cloud_details(, "streamDAG")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/streamDAG/new/streamDAG.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘streamDAG/DESCRIPTION’ ... OK
* this is package ‘streamDAG’ version ‘1.5-9’
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
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘streamDAG/DESCRIPTION’ ... OK
* this is package ‘streamDAG’ version ‘1.5-9’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘asbio’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# survex

<details>

* Version: 1.2.0
* GitHub: https://github.com/ModelOriented/survex
* Source code: https://github.com/cran/survex
* Date/Publication: 2023-10-24 18:50:07 UTC
* Number of recursive dependencies: 183

Run `revdepcheck::cloud_details(, "survex")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/survex/new/survex.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘survex/DESCRIPTION’ ... OK
...
 [32m A new explainer has been created! [39m 
> 
> y <- cph_exp$y
> times <- cph_exp$times
> surv <- cph_exp$predict_survival_function(cph, cph_exp$data, times)
Error in loadNamespace(x) : there is no package called ‘riskRegression’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
* DONE
Status: 1 ERROR, 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/survex/old/survex.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘survex/DESCRIPTION’ ... OK
...
 [32m A new explainer has been created! [39m 
> 
> y <- cph_exp$y
> times <- cph_exp$times
> surv <- cph_exp$predict_survival_function(cph, cph_exp$data, times)
Error in loadNamespace(x) : there is no package called ‘riskRegression’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
* DONE
Status: 1 ERROR, 1 NOTE





```
# survHE

<details>

* Version: 2.0.3
* GitHub: https://github.com/giabaio/survHE
* Source code: https://github.com/cran/survHE
* Date/Publication: 2025-03-03 17:40:02 UTC
* Number of recursive dependencies: 129

Run `revdepcheck::cloud_details(, "survHE")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/survHE/new/survHE.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘survHE/DESCRIPTION’ ... OK
...
* checking package dependencies ... ERROR
Package required but not available: ‘rms’

Packages suggested but not available for checking:
  'survHEinla', 'survHEhmc'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/survHE/old/survHE.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘survHE/DESCRIPTION’ ... OK
...
* checking package dependencies ... ERROR
Package required but not available: ‘rms’

Packages suggested but not available for checking:
  'survHEinla', 'survHEhmc'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c survidm_init.c -o survidm_init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c survivalBIV.c -o survivalBIV.o
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c survidm_init.c -o survidm_init.o
gcc -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c survivalBIV.c -o survivalBIV.o
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
# SurvMetrics

<details>

* Version: 0.5.1
* GitHub: https://github.com/whcsu/SurvMetrics
* Source code: https://github.com/cran/SurvMetrics
* Date/Publication: 2025-02-05 11:50:28 UTC
* Number of recursive dependencies: 194

Run `revdepcheck::cloud_details(, "SurvMetrics")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SurvMetrics/new/SurvMetrics.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SurvMetrics/DESCRIPTION’ ... OK
...
  [ FAIL 4 | WARN 0 | SKIP 0 | PASS 38 ]
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘SurvMetrics-vignette.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/SurvMetrics/old/SurvMetrics.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SurvMetrics/DESCRIPTION’ ... OK
...
  [ FAIL 4 | WARN 0 | SKIP 0 | PASS 38 ]
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... OK
  ‘SurvMetrics-vignette.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR





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
# TestAnaAPP

<details>

* Version: 1.1.2
* GitHub: https://github.com/jiangyouxiang/TestAnaAPP
* Source code: https://github.com/cran/TestAnaAPP
* Date/Publication: 2024-11-09 04:00:02 UTC
* Number of recursive dependencies: 254

Run `revdepcheck::cloud_details(, "TestAnaAPP")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/TestAnaAPP/new/TestAnaAPP.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TestAnaAPP/DESCRIPTION’ ... OK
...
* this is package ‘TestAnaAPP’ version ‘1.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘lordif’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/TestAnaAPP/old/TestAnaAPP.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘TestAnaAPP/DESCRIPTION’ ... OK
...
* this is package ‘TestAnaAPP’ version ‘1.1.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘lordif’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# tidyEdSurvey

<details>

* Version: 0.1.3
* GitHub: NA
* Source code: https://github.com/cran/tidyEdSurvey
* Date/Publication: 2024-05-14 20:20:03 UTC
* Number of recursive dependencies: 113

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
* Number of recursive dependencies: 197

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

* Version: 1.5.2
* GitHub: https://github.com/certara/tidyvpc
* Source code: https://github.com/cran/tidyvpc
* Date/Publication: 2024-11-21 23:10:02 UTC
* Number of recursive dependencies: 181

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
# tinyarray

<details>

* Version: 2.4.3
* GitHub: https://github.com/xjsun1221/tinyarray
* Source code: https://github.com/cran/tinyarray
* Date/Publication: 2025-03-05 13:20:02 UTC
* Number of recursive dependencies: 251

Run `revdepcheck::cloud_details(, "tinyarray")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tinyarray/new/tinyarray.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tinyarray/DESCRIPTION’ ... OK
...
* this is package ‘tinyarray’ version ‘2.4.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/tinyarray/old/tinyarray.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0
* running under: Ubuntu 24.04.1 LTS
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tinyarray/DESCRIPTION’ ... OK
...
* this is package ‘tinyarray’ version ‘2.4.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# treeclim

<details>

* Version: 2.0.7.1
* GitHub: https://github.com/cszang/treeclim
* Source code: https://github.com/cran/treeclim
* Date/Publication: 2024-12-16 16:20:02 UTC
* Number of recursive dependencies: 60

Run `revdepcheck::cloud_details(, "treeclim")` for more info

</details>

## In both

*   checking whether package ‘treeclim’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/treeclim/new/treeclim.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘treeclim’ ...
** package ‘treeclim’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c corfun.cpp -o corfun.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c corfun_exact.cpp -o corfun_exact.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c corfun_noboot.cpp -o corfun_noboot.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c pcor.cpp -o pcor.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘treeclim’
* removing ‘/tmp/workdir/treeclim/new/treeclim.Rcheck/treeclim’


```
### CRAN

```
* installing *source* package ‘treeclim’ ...
** package ‘treeclim’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c corfun.cpp -o corfun.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c corfun_exact.cpp -o corfun_exact.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c corfun_noboot.cpp -o corfun_noboot.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I'/usr/local/lib/R/site-library/RcppArmadillo/include' -I/usr/local/include    -fpic  -g -O2  -c pcor.cpp -o pcor.o
...
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘Matrix’ 1.5-4.1 is already loaded, but >= 1.6.0 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘treeclim’
* removing ‘/tmp/workdir/treeclim/old/treeclim.Rcheck/treeclim’


```
# TriDimRegression

<details>

* Version: 1.0.2
* GitHub: https://github.com/alexander-pastukhov/tridim-regression
* Source code: https://github.com/cran/TriDimRegression
* Date/Publication: 2023-09-13 14:10:03 UTC
* Number of recursive dependencies: 98

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
* Number of recursive dependencies: 71

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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c FeatureClippingTrending.cpp -o FeatureClippingTrending.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c helpers.cpp -o helpers.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c measures.cpp -o measures.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c normalizations.cpp -o normalizations.o
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
using C++ compiler: ‘g++ (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c FeatureClippingTrending.cpp -o FeatureClippingTrending.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c helpers.cpp -o helpers.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c measures.cpp -o measures.o
g++ -std=gnu++17 -I"/opt/R/4.3.1/lib/R/include" -DNDEBUG  -I'/usr/local/lib/R/site-library/Rcpp/include' -I/usr/local/include    -fpic  -g -O2  -c normalizations.cpp -o normalizations.o
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

* Version: 2.6.1
* GitHub: NA
* Source code: https://github.com/cran/twang
* Date/Publication: 2024-07-22 16:10:01 UTC
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
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
using C compiler: ‘gcc (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
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
# ubair

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/ubair
* Number of recursive dependencies: 111

Run `revdepcheck::cloud_details(, "ubair")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






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
using Fortran compiler: ‘GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
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
using Fortran compiler: ‘GNU Fortran (Ubuntu 13.2.0-23ubuntu4) 13.2.0’
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
# VecDep

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/VecDep
* Number of recursive dependencies: 100

Run `revdepcheck::cloud_details(, "VecDep")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# visa

<details>

* Version: 1.0.0
* GitHub: https://github.com/kang-yu/visa
* Source code: https://github.com/cran/visa
* Date/Publication: 2025-03-19 19:50:02 UTC
* Number of recursive dependencies: 144

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
# VisualizeSimon2Stage

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/VisualizeSimon2Stage
* Number of recursive dependencies: 75

Run `revdepcheck::cloud_details(, "VisualizeSimon2Stage")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# WRTDStidal

<details>

* Version: 1.1.4
* GitHub: https://github.com/fawda123/WRTDStidal
* Source code: https://github.com/cran/WRTDStidal
* Date/Publication: 2023-10-20 09:00:11 UTC
* Number of recursive dependencies: 141

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
# xxdi

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/xxdi
* Number of recursive dependencies: 37

Run `revdepcheck::cloud_details(, "xxdi")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
