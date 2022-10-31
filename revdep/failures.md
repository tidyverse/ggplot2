# CausalImpact

<details>

* Version: 1.2.7
* GitHub: NA
* Source code: https://github.com/cran/CausalImpact
* Date/Publication: 2021-06-07 06:40:02 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "CausalImpact")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/CausalImpact/new/CausalImpact.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘CausalImpact/DESCRIPTION’ ... OK
* this is package ‘CausalImpact’ version ‘1.2.7’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'Boom'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/CausalImpact/old/CausalImpact.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘CausalImpact/DESCRIPTION’ ... OK
* this is package ‘CausalImpact’ version ‘1.2.7’
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'bsts', 'Boom'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# ctsem

<details>

* Version: 3.7.1
* GitHub: https://github.com/cdriveraus/ctsem
* Source code: https://github.com/cran/ctsem
* Date/Publication: 2022-08-08 14:00:02 UTC
* Number of recursive dependencies: 135

Run `cloud_details(, "ctsem")` for more info

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
"/opt/R/4.1.1/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/ctsm.stan
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: d * d - d / 2
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

Wrote C++ file "stan_files/ctsm.cc"
...
 2340 |     T__ log_prob(std::vector<T__>& params_r__,
      |         ^~~~~~~~
stan_files/ctsm.hpp: In member function ‘T__ model_ctsm_namespace::model_ctsm::log_prob(std::vector<T_l>&, std::vector<int>&, std::ostream*) const [with bool propto__ = false; bool jacobian__ = false; T__ = double]’:
stan_files/ctsm.hpp:2340:9: note: variable tracking size limit exceeded with ‘-fvar-tracking-assignments’, retrying without
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.1.1/lib/R/etc/Makeconf:175: stan_files/ctsm.o] Error 1
rm stan_files/ctsm.cc
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/new/ctsem.Rcheck/ctsem’


```
### CRAN

```
* installing *source* package ‘ctsem’ ...
** package ‘ctsem’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
"/opt/R/4.1.1/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/ctsm.stan
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: d * d - d / 2
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

Wrote C++ file "stan_files/ctsm.cc"
...
 2340 |     T__ log_prob(std::vector<T__>& params_r__,
      |         ^~~~~~~~
stan_files/ctsm.hpp: In member function ‘T__ model_ctsm_namespace::model_ctsm::log_prob(std::vector<T_l>&, std::vector<int>&, std::ostream*) const [with bool propto__ = false; bool jacobian__ = false; T__ = double]’:
stan_files/ctsm.hpp:2340:9: note: variable tracking size limit exceeded with ‘-fvar-tracking-assignments’, retrying without
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.1.1/lib/R/etc/Makeconf:175: stan_files/ctsm.o] Error 1
rm stan_files/ctsm.cc
ERROR: compilation failed for package ‘ctsem’
* removing ‘/tmp/workdir/ctsem/old/ctsem.Rcheck/ctsem’


```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# expss

<details>

* Version: 0.11.2
* GitHub: https://github.com/gdemin/expss
* Source code: https://github.com/cran/expss
* Date/Publication: 2022-10-16 02:52:38 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "expss")` for more info

</details>

## In both

*   checking whether package ‘expss’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/expss/new/expss.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘expss’ ...
** package ‘expss’ successfully unpacked and MD5 sums checked
** using staged installation
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
Error: package or namespace load failed for ‘expss’:
 object 'mtfrm' not found whilst loading namespace 'expss'
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/expss/new/expss.Rcheck/expss’


```
### CRAN

```
* installing *source* package ‘expss’ ...
** package ‘expss’ successfully unpacked and MD5 sums checked
** using staged installation
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
Error: package or namespace load failed for ‘expss’:
 object 'mtfrm' not found whilst loading namespace 'expss'
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/expss/old/expss.Rcheck/expss’


```
# ggPMX

<details>

* Version: 1.2.8
* GitHub: https://github.com/ggPMXdevelopment/ggPMX
* Source code: https://github.com/cran/ggPMX
* Date/Publication: 2022-06-17 23:10:02 UTC
* Number of recursive dependencies: 177

Run `cloud_details(, "ggPMX")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/ggPMX/new/ggPMX.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ggPMX/DESCRIPTION’ ... OK
* this is package ‘ggPMX’ version ‘1.2.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  [ FAIL 1 | WARN 13 | SKIP 8 | PASS 327 ]
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘ggPMX-guide.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/ggPMX/old/ggPMX.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘ggPMX/DESCRIPTION’ ... OK
* this is package ‘ggPMX’ version ‘1.2.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
  [ FAIL 1 | WARN 11 | SKIP 8 | PASS 327 ]
  Error: Test failures
  Execution halted
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘ggPMX-guide.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 ERROR, 2 NOTEs





```
# ggprism

<details>

* Version: 1.0.3
* GitHub: https://github.com/csdaw/ggprism
* Source code: https://github.com/cran/ggprism
* Date/Publication: 2021-06-08 11:40:02 UTC
* Number of recursive dependencies: 104

Run `cloud_details(, "ggprism")` for more info

</details>

## Newly broken

*   checking whether package ‘ggprism’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggprism/new/ggprism.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘ggprism’ ...
** package ‘ggprism’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'new_data_frame' not found
Error: unable to load R code in package ‘ggprism’
Execution halted
ERROR: lazy loading failed for package ‘ggprism’
* removing ‘/tmp/workdir/ggprism/new/ggprism.Rcheck/ggprism’


```
### CRAN

```
* installing *source* package ‘ggprism’ ...
** package ‘ggprism’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (ggprism)


```
# ggshadow

<details>

* Version: 0.0.2
* GitHub: https://github.com/marcmenem/ggshadow
* Source code: https://github.com/cran/ggshadow
* Date/Publication: 2021-01-22 08:50:03 UTC
* Number of recursive dependencies: 49

Run `cloud_details(, "ggshadow")` for more info

</details>

## Newly broken

*   checking whether package ‘ggshadow’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/ggshadow/new/ggshadow.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

## Installation

### Devel

```
* installing *source* package ‘ggshadow’ ...
** package ‘ggshadow’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in get(x, envir = ns, inherits = FALSE) : 
  object 'new_data_frame' not found
Error: unable to load R code in package ‘ggshadow’
Execution halted
ERROR: lazy loading failed for package ‘ggshadow’
* removing ‘/tmp/workdir/ggshadow/new/ggshadow.Rcheck/ggshadow’


```
### CRAN

```
* installing *source* package ‘ggshadow’ ...
** package ‘ggshadow’ successfully unpacked and MD5 sums checked
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
* DONE (ggshadow)


```
# ggtern

<details>

* Version: 3.3.5
* GitHub: NA
* Source code: https://github.com/cran/ggtern
* Date/Publication: 2021-07-23 05:20:02 UTC
* Number of recursive dependencies: 43

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
  object 'message_wrap' not found
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
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# loon.ggplot

<details>

* Version: 1.3.2
* GitHub: https://github.com/great-northern-diver/loon.ggplot
* Source code: https://github.com/cran/loon.ggplot
* Date/Publication: 2022-10-03 14:50:02 UTC
* Number of recursive dependencies: 104

Run `cloud_details(, "loon.ggplot")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/loon.ggplot/new/loon.ggplot.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.ggplot/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘loon.ggplot’ version ‘1.3.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘loon’

Package suggested but not available for checking: ‘zenplots’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/loon.ggplot/old/loon.ggplot.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘loon.ggplot/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘loon.ggplot’ version ‘1.3.2’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘loon’

Package suggested but not available for checking: ‘zenplots’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# nlmixr2

<details>

* Version: 2.0.8
* GitHub: https://github.com/nlmixr2/nlmixr2
* Source code: https://github.com/cran/nlmixr2
* Date/Publication: 2022-10-23 04:02:36 UTC
* Number of recursive dependencies: 199

Run `cloud_details(, "nlmixr2")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/nlmixr2/new/nlmixr2.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘nlmixr2/DESCRIPTION’ ... OK
* this is package ‘nlmixr2’ version ‘2.0.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'nlmixr2est', 'nlmixr2extra', 'nlmixr2plot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/nlmixr2/old/nlmixr2.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘nlmixr2/DESCRIPTION’ ... OK
* this is package ‘nlmixr2’ version ‘2.0.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available:
  'nlmixr2est', 'nlmixr2extra', 'nlmixr2plot'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# nlmixr2extra

<details>

* Version: 2.0.8
* GitHub: https://github.com/nlmixr2/nlmixr2extra
* Source code: https://github.com/cran/nlmixr2extra
* Date/Publication: 2022-10-22 22:32:34 UTC
* Number of recursive dependencies: 202

Run `cloud_details(, "nlmixr2extra")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/nlmixr2extra/new/nlmixr2extra.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘nlmixr2extra/DESCRIPTION’ ... OK
* this is package ‘nlmixr2extra’ version ‘2.0.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'nlmixr2est', 'symengine'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/nlmixr2extra/old/nlmixr2extra.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘nlmixr2extra/DESCRIPTION’ ... OK
* this is package ‘nlmixr2extra’ version ‘2.0.8’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'nlmixr2est', 'symengine'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# nlmixr2plot

<details>

* Version: 2.0.7
* GitHub: https://github.com/nlmixr2/nlmixr2plot
* Source code: https://github.com/cran/nlmixr2plot
* Date/Publication: 2022-10-20 03:12:36 UTC
* Number of recursive dependencies: 160

Run `cloud_details(, "nlmixr2plot")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/nlmixr2plot/new/nlmixr2plot.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘nlmixr2plot/DESCRIPTION’ ... OK
* this is package ‘nlmixr2plot’ version ‘2.0.7’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'nlmixr2est', 'nlmixr2extra'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/nlmixr2plot/old/nlmixr2plot.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘nlmixr2plot/DESCRIPTION’ ... OK
* this is package ‘nlmixr2plot’ version ‘2.0.7’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Packages required but not available: 'nlmixr2est', 'nlmixr2extra'

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# OpenMx

<details>

* Version: 2.20.7
* GitHub: https://github.com/OpenMx/OpenMx
* Source code: https://github.com/cran/OpenMx
* Date/Publication: 2022-10-19 10:38:03 UTC
* Number of recursive dependencies: 145

Run `cloud_details(, "OpenMx")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/OpenMx/new/OpenMx.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘OpenMx/DESCRIPTION’ ... OK
* this is package ‘OpenMx’ version ‘2.20.7’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘deriv.Rmd’ using ‘UTF-8’... OK
  ‘reg_mimic.Rmd’ using ‘UTF-8’... OK
  ‘factor_analysis.Rmd’ using ‘UTF-8’... OK
  ‘regularization.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 4 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/OpenMx/old/OpenMx.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘OpenMx/DESCRIPTION’ ... OK
* this is package ‘OpenMx’ version ‘2.20.7’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘deriv.Rmd’ using ‘UTF-8’... OK
  ‘reg_mimic.Rmd’ using ‘UTF-8’... OK
  ‘factor_analysis.Rmd’ using ‘UTF-8’... OK
  ‘regularization.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 4 NOTEs





```
# Platypus

<details>

* Version: 3.4.1
* GitHub: NA
* Source code: https://github.com/cran/Platypus
* Date/Publication: 2022-08-15 07:20:20 UTC
* Number of recursive dependencies: 353

Run `cloud_details(, "Platypus")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/Platypus/new/Platypus.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Platypus/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘Platypus’ version ‘3.4.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘PlatypusV3_agedCNS.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/Platypus/old/Platypus.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘Platypus/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘Platypus’ version ‘3.4.1’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘PlatypusV3_agedCNS.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# RcppCensSpatial

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/RcppCensSpatial
* Date/Publication: 2022-06-27 23:00:02 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "RcppCensSpatial")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/RcppCensSpatial/new/RcppCensSpatial.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RcppCensSpatial/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘RcppCensSpatial’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MomTrunc’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/RcppCensSpatial/old/RcppCensSpatial.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘RcppCensSpatial/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘RcppCensSpatial’ version ‘0.3.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘MomTrunc’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# rPBK

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/rPBK
* Date/Publication: 2022-09-01 15:10:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "rPBK")` for more info

</details>

## In both

*   checking whether package ‘rPBK’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/rPBK/new/rPBK.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘rPBK’ ...
** package ‘rPBK’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rPBK’
* removing ‘/tmp/workdir/rPBK/new/rPBK.Rcheck/rPBK’


```
### CRAN

```
* installing *source* package ‘rPBK’ ...
** package ‘rPBK’ successfully unpacked and MD5 sums checked
** using staged installation
Error in loadNamespace(x) : there is no package called ‘rstantools’
Calls: loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: configuration failed for package ‘rPBK’
* removing ‘/tmp/workdir/rPBK/old/rPBK.Rcheck/rPBK’


```
# simpr

<details>

* Version: 0.2.2
* GitHub: https://github.com/statisfactions/simpr
* Source code: https://github.com/cran/simpr
* Date/Publication: 2022-02-13 00:40:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "simpr")` for more info

</details>

## In both

*   checking whether package ‘simpr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/simpr/new/simpr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘simpr’ ...
** package ‘simpr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘listenv’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘simpr’
* removing ‘/tmp/workdir/simpr/new/simpr.Rcheck/simpr’


```
### CRAN

```
* installing *source* package ‘simpr’ ...
** package ‘simpr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘listenv’
Calls: <Anonymous> ... loadNamespace -> withRestarts -> withOneRestart -> doWithOneRestart
Execution halted
ERROR: lazy loading failed for package ‘simpr’
* removing ‘/tmp/workdir/simpr/old/simpr.Rcheck/simpr’


```
# Sofi

<details>

* Version: 0.16.4.8
* GitHub: https://github.com/loerasg/Sofi
* Source code: https://github.com/cran/Sofi
* Date/Publication: 2016-04-10 00:49:47
* Number of recursive dependencies: 63

Run `cloud_details(, "Sofi")` for more info

</details>

## In both

*   checking whether package ‘Sofi’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/Sofi/new/Sofi.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘Sofi’ ...
** package ‘Sofi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘shiny’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘fastmap’
Execution halted
ERROR: lazy loading failed for package ‘Sofi’
* removing ‘/tmp/workdir/Sofi/new/Sofi.Rcheck/Sofi’


```
### CRAN

```
* installing *source* package ‘Sofi’ ...
** package ‘Sofi’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘shiny’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 there is no package called ‘fastmap’
Execution halted
ERROR: lazy loading failed for package ‘Sofi’
* removing ‘/tmp/workdir/Sofi/old/Sofi.Rcheck/Sofi’


```
# SSVS

<details>

* Version: 2.0.0
* GitHub: https://github.com/sabainter/SSVS
* Source code: https://github.com/cran/SSVS
* Date/Publication: 2022-05-29 05:40:09 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "SSVS")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/SSVS/new/SSVS.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SSVS/DESCRIPTION’ ... OK
* this is package ‘SSVS’ version ‘2.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘BoomSpikeSlab’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/SSVS/old/SSVS.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘SSVS/DESCRIPTION’ ... OK
* this is package ‘SSVS’ version ‘2.0.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘BoomSpikeSlab’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
# tidySEM

<details>

* Version: 0.2.3
* GitHub: https://github.com/cjvanlissa/tidySEM
* Source code: https://github.com/cran/tidySEM
* Date/Publication: 2022-04-14 17:50:02 UTC
* Number of recursive dependencies: 170

Run `cloud_details(, "tidySEM")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/tidySEM/new/tidySEM.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tidySEM/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘tidySEM’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘Generating_syntax.Rmd’ using ‘UTF-8’... OK
  ‘Plotting_graphs.Rmd’ using ‘UTF-8’... OK
  ‘Tabulating_results.Rmd’ using ‘UTF-8’... OK
  ‘sem_graph.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
### CRAN

```
* using log directory ‘/tmp/workdir/tidySEM/old/tidySEM.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘tidySEM/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘tidySEM’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
...
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘Generating_syntax.Rmd’ using ‘UTF-8’... OK
  ‘Plotting_graphs.Rmd’ using ‘UTF-8’... OK
  ‘Tabulating_results.Rmd’ using ‘UTF-8’... OK
  ‘sem_graph.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 1 NOTE





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# valse

<details>

* Version: 0.1-0
* GitHub: NA
* Source code: https://github.com/cran/valse
* Date/Publication: 2021-05-31 08:00:02 UTC
* Number of recursive dependencies: 52

Run `cloud_details(, "valse")` for more info

</details>

## In both

*   checking whether package ‘valse’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/valse/new/valse.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘valse’ ...
** package ‘valse’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c EMGLLF.c -o EMGLLF.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c EMGrank.c -o EMGrank.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c a.EMGLLF.c -o a.EMGLLF.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c a.EMGrank.c -o a.EMGrank.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c valse_init.c -o valse_init.o
Error in loadNamespace(x) : there is no package called ‘RcppGSL’
...
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘valse’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/tmp/workdir/valse/new/valse.Rcheck/00LOCK-valse/00new/valse/libs/valse.so':
  /tmp/workdir/valse/new/valse.Rcheck/00LOCK-valse/00new/valse/libs/valse.so: undefined symbol: gsl_vector_free
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/valse/new/valse.Rcheck/valse’


```
### CRAN

```
* installing *source* package ‘valse’ ...
** package ‘valse’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c EMGLLF.c -o EMGLLF.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c EMGrank.c -o EMGrank.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c a.EMGLLF.c -o a.EMGLLF.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c a.EMGrank.c -o a.EMGrank.o
gcc -I"/opt/R/4.1.1/lib/R/include" -DNDEBUG   -I/usr/local/include   -fpic  -g -O2  -c valse_init.c -o valse_init.o
Error in loadNamespace(x) : there is no package called ‘RcppGSL’
...
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘valse’ in dyn.load(file, DLLpath = DLLpath, ...):
 unable to load shared object '/tmp/workdir/valse/old/valse.Rcheck/00LOCK-valse/00new/valse/libs/valse.so':
  /tmp/workdir/valse/old/valse.Rcheck/00LOCK-valse/00new/valse/libs/valse.so: undefined symbol: gsl_vector_free
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/tmp/workdir/valse/old/valse.Rcheck/valse’


```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# vivid

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/vivid
* Date/Publication: 2021-11-20 01:30:02 UTC
* Number of recursive dependencies: 202

Run `cloud_details(, "vivid")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/vivid/new/vivid.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘vivid/DESCRIPTION’ ... OK
* this is package ‘vivid’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘vivid.Rmd’ using ‘UTF-8’... OK
  ‘vividQStart.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
### CRAN

```
* using log directory ‘/tmp/workdir/vivid/old/vivid.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘vivid/DESCRIPTION’ ... OK
* this is package ‘vivid’ version ‘0.2.3’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... NOTE
...
* checking tests ... OK
  Running ‘testthat.R’
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking running R code from vignettes ... NONE
  ‘vivid.Rmd’ using ‘UTF-8’... OK
  ‘vividQStart.Rmd’ using ‘UTF-8’... OK
* checking re-building of vignette outputs ... OK
* DONE
Status: 2 NOTEs





```
# NA

<details>

* Version: NA
* GitHub: NA
* Source code: https://github.com/cran/NA
* Number of recursive dependencies: 0

Run `cloud_details(, "NA")` for more info

</details>

## Error before installation

### Devel

```






```
### CRAN

```






```
# xpose.nlmixr2

<details>

* Version: 0.4.0
* GitHub: NA
* Source code: https://github.com/cran/xpose.nlmixr2
* Date/Publication: 2022-06-08 09:10:02 UTC
* Number of recursive dependencies: 154

Run `cloud_details(, "xpose.nlmixr2")` for more info

</details>

## Error before installation

### Devel

```
* using log directory ‘/tmp/workdir/xpose.nlmixr2/new/xpose.nlmixr2.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘xpose.nlmixr2/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘xpose.nlmixr2’ version ‘0.4.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘nlmixr2est’

Package suggested but not available for checking: ‘nlmixr2’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
### CRAN

```
* using log directory ‘/tmp/workdir/xpose.nlmixr2/old/xpose.nlmixr2.Rcheck’
* using R version 4.1.1 (2021-08-10)
* using platform: x86_64-pc-linux-gnu (64-bit)
* using session charset: UTF-8
* using option ‘--no-manual’
* checking for file ‘xpose.nlmixr2/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘xpose.nlmixr2’ version ‘0.4.0’
* package encoding: UTF-8
* checking package namespace information ... OK
* checking package dependencies ... ERROR
Package required but not available: ‘nlmixr2est’

Package suggested but not available for checking: ‘nlmixr2’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
* DONE
Status: 1 ERROR





```
