# accSDA

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rARPACK’ ‘sparseLDA’
      All declared Imports should be used.
    ```

# adapr

Version: 1.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘plotly’ ‘shinydashboard’
      All declared Imports should be used.
    ```

# affycoretools

Version: 1.48.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘oligo’
    ```

# afmToolkit

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘assertthat’ ‘tibble’
      All declared Imports should be used.
    ```

# aire.zmvm

Version: 0.5.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      testthat results ================================================================
      OK: 58 SKIPPED: 0 FAILED: 9
      1. Failure: .convert_time correctly parses string (@test_data.R#7) 
      2. Failure: .convert_time correctly parses string (@test_data.R#11) 
      3. Failure: .convert_time correctly parses string (@test_data.R#13) 
      4. Failure: .convert_time correctly parses string (@test_data.R#16) 
      5. Failure: .convert_time correctly parses string (@test_data.R#20) 
      6. Failure: .convert_time correctly parses string (@test_data.R#23) 
      7. Failure: .convert_time correctly parses string (@test_data.R#26) 
      8. Failure: .convert_time correctly parses string (@test_data.R#30) 
      9. Failure: .convert_time correctly parses string (@test_data.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 38 marked UTF-8 strings
    ```

# ameco

Version: 0.2.8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.7Mb
      sub-directories of 1Mb or more:
        data  15.6Mb
    ```

# ampliQueso

Version: 1.14.0

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘rnaSeqMap’ ‘knitr’ ‘rgl’ ‘ggplot2’ ‘gplots’ ‘parallel’ ‘doParallel’
      ‘foreach’ ‘VariantAnnotation’ ‘genefilter’ ‘statmod’ ‘xtable’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘samr’
      All declared Imports should be used.
    Packages in Depends field not imported from:
      ‘VariantAnnotation’ ‘doParallel’ ‘foreach’ ‘genefilter’ ‘ggplot2’
      ‘gplots’ ‘knitr’ ‘parallel’ ‘rgl’ ‘statmod’ ‘xtable’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ':::' call which should be '::': ‘rnaSeqMap:::newSeqReads’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    compareCoverages: no visible global function definition for
      ‘read.table’
    getCountTable: no visible global function definition for ‘read.table’
    runAQReport: no visible global function definition for ‘read.table’
    runAQReport: no visible global function definition for ‘rowttests’
    runAQReport: no visible global function definition for ‘p.adjust’
    runAQReport: no visible global function definition for ‘%do%’
    runAQReport: no visible global function definition for ‘foreach’
    runAQReport: no visible global function definition for ‘aggregate’
    runAQReport: no visible global function definition for ‘knit’
    Undefined global functions or variables:
      %do% %dopar% aggregate combn detectCores end foreach hasArg knit
      makeCluster p.adjust permp qual ranges read.table readVcf ref
      registerDoParallel rowttests seqnames start stopCluster str strand
      width
    Consider adding
      importFrom("methods", "hasArg")
      importFrom("stats", "aggregate", "end", "p.adjust", "start")
      importFrom("utils", "combn", "read.table", "str")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# AnalysisPageServer

Version: 1.10.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘Rook’ ‘fork’ ‘FastRWeb’
    ```

*   checking for portable file names ... NOTE
    ```
    ...
      AnalysisPageServer/inst/doc/static-example4/jspm_packages/github/t0m/select2-bootstrap-css@1.4.6/_jekyll/fonts
      AnalysisPageServer/inst/htdocs/client/dist-aps/jspm_packages/github/marionettejs/backbone.marionette@2.4.4
      AnalysisPageServer/inst/htdocs/client/dist-aps/jspm_packages/github/t0m/select2-bootstrap-css@1.4.6/_jekyll/fonts
      AnalysisPageServer/inst/htdocs/client/dist-apss/jspm_packages/github/marionettejs/backbone.marionette@2.4.4
      AnalysisPageServer/inst/htdocs/client/dist-apss/jspm_packages/github/t0m/select2-bootstrap-css@1.4.6/_jekyll/fonts
      AnalysisPageServer/inst/htdocs/client/src/AnalysisPageServer/Pages/Landing/ToolOverview/show/itemview
      AnalysisPageServer/inst/htdocs/client/src/AnalysisPageServer/Pages/Landing/ToolOverview/show/layoutview
      AnalysisPageServer/inst/htdocs/client/src/AnalysisPageServerStatic/Pages/Datasets/show/collectionview
      AnalysisPageServer/vignettes/static-example/jspm_packages/github/marionettejs/backbone.marionette@2.4.4
      AnalysisPageServer/vignettes/static-example/jspm_packages/github/t0m/select2-bootstrap-css@1.4.6/_jekyll/fonts
      AnalysisPageServer/vignettes/static-example2/jspm_packages/github/marionettejs/backbone.marionette@2.4.4
      AnalysisPageServer/vignettes/static-example2/jspm_packages/github/t0m/select2-bootstrap-css@1.4.6/_jekyll/fonts
      AnalysisPageServer/vignettes/static-example3/jspm_packages/github/marionettejs/backbone.marionette@2.4.4
      AnalysisPageServer/vignettes/static-example3/jspm_packages/github/t0m/select2-bootstrap-css@1.4.6/_jekyll/fonts
      AnalysisPageServer/vignettes/static-example4/jspm_packages/github/marionettejs/backbone.marionette@2.4.4
      AnalysisPageServer/vignettes/static-example4/jspm_packages/github/t0m/select2-bootstrap-css@1.4.6/_jekyll/fonts
    
    Tarballs are only required to store paths of up to 100 bytes and cannot
    store those of more than 256 bytes, with restrictions including to 100
    bytes for the final component.
    See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 41.8Mb
      sub-directories of 1Mb or more:
        doc     31.5Mb
        htdocs   8.6Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘FastRWeb’ ‘RUnit’ ‘fork’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Unexported object imported by a ':::' call: ‘log4r:::INFO’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘printTextProtocol’
    uniquify.ids.in.svg.files: no visible global function definition for
      ‘setNames’
    validate.compound.param.value: no visible global function definition
      for ‘setNames’
    Undefined global functions or variables:
      App FILES GET POST SERVER WebResult all.pages analysis analysis.id
      brand capture.output checkEquals clean.tmpdir defineTestSuite dev.cur
      dev.list dev.off events file.params handler handlers head in.analysis
      is.textarea.wrap.request no.rook.fork.msg packageVersion page.params
      params parse.multipart plot plot.file png printTextProtocol
      process.response request resources retrieve runTestSuite sendBin
      server.pid sessionInfo setContentType setHeader setNames status str
      svg temp.plot.file
    Consider adding
      importFrom("grDevices", "dev.cur", "dev.list", "dev.off", "png", "svg")
      importFrom("graphics", "plot")
      importFrom("stats", "setNames")
      importFrom("utils", "capture.output", "head", "packageVersion",
                 "sessionInfo", "str")
    to your NAMESPACE file.
    ```

*   checking installed files from ‘inst/doc’ ... NOTE
    ```
    The following directories should probably not be installed:
      ‘images’, ‘img’
    
    Consider the use of a .Rinstignore file: see ‘Writing R Extensions’,
    or move the vignette sources from ‘inst/doc’ to ‘vignettes’.
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# Anaquin

Version: 1.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > title <- 'Assembly Plot'
    > xlab  <- 'Input Concentration (log2)'
    > ylab  <- 'Sensitivity'
    > 
    > # Sequin names
    > seqs <- row.names(UserGuideData_5.4.5.1)
    > 
    > # Input concentration
    > input <- log2(UserGuideData_5.4.5.1$InputConcent)
    > 
    > # Measured sensitivity
    > measured <- UserGuideData_5.4.5.1$Sn
    > 
    > anaquin <- AnaquinData(analysis='PlotLogistic',
    +                            seqs=seqs,
    +                           input=input,
    +                        measured=measured)
    > 
    > plotLogistic(anaquin, title=title, xlab=xlab, ylab=ylab, showLOA=TRUE)
    Error: !is.null(minX) is not TRUE
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/tPlotLODR.R’ failed.
    Last 13 lines of output:
      [1] "Estmating LODR for 2"
      [1] "Estmating LODR for 3"
      [1] "Estmating LODR for 4"
      
      
      |   | Ratio|     LODR|
      |:--|-----:|--------:|
      |2  |     1| 3.617511|
      |3  |     2| 5.673070|
      |4  |     3| 2.584553|
      |5  |     4| 3.896317|
      Error: !is.null(minX) is not TRUE
      In addition: Warning message:
      Transformation introduced infinite values in continuous y-axis 
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 114-127 (Anaquin.Rmd) 
    Error: processing vignette 'Anaquin.Rmd' failed with diagnostics:
    !is.null(minX) is not TRUE
    Execution halted
    ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘ggplot2’
    A package should be listed in only one of these fields.
    ```

*   checking R code for possible problems ... NOTE
    ```
    RnaQuin.genes: no visible binding for global variable ‘RnaQuinMixtureA’
    RnaQuin.isoforms: no visible binding for global variable
      ‘RnaQuinMixtureA’
    Undefined global functions or variables:
      RnaQuinMixtureA
    ```

# AneuFinder

Version: 1.4.0

## In both

*   checking whether package ‘AneuFinder’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/AneuFinder/new/AneuFinder.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘AneuFinder’ ...
** libs
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c loghmm.cpp -o loghmm.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c R_interface.cpp -o R_interface.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c utility.cpp -o utility.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c init.cpp -o init.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c scalehmm.cpp -o scalehmm.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c densities.cpp -o densities.o
g++ -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o AneuFinder.so R_interface.o densities.o init.o loghmm.o scalehmm.o utility.o -L/usr/lib/R/lib -lR
installing to /home/muelleki/git/R/ggplot2/revdep/checks/AneuFinder/new/AneuFinder.Rcheck/AneuFinder/libs
** R
** inst
** preparing package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/home/muelleki/git/R/ggplot2/revdep/library/AneuFinder/bamsignals/libs/bamsignals.so':
  libhts.so.0: cannot open shared object file: No such file or directory
ERROR: lazy loading failed for package ‘AneuFinder’
* removing ‘/home/muelleki/git/R/ggplot2/revdep/checks/AneuFinder/new/AneuFinder.Rcheck/AneuFinder’

```
### CRAN

```
* installing *source* package ‘AneuFinder’ ...
** libs
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c scalehmm.cpp -o scalehmm.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c utility.cpp -o utility.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c init.cpp -o init.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c R_interface.cpp -o R_interface.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c loghmm.cpp -o loghmm.o
g++  -I/usr/share/R/include -DNDEBUG      -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c densities.cpp -o densities.o
g++ -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o AneuFinder.so R_interface.o densities.o init.o loghmm.o scalehmm.o utility.o -L/usr/lib/R/lib -lR
installing to /home/muelleki/git/R/ggplot2/revdep/checks/AneuFinder/old/AneuFinder.Rcheck/AneuFinder/libs
** R
** inst
** preparing package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/home/muelleki/git/R/ggplot2/revdep/library/AneuFinder/bamsignals/libs/bamsignals.so':
  libhts.so.0: cannot open shared object file: No such file or directory
ERROR: lazy loading failed for package ‘AneuFinder’
* removing ‘/home/muelleki/git/R/ggplot2/revdep/checks/AneuFinder/old/AneuFinder.Rcheck/AneuFinder’

```
# annotatr

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Building CpG islands...
    loading from cache '/home/muelleki//.AnnotationHub/5086'
    Building CpG shores...
    Building CpG shelves...
    Building inter-CpG-islands...
    Annotating...
    Randomizing regions...
    `summarise_each()` is deprecated.
    Use `summarise_all()`, `summarise_at()` or `summarise_if()` instead.
    To map `funs` over a selection of variables, use `summarise_at()`
    Warning in subset_order_tbl(tbl = annotated_random, col = "annot.type",  :
      There are elements in col_order that are not present in the corresponding column. Check for typos, or this could be a result of 0 tallies.
    Warning in bind_rows_(x, .id) :
      Unequal factor levels: coercing to character
    Warning in bind_rows_(x, .id) :
      binding character and factor vector, coercing into character vector
    Warning in bind_rows_(x, .id) :
      binding character and factor vector, coercing into character vector
    Error: processing vignette 'annotatr-vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot_coannotations: no visible binding for global variable ‘.’
    plot_numerical_coannotations: no visible binding for global variable
      ‘.’
    Undefined global functions or variables:
      .
    ```

# anomalyDetection

Version: 0.2.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘caret’
      All declared Imports should be used.
    ```

# aoristic

Version: 0.6

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘slot’
    aoristic.grid: no visible global function definition for ‘browseURL’
    aoristic.shp: no visible global function definition for ‘aggregate’
    aoristic.shp: no visible global function definition for ‘slot’
    aoristic.shp : <anonymous>: no visible global function definition for
      ‘as’
    aoristic.shp : <anonymous>: no visible global function definition for
      ‘slot’
    aoristic.shp: no visible global function definition for ‘browseURL’
    Undefined global functions or variables:
      aggregate as browseURL colorRampPalette contourLines dev.off image
      par png quantile slot
    Consider adding
      importFrom("grDevices", "colorRampPalette", "contourLines", "dev.off",
                 "png")
      importFrom("graphics", "image", "par")
      importFrom("methods", "as", "slot")
      importFrom("stats", "aggregate", "quantile")
      importFrom("utils", "browseURL")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# apsimr

Version: 1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘sensitivity’, ‘APSIMBatch’
    ```

# ArchaeoPhases

Version: 1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘DT’
      All declared Imports should be used.
    ```

# archetypes

Version: 2.2-0

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘MASS’ ‘TSP’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    xyplot.stepArchetypes: no visible global function definition for ‘gray’
    xyplot.stepArchetypes: no visible global function definition for ‘plot’
    xyplot.stepArchetypes: no visible global function definition for
      ‘points’
    xyplot.stepArchetypes: no visible global function definition for
      ‘lines’
    xyplot.weightedArchetypes: no visible binding for global variable
      ‘gray’
    Undefined global functions or variables:
      TSP abline arrows axis box chull cmdscale combn dist ecdf ginv gray
      head history layout lines matlines matplot median mtext optim par
      plot points polygon rgb rnorm sd solve_TSP symbols tail text
    Consider adding
      importFrom("grDevices", "chull", "gray", "rgb")
      importFrom("graphics", "abline", "arrows", "axis", "box", "layout",
                 "lines", "matlines", "matplot", "mtext", "par", "plot",
                 "points", "polygon", "symbols", "text")
      importFrom("stats", "cmdscale", "dist", "ecdf", "median", "optim",
                 "rnorm", "sd")
      importFrom("utils", "combn", "head", "history", "tail")
    to your NAMESPACE file.
    ```

# archivist

Version: 2.1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘archivist.github’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘rmarkdown’, ‘archivist.github’
    ```

# ARPobservation

Version: 1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    reported_observations: no visible global function definition for
      ‘aggregate’
    reported_observations: no visible binding for global variable ‘var’
    rgamma_eq : <anonymous>: no visible global function definition for
      ‘uniroot’
    rgamma_eq: no visible global function definition for ‘runif’
    rgamma_mix_eq : <anonymous>: no visible global function definition for
      ‘uniroot’
    rgamma_mix_eq: no visible global function definition for ‘runif’
    rweibull_eq : <anonymous>: no visible global function definition for
      ‘uniroot’
    rweibull_eq: no visible global function definition for ‘runif’
    smooth_cov: no visible global function definition for ‘dist’
    Undefined global functions or variables:
      aggregate dist integrate nobs pgamma qnorm quantile rbinom rexp
      rgamma rnorm runif rweibull uniroot var
    Consider adding
      importFrom("stats", "aggregate", "dist", "integrate", "nobs", "pgamma",
                 "qnorm", "quantile", "rbinom", "rexp", "rgamma", "rnorm",
                 "runif", "rweibull", "uniroot", "var")
    to your NAMESPACE file.
    ```

# asremlPlus

Version: 2.0-12

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘asreml’
    ```

# asVPC

Version: 1.0.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    VPC.graph : findQuantile: no visible global function definition for
      ‘quantile’
    VPC.graph : findQuantileCI : <anonymous>: no visible global function
      definition for ‘quantile’
    asVPC.binW: no visible global function definition for ‘median’
    asVPC.binW : <anonymous>: no visible global function definition for
      ‘quantile’
    asVPC.binW: no visible global function definition for ‘quantile’
    asVPC.distanceW: no visible global function definition for ‘median’
    asVPC.distanceW : <anonymous>: no visible global function definition
      for ‘quantile’
    asVPC.distanceW: no visible global function definition for ‘quantile’
    makeCOVbin: no visible global function definition for ‘quantile’
    makeCOVbin: no visible global function definition for ‘median’
    read_Simdata: no visible global function definition for ‘read.table’
    Undefined global functions or variables:
      median quantile read.table
    Consider adding
      importFrom("stats", "median", "quantile")
      importFrom("utils", "read.table")
    to your NAMESPACE file.
    ```

# autoimage

Version: 2.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘akima’
    ```

# automap

Version: 1.0-14

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘ggplot2’ ‘gpclib’ ‘maptools’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘geom_point’
    posPredictionInterval: no visible global function definition for
      ‘median’
    posPredictionInterval: no visible global function definition for
      ‘qnorm’
    summary.autoKrige.cv: no visible global function definition for ‘cor’
    summary.autoKrige.cv: no visible global function definition for ‘sd’
    summary.autoKrige.cv: no visible global function definition for ‘IQR’
    Undefined global functions or variables:
      IQR aes_string as.formula chull coord_equal cor facet_wrap fortify
      geom_path geom_point ggplot grey is median modifyList qnorm quantile
      scale_color_gradient2 scale_size_continuous scale_x_continuous
      scale_y_continuous sd
    Consider adding
      importFrom("grDevices", "chull", "grey")
      importFrom("methods", "is")
      importFrom("stats", "IQR", "as.formula", "cor", "median", "qnorm",
                 "quantile", "sd")
      importFrom("utils", "modifyList")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# BaalChIP

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
    Loading required package: Rsamtools
    Loading required package: Biostrings
    Loading required package: XVector
    
    Attaching package: 'Biostrings'
    
    The following object is masked from 'package:base':
    
        strsplit
    
    -samplesheet checks: OK!
    -samplesheet checks: OK!
    -no filters applied yet
    -detected readlengths323436
    
    Error: processing vignette 'BaalChIP.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 198.6Mb
      sub-directories of 1Mb or more:
        data   95.8Mb
        test  101.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    applyBayes: no visible binding for global variable ‘SNP_id’
    plot.filt.barplot: no visible binding for global variable ‘cellname’
    plot.filt.barplot: no visible binding for global variable ‘value’
    plot.filt.barplot: no visible binding for global variable ‘variable’
    plot.filt.boxplot: no visible binding for global variable ‘variable’
    plot.filt.boxplot: no visible binding for global variable ‘value’
    plot.filt.boxplot: no visible binding for global variable ‘coltype’
    plot.filt.pie: no visible binding for global variable ‘variable’
    plot.filt.pie: no visible binding for global variable ‘value.mean’
    plot.simul: no visible binding for global variable ‘readslen’
    plot.simul: no visible binding for global variable ‘perc_right’
    plotadjustment: no visible binding for global variable ‘value’
    plotadjustment: no visible binding for global variable ‘variable’
    Undefined global functions or variables:
      SNP_id cellname coltype perc_right readslen value value.mean variable
    ```

# BacArena

Version: 1.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘sybilSBML’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        libs   3.7Mb
    ```

# backShift

Version: 0.1.4.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘InvariantCausalPrediction’, ‘CompareCausalNetworks’
    ```

# bacon

Version: 1.4.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    .hist: no visible binding for global variable ‘..density..’
    .qq: no visible binding for global variable ‘column’
    Undefined global functions or variables:
      ..density.. column
    ```

# ballr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘XML’ ‘devtools’ ‘ggplot2’ ‘scales’
      All declared Imports should be used.
    ```

# BatchMap

Version: 1.0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        libs   4.2Mb
    ```

# bayesAB

Version: 1.1.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      
      4. Failure: Success (@test-generics.R#43) --------------------------------------
      print(plot(x, rep(0.5, 4))) produced warnings.
      
      
      testthat results ================================================================
      OK: 132 SKIPPED: 0 FAILED: 4
      1. Failure: Success (@test-generics.R#37) 
      2. Failure: Success (@test-generics.R#38) 
      3. Failure: Success (@test-generics.R#42) 
      4. Failure: Success (@test-generics.R#43) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# bayesDP

Version: 1.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# bayesplot

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        R     1.6Mb
        doc   3.6Mb
    ```

# BCEA

Version: 2.2-5

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

# Bclim

Version: 3.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# bcp

Version: 4.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        libs   5.1Mb
    ```

# bcrm

Version: 0.4.6

## In both

*   checking examples ... ERROR
    ```
    ...
    75%   0.8931515 0.9480442
    97.5% 0.9278585 0.9652638
    
    > 
    > ## Posterior distribution of the model parameter using rjags
    > post.rjags<-Posterior.rjags(tox,notox,sdose,ff,prior.alpha
    +   ,burnin.itr=2000,production.itr=2000)
    > print(mean(post.rjags))
    [1] 0.6712911
    > hist(post.rjags)
    > 
    > ## Posterior distribution of the model parameter using BRugs (Windows and i386 Linux only)
    > if(Sys.info()["sysname"] %in% c("Windows","Linux")){
    + 	post.BRugs<-Posterior.BRugs(tox,notox,sdose,ff,prior.alpha
    + 	  ,burnin.itr=2000,production.itr=2000)
    + 	print(mean(post.BRugs))
    + 	hist(post.BRugs)
    + 	}
    Error in loadNamespace(name) : there is no package called ‘BRugs’
    Calls: Posterior.BRugs ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BRugs’
    ```

# bde

Version: 1.0.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    rsample,BoundedDensity: no visible global function definition for
      ‘runif’
    setNormalizationConst,MacroBetaChen99Kernel: no visible global function
      definition for ‘integrate’
    setNormalizationConst,MacroBetaHirukawaTSKernel: no visible global
      function definition for ‘integrate’
    setNormalizationConstant,MacroBetaHirukawaJLNKernel: no visible global
      function definition for ‘integrate’
    setNormalizationConstants,MicroBetaChen99Kernel :
      density.kernelFunction: no visible global function definition for
      ‘dbeta’
    setNormalizationConstants,MicroBetaChen99Kernel : <anonymous>: no
      visible global function definition for ‘integrate’
    show,BoundedDensity: no visible global function definition for ‘str’
    show,KernelDensity: no visible global function definition for ‘str’
    Undefined global functions or variables:
      dbeta integrate runif str
    Consider adding
      importFrom("stats", "dbeta", "integrate", "runif")
      importFrom("utils", "str")
    to your NAMESPACE file.
    ```

# bea.R

Version: 1.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘chron’ ‘colorspace’ ‘gtable’ ‘htmltools’ ‘htmlwidgets’
      ‘httpuv’ ‘magrittr’ ‘munsell’ ‘plyr’ ‘scales’ ‘stringi’ ‘xtable’
      ‘yaml’
      All declared Imports should be used.
    ```

# beadarray

Version: 2.26.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘Nozzle.R1’ ‘affy’ ‘ggbio’ ‘hwriter’ ‘lumi’ ‘vsn’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Packages in Depends field not imported from:
      ‘ggplot2’ ‘methods’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    Unexported objects imported by ':::' calls:
      ‘BeadDataPackR:::combineFiles’ ‘BeadDataPackR:::readHeader’
      ‘Biobase:::assayDataStorageMode’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘illuminaForeground_6x6’ ‘locsIndicesToGrid’ ‘obtainLocs’
      ‘simpleXMLparse’
    ```

*   checking foreign function calls ... NOTE
    ```
    Foreign function call to a different package:
      .Call("roundLocsFileValues", ..., PACKAGE = "BeadDataPackR")
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      geom_hline geom_point geom_tile geom_vline ggplot ggsave ggtitle
      hwrite hwriteImage is jpeg loess lowess lumiHumanIDMapping_dbconn
      lumiMouseIDMapping_dbconn lumiRatIDMapping_dbconn lumiT menu
      metaTemplate model.matrix new newCustomReport newFigure newParagraph
      newSection newTable normalize.invariantset normalize.qspline openPage
      opts p.adjust packageDescription pdf platformSigs plotIdeogram png
      predict qplot rainbow read.csv read.table rgb rsn runif
      scale_fill_discrete scale_fill_gradient setTxtProgressBar stat_binhex
      theme theme_blank theme_bw tracks txtProgressBar value value.1 vsn2
      write.csv write.table writeReport xlab ylab
    Consider adding
      importFrom("grDevices", "col2rgb", "dev.off", "jpeg", "pdf", "png",
                 "rainbow", "rgb")
      importFrom("methods", "as", "callNextMethod", "is", "new")
      importFrom("stats", "aggregate", "approx", "density", "loess",
                 "lowess", "model.matrix", "p.adjust", "predict", "runif")
      importFrom("utils", "data", "menu", "packageDescription", "read.csv",
                 "read.table", "setTxtProgressBar", "txtProgressBar",
                 "write.csv", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# benchmark

Version: 0.3-6

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘multcomp’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘algorithms’
    stripchart.AlgorithmPerformance: no visible binding for global variable
      ‘value’
    stripchart.AlgorithmPerformance: no visible binding for global variable
      ‘samples’
    Undefined global functions or variables:
      abline addEdge agopen algorithms as.formula axis barplot box cancor
      characteristics col2rgb getDefaultAttrs gray is layout legend lines
      lm median mi.plugin mtext na.omit new par points predict rect
      relation_is_strict_weak_order reshape rgb samples segments terms
      theme_text value
    Consider adding
      importFrom("grDevices", "col2rgb", "gray", "rgb")
      importFrom("graphics", "abline", "axis", "barplot", "box", "layout",
                 "legend", "lines", "mtext", "par", "points", "rect",
                 "segments")
      importFrom("methods", "is", "new")
      importFrom("stats", "as.formula", "cancor", "lm", "median", "na.omit",
                 "predict", "reshape", "terms")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# benchr

Version: 0.2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 184 SKIPPED: 0 FAILED: 10
      1.  Failure: Boxplot ggplot without log (@test-plot.R#39) 
      2.  Failure: Boxplot ggplot without log (@test-plot.R#40) 
      3.  Failure: Boxplot ggplot with log and horizontal (@test-plot.R#53) 
      4.  Failure: Boxplot ggplot with log and horizontal (@test-plot.R#54) 
      5.  Failure: Boxplot ggplot with violin geom (@test-plot.R#67) 
      6.  Failure: Boxplot ggplot with violin geom (@test-plot.R#68) 
      7.  Failure: Scatter plot ggplot with log (@test-plot.R#83) 
      8.  Failure: Scatter plot ggplot with log (@test-plot.R#84) 
      9.  Failure: Scatter plot ggplot without log (@test-plot.R#99) 
      10. Failure: Scatter plot ggplot without log (@test-plot.R#100) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# bife

Version: 0.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.7Mb
      sub-directories of 1Mb or more:
        data   2.1Mb
        libs   3.3Mb
    ```

# bigKRLS

Version: 2.0.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        libs   4.8Mb
    ```

# BIGL

Version: 1.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning: Transformation introduced infinite values in continuous x-axis
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Transformation introduced infinite values in continuous x-axis
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Removed 199 rows containing non-finite values (stat_contour).
    Warning in is.na(verts[, 2]) :
      is.na() applied to non-(list or vector) of type 'NULL'
    Warning in is.na(verts[, 3]) :
      is.na() applied to non-(list or vector) of type 'NULL'
    Warning in is.na(verts[, 1]) :
      is.na() applied to non-(list or vector) of type 'NULL'
    Warning in is.na(verts[, 3]) :
      is.na() applied to non-(list or vector) of type 'NULL'
    Warning in is.na(verts[, 1]) :
      is.na() applied to non-(list or vector) of type 'NULL'
    Warning in is.na(verts[, 2]) :
      is.na() applied to non-(list or vector) of type 'NULL'
    Quitting from lines 240-243 (analysis.Rmd) 
    Error: processing vignette 'analysis.Rmd' failed with diagnostics:
    there is no package called 'webshot'
    Execution halted
    ```

# bigstatsr

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      'dimnames' applied to non-array
      1: sparseSVM::sparseSVM(X2, y.factor, alpha = alpha, lambda.min = lambda.min, penalty.factor = m) at testthat/test-spSVM.R:32
      
      2. Error: equality with sparseSVM with only half the data (@test-spSVM.R#68) ---
      'dimnames' applied to non-array
      1: sparseSVM::sparseSVM(X2[ind, ], y.factor[ind], alpha = alpha, lambda.min = lambda.min, 
             penalty.factor = m) at testthat/test-spSVM.R:68
      
      testthat results ================================================================
      OK: 1629 SKIPPED: 0 FAILED: 2
      1. Error: equality with sparseSVM with all data (@test-spSVM.R#32) 
      2. Error: equality with sparseSVM with only half the data (@test-spSVM.R#68) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.7Mb
      sub-directories of 1Mb or more:
        extdata   2.3Mb
        libs      9.6Mb
    ```

# binom

Version: 1.1-1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘ggplot2’ ‘lattice’ ‘polynom’ ‘tcltk’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      definition for ‘tkdestroy’
    var.probit: no visible global function definition for ‘qnorm’
    var.probit: no visible global function definition for ‘dnorm’
    Undefined global functions or variables:
      abline aes_string aggregate approx axis binomial box col.whitebg
      current.panel.limits dbeta dbinom dnorm facet_wrap find geom_polygon
      ggplot heat.colors integral legend lines llines lpoints lpolygon
      ltext optim panel.abline panel.levelplot panel.xyplot pbeta plot
      pnorm polynomial predict qbeta qnorm rbinom reorder spline tclObj
      tclVar theme_bw tkbutton tkcheckbutton tkdestroy tkframe tklabel
      tkpack tkradiobutton tkscale tktoplevel tkwm.title trellis.par.get
      trellis.par.set uniroot xlab xlim xyplot ylab ylim
    Consider adding
      importFrom("grDevices", "heat.colors")
      importFrom("graphics", "abline", "axis", "box", "legend", "lines",
                 "plot")
      importFrom("stats", "aggregate", "approx", "binomial", "dbeta",
                 "dbinom", "dnorm", "optim", "pbeta", "pnorm", "predict",
                 "qbeta", "qnorm", "rbinom", "reorder", "spline", "uniroot")
      importFrom("utils", "find")
    to your NAMESPACE file.
    ```

# biobroom

Version: 1.8.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘DESeq2’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Missing or unexported object: ‘dplyr::tbl_dt’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      for ‘colData’
    tidy.deSet: no visible global function definition for ‘exprs<-’
    tidy.deSet: no visible binding for global variable ‘value’
    tidy.deSet: no visible binding for global variable ‘gene’
    tidy.deSet: no visible global function definition for ‘pData’
    tidy.qvalue: no visible binding for global variable ‘smoothed’
    tidy.qvalue: no visible binding for global variable ‘pi0’
    tidy.qvalue: no visible binding for global variable ‘lambda’
    tidy_matrix: no visible binding for global variable ‘value’
    tidy_matrix: no visible binding for global variable ‘gene’
    Undefined global functions or variables:
      . DGEList calcNormFactors colData counts design end estimate
      estimateSizeFactors exprs<- fData<- gene gr is lambda model.matrix
      p.adjust pData pData<- pi0 protein rowRanges sample.id seqnames
      setNames smoothed start tbl_dt term value voom voomWithQualityWeights
    Consider adding
      importFrom("methods", "is")
      importFrom("stats", "end", "model.matrix", "p.adjust", "setNames",
                 "start")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# bioCancer

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'bioCancer.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘magrittr’ ‘ggplot2’ ‘lubridate’ ‘tidyr’ ‘cgdsr’ ‘RCurl’ ‘XML’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 22.3Mb
      sub-directories of 1Mb or more:
        base        6.9Mb
        bioCancer   3.1Mb
        doc         2.8Mb
        quant       7.7Mb
    ```

# biogas

Version: 1.8.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'biogas_quick_start.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `mhchem.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.58 \usepackage
                    [colorlinks = true, urlcolor = blue]{hyperref} % Must be loa...
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# biogram

Version: 1.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘seqinr’
    ```

# biomod2

Version: 3.3-7

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    	> Projecting GuloGulo_AllData_RUN1_RF ...
    	> Projecting GuloGulo_AllData_RUN2_RF ...
    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= Done -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    >                           
    > 
    > # 4. Creating the ensemble projections
    > BIOMOD_EnsembleForecasting( projection.output = myBiomodProjection,
    +                             EM.output = myBiomodEM)
    
    -=-=-=-=-=-=-=-=-=-=-=-= Do Ensemble Models Projections -=-=-=-=-=-=-=-=-=-=-=-=
    
    
    	> Projecting GuloGulo_EMmeanByTSS_RF_mergedRun_mergedData ...Error in calc(formal_predictions, function(x) { : 
      unused arguments (function(x) {
        m <- mean(x)
        if (on_0_1000) m <- round(m)
        return(m)
    }, filename = filename, overwrite = TRUE)
    Calls: BIOMOD_EnsembleForecasting ... .local -> .predict.EMmean_biomod2_model.RasterStack
    Execution halted
    ```

*   checking whether package ‘biomod2’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘raster::calc’ by ‘ggplot2::calc’ when loading ‘biomod2’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/biomod2/new/biomod2.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      as.numeric(predict.maxent(get_formal_model(object), x.no.na)[, "1"])
      proj.out[-attr(x.no.na, "na.action")] <- proj.not.na }
      return(proj.out) })
    .predict.MAXENT.Tsuruoka_biomod2_model.RasterStack: possible error in
      return(proj.out): unused argument (function(x) { proj.out <- rep(NA,
      nrow(x)) x.no.na <- na.omit(x) if (nrow(x.no.na)) { proj.not.na <-
      as.numeric(predict.maxent(get_formal_model(object), x.no.na)[, "1"])
      proj.out[-attr(x.no.na, "na.action")] <- proj.not.na }
      return(proj.out) })
    .predict.MAXENT.Tsuruoka_biomod2_model.RasterStack: possible error in
      }): unused argument (function(x) { proj.out <- rep(NA, nrow(x))
      x.no.na <- na.omit(x) if (nrow(x.no.na)) { proj.not.na <-
      as.numeric(predict.maxent(get_formal_model(object), x.no.na)[, "1"])
      proj.out[-attr(x.no.na, "na.action")] <- proj.not.na }
      return(proj.out) })
    BinaryTransformation,RasterStack: possible error in calc(data,
      function(x) {: unused argument (function(x) { x >= threshold })
    BinaryTransformation,RasterStack: possible error in x >= threshold:
      unused argument (function(x) { x >= threshold })
    BinaryTransformation,RasterStack: possible error in }): unused argument
      (function(x) { x >= threshold })
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ecospat’
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    BugReports field is not a suitable URL but contains an email address
      which will be used as from R 3.4.0
    ```

# bioplots

Version: 0.0.1

## In both

*   checking files in ‘vignettes’ ... NOTE
    ```
    Package has no Sweave vignette sources and no VignetteBuilder field.
    ```

# BioStatR

Version: 2.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    cvar: no visible global function definition for ‘sd’
    eta2: no visible global function definition for ‘lm’
    eta2: no visible global function definition for ‘as.formula’
    gg_qqplot: no visible binding for global variable ‘qnorm’
    gg_qqplot: no visible global function definition for ‘quantile’
    panel.hist: no visible global function definition for ‘par’
    panel.hist: no visible global function definition for ‘hist’
    panel.hist: no visible global function definition for ‘rect’
    plotcdf2: no visible global function definition for ‘colorRampPalette’
    plotcdf2 : jet.colors: no visible global function definition for ‘gray’
    plotcdf2: no visible global function definition for ‘persp’
    poi.ci: no visible global function definition for ‘qchisq’
    Undefined global functions or variables:
      as.formula colorRampPalette gray hist lm par persp qchisq qf qnorm
      quantile rect sd
    Consider adding
      importFrom("grDevices", "colorRampPalette", "gray")
      importFrom("graphics", "hist", "par", "persp", "rect")
      importFrom("stats", "as.formula", "lm", "qchisq", "qf", "qnorm",
                 "quantile", "sd")
    to your NAMESPACE file.
    ```

# biotmle

Version: 1.0.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘SuperLearner’ ‘biotmleData’
      All declared Imports should be used.
    ```

# blandr

Version: 0.4.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# blastula

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rJava’
      All declared Imports should be used.
    ```

# blkbox

Version: 1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘bigrf’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘glmnet’ ‘gtools’ ‘knitr’ ‘nnet’ ‘parallel’ ‘rJava’ ‘reshape’
      ‘rmarkdown’ ‘shinyjs’
      All declared Imports should be used.
    Missing or unexported object: ‘xgboost::predict’
    ```

# bmlm

Version: 1.3.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 32.9Mb
      sub-directories of 1Mb or more:
        libs  32.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# bmmix

Version: 0.1-2

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    bmmix : LL.y: no visible global function definition for ‘dmultinom’
    bmmix : LL.x : <anonymous>: no visible global function definition for
      ‘dmultinom’
    bmmix : LPrior.alpha: no visible global function definition for ‘dexp’
    bmmix : alpha.move: no visible global function definition for ‘rnorm’
    bmmix : alpha.move: no visible global function definition for ‘runif’
    bmmix : phi.move: no visible global function definition for ‘rnorm’
    bmmix : phi.move: no visible global function definition for ‘runif’
    bmmix: no visible global function definition for ‘read.table’
    Undefined global functions or variables:
      dexp dmultinom read.table rnorm runif
    Consider adding
      importFrom("stats", "dexp", "dmultinom", "rnorm", "runif")
      importFrom("utils", "read.table")
    to your NAMESPACE file.
    ```

# bodenmiller

Version: 0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        data   8.7Mb
    ```

# bomrang

Version: 0.0.8

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [2019] "IDE00437.201711210540.tif" "IDE00437.201711210550.tif"
      [2021] "IDE00437.201711210600.tif" "IDE00437.201711210610.tif"
      [2023] "IDE00437.201711210620.tif" "IDE00437.201711210630.tif"
      [2025] "IDE00437.201711210640.tif" "IDE00437.201711210650.tif"
      [2027] "IDE00437.201711210700.tif" "IDE00437.201711210710.tif"
      [2029] "IDE00437.201711210720.tif" "IDE00437.201711210730.tif"
      trying URL 'ftp://ftp.bom.gov.au/anon/gen/gms/IDE00425.201711210730.tif'
      Content type 'unknown' length 18648256 bytes (17.8 MB)
      ==================================================
      testthat results ================================================================
      OK: 102 SKIPPED: 0 FAILED: 1
      1. Error: caching utils list files in cache and delete when asked (@test-check_cache.R#47) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# bossMaps

Version: 0.1.0

## Newly broken

*   checking whether package ‘bossMaps’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::calc’ by ‘raster::calc’ when loading ‘bossMaps’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/bossMaps/new/bossMaps.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rgdal’ ‘tidyr’
      All declared Imports should be used.
    ```

# BradleyTerryScalable

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        libs   6.1Mb
    ```

# BrailleR

Version: 0.26.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘installr’
    ```

# brainGraph

Version: 1.0.0

## In both

*   checking whether package ‘brainGraph’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/brainGraph/new/brainGraph.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘brainGraph’ ...
** package ‘brainGraph’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** preparing package for lazy loading
R session is headless; GTK+ not initialized.

(R:37582): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
Error: package or namespace load failed for ‘cairoDevice’:
 .onLoad failed in loadNamespace() for 'cairoDevice', details:
  call: fun(libname, pkgname)
  error: GDK display not found - please make sure X11 is running
Error : package ‘cairoDevice’ could not be loaded
ERROR: lazy loading failed for package ‘brainGraph’
* removing ‘/home/muelleki/git/R/ggplot2/revdep/checks/brainGraph/new/brainGraph.Rcheck/brainGraph’

```
### CRAN

```
* installing *source* package ‘brainGraph’ ...
** package ‘brainGraph’ successfully unpacked and MD5 sums checked
** R
** data
*** moving datasets to lazyload DB
** inst
** preparing package for lazy loading
R session is headless; GTK+ not initialized.

(R:37542): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
Error: package or namespace load failed for ‘cairoDevice’:
 .onLoad failed in loadNamespace() for 'cairoDevice', details:
  call: fun(libname, pkgname)
  error: GDK display not found - please make sure X11 is running
Error : package ‘cairoDevice’ could not be loaded
ERROR: lazy loading failed for package ‘brainGraph’
* removing ‘/home/muelleki/git/R/ggplot2/revdep/checks/brainGraph/old/brainGraph.Rcheck/brainGraph’

```
# brazilmaps

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ggthemes’
    ```

# breathtestcore

Version: 0.4.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘breathteststan’
    ```

# breathteststan

Version: 0.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 28.9Mb
      sub-directories of 1Mb or more:
        libs  28.7Mb
    ```

# brms

Version: 1.10.2

## Newly fixed

*   R CMD check timed out
    

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Start sampling
    Compiling the C++ model
    Start sampling
    Using 10 posterior samples for ppc type 'dens_overlay' by default.
    Using 10 posterior samples for ppc type 'dens_overlay' by default.
    Warning: Found 1 observations with a pareto_k > 0.7 in model 'fit1'. It is recommended to set 'reloo = TRUE' in order to calculate the ELPD without the assumption that these observations are negligible. This will refit the model 1 times to compute the ELPDs for the problematic observations directly.
    Warning: Found 1 observations with a pareto_k > 0.7 in model 'fit2'. It is recommended to set 'reloo = TRUE' in order to calculate the ELPD without the assumption that these observations are negligible. This will refit the model 1 times to compute the ELPDs for the problematic observations directly.
    Compiling the C++ model
    Start sampling
    Compiling the C++ model
    Start sampling
    Compiling the C++ model
    Start sampling
    Compiling the C++ model
    Start sampling
    Warning in file(file, "r") :
      cannot open file 'C:/Users/paulb/Dropbox/Psychologie/Paper/2015_Bayesian_Regression_Models/Models/MCMCglmm/phylo.nex': No such file or directory
    Quitting from lines 35-40 (brms_phylogenetics.Rmd) 
    Error: processing vignette 'brms_phylogenetics.Rmd' failed with diagnostics:
    cannot open the connection
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        R     2.2Mb
        doc   2.4Mb
    ```

# broom

Version: 0.4.2

## In both

*   checking examples ... ERROR
    ```
    ...
    +   f2 <- Finance[1:300, "hml"] - rf
    +   f3 <- Finance[1:300, "smb"] - rf
    +   h <- cbind(f1, f2, f3)
    +   res2 <- gmm(z ~ f1 + f2 + f3, x = h)
    +   
    +   td2 <- tidy(res2, conf.int = TRUE)
    +   td2
    +   
    +   # coefficient plot
    +   td2 %>%
    +     mutate(variable = reorder(variable, estimate)) %>%
    +     ggplot(aes(estimate, variable)) +
    +     geom_point() +
    +     geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    +     facet_wrap(~ term) +
    +     geom_vline(xintercept = 0, color = "red", lty = 2)
    + }
    Error in `colnames<-`(`*tmp*`, value = c("conf.low", "conf.high")) : 
      attempt to set 'colnames' on an object with less than two dimensions
    Calls: tidy -> tidy.gmm -> process_lm -> colnames<-
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Complete output:
      > library(testthat)
      > test_check("broom")
      Loading required package: broom
      Error in lahman_df() : could not find function "lahman_df"
      Calls: test_check ... with_reporter -> force -> source_file -> eval -> eval -> tbl
      testthat results ================================================================
      OK: 621 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

# brotli

Version: 1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        bin    2.3Mb
        doc    1.2Mb
        libs   2.2Mb
    ```

# bsam

Version: 1.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rworldxtra’ ‘sp’
      All declared Imports should be used.
    ```

# bssm

Version: 0.1.1-1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 29.4Mb
      sub-directories of 1Mb or more:
        libs  28.5Mb
    ```

# btergm

Version: 1.9.0

## In both

*   checking whether package ‘btergm’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/btergm/new/btergm.Rcheck/00install.out’ for details.
    ```

# BTSPAS

Version: 2014.0901

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘BRugs’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    trace_plot: no visible global function definition for ‘text’
    trace_plot: no visible global function definition for ‘close.screen’
    visualize.muTT.prior: no visible global function definition for ‘rnorm’
    visualize.muTT.prior: no visible global function definition for ‘stack’
    visualize.muTT.prior: no visible global function definition for
      ‘boxplot’
    Undefined global functions or variables:
      abline acf approx boxplot chisq.test close.screen dbinom density
      dev.off dmultinom flush.console lines lm matplot median par pdf plot
      pnorm points quantile rbinom rmultinom rnorm runif screen sd segments
      spline split.screen stack text var write.table
    Consider adding
      importFrom("grDevices", "dev.off", "pdf")
      importFrom("graphics", "abline", "boxplot", "close.screen", "lines",
                 "matplot", "par", "plot", "points", "screen", "segments",
                 "split.screen", "text")
      importFrom("stats", "acf", "approx", "chisq.test", "dbinom", "density",
                 "dmultinom", "lm", "median", "pnorm", "quantile", "rbinom",
                 "rmultinom", "rnorm", "runif", "sd", "spline", "var")
      importFrom("utils", "flush.console", "stack", "write.table")
    to your NAMESPACE file.
    ```

# BubbleTree

Version: 2.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in get_engine(options$engine) :
      Unknown language engine 'rr' (must be registered via knit_engines$set()).
    Warning in get_engine(options$engine) :
      Unknown language engine 'rr' (must be registered via knit_engines$set()).
    Warning in get_engine(options$engine) :
      Unknown language engine 'rr' (must be registered via knit_engines$set()).
    Error: processing vignette 'BubbleTree-vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 25.4Mb
      sub-directories of 1Mb or more:
        data  22.9Mb
        doc    2.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    annoByOverlap,Annotate: no visible binding for global variable
      'queryHits'
    Undefined global functions or variables:
      queryHits
    ```

# CAFE

Version: 1.12.0

## In both

*   checking whether package ‘CAFE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/CAFE/new/CAFE.Rcheck/00install.out’ for details.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    facetPlot: no visible global function definition for 'dev.off'
    facetPlot: no visible global function definition for 'na.omit'
    fisher.method: no visible global function definition for 'pchisq'
    fisherExact: no visible global function definition for 'fisher.test'
    makelevels: no visible global function definition for 'na.omit'
    rawPlot: no visible global function definition for 'png'
    rawPlot: no visible global function definition for 'dev.off'
    rawPlot: no visible global function definition for 'data'
    slidPlot: no visible global function definition for 'png'
    slidPlot: no visible global function definition for 'dev.off'
    slidPlot: no visible global function definition for 'data'
    Undefined global functions or variables:
      chisq.test data dev.off download.file fisher.test na.omit p.adjust
      pchisq png read.table setTxtProgressBar txtProgressBar
    Consider adding
      importFrom("grDevices", "dev.off", "png")
      importFrom("stats", "chisq.test", "fisher.test", "na.omit", "p.adjust",
                 "pchisq")
      importFrom("utils", "data", "download.file", "read.table",
                 "setTxtProgressBar", "txtProgressBar")
    to your NAMESPACE file.
    ```

# caffsim

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘markdown’
      All declared Imports should be used.
    ```

# CAnD

Version: 1.8.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    CAnD : pairedTtest: no visible global function definition for ‘t.test’
    barPlotAncest: no visible binding for global variable ‘id’
    barPlotAncest: no visible binding for global variable ‘value’
    barPlotAncest: no visible binding for global variable ‘variable’
    calc_combP: no visible binding for global variable ‘var’
    calc_combP: no visible global function definition for ‘pchisq’
    plotPvals: no visible binding for global variable ‘numPs’
    plotPvals: no visible binding for global variable ‘pval’
    Undefined global functions or variables:
      id numPs pchisq pval t.test value var variable
    Consider adding
      importFrom("stats", "pchisq", "t.test", "var")
    to your NAMESPACE file.
    ```

# caret

Version: 6.0-77

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data     1.5Mb
        models   2.4Mb
    ```

# caretEnsemble

Version: 2.0.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
                                                     ^~~~~~
      tests/testthat/test-theme.r:325:49: style: Only use double-quotes.
            axis.line.y.right = element_line(colour = 'yellow')
                                                      ^~~~~~~~
      tests/testthat/test-viridis.R:16:1: style: Trailing whitespace is superfluous.
        
      ^~
      
      
      testthat results ================================================================
      OK: 733 SKIPPED: 1 FAILED: 1
      1. Failure: Code Lint (@test-lintr.R#25) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# CATALYST

Version: 1.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.2Mb
      sub-directories of 1Mb or more:
        data   2.3Mb
        doc    8.5Mb
    ```

# cate

Version: 1.0.4

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'cate-vignette.tex' failed.
    BibTeX errors:
    The top-level auxiliary file: cate-vignette.aux
    I couldn't open style file chicago.bst
    ---line 40 of file cate-vignette.aux
     : \bibstyle{chicago
     :                  }
    I'm skipping whatever remains of this command
    I found no style file---while reading file cate-vignette.aux
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# Causata

Version: 4.2-0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    PredictivePowerCv: no visible global function definition for ‘sd’
    ReadCausataCsv: no visible global function definition for ‘read.csv’
    ToPmml.GlmnetModelDefinition: no visible global function definition for
      ‘coef’
    ToPmml.GlmnetModelDefinition: no visible global function definition for
      ‘terms.formula’
    ValidateModel: no visible global function definition for ‘predict’
    predict.GlmnetModelDefinition: no visible global function definition
      for ‘model.matrix’
    predict.GlmnetModelDefinition: no visible binding for global variable
      ‘contrasts’
    predict.GlmnetModelDefinition: no visible global function definition
      for ‘predict’
    Undefined global functions or variables:
      coef contrasts dbGetQuery median model.matrix na.omit predict
      quantile read.csv sd terms.formula
    Consider adding
      importFrom("stats", "coef", "contrasts", "median", "model.matrix",
                 "na.omit", "predict", "quantile", "sd", "terms.formula")
      importFrom("utils", "read.csv")
    to your NAMESPACE file.
    ```

# cda

Version: 2.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.9Mb
      sub-directories of 1Mb or more:
        libs   7.6Mb
    ```

# cellHTS2

Version: 2.40.0

## In both

*   checking Rd cross-references ... WARNING
    ```
    Unknown package ‘cellHTS’ in Rd xrefs
    ```

*   checking running R code from vignettes ...
    ```
       ‘cellhts2.Rnw’ ... OK
       ‘cellhts2Complete.Rnw’ ... failed
       ‘twoChannels.Rnw’ ... OK
       ‘twoWay.Rnw’ ... OK
     WARNING
    Errors in running code in vignettes:
    when running code in ‘cellhts2Complete.Rnw’
      ...
    > categs <- cateGOry(genes, unlist(goids, use.names = FALSE))
    Loading required namespace: GO.db
    Failed with error:  ‘there is no package called ‘GO.db’’
    
      When sourcing ‘cellhts2Complete.R’:
    Error: use 'biocLite("GO.db")' to install the GO.db package
    recover called non-interactively; frames dumped, use debugger() to view
    > proc.time()
       user  system elapsed 
     35.220   0.300  42.684 
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘RColorBrewer’ ‘Biobase’ ‘genefilter’ ‘splots’ ‘vsn’ ‘hwriter’
      ‘locfit’ ‘grid’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        KcViab   2.0Mb
        doc      1.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘genefilter’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    Unexported object imported by a ':::' call: ‘Biobase:::.showAnnotatedDataFrame’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' calls not declared from:
      ‘GO.db’ ‘KEGG.db’
    ```

# cellity

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: Biobase
    Welcome to Bioconductor
    
        Vignettes contain introductory material; view with
        'browseVignettes()'. To cite Bioconductor, see
        'citation("Biobase")', and for packages 'citation("pkgname")'.
    
    Loading required package: IRanges
    Loading required package: S4Vectors
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: GO.db
    Loading required package: lattice
    Error: processing vignette 'cellity_vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# CellNOptR

Version: 1.22.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘CellNOptR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: CNOdata
    > ### Title: Get data from a CellNOpt data repository
    > ### Aliases: CNOdata
    > 
    > ### ** Examples
    > 
    > readSIF(CNOdata("PKN-ToyMMB.sif"))
    Error in matrix(sif, ncol = 3, byrow = TRUE) : 
      'data' must be of a vector type, was 'NULL'
    Calls: readSIF -> matrix
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘RBGL’ ‘graph’ ‘hash’ ‘ggplot2’ ‘RCurl’ ‘Rgraphviz’ ‘XML’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls to packages already attached by Depends:
      ‘RBGL’ ‘RCurl’ ‘Rgraphviz’ ‘XML’ ‘hash’
      Please remove these calls from your code.
    'library' or 'require' call to ‘igraph’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Packages in Depends field not imported from:
      ‘RCurl’ ‘Rgraphviz’ ‘XML’ ‘ggplot2’ ‘hash’ ‘methods’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    Undefined global functions or variables:
      aes arrows axis close.screen combn count.fields dev.new dev.off
      element_rect element_text facet_grid geom_line geom_point getURL
      ggplot has.key hash heat.colors igraph.from.graphNEL image
      installed.packages layoutGraph lines new par pdf plot.new png points
      read.csv read.table rect renderGraph rgb rnorm runif screen segments
      split.screen svg tail text theme theme_bw toDot values var
      write.table xlab xmlApply xmlChildren xmlGetAttr xmlRoot xmlTreeParse
      ylab ylim
    Consider adding
      importFrom("grDevices", "dev.new", "dev.off", "heat.colors", "pdf",
                 "png", "rgb", "svg")
      importFrom("graphics", "arrows", "axis", "close.screen", "image",
                 "lines", "par", "plot.new", "points", "rect", "screen",
                 "segments", "split.screen", "text")
      importFrom("methods", "new")
      importFrom("stats", "rnorm", "runif", "var")
      importFrom("utils", "combn", "count.fields", "installed.packages",
                 "read.csv", "read.table", "tail", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# cellWise

Version: 1.0.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: ggplot2
    Loading required package: perry
    Loading required package: parallel
    Loading required package: robustbase
    Quitting from lines 121-205 (DDC_examples.Rmd) 
    Error: processing vignette 'DDC_examples.Rmd' failed with diagnostics:
    unused argument (range = c(0, 1))
    Execution halted
    ```

# Census2016

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘data.table’
      All declared Imports should be used.
    ```

# ChannelAttributionApp

Version: 1.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Checking should be performed on sources prepared by ‘R CMD build’.
    ```

# ChemmineR

Version: 2.28.3

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘ChemmineOB’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘ChemmineDrugs’ ‘ChemmineOB’ ‘RPostgreSQL’ ‘RSQLite’ ‘fmcsR’ ‘png’
      ‘snow’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Namespace in Imports field not imported from: ‘BiocGenerics’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    smartsSearchOB: no visible global function definition for
      ‘smartsSearch_OB’
    smile2sdfFile: no visible global function definition for
      ‘convertFormatFile’
    smiles2sdf: no visible global function definition for ‘convertFormat’
    smiles2sdfOB: no visible global function definition for ‘convertFormat’
    write.SMI: no visible global function definition for ‘write.table’
    Undefined global functions or variables:
      AW C1 C1.1 C2 C2.1 browseURL canonicalNumbering_OB clusterApplyLB
      clusterExport combn convertFormat convertFormatFile data dev.off
      error exactMass_OB fingerprint_OB fmcs forEachMol pdf
      postgresqlCopyInDataframe postgresqlQuoteId postgresqlTableRef
      postgresqlgetResult postgresqlpqExec postscript prop_OB rainbow
      read.delim read.table readPNG rgb smartsSearch_OB str string
      write.table
    Consider adding
      importFrom("grDevices", "dev.off", "pdf", "postscript", "rainbow",
                 "rgb")
      importFrom("utils", "browseURL", "combn", "data", "read.delim",
                 "read.table", "str", "write.table")
    to your NAMESPACE file.
    ```

*   checking compiled code ... NOTE
    ```
    File ‘ChemmineR/libs/ChemmineR.so’:
      Found ‘_ZSt4cerr’, possibly from ‘std::cerr’ (C++)
        Objects: ‘desc.o’, ‘formats.o’, ‘script.o’
      Found ‘_ZSt4cout’, possibly from ‘std::cout’ (C++)
        Object: ‘cluster.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# childsds

Version: 0.6.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gamlss.dist’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 20 marked UTF-8 strings
    ```

# ChIPQC

Version: 1.12.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘TxDb.Hsapiens.UCSC.hg38.knownGene’
      All declared Imports should be used.
    Unexported object imported by a ':::' call: ‘S4Vectors:::tabulate2’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘makeFripPlot’ ‘mergeMetadata’
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

*   checking data for non-ASCII characters ... NOTE
    ```
      Error in .requirePackage(package) : 
        unable to find required package 'ChIPQC'
      Calls: <Anonymous> ... getClass -> getClassDef -> .classEnv -> .requirePackage
      Execution halted
    ```

# ChIPseeker

Version: 1.12.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    DOSE v3.2.0  For help: https://guangchuangyu.github.io/DOSE
    
    If you use DOSE in published research, please cite:
    Guangchuang Yu, Li-Gen Wang, Guang-Rong Yan, Qing-Yu He. DOSE: an R/Bioconductor package for Disease Ontology Semantic and Enrichment analysis. Bioinformatics 2015, 31(4):608-609
    
    clusterProfiler v3.4.4  For help: https://guangchuangyu.github.io/clusterProfiler
    
    If you use clusterProfiler in published research, please cite:
    Guangchuang Yu., Li-Gen Wang, Yanyan Han, Qing-Yu He. clusterProfiler: an R package for comparing biological themes among gene clusters. OMICS: A Journal of Integrative Biology. 2012, 16(5):284-287.
    ReactomePA v1.20.2  For help: https://guangchuangyu.github.io/ReactomePA
    
    If you use ReactomePA in published research, please cite:
    Guangchuang Yu, Qing-Yu He. ReactomePA: an R/Bioconductor package for reactome pathway analysis and visualization. Molecular BioSystems 2016, 12(2):477-479
    ChIPseeker v1.12.1  For help: https://guangchuangyu.github.io/ChIPseeker
    
    If you use ChIPseeker in published research, please cite:
    Guangchuang Yu, Li-Gen Wang, Qing-Yu He. ChIPseeker: an R/Bioconductor package for ChIP peak annotation, comparison and visualization. Bioinformatics 2015, 31(14):2382-2383
    'select()' returned 1:many mapping between keys and columns
    Error: processing vignette 'ChIPseeker.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# ChocoLattes

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘knitr’ ‘tools’
      All declared Imports should be used.
    ```

# choroplethr

Version: 3.6.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: calculate_percent_change
    > ### Title: Calculate the percentage change between two choroplethr
    > ###   dataframes.
    > ### Aliases: calculate_percent_change
    > 
    > ### ** Examples
    > 
    > # load median age estimates from 2010 and 2015
    > data(df_state_age_2010)
    > data(df_state_age_2015)
    > 
    > df_age_diff = calculate_percent_change(df_state_age_2010, df_state_age_2015)
    > state_choropleth(df_age_diff, 
    +     title      = "Percent Change in Median Age, 2010-2015", 
    +     legend     = "Percent Change", 
    +     num_colors = 0)
    Theme element panel.border missing
    Error in if (theme$panel.ontop) { : argument is of length zero
    Calls: state_choropleth ... ggplotGrob -> ggplot_gtable -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

# choroplethrAdmin1

Version: 1.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 18.0Mb
      sub-directories of 1Mb or more:
        data  17.9Mb
    ```

# chromstaR

Version: 1.2.0

## In both

*   checking whether package ‘chromstaR’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/chromstaR/new/chromstaR.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘chromstaR’ ...
** libs
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c utility.cpp -o utility.o
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c init.cpp -o init.o
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c densities.cpp -o densities.o
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c R_interface.cpp -o R_interface.o
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c scalehmm.cpp -o scalehmm.o
g++ -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o chromstaR.so R_interface.o densities.o init.o scalehmm.o utility.o -fopenmp -L/usr/lib/R/lib -lR
installing to /home/muelleki/git/R/ggplot2/revdep/checks/chromstaR/new/chromstaR.Rcheck/chromstaR/libs
** R
** data
** inst
** preparing package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/home/muelleki/git/R/ggplot2/revdep/library/chromstaR/bamsignals/libs/bamsignals.so':
  libhts.so.0: cannot open shared object file: No such file or directory
ERROR: lazy loading failed for package ‘chromstaR’
* removing ‘/home/muelleki/git/R/ggplot2/revdep/checks/chromstaR/new/chromstaR.Rcheck/chromstaR’

```
### CRAN

```
* installing *source* package ‘chromstaR’ ...
** libs
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c densities.cpp -o densities.o
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c utility.cpp -o utility.o
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c init.cpp -o init.o
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c R_interface.cpp -o R_interface.o
g++  -I/usr/share/R/include -DNDEBUG     -fopenmp -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -c scalehmm.cpp -o scalehmm.o
g++ -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o chromstaR.so R_interface.o densities.o init.o scalehmm.o utility.o -fopenmp -L/usr/lib/R/lib -lR
installing to /home/muelleki/git/R/ggplot2/revdep/checks/chromstaR/old/chromstaR.Rcheck/chromstaR/libs
** R
** data
** inst
** preparing package for lazy loading
Error in dyn.load(file, DLLpath = DLLpath, ...) : 
  unable to load shared object '/home/muelleki/git/R/ggplot2/revdep/library/chromstaR/bamsignals/libs/bamsignals.so':
  libhts.so.0: cannot open shared object file: No such file or directory
ERROR: lazy loading failed for package ‘chromstaR’
* removing ‘/home/muelleki/git/R/ggplot2/revdep/checks/chromstaR/old/chromstaR.Rcheck/chromstaR’

```
# chron

Version: 2.3-51

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘zoo’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘zoo’
    ```

# CINNA

Version: 1.1.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      ...
    
    Attaching package: 'igraph'
    
    The following objects are masked from 'package:stats':
    
        decompose, spectrum
    
    The following object is masked from 'package:base':
    
        union
    
    Loading required namespace: circlize
    Failed with error:  'there is no package called 'circlize''
    Quitting from lines 290-293 (CINNA.Rmd) 
    Error: processing vignette 'CINNA.Rmd' failed with diagnostics:
    The 'circlize' package is not installed on your system.
    You first need to run:
     install.packages('circlize') 
    before you can use the circlize_dendrogram function.
    Execution halted
    ```

# CINOEDV

Version: 2.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    NormalizationEffect: no visible global function definition for ‘layout’
    NormalizationEffect: no visible global function definition for ‘plot’
    NormalizationEffect: no visible global function definition for ‘lines’
    PSOSearch: no visible global function definition for ‘combn’
    PSOSearch: no visible global function definition for ‘runif’
    PlotTopEffects: no visible global function definition for ‘dev.new’
    PlotTopEffects: no visible global function definition for ‘par’
    PlotTopEffects: no visible global function definition for ‘layout’
    PlotTopEffects: no visible global function definition for ‘barplot’
    PlotTopEffects: no visible global function definition for ‘rainbow’
    PlotTopEffects: no visible global function definition for ‘title’
    Undefined global functions or variables:
      barplot combn dev.new graphics.off hist install.packages layout lines
      par plot rainbow runif title
    Consider adding
      importFrom("grDevices", "dev.new", "graphics.off", "rainbow")
      importFrom("graphics", "barplot", "hist", "layout", "lines", "par",
                 "plot", "title")
      importFrom("stats", "runif")
      importFrom("utils", "combn", "install.packages")
    to your NAMESPACE file.
    ```

# classifierplots

Version: 1.3.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# classify

Version: 1.3

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking R code for possible problems ... NOTE
    ```
    classify.bug: no visible global function definition for
      ‘txtProgressBar’
    classify.bug: no visible global function definition for
      ‘setTxtProgressBar’
    plot.scores: no visible binding for global variable ‘quantile’
    plot.scores: no visible global function definition for ‘runif’
    plot.scores: no visible binding for global variable ‘median’
    scores.gpcm.bug: no visible global function definition for
      ‘txtProgressBar’
    scores.gpcm.bug: no visible global function definition for
      ‘setTxtProgressBar’
    across.reps,classification: no visible global function definition for
      ‘sd’
    Undefined global functions or variables:
      median quantile runif sd setTxtProgressBar txtProgressBar
    Consider adding
      importFrom("stats", "median", "quantile", "runif", "sd")
      importFrom("utils", "setTxtProgressBar", "txtProgressBar")
    to your NAMESPACE file.
    ```

# ClassifyR

Version: 1.10.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Loading required package: BiocParallel
    Loading required package: affy
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    Error: processing vignette 'ClassifyR.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘combn’
    samplesMetricMap,list: no visible binding for global variable ‘Class’
    samplesMetricMap,list: no visible binding for global variable ‘name’
    samplesMetricMap,list: no visible binding for global variable ‘type’
    samplesMetricMap,list: no visible binding for global variable ‘Metric’
    selectionPlot,list: no visible binding for global variable ‘Freq’
    selectionPlot,list: no visible global function definition for
      ‘reformulate’
    subtractFromLocation,ExpressionSet: no visible binding for global
      variable ‘median’
    Undefined global functions or variables:
      ..density.. Class Freq Metric bartlett.test capture.output classes
      combn density dlda dnorm expr featureValues ks.test largerClass mad
      median model.matrix na.omit name predict reformulate sd splinefun
      type var
    Consider adding
      importFrom("stats", "bartlett.test", "density", "dnorm", "ks.test",
                 "mad", "median", "model.matrix", "na.omit", "predict",
                 "reformulate", "sd", "splinefun", "var")
      importFrom("utils", "capture.output", "combn")
    to your NAMESPACE file.
    ```

# classyfire

Version: 0.1-2

## In both

*   checking R code for possible problems ... NOTE
    ```
    .boxRadial: no visible global function definition for ‘predict’
    .radialSVM: no visible global function definition for ‘predict’
    cfPredict: no visible global function definition for ‘predict’
    getPerm5Num: no visible global function definition for ‘median’
    getPerm5Num: no visible global function definition for ‘quantile’
    ggClassPred: no visible global function definition for ‘ftable’
    ggEnsHist: no visible global function definition for ‘sd’
    ggPermHist: no visible global function definition for ‘sd’
    Undefined global functions or variables:
      ftable median predict quantile sd
    Consider adding
      importFrom("stats", "ftable", "median", "predict", "quantile", "sd")
    to your NAMESPACE file.
    ```

# cleanEHR

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stats’
      All declared Imports should be used.
    ```

# clusterfly

Version: 0.4

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘clusterfly-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cfly_show
    > ### Title: Show in ggobi. Opens an instance ggobi for this dataset (if not
    > ###   already open), and colours the points according the cluster
    > ###   assignment.
    > ### Aliases: cfly_show
    > ### Keywords: dynamic
    > 
    > ### ** Examples
    > 
    > o <- olive_example()
    > cfly_show(o, 1)
    
    (R:42504): Gtk-WARNING **: cannot open display: 
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘ggplot2’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    hierfly: no visible global function definition for ‘dist’
    olive_example: no visible binding for global variable ‘kmeans’
    print.hierfly: no visible global function definition for ‘str’
    rescaler : <anonymous>: no visible global function definition for ‘sd’
    rescaler : <anonymous>: no visible global function definition for
      ‘median’
    rescaler : <anonymous>: no visible global function definition for ‘mad’
    som_iterate: no visible global function definition for ‘quantile’
    som_iterate: no visible global function definition for ‘unit.distances’
    summary.somiter : interesting: no visible global function definition
      for ‘sd’
    Undefined global functions or variables:
      aes_string cutree dist facet_grid facet_wrap geom_density geom_line
      geom_tile ggplot hclust kmeans mad median qf quantile rnorm
      scale_x_discrete scale_y_discrete sd step str theme unit.distances
      var
    Consider adding
      importFrom("stats", "cutree", "dist", "hclust", "kmeans", "mad",
                 "median", "qf", "quantile", "rnorm", "sd", "step", "var")
      importFrom("utils", "str")
    to your NAMESPACE file.
    ```

# ClusterR

Version: 1.0.8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        libs   6.4Mb
    ```

# ClusterSignificance

Version: 1.4.1

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# clustRcompaR

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# CNEr

Version: 1.12.1

## In both

*   checking compiled code ... WARNING
    ```
    File ‘CNEr/libs/CNEr.so’:
      Found ‘abort’, possibly from ‘abort’ (C)
        Object: ‘ucsc/errabort.o’
      Found ‘exit’, possibly from ‘exit’ (C)
        Objects: ‘ucsc/errabort.o’, ‘ucsc/pipeline.o’
      Found ‘puts’, possibly from ‘printf’ (C), ‘puts’ (C)
        Object: ‘ucsc/pipeline.o’
      Found ‘rand’, possibly from ‘rand’ (C)
        Object: ‘ucsc/obscure.o’
      Found ‘stderr’, possibly from ‘stderr’ (C)
        Objects: ‘ucsc/axt.o’, ‘ucsc/errabort.o’, ‘ucsc/obscure.o’,
          ‘ucsc/verbose.o’, ‘ucsc/os.o’
      Found ‘stdout’, possibly from ‘stdout’ (C)
        Objects: ‘ucsc/common.o’, ‘ucsc/errabort.o’, ‘ucsc/verbose.o’,
          ‘ucsc/os.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
    Loading required package: GenomicRanges
    Loading required package: Biostrings
    Loading required package: XVector
    
    Attaching package: 'Biostrings'
    
    The following object is masked from 'package:CNEr':
    
        N50
    
    The following object is masked from 'package:base':
    
        strsplit
    
    Loading required package: rtracklayer
    Loading required package: grid
    Error: processing vignette 'CNEr.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 18.3Mb
      sub-directories of 1Mb or more:
        extdata  15.9Mb
        libs      1.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘BiocGenerics:::replaceSlots’ ‘S4Vectors:::make_zero_col_DataFrame’
      See the note in ?`:::` about the use of this operator.
    ```

# CNPBayes

Version: 1.6.1

## In both

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'labelSwitching':
    labelSwitching
      Code: function(object, ...)
      Docs: function(object, merge = TRUE)
      Argument names in code not in docs:
        ...
      Argument names in docs not in code:
        merge
      Mismatches in argument names:
        Position: 2 Code: ... Docs: merge
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.7Mb
      sub-directories of 1Mb or more:
        libs  10.6Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘GenomicRanges’
    A package should be listed in only one of these fields.
    ```

*   checking R code for possible problems ... NOTE
    ```
    consensusRegion: no visible global function definition for
      ‘elementLengths’
    Undefined global functions or variables:
      elementLengths
    ```

# CNVPanelizer

Version: 1.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'CNVPanelizer.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `biblatex.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.66 \usepackage
                    [T1]{fontenc}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# CNVrd2

Version: 1.14.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DNAcopy’ ‘Rsamtools’
      All declared Imports should be used.
    Packages in Depends field not imported from:
      ‘VariantAnnotation’ ‘ggplot2’ ‘gridExtra’ ‘methods’ ‘parallel’
      ‘rjags’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      definition for ‘as’
    segmentSamplesUsingPopInformation,CNVrd2: no visible global function
      definition for ‘fitted’
    segmentSamplesUsingPopInformation,CNVrd2: no visible global function
      definition for ‘lm’
    Undefined global functions or variables:
      GRanges Hsapiens Quantile ScanBamParam TabixFile abline aes
      alphabetFrequency as axis chisq.test coda.samples coord_cartesian
      countBam dnorm fisher.test fitted geno geom_line geom_rect geom_text
      ggplot grid.arrange hist jags.model kmeans label lines lm mclapply
      objectCNVrd2 p.adjust par plot readDNAStringSet readVcf rect text
      theme unmasked write.table x x1 x2 xmax xmin y ylab ymax ymin
    Consider adding
      importFrom("graphics", "abline", "axis", "hist", "lines", "par",
                 "plot", "rect", "text")
      importFrom("methods", "as")
      importFrom("stats", "chisq.test", "dnorm", "fisher.test", "fitted",
                 "kmeans", "lm", "p.adjust")
      importFrom("utils", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# cobalt

Version: 3.1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘cobalt-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bal.tab.weightit
    > ### Title: Balance Statistics for WeightIt Objects
    > ### Aliases: bal.tab.weightit
    > 
    > ### ** Examples
    > 
    > library(WeightIt); data("lalonde", package = "cobalt")
    Error in library(WeightIt) : there is no package called ‘WeightIt’
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Note: estimand and s.d.denom not specified; assuming ATT and treated.
    Note: estimand and s.d.denom not specified; assuming ATT and treated.
    Note: estimand and s.d.denom not specified; assuming ATT and treated.
    Note: estimand and s.d.denom not specified; assuming ATT and treated.
    Note: estimand and s.d.denom not specified; assuming ATT and treated.
    Note: estimand and s.d.denom not specified; assuming ATE and pooled.
    
    Attaching package: 'MatchIt'
    
    The following object is masked _by_ '.GlobalEnv':
    
        lalonde
    
    The following object is masked from 'package:cobalt':
    
        lalonde
    
    Quitting from lines 241-252 (cobalt_basic_use.Rmd) 
    Error: processing vignette 'cobalt_basic_use.Rmd' failed with diagnostics:
    there is no package called 'WeightIt'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘WeightIt’
    ```

# cocoreg

Version: 0.1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    mapping_rf: Error while checking: there is no package called
      ‘randomForest’
    ```

# codingMatrices

Version: 0.3.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 478-486 (codingMatrices.Rnw) 
    Error: processing vignette 'codingMatrices.Rnw' failed with diagnostics:
    xtable not installed.
    Execution halted
    ```

# coefplot

Version: 1.2.4

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘glmnet’ ‘maxLik’
    ```

# CoGAPS

Version: 2.10.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        libs   7.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    GWCoGAPS: no visible binding for global variable ‘i’
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘Samples’
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘value’
    patternMatcher : <anonymous>: no visible binding for global variable
      ‘BySet’
    Undefined global functions or variables:
      BySet Samples i value
    ```

# cogena

Version: 1.10.0

## In both

*   checking whether package ‘cogena’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘class::somgrid’ by ‘kohonen::somgrid’ when loading ‘cogena’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/cogena/new/cogena.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        doc       1.9Mb
        extdata   3.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    corInCluster,cogena: no visible global function definition for ‘cor’
    heatmapCluster,cogena: no visible global function definition for
      ‘topo.colors’
    heatmapCluster,cogena: no visible global function definition for
      ‘rainbow’
    heatmapCluster,cogena: no visible global function definition for ‘par’
    heatmapCluster,cogena: no visible global function definition for
      ‘legend’
    Undefined global functions or variables:
      abline as.dist axis cor data density dist hist image layout legend
      lines median mtext order.dendrogram p.adjust par phyper plot.new
      rainbow rect reorder sd text title topo.colors
    Consider adding
      importFrom("grDevices", "rainbow", "topo.colors")
      importFrom("graphics", "abline", "axis", "hist", "image", "layout",
                 "legend", "lines", "mtext", "par", "plot.new", "rect",
                 "text", "title")
      importFrom("stats", "as.dist", "cor", "density", "dist", "median",
                 "order.dendrogram", "p.adjust", "phyper", "reorder", "sd")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘clValid’
    ```

# CollapseLevels

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# coloc

Version: 2.3-1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    pcs.prepare: no visible global function definition for ‘prcomp’
    prepare.df: no visible global function definition for ‘cor’
    sdY.est: no visible global function definition for ‘lm’
    sdY.est: no visible global function definition for ‘coef’
    p.value,coloc: no visible global function definition for ‘pchisq’
    plot,colocPCs-missing: no visible binding for global variable ‘object’
    plot,colocPCs-missing: no visible global function definition for
      ‘abline’
    Undefined global functions or variables:
      abline arrows as.formula axis box coef coefficients col.summary combn
      cor glm impute.snps integrate lm object optimize pchisq pf points
      polygon prcomp qnorm segments single.snp.tests snp.imputation var
      vcov
    Consider adding
      importFrom("graphics", "abline", "arrows", "axis", "box", "points",
                 "polygon", "segments")
      importFrom("stats", "as.formula", "coef", "coefficients", "cor", "glm",
                 "integrate", "lm", "optimize", "pchisq", "pf", "prcomp",
                 "qnorm", "var", "vcov")
      importFrom("utils", "combn")
    to your NAMESPACE file.
    ```

# colorpatch

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘grid’ ‘gridExtra’
      All declared Imports should be used.
    ```

# coMET

Version: 1.8.0

## In both

*   checking whether package ‘coMET’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      WARNING: omitting pointless dependence on 'R' without a version requirement
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/coMET/new/coMET.Rcheck/00install.out’ for details.
    ```

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      Error in if (dep$op != ">=") next : argument is of length zero
      Calls: <Anonymous>
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘grid’ ‘biomaRt’ ‘Gviz’ ‘psych’ ‘ggbio’ ‘trackViewer’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 17.3Mb
      sub-directories of 1Mb or more:
        data      9.7Mb
        doc       2.6Mb
        extdata   4.5Mb
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
    eQTL_GTEx: no visible binding for global variable 'snp_pos'
    eQTL_GTEx: no visible binding for global variable 'snp_chrom'
    eQTL_GTEx: no visible binding for global variable 'gene_start'
    eQTL_GTEx: no visible binding for global variable 'gene_stop'
    eQTL_GTEx: no visible binding for global variable 'gene_chr'
    geneExpression_GTEx: no visible global function definition for 'as'
    metQTL: no visible binding for global variable 'chromosome_stop'
    metQTL: no visible binding for global variable 'chromosome_start'
    metQTL: no visible binding for global variable 'chromosome_name'
    psiQTL_GTEx: no visible binding for global variable 'pos_snp'
    psiQTL_GTEx: no visible binding for global variable 'chr_snp'
    psiQTL_GTEx: no visible binding for global variable 'pos_middle_exon'
    psiQTL_GTEx: no visible binding for global variable 'chr_exon'
    Undefined global functions or variables:
      as chr_exon chr_snp chromosome_name chromosome_start chromosome_stop
      gene_chr gene_start gene_stop pos_middle_exon pos_snp snp_chrom
      snp_pos
    Consider adding
      importFrom("methods", "as")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# CommT

Version: 0.1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    CommT.anova: no visible global function definition for ‘aov’
    CommT.legendpos: no visible global function definition for ‘quantile’
    CommT.plotcolors: no visible global function definition for ‘hcl’
    Undefined global functions or variables:
      aov hcl quantile
    Consider adding
      importFrom("grDevices", "hcl")
      importFrom("stats", "aov", "quantile")
    to your NAMESPACE file.
    ```

# compcodeR

Version: 1.12.0

## In both

*   checking whether package ‘compcodeR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/compcodeR/new/compcodeR.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking: ‘rpanel’ ‘DSS’
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘rpanel’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘packageVersion’
    voom.limma.createRmd: no visible global function definition for
      ‘packageVersion’
    voom.ttest.createRmd: no visible global function definition for
      ‘packageVersion’
    vst.limma.createRmd: no visible global function definition for
      ‘packageVersion’
    vst.ttest.createRmd: no visible global function definition for
      ‘packageVersion’
    show,compData: no visible global function definition for ‘head’
    Undefined global functions or variables:
      as.dist axis cor hclust head heat.colors legend lines loess median
      na.omit packageVersion par predict rexp rnbinom rpois runif sd title
    Consider adding
      importFrom("grDevices", "heat.colors")
      importFrom("graphics", "axis", "legend", "lines", "par", "title")
      importFrom("stats", "as.dist", "cor", "hclust", "loess", "median",
                 "na.omit", "predict", "rexp", "rnbinom", "rpois", "runif",
                 "sd")
      importFrom("utils", "head", "packageVersion")
    to your NAMESPACE file.
    ```

# CompGO

Version: 1.12.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘nodeRenderInfo<-’
    plotZScores: no visible global function definition for ‘complete.cases’
    plotZScores: no visible global function definition for ‘setNames’
    plotZScores: no visible global function definition for ‘cor’
    slidingJaccard: no visible binding for global variable ‘useRawPvals’
    zTransformDirectory: no visible global function definition for
      ‘read.table’
    zTransformDirectory: no visible global function definition for
      ‘complete.cases’
    zTransformDirectory: no visible binding for global variable ‘var’
    Undefined global functions or variables:
      Term Var1 Var2 Z complete.cases cor dev.off dist goDag hclust
      nodeRenderInfo<- nodes p.adjust par png pnorm read.table setNames
      text useRawPvals value var
    Consider adding
      importFrom("grDevices", "dev.off", "png")
      importFrom("graphics", "par", "text")
      importFrom("stats", "complete.cases", "cor", "dist", "hclust",
                 "p.adjust", "pnorm", "setNames", "var")
      importFrom("utils", "read.table")
    to your NAMESPACE file.
    ```

# CONFESS

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘EBImage’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# confidence

Version: 1.1-2

## In both

*   checking whether package ‘confidence’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/confidence/new/confidence.Rcheck/00install.out’ for details.
    ```

# congressbr

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# consensusSeekeR

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        is.unsorted, lapply, lengths, mapply, match, mget, order,
        paste, pmax, pmax.int, pmin, pmin.int, rank, rbind, rowMeans,
        rowSums, rownames, sapply, setdiff, sort, table, tapply,
        union, unique, unsplit, which, which.max, which.min
    
    Loading required package: IRanges
    Loading required package: S4Vectors
    Loading required package: stats4
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: GenomicRanges
    Loading required package: GenomeInfoDb
    Loading required package: BiocParallel
    Error: processing vignette 'consensusSeekeR.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# contiBAIT

Version: 1.4.0

## In both

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'switch' is deprecated.
    Deprecated functions may be defunct as soon as of the next release of
    R.
    See ?Deprecated.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        extdata   3.4Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      definition for ‘boxplot’
    plotWCdistribution,StrandFreqMatrix: no visible global function
      definition for ‘lines’
    plotWCdistribution,StrandFreqMatrix: no visible global function
      definition for ‘text’
    show,ContigOrdering: no visible global function definition for ‘head’
    show,ContigOrdering: no visible global function definition for ‘tail’
    show,LibraryGroupList: no visible global function definition for ‘head’
    show,LibraryGroupList: no visible global function definition for ‘tail’
    show,LinkageGroupList: no visible global function definition for ‘head’
    show,LinkageGroupList: no visible global function definition for ‘tail’
    show,StrandStateList: no visible global function definition for ‘head’
    show,StrandStateList: no visible global function definition for ‘tail’
    Undefined global functions or variables:
      boxplot head hist legend lines lm queryHits rbinom runif subjectHits
      tail text unstrand
    Consider adding
      importFrom("graphics", "boxplot", "hist", "legend", "lines", "text")
      importFrom("stats", "lm", "rbinom", "runif")
      importFrom("utils", "head", "tail")
    to your NAMESPACE file.
    ```

# coseq

Version: 1.0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ****************************************
      1. Failure: compareARI (@test-coseq.R#45) --------------------------------------
      compareARI(obj2) did not throw an error.
      
      
      ****************************************
      coseq analysis: Normal approach & logMedianRef transformation
      K = 2 to 4 
      ****************************************
      testthat results ================================================================
      OK: 30 SKIPPED: 0 FAILED: 1
      1. Failure: compareARI (@test-coseq.R#45) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# cosinor

Version: 1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    cosinor_analyzer : <anonymous>: no visible global function definition
      for ‘as.formula’
    ggplot.cosinor.lm: no visible global function definition for ‘predict’
    predict.cosinor.lm: no visible global function definition for ‘fitted’
    predict.cosinor.lm: no visible global function definition for ‘predict’
    simulate_cosinor: no visible global function definition for ‘runif’
    simulate_cosinor: no visible global function definition for ‘rbinom’
    simulate_cosinor: no visible global function definition for ‘rnorm’
    summary.cosinor.lm: no visible global function definition for ‘vcov’
    summary.cosinor.lm: no visible global function definition for ‘qnorm’
    summary.cosinor.lm: no visible global function definition for ‘pnorm’
    test_cosinor: no visible global function definition for ‘pchisq’
    test_cosinor: no visible global function definition for ‘pnorm’
    Undefined global functions or variables:
      as.formula fitted lm na.omit pchisq pnorm predict qnorm rbinom rnorm
      runif terms vcov vitamind
    Consider adding
      importFrom("stats", "as.formula", "fitted", "lm", "na.omit", "pchisq",
                 "pnorm", "predict", "qnorm", "rbinom", "rnorm", "runif",
                 "terms", "vcov")
    to your NAMESPACE file.
    ```

# cosinor2

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
           25        26        27        28        29        30 
    1.6250000 1.3815789 1.2894737 1.3333333 1.1710526 0.8088235 
    
    $cosinors[[17]]$fitted.values
     [1] 0.9751600 1.3201375 1.6020512 1.6086144 1.3348849 0.9869865 0.8268939
     [8] 0.9751600 1.3201375 1.6020512 1.6086144 1.3348849 0.9869865 0.8268939
    
    $cosinors[[17]]$residuals
              17           18           19           20           21           22 
     0.372062185 -0.099549245 -0.129828980  0.030274467  0.408536153  0.061624562 
              23           24           25           26           27           28 
    -0.368560591 -0.113317932  0.304862520 -0.220472255 -0.319140737 -0.001551566 
              29           30 
     0.184066083 -0.018070395 
    
    
    
    $plots
    $plots[[1]]
    Error: Column `y` must be a 1d atomic vector or a list
    Execution halted
    ```

# CosmoPhotoz

Version: 0.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    CosmoPhotoZestimator: no visible global function definition for
      ‘predict’
    computeCombPCA: no visible global function definition for ‘prcomp’
    computeDiagPhotoZ: no visible global function definition for ‘sd’
    computeDiagPhotoZ: no visible global function definition for ‘median’
    computeDiagPhotoZ: no visible global function definition for ‘mad’
    glmPredictPhotoZ: no visible global function definition for ‘predict’
    glmTrainPhotoZ: no visible global function definition for ‘glm’
    glmTrainPhotoZ: no visible global function definition for ‘Gamma’
    glmTrainPhotoZ: no visible global function definition for
      ‘inverse.gaussian’
    plotDiagPhotoZ: no visible global function definition for ‘median’
    plotDiagPhotoZ: no visible global function definition for ‘mad’
    Undefined global functions or variables:
      Gamma glm inverse.gaussian mad median prcomp predict sd
    Consider adding
      importFrom("stats", "Gamma", "glm", "inverse.gaussian", "mad",
                 "median", "prcomp", "predict", "sd")
    to your NAMESPACE file.
    ```

# CountClust

Version: 1.3.0

## In both

*   checking whether package ‘CountClust’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/CountClust/new/CountClust.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘CountClust’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** preparing package for lazy loading
Error : object ‘switch_axis_position’ is not exported by 'namespace:cowplot'
ERROR: lazy loading failed for package ‘CountClust’
* removing ‘/home/muelleki/git/R/ggplot2/revdep/checks/CountClust/new/CountClust.Rcheck/CountClust’

```
### CRAN

```
* installing *source* package ‘CountClust’ ...
** R
** data
*** moving datasets to lazyload DB
** inst
** preparing package for lazy loading
Error : object ‘switch_axis_position’ is not exported by 'namespace:cowplot'
ERROR: lazy loading failed for package ‘CountClust’
* removing ‘/home/muelleki/git/R/ggplot2/revdep/checks/CountClust/old/CountClust.Rcheck/CountClust’

```
# countyfloods

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    To use this package, you must install the hurricaneexposuredata
    package. To install that package, run
    `install.packages('hurricaneexposuredata',
    repos='https://geanders.github.io/drat/', type='source')`. See the
    `hurricaneexposure` vignette for more details.
    To use this function, you must have the `hurricaneexposuredata`
    package installed. See the `hurricaneexposure` package vignette
    for more details.
    Quitting from lines 311-313 (countyflood.Rmd) 
    Error: processing vignette 'countyflood.Rmd' failed with diagnostics:
    there is no package called 'hurricaneexposuredata'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# countytimezones

Version: 1.0.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    
    Attaching package: 'dplyr'
    
    The following object is masked from 'package:acs':
    
        combine
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Theme element panel.border missing
    Quitting from lines 111-123 (countytimezones.Rmd) 
    Error: processing vignette 'countytimezones.Rmd' failed with diagnostics:
    argument is of length zero
    Execution halted
    ```

# countyweather

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# covafillr

Version: 0.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        libs   9.1Mb
    ```

# cowbell

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘misc3d’
      All declared Imports should be used.
    ```

# cowplot

Version: 0.9.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

# cpvSNP

Version: 1.8.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘GSEABase’ which was already attached by Depends.
      Please remove these calls from your code.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    createArrayData: no visible global function definition for
      ‘elementMetadata<-’
    geneToSNPList: no visible global function definition for ‘findOverlaps’
    glossiMarginal: no visible global function definition for ‘pgamma’
    plotPvals: no visible binding for global variable ‘df’
    plotPvals: no visible binding for global variable ‘pval’
    simulate_chisq: no visible global function definition for ‘rnorm’
    vegasMarginal: no visible global function definition for ‘rbinom’
    vegasMarginal: no visible global function definition for ‘qchisq’
    vegasPrep: no visible global function definition for ‘elementMetadata’
    show,VEGASResult: no visible global function definition for ‘var’
    Undefined global functions or variables:
      GRanges IRanges Rle col2rgb density df elementMetadata
      elementMetadata<- findOverlaps legend lines pgamma plot points
      polygon pval qchisq rbinom rgb rnorm var
    Consider adding
      importFrom("grDevices", "col2rgb", "rgb")
      importFrom("graphics", "legend", "lines", "plot", "points", "polygon")
      importFrom("stats", "density", "df", "pgamma", "qchisq", "rbinom",
                 "rnorm", "var")
    to your NAMESPACE file.
    ```

# crawl

Version: 2.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        libs   6.5Mb
    ```

# cricketr

Version: 0.0.14

## Newly broken

*   checking whether package ‘cricketr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::autolayer’ by ‘forecast::autolayer’ when loading ‘cricketr’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/cricketr/new/cricketr.Rcheck/00install.out’ for details.
    ```

# crossmeta

Version: 1.2.0

## In both

*   checking for code/documentation mismatches ... WARNING
    ```
    Data with usage in documentation object 'gs.names' but not in code:
      gs.names
    
    Data with usage in documentation object 'gslist' but not in code:
      gslist
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported objects:
      ‘crossmeta::gs.names’ ‘crossmeta::gslist’
    ```

*   checking R code for possible problems ... NOTE
    ```
    explore_paths : server: no visible binding for global variable ‘gslist’
    explore_paths : server: no visible binding for global variable
      ‘gs.names’
    Undefined global functions or variables:
      gs.names gslist
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘PADOG’, ‘GeneMeta’
    ```

# Crossover

Version: 0.1-17

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following object is masked from 'package:MASS':
    
        geyser
    
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'Crossover.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithmic.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.68 \usepackage
                    {algorithm}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# csabounds

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘qte’
      All declared Imports should be used.
    ```

# csp

Version: 0.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        data  10.9Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 59670 marked Latin-1 strings
      Note: found 16 marked UTF-8 strings
    ```

# ctsGE

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        html_document, md_document, pdf_document
    
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Using tags as id variables
    Using tags as id variables
    Using tags as id variables
    Using tags as id variables
    Error: processing vignette 'ctsGE.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# cummeRbund

Version: 2.18.0

## In both

*   checking examples ... ERROR
    ```
    ...
    Warning in rsqlite_fetch(res@ptr, n = n) :
      Don't need to call dbFetch() for statements, only for queries
    Warning in rsqlite_fetch(res@ptr, n = n) :
      Don't need to call dbFetch() for statements, only for queries
    Reading Run Info File /home/muelleki/git/R/ggplot2/revdep/checks/cummeRbund/new/cummeRbund.Rcheck/cummeRbund/extdata/run.info
    Writing runInfo Table
    Warning: RSQLite::dbGetPreparedQuery() is deprecated, please switch to DBI::dbGetQuery(params = bind.data).
    Warning: Factors converted to character
    Warning in rsqlite_fetch(res@ptr, n = n) :
      Don't need to call dbFetch() for statements, only for queries
    Reading Read Group Info  /home/muelleki/git/R/ggplot2/revdep/checks/cummeRbund/new/cummeRbund.Rcheck/cummeRbund/extdata/read_groups.info
    Warning: RSQLite::make.db.names() is deprecated, please switch to DBI::dbQuoteIdentifier().
    Writing replicates Table
    Warning: Factors converted to character
    Warning in rsqlite_fetch(res@ptr, n = n) :
      Don't need to call dbFetch() for statements, only for queries
    Reading /home/muelleki/git/R/ggplot2/revdep/checks/cummeRbund/new/cummeRbund.Rcheck/cummeRbund/extdata/genes.fpkm_tracking
    Checking samples table...
    Populating samples table...
    Error: Column name mismatch.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘BiocGenerics’ ‘RSQLite’ ‘ggplot2’ ‘reshape2’ ‘fastcluster’
      ‘rtracklayer’ ‘Gviz’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        doc       1.6Mb
        extdata   5.6Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    Packages listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘BiocGenerics’ ‘plyr’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      'NMFN' 'cluster' 'rjson' 'stringr'
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Packages in Depends field not imported from:
      'Gviz' 'RSQLite' 'fastcluster' 'ggplot2' 'reshape2' 'rtracklayer'
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      geom_hline geom_line geom_point geom_rect geom_rug geom_segment
      geom_smooth geom_text geom_tile geom_vline ggplot guides hasAxis<-
      hclust ids import labs log2_fold_change make.db.names makeTxDb
      mean_cl_boot melt nnmf obsnames order.dendrogram p.adjust p_value pam
      plot plotIdeogram plotTracks position_dodge prcomp quant_status
      ranges read.delim read.table rowInd sample_1 sample_2 sample_name
      scale_color_gradient scale_color_hue scale_color_manual
      scale_colour_manual scale_fill_continuous scale_fill_gradient
      scale_fill_gradient2 scale_fill_hue scale_x_continuous
      scale_x_discrete scale_x_log10 scale_y_continuous scale_y_discrete
      scale_y_log10 seqnames significant stat_density stat_smooth stat_sum
      stat_summary stdev str_split_fixed strand theme theme_bw toJSON
      tracking_id tracks unit v1 v2 value variable varnames write.table x
      xlab xlim y ylab
    Consider adding
      importFrom("graphics", "plot")
      importFrom("stats", "as.dendrogram", "as.dist", "as.formula",
                 "cmdscale", "dist", "hclust", "order.dendrogram",
                 "p.adjust", "prcomp")
      importFrom("utils", "read.delim", "read.table", "write.table")
    to your NAMESPACE file.
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    Loading required package: grid
    
    Attaching package: 'cummeRbund'
    
    The following object is masked from 'package:GenomicRanges':
    
        promoters
    
    The following object is masked from 'package:IRanges':
    
        promoters
    
    The following object is masked from 'package:BiocGenerics':
    
        conditions
    
    
    Error: processing vignette 'cummeRbund-example-workflow.Rnw' failed with diagnostics:
     chunk 4 (label = model_fit_1) 
    Error in rsqlite_send_query(conn@ptr, statement) : near ")": syntax error
    Execution halted
    ```

# curatedBreastData

Version: 2.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 283.1Mb
      sub-directories of 1Mb or more:
        data  282.7Mb
    ```

# curatedMetagenomicData

Version: 1.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      The data you have provided does not have
    any singletons. This is highly suspicious. Results of richness
    estimates (for example) are probably unreliable, or wrong, if you have already
    trimmed low-abundance taxa from the data.
    
    We recommended that you find the un-trimmed data and retry.
    Warning in graphics:::plotHclust(n1, merge, height, order(x$order), hang,  :
      "method" is not a graphical parameter
    Warning in graphics:::plotHclust(n1, merge, height, order(x$order), hang,  :
      "method" is not a graphical parameter
    Warning in axis(2, at = pretty(range(height)), ...) :
      "method" is not a graphical parameter
    Warning in title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...) :
      "method" is not a graphical parameter
    Warning in replayPlot(x) : "method" is not a graphical parameter
    Warning in replayPlot(x) : "method" is not a graphical parameter
    Warning in replayPlot(x) : "method" is not a graphical parameter
    Warning in replayPlot(x) : "method" is not a graphical parameter
    Error: processing vignette 'curatedMetagenomicData.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘dplyr’ ‘phyloseq’ ‘Biobase’ ‘ExperimentHub’ ‘AnnotationHub’
      ‘magrittr’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        help   7.9Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘BiocInstaller’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘BiocInstaller’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ExpressionSet2MRexperiment: no visible global function definition for
      ‘AnnotatedDataFrame’
    ExpressionSet2MRexperiment: no visible global function definition for
      ‘phenoData’
    curatedMetagenomicData : <anonymous>: no visible global function
      definition for ‘exprs<-’
    Undefined global functions or variables:
      AnnotatedDataFrame exprs<- phenoData
    ```

*   checking Rd files ... NOTE
    ```
    prepare_Rd: HMP_2012.Rd:540-542: Dropping empty section \seealso
    prepare_Rd: KarlssonFH_2013.Rd:90-92: Dropping empty section \seealso
    prepare_Rd: LeChatelierE_2013.Rd:86-88: Dropping empty section \seealso
    prepare_Rd: LomanNJ_2013_Hi.Rd:82-84: Dropping empty section \seealso
    prepare_Rd: LomanNJ_2013_Mi.Rd:82-84: Dropping empty section \seealso
    prepare_Rd: NielsenHB_2014.Rd:94-96: Dropping empty section \seealso
    prepare_Rd: Obregon_TitoAJ_2015.Rd:94-96: Dropping empty section \seealso
    prepare_Rd: OhJ_2014.Rd:86-88: Dropping empty section \seealso
    prepare_Rd: QinJ_2012.Rd:106-108: Dropping empty section \seealso
    prepare_Rd: QinN_2014.Rd:94-96: Dropping empty section \seealso
    prepare_Rd: RampelliS_2015.Rd:90-92: Dropping empty section \seealso
    prepare_Rd: TettAJ_2016.Rd:184-186: Dropping empty section \seealso
    prepare_Rd: ZellerG_2014.Rd:94-96: Dropping empty section \seealso
    ```

# cutoffR

Version: 1.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    Cut: no visible global function definition for ‘cor’
    MissSimulation: no visible global function definition for ‘rbinom’
    cutoff: no visible global function definition for ‘cor’
    Undefined global functions or variables:
      cor rbinom
    Consider adding
      importFrom("stats", "cor", "rbinom")
    to your NAMESPACE file.
    ```

# CVE

Version: 1.2.0

## In both

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                                    old_size new_size compress
      WGCNAmelanoma_extension.RData    2.4Mb    2.1Mb       xz
      crcCase.RData                    1.1Mb    676Kb       xz
      melanomaCase.RData               654Kb    473Kb       xz
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    gdac.broadinstitute.org_COAD.Clinical_Pick_Tier1.Level_4.2016012800.0.0.tar.gz
    trying URL 'http://gdac.broadinstitute.org/runs/stddata__2016_01_28/data/COAD/20160128/gdac.broadinstitute.org_COAD.Clinical_Pick_Tier1.Level_4.2016012800.0.0.tar.gz'
    Content type 'application/x-gzip' length 80929 bytes (79 KB)
    ==================================================
    downloaded 79 KB
    
    gdac.broadinstitute.org_COAD.Clinical_Pick_Tier1.Level_4.2016012800.0.0
    gdac.broadinstitute.org_COAD.Mutation_Packager_Calls.Level_3.2016012800.0.0.tar.gz
    trying URL 'http://gdac.broadinstitute.org/runs/stddata__2016_01_28/data/COAD/20160128/gdac.broadinstitute.org_COAD.Mutation_Packager_Calls.Level_3.2016012800.0.0.tar.gz'
    Content type 'application/x-gzip' length 2675595 bytes (2.6 MB)
    ==================================================
    downloaded 2.6 MB
    
    Error: processing vignette 'CVE_tutorial.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘shiny’ ‘ConsensusClusterPlus’ ‘RColorBrewer’ ‘gplots’ ‘plyr’
      ‘ggplot2’ ‘jsonlite’ ‘ape’ ‘WGCNA’ ‘RTCGAToolbox’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘RTCGAToolbox’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

# cvxclustr

Version: 1.1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    create_clustering_problem: no visible global function definition for
      ‘rnorm’
    Undefined global functions or variables:
      rnorm
    Consider adding
      importFrom("stats", "rnorm")
    to your NAMESPACE file.
    ```

*   checking line endings in Makefiles ... NOTE
    ```
    Found the following Makefile(s) without a final LF:
      src/Makevars
    Some ‘make’ programs ignore lines not ending in LF.
    ```

# Cyclops

Version: 1.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 23.2Mb
      sub-directories of 1Mb or more:
        libs  22.5Mb
    ```

# cytofkit

Version: 1.8.4

## In both

*   checking whether package ‘cytofkit’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/cytofkit/new/cytofkit.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        doc       2.1Mb
        extdata   3.6Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    cytof_clusterPlot: no visible global function definition for
      ‘geom_text’
    cytof_progressionPlot: no visible global function definition for ‘aes’
    cytof_progressionPlot: no visible binding for global variable
      ‘Pseudotime’
    cytof_progressionPlot: no visible binding for global variable ‘cluster’
    cytofkitShinyAPP : <anonymous> : C_ScatterPlotInput: no visible global
      function definition for ‘scatterPlot’
    cytofkitShinyAPP : <anonymous>: no visible global function definition
      for ‘heatMap’
    cytofkitShinyAPP : <anonymous> : M_markerExpressionPlotInput: no
      visible global function definition for ‘scatterPlot’
    cytofkitShinyAPP : <anonymous> : M_stackDensityPlotInput: no visible
      global function definition for ‘stackDenistyPlot’
    cytofkitShinyAPP : <anonymous> : P_markerPlotInput: no visible global
      function definition for ‘cytof_expressionTrends’
    Undefined global functions or variables:
      Pseudotime aes cluster cytof_expressionTrends geom_text heatMap
      scatterPlot stackDenistyPlot
    ```

# dada2

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        doc    1.3Mb
        libs   5.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘ShortRead:::.set_omp_threads’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking foreign function calls ... NOTE
    ```
    Foreign function call to a different package:
      .Call(ShortRead:::.set_omp_threads, ...)
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotComplementarySubstitutions: no visible binding for global variable
      ‘Quality’
    plotErrors: no visible binding for global variable ‘Qual’
    plotErrors: no visible binding for global variable ‘Observed’
    plotErrors: no visible binding for global variable ‘Estimated’
    plotErrors: no visible binding for global variable ‘Input’
    plotErrors: no visible binding for global variable ‘Nominal’
    plotQualityProfile: no visible binding for global variable ‘Cycle’
    plotQualityProfile: no visible binding for global variable ‘Score’
    plotQualityProfile: no visible binding for global variable ‘Count’
    plotQualityProfile: no visible binding for global variable ‘Mean’
    plotQualityProfile: no visible binding for global variable ‘Q25’
    plotQualityProfile: no visible binding for global variable ‘Q50’
    plotQualityProfile: no visible binding for global variable ‘Q75’
    plotQualityProfile: no visible binding for global variable ‘minScore’
    plotQualityProfile: no visible binding for global variable ‘label’
    Undefined global functions or variables:
      Count Cycle Direction Estimated Forward Input Mean Nominal Observed
      Q25 Q50 Q75 Qual Quality Reverse Score Sub1 Sub2 SubGrp Substitution
      abundance accept allMismatch als1 als2 indel label minScore mismatch
      n0F n0R prefer seqF seqR
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# DaMiRseq

Version: 1.0.0

## Newly fixed

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Loading required package: ggplot2
      > 
      > test_check("DaMiRseq")
      10 Predictors have been selected for classification 
      3 Predictors have been selected for classification 
      100 Highly correlated features have been discarded for classification. 
       0 Features remained. 
      100 Highly correlated features have been discarded for classification. 
       0 Features remained. 
      Error in array(STATS, dims[perm]) : 'dims' cannot be of length 0
      Calls: test_check ... DaMiR.FSelect -> bve_pls -> VIP -> apply -> sweep -> aperm -> array
      In addition: There were 50 or more warnings (use warnings() to see the first 50)
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:Biobase':
    
        anyMissing, rowMedians
    
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following object is masked from 'package:base':
    
        apply
    
    Loading required package: ggplot2
    Quitting from lines 334-338 (DaMiRseq.Rnw) 
    Error: processing vignette 'DaMiRseq.Rnw' failed with diagnostics:
    'dims' cannot be of length 0
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        data   5.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    DaMiR.Allplot: warning in pheatmap(sampleDistMatrix,
      clustering_distance_rows = mydist, clustering_distance_cols = mydist,
      col = colors, breaks = seque, annotation_col = df): partial argument
      match of 'col' to 'color'
    DaMiR.Clustplot: warning in pheatmap(count_data,
      clustering_distance_rows = d_r, clustering_distance_cols = d_c, scale
      = "row", col = colors, annotation_col = df): partial argument match
      of 'col' to 'color'
    DaMiR.Allplot: no visible binding for global variable ‘X1’
    DaMiR.Allplot: no visible binding for global variable ‘X2’
    DaMiR.EnsembleLearning: no visible binding for global variable
      ‘Predictors’
    DaMiR.EnsembleLearning: no visible binding for global variable
      ‘Accuracy’
    DaMiR.MDSplot: no visible binding for global variable ‘X1’
    DaMiR.MDSplot: no visible binding for global variable ‘X2’
    Undefined global functions or variables:
      Accuracy Predictors X1 X2
    ```

# DAPAR

Version: 1.8.7

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      4. Failure: corrMatrixD (@test_descriptiveStatistics.R#174) --------------------
      t$data[[1]] inherits from `factor` not `data.frame`.
      
      
      Procedure of Benjamini-Hochberg is used. pi0 is fixed to 1.
      testthat results ================================================================
      OK: 176 SKIPPED: 0 FAILED: 4
      1. Failure: wrapper.corrMatrixD (@test_descriptiveStatistics.R#161) 
      2. Failure: wrapper.corrMatrixD (@test_descriptiveStatistics.R#162) 
      3. Failure: corrMatrixD (@test_descriptiveStatistics.R#173) 
      4. Failure: corrMatrixD (@test_descriptiveStatistics.R#174) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented data sets:
      ‘testWithoutNA’ ‘test’ ‘UPSpep25’
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘Prostar’
    ```

# darksky

Version: 1.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      + }
      1. Error: the API call works (@test-darksky.R#6) -------------------------------
      Please set env var DARKSKY_API_KEY to your Dark Sky API key
      1: get_current_forecast(43.2672, -70.8617) at testthat/test-darksky.R:6
      2: sprintf("https://api.darksky.net/forecast/%s/%s,%s", darksky_api_key(), latitude, 
             longitude)
      3: darksky_api_key()
      4: stop("Please set env var DARKSKY_API_KEY to your Dark Sky API key", call. = FALSE)
      
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 1
      1. Error: the API call works (@test-darksky.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dartR

Version: 0.93

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘Demerelate’ ‘misc3d’ ‘plotly’ ‘quadprog’ ‘rgl’
      All declared Imports should be used.
    ```

# data.table

Version: 1.10.4-3

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/main.R’ failed.
    Last 13 lines of output:
      > x = names(print(ggplot(DT) + geom_hex(aes(b, f)) + facet_wrap(~grp)))[c(1,      3)] 
      [1] "data"   "scales"
      > y = c("data", "plot") 
      [1] "data" "plot"
      1 string mismatch
      Don't know how to automatically pick scale for object of type ITime. Defaulting to continuous.
      Don't know how to automatically pick scale for object of type ITime. Defaulting to continuous.
      Tests 1441-1444 not run. If required install the 'fr_FR.utf8' locale.
      
      
      Running test id 1696     
      Error in eval(exprs[i], envir) : 
        2 errors out of 5939 (lastID=1751, endian==little, sizeof(long double)==16, sizeof(pointer)==8) in inst/tests/tests.Rraw on Tue Nov 21 12:27:40 2017. Search tests.Rraw for test numbers: 167, 167.2.
      Calls: test.data.table -> sys.source -> eval -> eval
      Execution halted
    ```

# DataExplorer

Version: 0.4.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(DataExplorer)
      > 
      > test_check("DataExplorer")
      1. Failure: test if quiet is working for GenerateReport (@test-generate-report.r#8) 
      GenerateReport(...) produced warnings.
      
      
      testthat results ================================================================
      OK: 58 SKIPPED: 0 FAILED: 1
      1. Failure: test if quiet is working for GenerateReport (@test-generate-report.r#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# datarobot

Version: 2.7.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘curl’
      All declared Imports should be used.
    ```

# DChIPRep

Version: 1.6.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        anyMissing, rowMedians
    
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following object is masked from 'package:base':
    
        apply
    
    
    gene-wise dispersion estimates
    mean-dispersion relationship
    final dispersion estimates
    `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
    Error: processing vignette 'DChIPRepVignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

# dcmr

Version: 1.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘combn’
    SampleParameterEstimates: no visible global function definition for
      ‘rnorm’
    plot,attribute.class-missing: no visible binding for global variable
      ‘attr.number’
    plot,attribute.class-missing: no visible binding for global variable
      ‘mean.attr’
    plot,attribute.profile.class-missing: no visible binding for global
      variable ‘attr.profile.number’
    plot,attribute.profile.class-missing: no visible binding for global
      variable ‘mean.attr.profile’
    summary,attribute.class: no visible binding for global variable ‘value’
    summary,attribute.profile.class: no visible binding for global variable
      ‘max.class’
    Undefined global functions or variables:
      attr.number attr.profile.number combn max.class mean.attr
      mean.attr.profile rmultinom rnorm runif value
    Consider adding
      importFrom("stats", "rmultinom", "rnorm", "runif")
      importFrom("utils", "combn")
    to your NAMESPACE file.
    ```

# debrowser

Version: 1.4.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'DEBrowser.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.9Mb
      sub-directories of 1Mb or more:
        doc       6.3Mb
        extdata   2.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    scatterZoom: no visible binding for global variable ‘size.hover’
    scatterZoom: no visible binding for global variable ‘key’
    startDEBrowser: no visible binding for global variable
      ‘.startdebrowser.called’
    volcanoPlot: no visible global function definition for ‘:=’
    volcanoPlot: no visible binding for global variable ‘size’
    volcanoPlot: no visible binding for global variable ‘size.hover’
    volcanoPlot: no visible binding for global variable ‘fillOpacity’
    volcanoPlot: no visible binding for global variable ‘fillOpacity.hover’
    volcanoPlot: no visible binding for global variable ‘fill.brush’
    volcanoPlot: no visible binding for global variable ‘opacity’
    volcanoPlot: no visible binding for global variable ‘key’
    volcanoZoom: no visible global function definition for ‘:=’
    volcanoZoom: no visible binding for global variable ‘size’
    volcanoZoom: no visible binding for global variable ‘size.hover’
    volcanoZoom: no visible binding for global variable ‘key’
    Undefined global functions or variables:
      .startdebrowser.called := NUL align baseline biocLite conds debrowser
      demodata fill fill.brush fillOpacity fillOpacity.hover fit fontSize
      get_user_info googleAuth googleAuthUI initStore key opacity samples
      searched size size.hover stroke updateStore with_shiny
    ```

# DeconRNASeq

Version: 1.18.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        data   6.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘grid’ which was already attached by Depends.
      Please remove these calls from your code.
    Packages in Depends field not imported from:
      ‘ggplot2’ ‘grid’ ‘pcaMethods’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    condplot: no visible global function definition for ‘plot’
    condplot: no visible global function definition for ‘rainbow’
    condplot: no visible global function definition for ‘lines’
    condplot: no visible global function definition for ‘axis’
    condplot: no visible global function definition for ‘title’
    decon.bootstrap: no visible global function definition for ‘t.test’
    multiplot: no visible global function definition for ‘grid.newpage’
    multiplot: no visible global function definition for ‘pushViewport’
    multiplot: no visible global function definition for ‘viewport’
    multiplot: no visible global function definition for ‘grid.layout’
    multiplot : vplayout: no visible global function definition for
      ‘viewport’
    Undefined global functions or variables:
      R2cum aes axis geom_abline geom_point ggplot grid.layout grid.newpage
      labs lines pca plot prep pushViewport rainbow t.test title viewport
      xlab ylab
    Consider adding
      importFrom("grDevices", "rainbow")
      importFrom("graphics", "axis", "lines", "plot", "title")
      importFrom("stats", "t.test")
    to your NAMESPACE file.
    ```

# Deducer

Version: 0.7-9

## In both

*   checking whether the namespace can be unloaded cleanly ... WARNING
    ```
    ...
    7f302349c000-7f30234c2000 r-xp 00000000 08:01 16865                      /lib/x86_64-linux-gnu/ld-2.23.so
    7f30234c2000-7f30234c4000 r--s 00001000 08:01 1025768                    /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/dnsns.jar
    7f30234c4000-7f30234ca000 r--s 0003a000 08:01 1025763                    /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/sunjce_provider.jar
    7f30234ca000-7f30234cb000 r--s 00010000 08:01 1025760                    /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/zipfs.jar
    7f30234cb000-7f30234cc000 r--s 0000a000 08:01 1025767                    /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/jaccess.jar
    7f30234cc000-7f30234ce000 r--s 00008000 08:01 1025764                    /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/ext/sunec.jar
    7f30234ce000-7f30236ae000 rw-p 00000000 00:00 0 
    7f30236ae000-7f30236b6000 rw-s 00000000 08:01 1292559                    /tmp/hsperfdata_muelleki/43455
    7f30236b6000-7f30236b7000 rw-p 00000000 00:00 0 
    7f30236b7000-7f30236b8000 r--p 00000000 00:00 0 
    7f30236b8000-7f30236bf000 r--s 00000000 08:01 27320                      /usr/lib/x86_64-linux-gnu/gconv/gconv-modules.cache
    7f30236bf000-7f30236c1000 rw-p 00000000 00:00 0 
    7f30236c1000-7f30236c2000 r--p 00025000 08:01 16865                      /lib/x86_64-linux-gnu/ld-2.23.so
    7f30236c2000-7f30236c3000 rw-p 00026000 08:01 16865                      /lib/x86_64-linux-gnu/ld-2.23.so
    7f30236c3000-7f30236c4000 rw-p 00000000 00:00 0 
    7fff29fbc000-7fff29fbf000 ---p 00000000 00:00 0 
    7fff29fbf000-7fff2a7bc000 rw-p 00000000 00:00 0                          [stack]
    7fff2a7e2000-7fff2a7e4000 r--p 00000000 00:00 0                          [vvar]
    7fff2a7e4000-7fff2a7e6000 r-xp 00000000 00:00 0                          [vdso]
    ffffffffff600000-ffffffffff601000 r-xp 00000000 00:00 0                  [vsyscall]
    Aborted (core dumped)
    ```

# DeepBlueR

Version: 1.2.10

## In both

*   checking examples ... ERROR
    ```
    ...
    + 
    +     experiment_names = deepblue_extract_names(experiments_list)
    +     histones_datasets[[epigenetic_marks[[i]]]] = experiment_names
    + }
    Called method: deepblue_list_experiments
    Reported status was: okay
    Called method: deepblue_list_experiments
    Reported status was: okay
    Called method: deepblue_list_experiments
    Reported status was: okay
    > 
    > deepblue_enrich_region_overlap(
    +   query_id=filtered_query_id,
    +   background_query=rg_10kb_tilling,
    +   datasets=histones_datasets,
    +   genome="grch38")
    Called method: deepblue_enrich_region_overlap
    Reported status was: error
    Error in deepblue_enrich_region_overlap(query_id = filtered_query_id,  : 
      Command enrich_region_overlap does not exists.
    Execution halted
    ```

# DEGreport

Version: 1.12.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘plotCounts’
    degPlotWide: no visible binding for global variable ‘gene’
    degPlotWide: no visible binding for global variable ‘count’
    degPlotWide: no visible binding for global variable ‘treatment’
    degResults: no visible global function definition for ‘assay’
    degResults: no visible global function definition for ‘rlog’
    degResults: no visible global function definition for ‘results’
    degResults: no visible global function definition for ‘colData’
    degResults: no visible global function definition for ‘rowMax’
    degVolcano: no visible binding for global variable ‘logFC’
    degVolcano: no visible binding for global variable ‘V1’
    degVolcano: no visible binding for global variable ‘V2’
    degVolcano: no visible binding for global variable ‘adj.P.Val’
    degVolcano: no visible binding for global variable ‘x’
    degVolcano: no visible binding for global variable ‘y’
    degVolcano: no visible binding for global variable ‘name’
    Undefined global functions or variables:
      MulticoreParam V1 V2 adj.P.Val assay bplapply coda.samples colData
      comp count enrichGO gene group jags.model keys label log2FoldChange
      logFC name one plotCounts results rlog rowMax simplify treatment two
      value variable x y
    ```

# DeLorean

Version: 1.2.5

## In both

*   checking S3 generic/method consistency ... WARNING
    ```
    filter:
      function(x, filter, method, sides, circular, init)
    filter.cells:
      function(dl, .filter, number, cells)
    
    filter:
      function(x, filter, method, sides, circular, init)
    filter.genes:
      function(dl, .filter, number, genes)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

# deltaGseg

Version: 1.16.0

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘ggplot2’ ‘changepoint’ ‘wavethresh’ ‘tseries’ ‘pvclust’ ‘fBasics’
      ‘grid’ ‘reshape’ ‘scales’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    diagnosticPlots,SegSeriesTrajectories: no visible binding for global
      variable ‘residuals’
    diagnosticPlots,SegSeriesTrajectories: no visible global function
      definition for ‘acf’
    diagnosticPlots,SegSeriesTrajectories: no visible global function
      definition for ‘qnorm’
    diagnosticPlots,SegSeriesTrajectories: no visible binding for global
      variable ‘lag’
    Undefined global functions or variables:
      abline acf as.dist cor cutree dist hclust identify lag layout lines
      locator median mtext na.omit par points qnorm quantile read.table
      residuals setTxtProgressBar text txtProgressBar
    Consider adding
      importFrom("graphics", "abline", "identify", "layout", "lines",
                 "locator", "mtext", "par", "points", "text")
      importFrom("stats", "acf", "as.dist", "cor", "cutree", "dist",
                 "hclust", "lag", "median", "na.omit", "qnorm", "quantile",
                 "residuals")
      importFrom("utils", "read.table", "setTxtProgressBar",
                 "txtProgressBar")
    to your NAMESPACE file.
    ```

*   checking installed files from ‘inst/doc’ ... NOTE
    ```
    The following files should probably not be installed:
      ‘diagplots.png’, ‘simclust.png’, ‘simtraj.png’, ‘simtrajtr.png’,
      ‘simtrajtr2.png’, ‘traj1.png’, ‘traj1break.png’, ‘traj1ss0.png’,
      ‘traj1ss1.png’, ‘traj1tr.png’
    
    Consider the use of a .Rinstignore file: see ‘Writing R Extensions’,
    or move the vignette sources from ‘inst/doc’ to ‘vignettes’.
    ```

# demi

Version: 1.1.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    demi.wilcox.test.fast: no visible global function definition for
      ‘wilcox.test’
    demiequal: no visible global function definition for ‘wilcox.test’
    wprob: no visible global function definition for ‘combn’
    diffexp,DEMIDiff: no visible global function definition for ‘median’
    diffexp,DEMIDiff: no visible global function definition for ‘p.adjust’
    loadAnnotation,DEMIExperiment-environment: no visible global function
      definition for ‘data’
    loadBlat,DEMIExperiment-environment: no visible global function
      definition for ‘data’
    loadCytoband,DEMIExperiment-environment: no visible global function
      definition for ‘data’
    loadPathway,DEMIExperiment-environment: no visible global function
      definition for ‘data’
    Undefined global functions or variables:
      combn data dhyper median p.adjust t.test wilcox.test write.table
    Consider adding
      importFrom("stats", "dhyper", "median", "p.adjust", "t.test",
                 "wilcox.test")
      importFrom("utils", "combn", "data", "write.table")
    to your NAMESPACE file.
    ```

# dendextend

Version: 1.6.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘ggdendro’ ‘labeltodendro’ ‘dendroextras’ ‘Hmisc’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘WGCNA’, ‘dendroextras’, ‘moduleColor’, ‘distory’, ‘phangorn’, ‘ggdendro’, ‘zoo’
    ```

# DendroSync

Version: 0.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: sync.plot
    > ### Title: Plot within- and between-group synchrony
    > ### Aliases: sync.plot
    > 
    > ### ** Examples
    > 
    > ## Plot homoscedastic narrow evaluation (mNE) and unstructured model (mUN)
    >  # synchronies for conifersIP data:
    >  data(conifersIP)
    >      
    >  ##Fit the homoscedastic set of varcov models (mBE, mNE, mCS, mUN)
    >  # using geographic grouping criteria (ie. Region)
    >  ModHm <- dendro.varcov(TRW ~ Code, varTime = "Year", varGroup = "Region", 
    +                         data = conifersIP, homoscedastic = TRUE)
    [1] "Please wait. I am fitting the models now :)"
    >  
    >  sync.plot(sync(ModHm, modname = "mNE"))
    Error: Columns `ymin`, `ymax` must be 1d atomic vectors or lists
    Execution halted
    ```

# DepthProc

Version: 2.0.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

# derfinder

Version: 1.10.6

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in citation("BiocStyle") :
      no date field in DESCRIPTION file of package 'BiocStyle'
    Quitting from lines 53-115 (derfinder-quickstart.Rmd) 
    Error: processing vignette 'derfinder-quickstart.Rmd' failed with diagnostics:
    package 'knitrBootstrap' not found
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘derfinderPlot’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘GenomeInfoDb:::.guessSpeciesStyle’
      ‘GenomeInfoDb:::.supportedSeqnameMappings’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘.smootherFstats’
    ```

# derfinderPlot

Version: 1.10.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning in citation("biovizBase") :
      no date field in DESCRIPTION file of package 'biovizBase'
    Warning in citation("TxDb.Hsapiens.UCSC.hg19.knownGene") :
      no date field in DESCRIPTION file of package 'TxDb.Hsapiens.UCSC.hg19.knownGene'
    Writing 24 Bibtex entries ... OK
    Results written to file 'derfinderPlotRef.bib'
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: Transformation introduced infinite values in continuous y-axis
    Error: processing vignette 'derfinderPlot.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘RefManageR’
    ```

# DESeq2

Version: 1.16.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("DESeq2")
      1. Error: tximport works (@test_tximport.R#12) ---------------------------------
      importing inferential replicates for Salmon or Sailfish requires package `rjson`.
        to skip this step, set dropInfReps=TRUE
      1: tximport(files, type = "salmon", tx2gene = tx2gene) at testthat/test_tximport.R:12
      2: infRepImporter(dirname(files[i]))
      3: readInfRepFish(x, type)
      4: stop("importing inferential replicates for Salmon or Sailfish requires package `rjson`.\n  to skip this step, set dropInfReps=TRUE")
      
      testthat results ================================================================
      OK: 203 SKIPPED: 0 FAILED: 1
      1. Error: tximport works (@test_tximport.R#12) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    reading in files with read_tsv
    1 Quitting from lines 256-257 (DESeq2.Rmd) 
    Error: processing vignette 'DESeq2.Rmd' failed with diagnostics:
    importing inferential replicates for Salmon or Sailfish requires package `rjson`.
      to skip this step, set dropInfReps=TRUE
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc    2.4Mb
        libs   2.6Mb
    ```

# destiny

Version: 2.4.5

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘rgl’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc    3.1Mb
        libs   2.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scatterplot3d’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rgl’
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'plot.DPT':
      ‘plot.DPT’
    
    S3 methods shown with full name in documentation object 'plot.DiffusionMap':
      ‘plot.DiffusionMap’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘viridis’
    'library' or 'require' calls not declared from:
      ‘IRdisplay’ ‘IRkernel’ ‘base64enc’ ‘repr’ ‘xlsx’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'DPT.ipynbmeta' failed with diagnostics:
    Either IPython 3+ or Jupyter has to be installed, but neither could be called.
    Execution halted
    ```

# detzrcr

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# dfexplore

Version: 0.2.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    simulate_dataframe : generate_dumb_answer: no visible global function
      definition for ‘rbinom’
    simulate_dataframe : generate_dumb_answer: no visible global function
      definition for ‘rnorm’
    simulate_dataframe : generate_initial: no visible global function
      definition for ‘rbinom’
    simulate_dataframe: no visible binding for global variable ‘randu’
    simulate_dataframe: no visible global function definition for ‘rbinom’
    simulate_dataframe: no visible global function definition for ‘rnorm’
    simulate_dataframe: no visible global function definition for ‘rpois’
    simulate_dataframe : add_NAs: no visible global function definition for
      ‘rbinom’
    Undefined global functions or variables:
      randu rbinom rnorm rpois
    Consider adding
      importFrom("datasets", "randu")
      importFrom("stats", "rbinom", "rnorm", "rpois")
    to your NAMESPACE file.
    ```

# dfpk

Version: 3.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 43.2Mb
      sub-directories of 1Mb or more:
        libs  42.8Mb
    ```

# dggridR

Version: 2.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.5Mb
      sub-directories of 1Mb or more:
        doc    1.9Mb
        libs  25.2Mb
    ```

# diagis

Version: 0.1.3

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > #optimal case
    > set.seed(42)
    > s_opt <- sqrt(2)
    > x_opt <- rnorm(1000, sd = s_opt)
    > w_opt <- p(x_opt) / q(x_opt, s_opt) 
    > weighted_mean(x_opt, w_opt)
    [1] -0.01223051
    > weighted_var(x_opt, w_opt)
    [1] 0.9954861
    > s_inf <- 0.25
    > x_inf <- rnorm(1000, sd = s_inf)
    > w_inf <- p(x_inf) / q(x_inf, s_inf)
    > weighted_mean(x_inf, w_inf) #!!
    [1] 0.1281881
    > weighted_var(x_inf, w_inf) #!!
    [1] 0.2531165
    > # diagnostic plots
    > weight_plot(w_inf)
    Error: Column `y` must be a 1d atomic vector or a list
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 73-74 (diagis.Rmd) 
    Error: processing vignette 'diagis.Rmd' failed with diagnostics:
    Column `y` must be a 1d atomic vector or a list
    Execution halted
    ```

# dielectric

Version: 0.2.3

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘methods’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    L2eV: no visible binding for global variable ‘constants’
    L2w: no visible binding for global variable ‘constants’
    dielectric: no visible global function definition for ‘new’
    dielectric2plot: no visible global function definition for ‘reshape’
    eV2L: no visible binding for global variable ‘constants’
    t2eV: no visible binding for global variable ‘constants’
    Undefined global functions or variables:
      constants new reshape
    Consider adding
      importFrom("methods", "new")
      importFrom("stats", "reshape")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# DiffBind

Version: 2.4.8

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘rgl’ ‘XLConnect’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        libs   3.3Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    pv.DBAplotVolcano: no visible binding for global variable ‘Fold’
    pv.DBAplotVolcano: no visible binding for global variable ‘Legend’
    Undefined global functions or variables:
      Fold Legend
    ```

# diffeR

Version: 0.0-4

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘diffeR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: MAD
    > ### Title: Mean Absolute Deviation (MAD)
    > ### Aliases: MAD
    > ### Keywords: spatial
    > 
    > ### ** Examples
    > 
    > old.par <- par(no.readonly = TRUE)
    > grid1 <- raster(system.file("external/GRID1_INT.rst", package="diffeR"))
    > grid2 <- raster(system.file("external/GRID2_INT.rst", package="diffeR"))
    > strata <- raster(system.file("external/strata_int.rst", package="diffeR"))
    > MAD(grid1, grid2, strata, eval="original")
    Error in calc(strata, fun) : unused argument (fun)
    Calls: MAD
    Execution halted
    ```

*   checking whether package ‘diffeR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘raster::calc’ by ‘ggplot2::calc’ when loading ‘diffeR’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/diffeR/new/diffeR.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    MAD: possible error in calc(strata, fun): unused argument (fun)
    MAD: possible error in calc(strataM2, fun1): unused argument (fun1)
    MAD: possible error in calc(tmp1, fun2): unused argument (fun2)
    MAD: possible error in calc(strataM, sum): unused argument (sum)
    ```

# diffloop

Version: 1.4.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

# directlabels

Version: 2017.03.31

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The error most likely occurred in:
    
    > ### Name: normal.l2.cluster
    > ### Title: Clustering of some normal data in 2d with the l2 clusterpath
    > ### Aliases: normal.l2.cluster
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > data(normal.l2.cluster)
    > if(require(ggplot2)){
    +   p <- ggplot(normal.l2.cluster$path,aes(x,y))+
    +     geom_path(aes(group=row),colour="grey")+
    +     geom_point(aes(size=lambda),colour="grey")+
    +     geom_point(aes(colour=class),data=normal.l2.cluster$pts)+
    +     coord_equal()
    +   print(direct.label(p))
    + }
    Loading required package: ggplot2
    Error: Column `colour` must have a unique name
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/ggplot.R’ failed.
    Last 13 lines of output:
      +       
      +       stopifnot(result$hide %in% L$hide)
      +       stopifnot(length(result$hide) == length(L$hide))
      +       stopifnot(L$colour == result$colour)
      +       ## label it and check for different legends.
      +       dl <- direct.label(L$plot)
      +       after <- legends2hide(dl)
      +       stopifnot(is.null(after))
      +     }
      +   }
      + }
      Loading required package: ggplot2
      Error in loadNamespace(name) : there is no package called 'dplyr'
      Calls: geom_point ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
      Execution halted
    ```

# diveRsity

Version: 1.9.90

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        libs   5.9Mb
    ```

# DLMtool

Version: 5.0

## In both

*   checking whether package ‘DLMtool’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/DLMtool/new/DLMtool.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        libs   4.6Mb
    ```

# dMod

Version: 0.4

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rPython’
    ```

# DMRScan

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   4.9Mb
    ```

# doBy

Version: 4.5-15

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gdata’
    ```

# dodgr

Version: 0.0.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        doc    3.4Mb
        libs   6.1Mb
    ```

# DOSE

Version: 3.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    DOSE v3.2.0  For help: https://guangchuangyu.github.io/DOSE
    
    If you use DOSE in published research, please cite:
    Guangchuang Yu, Li-Gen Wang, Guang-Rong Yan, Qing-Yu He. DOSE: an R/Bioconductor package for Disease Ontology Semantic and Enrichment analysis. Bioinformatics 2015, 31(4):608-609
    
    Error: processing vignette 'DOSE.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   3.0Mb
        doc    2.5Mb
    ```

# dotwhisker

Version: 0.3.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘arm’
    ```

# dplyr

Version: 0.7.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 33.0Mb
      sub-directories of 1Mb or more:
        libs  31.0Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# dr4pl

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘drc’ ‘testthat’
      All declared Imports should be used.
    ```

# DRIMSeq

Version: 1.4.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    3 FBgn0259735   0.9383577  2 6.255157e-01 7.744480e-01
    4 FBgn0032785   3.4718633  2 1.762359e-01 3.143951e-01
    5 FBgn0040297   6.0769622  4 1.934739e-01 3.143951e-01
    6 FBgn0032979   1.7139224  1 1.904773e-01 3.143951e-01
    > 
    > ## Plot feature proportions for a top DTU gene
    > res <- results(d)
    > res <- res[order(res$pvalue, decreasing = FALSE), ]
    > 
    > top_gene_id <- res$gene_id[1]
    > 
    > plotProportions(d, gene_id = top_gene_id, group_variable = "group")
    > 
    > plotProportions(d, gene_id = top_gene_id, group_variable = "group", 
    +   plot_type = "lineplot")
    > 
    > plotProportions(d, gene_id = top_gene_id, group_variable = "group", 
    +   plot_type = "ribbonplot")
    Coordinate system already present. Adding new coordinate system, which will replace the existing one.
    Error: Columns `ymin`, `ymax` must be 1d atomic vectors or lists
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 369-379 (DRIMSeq.Rnw) 
    Error: processing vignette 'DRIMSeq.Rnw' failed with diagnostics:
    Columns `ymin`, `ymax` must be 1d atomic vectors or lists
    Execution halted
    ```

# dSimer

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    finding first neighbours of the disease-related genes..
    calculating shortest path of each gene pair.. this may take a while..
    calculating transformed distance of each gene pair.. this may take a while..
    calculating similarity of each disease pair..
    done..
    calculating similarity of each disease pair..
    done..
    calculating similarity of each disease pair..
    done..
    calculating similarity of each disease pair..
    done..
    filtering disease-gene associations..
    calculating separation distance between diseases.. this may need a lot of time.. be patient..
    done..
    filtering disease-gene associations..
    calculating separation distance between diseases.. this may need a lot of time.. be patient..
    done..
    Using ID as id variables
    Error: processing vignette 'dSimer.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
        libs   3.5Mb
    ```

# dsm

Version: 2.2.15

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Error in .requirePackage(package) : unable to find required package 'sp'
      Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
      Execution halted
    ```

# dtwclust

Version: 4.1.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 1920 SKIPPED: 0 FAILED: 11
      1. Error: Pairwise proxy distances give the same result as references 
      2. Error: Included (valid) distances can accept multivariate series. 
      3. Error: Pairwise proxy distances give the same result as references 
      4. Error: Distance matrices calculated with families give the same results as references. 
      5. Error: Centroids calculated with families give the same results as references. 
      6. Error: A custom distance in dtwclust give the same results as references. 
      7. Error: CVIs give the same results as references. 
      8. Error: Fuzzy clustering gives the same results as references. 
      9. Error: Hierarchical clustering gives the same results as references. 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 10.1Mb
      sub-directories of 1Mb or more:
        doc    1.9Mb
        libs   7.2Mb
    ```

# dtwSat

Version: 0.2.3

## Newly broken

*   checking whether package ‘dtwSat’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::calc’ by ‘raster::calc’ when loading ‘dtwSat’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/dtwSat/new/dtwSat.Rcheck/00install.out’ for details.
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'caret'
    
    The following object is masked from 'package:survival':
    
        cluster
    
    This is BibTeX, Version 0.99d (TeX Live 2015/Debian)
    The top-level auxiliary file: applying_twdtw.aux
    The style file: jss.bst
    Illegal, another \bibstyle command---line 155 of file applying_twdtw.aux
     : \bibstyle
     :          {jss}
    I'm skipping whatever remains of this command
    Database file #1: references.bib.bib
    Warning--entry type for "Dutrieux:2014" isn't style-file defined
    --line 584 of file references.bib.bib
    (There was 1 error message)
    Error: processing vignette 'applying_twdtw.Rmd' failed with diagnostics:
    Failed to build the bibliography via bibtex
    Execution halted
    ```

# DVHmetrics

Version: 0.3.6

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Coordinate system already present. Adding new coordinate system, which will replace the existing one.
    Coordinate system already present. Adding new coordinate system, which will replace the existing one.
    Coordinate system already present. Adding new coordinate system, which will replace the existing one.
    Warning in getEQD2.default(D = D$dvh[, "dose"], fd = fd, ab = ab) :
      'D' must be > 0
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'DVHmetrics.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `apacite.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.93 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# dynr

Version: 0.1.11-8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        models   4.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rdpack’
      All declared Imports should be used.
    ```

# earlywarnings

Version: 1.0.59

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    surrogates_RShiny: no visible global function definition for ‘par’
    surrogates_RShiny: no visible global function definition for ‘hist’
    surrogates_RShiny: no visible global function definition for ‘abline’
    surrogates_RShiny: no visible global function definition for ‘points’
    surrogates_RShiny: no visible global function definition for ‘title’
    surrogates_ews: no visible global function definition for ‘dev.new’
    surrogates_ews: no visible global function definition for ‘par’
    surrogates_ews: no visible global function definition for ‘hist’
    surrogates_ews: no visible global function definition for ‘abline’
    surrogates_ews: no visible global function definition for ‘points’
    surrogates_ews: no visible global function definition for ‘mtext’
    Undefined global functions or variables:
      abline aes contour dev.new geom_tile grid hist labs layout legend
      lines mtext par plot points rainbow scale_fill_gradient stat_contour
      text title topo.colors xlab ylab
    Consider adding
      importFrom("grDevices", "dev.new", "rainbow", "topo.colors")
      importFrom("graphics", "abline", "contour", "grid", "hist", "layout",
                 "legend", "lines", "mtext", "par", "plot", "points", "text",
                 "title")
    to your NAMESPACE file.
    ```

# EasyHTMLReport

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘reshape2’ ‘scales’ ‘xtable’
      All declared Imports should be used.
    Packages in Depends field not imported from:
      ‘base64enc’ ‘knitr’ ‘markdown’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    .file_attachment: no visible global function definition for
      ‘base64encode’
    .plot_attachment: no visible global function definition for ‘dev.off’
    .update_list: no visible global function definition for ‘modifyList’
    easyHtmlReport: no visible global function definition for ‘knit’
    easyHtmlReport: no visible global function definition for
      ‘markdownToHTML’
    mime_part.data.frame: no visible global function definition for
      ‘write.table’
    mime_part.ggplot: no visible binding for global variable ‘pdf’
    mime_part.matrix: no visible global function definition for
      ‘write.table’
    mime_part.trellis: no visible binding for global variable ‘pdf’
    simpleHtmlReport : <anonymous>: no visible global function definition
      for ‘write.table’
    Undefined global functions or variables:
      base64encode dev.off knit markdownToHTML modifyList pdf write.table
    Consider adding
      importFrom("grDevices", "dev.off", "pdf")
      importFrom("utils", "modifyList", "write.table")
    to your NAMESPACE file.
    ```

# easyml

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘corrplot’ ‘scorer’
      All declared Imports should be used.
    ```

# ecoengine

Version: 1.11.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# EcoGenetics

Version: 1.2.1-2

## Newly broken

*   checking whether package ‘EcoGenetics’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::calc’ by ‘raster::calc’ when loading ‘EcoGenetics’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/EcoGenetics/new/EcoGenetics.Rcheck/00install.out’ for details.
    ```

# edge

Version: 2.8.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    fitFDist: no visible global function definition for ‘median’
    fitFDist: no visible global function definition for ‘lm.fit’
    fitFDist: no visible global function definition for ‘predict’
    fit_wmodels: no visible global function definition for ‘model.matrix’
    fit_wmodels: no visible global function definition for ‘lm.wfit’
    null: no visible global function definition for ‘model.matrix’
    apply_sva,deSet: no visible global function definition for ‘as.formula’
    apply_sva,deSet: no visible global function definition for ‘terms’
    fit_models,deSet: no visible global function definition for
      ‘model.matrix’
    fullModel<-,deSet: no visible global function definition for
      ‘model.matrix’
    lrt,deSet-deFit: no visible global function definition for ‘pf’
    nullModel<-,deSet: no visible global function definition for
      ‘model.matrix’
    Undefined global functions or variables:
      as.formula lm.fit lm.wfit median model.matrix pf predict terms
    Consider adding
      importFrom("stats", "as.formula", "lm.fit", "lm.wfit", "median",
                 "model.matrix", "pf", "predict", "terms")
    to your NAMESPACE file.
    ```

# eechidna

Version: 1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        data   4.9Mb
        doc    1.2Mb
    ```

# eegc

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 28-29 (eegc.Rnw) 
    Error: processing vignette 'eegc.Rnw' failed with diagnostics:
    there is no package called 'BiocStyle'
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        data  10.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    grnPlot: no visible global function definition for ‘title’
    grnPlot: no visible global function definition for ‘legend’
    markerScatter: no visible global function definition for
      ‘colorRampPalette’
    markerScatter: no visible global function definition for ‘points’
    markerScatter: no visible global function definition for ‘lm’
    markerScatter: no visible global function definition for ‘abline’
    markerScatter: no visible global function definition for ‘text’
    markerScatter: no visible global function definition for ‘legend’
    Undefined global functions or variables:
      abline adjustcolor axis colorRampPalette control density dev.copy2pdf
      legend lines lm model.matrix p.adjust par phyper points quantile
      results text title treat
    Consider adding
      importFrom("grDevices", "adjustcolor", "colorRampPalette",
                 "dev.copy2pdf")
      importFrom("graphics", "abline", "axis", "legend", "lines", "par",
                 "points", "text", "title")
      importFrom("stats", "density", "lm", "model.matrix", "p.adjust",
                 "phyper", "quantile")
    to your NAMESPACE file.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘BiocStyle’
    ```

# eeptools

Version: 1.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: ggplot2
    Quitting from lines 172-181 (intro.Rmd) 
    Error: processing vignette 'intro.Rmd' failed with diagnostics:
    Package `maps` required for `map_data`.
    Please install and try again.
    Execution halted
    ```

# EFDR

Version: 0.1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    .gdf : find_loss: no visible global function definition for ‘rnorm’
    .p.values : <anonymous>: no visible global function definition for
      ‘pnorm’
    .relist.dwt: no visible global function definition for ‘relist’
    .relist.dwt: no visible global function definition for ‘as’
    .std.wav.coeff : <anonymous>: no visible global function definition for
      ‘mad’
    regrid: no visible global function definition for ‘predict’
    regrid: no visible global function definition for ‘var’
    regrid: no visible global function definition for ‘medpolish’
    Undefined global functions or variables:
      as mad medpolish pnorm predict relist rnorm var
    Consider adding
      importFrom("methods", "as")
      importFrom("stats", "mad", "medpolish", "pnorm", "predict", "rnorm",
                 "var")
      importFrom("utils", "relist")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# EGSEA

Version: 1.4.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    buildGeneSetDBIdx: no visible binding for global variable ‘GOTERM’
    buildMSigDBIdx: no visible binding for global variable ‘msigdb’
    generateSummaryPlots: no visible binding for global variable ‘x.data’
    generateSummaryPlots: no visible binding for global variable ‘y.data’
    generateSummaryPlots: no visible binding for global variable ‘gsSize’
    generateSummaryPlots: no visible binding for global variable ‘id’
    generateSummaryPlots: no visible binding for global variable ‘sig’
    loadKeggData: no visible binding for global variable ‘kegg.pathways’
    plotBars,EGSEAResults: no visible global function definition for
      ‘abline’
    Undefined global functions or variables:
      GOTERM abline gsSize id kegg.pathways msigdb sig x.data y.data
    Consider adding
      importFrom("graphics", "abline")
    to your NAMESPACE file.
    ```

# ELMER

Version: 1.6.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘ELMER.data’ ‘Homo.sapiens’
      ‘IlluminaHumanMethylation450kanno.ilmn12.hg19’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    motif.enrichment.plot: no visible binding for global variable ‘OR’
    promoterMeth: no visible global function definition for ‘write.csv’
    scatter: no visible binding for global variable ‘value’
    schematic: no visible global function definition for ‘pdf’
    schematic: no visible global function definition for ‘dev.off’
    txs: no visible binding for global variable ‘Homo.sapiens’
    show,MEE.data: no visible global function definition for ‘str’
    show,Pair: no visible global function definition for ‘str’
    summary,MEE.data: no visible global function definition for ‘str’
    summary,Pair: no visible global function definition for ‘str’
    Undefined global functions or variables:
      Homo.sapiens IlluminaHumanMethylation450kanno.ilmn12.hg19 OR coef
      data dev.off label lm lowerOR motif p.adjust pdf pvalue read.csv
      read.delim read.table str t.test upperOR value wilcox.test write.csv
      write.table
    Consider adding
      importFrom("grDevices", "dev.off", "pdf")
      importFrom("stats", "coef", "lm", "p.adjust", "t.test", "wilcox.test")
      importFrom("utils", "data", "read.csv", "read.delim", "read.table",
                 "str", "write.csv", "write.table")
    to your NAMESPACE file.
    ```

# emdi

Version: 1.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘FNN’ ‘ggmap’ ‘simFrame’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2098 marked UTF-8 strings
    ```

# EMDomics

Version: 2.6.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Authors@R field gives more than one person with maintainer role:
      Sadhika Malladi <contact@sadhikamalladi.com> [aut, cre]
      Daniel Schmolze <emd@schmolze.com> [aut, cre]
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    calculate_cvm : <anonymous>: no visible global function definition for
      ‘median’
    calculate_cvm_gene: no visible global function definition for ‘combn’
    calculate_emd: no visible global function definition for ‘combn’
    calculate_emd : <anonymous>: no visible global function definition for
      ‘median’
    calculate_emd_gene: no visible global function definition for ‘combn’
    calculate_ks: no visible global function definition for ‘combn’
    calculate_ks : <anonymous>: no visible global function definition for
      ‘p.adjust’
    calculate_ks : <anonymous>: no visible global function definition for
      ‘median’
    calculate_ks_gene: no visible global function definition for ‘combn’
    calculate_ks_gene: no visible global function definition for ‘ks.test’
    Undefined global functions or variables:
      combn hist ks.test median p.adjust
    Consider adding
      importFrom("graphics", "hist")
      importFrom("stats", "ks.test", "median", "p.adjust")
      importFrom("utils", "combn")
    to your NAMESPACE file.
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# emil

Version: 2.2.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# emmeans

Version: 0.9.1

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘CARBayes’ ‘coxme’ ‘gee’ ‘geepack’ ‘glmmADMB’ ‘MCMCglmm’ ‘MCMCpack’
      ‘pscl’ ‘rstanarm’
    ```

# emojifont

Version: 0.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        emoji_fonts   6.5Mb
    ```

# enrichwith

Version: 0.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘betareg’
    ```

# EnvStats

Version: 2.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        doc    3.3Mb
        help   3.4Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

# episensr

Version: 0.9.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘llogistic’ ‘logitnorm’
      All declared Imports should be used.
    ```

# erccdashboard

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    dynRangePlot: no visible binding for global variable ‘value’
    dynRangePlot: no visible binding for global variable ‘Rep’
    Undefined global functions or variables:
      Rep value
    ```

# erma

Version: 0.8.0

## In both

*   checking whether the namespace can be loaded with stated dependencies ... WARNING
    ```
    Error: .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 199.7Mb
      sub-directories of 1Mb or more:
        bed_tabix  161.3Mb
        data        37.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    Error: .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    ```

# esetVis

Version: 1.2.0

## In both

*   checking examples ... ERROR
    ```
    ...
    
        expand.grid
    
    Loading required package: org.Hs.eg.db
    
    
    > probeIDs <- featureNames(ALL)
    > geneInfo <- select(hgu95av2.db, probeIDs,"ENTREZID", "PROBEID")
    'select()' returned 1:many mapping between keys and columns
    > 
    > # get pathway annotation for the genes contained in the ALL dataset (can take a few minutes)
    > geneSets <- getGeneSetsForPlot(entrezIdentifiers = geneInfo$ENTREZID, species = "Human", 
    + 	geneSetSource = 'GOBP',
    + 	useDescription = FALSE, trace = TRUE)
    Loading required package: GO.db
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called ‘GO.db’
    Error in as.list(GOBPANCESTOR) : object 'GOBPANCESTOR' not found
    Calls: getGeneSetsForPlot ... system.time -> lapply -> FUN -> getGeneSets -> as.list
    Timing stopped at: 23.12 0.44 32.89
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    
    'select()' returned 1:many mapping between keys and columns
    Loading required package: MASS
    
    Attaching package: 'MASS'
    
    The following object is masked from 'package:AnnotationDbi':
    
        select
    
    Warning: Removed 5 rows containing missing values (geom_point).
    Warning: Removed 5 rows containing missing values (geom_point).
    Loading required package: GO.db
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called 'GO.db'
    Quitting from lines 382-389 (esetVis-vignette.Rmd) 
    Timing stopped at: 21.73 0.296 32.16
    Error: processing vignette 'esetVis-vignette.Rmd' failed with diagnostics:
    object 'GOBPANCESTOR' not found
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   5.6Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    esetPlotWrapper: no visible global function definition for ‘:=’
    esetPlotWrapper: no visible binding for global variable ‘fill’
    getMethodsInputObjectEsetVis: no visible binding for global variable
      ‘rowData’
    getMethodsInputObjectEsetVis: no visible binding for global variable
      ‘colData’
    getMethodsInputObjectEsetVis: no visible binding for global variable
      ‘assay’
    getMethodsInputObjectEsetVis : <anonymous>: no visible global function
      definition for ‘colData’
    getMethodsInputObjectEsetVis : <anonymous>: no visible global function
      definition for ‘rowData’
    Undefined global functions or variables:
      := assay colData fill rowData
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘a4Base’
    ```

# ESGtoolkit

Version: 0.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    esgplotts: no visible global function definition for ‘deltat’
    esgplotts: no visible global function definition for ‘xlab’
    esgplotts: no visible global function definition for ‘ylab’
    esgplotts: no visible global function definition for ‘theme’
    simdiff: no visible global function definition for ‘ts’
    simshocks: no visible global function definition for ‘ts’
    simshocks: no visible global function definition for ‘qnorm’
    simshocks : <anonymous>: no visible global function definition for ‘ts’
    Undefined global functions or variables:
      abline aes colorRampPalette coord_flip cor cor.test deltat
      element_blank end geom_density geom_point is.ts lines matplot par
      plot points polygon pt qnorm qt quantile scale_color_manual
      scale_fill_manual sd start t.test theme time ts tsp window xlab ylab
    Consider adding
      importFrom("grDevices", "colorRampPalette")
      importFrom("graphics", "abline", "lines", "matplot", "par", "plot",
                 "points", "polygon")
      importFrom("stats", "cor", "cor.test", "deltat", "end", "is.ts", "pt",
                 "qnorm", "qt", "quantile", "sd", "start", "t.test", "time",
                 "ts", "tsp", "window")
    to your NAMESPACE file.
    ```

# esmisc

Version: 0.0.3

## Newly broken

*   checking whether package ‘esmisc’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::calc’ by ‘raster::calc’ when loading ‘esmisc’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/esmisc/new/esmisc.Rcheck/00install.out’ for details.
    ```

# etm

Version: 0.6-2

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plot.clos.etm: no visible global function definition for ‘plot’
    plot.clos.etm: no visible global function definition for ‘axis’
    plot.clos.etm: no visible global function definition for ‘box’
    plot.clos.etm: no visible global function definition for ‘lines’
    plot.clos.etm: no visible global function definition for ‘close.screen’
    plot.etm: no visible global function definition for ‘plot’
    plot.etm: no visible global function definition for ‘lines’
    plot.etmCIF: no visible global function definition for ‘plot’
    plot.etmCIF: no visible global function definition for ‘lines’
    plot.etmCIF: no visible global function definition for ‘segments’
    print.summary.etm: no visible global function definition for ‘quantile’
    print.summary.etmCIF: no visible global function definition for
      ‘quantile’
    Undefined global functions or variables:
      axis box close.screen lines model.extract par plot qnorm quantile
      screen segments split.screen terms
    Consider adding
      importFrom("graphics", "axis", "box", "close.screen", "lines", "par",
                 "plot", "screen", "segments", "split.screen")
      importFrom("stats", "model.extract", "qnorm", "quantile", "terms")
    to your NAMESPACE file.
    ```

# evaluator

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggalt’ ‘pander’ ‘psych’
      All declared Imports should be used.
    Missing or unexported object: ‘purrr::by_row’
    ```

# EventStudy

Version: 0.34

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.5Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘curl’ ‘openxlsx’ ‘stringr’
      All declared Imports should be used.
    ```

# evolqg

Version: 0.2-5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# EWCE

Version: 1.3.0

## In both

*   checking examples ... ERROR
    ```
    ...
    [1] ""
    [1] "oligodendrocytes"
    [1] 0.03
    [1] "Fold enrichment: 10.8249226105033"
    [1] "Standard deviations from mean: 3.71155891479919"
    [1] ""
    [1] "pyramidal CA1"
    [1] 0.13
    [1] ""
    [1] "pyramidal SS"
    [1] 0.16
    [1] ""
    > 
    > # Bootstrap significance testing controlling for transcript length and GC content
    > full_results = bootstrap.enrichment.test(sct_data=celltype_data,human.hits=human.hits,
    +   human.bg=human.bg,reps=reps,sub=subCellStatus,geneSizeControl=TRUE)
    Error in getBM(attributes = c("transcript_length", "percentage_gc_content",  : 
      Invalid attribute(s): percentage_gc_content 
    Please use the function 'listAttributes' to get valid attribute names
    Calls: bootstrap.enrichment.test -> prepare.genesize.control.network -> getBM
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning: Removed 199 rows containing non-finite values (stat_boxplot).
    Warning: The plyr::rename operation has created duplicates for the following name(s): (`colour`)
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Removed 221 rows containing non-finite values (stat_boxplot).
    Warning: The plyr::rename operation has created duplicates for the following name(s): (`colour`)
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Removed 221 rows containing non-finite values (stat_boxplot).
    Warning: The plyr::rename operation has created duplicates for the following name(s): (`colour`)
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Removed 221 rows containing non-finite values (stat_boxplot).
    Warning: The plyr::rename operation has created duplicates for the following name(s): (`colour`)
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Removed 413 rows containing non-finite values (stat_boxplot).
    Warning: The plyr::rename operation has created duplicates for the following name(s): (`colour`)
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Removed 335 rows containing non-finite values (stat_boxplot).
    Quitting from lines 238-242 (EWCE.Rmd) 
    Error: processing vignette 'EWCE.Rmd' failed with diagnostics:
    Invalid attribute(s): percentage_gc_content 
    Please use the function 'listAttributes' to get valid attribute names
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘pdf’
    generate.bootstrap.plots: no visible global function definition for
      ‘dev.off’
    merged_ewce: no visible global function definition for ‘sd’
    prepare.genesize.control.network: no visible global function definition
      for ‘aggregate’
    prepare.genesize.control.network: no visible global function definition
      for ‘data’
    prepare.genesize.control.network: no visible global function definition
      for ‘quantile’
    read_celltype_data: no visible global function definition for
      ‘read.csv’
    read_celltype_data: no visible global function definition for
      ‘aggregate’
    Undefined global functions or variables:
      aggregate data dev.off p.adjust pdf quantile read.csv sd
    Consider adding
      importFrom("grDevices", "dev.off", "pdf")
      importFrom("stats", "aggregate", "p.adjust", "quantile", "sd")
      importFrom("utils", "data", "read.csv")
    to your NAMESPACE file.
    ```

# ExtDist

Version: 0.6-3

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    rBeta: no visible global function definition for ‘rbeta’
    rBeta_ab: no visible global function definition for ‘rbeta’
    rLaplace: no visible global function definition for ‘runif’
    rLogistic: no visible global function definition for ‘rlogis’
    rNormal: no visible global function definition for ‘rnorm’
    rSSRTB: no visible global function definition for ‘runif’
    rUniform: no visible global function definition for ‘runif’
    vcov.eDist: no visible global function definition for ‘cov2cor’
    wmle: no visible global function definition for ‘coef’
    Undefined global functions or variables:
      abline capture.output coef cov2cor curve dbeta dlogis dnorm dunif
      getS3method hist median par pbeta plogis plot pnorm punif qbeta
      qlogis qnorm qunif rbeta rlogis rnorm runif sd uniroot var
    Consider adding
      importFrom("graphics", "abline", "curve", "hist", "par", "plot")
      importFrom("stats", "coef", "cov2cor", "dbeta", "dlogis", "dnorm",
                 "dunif", "median", "pbeta", "plogis", "pnorm", "punif",
                 "qbeta", "qlogis", "qnorm", "qunif", "rbeta", "rlogis",
                 "rnorm", "runif", "sd", "uniroot", "var")
      importFrom("utils", "capture.output", "getS3method")
    to your NAMESPACE file.
    ```

# eyetrackingR

Version: 0.1.6

## In both

*   checking examples ... ERROR
    ```
    ...
    +                                time_column = "TimeFromTrialOnset",
    +                                trackloss_column = "TrackLoss",
    +                                aoi_columns = c('Animate','Inanimate'),
    +                                treat_non_aoi_looks_as_missing = TRUE )
    `mutate_each()` is deprecated.
    Use `mutate_all()`, `mutate_at()` or `mutate_if()` instead.
    To map `funs` over a selection of variables, use `mutate_at()`
    > response_window <- subset_by_window(data, window_start_time = 15500, window_end_time = 21000, 
    +                                     rezero = FALSE)
    Avg. window length in new data will be 5500
    > response_time <- make_time_sequence_data(response_window, time_bin_size = 500, aois = "Animate", 
    +                                          predictor_columns = "Sex")
    > 
    > time_cluster_data <- make_time_cluster_data(data = response_time, predictor_column = "SexM", 
    +                          aoi = "Animate", test = "lmer", 
    +                          threshold = 1.5, 
    +                          formula = LogitAdjusted ~ Sex + (1|Trial) + (1|ParticipantName))
    Error in UseMethod("analyze_time_bins") : 
      no applicable method for 'analyze_time_bins' applied to an object of class "data.frame"
    Calls: make_time_cluster_data ... make_time_cluster_data.time_sequence_data -> do.call -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Computing t.test for each time bin...
      Computing t.test for each time bin...
      `mutate_each()` is deprecated.
      Use `mutate_all()`, `mutate_at()` or `mutate_if()` instead.
      To map `funs` over a selection of variables, use `mutate_at()`
      Avg. window length in new data will be 5500
      Performing Trackloss Analysis...
      Will exclude trials whose trackloss proportion is greater than : 0.25
      	...removed  33  trials.
      Error in UseMethod("make_time_cluster_data") : 
        no applicable method for 'make_time_cluster_data' applied to an object of class "data.frame"
      Calls: test_check ... source_file -> eval -> eval -> make_time_cluster_data
      testthat results ================================================================
      OK: 38 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

# ezsim

Version: 0.5.5

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    BugReports field is not a suitable URL but contains an email address
      which will be used as from R 3.4.0
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plot.ezsim: no visible global function definition for ‘tail’
    plot.ezsim: no visible global function definition for ‘plot’
    plot.ezsim: no visible binding for global variable ‘dnorm’
    plot.ezsim: no visible binding for global variable ‘pdf’
    plot.ezsim: no visible global function definition for ‘dev.off’
    plot.ezsim : <anonymous>: no visible global function definition for
      ‘dev.new’
    plot.summary.ezsim: no visible global function definition for ‘head’
    plot.summary.ezsim: no visible global function definition for ‘tail’
    plot.summary.ezsim: no visible binding for global variable ‘pdf’
    plot.summary.ezsim: no visible global function definition for ‘dev.off’
    plot.summary.ezsim : <anonymous>: no visible global function definition
      for ‘dev.new’
    Undefined global functions or variables:
      as.formula dev.new dev.off dnorm head pdf plot quantile runif tail
    Consider adding
      importFrom("grDevices", "dev.new", "dev.off", "pdf")
      importFrom("graphics", "plot")
      importFrom("stats", "as.formula", "dnorm", "quantile", "runif")
      importFrom("utils", "head", "tail")
    to your NAMESPACE file.
    ```

# facopy

Version: 1.10.0

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘cgdsr’ ‘coin’ ‘ggplot2’ ‘gridExtra’ ‘facopy.annot’ ‘grid’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘grid’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    variableSummary: no visible global function definition for
      ‘write.table’
    Undefined global functions or variables:
      abline anova axis binomial chisq.test colorRampPalette combn cor.test
      data dev.off fisher.test formula glm grid heat.colors image
      kruskal.test layout legend lm mtext oneway.test p.adjust par pdf
      phyper plot.new plot.window points quantile read.delim read.table
      rect title wilcox.test write.table
    Consider adding
      importFrom("grDevices", "colorRampPalette", "dev.off", "heat.colors",
                 "pdf")
      importFrom("graphics", "abline", "axis", "grid", "image", "layout",
                 "legend", "mtext", "par", "plot.new", "plot.window",
                 "points", "rect", "title")
      importFrom("stats", "anova", "binomial", "chisq.test", "cor.test",
                 "fisher.test", "formula", "glm", "kruskal.test", "lm",
                 "oneway.test", "p.adjust", "phyper", "quantile",
                 "wilcox.test")
      importFrom("utils", "combn", "data", "read.delim", "read.table",
                 "write.table")
    to your NAMESPACE file.
    ```

# factoextra

Version: 1.0.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘NbClust’
    ```

# FAOSTAT

Version: 2.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    Aggregation : foo: no visible global function definition for
      ‘weighted.mean’
    Aggregation: no visible global function definition for ‘txtProgressBar’
    Aggregation: no visible global function definition for
      ‘setTxtProgressBar’
    constructSYB: no visible global function definition for ‘str’
    fillCountryCode: no visible global function definition for ‘na.omit’
    getFAO: no visible global function definition for ‘read.csv’
    getWDImetaData: no visible global function definition for ‘write.csv’
    getWDItoSYB: no visible global function definition for ‘na.omit’
    lsgr: no visible global function definition for ‘na.omit’
    lsgr: no visible global function definition for ‘coef’
    lsgr: no visible global function definition for ‘lm’
    Undefined global functions or variables:
      coef lm na.omit read.csv setTxtProgressBar str txtProgressBar
      weighted.mean write.csv
    Consider adding
      importFrom("stats", "coef", "lm", "na.omit", "weighted.mean")
      importFrom("utils", "read.csv", "setTxtProgressBar", "str",
                 "txtProgressBar", "write.csv")
    to your NAMESPACE file.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 179 marked UTF-8 strings
    ```

# fastR2

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# fCCAC

Version: 1.2.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    fccac: no visible binding for global variable ‘variables’
    Undefined global functions or variables:
      variables
    ```

# fdq

Version: 0.2

## In both

*   checking whether package ‘fdq’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/fdq/new/fdq.Rcheck/00install.out’ for details.
    ```

# FField

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘ggplot2’ ‘gridExtra’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    FFieldPtRepDemo: no visible global function definition for ‘ggplot’
    FFieldPtRepDemo: no visible binding for global variable ‘mtcars’
    FFieldPtRepDemo: no visible global function definition for ‘aes’
    FFieldPtRepDemo: no visible binding for global variable ‘mpg’
    FFieldPtRepDemo: no visible global function definition for ‘geom_point’
    FFieldPtRepDemo: no visible global function definition for ‘geom_text’
    FFieldPtRepDemo: no visible global function definition for ‘ggtitle’
    FFieldPtRepDemo: no visible global function definition for
      ‘geom_segment’
    FFieldPtRepDemo: no visible global function definition for
      ‘grid.arrange’
    Undefined global functions or variables:
      aes geom_point geom_segment geom_text ggplot ggtitle grid.arrange mpg
      mtcars
    Consider adding
      importFrom("datasets", "mtcars")
    to your NAMESPACE file.
    ```

# Fgmutils

Version: 0.9.4

## In both

*   checking whether package ‘Fgmutils’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/Fgmutils/new/Fgmutils.Rcheck/00install.out’ for details.
    ```

# fgsea

Version: 1.2.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    gmtPathways: no visible binding for global variable ‘head’
    Undefined global functions or variables:
      head
    Consider adding
      importFrom("utils", "head")
    to your NAMESPACE file.
    ```

# FindMyFriends

Version: 1.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Found more than one class "file" in cache; using the first, from namespace 'BiocGenerics'
    Also defined by 'filehash'
    Preclustering resulted in 3326 gene groups (1.119 seconds elapsed)
    Grouping resulted in 3138 gene groups (10.986 seconds elapsed)
    Total time elapsed was 12.113 seconds
    Presplitting resulted in 3160 gene groups (0.046 seconds elapsed)
    Adding missing grouping variables: `org`, `contig`
    Splitting resulted in 3603 gene groups (5 minutes and 23.61 seconds elapsed)
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Adding missing grouping variables: `org`, `contig`
    Merging resulted in 3399 gene groups (6.665 seconds elapsed)
    Total time elapsed was 5 minutes and 30.321 seconds
    Error: processing vignette 'FindMyFriends_intro.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        extdata   1.8Mb
        libs      5.6Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘gtable:::insert.unit’ ‘gtable:::z_arrange_gtables’
      See the note in ?`:::` about the use of this operator.
    ```

# fishmove

Version: 0.3-3

## In both

*   checking R code for possible problems ... NOTE
    ```
    fishmove : coefs: no visible global function definition for ‘coef’
    fishmove : coefs: no visible global function definition for ‘pf’
    fishmove: no visible global function definition for ‘lm’
    fishmove: no visible global function definition for ‘predict.lm’
    fishmove.estimate: no visible global function definition for ‘quantile’
    fishmove.estimate : ddoublenorm: no visible global function definition
      for ‘dnorm’
    fishmove.query: no visible global function definition for ‘hasArg’
    fishmove.query : f: no visible global function definition for ‘pnorm’
    fishmove.query: no visible global function definition for ‘uniroot’
    pdk: no visible global function definition for ‘hasArg’
    pdk : eq: no visible global function definition for ‘dnorm’
    Undefined global functions or variables:
      coef dnorm hasArg lm pf pnorm predict.lm quantile uniroot
    Consider adding
      importFrom("methods", "hasArg")
      importFrom("stats", "coef", "dnorm", "lm", "pf", "pnorm", "predict.lm",
                 "quantile", "uniroot")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# fitbitScraper

Version: 0.1.8

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 22-24 (fitbitScraper-examples.Rmd) 
    Error: processing vignette 'fitbitScraper-examples.Rmd' failed with diagnostics:
    login failed
    Execution halted
    ```

# fitdistrplus

Version: 1.0-9

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

# FLightR

Version: 0.4.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rgdal’
      All declared Imports should be used.
    ```

# flowCHIC

Version: 1.10.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘EBImage’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# fontHind

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# fontMPlus

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# forecast

Version: 8.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        libs   4.3Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘seasonal’, ‘forecTheta’
    ```

# FourCSeq

Version: 1.10.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Packages listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘DESeq2’ ‘GenomicRanges’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      'ggplot2' 'methods'
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      'keepSeqlevels'
    updateObject,FourC: no visible global function definition for
      'callNextMethod'
    Undefined global functions or variables:
      DataFrame IRanges Rle Seqinfo SimpleList abline aes axis blues9
      callNextMethod change count differentialInteraction element_blank fit
      fitDown fitUp formula geom_path geom_point ggplot keepSeqlevels labs
      legend mad median metadata mid mtext new p.adjust par peak plot pnorm
      points predict rel relevel scale_fill_gradient2 scale_y_continuous
      seqlengths seqlevels seqlevels<- subjectHits subsetByOverlaps theme
      theme_bw theme_set write.table
    Consider adding
      importFrom("grDevices", "blues9")
      importFrom("graphics", "abline", "axis", "legend", "mtext", "par",
                 "plot", "points")
      importFrom("methods", "callNextMethod", "new")
      importFrom("stats", "formula", "mad", "median", "p.adjust", "pnorm",
                 "predict", "relevel")
      importFrom("utils", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# fourierin

Version: 0.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        libs   4.5Mb
    ```

# fpp2

Version: 2.1

## Newly broken

*   checking whether package ‘fpp2’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘forecast::autolayer’ by ‘ggplot2::autolayer’ when loading ‘fpp2’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/fpp2/new/fpp2.Rcheck/00install.out’ for details.
    ```

# freqweights

Version: 1.0.4

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘Hmisc’
    ```

# FRK

Version: 0.1.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    
    Package which this enhances but not available for checking: ‘dggrids’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   4.8Mb
        doc    1.6Mb
    ```

# frontiles

Version: 1.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    alphafrontier.2d: no visible global function definition for ‘par’
    alphafrontier.2d: no visible global function definition for ‘polygon’
    alphafrontier.2d: no visible global function definition for ‘lines’
    alphafrontier.2d: no visible global function definition for ‘points’
    alphafrontier.3d: no visible global function definition for ‘layout’
    alphafrontier.3d: no visible global function definition for ‘barplot’
    alphafrontier.3d: no visible global function definition for ‘axis’
    alphafrontier.3d: no visible global function definition for ‘lines’
    alphafrontier.3d: no visible global function definition for ‘na.omit’
    ordermfrontier.2d: no visible global function definition for ‘par’
    ordermfrontier.2d: no visible global function definition for ‘polygon’
    ordermfrontier.2d: no visible global function definition for ‘lines’
    ordermfrontier.2d: no visible global function definition for ‘points’
    ordermscore.boot: no visible global function definition for ‘sd’
    Undefined global functions or variables:
      axis barplot layout legend lines mtext na.omit par points polygon sd
    Consider adding
      importFrom("graphics", "axis", "barplot", "layout", "legend", "lines",
                 "mtext", "par", "points", "polygon")
      importFrom("stats", "na.omit", "sd")
    to your NAMESPACE file.
    ```

# FSelectorRcpp

Version: 0.1.8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        doc    2.2Mb
        libs   8.7Mb
    ```

# fSRM

Version: 0.6.4

## In both

*   checking whether package ‘fSRM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/fSRM/new/fSRM.Rcheck/00install.out’ for details.
    ```

# FunciSNP

Version: 1.20.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘ggplot2’
    A package should be listed in only one of these fields.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    bedColors: no visible global function definition for 'col2rgb'
    bedColors: no visible global function definition for 'png'
    bedColors: no visible global function definition for 'box'
    bedColors: no visible global function definition for 'par'
    bedColors: no visible global function definition for 'axis'
    bedColors: no visible global function definition for 'dev.off'
    Undefined global functions or variables:
      R.squared TSS.human.GRCh37 abline as.dendrogram axis box col2rgb
      colorRampPalette data dev.off dist distance.from.tag fisher.test
      hclust lincRNA order.dendrogram p.adjust par pdf plot png r.2
      read.delim read.table refseqgenes sig text timestamp value variable
      write.table
    Consider adding
      importFrom("grDevices", "col2rgb", "colorRampPalette", "dev.off",
                 "pdf", "png")
      importFrom("graphics", "abline", "axis", "box", "par", "plot", "text")
      importFrom("stats", "as.dendrogram", "dist", "fisher.test", "hclust",
                 "order.dendrogram", "p.adjust")
      importFrom("utils", "data", "read.delim", "read.table", "timestamp",
                 "write.table")
    to your NAMESPACE file.
    ```

# furrowSeg

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘EBImage’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# fuser

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        libs   5.0Mb
    ```

# GADMTools

Version: 2.1-1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

# gaiah

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘maptools’ ‘rgeos’ ‘stringr’ ‘tidyr’
      All declared Imports should be used.
    ```

# gapfill

Version: 0.9.5-3

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘raster’ ‘doParallel’ ‘doMPI’
    ```

# gastempt

Version: 0.4.01

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 58.6Mb
      sub-directories of 1Mb or more:
        libs  58.2Mb
    ```

# gcatest

Version: 1.6.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    gcat: no visible global function definition for ‘pchisq’
    gcat.stat: no visible global function definition for ‘complete.cases’
    gcatest: no visible global function definition for ‘pchisq’
    Undefined global functions or variables:
      complete.cases pchisq
    Consider adding
      importFrom("stats", "complete.cases", "pchisq")
    to your NAMESPACE file.
    ```

# gCrisprTools

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.2Mb
      sub-directories of 1Mb or more:
        data   2.3Mb
        doc    1.1Mb
    ```

# GDINA

Version: 1.4.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        libs   5.7Mb
    ```

# GEM

Version: 1.2.0

## In both

*   checking whether package ‘GEM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/GEM/new/GEM.Rcheck/00install.out’ for details.
    ```

# gender

Version: 0.5.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘genderdata’
    ```

# GeneralizedUmatrix

Version: 0.9.5

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ProjectionBasedClustering’
    ```

# GenoGAM

Version: 1.4.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    plot_base: no visible global function definition for ‘lines’
    plot_base: no visible global function definition for ‘abline’
    plot_base: no visible global function definition for ‘mtext’
    writeToBroadPeaks: no visible global function definition for
      ‘write.table’
    writeToNarrowPeaks: no visible global function definition for
      ‘write.table’
    xsd : <anonymous>: no visible binding for global variable ‘position’
    xsd: no visible binding for global variable ‘position’
    Undefined global functions or variables:
      abline as.formula axis coefficients dev.off dnbinom estimate fdr fit
      gene hist id lines mtext optim p.adjust par plot png pnorm position
      pval pvalue region runif write.table zscore
    Consider adding
      importFrom("grDevices", "dev.off", "png")
      importFrom("graphics", "abline", "axis", "hist", "lines", "mtext",
                 "par", "plot")
      importFrom("stats", "as.formula", "coefficients", "dnbinom", "optim",
                 "p.adjust", "pnorm", "runif")
      importFrom("utils", "write.table")
    to your NAMESPACE file.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'plot.GenoGAM':
      ‘plot.GenoGAM’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# genomation

Version: 1.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc       3.6Mb
        extdata   1.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RUnit’
      All declared Imports should be used.
    Unexported object imported by a ':::' call: ‘BiocGenerics:::testPackage’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘cache’
    Please remove from your package.
    ```

# GenomicDataCommons

Version: 1.0.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Loading required package: magrittr
    
    Attaching package: 'GenomicDataCommons'
    
    The following object is masked from 'package:stats':
    
        filter
    
    sh: 1: gdc-client: not found
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    default_fields.character: no visible binding for global variable
      ‘defaults’
    Undefined global functions or variables:
      defaults
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'as.data.frame.GDCResults':
      ‘as.data.frame.GDCResults’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# GenomicInteractions

Version: 1.10.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc       2.0Mb
        extdata   7.9Mb
    ```

# genotypeeval

Version: 1.8.0

## In both

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented code objects:
      ‘makeHistogram’ ‘pcaPlot’ ‘tsnePlot’
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# genphen

Version: 1.4.0

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘randomForest’ ‘e1071’ ‘ggplot2’ ‘effsize’ ‘Biostrings’ ‘rjags’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    getBayesianTtest : getDataList: no visible global function definition
      for ‘sd’
    getBayesianTtest : getEss: no visible global function definition for
      ‘effectiveSize’
    getBayesianTtest: no visible global function definition for ‘update’
    getTTestScore: no visible global function definition for ‘t.test’
    plotGenphenBayes: no visible binding for global variable ‘g.1’
    plotGenphenBayes: no visible binding for global variable ‘site’
    plotGenphenBayes: no visible binding for global variable ‘g.2’
    plotGenphenRfSvm: no visible binding for global variable ‘ca.hdi.H’
    plotGenphenRfSvm: no visible binding for global variable ‘ca.hdi.L’
    plotGenphenRfSvm: no visible global function definition for
      ‘terrain.colors’
    plotManhattan: no visible binding for global variable ‘t.test.pvalue’
    Undefined global functions or variables:
      ca.hdi.H ca.hdi.L effectiveSize g.1 g.2 sd site t.test t.test.pvalue
      terrain.colors update
    Consider adding
      importFrom("grDevices", "terrain.colors")
      importFrom("stats", "sd", "t.test", "update")
    to your NAMESPACE file.
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    Loaded modules: basemod,bugs
    Warning: Ignoring unknown aesthetics: x
    Warning: Ignoring unknown aesthetics: x
    Warning: Ignoring unknown aesthetics: x
    Warning: Ignoring unknown aesthetics: fill
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'genphenManual.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm2e.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.7 ^^M
           
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# GenVisR

Version: 1.6.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Obtaining CDS Coordinates
    'select()' returned 1:many mapping between keys and columns
    Obtaining UTR Coordinates
    'select()' returned 1:many mapping between keys and columns
    Calculating transform
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-25>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-26>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-27>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Error: processing vignette 'GenVisR_intro.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    waterfall: warning in waterfall_align(genes = gene_plot, heatmap =
      heatmap, burden = burden_plot, clinical = clinical_plot, proportion =
      proportions_plot, section_heights = section_heights): partial
      argument match of 'proportion' to 'proportions'
    waterfall: warning in waterfall_align(genes = gene_plot, heatmap =
      heatmap, burden = burden_plot, proportion = proportions_plot,
      section_heights = section_heights): partial argument match of
      'proportion' to 'proportions'
    ```

# geofacet

Version: 0.1.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
                                                             ~^~
      tests/testthat/test-utilities.r:42:1: style: lines should not be more than 80 characters.
        expect_true(all(c("arg1", "arg2", "arg3") %in% names(test_fun(arg1 = 1, arg2 = 1, arg3 = 1))))
      ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tests/testthat/test-viridis.R:16:1: style: Trailing whitespace is superfluous.
        
      ^~
      
      
      testthat results ================================================================
      OK: 14 SKIPPED: 0 FAILED: 1
      1. Failure: package Style (@test-zzz-lintr.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 79 marked UTF-8 strings
    ```

# geoknife

Version: 1.5.5

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      27: eval(exprs, env)
      28: source_file(path, new.env(parent = env), chdir = TRUE)
      29: force(code)
      30: with_reporter(reporter = reporter, start_end_reporter = start_end_reporter,     {        lister$start_file(basename(path))        source_file(path, new.env(parent = env), chdir = TRUE)        end_context()    })
      31: FUN(X[[i]], ...)
      32: lapply(paths, test_file, env = env, reporter = current_reporter,     start_end_reporter = FALSE, load_helpers = FALSE)
      33: force(code)
      34: with_reporter(reporter = current_reporter, results <- lapply(paths,     test_file, env = env, reporter = current_reporter, start_end_reporter = FALSE,     load_helpers = FALSE))
      35: test_files(paths, reporter = reporter, env = env, ...)
      36: test_dir(test_path, reporter = reporter, env = env, filter = filter,     ...)
      37: with_top_env(env, {    test_dir(test_path, reporter = reporter, env = env, filter = filter,         ...)})
      38: run_tests(package, test_path, filter, reporter, ...)
      39: test_check("geoknife")
      An irrecoverable exception occurred. R is aborting now ...
      Segmentation fault (core dumped)
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 360-364 (geoknife.Rmd) 
    Error: processing vignette 'geoknife.Rmd' failed with diagnostics:
    need finite 'xlim' values
    Execution halted
    ```

# GeomComb

Version: 1.0

## Newly broken

*   checking whether package ‘GeomComb’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘forecast::autolayer’ by ‘ggplot2::autolayer’ when loading ‘GeomComb’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/GeomComb/new/GeomComb.Rcheck/00install.out’ for details.
    ```

# geotoolsR

Version: 1.0

## In both

*   checking whether package ‘geotoolsR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/geotoolsR/new/geotoolsR.Rcheck/00install.out’ for details.
    ```

# GERGM

Version: 0.11.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        libs   4.2Mb
    ```

# GerminaR

Version: 1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘shinydashboard’
      All declared Imports should be used.
    ```

# gespeR

Version: 1.8.0

## In both

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'c,Phenotypes-method':
    \S4method{c}{Phenotypes}
      Code: function(x, ...)
      Docs: function(x, ..., recursive = FALSE)
      Argument names in docs not in code:
        recursive
    ```

*   checking R code for possible problems ... NOTE
    ```
    .gespeR.cv: no visible global function definition for ‘coef’
    .select.model: no visible global function definition for ‘predict’
    concordance: no visible global function definition for ‘cor’
    lasso.rand: no visible global function definition for ‘runif’
    plot.gespeR: no visible global function definition for ‘hist’
    stability.selection: no visible global function definition for ‘lm’
    Phenotypes,character: no visible global function definition for
      ‘read.delim’
    Undefined global functions or variables:
      coef cor hist lm predict read.delim runif
    Consider adding
      importFrom("graphics", "hist")
      importFrom("stats", "coef", "cor", "lm", "predict", "runif")
      importFrom("utils", "read.delim")
    to your NAMESPACE file.
    ```

# GetLattesData

Version: 0.8

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘GetLattesData-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gld_get_lattes_data
    > ### Title: Downloads and reads Lattes data based on a vector of Lattes ids
    > ### Aliases: gld_get_lattes_data
    > 
    > ### ** Examples
    > 
    > 
    > l.out <- gld_get_lattes_data(id.vec = 'K4713546D3',
    +                              field.qualis = 'ECONOMIA')
    
    Downloading file  /home/muelleki/tmp/RtmpglWMNF/K4713546D3_2017-11-20.zipWarning in utils::download.file(url = my.link, destfile = dest.file, quiet = T,  :
      unable to resolve 'buscacv.cnpq.br'
    Error in utils::download.file(url = my.link, destfile = dest.file, quiet = T,  : 
      cannot open URL 'http://buscacv.cnpq.br/buscacv/rest/download/curriculo/K4713546D3'
    Calls: gld_get_lattes_data -> sapply -> lapply -> FUN -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: identical(as.vector(object), TRUE)
      5: as.vector(object)
      6: gld_get_lattes_data("K4713546D3", folder.dl = "lattes files")
      7: sapply(X = id.vec, FUN = gld_download_lattes_files, folder.dl = folder.dl)
      8: lapply(X = X, FUN = FUN, ...)
      9: FUN(X[[i]], ...)
      10: utils::download.file(url = my.link, destfile = dest.file, quiet = T, mode = "wb", 
             method = "internal")
      
      testthat results ================================================================
      OK: 1 SKIPPED: 0 FAILED: 1
      1. Error: Test of main function (@test_gld.R#5) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 36-45 (gld_vignette-ReadLattes.Rmd) 
    Error: processing vignette 'gld_vignette-ReadLattes.Rmd' failed with diagnostics:
    cannot open URL 'http://buscacv.cnpq.br/buscacv/rest/download/curriculo/K4713546D3'
    Execution halted
    ```

# gettingtothebottom

Version: 3.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    example.alpha: no visible global function definition for ‘rnorm’
    example.quadratic.approx: no visible global function definition for
      ‘rnorm’
    generate_nnm: no visible global function definition for ‘rnorm’
    generate_nnm: no visible global function definition for ‘rgamma’
    generate_nnm: no visible global function definition for ‘dnorm’
    plot_nnm: no visible global function definition for ‘stack’
    testmatrix: no visible global function definition for ‘rnorm’
    Undefined global functions or variables:
      dnorm rgamma rnorm stack
    Consider adding
      importFrom("stats", "dnorm", "rgamma", "rnorm")
      importFrom("utils", "stack")
    to your NAMESPACE file.
    ```

# gfer

Version: 0.1.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# GGally

Version: 1.3.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    No edge attributes
    > 
    > ggnet(n, label = TRUE, alpha = 1, color = "white", segment.color = "black")
    Loading required package: sna
    Loading required package: statnet.common
    
    Attaching package: ‘statnet.common’
    
    The following object is masked from ‘package:base’:
    
        order
    
    sna: Tools for Social Network Analysis
    Version 2.4 created on 2016-07-23.
    copyright (c) 2005, Carter T. Butts, University of California-Irvine
     For citation information, type citation("sna").
     Type help(package="sna") to get started.
    
    Loading required package: scales
    Error: Columns `x`, `y`, `xend`, `yend` must be 1d atomic vectors or lists
    Execution halted
    ```

# ggalt

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘plotly’
      All declared Imports should be used.
    ```

# ggbio

Version: 1.24.1

## In both

*   checking examples ... ERROR
    ```
    ...
    > data(genesymbol, package = "biovizBase")
    > txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
    > 
    > 
    > ###################################################
    > ### code chunk number 27: txdb-visual
    > ###################################################
    > p1 <- autoplot(txdb, which = genesymbol["ALDOA"], names.expr = "tx_name:::gene_id")
    Parsing transcripts...
    Parsing exons...
    Parsing cds...
    Parsing utrs...
    ------exons...
    ------cdss...
    ------introns...
    ------utr...
    aggregating...
    Done
    Constructing graphics...
    Error: Don't know how to add o to a plot
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      'S4Vectors:::top_prenv' 'ggplot2:::add_ggplot' 'ggplot2:::cunion'
      'ggplot2:::rename_aes' 'ggplot2:::rescale01'
      'ggplot2:::set_last_plot'
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    layout_karyogram,GRanges: no visible binding for global variable
      'yend2'
    layout_karyogram,GRanges : <anonymous>: no visible binding for global
      variable 'name'
    layout_karyogram,GRanges : <anonymous>: no visible binding for global
      variable 'gieStain'
    plotFragLength,character-GRanges: no visible binding for global
      variable '.fragLength'
    plotSpliceSum,character-EnsDb: possible error in GRangesFilter(which,
      condition = "overlapping"): unused argument (condition =
      "overlapping")
    stat_mismatch,GRanges: no visible binding for global variable 'sts'
    stat_mismatch,GRanges: no visible binding for global variable 'eds'
    stat_mismatch,GRanges: no visible binding for global variable 'read'
    Undefined global functions or variables:
      .fragLength .layout_circle.stats .x breaks coefs cytobands data eds
      fe fl gieStain ideoCyto indexProbesProcessed midpoint mt name read se
      stepping sts value variable x xend y y.text y2 yend yend2
    Consider adding
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# ggCompNet

Version: 0.1.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 79-98 (examples-from-paper.Rmd) 
    Error: processing vignette 'examples-from-paper.Rmd' failed with diagnostics:
    Columns `x`, `y`, `xend`, `yend` must be 1d atomic vectors or lists
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc   6.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggmap’ ‘gridExtra’ ‘scales’ ‘tnet’
      All declared Imports should be used.
    ```

# ggconf

Version: 0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

# ggcyto

Version: 1.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        doc   5.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    Unexported objects imported by ':::' calls:
      ‘flowWorkspace:::.mergeGates’ ‘flowWorkspace:::compact’
      ‘flowWorkspace:::fix_y_axis’ ‘ggplot2:::+.gg’ ‘ggplot2:::add_group’
      ‘ggplot2:::check_aesthetics’ ‘ggplot2:::ggplot.data.frame’
      ‘ggplot2:::is.waive’ ‘ggplot2:::is_calculated_aes’
      ‘ggplot2:::make_scale’ ‘ggplot2:::plot_clone’
      ‘ggplot2:::print.ggplot’ ‘ggplot2:::scales_add_defaults’
      ‘ggplot2:::update_theme’
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
      approx axis density desc dist getS3method gray modifyList name
    Consider adding
      importFrom("grDevices", "gray")
      importFrom("graphics", "axis")
      importFrom("stats", "approx", "density", "dist")
      importFrom("utils", "getS3method", "modifyList")
    to your NAMESPACE file.
    ```

# ggdmc

Version: 0.1.3.9

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.3Mb
      sub-directories of 1Mb or more:
        libs   8.9Mb
    ```

# GGEBiplots

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘GGEBiplotGUI’ ‘gge’ ‘utils’
      All declared Imports should be used.
    ```

# ggenealogy

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2356 marked UTF-8 strings
    ```

# ggExtra

Version: 0.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grDevices’
      All declared Imports should be used.
    ```

# ggfan

Version: 0.1.0

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 59-62 (geom_fan.Rmd) 
    Error: processing vignette 'geom_fan.Rmd' failed with diagnostics:
    replacement has 0 rows, data has 1
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘colorspace’ ‘grid’
      All declared Imports should be used.
    ```

# ggforce

Version: 0.1.1

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'geom_link':
    geom_link0
      Code: function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", ..., arrow = NULL, lineend =
                     "butt", linejoin = "round", na.rm = FALSE, show.legend
                     = NA, inherit.aes = TRUE)
      Docs: function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", ..., arrow = NULL, lineend =
                     "butt", na.rm = FALSE, show.legend = NA, inherit.aes =
                     TRUE)
      Argument names in code not in docs:
        linejoin
      Mismatches in argument names:
        Position: 8 Code: linejoin Docs: na.rm
        Position: 9 Code: na.rm Docs: show.legend
        Position: 10 Code: show.legend Docs: inherit.aes
    ```

# ggformula

Version: 0.6

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggformula-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gf_dist
    > ### Title: Plot distributions
    > ### Aliases: gf_dist
    > 
    > ### ** Examples
    > 
    > gf_histogram( ..density.. ~ rnorm(100), bins = 20) %>%
    +   gf_dist("norm", color = "red")
    > 
    > gf_dist(dist = "norm", color = "red")
    Error: Cannot add ggproto objects together. Did you forget to add this object to a ggplot object?
    Execution halted
    ```

# ggfortify

Version: 0.4.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘ggfortify-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gglagplot
    > ### Title: Plot time series against lagged versions of themselves
    > ### Aliases: gglagplot
    > 
    > ### ** Examples
    > 
    > gglagplot(AirPassengers)
    Error: `x` must be a vector, not a ts object, do you want `stats::lag()`?
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
        
      ^~
      
      
      testthat results ================================================================
      OK: 1616 SKIPPED: 0 FAILED: 6
      1. Failure: autoplot.aareg works for lung (@test-surv.R#220) 
      2. Failure: autoplot.aareg works for lung (@test-surv.R#221) 
      3. Failure: autoplot.aareg works for lung (@test-surv.R#222) 
      4. Failure: autoplot.aareg works for lung (@test-surv.R#223) 
      5. Error: gglagplot (@test-tslib.R#103) 
      6. Failure: Code Lint (@test_lint.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc   5.0Mb
    ```

# ggguitar

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘lazyeval’ ‘readr’
      All declared Imports should be used.
    ```

# ggimage

Version: 0.0.7

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘magick’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# ggiraphExtra

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘XML’ ‘mapproj’ ‘moonBook’
      All declared Imports should be used.
    ```

# gglogo

Version: 0.1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘knitr’
      All declared Imports should be used.
    ```

# ggmap

Version: 2.6.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘ggmap-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: theme_nothing
    > ### Title: Make a blank ggplot2 theme.
    > ### Aliases: theme_nothing
    > 
    > ### ** Examples
    > 
    > 
    > 
    > # no legend example
    > n <- 50
    > df <- expand.grid(x = 1:n,y = 1:n)[sample(n^2,.5*n^2),]
    > p <- qplot(x, y, data = df, geom = 'tile')
    > p
    > p + theme_nothing()
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Error in if (theme$panel.ontop) { : argument is of length zero
    Calls: <Anonymous> ... print.ggplot -> ggplot_gtable -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

# ggmosaic

Version: 0.1.2

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggmosaic-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_mosaic
    > ### Title: Mosaic plots.
    > ### Aliases: geom_mosaic stat_mosaic
    > 
    > ### ** Examples
    > 
    > 
    > data(Titanic)
    > titanic <- as.data.frame(Titanic)
    > titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))
    > 
    > 
    > ggplot(data=titanic) +
    +   geom_mosaic(aes(weight=Freq, x=product(Class), fill=Survived))
    Error in is.finite(x) : default method not implemented for type 'list'
    Calls: <Anonymous> ... lapply -> FUN -> <Anonymous> -> f -> lapply -> FUN -> f
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    
    Attaching package: 'gridExtra'
    
    The following object is masked from 'package:dplyr':
    
        combine
    
    Quitting from lines 171-175 (ggmosaic.Rmd) 
    Error: processing vignette 'ggmosaic.Rmd' failed with diagnostics:
    default method not implemented for type 'list'
    Execution halted
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'scale_type.product':
      ‘scale_type.product’
    
    S3 methods shown with full name in documentation object 'scale_type.productlist':
      ‘scale_type.productlist’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘NHANES’ ‘gridExtra’
      All declared Imports should be used.
    ```

# ggnetwork

Version: 0.5.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
                        Skye Bender-deMoll, University of Washington
     For citation information, type citation("network").
     Type help("network-package") to get started.
    
    Loading required package: sna
    Loading required package: statnet.common
    
    Attaching package: ‘statnet.common’
    
    The following object is masked from ‘package:base’:
    
        order
    
    sna: Tools for Social Network Analysis
    Version 2.4 created on 2016-07-23.
    copyright (c) 2005, Carter T. Butts, University of California-Irvine
     For citation information, type citation("sna").
     Type help(package="sna") to get started.
    
    Error: Columns `x`, `y`, `xend`, `yend` must be 1d atomic vectors or lists
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
     For citation information, type citation("network").
     Type help("network-package") to get started.
    
    Loading required package: statnet.common
    
    Attaching package: 'statnet.common'
    
    The following object is masked from 'package:base':
    
        order
    
    sna: Tools for Social Network Analysis
    Version 2.4 created on 2016-07-23.
    copyright (c) 2005, Carter T. Butts, University of California-Irvine
     For citation information, type citation("sna").
     Type help(package="sna") to get started.
    
    Quitting from lines 130-133 (ggnetwork.Rmd) 
    Error: processing vignette 'ggnetwork.Rmd' failed with diagnostics:
    Columns `x`, `y`, `xend`, `yend` must be 1d atomic vectors or lists
    Execution halted
    ```

# ggplotAssist

Version: 0.1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gcookbook’ ‘ggthemes’ ‘moonBook’ ‘tidyverse’
      All declared Imports should be used.
    ```

# ggpubr

Version: 0.1.6

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘FactoMineR’
    ```

# ggQC

Version: 0.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘plyr’ ‘reshape2’
      All declared Imports should be used.
    ```

# ggRandomForests

Version: 2.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘randomForest’
      All declared Imports should be used.
    ```

# ggraph

Version: 1.0.0

## Newly broken

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'scale_type.geometry':
      ‘scale_type.geometry’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc    3.0Mb
        libs   2.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# ggraptR

Version: 1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > if (Sys.getenv("NOT_CRAN") == "true") {  # like global skip_on_cran
      +   Sys.setenv("R_TESTS" = "")  # accroding to https://github.com/hadley/testthat/issues/144
      +   test_check("ggraptR")
      + }
      
      Initial plotError in file(filename, "r", encoding = encoding) : 
        cannot open the connection
      Calls: test_check ... source -> withVisible -> eval -> eval -> source -> file
      In addition: Warning message:
      In file(filename, "r", encoding = encoding) :
        cannot open file '../../inst/ggraptR/functions/helper.R': No such file or directory
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DBI’ ‘GGally’ ‘RColorBrewer’ ‘Rcpp’ ‘assertthat’ ‘backports’
      ‘colorspace’ ‘colourpicker’ ‘evaluate’ ‘futile.options’ ‘gdtools’
      ‘gtable’ ‘htmltools’ ‘htmlwidgets’ ‘httpuv’ ‘labeling’ ‘lambda.r’
      ‘lazyeval’ ‘magrittr’ ‘miniUI’ ‘munsell’ ‘plyr’ ‘reshape’ ‘rprojroot’
      ‘scales’ ‘stringi’ ‘stringr’ ‘svglite’ ‘tibble’ ‘xtable’ ‘yaml’
      All declared Imports should be used.
    ```

# ggrepel

Version: 0.7.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 332-362 (ggrepel.Rmd) 
    Error: processing vignette 'ggrepel.Rmd' failed with diagnostics:
    there is no package called 'gridExtra'
    Execution halted
    ```

# ggridges

Version: 0.4.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6242 marked UTF-8 strings
    ```

# ggROC

Version: 1.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘ggplot2’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘ggplot2’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ggroc: no visible global function definition for ‘ggplot’
    ggroc: no visible global function definition for ‘aes’
    ggroc: no visible global function definition for ‘geom_point’
    ggroc: no visible global function definition for ‘geom_line’
    ggroc: no visible global function definition for ‘theme’
    ggroc: no visible global function definition for ‘element_text’
    ggroc: no visible global function definition for ‘labs’
    ggroc: no visible global function definition for ‘ggsave’
    Undefined global functions or variables:
      aes element_text geom_line geom_point ggplot ggsave labs theme
    ```

# ggstance

Version: 0.3

## Newly broken

*   checking tests ...
    ```
    ...
      4. Failure: facet_grid() with free scales flips (@test-geoms.R#95) 
      
      Error: testthat unit tests failed
      Execution halted
    Running the tests in ‘tests/vdiffr.[rR]’ failed.
    Last 13 lines of output:
      <polyline points='28.20,283.68 30.94,283.68 ' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)' />
      <polyline points='28.20,121.05 30.94,121.05 ' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)' />
      <polyline points='113.30,546.61 113.30,543.88 ' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)' />
      <polyline points='264.13,546.61 264.13,543.88 ' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)' />
      <polyline points='414.97,546.61 414.97,543.88 ' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)' />
      <polyline points='565.80,546.61 565.80,543.88 ' style='stroke-width: 1.07; stroke: #333333; stroke-linecap: butt;' clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)' />
      <g clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)'><text x='108.29' y='554.99' style='font-size: 8.80px; fill: #4D4D4D; font-family: Liberation Sans;' textLength='10.01px' lengthAdjust='spacingAndGlyphs'>15</text></g>
      <g clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)'><text x='259.13' y='554.99' style='font-size: 8.80px; fill: #4D4D4D; font-family: Liberation Sans;' textLength='10.01px' lengthAdjust='spacingAndGlyphs'>20</text></g>
      <g clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)'><text x='409.96' y='554.99' style='font-size: 8.80px; fill: #4D4D4D; font-family: Liberation Sans;' textLength='10.01px' lengthAdjust='spacingAndGlyphs'>25</text></g>
      <g clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)'><text x='560.79' y='554.99' style='font-size: 8.80px; fill: #4D4D4D; font-family: Liberation Sans;' textLength='10.01px' lengthAdjust='spacingAndGlyphs'>30</text></g>
      <g clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)'><text x='362.03' y='568.04' style='font-size: 11.00px; font-family: Liberation Sans;' textLength='21.40px' lengthAdjust='spacingAndGlyphs'>mpg</text></g>
      <g clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)'><text transform='translate(13.04,307.82) rotate(-90)' style='font-size: 11.00px; font-family: Liberation Sans;' textLength='48.28px' lengthAdjust='spacingAndGlyphs'>factor(cyl)</text></g>
      <g clip-path='url(#cpMC4wMHw3MjAuMDB8NTc2LjAwfDAuMDA=)'><text x='30.94' y='14.42' style='font-size: 13.20px; font-family: Liberation Sans;' textLength='169.76px' lengthAdjust='spacingAndGlyphs'>stat_summaryh() with fun.x*()</text></g>
      </svg>
      
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# ggtern

Version: 2.2.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: annotate
    > ### Title: Create an annotation layer (ggtern version).
    > ### Aliases: annotate
    > 
    > ### ** Examples
    > 
    > ggtern() + 
    + annotate(geom  = 'text',
    +               x     = c(0.5,1/3,0.0),
    +               y     = c(0.5,1/3,0.0),
    +               z     = c(0.0,1/3,1.0),
    +               angle = c(0,30,60),
    +               vjust = c(1.5,0.5,-0.5),
    +               label = paste("Point",c("A","B","C")),
    +               color = c("green","red",'blue')) +
    +   theme_dark() + 
    +   theme_nomask()
    Error in f(..., self = self) : unused argument (<environment>)
    Calls: <Anonymous> -> print.ggplot -> ggplot_build -> <Anonymous>
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘sp’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘chemometrics’
    ```

# ggthemes

Version: 3.4.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggthemes-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: scale_shape_stata
    > ### Title: Stata shape scale
    > ### Aliases: scale_shape_stata
    > 
    > ### ** Examples
    > 
    > library("ggplot2")
    > p <- ggplot(mtcars) +
    +      geom_point(aes(x = wt, y = mpg, shape = factor(gear))) +
    +      facet_wrap(~am)
    > p + theme_stata() + scale_shape_stata()
    Error in if (theme$panel.ontop) { : argument is of length zero
    Calls: <Anonymous> ... print.ggplot -> ggplot_gtable -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 108-110 (./children/examples.Rmd) 
    Quitting from lines 36-40 (./children/examples.Rmd) 
    Error: processing vignette 'ggthemes.Rmd' failed with diagnostics:
    argument is of length zero
    Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘latticeExtra’
    ```

# GGtools

Version: 5.12.1

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘MatrixEQTL’ ‘foreach’ ‘doParallel’ ‘gwascat’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 73.2Mb
      sub-directories of 1Mb or more:
        data   27.0Mb
        doc     1.6Mb
        parts   2.0Mb
        pup     2.0Mb
        rdas   10.3Mb
        vcf    28.8Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to 'Homo.sapiens' which was already attached by Depends.
      Please remove these calls from your code.
    Namespace in Imports field not imported from: 'stats'
      All declared Imports should be used.
    Packages in Depends field not imported from:
      'Homo.sapiens' 'parallel'
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      variable 'smoothScatter'
    plot,gwSnpScreenResult-character: no visible global function definition
      for 'axis'
    Undefined global functions or variables:
      %dopar% .N FDR Homo.sapiens Matrix_eQTL_engine SlicedData abline
      approx as.data.table as.formula assay assays axis bindcadd bindmaf
      binomial chi.squared coef colData curp detectCores dffits excl
      export.gff3 firstHalf firstThird foreach forestplot formula ftable
      gwastagger gwrngs hg19.si.df hist hmm878 i i1 i2 lastThird lm maf
      mafs mclapply midThird model.matrix modelLINEAR mtext npc overlapsAny
      par pl plogis points pos predict qqplot radiusUsed ranges<- relevel
      rowRanges runOneSplit runif segments select setkey setkeyv setnames
      smoothScatter snp snpcount snpsBySeqname target text tileGenome value
      vcov wald.test x
    Consider adding
      importFrom("graphics", "abline", "axis", "hist", "mtext", "par",
                 "points", "segments", "smoothScatter", "text")
      importFrom("stats", "approx", "as.formula", "binomial", "coef",
                 "dffits", "formula", "ftable", "lm", "model.matrix",
                 "plogis", "predict", "qqplot", "relevel", "runif", "vcov")
    to your NAMESPACE file.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘MatrixEQTL’, ‘gwascat’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
      ...
    Loading required package: illuminaHumanv1.db
    
    
    Attaching package: 'Biostrings'
    
    The following object is masked from 'package:base':
    
        strsplit
    
    NOTE: some SNP in rsid were not found in location db SNPlocs.Hsapiens.dbSNP144.GRCh37
    NOTE: expanding gene ranges by radius 75000 leads to negative start positions that are reset to 1.
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'GGtools.tex' failed.
    LaTeX errors:
    ! Package auto-pst-pdf Error: 
        "shell escape" (or "write18") is not enabled:
        auto-pst-pdf will not work!
    .
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# ggtree

Version: 1.8.2

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        collapse
    
    The following object is masked from 'package:IRanges':
    
        collapse
    
    The following object is masked from 'package:S4Vectors':
    
        expand
    
    The following object is masked from 'package:ape':
    
        rotate
    
    Scale for 'fill' is already present. Adding another scale for 'fill',
    which will replace the existing scale.
    Warning: The plyr::rename operation has created duplicates for the following name(s): (`size`)
    Quitting from lines 70-75 (advanceTreeAnnotation.Rmd) 
    Error: processing vignette 'advanceTreeAnnotation.Rmd' failed with diagnostics:
    invalid line join
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.1Mb
      sub-directories of 1Mb or more:
        doc        6.8Mb
        examples   3.7Mb
    ```

# ghibli

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# gitter

Version: 1.1.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘EBImage’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# GJRM

Version: 0.1-3

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘sp’
    ```

# glmmTMB

Version: 0.1.4

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning in f(x, order = 1) : value out of range in 'lgamma'
    Warning in f(x, order = 1) : value out of range in 'lgamma'
    Warning in f(x, order = 1) : value out of range in 'lgamma'
    Warning in f(x, order = 1) : value out of range in 'lgamma'
    Warning in f(x, order = 1) : value out of range in 'lgamma'
    Warning in f(x, order = 1) : value out of range in 'lgamma'
    Warning in f(x, order = 1) : value out of range in 'lgamma'
    Loading required package: stats4
    Warning in qt((1 - level)/2, df) : NaNs produced
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'glmmTMB.tex' failed.
    BibTeX errors:
    The top-level auxiliary file: glmmTMB.aux
    I couldn't open style file chicago.bst
    ---line 19 of file glmmTMB.aux
     : \bibstyle{chicago
     :                  }
    I'm skipping whatever remains of this command
    I found no style file---while reading file glmmTMB.aux
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 44.4Mb
      sub-directories of 1Mb or more:
        libs  43.4Mb
    ```

# gmum.r

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 20.9Mb
      sub-directories of 1Mb or more:
        libs  19.9Mb
    ```

# GOexpress

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    GO_analyse: no visible binding for global variable ‘microarray2dataset’
    GO_analyse: no visible binding for global variable ‘prefix2dataset’
    mart_from_ensembl: no visible binding for global variable
      ‘prefix2dataset’
    Undefined global functions or variables:
      microarray2dataset prefix2dataset
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘VennDiagram’
    ```

# GoogleGenomics

Version: 1.8.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘GoogleGenomics-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getReads
    > ### Title: Get reads from Google Genomics.
    > ### Aliases: getReads
    > 
    > ### ** Examples
    > 
    > # Authenticated on package load from the env variable GOOGLE_API_KEY.
    > reads <- getReads()
    Error in getSearchPage("reads", body, fields, pageToken) : 
      You are not authenticated; see ?GoogleGenomics::authenticate.
    Calls: getReads -> getReadsPage -> getSearchPage
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'AnnotatingVariants.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# googlesheets

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(googlesheets)
      > 
      > if (identical(tolower(Sys.getenv("NOT_CRAN")), "true")) {
      +   test_check("googlesheets")
      + }
      Error: Cannot read token from alleged .rds file:
      googlesheets_token.rds
      testthat results ================================================================
      OK: 2 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 37-41 (basic-usage.Rmd) 
    Error: processing vignette 'basic-usage.Rmd' failed with diagnostics:
    Cannot read token from alleged .rds file:
    ../tests/testthat/googlesheets_token.rds
    Execution halted
    ```

# GOsummaries

Version: 2.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    panel_boxplot: no visible binding for global variable ‘x’
    panel_boxplot: no visible binding for global variable ‘y’
    panel_boxplot: no visible binding for global variable ‘Class’
    panel_dummy: no visible binding for global variable ‘y’
    panel_dummy: no visible binding for global variable ‘x’
    plotWordcloud: no visible global function definition for
      ‘colorRampPalette’
    plotWordcloud: no visible global function definition for ‘runif’
    plot_motor: no visible global function definition for ‘dev.off’
    print.gosummaries: no visible global function definition for ‘head’
    pspearman: no visible global function definition for ‘pt’
    spearman_mds: no visible global function definition for ‘cor’
    Undefined global functions or variables:
      Class bmp boxplot.stats colorRampPalette cor dev.off head jpeg
      packageVersion pdf png pt runif tiff x y
    Consider adding
      importFrom("grDevices", "bmp", "boxplot.stats", "colorRampPalette",
                 "dev.off", "jpeg", "pdf", "png", "tiff")
      importFrom("stats", "cor", "pt", "runif")
      importFrom("utils", "head", "packageVersion")
    to your NAMESPACE file.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘BiocStyle’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error in re-building vignettes:
      ...
    
    Error: processing vignette 'GOsummaries-basics.Rnw' failed with diagnostics:
     chunk 1 (label = style-Sweave) 
    Error in loadNamespace(name) : there is no package called ‘BiocStyle’
    Execution halted
    ```

# GOTHiC

Version: 1.12.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    ':::' call which should be '::': ‘S4Vectors:::orderIntegerPairs’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    .getRestrictionSitesFromBSgenome: no visible global function definition
      for ‘seqlengths’
    .importHicup: no visible global function definition for ‘read.table’
    .onlyPairing: no visible global function definition for ‘read.table’
    .onlyPairing: no visible global function definition for ‘ScanBamParam’
    GOTHiC: no visible binding for global variable
      ‘BSgenome.Hsapiens.UCSC.hg19’
    GOTHiC: no visible binding for global variable ‘filtered’
    GOTHiC: no visible binding for global variable ‘interactingLoci’
    mapReadsToRestrictionSites: no visible binding for global variable
      ‘resGR’
    Undefined global functions or variables:
      BSgenome.Hsapiens.UCSC.hg19 IntervalTree ScanBamParam V1 binom.test
      biocLite chr1 chr2 dev.off filtered frequencies int1 int2
      interactingLoci isCircular locus1 locus2 p.adjust pdf pvalue
      read.table resGR seqlengths x11
    Consider adding
      importFrom("grDevices", "dev.off", "pdf", "x11")
      importFrom("stats", "binom.test", "p.adjust")
      importFrom("utils", "read.table")
    to your NAMESPACE file.
    ```

# gpmap

Version: 0.1.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    Checking should be performed on sources prepared by ‘R CMD build’.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘foreach’ ‘ggplot2’ ‘isotone’ ‘plyr’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plot3_dec: no visible global function definition for ‘foreach’
    plot3_dec: no visible global function definition for ‘ggplot’
    plot3_dec: no visible global function definition for ‘aes_string’
    plot3_dec: no visible global function definition for ‘geom_line’
    plot3_dec: no visible global function definition for ‘facet_grid’
    plot3_dec: no visible global function definition for ‘as.formula’
    plot3_dec: no visible global function definition for ‘labs’
    plot3_orig: no visible global function definition for ‘%do%’
    plot3_orig: no visible global function definition for ‘foreach’
    plot3_orig: no visible global function definition for ‘ggplot’
    plot3_orig: no visible global function definition for ‘aes_string’
    plot3_orig: no visible global function definition for ‘geom_line’
    plot3_orig: no visible global function definition for ‘facet_wrap’
    plot3_orig: no visible global function definition for ‘as.formula’
    plot3_orig: no visible global function definition for ‘labs’
    Undefined global functions or variables:
      %do% %dopar% aaply activeSet aes_string as.formula facet_grid
      facet_wrap foreach geom_line geom_point ggplot labs laply var
    Consider adding
      importFrom("stats", "as.formula", "var")
    to your NAMESPACE file.
    ```

# gQTLstats

Version: 1.8.0

## In both

*   checking whether the package can be unloaded cleanly ... WARNING
    ```
    Error: package or namespace load failed for ‘gQTLstats’:
     .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    ```

*   checking whether the namespace can be loaded with stated dependencies ... WARNING
    ```
    Error: .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    ...
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking replacement functions ... WARNING
    ```
    ...
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    The argument of a replacement function which corresponds to the right
    hand side must be named ‘value’.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
    Call sequence:
    6: stop(msg, call. = FALSE, domain = NA)
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 65.8Mb
      sub-directories of 1Mb or more:
        data        11.0Mb
        doc          1.1Mb
        registries  18.9Mb
        vcf         33.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    ...
    Call sequence:
    6: stop(msg, call. = FALSE, domain = NA)
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    ```

*   checking foreign function calls ... NOTE
    ```
    ...
    5: value[[3L]](cond)
    4: tryCatchOne(expr, names, parentenv, handlers[[1L]])
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    Error: .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘MultiAssayExperiment’
    ```

*   checking Rd \usage sections ... NOTE
    ```
    ...
    3: tryCatchList(expr, classes, parentenv, handlers)
    2: tryCatch({
           attr(package, "LibPath") <- which.lib.loc
           ns <- loadNamespace(package, lib.loc)
           env <- attachNamespace(ns, pos = pos, deps)
       }, error = function(e) {
           P <- if (!is.null(cc <- conditionCall(e))) 
               paste(" in", deparse(cc)[1L])
           else ""
           msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
               sQuote(package), P, conditionMessage(e))
           if (logical.return) 
               message(paste("Error:", msg), domain = NA)
           else stop(msg, call. = FALSE, domain = NA)
       })
    1: library(package, lib.loc = lib.loc
    Execution halted
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 8 marked Latin-1 strings
      Note: found 12 marked UTF-8 strings
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' calls not declared from:
      ‘TxDb.Hsapiens.UCSC.hg19.knownGene’ ‘org.Hs.eg.db’
    ```

# grattan

Version: 1.5.2.5

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘taxstats’
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# Greg

Version: 1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rmeta’
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'printCrudeAndAdjustedModel':
      ‘rbind.printCrudeAndAdjusted’ ‘cbind.printCrudeAndAdjusted’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# GRENITS

Version: 1.28.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        libs   8.0Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘Rcpp’ ‘RcppArmadillo’ ‘ggplot2’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘scale_fill_gradient’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘theme’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘element_blank’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘element_text’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘ggtitle’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘labs’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘scale_x_discrete’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘scale_y_discrete’
    .plotDistribParents.LargeMat: no visible global function definition for
      ‘facet_wrap’
    Undefined global functions or variables:
      GeneNames Var1 Var2 aes element_blank element_text facet_wrap
      geom_tile ggplot ggtitle labs scale_fill_gradient scale_x_discrete
      scale_y_discrete theme value variable
    ```

# greport

Version: 0.7-1

## In both

*   checking R code for possible problems ... NOTE
    ```
    accrualReport: multiple local function definitions for ‘gg’ with
      different formal arguments
    ```

# gridsampler

Version: 0.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘BiasedUrn’
      All declared Imports should be used.
    ```

# GRmetrics

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'GRmetrics-vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# growcurves

Version: 0.2.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 18.5Mb
      sub-directories of 1Mb or more:
        libs  18.0Mb
    ```

# growfunctions

Version: 0.13

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.2Mb
      sub-directories of 1Mb or more:
        libs  12.8Mb
    ```

# GSCA

Version: 2.6.0

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘shiny’ ‘sp’ ‘gplots’ ‘ggplot2’ ‘reshape2’ ‘RColorBrewer’ ‘rhdf5’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘Affyhgu133A2Expr’ ‘Affyhgu133Plus2Expr’ ‘Affyhgu133aExpr’
      ‘Affymoe4302Expr’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    GSCAplot: no visible global function definition for ‘title’
    GSCAplot: no visible global function definition for ‘dev.off’
    annotatePeaks: no visible binding for global variable ‘allreffile’
    tabSearch: no visible global function definition for ‘data’
    tabSearch: no visible binding for global variable ‘Affyhgu133aExprtab’
    tabSearch: no visible binding for global variable ‘Affymoe4302Exprtab’
    tabSearch: no visible binding for global variable ‘Affyhgu133A2Exprtab’
    tabSearch: no visible binding for global variable
      ‘Affyhgu133Plus2Exprtab’
    Undefined global functions or variables:
      Affyhgu133A2Exprtab Affyhgu133Plus2Exprtab Affyhgu133aExprtab
      Affymoe4302Exprtab P.value SampleType Var1 Var2 allreffile
      colorRampPalette data dev.off fisher.test geneid hist par pdf qnorm
      quantile sd str t.stat t.test title value variable write.csv
      write.table
    Consider adding
      importFrom("grDevices", "colorRampPalette", "dev.off", "pdf")
      importFrom("graphics", "hist", "par", "title")
      importFrom("stats", "fisher.test", "qnorm", "quantile", "sd", "t.test")
      importFrom("utils", "data", "str", "write.csv", "write.table")
    to your NAMESPACE file.
    ```

# gsDesign

Version: 3.0-1

## In both

*   checking R code for possible problems ... NOTE
    ```
    plotgsCP: no visible global function definition for ‘opts’
    plotgsPower: no visible global function definition for ‘opts’
    qplotit: no visible global function definition for ‘opts’
    Undefined global functions or variables:
      opts
    ```

# GSE

Version: 4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        libs   4.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rrcov’
      All declared Imports should be used.
    ```

# GUIgems

Version: 0.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rpanel’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# Guitar

Version: 1.14.0

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘Rsamtools’ ‘GenomicFeatures’ ‘rtracklayer’ ‘GenomicAlignments’
      ‘GenomicRanges’ ‘ggplot2’ ‘grid’ ‘IRanges’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        extdata   4.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    .makeFigure_fill: no visible global function definition for ‘pdf’
    .makeFigure_fill: no visible global function definition for ‘dev.off’
    .makeFigure_nofill: no visible global function definition for ‘pdf’
    .makeFigure_nofill: no visible global function definition for ‘dev.off’
    BED12toGRangesList: no visible global function definition for
      ‘read.table’
    BED12toGRangesList: no visible global function definition for ‘head’
    narrowPeaktoGRanges: no visible global function definition for
      ‘read.table’
    Undefined global functions or variables:
      dev.off head pdf read.table
    Consider adding
      importFrom("grDevices", "dev.off", "pdf")
      importFrom("utils", "head", "read.table")
    to your NAMESPACE file.
    ```

# gutenbergr

Version: 0.1.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13617 marked UTF-8 strings
    ```

# gwascat

Version: 2.8.0

## In both

*   checking examples ... ERROR
    ```
    ...
    
    The following object is masked from 'package:DelayedArray':
    
        type
    
    The following object is masked from 'package:base':
    
        strsplit
    
    
    Attaching package: 'VariantAnnotation'
    
    The following object is masked from 'package:base':
    
        tabulate
    
    > expath = dir(system.file("vcf", package="GGtools"), patt=".*exon.*gz$", full=TRUE)
    > tf = TabixFile(expath)
    Error: TabixFile: file(s) do not exist:
      '.tbi'
    Execution halted
    ```

*   checking whether the namespace can be loaded with stated dependencies ... WARNING
    ```
    Error: .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘SNPlocs.Hsapiens.dbSNP144.GRCh37’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 38.8Mb
      sub-directories of 1Mb or more:
        data     30.9Mb
        obo       3.1Mb
        olddata   2.2Mb
        tab       1.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    Error: .onLoad failed in loadNamespace() for 'Homo.sapiens', details:
      call: loadMethod(structure(function (object) 
      error: could not find function "loadMethod"
    Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked Latin-1 strings
      Note: found 1 marked UTF-8 string
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘BiocStyle’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    gwascat loaded.  Use data(ebicat38) for hg38 coordinates;
     data(ebicat37) for hg19 coordinates.
    Loading required package: ggplot2
    Need specific help about ggbio? try mailing 
     the maintainer or visit http://tengfei.github.com/ggbio/
    
    Attaching package: 'ggbio'
    
    The following objects are masked from 'package:ggplot2':
    
        geom_bar, geom_rect, geom_segment, ggsave, stat_bin, stat_identity,
        xlim
    
    Loading required package: grid
    'select()' returned 1:1 mapping between keys and columns
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 18-22 (gwascatOnt.Rmd) 
    Error: processing vignette 'gwascatOnt.Rmd' failed with diagnostics:
    there is no package called 'BiocStyle'
    Execution halted
    ```

# h2o

Version: 3.14.0.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 86.4Mb
      sub-directories of 1Mb or more:
        java  85.1Mb
    ```

# h5vc

Version: 2.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    callVariantsSingle : <anonymous> : <anonymous>: no visible binding for
      global variable ‘BlockID’
    mergeTallyFiles : <anonymous>: no visible binding for global variable
      ‘group’
    mergeTallyFiles: no visible binding for global variable ‘SourceFile’
    mismatchPlot: no visible binding for global variable ‘Sample’
    plotMutationSpectrum: no visible binding for global variable
      ‘altAllele’
    plotMutationSpectrum: no visible binding for global variable ‘tmp’
    rerunBatchTallies: no visible binding for global variable ‘regID’
    resizeCohort: no visible binding for global variable ‘newSamples’
    tallyRangesBatch : <anonymous>: no visible binding for global variable
      ‘bamFiles’
    tallyRangesBatch: no visible binding for global variable ‘verbose’
    Undefined global functions or variables:
      AF BlockID Sample SourceFile SupFwd SupRev Support altAllele bamFiles
      binom.test fisher.test group hist newSamples pValue regID tmp verbose
    Consider adding
      importFrom("graphics", "hist")
      importFrom("stats", "binom.test", "fisher.test")
    to your NAMESPACE file.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘deepSNV’
    ```

# harrietr

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# hazus

Version: 0.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    extract_hazus_functions: no visible global function definition for
      ‘data’
    Undefined global functions or variables:
      data
    Consider adding
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# hBayesDM

Version: 0.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstantools’
      All declared Imports should be used.
    ```

# hdr

Version: 0.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘hdr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: get_data
    > ### Title: Fetch data from the UNDP Human Development Report
    > ### Aliases: get_data
    > 
    > ### ** Examples
    > 
    > # Get the Human Development Index for Germany in 2013
    > df <- get_data(indicator = 137506, country = "DEU", year = 2013)
    Error in curl::curl_fetch_memory(url, handle = handle) : 
      Failed to connect to ec2-52-1-168-42.compute-1.amazonaws.com port 80: Connection refused
    Calls: get_data ... request_fetch -> request_fetch.write_memory -> <Anonymous> -> .Call
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 31-33 (undp_hdr.Rmd) 
    Error: processing vignette 'undp_hdr.Rmd' failed with diagnostics:
    Failed to connect to ec2-52-1-168-42.compute-1.amazonaws.com port 80: Connection refused
    Execution halted
    ```

# heatmaply

Version: 0.13.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘d3heatmap’
    ```

# hexSticker

Version: 0.4.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘ggimage’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# hiAnnotator

Version: 1.11.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    makeChunks: no visible global function definition for ‘breakInChunks’
    makeChunks: no visible global function definition for ‘detectCores’
    makeChunks : <anonymous>: no visible global function definition for
      ‘keepSeqlevels’
    makeChunks : <anonymous>: no visible global function definition for
      ‘seqlevelsInUse’
    makeGRanges: no visible global function definition for ‘IRanges’
    makeGRanges: no visible global function definition for ‘seqlengths’
    makeGRanges: no visible global function definition for ‘seqlevels<-’
    makeGRanges: no visible global function definition for ‘sortSeqlevels’
    makeGRanges: no visible global function definition for ‘seqlevelsInUse’
    makeGRanges: no visible global function definition for ‘seqlengths<-’
    makeGRanges: no visible global function definition for ‘seqlevels’
    Undefined global functions or variables:
      IRanges breakInChunks countQueryHits detectCores dist featureName
      keepSeqlevels mid n overlapsAny qStrand queryHits seqlengths
      seqlengths<- seqlevels seqlevels<- seqlevelsInUse sortSeqlevels
      subjectHits
    Consider adding
      importFrom("stats", "dist")
    to your NAMESPACE file.
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# hierarchicalDS

Version: 2.9

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    rrw: no visible global function definition for ‘rnorm’
    simulate_data: no visible global function definition for ‘pnorm’
    simulate_data: no visible global function definition for ‘rpois’
    switch_pdf: no visible global function definition for ‘dpois’
    switch_pdf: no visible global function definition for ‘dnorm’
    switch_sample: no visible global function definition for ‘rpois’
    switch_sample: no visible global function definition for ‘rnorm’
    switch_sample: no visible global function definition for ‘runif’
    switch_sample_prior: no visible global function definition for ‘rgamma’
    switch_sample_prior: no visible global function definition for ‘rnorm’
    table.mcmc: no visible global function definition for ‘write.csv’
    Undefined global functions or variables:
      abline dnorm dpois legend median model.matrix optimize par plot pnorm
      points rgamma rnorm rpois runif var write.csv
    Consider adding
      importFrom("graphics", "abline", "legend", "par", "plot", "points")
      importFrom("stats", "dnorm", "dpois", "median", "model.matrix",
                 "optimize", "pnorm", "rgamma", "rnorm", "rpois", "runif",
                 "var")
      importFrom("utils", "write.csv")
    to your NAMESPACE file.
    ```

# hierarchicalSets

Version: 1.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘ggplot2:::build_guides’ ‘ggplot2:::guide_axis’
      ‘ggplot2:::scales_list’
      See the note in ?`:::` about the use of this operator.
    ```

# highcharter

Version: 0.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 16.5Mb
      sub-directories of 1Mb or more:
        doc          13.7Mb
        htmlwidgets   1.9Mb
    ```

# HighDimOut

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    Func.ABOD: no visible global function definition for ‘combn’
    Func.ABOD: no visible global function definition for ‘var’
    Func.SOD: no visible global function definition for ‘var’
    Func.trans : erf: no visible global function definition for ‘pnorm’
    Func.trans: no visible global function definition for ‘sd’
    Undefined global functions or variables:
      combn pnorm sd var
    Consider adding
      importFrom("stats", "pnorm", "sd", "var")
      importFrom("utils", "combn")
    to your NAMESPACE file.
    ```

# HistData

Version: 0.8-2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘psych’, ‘Guerry’, ‘alr3’, ‘agridat’, ‘coin’
    ```

# HMP

Version: 1.5

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘genalg’
    ```

# horserule

Version: 0.1.0

## In both

*   checking whether package ‘horserule’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: No Rd macros in package 'Rdpack'.
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/horserule/new/horserule.Rcheck/00install.out’ for details.
    ```

*   checking Rd files ... WARNING
    ```
    Warning: No Rd macros in package 'Rdpack'.
    prepare_Rd: HorseRuleFit.Rd:75: unknown macro '\insertRef'
    prepare_Rd: hs.Rd:33: unknown macro '\insertRef'
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘MASS’
      All declared Imports should be used.
    ```

# HRM

Version: 0.7.4

## In both

*   checking whether package ‘HRM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/HRM/new/HRM.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    
    (R:95155): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    ```

# HTSSIP

Version: 1.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Sparsity threshold: 0 
      Density window: 1.72-1.75 
      Sparsity threshold: 0.2 
      Density window: 1.72-1.75 
      Sparsity threshold with the most rejected hypotheses: 0 
      testthat results ================================================================
      OK: 189 SKIPPED: 0 FAILED: 5
      1. Error: Beta diversity from a list of phyloseq objects (@test-BD_ordinations.R#2) 
      2. Error: Beta diversity from a list of phyloseq objects (parallel) (@test-BD_ordinations.R#9) 
      3. Error: Make a data.frame for ordination plotting (parallel) (@test-BD_ordinations.R#20) 
      4. Error: Plots created from phyloseq object (@test-BD_ordinations.R#46) 
      5. Error: Plot comparing all (@test-BD_ordinations.R#70) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    converting counts to integer mode
    Quitting from lines 68-72 (beta_diversity_ordinations.Rmd) 
    Error: processing vignette 'beta_diversity_ordinations.Rmd' failed with diagnostics:
    Incorrect number of arguments (7), expecting 5 for 'node_depth_edgelength'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘igraph’
      All declared Imports should be used.
    ```

# httk

Version: 1.7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.8Mb
      sub-directories of 1Mb or more:
        data  12.3Mb
        doc    1.0Mb
    ```

# hurricaneexposure

Version: 0.0.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘hurricaneexposure-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: default_map
    > ### Title: Create a default map with eastern US states
    > ### Aliases: default_map
    > 
    > ### ** Examples
    > 
    > default_map()
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Theme element panel.border missing
    Error in if (theme$panel.ontop) { : argument is of length zero
    Calls: <Anonymous> ... print.ggplot -> ggplot_gtable -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

# huxtable

Version: 1.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: withCallingHandlers(withVisible(code), warning = handle_warning, message = handle_message)
      5: withVisible(code)
      6: rmarkdown::render("table-tester-2.Rmd", quiet = TRUE, output_format = "pdf_document")
      7: convert(output_file, run_citeproc)
      8: pandoc_convert(utf8_input, pandoc_to, output_format$pandoc$from, output, citeproc, 
             output_format$pandoc$args, !quiet)
      9: stop("pandoc document conversion failed with error ", result, call. = FALSE)
      
      testthat results ================================================================
      OK: 289 SKIPPED: 48 FAILED: 2
      1. Error: Row heights do not screw up LaTeX multicol (@test-with-pandoc.R#20) 
      2. Error: table-tester-2.Rmd renders without errors in LaTeX (@test-with-pandoc.R#27) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘xtable’
    ```

# hyperSpec

Version: 0.99-20171005

## In both

*   checking R code for possible problems ... NOTE
    ```
    Warning: local assignments to syntactic functions: ~
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘caTools’
    ```

# IAPWS95

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘ggplot2’ ‘pander’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

# iBMQ

Version: 1.16.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘Biobase’ ‘ggplot2’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    eqtlMcmc: no visible global function definition for ‘is’
    eqtlMcmc: no visible global function definition for ‘exprs’
    Undefined global functions or variables:
      exprs is
    Consider adding
      importFrom("methods", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking compilation flags in Makevars ... NOTE
    ```
    Package has both ‘src/Makevars.in’ and ‘src/Makevars’.
    Installation with --no-configure' is unlikely to work.  If you intended
    ‘src/Makevars’ to be used on Windows, rename it to ‘src/Makevars.win’
    otherwise remove it.  If ‘configure’ created ‘src/Makevars’, you need a
    ‘cleanup’ script.
    ```

# icd9

Version: 1.3.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        libs   4.6Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 14 marked Latin-1 strings
      Note: found 39 marked UTF-8 strings
    ```

# ICtest

Version: 0.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘fICA’
    ```

# ideal

Version: 1.0.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ideal: no visible binding for '<<-' assignment to ‘ideal_env’
    ideal : <anonymous>: no visible binding for global variable ‘airway’
    ideal : <anonymous>: no visible binding for global variable ‘ideal_env’
    Undefined global functions or variables:
      airway ideal_env
    ```

# IGM.MEA

Version: 0.3.5

## In both

*   checking whether package ‘IGM.MEA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/IGM.MEA/new/IGM.MEA.Rcheck/00install.out’ for details.
    ```

# IGP

Version: 0.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘PythonInR’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# IHW

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        intersect, setdiff, setequal, union
    
    estimating size factors
    estimating dispersions
    gene-wise dispersion estimates
    mean-dispersion relationship
    final dispersion estimates
    fitting model and testing
    
    Attaching package: 'ggplot2'
    
    The following object is masked from 'package:IHW':
    
        alpha
    
    Warning: Removed 30633 rows containing missing values (geom_point).
    Warning: Removed 12861 rows containing missing values (geom_point).
    Error: processing vignette 'introduction_to_ihw.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    ihw.default: no visible global function definition for ‘p.adjust’
    ihw_convex: no visible global function definition for ‘gurobi’
    ihw_internal: no visible global function definition for ‘p.adjust’
    ihw_milp: no visible global function definition for ‘str’
    ihw_milp: no visible global function definition for ‘gurobi’
    plot_decisionboundary: no visible binding for global variable ‘stratum’
    plot_decisionboundary: no visible binding for global variable
      ‘covariate’
    plot_decisionboundary: no visible binding for global variable ‘pvalue’
    plot_decisionboundary: no visible binding for global variable ‘fold’
    thresholds_ihwResult: no visible global function definition for
      ‘na.exclude’
    thresholds,ihwResult: no visible global function definition for
      ‘na.exclude’
    Undefined global functions or variables:
      covariate fold gurobi mcols mcols<- metadata metadata<- na.exclude
      p.adjust pvalue runif str stratum
    Consider adding
      importFrom("stats", "na.exclude", "p.adjust", "runif")
      importFrom("utils", "str")
    to your NAMESPACE file.
    ```

# IHWpaper

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'BH-explanation.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 14.9Mb
      sub-directories of 1Mb or more:
        doc       3.4Mb
        extdata   9.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    scott_fdrreg: no visible global function definition for ‘FDRreg’
    scott_fdrreg: no visible global function definition for ‘getFDR’
    sim_fun_eval: no visible binding for global variable ‘fdr_method’
    sim_fun_eval: no visible binding for global variable ‘fdr_pars’
    sim_fun_eval: no visible binding for global variable ‘FDP’
    sim_fun_eval: no visible binding for global variable ‘rj_ratio’
    sim_fun_eval: no visible binding for global variable ‘FPR’
    sim_fun_eval: no visible binding for global variable ‘FWER’
    Undefined global functions or variables:
      FDP FDRreg FPR FWER fdr_method fdr_pars getFDR rj_ratio
    ```

# imager

Version: 0.40.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘spatstat’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 43.7Mb
      sub-directories of 1Mb or more:
        data      1.4Mb
        doc       4.9Mb
        extdata   1.0Mb
        include   2.8Mb
        libs     33.0Mb
    ```

# Imetagene

Version: 1.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'imetagene.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# ImmuneSpaceR

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    A .netrc file is required to connect to ImmuneSpace. For more information on how to create one, refer to the Configuration section of the introduction vignette.
    Quitting from lines 71-74 (Using_RImmuneSpace.Rmd) 
    Error: processing vignette 'Using_RImmuneSpace.Rmd' failed with diagnostics:
    HTTP request was unsuccessful. Status code = 401, Error message = NA 401
    Execution halted
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    File ‘ImmuneSpaceR/R/zzz.R’:
      .onLoad calls:
        packageStartupMessage("A .netrc file is required to connect to ImmuneSpace. For more information on how to create one, refer to the Configuration section of the introduction vignette.")
    
    See section ‘Good practice’ in '?.onAttach'.
    
    CreateConnection: no visible global function definition for
      ‘packageVersion’
    Undefined global functions or variables:
      packageVersion
    Consider adding
      importFrom("utils", "packageVersion")
    to your NAMESPACE file.
    ```

# ImpulseDE2

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    plotGenes: no visible global function definition for ‘error’
    plotGenes: no visible binding for global variable ‘normCounts’
    plotGenes: no visible binding for global variable ‘Condition’
    plotGenes: no visible binding for global variable ‘Batch’
    plotGenes: no visible binding for global variable ‘value’
    plotGenes: no visible binding for global variable ‘BatchFit’
    Undefined global functions or variables:
      Batch BatchFit Condition error normCounts value
    ```

# imputeTestbench

Version: 3.0.1

## Newly broken

*   checking whether package ‘imputeTestbench’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘forecast::autolayer’ by ‘ggplot2::autolayer’ when loading ‘imputeTestbench’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/imputeTestbench/new/imputeTestbench.Rcheck/00install.out’ for details.
    ```

# incidence

Version: 1.2.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +  ## plot data and model (recommended)
    +  plot(i.7, fit=f)
    +  plot(i.7[1:25], fit=f)
    + 
    + 
    + ## EXAMPLE WITH 2 PHASES
    +  ## specifying the peak manually
    +  f2 <- fit(i.7, split=as.Date("2014-10-15"))
    +  f2
    +  plot(i.7, fit=f2)
    + 
    + ## finding the best 'peak' date
    + f3 <- fit_optim_split(i.7)
    + f3
    + plot(i.7, fit=f3$fit)
    + }
    Loading required package: outbreaks
    Error in as.Date.default(x.major_source, origin = "1970-01-01") : 
      do not know how to convert 'x.major_source' to class “Date”
    Calls: plot -> plot.incidence -> as.Date -> as.Date.default
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1: plot(i.isoweek) at testthat/test-plot.R:32
      2: plot.incidence(i.isoweek)
      3: as.Date(x.major_source, origin = "1970-01-01")
      4: as.Date.default(x.major_source, origin = "1970-01-01")
      5: stop(gettextf("do not know how to convert '%s' to class %s", deparse(substitute(x)), 
             dQuote("Date")), domain = NA)
      
      testthat results ================================================================
      OK: 213 SKIPPED: 0 FAILED: 3
      1. Failure: fit_optim_split (@test-fit.R#34) 
      2. Failure: fit_optim_split (@test-fit.R#35) 
      3. Error: plot for incidence object (@test-plot.R#32) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 80-86 (conversions.Rmd) 
    Error: processing vignette 'conversions.Rmd' failed with diagnostics:
    do not know how to convert 'x.major_source' to class "Date"
    Execution halted
    ```

# InformationValue

Version: 1.2.3

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    BugReports field should be the URL of a single webpage
    ```

# inlabru

Version: 2.1.2

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘inlabru-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: cprod
    > ### Title: Cross product of integration points
    > ### Aliases: cprod
    > 
    > ### ** Examples
    > 
    > 
    > # Create integration points in dimension 'myDim' and 'myDiscreteDim' 
    > ips1 = ipoints(c(0,8), name = "myDim")
    Error in loadNamespace(name) : there is no package called ‘INLA’
    Calls: ipoints ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘INLA’
    ```

# INLAutils

Version: 0.0.4

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘INLA’
    ```

# IntClust

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘prodlim’
      All declared Imports should be used.
    ```

# interactiveDisplay

Version: 1.14.0

## In both

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘.interactiveDisplayBase’
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘rstudio’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      for global variable ‘Var1’
    display,RangedSummarizedExperiment : <anonymous>: no visible binding
      for global variable ‘Var2’
    display,RangedSummarizedExperiment : <anonymous>: no visible binding
      for global variable ‘value’
    Undefined global functions or variables:
      AnnotationTrack GO.db GRanges GRangesList GenomeAxisTrack IRanges
      IdeogramTrack MRcounts Var1 Var2 as.dendrogram assays cim
      colorRampPalette coord_flip cutree dendrapply dev.off dist
      elementMetadata experimentData exprs fData hclust installed.packages
      is.leaf layout_circle legend libSize mcols pData plot plotFeature
      plotOrd plotTracks png rainbow ranges rowRanges seqlengths
      seqlengths<- seqlevels<- seqnames ucscGenomes value
    Consider adding
      importFrom("grDevices", "colorRampPalette", "dev.off", "png",
                 "rainbow")
      importFrom("graphics", "legend", "plot")
      importFrom("stats", "as.dendrogram", "cutree", "dendrapply", "dist",
                 "hclust", "is.leaf")
      importFrom("utils", "installed.packages")
    to your NAMESPACE file.
    ```

# IONiseR

Version: 2.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    The following object is masked from 'package:base':
    
        apply
    
    
    Attaching package: 'GenomicAlignments'
    
    The following object is masked from 'package:dplyr':
    
        last
    
    
    Attaching package: 'ShortRead'
    
    The following object is masked from 'package:dplyr':
    
        id
    
    Error: processing vignette 'IONiseR.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        doc       3.6Mb
        extdata   1.5Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘start_time’
    readFast5Summary.mc: no visible binding for global variable ‘duration’
    readFast5Summary.mc: no visible binding for global variable
      ‘num_events’
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘baseCalledTemplate’
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘baseCalledComplement’
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘component’
    [,Fast5Summary-ANY-ANY-ANY: no visible binding for global variable
      ‘idx’
    show,Fast5Summary: no visible binding for global variable ‘full_2D’
    show,Fast5Summary: no visible binding for global variable ‘pass’
    Undefined global functions or variables:
      := AAAAA TTTTT accumulation baseCalledComplement baseCalledTemplate
      bases_called category channel circleFun component duration error freq
      full_2D group hour idx matrixCol matrixRow meanZValue mean_value
      median_signal minute mux name nbases new_reads num_events oddEven
      pass pentamer rbindlist readIDs seq_length start_time time_bin
      time_group x y zvalue
    ```

# ipft

Version: 0.7.1

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ipft-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ipfPlotLoc
    > ### Title: Plots the spatial location of the observations
    > ### Aliases: ipfPlotLoc
    > 
    > ### ** Examples
    > 
    > 
    >     ipfPlotLoc(ipftrain[, 169:170])
    > 
    >     ipfPlotLoc(ipftrain[, 169:170], plabel = TRUE, reverseAxis = TRUE,
    +                title = 'Position of training set observations')
    Error: Columns `x`, `y` must be 1d atomic vectors or lists
    Execution halted
    ```

# iprior

Version: 0.7.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.1Mb
      sub-directories of 1Mb or more:
        doc    1.1Mb
        libs   8.7Mb
    ```

# iqspr

Version: 2.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        data   6.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘gridExtra’ ‘rBayesianOptimization’
      All declared Imports should be used.
    ```

# isobar

Version: 1.22.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘.as.matrix’ ‘.as.vect’ ‘.convertPeptideModif’
      ‘.proteinGroupAsConciseDataFrame’ ‘.read.idfile’ ‘.sum.bool’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    coerce,IBSpectra-MSnSet: no visible binding for global variable ‘o’
    coerce,MSnSet-IBSpectra: no visible global function definition for
      ‘qual’
    df,Tlsd: no visible global function definition for ‘param’
    estimateRatio,IBSpectra-ANY-missing-missing-character-missing: no
      visible binding for global variable ‘i’
    estimateRatio,IBSpectra-ANY-missing-missing-missing-character: no
      visible binding for global variable ‘i’
    estimateRatioNumeric,numeric-numeric-NoiseModel: no visible binding for
      global variable ‘center.var’
    location,Tlsd: no visible global function definition for ‘param’
    plotRatio,IBSpectra-character-character-character: no visible binding
      for global variable ‘pch’
    plotRatio,IBSpectra-character-character-character: no visible binding
      for global variable ‘noise.model.col’
    plotRatio,IBSpectra-character-character-character: no visible binding
      for global variable ‘pch.p’
    scale,Tlsd: no visible global function definition for ‘param’
    Undefined global functions or variables:
      center.var d g i mz name noise.model.col o param pch pch.p peptide
      qual ratio type
    ```

# isomiRs

Version: 1.4.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
      1 Test Suite : 
      isomiRs RUnit Tests - 3 test functions, 0 errors, 1 failure
      FAILURE in test_plotFunctions: Error in checkTrue(class(isoPlot(mirData, type = "iso5", column = "group"))[1] ==  : 
        Test not TRUE
      
      
      Test files with failing tests
      
         test_singleFunctions.R 
           test_plotFunctions 
      
      
      Error in BiocGenerics:::testPackage("isomiRs") : 
        unit tests failed for package isomiRs
      Execution halted
    ```

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    isoSelect.IsomirDataSeq: no visible binding for global variable ‘freq’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘mir’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘mism’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘add’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘t5’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘t3’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘id’
    isoSelect,IsomirDataSeq : <anonymous>: no visible binding for global
      variable ‘freq’
    isoSelect,IsomirDataSeq: no visible binding for global variable ‘freq’
    Undefined global functions or variables:
      Count DB X1 X2 add af ambiguity average change condition current
      enrich enrichGO error freq gene go group id mir mir_f mir_n mism
      mism_f mism_n ngene pct_abundance reference rowMax rowMin sel_genes
      t3 t5 term term_short type value y
    ```

# ITGM

Version: 0.6

## In both

*   checking whether package ‘ITGM’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/ITGM/new/ITGM.Rcheck/00install.out’ for details.
    ```

# jcolors

Version: 0.0.1

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
    Error in if (theme$panel.ontop) { : argument is of length zero
    Calls: <Anonymous> ... print.ggplot -> ggplot_gtable -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

# jmv

Version: 0.8

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BayesFactor’ ‘GGally’ ‘GPArotation’ ‘PMCMR’ ‘R6’ ‘afex’ ‘car’
      ‘ggridges’ ‘lavaan’ ‘lsmeans’ ‘multcomp’ ‘mvnormtest’ ‘psych’ ‘vcd’
      ‘vcdExtra’
      All declared Imports should be used.
    ```

# jmvcore

Version: 0.8.1.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RProtoBuf’
    ```

# joineRML

Version: 0.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'technical.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `biblatex.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.17 ^^M
            
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        data   1.3Mb
        libs   4.9Mb
    ```

# jpndistrict

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 188 marked UTF-8 strings
    ```

# jtools

Version: 0.9.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘effects’, ‘ReporteRs’, ‘arm’, ‘rockchalk’, ‘pequod’, ‘MuMIn’
    ```

# kdetrees

Version: 0.1.5

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    as.matrix.multiPhylo : tip2tip: no visible global function definition
      for ‘cophenetic’
    bw.nn : <anonymous>: no visible global function definition for
      ‘quantile’
    dist.diss: no visible binding for global variable ‘median’
    dist.diss: no visible global function definition for ‘dist’
    hist.kdetrees: no visible binding for global variable ‘density’
    kdetrees : cutoff: no visible global function definition for ‘quantile’
    kdetrees.complete: no visible global function definition for ‘plot’
    kdetrees.complete: no visible global function definition for ‘hist’
    kdetrees.complete: no visible global function definition for
      ‘write.csv’
    plot.kdetrees: no visible binding for global variable ‘density’
    Undefined global functions or variables:
      cophenetic density dist hist median plot quantile write.csv
    Consider adding
      importFrom("graphics", "hist", "plot")
      importFrom("stats", "cophenetic", "density", "dist", "median",
                 "quantile")
      importFrom("utils", "write.csv")
    to your NAMESPACE file.
    ```

# Kmisc

Version: 0.5.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    pp_plot : <anonymous>: no visible global function definition for ‘rgb’
    rcpp_apply_generator: no visible global function definition for
      ‘packageVersion’
    rcpp_tapply_generator: no visible global function definition for
      ‘packageVersion’
    read.cb: no visible global function definition for ‘read.table’
    remove_na: no visible global function definition for ‘complete.cases’
    size: no visible global function definition for ‘object.size’
    write.cb: no visible global function definition for ‘write.table’
    Undefined global functions or variables:
      anova capture.output coef colorRampPalette complete.cases dev.off
      kmeans object.size packageVersion par qbeta read.table rgb svg
      write.table
    Consider adding
      importFrom("grDevices", "colorRampPalette", "dev.off", "rgb", "svg")
      importFrom("graphics", "par")
      importFrom("stats", "anova", "coef", "complete.cases", "kmeans",
                 "qbeta")
      importFrom("utils", "capture.output", "object.size", "packageVersion",
                 "read.table", "write.table")
    to your NAMESPACE file.
    ```

# kobe

Version: 1.3.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      actual arguments
    theme_ms: possible error in colour = "grey50")): formal argument
      "strip.background" matched by multiple actual arguments
    theme_ms: no visible global function definition for ‘modifyList’
    kobe2sm,data.frame: no visible global function definition for
      ‘colorRampPalette’
    kobe2sm,data.frame: no visible global function definition for ‘par’
    kobe2sm,data.frame: no visible global function definition for
      ‘kobe2smFn’
    kobe2sm,data.frame: no visible global function definition for ‘mtext’
    kobe,data.frame-missing : <anonymous>: no visible global function
      definition for ‘kobeFn’
    Undefined global functions or variables:
      colorRampPalette grey kobe2smFn kobeFn median modifyList mtext par
      quantile read.csv read.table
    Consider adding
      importFrom("grDevices", "colorRampPalette", "grey")
      importFrom("graphics", "mtext", "par")
      importFrom("stats", "median", "quantile")
      importFrom("utils", "modifyList", "read.csv", "read.table")
    to your NAMESPACE file.
    ```

# KraljicMatrix

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘tibble’
      All declared Imports should be used.
    ```

# Lahman

Version: 6.0-0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.5Mb
      sub-directories of 1Mb or more:
        data   7.2Mb
    ```

# LANDD

Version: 1.1.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    getGO: no visible global function definition for ‘new’
    getGO: no visible global function definition for ‘getGeneric’
    graph.kd: no visible global function definition for ‘as’
    Undefined global functions or variables:
      as getGeneric new
    Consider adding
      importFrom("methods", "as", "getGeneric", "new")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# largeVis

Version: 0.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      with expansion:
        88 == 90
      
      ===============================================================================
      test cases:  2 |  1 passed |  1 failed
      assertions: 32 | 12 passed | 20 failed
      
      
      
      testthat results ================================================================
      OK: 147 SKIPPED: 1 FAILED: 1
      1. Failure: Catch unit tests pass (@test-cpp.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 19.7Mb
      sub-directories of 1Mb or more:
        libs          14.7Mb
        testdata       1.8Mb
        vignettedata   2.0Mb
    ```

# learningCurve

Version: 1.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gridExtra’ ‘scales’ ‘tidyverse’
      All declared Imports should be used.
    ```

# learnstats

Version: 0.1.1

## In both

*   checking whether package ‘learnstats’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/learnstats/new/learnstats.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    qqsim: no visible global function definition for ‘mtext’
    qqsim: no visible global function definition for ‘jpeg’
    qqsim: no visible global function definition for ‘dev.off’
    timeseressim: no visible global function definition for ‘rnorm’
    timeseressim: no visible global function definition for ‘par’
    timeseressim: no visible global function definition for ‘plot’
    timeseressim: no visible global function definition for ‘mtext’
    timeseressim: no visible global function definition for ‘jpeg’
    timeseressim: no visible global function definition for ‘dev.off’
    Undefined global functions or variables:
      axis dchisq dev.off dnorm dt hist jpeg legend mtext par pchisq pf
      plot pnorm polygon pt qchisq qf qqline qqnorm qt rchisq rnorm rt
      runif segments title
    Consider adding
      importFrom("grDevices", "dev.off", "jpeg")
      importFrom("graphics", "axis", "hist", "legend", "mtext", "par",
                 "plot", "polygon", "segments", "title")
      importFrom("stats", "dchisq", "dnorm", "dt", "pchisq", "pf", "pnorm",
                 "pt", "qchisq", "qf", "qqline", "qqnorm", "qt", "rchisq",
                 "rnorm", "rt", "runif")
    to your NAMESPACE file.
    ```

# LedPred

Version: 1.10.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License components which are templates and need '+ file LICENSE':
      MIT
    ```

*   checking R code for possible problems ... NOTE
    ```
    .plotCostGamma: no visible global function definition for ‘title’
    .plotCostGamma: no visible global function definition for ‘points’
    mapFeaturesToCRMs: no visible global function definition for
      ‘write.table’
    mapFeaturesToCRMs: no visible global function definition for
      ‘read.table’
    mapFeaturesToCRMs: no visible global function definition for
      ‘download.file’
    scoreData: no visible global function definition for ‘write.table’
    Undefined global functions or variables:
      download.file points read.table title write.table
    Consider adding
      importFrom("graphics", "points", "title")
      importFrom("utils", "download.file", "read.table", "write.table")
    to your NAMESPACE file.
    ```

# lfa

Version: 1.6.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    compute.nulls: no visible global function definition for ‘rbinom’
    lfa: no visible binding for global variable ‘sd’
    lreg: no visible global function definition for ‘glm’
    read.bed: no visible global function definition for ‘read.table’
    trunc.svd: no visible global function definition for ‘rnorm’
    Undefined global functions or variables:
      glm rbinom read.table rnorm sd
    Consider adding
      importFrom("stats", "glm", "rbinom", "rnorm", "sd")
      importFrom("utils", "read.table")
    to your NAMESPACE file.
    ```

# likeLTD

Version: 6.2.1

## In both

*   checking whether package ‘likeLTD’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/likeLTD/new/likeLTD.Rcheck/00install.out’ for details.
    ```

# likert

Version: 1.3.5

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# LINC

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        doc    3.6Mb
        libs   1.4Mb
    ```

*   checking R/sysdata.rda ... NOTE
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                  old_size new_size compress
      sysdata.rda    449Kb    313Kb       xz
    ```

# Linnorm

Version: 2.0.8

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(Linnorm)
      > 
      > test_check("Linnorm")
      Error in library(matrixStats) : there is no package called 'matrixStats'
      Calls: test_check ... with_reporter -> force -> source_file -> eval -> eval -> library
      testthat results ================================================================
      OK: 4 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        data   2.3Mb
        doc    1.9Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    Linnorm.HClust: no visible binding for global variable ‘x’
    Linnorm.HClust: no visible binding for global variable ‘y’
    Linnorm.HClust: no visible binding for global variable ‘xend’
    Linnorm.HClust: no visible binding for global variable ‘yend’
    Linnorm.HClust: no visible binding for global variable ‘cluster’
    Linnorm.HClust: no visible binding for global variable ‘X1’
    Linnorm.HClust: no visible binding for global variable ‘X2’
    Linnorm.HVar: no visible binding for global variable ‘SD’
    Linnorm.HVar: no visible binding for global variable ‘group’
    Undefined global functions or variables:
      SD X1 X2 cluster group x xend y yend
    ```

# lme4

Version: 1.1-14

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 26.8Mb
      sub-directories of 1Mb or more:
        doc        1.5Mb
        libs      21.7Mb
        testdata   1.5Mb
    ```

# LocFDRPois

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    AnalyticalOptim: no visible global function definition for ‘optim’
    LLConstructor : LL: no visible global function definition for ‘dpois’
    MixtureDensity: no visible global function definition for ‘glm’
    MixtureDensity : f_hat: no visible global function definition for
      ‘predict’
    NullDensity : f0: no visible global function definition for ‘dpois’
    Undefined global functions or variables:
      dpois glm optim predict
    Consider adding
      importFrom("stats", "dpois", "glm", "optim", "predict")
    to your NAMESPACE file.
    ```

# lslx

Version: 0.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 44.1Mb
      sub-directories of 1Mb or more:
        doc    1.5Mb
        libs  42.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘ggplot2’ ‘lavaan’
      All declared Imports should be used.
    ```

# LymphoSeq

Version: 1.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        doc       2.6Mb
        extdata   5.5Mb
    ```

# m2b

Version: 1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘e1071’ ‘utils’
      All declared Imports should be used.
    ```

# MAc

Version: 1.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘R2wd’
    
    Packages which this enhances but not available for checking:
      ‘compute.es’ ‘irr’
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘R2wd’ ‘ggplot2’ ‘metafor’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    wd.mareg: no visible global function definition for ‘wdGet’
    wd.mareg: no visible global function definition for ‘wdNewDoc’
    wd.mareg: no visible global function definition for ‘wdHeading’
    wd.mareg: no visible global function definition for ‘wdTable’
    wd.omni: no visible global function definition for ‘wdGet’
    wd.omni: no visible global function definition for ‘wdNewDoc’
    wd.omni: no visible global function definition for ‘wdHeading’
    wd.omni: no visible global function definition for ‘wdTable’
    Undefined global functions or variables:
      TukeyHSD aes aggregate anova aov contrasts facet_wrap g geom_boxplot
      geom_errorbarh geom_jitter geom_point geom_smooth geom_vline ggplot
      id l.ci95 lm model.extract model.matrix model.response na.omit opts
      pchisq pf pnorm printCoefmat qf qt r rma scale_y_continuous se.z
      se.z.tau stat_abline stat_summary u.ci95 var var.g wdGet wdHeading
      wdNewDoc wdTable wi wi.tau xlab xlim ylab ylim z
    Consider adding
      importFrom("stats", "TukeyHSD", "aggregate", "anova", "aov",
                 "contrasts", "lm", "model.extract", "model.matrix",
                 "model.response", "na.omit", "pchisq", "pf", "pnorm",
                 "printCoefmat", "qf", "qt", "var")
    to your NAMESPACE file.
    ```

# macleish

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘DBI’
      All declared Imports should be used.
    ```

# MAd

Version: 0.8-2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘R2wd’
    
    Packages which this enhances but not available for checking:
      ‘compute.es’ ‘irr’
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘R2wd’ ‘ggplot2’ ‘metafor’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    omni: no visible global function definition for ‘pnorm’
    omni: no visible global function definition for ‘pchisq’
    p.ancova_to_d1: no visible global function definition for ‘qt’
    p.ancova_to_d2: no visible global function definition for ‘qt’
    p_to_d1: no visible global function definition for ‘qt’
    p_to_d2: no visible global function definition for ‘qt’
    print.summary.mareg: no visible global function definition for
      ‘printCoefmat’
    robustSE: no visible global function definition for ‘residuals’
    robustSE: no visible global function definition for ‘pt’
    robustSE: no visible global function definition for ‘qt’
    Undefined global functions or variables:
      TukeyHSD aggregate anova aov contrasts model.extract model.matrix
      model.response na.omit pchisq pf pnorm printCoefmat pt qf qt
      residuals var
    Consider adding
      importFrom("stats", "TukeyHSD", "aggregate", "anova", "aov",
                 "contrasts", "model.extract", "model.matrix",
                 "model.response", "na.omit", "pchisq", "pf", "pnorm",
                 "printCoefmat", "pt", "qf", "qt", "residuals", "var")
    to your NAMESPACE file.
    ```

# mafs

Version: 0.0.3

## Newly broken

*   checking whether package ‘mafs’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘forecast::autolayer’ by ‘ggplot2::autolayer’ when loading ‘mafs’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/mafs/new/mafs.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘cmprsk’ ‘colorspace’ ‘etm’ ‘fracdiff’ ‘gtable’ ‘munsell’
      ‘numDeriv’ ‘plyr’ ‘quadprog’ ‘scales’ ‘timeDate’ ‘tseries’ ‘zoo’
      All declared Imports should be used.
    ```

# maftools

Version: 1.2.30

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'maftools.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc       1.8Mb
        extdata   4.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      SubstitutionMotif SubstitutionTypeMotif T>A T>C T>G TCA TCGA TCT TGA
      T[C>A]A T[C>A]T T[C>G]A T[C>G]T T[C>T]A T[C>T]T T[G>A]A T[G>C]A
      T[G>T]A TiTv Time TumorSampleBarcode Tumor_Sample_Barcode
      Tumor_Seq_Allele2 Unique_Name V1 V2 Var1 Var2 Variant_Classification
      Variant_Type Wide_Peak_Limits aa.length amp assembly_version bg
      chromosome chromosome_end chromosome_start ci.low ci.up clusters
      cohort con con.class consequence_type conv count count2 cytoband dat
      distance downstream endDist ens_id fdr fisher_pvalue flow fract
      fract_muts_in_clusters fraction fraction_APOBEC_mutations fs gene
      gene_affected hgnc_symbol i.End_Position i.Start_Position
      icgc_sample_id id idx lab labThis label loc log10OR
      mutated_from_allele mutated_to_allele mutations muts_in_clusters
      nGenes nMut nMuts nSamples nVars n_A n_C n_C>G_and_C>T n_G n_T
      n_mutations nonApobec non_APOBEC_mutations or peakID pfam poissonFdr
      pos pos2 posRounded protein.ID pval qvalues reference_genome_allele
      refseq.ID sequencing_strategy significant site sort_by_anno startDist
      statFontSize survLower survProb survUp tCw tCw_to_A tCw_to_G
      tCw_to_G+tCw_to_T tCw_to_T tFdr t_alt_count t_ref_count t_vaf tcw th
      total trinucleotide uid updown upstream value variable variantId
      verification_platform verification_status wGa wGa_to_A wGa_to_C
      wGa_to_T wga x y ymax ymin ystart
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘corrplot’
    'library' or 'require' call not declared from: ‘corrplot’
    ```

# magick

Version: 1.5

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘tesseract’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 12.8Mb
      sub-directories of 1Mb or more:
        doc    6.8Mb
        libs   5.8Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘tesseract’
    ```

# manhattanly

Version: 0.2.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘qqman’
    ```

# mapr

Version: 0.3.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-that.R’ failed.
    Last 13 lines of output:
      9: request_build("POST", hu$url, body_config(body, match.arg(encode)), as.request(config), 
             ...)
      10: as.request(config)
      11: gist_auth()
      12: stop("In non-interactive environments, please set GITHUB_PAT env to a GitHub", " access token (https://help.github.com/articles/creating-an-access-token-for-command-line-use)", 
             call. = FALSE)
      
      testthat results ================================================================
      OK: 7 SKIPPED: 0 FAILED: 1
      1. Error: map_gist works as expected (@test-map_gist.R#13) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      closing unused connection 3 (/home/muelleki/tmp/RtmpwlKRRD/filee483fcb058d.geojson) 
      Execution halted
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 164 marked UTF-8 strings
    ```

# marmap

Version: 0.9.6

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘maptools’
    ```

# MAST

Version: 1.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        data   3.7Mb
        doc    1.9Mb
    ```

# matrixStats

Version: 0.52.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.3Mb
      sub-directories of 1Mb or more:
        libs   7.6Mb
    ```

# mcaGUI

Version: 1.24.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgetsRGtk2’
    
    Packages which this enhances but not available for checking:
      ‘iplots’ ‘reshape’
    
    Depends: includes the non-default packages:
      ‘lattice’ ‘MASS’ ‘proto’ ‘foreign’ ‘gWidgets’ ‘gWidgetsRGtk2’
      ‘OTUbase’ ‘vegan’ ‘bpca’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# MCbiclust

Version: 1.0.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        data   3.2Mb
        doc    4.9Mb
    ```

# mcMST

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘parallelMap’ ‘reshape2’
      All declared Imports should be used.
    ```

# mdpeer

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glmnet’
      All declared Imports should be used.
    ```

# mdsr

Version: 0.1.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   5.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyverse’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2698 marked UTF-8 strings
    ```

# MEAL

Version: 1.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Fitting chr22...
    Fitting chr3...
    Fitting chr4...
    Fitting chr5...
    Fitting chr6...
    Fitting chr7...
    Fitting chr8...
    Fitting chr9...
    Fitting chrX...
    Fitting chrY...
    Demarcating regions...
    Done!
    Warning in rlm.default(x = X, y = y, weights = w, ...) :
      'rlm' failed to converge in 100 steps
    Warning: In BioC 3.5, the 'force' argument was replaced by the more
      flexible 'pruning.mode' argument, and is deprecated. See ?seqinfo
      for the supported pruning modes. Note that 'force=TRUE' is
      equivalent to 'pruning.mode="coarse"'.
    Error: processing vignette 'MEAL.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    correlationMethSNPs: no visible binding for global variable 'num_cores'
    prepareMethylationSet: no visible global function definition for
      'colData'
    Undefined global functions or variables:
      colData num_cores
    ```

# meaRtools

Version: 1.0.1

## In both

*   checking whether package ‘meaRtools’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/meaRtools/new/meaRtools.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.3Mb
      sub-directories of 1Mb or more:
        extdata   3.9Mb
        libs      1.4Mb
    ```

# medmod

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘ggplot2’ ‘lavaan’
      All declared Imports should be used.
    ```

# memapp

Version: 2.6

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘RColorBrewer’ ‘RODBC’ ‘formattable’ ‘ggplot2’ ‘ggthemes’ ‘mem’
      ‘openxlsx’ ‘plotly’ ‘readxl’ ‘shinyBS’ ‘shinydashboard’ ‘shinyjs’
      ‘shinythemes’ ‘stringr’
      All declared Imports should be used.
    ```

# meme

Version: 0.0.7

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘magick’
    
    Package suggested but not available for checking: ‘ggimage’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# MergeGUI

Version: 0.2-1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgetsRGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# messina

Version: 1.12.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘doMC’
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘Biobase’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘quantile’
    messinaSurvKMplot : <anonymous>: no visible binding for global variable
      ‘sd’
    messinaSurvKMplotSingleGroup: no visible binding for global variable
      ‘median’
    messinaSurvKMplotSingleGroup: no visible binding for global variable
      ‘quantile’
    messinaSurvKMplotSingleGroup: no visible binding for global variable
      ‘sd’
    messinaSurvObjPlot: no visible global function definition for
      ‘quantile’
    messinaSurvObjectiveFunc: no visible global function definition for
      ‘coef’
    messinaSurvPlot: no visible global function definition for ‘str’
    Undefined global functions or variables:
      approx coef median quantile reorder sd str
    Consider adding
      importFrom("stats", "approx", "coef", "median", "quantile", "reorder",
                 "sd")
      importFrom("utils", "str")
    to your NAMESPACE file.
    ```

# metagen

Version: 1.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    pivotalStream: no visible global function definition for ‘rchisq’
    pivotalStream : <anonymous>: no visible global function definition for
      ‘rchisq’
    pivotalStream: no visible global function definition for ‘rnorm’
    rBinomGauss: no visible global function definition for ‘rnorm’
    rBinomGauss: no visible global function definition for ‘rbinom’
    rD: no visible global function definition for ‘rchisq’
    rY: no visible global function definition for ‘rnorm’
    render: no visible global function definition for ‘pdf’
    render: no visible global function definition for ‘dev.off’
    renderSVG: no visible global function definition for ‘svg’
    renderSVG: no visible global function definition for ‘dev.off’
    Undefined global functions or variables:
      data dev.off pdf qchisq qnorm qt quantile rbinom rchisq rnorm svg
      uniroot var
    Consider adding
      importFrom("grDevices", "dev.off", "pdf", "svg")
      importFrom("stats", "qchisq", "qnorm", "qt", "quantile", "rbinom",
                 "rchisq", "rnorm", "uniroot", "var")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# metagene

Version: 2.8.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        as.data.frame, cbind, colMeans, colSums, colnames, do.call,
        duplicated, eval, evalq, get, grep, grepl, intersect,
        is.unsorted, lapply, lengths, mapply, match, mget, order,
        paste, pmax, pmax.int, pmin, pmin.int, rank, rbind, rowMeans,
        rowSums, rownames, sapply, setdiff, sort, table, tapply,
        union, unique, unsplit, which, which.max, which.min
    
    Loading required package: S4Vectors
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
    Loading required package: BiocParallel
    Error: processing vignette 'metagene.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    get_demo_design: no visible global function definition for ‘read.table’
    get_promoters_txdb: no visible global function definition for ‘is’
    plot_metagene: no visible binding for global variable ‘position’
    plot_metagene: no visible binding for global variable ‘value’
    plot_metagene: no visible binding for global variable ‘qinf’
    plot_metagene: no visible binding for global variable ‘qsup’
    plot_metagene: no visible binding for global variable ‘group’
    Undefined global functions or variables:
      group is position qinf qsup read.table value
    Consider adding
      importFrom("methods", "is")
      importFrom("utils", "read.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# MetaIntegrator

Version: 1.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Setting options('download.file.method.GEOquery'='auto')
    Setting options('GEOquery.inmemory.gpl'=FALSE)
    Error: processing vignette 'MetaIntegrator.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# metaMix

Version: 0.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      visible global function definition for ‘runif’
    parallel.temper.nucl : parallel.temper.nucl.wrapped : singleChain: no
      visible global function definition for ‘quantile’
    parallel.temper.nucl.explicit : singleChain : moveRemove: no visible
      global function definition for ‘quantile’
    parallel.temper.nucl.explicit : singleChain: no visible global function
      definition for ‘runif’
    parallel.temper.nucl.explicit : singleChain: no visible global function
      definition for ‘quantile’
    reduce.space : reduce.space.wrapped: no visible global function
      definition for ‘quantile’
    reduce.space.explicit: no visible global function definition for
      ‘quantile’
    Undefined global functions or variables:
      dev.off pdf plot ppois quantile runif write.table
    Consider adding
      importFrom("grDevices", "dev.off", "pdf")
      importFrom("graphics", "plot")
      importFrom("stats", "ppois", "quantile", "runif")
      importFrom("utils", "write.table")
    to your NAMESPACE file.
    ```

# meteogRam

Version: 1.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    crosssection: no visible global function definition for
      ‘colorRampPalette’
    crosssection: no visible global function definition for
      ‘filled.contour’
    crosssection: no visible global function definition for ‘axis’
    crosssection: no visible global function definition for ‘par’
    crosssection: no visible global function definition for ‘contour’
    temperatures: no visible binding for global variable ‘time’
    Undefined global functions or variables:
      axis colorRampPalette contour filled.contour par time
    Consider adding
      importFrom("grDevices", "colorRampPalette")
      importFrom("graphics", "axis", "contour", "filled.contour", "par")
      importFrom("stats", "time")
    to your NAMESPACE file.
    ```

# Methplot

Version: 1.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    getdata: no visible global function definition for ‘read.csv’
    plotdata: no visible global function definition for ‘head’
    Undefined global functions or variables:
      head read.csv
    Consider adding
      importFrom("utils", "head", "read.csv")
    to your NAMESPACE file.
    ```

# MethylAid

Version: 1.10.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘MethylAidData’
    ```

# methylInheritance

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        duplicated, eval, evalq, get, grep, grepl, intersect,
        is.unsorted, lapply, lengths, mapply, match, mget, order,
        paste, pmax, pmax.int, pmin, pmin.int, rank, rbind, rowMeans,
        rowSums, rownames, sapply, setdiff, sort, table, tapply,
        union, unique, unsplit, which, which.max, which.min
    
    Loading required package: S4Vectors
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
    Warning message:
    In max(i) : no non-missing arguments to max; returning -Inf
    Error: processing vignette 'methylInheritance.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# MethylMix

Version: 2.4.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    FOXD1 :  2  components are best.
    ME1 :  2  components are best.
    MGMT :  2  components are best.
    OAS1 :  2  components are best.
    SOX10 :  2  components are best.
    TRAF6 :  2  components are best.
    ZNF217 :  2  components are best.
    > 
    > # Plot the most famous methylated gene for glioblastoma
    > MethylMix_PlotModel("MGMT", MethylMixResults, METcancer)
    $MixtureModelPlot
    
    $CorrelationPlot
    NULL
    
    > 
    > # Plot MGMT also with its normal methylation variation
    > MethylMix_PlotModel("MGMT", MethylMixResults, METcancer, METnormal = METnormal)
    $MixtureModelPlot
    Error: Aesthetics must be either length 1 or the same as the data (251): x, y, xend, yend
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘digest’
      All declared Imports should be used.
    ```

# methylumi

Version: 2.22.0

## In both

*   checking for missing documentation entries ... WARNING
    ```
    Undocumented code objects:
      ‘IDATsToMatrices’ ‘IDATtoMatrix’ ‘tcgaPipeline’
    Undocumented S4 methods:
      generic '[' and siglist 'MethyLumiM,ANY,ANY,ANY'
      generic '[' and siglist 'MethyLumiSet,ANY,ANY,ANY'
    All user-level objects in a package (including S4 classes and methods)
    should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking contents of ‘data’ directory ... WARNING
    ```
    Files not of a type allowed in a ‘data’ directory:
      ‘5318317007_A_Grn.idat’ ‘5318317007_A_Red.idat’
      ‘5318317007_B_Grn.idat’ ‘5318317007_B_Red.idat’
      ‘5318317007_C_Grn.idat’ ‘5318317007_C_Red.idat’
    Please use e.g. ‘inst/extdata’ for non-R data files
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘Biobase’ ‘scales’ ‘reshape2’ ‘ggplot2’ ‘matrixStats’
      ‘FDb.InfiniumMethylation.hg19’ ‘minfi’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.8Mb
      sub-directories of 1Mb or more:
        data      6.8Mb
        extdata   1.7Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Packages listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘Biobase’ ‘minfi’ ‘lattice’ ‘matrixStats’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls to packages already attached by Depends:
      ‘FDb.InfiniumMethylation.hg19’ ‘ggplot2’ ‘matrixStats’ ‘minfi’
      ‘reshape2’ ‘scales’
      Please remove these calls from your code.
    'library' or 'require' calls in package code:
      ‘Biostrings’ ‘MASS’ ‘lumi’ ‘parallel’ ‘rtracklayer’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    Packages in Depends field not imported from:
      ‘FDb.InfiniumMethylation.hg19’ ‘ggplot2’ ‘matrixStats’ ‘methods’
      ‘reshape2’ ‘scales’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ':::' call which should be '::': ‘lumi:::produceMethylationGEOSubmissionFile’
      See the note in ?`:::` about the use of this operator.
    Unexported objects imported by ':::' calls:
      ‘Biobase:::unsafeSetSlot’ ‘genefilter:::.findCentralMap’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      IlluminaHumanMethylation450kCOLORCHANNEL MethylSet RGChannelSet
      abline addColorChannelInfo aes allele as.dist axis box capture.output
      channel.probes colMedians colQuantiles colSds colorRampPalette
      coord_flip data dnorm dropouts drops ecdf facet_grid features
      gamma.integral gamma.mle gamma.mode geom_histogram ggplot hclust
      hm27.controls hm27.ordering hm450.controls hm450.ordering huber index
      intensity lines log_trans mclapply melt mu offset opts p.adjust
      packageDescription packageVersion par plot.density pnorm points
      position_identity read.csv read.delim read.table rect rowMins
      scale_colour_manual scale_fill_manual scale_shape_manual
      scale_x_continuous scale_y_continuous scale_y_discrete
      subsetCommonProbes text theme_bw title value variable weighted.mean
    Consider adding
      importFrom("grDevices", "colorRampPalette")
      importFrom("graphics", "abline", "axis", "box", "lines", "par",
                 "points", "rect", "text", "title")
      importFrom("stats", "as.dist", "dnorm", "ecdf", "hclust", "offset",
                 "p.adjust", "pnorm", "weighted.mean")
      importFrom("utils", "capture.output", "data", "packageDescription",
                 "packageVersion", "read.csv", "read.delim", "read.table")
    to your NAMESPACE file.
    ```

*   checking Rd files ... NOTE
    ```
    prepare_Rd: estimateM.Rd:34-36: Dropping empty section \seealso
    prepare_Rd: estimateM.Rd:37-39: Dropping empty section \examples
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' call not declared from: ‘TxDb.Hsapiens.UCSC.hg19.knownGene’
    ```

# microbenchmark

Version: 1.4-2.1

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# micromap

Version: 1.9.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    dot_cl_build: no visible global function definition for ‘median’
    lmplot: no visible global function definition for ‘gray’
    map_att: no visible global function definition for ‘gray’
    mmgroupedplot: no visible global function definition for ‘median’
    mmplot: no visible global function definition for ‘gray’
    mmplot: no visible global function definition for ‘median’
    print.mm: no visible global function definition for ‘dev.new’
    print.mm: no visible global function definition for ‘pdf’
    print.mm: no visible global function definition for ‘tiff’
    print.mm: no visible global function definition for ‘jpeg’
    print.mm: no visible global function definition for ‘png’
    print.mm: no visible global function definition for ‘postscript’
    print.mm: no visible global function definition for ‘dev.off’
    Undefined global functions or variables:
      aggregate dev.new dev.off gray jpeg median pdf png poly postscript
      tiff
    Consider adding
      importFrom("grDevices", "dev.new", "dev.off", "gray", "jpeg", "pdf",
                 "png", "postscript", "tiff")
      importFrom("stats", "aggregate", "median", "poly")
    to your NAMESPACE file.
    ```

# MIGSA

Version: 1.0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
      
          Filter, Find, Map, Position, Reduce, anyDuplicated, append,
          as.data.frame, cbind, colMeans, colSums, colnames, do.call,
          duplicated, eval, evalq, get, grep, grepl, intersect, is.unsorted,
          lapply, lengths, mapply, match, mget, order, paste, pmax, pmax.int,
          pmin, pmin.int, rank, rbind, rowMeans, rowSums, rownames, sapply,
          setdiff, sort, table, tapply, union, unique, unsplit, which,
          which.max, which.min
      
      
      
      Error in library("RUnit", quietly = TRUE) : 
        there is no package called 'RUnit'
      Calls: <Anonymous> -> library
      Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    df' not found.
    
    See the pdftex.def package documentation for explanation.
    Type  H <return>  for immediate help.
    ! Package pdftex.def Error: File `ClassDiagrams/SEAparams-eps-converted-to.pdf'
     not found.
    
    See the pdftex.def package documentation for explanation.
    Type  H <return>  for immediate help.
    ! Package pdftex.def Error: File `ClassDiagrams/GSEAparams-eps-converted-to.pdf
    ' not found.
    
    See the pdftex.def package documentation for explanation.
    Type  H <return>  for immediate help.
    ! Package pdftex.def Error: File `ClassDiagrams/FitOptions-eps-converted-to.pdf
    ' not found.
    
    See the pdftex.def package documentation for explanation.
    Type  H <retur
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# MIMOSA

Version: 1.14.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    Undefined global functions or variables:
      AnnotatedDataFrame ExpressionSet PTID Proportion Proportion_REF
      RefTreat aes as.formula callNextMethod contour coord_trans dbeta dexp
      ecdf facet_grid facet_wrap fisher.test geom_boxplot geom_jitter
      geom_line ggtitle gray.colors huber image is.formula ldply legend
      lines mclapply model.frame na.omit optim p.adjust pData<- par pbeta
      plot points position_jitter qbeta quantile rbeta rbinom read.table
      rmultinom rnorm runif scale_color_brewer scale_fill_brewer show terms
      title var
    Consider adding
      importFrom("grDevices", "gray.colors")
      importFrom("graphics", "contour", "image", "legend", "lines", "par",
                 "plot", "points", "title")
      importFrom("methods", "callNextMethod", "show")
      importFrom("stats", "as.formula", "dbeta", "dexp", "ecdf",
                 "fisher.test", "model.frame", "na.omit", "optim",
                 "p.adjust", "pbeta", "qbeta", "quantile", "rbeta", "rbinom",
                 "rmultinom", "rnorm", "runif", "terms", "var")
      importFrom("utils", "read.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# MineICA

Version: 1.16.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘doMC’
    
    Depends: includes the non-default packages:
      ‘BiocGenerics’ ‘Biobase’ ‘plyr’ ‘ggplot2’ ‘scales’ ‘foreach’ ‘xtable’
      ‘biomaRt’ ‘gtools’ ‘GOstats’ ‘cluster’ ‘marray’ ‘mclust’
      ‘RColorBrewer’ ‘colorspace’ ‘igraph’ ‘Rgraphviz’ ‘graph’ ‘annotate’
      ‘Hmisc’ ‘fastICA’ ‘JADE’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Packages listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘biomaRt’ ‘GOstats’ ‘cluster’ ‘mclust’ ‘igraph’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘GOstats’ which was already attached by Depends.
      Please remove these calls from your code.
    Namespace in Imports field not imported from: ‘lumiHumanAll.db’
      All declared Imports should be used.
    Packages in Depends field not imported from:
      ‘GOstats’ ‘Hmisc’ ‘JADE’ ‘RColorBrewer’ ‘Rgraphviz’ ‘annotate’
      ‘biomaRt’ ‘cluster’ ‘colorspace’ ‘fastICA’ ‘foreach’ ‘ggplot2’
      ‘graph’ ‘gtools’ ‘igraph’ ‘marray’ ‘mclust’ ‘methods’ ‘plyr’ ‘scales’
      ‘xtable’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ':::' calls which should be '::':
      ‘Biobase:::annotation’ ‘Biobase:::validMsg’ ‘fpc:::pamk’
      ‘lumi:::getChipInfo’ ‘mclust:::adjustedRandIndex’
      See the note in ?`:::` about the use of this operator.
    Unexported object imported by a ':::' call: ‘Biobase:::isValidVersion’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      reorder scale_colour_gradientn scale_colour_manual scale_fill_manual
      scale_linetype_manual scale_shape_manual scale_x_continuous
      scale_x_discrete scale_y_continuous shapiro.test sigCategories
      terrain_hcl theme theme_bw title tkplot.fit.to.screen unit useMart
      validObject vcount viewport wilcox.test write.table xlab xtable
    Consider adding
      importFrom("grDevices", "cm.colors", "dev.off", "graphics.off",
                 "heat.colors", "pdf")
      importFrom("graphics", "abline", "axis", "frame", "hist", "layout",
                 "legend", "mtext", "par", "plot", "plot.new", "points",
                 "title")
      importFrom("methods", "callNextMethod", "new", "validObject")
      importFrom("stats", "aggregate", "as.dendrogram", "as.dist",
                 "as.hclust", "chisq.test", "cor", "cor.test", "cutree",
                 "dist", "hclust", "kmeans", "kruskal.test", "lm", "median",
                 "na.omit", "order.dendrogram", "p.adjust", "quantile",
                 "reorder", "shapiro.test", "wilcox.test")
      importFrom("utils", "capture.output", "combn", "read.table",
                 "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# MiRAnorm

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘cluster’ ‘dendextend’ ‘npmv’ ‘utils’
      All declared Imports should be used.
    ```

# mirIntegrator

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    integrate_mir : isGraphNEL: no visible global function definition for
      ‘is’
    pathways2pdf: no visible global function definition for ‘pdf’
    pathways2pdf: no visible global function definition for ‘par’
    pathways2pdf: no visible global function definition for ‘graphics.off’
    plotLines: no visible binding for global variable ‘x’
    plotLines: no visible binding for global variable ‘y’
    plotLines: no visible binding for global variable ‘pathways_set’
    plotPathway2Colors: no visible global function definition for ‘legend’
    plot_change: no visible global function definition for ‘complete.cases’
    Undefined global functions or variables:
      complete.cases graphics.off is legend par pathways_set pdf x y
    Consider adding
      importFrom("grDevices", "graphics.off", "pdf")
      importFrom("graphics", "legend", "par")
      importFrom("methods", "is")
      importFrom("stats", "complete.cases")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# MissingDataGUI

Version: 0.2-5

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgetsRGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# MixSIAR

Version: 3.1.7

## In both

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘MixSIAR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mixsiar_gui
    > ### Title: Run the GUI version of MixSIAR
    > ### Aliases: mixsiar_gui
    > 
    > ### ** Examples
    > 
    > mixsiar_gui()
    Error in mixsiar_gui() : 
      *** gWidgetsRGtk2 package not able to be loaded. ***
            If 'library('gWidgetsRGtk2')' does not work, MixSIAR GUI will not run.
            On Windows/Linux, try 'install.packages('gWidgetsRGtk2')'.
            On Mac, close R, download and install GTK+ from:
            http://r.research.att.com/#other. Then install latest X11 application
            (xQuartz) from http://xquartz.macosforge.org/landing/.
    
            If installing GTK+ continues to be problematic, consider using the
            script version of MixSIAR. See manual for help and examples.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘gWidgetsRGtk2’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rjags’
      All declared Imports should be used.
    ```

# mizer

Version: 0.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘Species’
    plotM2,MizerSim: no visible binding for global variable ‘w’
    plotM2,MizerSim: no visible binding for global variable ‘value’
    plotM2,MizerSim: no visible binding for global variable ‘Species’
    plotSpectra,MizerSim: no visible binding for global variable ‘w’
    plotSpectra,MizerSim: no visible binding for global variable ‘value’
    plotSpectra,MizerSim: no visible binding for global variable ‘Species’
    plotYield,MizerSim: no visible binding for global variable ‘time’
    plotYield,MizerSim: no visible binding for global variable ‘value’
    plotYield,MizerSim: no visible binding for global variable ‘Species’
    plotYieldGear,MizerSim: no visible binding for global variable ‘time’
    plotYieldGear,MizerSim: no visible binding for global variable ‘value’
    plotYieldGear,MizerSim: no visible binding for global variable
      ‘Species’
    plotYieldGear,MizerSim: no visible binding for global variable ‘gear’
    Undefined global functions or variables:
      Species combn gear lm time value w
    Consider adding
      importFrom("stats", "lm", "time")
      importFrom("utils", "combn")
    to your NAMESPACE file.
    ```

# mlmm

Version: 1.0

## In both

*   checking examples ... ERROR
    ```
    ...
    +  for ( i in 1:50) {if (miss[i]==1) var1[i]=NA;
    +  if (censor[i]==1) var1[i]=0.05}
    +  pdata=data.frame(var1,var2,miss,censor,geneid,sid)
    +  pathdir=getwd()
    +  if (R.Version()$arch=="i386") fp=system.file("chunks","mlmc_i32.rds",package="mlmm")
    +  if (R.Version()$arch=="x86_64") fp=system.file("chunks","mlmc_i64.rds",package="mlmm")
    +  mcfit=readRDS(fp)
    +  model1=mlmc(formula_completed=var1~var2,formula_missing=miss~var2,
    +              formula_censor=censor~1,formula_subject=~var2,pdata=pdata,response_censorlim=0.05,
    +              respond_dep_missing=TRUE,pidname="geneid",sidname="sid",pathname=pathdir,
    +              iterno=10,chains=2,savefile=FALSE,usefit=TRUE,stanfit=mcfit)
    +  }
    > system.time(testfun())
    Error in prep_call_sampler(object) : 
      the compiled object from C++ code for this model is invalid, possible reasons:
      - compiled with save_dso=FALSE;
      - compiled on a different platform;
      - does not exist (created from reading csv files).
    Calls: system.time ... sampling -> .local -> <Anonymous> -> prep_call_sampler
    Timing stopped at: 0.224 0.008 0.234
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 43.9Mb
      sub-directories of 1Mb or more:
        chunks   3.4Mb
        libs    40.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstantools’
      All declared Imports should be used.
    ```

# mlr

Version: 2.11

## In both

*   checking whether package ‘mlr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘BBmisc::isFALSE’ by ‘backports::isFALSE’ when loading ‘mlr’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/mlr/new/mlr.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘mlrMBO’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported objects:
      ‘batchtools::getAlgorithmIds’ ‘batchtools::getProblemIds’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘mlrMBO’
    ```

# mlrMBO

Version: 1.1.0

## In both

*   checking whether package ‘mlrMBO’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘BBmisc::isFALSE’ by ‘backports::isFALSE’ when loading ‘mlrMBO’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/mlrMBO/new/mlrMBO.Rcheck/00install.out’ for details.
    ```

# mlxR

Version: 3.2.0

## In both

*   checking whether package ‘mlxR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/mlxR/new/mlxR.Rcheck/00install.out’ for details.
    ```

# MMDiff2

Version: 1.4.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    DBAmmd: no visible global function definition for ‘new’
    Undefined global functions or variables:
      new
    Consider adding
      importFrom("methods", "new")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# mmnet

Version: 1.13.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘biom’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# Mobilize

Version: 2.16-4

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘theme_text’
    mapmobility: no visible global function definition for ‘lines’
    responseplot: no visible global function definition for ‘na.omit’
    scatterplot: no visible global function definition for ‘na.omit’
    scatterplot: no visible global function definition for ‘opts’
    scatterplot: no visible global function definition for ‘theme_text’
    sharedtimeplot: no visible global function definition for ‘na.omit’
    timeplot: no visible global function definition for ‘na.omit’
    timeplot: no visible global function definition for ‘opts’
    timeplot.character: no visible global function definition for ‘runif’
    timeplot.character: no visible global function definition for ‘rnorm’
    timeplot.character: no visible binding for global variable ‘text’
    userplot: no visible global function definition for ‘na.omit’
    Undefined global functions or variables:
      head lines na.omit opts quantile rnorm runif text theme_blank
      theme_text
    Consider adding
      importFrom("graphics", "lines", "text")
      importFrom("stats", "na.omit", "quantile", "rnorm", "runif")
      importFrom("utils", "head")
    to your NAMESPACE file.
    ```

# modelr

Version: 0.1.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘lme4’, ‘rstanarm’
    ```

# modeval

Version: 0.1.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning: Deprecated
    Quitting from lines 112-115 (modeval.Rmd) 
    Error: processing vignette 'modeval.Rmd' failed with diagnostics:
    the argument has already been evaluated
    Execution halted
    ```

# momentuHMM

Version: 1.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    1.8Mb
        libs   3.4Mb
    ```

# Momocs

Version: 1.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        doc    2.3Mb
    ```

# monocle

Version: 2.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning in if (method == "num_genes") { :
      the condition has length > 1 and only the first element will be used
    Warning: Deprecated, use tibble::rownames_to_column() instead.
    Warning: Removed 3576038 rows containing non-finite values (stat_density).
    Warning: Deprecated, use tibble::rownames_to_column() instead.
    Warning: Deprecated, use tibble::rownames_to_column() instead.
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Transformation introduced infinite values in continuous y-axis
    Quitting from lines 327-334 (monocle-vignette.Rnw) 
    Error: processing vignette 'monocle-vignette.Rnw' failed with diagnostics:
    BLAS/LAPACK routine 'DLASCL' gave error code -4
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    assign_cell_lineage: no visible global function definition for ‘nei’
    buildBranchCellDataSet: no visible global function definition for ‘nei’
    clusterCells: no visible binding for global variable ‘rho’
    clusterCells: no visible binding for global variable ‘delta’
    count_leaf_descendents: no visible global function definition for ‘nei’
    cth_classifier_cds: no visible global function definition for ‘nei’
    cth_classifier_cell: no visible global function definition for ‘nei’
    diff_test_helper: no visible binding for global variable ‘Size_Factor’
    extract_good_ordering: no visible global function definition for ‘nei’
    fit_model_helper: no visible binding for global variable ‘Size_Factor’
    get_next_node_id: no visible binding for '<<-' assignment to
      ‘next_node’
    get_next_node_id: no visible binding for global variable ‘next_node’
    make_canonical: no visible global function definition for ‘nei’
    measure_diameter_path: no visible global function definition for ‘nei’
    orderCells: no visible binding for '<<-' assignment to ‘next_node’
    project2MST: no visible global function definition for ‘nei’
    Undefined global functions or variables:
      Size_Factor delta nei next_node rho
    ```

# moonBook

Version: 0.1.3

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    num_summary: no visible binding for global variable ‘sd’
    num_summary: no visible binding for global variable ‘median’
    num_summary: no visible binding for global variable ‘mad’
    num_summary: no visible binding for global variable ‘IQR’
    num_summary: no visible binding for global variable ‘fivenum’
    Undefined global functions or variables:
      IQR abline anova as.formula chisq.test coef confint density
      fisher.test fivenum kruskal.test legend lines lm mad median na.omit
      par plot points rainbow rect resid sd segments shapiro.test t.test
      terms text var.test write.csv write.table xtabs
    Consider adding
      importFrom("grDevices", "rainbow")
      importFrom("graphics", "abline", "legend", "lines", "par", "plot",
                 "points", "rect", "segments", "text")
      importFrom("stats", "IQR", "anova", "as.formula", "chisq.test", "coef",
                 "confint", "density", "fisher.test", "fivenum",
                 "kruskal.test", "lm", "mad", "median", "na.omit", "resid",
                 "sd", "shapiro.test", "t.test", "terms", "var.test",
                 "xtabs")
      importFrom("utils", "write.csv", "write.table")
    to your NAMESPACE file.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'cbind.mytable':
      ‘cbind.mytable’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# mosaic

Version: 1.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘manipulate’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cubature’
    ```

# mosaicData

Version: 0.14.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 7 marked UTF-8 strings
    ```

# mosaicModel

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘caret’ ‘ggformula’ ‘knitr’ ‘testthat’ ‘tidyverse’
      All declared Imports should be used.
    ```

# mousetrap

Version: 3.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        libs   4.5Mb
    ```

# mrfDepth

Version: 1.0.5

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Running examples in ‘mrfDepth-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: fom
    > ### Title: Draws the Functional Outlier Map (FOM)
    > ### Aliases: fom
    > ### Keywords: Graphical
    > 
    > ### ** Examples
    > 
    > data(octane)
    > 
    > # To construct the FOM, one first need to calculate 
    > # the functional outlyingness.
    > # Note that the option diagnostic in fOutl must be 
    > # set to TRUE. If not calling fom will result in an 
    > # error
    > Result <- fOutl(octane, alpha = 0, type = "fAO", diagnostic = TRUE);
    > fom(Result)
    Error: Columns `x`, `y` must be 1d atomic vectors or lists
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.9Mb
      sub-directories of 1Mb or more:
        libs  15.1Mb
    ```

# mrMLM

Version: 2.1

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgetsRGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# MSCMT

Version: 1.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rdpack’
      All declared Imports should be used.
    ```

# msgbsR

Version: 1.0.0

## In both

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      
      Note: significantly better compression could be obtained
            by using R CMD build --resave-data
                   old_size new_size compress
      ratdata.rda     318Kb    128Kb       xz
      ratdata2.rda    287Kb    116Kb       xz
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 16.1Mb
      sub-directories of 1Mb or more:
        extdata  15.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: 'utils'
      All declared Imports should be used.
    ```

# MSnbase

Version: 2.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in has_utility("convert", "ImageMagick") :
      ImageMagick not installed or not in PATH
    Quitting from lines 1022-1029 (MSnbase-demo.Rnw) 
    Error: processing vignette 'MSnbase-demo.Rnw' failed with diagnostics:
    package or namespace load failed for 'Rdisop' in dyn.load(file, DLLpath = DLLpath, ...):
     unable to load shared object '/home/muelleki/git/R/ggplot2/revdep/library/MSnbase/Rdisop/libs/Rdisop.so':
      libRcppClassic.so: cannot open shared object file: No such file or directory
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.0Mb
      sub-directories of 1Mb or more:
        data   1.9Mb
        doc    4.3Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Authors@R field gives more than one person with maintainer role:
      Laurent Gatto <lg390@cam.ac.uk> [aut, cre]
      Johannes Rainer <Johannes.Rainer@eurac.edu> [aut, cre]
      Sebastian Gibb <mail@sebastiangibb.de> [aut, cre]
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘Biobase:::.showAnnotatedDataFrame’ ‘MALDIquant:::.estimateNoise’
      ‘MALDIquant:::.localMaxima’ ‘MALDIquant:::.movingAverage’
      ‘MALDIquant:::.savitzkyGolay’
      See the note in ?`:::` about the use of this operator.
    ```

# MSnID

Version: 1.10.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘modification’
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘DatabaseAccess’
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘DatabaseDescription’
    .read_mzIDs.mzR.engine.single.file: no visible binding for global
      variable ‘DBseqLength’
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘accession’
    infer_parsimonious_accessions,MSnID : infer_acc: no visible binding for
      global variable ‘pepSeq’
    recalibrate,MSnID: no visible global function definition for ‘median’
    recalibrate,MSnID: no visible global function definition for ‘density’
    Undefined global functions or variables:
      DBseqLength DatabaseAccess DatabaseDescription accession density i
      location mass median modification name optim pepSeq quantile rnorm
      spectrumID
    Consider adding
      importFrom("stats", "density", "median", "optim", "quantile", "rnorm")
    to your NAMESPACE file.
    ```

# msPurity

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'msPurity-vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .travis.yml
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘Rcpp’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    package 'methods' is used but not declared
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    removeIsotopes: no visible global function definition for ‘write.csv’
    rsde: no visible global function definition for ‘sd’
    snrFilter: no visible global function definition for ‘median’
    stde: no visible global function definition for ‘sd’
    stderror: no visible global function definition for ‘sd’
    averageSpectra,purityD: no visible binding for global variable ‘i’
    subtract,purityD: no visible binding for global variable ‘i’
    validate,purityA: no visible global function definition for ‘head’
    writeOut,purityD: no visible global function definition for ‘write.csv’
    Undefined global functions or variables:
      abline alli approxfun as.dist dev.off dist dnorm fix head i idx
      legend lines median mtch mtchi na.omit parallel plot png points
      purity read.csv scanid sd text variable write.csv
    Consider adding
      importFrom("grDevices", "dev.off", "png")
      importFrom("graphics", "abline", "legend", "lines", "plot", "points",
                 "text")
      importFrom("stats", "approxfun", "as.dist", "dist", "dnorm", "median",
                 "na.omit", "sd")
      importFrom("utils", "fix", "head", "read.csv", "write.csv")
    to your NAMESPACE file.
    ```

# MSstats

Version: 3.8.6

## In both

*   checking whether package ‘MSstats’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘MSnbase::image’ by ‘graphics::image’ when loading ‘MSstats’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/MSstats/new/MSstats.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    dataProcessPlots: no visible binding for global variable ‘Name’
    dataProcessPlots: no visible binding for global variable ‘analysis’
    dataProcessPlots: no visible binding for global variable ‘Mean’
    dataProcessPlots: no visible binding for global variable ‘ciw’
    groupComparisonPlots: no visible binding for global variable ‘Protein’
    groupComparisonPlots: no visible binding for global variable ‘logFC’
    groupComparisonPlots: no visible binding for global variable ‘ciw’
    linear_quantlim: no visible binding for global variable ‘label’
    modelBasedQCPlots: no visible binding for global variable ‘residual’
    nonlinear_quantlim: no visible binding for global variable ‘label’
    plot_quantlim: no visible binding for global variable ‘x’
    plot_quantlim: no visible binding for global variable ‘y’
    plot_quantlim: no visible binding for global variable ‘ymin’
    plot_quantlim: no visible binding for global variable ‘ymax’
    plot_quantlim: no visible binding for global variable ‘shape’
    transformMSnSetToMSstats: no visible global function definition for
      ‘pData’
    Undefined global functions or variables:
      ABUNDANCE LABEL Mean Name Protein RUN analysis ciw datafeature
      grid.layout grid.newpage label logFC missing.col pData pushViewport
      residual shape weight x y ymax ymin
    ```

# mudata2

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘hms’ ‘methods’
      All declared Imports should be used.
    ```

# MultiBD

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.5Mb
      sub-directories of 1Mb or more:
        libs   7.9Mb
    ```

# multilevelPSA

Version: 1.2.4

## In both

*   checking R code for possible problems ... NOTE
    ```
    missing.plot: possible error in theme(axis.text.x = element_text(size =
      6, angle = -90, hjust = 0.5, vjust = 0.5), axis.text.x =
      element_blank(), axis.ticks = element_blank()): formal argument
      "axis.text.x" matched by multiple actual arguments
    ```

# MultiMeta

Version: 0.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    multi_meta: no visible global function definition for ‘write.table’
    multi_meta: no visible global function definition for ‘read.table’
    multi_meta: no visible global function definition for ‘pchisq’
    qqplotter: no visible global function definition for ‘read.table’
    qqplotter: no visible global function definition for ‘qchisq’
    qqplotter: no visible global function definition for ‘ppoints’
    qqplotter: no visible global function definition for ‘median’
    qqplotter: no visible global function definition for ‘pchisq’
    qqplotter: no visible global function definition for ‘pdf’
    qqplotter: no visible global function definition for ‘plot’
    qqplotter: no visible global function definition for ‘abline’
    qqplotter: no visible global function definition for ‘dev.off’
    Undefined global functions or variables:
      abline axis colorRampPalette cov2cor dev.off median pchisq pdf plot
      points ppoints qchisq read.table write.table
    Consider adding
      importFrom("grDevices", "colorRampPalette", "dev.off", "pdf")
      importFrom("graphics", "abline", "axis", "plot", "points")
      importFrom("stats", "cov2cor", "median", "pchisq", "ppoints", "qchisq")
      importFrom("utils", "read.table", "write.table")
    to your NAMESPACE file.
    ```

# munsell

Version: 0.4.3

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘munsell-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: complement_slice
    > ### Title: A vertical slice through the Munsell space
    > ### Aliases: complement_slice
    > 
    > ### ** Examples
    > 
    > complement_slice("5PB")
    Warning: Removed 41 rows containing missing values (geom_text).
    Error: Free scales are only supported with `coord_cartesian()` and `coord_flip()`
    Execution halted
    ```

# MutationalPatterns

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    Warning: Transformation introduced infinite values in continuous y-axis
    Warning: Removed 8 rows containing missing values (geom_point).
    Using by, region as id variables
    Warning: Ignoring unknown aesthetics: ymax
    Warning: Using alpha for a discrete variable is not advised.
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'Introduction_to_MutationalPatterns.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `apacite.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.5 \bibliographystyle
                          {apacite}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# mvdalab

Version: 1.4

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘mvdalab-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.plusminus
    > ### Title: 2D Graph of the PCA scores associated with a plusminusFit
    > ### Aliases: plot.plusminus
    > 
    > ### ** Examples
    > 
    > ###  PLUS-Minus CLASSIFIER WITH validation = 'none', i.e. no CV ###
    > data(plusMinusDat)
    > mod1 <- plusminusFit(Y ~., data = plusMinusDat, validation = "none", n_cores = 2)
    > plot(mod1, ncomp = 2, comps = c(1, 2))
    Error: Columns `colour`, `shape` must be 1d atomic vectors or lists
    Execution halted
    ```

# MWASTools

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
       % metabolite features with CV < 0.3 : 99
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-6>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-7>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-8>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-9>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Error: processing vignette 'MWASTools.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    MWAS_scatterplotMS: no visible binding for global variable ‘logpval’
    QC_CV_scatterplot: no visible binding for global variable ‘abs.CV’
    Undefined global functions or variables:
      abs.CV logpval
    ```

# myTAI

Version: 0.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    2.7Mb
    ```

# nandb

Version: 0.2.1

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘autothresholdr’ ‘EBImage’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# natserv

Version: 0.1.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      3: cli$get(query = query)
      4: private$make_url(self$url, self$handle, path, query)
      5: add_query(query, url)
      6: check_key(key)
      7: getOption("NatureServeKey", stop("You need an API key for NatureServe"))
      8: stop("You need an API key for NatureServe")
      
      testthat results ================================================================
      OK: 14 SKIPPED: 0 FAILED: 3
      1. Error: ns_data works as expected (@test-ns_data.R#6) 
      2. Error: ns_images works as expected (@test-ns_images.R#6) 
      3. Error: ns_search works as expected (@test-ns_search.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# NeatMap

Version: 0.3.6.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    data.reduction: no visible global function definition for ‘prcomp’
    data.reduction: no visible global function definition for ‘hclust’
    data.reduction: no visible global function definition for ‘as.dist’
    data.reduction: no visible global function definition for ‘cor’
    data.reduction: no visible global function definition for ‘dist’
    heatmap1: no visible global function definition for ‘sd’
    lineplot: no visible global function definition for ‘sd’
    normalize: no visible global function definition for ‘sd’
    profileplot3d : ColorFunction: no visible global function definition
      for ‘rgb’
    profileplot3d : ColorFunction: no visible global function definition
      for ‘colorRamp’
    profileplot3d: no visible global function definition for ‘sd’
    profileplot3d: no visible global function definition for ‘gray’
    Undefined global functions or variables:
      as.dist colorRamp cor dist gray hclust prcomp rgb sd
    Consider adding
      importFrom("grDevices", "colorRamp", "gray", "rgb")
      importFrom("stats", "as.dist", "cor", "dist", "hclust", "prcomp", "sd")
    to your NAMESPACE file.
    ```

*   checking compiled code ... NOTE
    ```
    File ‘NeatMap/libs/NeatMap.so’:
      Found ‘rand’, possibly from ‘rand’ (C)
        Object: ‘nMDS_R.o’
      Found ‘srand’, possibly from ‘srand’ (C)
        Object: ‘nMDS_R.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# nethet

Version: 1.8.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    t2cov.lr: no visible global function definition for ‘var’
    t2cov.lr: no visible global function definition for ‘pchisq’
    t2diagcov.lr: no visible global function definition for ‘var’
    t2diagcov.lr: no visible global function definition for ‘pchisq’
    test.sd: no visible global function definition for ‘var’
    test.sd: no visible global function definition for ‘pnorm’
    twosample_single_regr: no visible global function definition for ‘coef’
    twosample_single_regr: no visible global function definition for ‘lm’
    Undefined global functions or variables:
      abline boxplot coef cor cov.wt dev.off dnorm grey hist kmeans legend
      lines lm median optimize p.adjust par pchisq pdf pnorm quantile rbeta
      rnorm segments shapiro.test var write.csv
    Consider adding
      importFrom("grDevices", "dev.off", "grey", "pdf")
      importFrom("graphics", "abline", "boxplot", "hist", "legend", "lines",
                 "par", "segments")
      importFrom("stats", "coef", "cor", "cov.wt", "dnorm", "kmeans", "lm",
                 "median", "optimize", "p.adjust", "pchisq", "pnorm",
                 "quantile", "rbeta", "rnorm", "shapiro.test", "var")
      importFrom("utils", "write.csv")
    to your NAMESPACE file.
    ```

# netprioR

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Warning: executing %dopar% sequentially: no parallel backend registered
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-10>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-11>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-12>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-13>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Error: processing vignette 'netprioR.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# netresponse

Version: 1.36.0

## In both

*   checking files in ‘vignettes’ ... WARNING
    ```
    Files in the 'vignettes' directory but no files in 'inst/doc':
      ‘NetResponse.Rmd’, ‘NetResponse.md’, ‘TODO/TODO.Rmd’,
        ‘fig/NetResponse2-1.png’, ‘fig/NetResponse2b-1.png’,
        ‘fig/NetResponse3-1.png’, ‘fig/NetResponse4-1.png’,
        ‘fig/NetResponse5-1.png’, ‘fig/NetResponse7-1.png’,
        ‘fig/vdp-1.png’, ‘main.R’, ‘netresponse.bib’, ‘netresponse.pdf’
    Package has no Sweave vignette sources and no VignetteBuilder field.
    ```

*   checking compiled code ... NOTE
    ```
    File ‘netresponse/libs/netresponse.so’:
      Found ‘rand’, possibly from ‘rand’ (C)
        Object: ‘netresponse.o’
      Found ‘srand’, possibly from ‘srand’ (C)
        Object: ‘netresponse.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# NetworkChange

Version: 0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘abind’
      All declared Imports should be used.
    ```

# networkreporting

Version: 0.1.1

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:plyr':
    
        arrange, count, desc, failwith, id, mutate, rename, summarise,
        summarize
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: functional
    Loading required package: stringr
    Quitting from lines 221-222 (network_scaleup.Rmd) 
    Error: processing vignette 'network_scaleup.Rmd' failed with diagnostics:
    Column `x` must be a 1d atomic vector or a list
    Execution halted
    ```

# networktools

Version: 1.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘networktools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot.expectedInf
    > ### Title: Plot "expectedInf" objects
    > ### Aliases: plot.expectedInf
    > 
    > ### ** Examples
    > 
    > ## Don't show: 
    > myNetwork <- cor(depression[,1:5])
    > ## End(Don't show)
    > out1 <- expectedInf(myNetwork)
    > plot(out1$step1)
    Error: Column `x` must be a 1d atomic vector or a list
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘devtools’
      All declared Imports should be used.
    ```

# neuropsychology

Version: 0.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘htmlTable’ ‘lme4’ ‘stringi’
      All declared Imports should be used.
    ```

# NFP

Version: 0.99.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘NFPdata’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   7.5Mb
    ```

# nima

Version: 0.4.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘survival’
      All declared Imports should be used.
    ```

# nimble

Version: 0.6-7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.5Mb
      sub-directories of 1Mb or more:
        CppCode        1.0Mb
        R              3.4Mb
        classic-bugs   1.6Mb
        include        5.8Mb
        tests          1.2Mb
    ```

# nlmixr

Version: 0.9.0-1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 195 SKIPPED: 0 FAILED: 11
      1. Failure: ODE (@test-model68.R#64) 
      2. Failure: ODE (@test-model68.R#65) 
      3. Failure: ODE (@test-model68.R#66) 
      4. Failure: ODE (@test-model68.R#68) 
      5. Failure: ODE (@test-model68.R#69) 
      6. Failure: ODE (@test-model68.R#71) 
      7. Failure: ODE (@test-model68.R#72) 
      8. Failure: ODE (@test-model68.R#73) 
      9. Failure: ODE (@test-model68.R#76) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking compiled code ... WARNING
    ```
    File ‘nlmixr/libs/nlmixr.so’:
      Found ‘__assert_fail’, possibly from ‘assert’ (C)
        Objects: ‘RcppExportMod.o’, ‘RcppExports.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 13.7Mb
      sub-directories of 1Mb or more:
        libs  12.5Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dparser’ ‘inline’ ‘n1qn1’
      All declared Imports should be used.
    ```

# NlsyLinks

Version: 2.0.6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.6Mb
      sub-directories of 1Mb or more:
        data      4.3Mb
        reports   2.3Mb
    ```

# NMF

Version: 0.20.6

## In both

*   checking Rd cross-references ... WARNING
    ```
    Unknown package ‘RcppOctave’ in Rd xrefs
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      ...
    Error in citation(x) : package 'doMC' not found
    Converted 9 of 10 package citations to BibTeX
    Writing 11 Bibtex entries ... OK
    Results written to file 'Rpackages.bib'
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'NMF-vignette.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `biblatex.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.131 \AtEveryCitekey
                         {\clearfield{url}}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘RcppOctave’
    ```

*   checking R code for possible problems ... NOTE
    ```
    algorithm,NMFStrategyOctave: no visible global function definition for
      ‘fstop’
    evar,ANY: no visible binding for global variable ‘Biobase’
    nmf,matrix-numeric-NMFStrategy : run.all: no visible binding for global
      variable ‘n’
    nmf,matrix-numeric-NMFStrategy : run.all: no visible binding for global
      variable ‘RNGobj’
    rss,matrix: no visible binding for global variable ‘Biobase’
    Undefined global functions or variables:
      Biobase RNGobj fstop n
    ```

# noaastormevents

Version: 0.1.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘hurricaneexposuredata’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘XML’ ‘choroplethr’ ‘choroplethrMaps’ ‘data.table’
      ‘forcats’ ‘hurricaneexposure’ ‘plyr’
      All declared Imports should be used.
    ```

# NormalizeMets

Version: 0.22

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.2Mb
      sub-directories of 1Mb or more:
        doc   8.6Mb
    ```

# NORRRM

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    CIPW: no visible global function definition for ‘data’
    CIPW: no visible binding for global variable ‘AtomWeight’
    CIPW: no visible binding for global variable ‘MinWeight’
    CIPW: no visible binding for global variable ‘OxiWeight’
    CIPW.trace: no visible global function definition for ‘data’
    CIPW.trace: no visible binding for global variable ‘AtomWeight’
    CIPW.trace: no visible binding for global variable ‘MinWeight’
    CIPW.trace: no visible binding for global variable ‘OxiWeight’
    Undefined global functions or variables:
      AtomWeight MinWeight OxiWeight data
    Consider adding
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# NPflow

Version: 0.13.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.8Mb
      sub-directories of 1Mb or more:
        libs  10.4Mb
    ```

# nullabor

Version: 0.3.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    reg_dist: no visible global function definition for ‘lm’
    resid_boot: no visible global function definition for ‘resid’
    resid_pboot: no visible global function definition for ‘rnorm’
    resid_rotate: no visible global function definition for ‘rnorm’
    resid_rotate: no visible global function definition for ‘update’
    resid_rotate: no visible global function definition for ‘resid’
    resid_sigma: no visible global function definition for ‘rnorm’
    rorschach: no visible global function definition for ‘rbinom’
    rss: no visible global function definition for ‘resid’
    sep_dist: no visible global function definition for ‘dist’
    sep_dist: no visible global function definition for ‘cutree’
    sep_dist: no visible global function definition for ‘hclust’
    uni_dist: no visible global function definition for ‘sd’
    Undefined global functions or variables:
      coef cutree dist filter hclust lm predict quantile rbinom resid rnorm
      sd update
    Consider adding
      importFrom("stats", "coef", "cutree", "dist", "filter", "hclust", "lm",
                 "predict", "quantile", "rbinom", "resid", "rnorm", "sd",
                 "update")
    to your NAMESPACE file.
    ```

# nzelect

Version: 0.4.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6409 marked UTF-8 strings
    ```

# oapackage

Version: 2.0.23

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.4Mb
      sub-directories of 1Mb or more:
        libs  27.3Mb
    ```

# observer

Version: 0.1.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘ensurer’, ‘validate’
    ```

# oddsratio

Version: 1.0.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    +                      pred = "x2", values = c(0.099, 0.198))
    > 
    > # insert first odds ratios to plot
    > plot_object <- insert_or(plot_object, or_object1, or_yloc = 3,
    +                          values_xloc = 0.04, line_size = 0.5,
    +                          line_type = "dotdash", text_size = 6,
    +                          values_yloc = 0.5, arrow_col = "red")
    > 
    > # calculate second odds ratio
    > or_object2 <- or_gam(data = data_gam, model = fit_gam, pred = "x2",
    +                      values = c(0.4, 0.6))
    > 
    > # add or_object2 into plot
    > insert_or(plot_object, or_object2, or_yloc = 2.1, values_yloc = 2,
    +           line_col = "green4", text_col = "black",
    +           rect_col = "green4", rect_alpha = 0.2,
    +           line_alpha = 1, line_type = "dashed",
    +           arrow_xloc_r = 0.01, arrow_xloc_l = -0.01,
    +           arrow_length = 0.01, rect = TRUE)
    Error: Aesthetics must be either length 1 or the same as the data (1): xmin, xmax, ymin, ymax, fill, alpha
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      
      Loading required package: nlme
      This is mgcv 1.8-22. For overview type 'help("mgcv-package")'.
      Warning: No confident interval calculation possible
              for 'glmmPQL' models
      
      testthat results ================================================================
      OK: 5 SKIPPED: 0 FAILED: 3
      1. Failure: check bevhaviour of ggplot_build (changed in ggplot2 v2.2) (@test-insert-or.R#19) 
      2. Failure: check bevhaviour of ggplot_build (changed in ggplot2 v2.2) (@test-insert-or.R#20) 
      3. Failure: check bevhaviour of ggplot_build (changed in ggplot2 v2.2) (@test-insert-or.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 124-133 (function_tutorial.Rmd) 
    Error: processing vignette 'function_tutorial.Rmd' failed with diagnostics:
    Aesthetics must be either length 1 or the same as the data (1): xmin, xmax, ymin, ymax, fill, alpha
    Execution halted
    ```

# officer

Version: 0.1.8

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘officer-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: slip_in_img
    > ### Title: append an image
    > ### Aliases: slip_in_img
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    > img.file <- file.path( Sys.getenv("R_HOME"), "doc", "html", "logo.jpg" )
    > x <- read_docx() %>%
    +   body_add_par("R logo: ", style = "Normal") %>%
    +   slip_in_img(src = img.file, style = "strong", width = .3, height = .3)
    Error: file.exists(src) is not TRUE
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      6: freduce(value, `_function_list`)
      7: withVisible(function_list[[k]](value))
      8: function_list[[k]](value)
      9: ph_with_img(., type = "body", src = img.file, height = 1.06, width = 1.39)
      10: external_img(src, width = width/914400, height = height/914400)
      11: stopifnot(file.exists(src))
      12: stop(msg, call. = FALSE, domain = NA)
      
      testthat results ================================================================
      OK: 341 SKIPPED: 1 FAILED: 2
      1. Error: image add  (@test-docx-add.R#68) 
      2. Error: add img into placeholder (@test-pptx-add.R#67) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 180-190 (powerpoint.Rmd) 
    Error: processing vignette 'powerpoint.Rmd' failed with diagnostics:
    file.exists(src) is not TRUE
    Execution halted
    ```

# OncoSimulR

Version: 2.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc    5.4Mb
        libs   4.9Mb
    ```

# onemap

Version: 2.1.1

## In both

*   checking whether package ‘onemap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
      Warning: loading Rplot failed
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/onemap/new/onemap.Rcheck/00install.out’ for details.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        libs   3.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maps’
      All declared Imports should be used.
    ```

# openwindfarm

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 22-24 (openwindfarm-vignette.Rmd) 
    Error: processing vignette 'openwindfarm-vignette.Rmd' failed with diagnostics:
    there is no package called 'webshot'
    Execution halted
    ```

# OperaMate

Version: 1.8.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    generateReport: no visible global function definition for ‘write.table’
    operaMate : configParser: no visible global function definition for
      ‘read.csv’
    parseFullTab: no visible global function definition for ‘read.delim’
    parseMatrix: no visible global function definition for ‘read.delim’
    parseTab: no visible global function definition for ‘read.delim’
    wellQC : <anonymous>: no visible global function definition for
      ‘boxplot’
    wellQC: no visible global function definition for ‘boxplot’
    cellNorm,cellData: no visible global function definition for
      ‘capture.output’
    show,cellData: no visible global function definition for ‘str’
    show,expData: no visible global function definition for ‘str’
    Undefined global functions or variables:
      abline boxplot capture.output combn read.csv read.delim str strheight
      strwidth write.table
    Consider adding
      importFrom("graphics", "abline", "boxplot", "strheight", "strwidth")
      importFrom("utils", "capture.output", "combn", "read.csv",
                 "read.delim", "str", "write.table")
    to your NAMESPACE file.
    ```

# optiRum

Version: 0.37.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      `check1` inherits from `ggplot_built` not `list`.
      
      
      3. Failure: giniChart correctly produce a chart, factor outcome (@test-giniChart.R#14) 
      `check1` inherits from `ggplot_built` not `list`.
      
      
      testthat results ================================================================
      OK: 201 SKIPPED: 0 FAILED: 3
      1. Error: generatePDF - correct behaviour, DATED=FALSE,CLEANUP=TRUE, compiler=xelatex (@test-generatePDF.R#75) 
      2. Failure: giniChart correctly produce a chart, numeric outcome (@test-giniChart.R#7) 
      3. Failure: giniChart correctly produce a chart, factor outcome (@test-giniChart.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# optiSel

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        extdata   1.8Mb
        libs      5.5Mb
    ```

# orderedLasso

Version: 1.7

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    plot.orderedLasso.cv: no visible global function definition for
      ‘abline’
    plot.timeLagLasso.cv: no visible global function definition for ‘par’
    plot.timeLagLasso.cv: no visible global function definition for ‘plot’
    plot.timeLagLasso.cv: no visible global function definition for ‘axis’
    plot.timeLagLasso.cv: no visible global function definition for ‘mtext’
    plot.timeLagLasso.cv: no visible global function definition for
      ‘points’
    plot.timeLagLasso.cv: no visible global function definition for
      ‘abline’
    timeLagLasso: no visible binding for global variable ‘sd’
    timeLagLasso.cv: no visible binding for global variable ‘sd’
    timeLagLasso.cv: no visible binding for global variable ‘var’
    timeLagLasso.path: no visible binding for global variable ‘sd’
    Undefined global functions or variables:
      abline approx axis mtext par plot points sd segments var
    Consider adding
      importFrom("graphics", "abline", "axis", "mtext", "par", "plot",
                 "points", "segments")
      importFrom("stats", "approx", "sd", "var")
    to your NAMESPACE file.
    ```

# ordinalForest

Version: 2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        libs   5.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# Organism.dplyr

Version: 1.2.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'Organism.dplyr.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘AnnotationDbi:::smartKeys’ ‘GenomicFeatures:::.exons_with_3utr’
      ‘GenomicFeatures:::.exons_with_5utr’
      ‘GenomicFeatures:::get_TxDb_seqinfo0’
      ‘S4Vectors:::extract_data_frame_rows’
      See the note in ?`:::` about the use of this operator.
    ```

# osmplotr

Version: 0.3.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        doc   5.9Mb
    ```

# otvPlots

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘quantreg’
      All declared Imports should be used.
    ```

# P2C2M

Version: 0.7.6

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rPython’
    
    Packages suggested but not available for checking:
      ‘genealogicalSorting’ ‘phybase’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# pa

Version: 1.2-1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Package in Depends field not imported from: ‘grid’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .formula.make: no visible global function definition for ‘formula’
    regress: no visible global function definition for ‘lm’
    regress: no visible global function definition for ‘model.matrix’
    Undefined global functions or variables:
      formula lm model.matrix
    Consider adding
      importFrom("stats", "formula", "lm", "model.matrix")
    to your NAMESPACE file.
    ```

# pacotest

Version: 0.2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        libs   6.9Mb
    ```

# PairedData

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls to packages already attached by Depends:
      ‘MASS’ ‘gld’ ‘mvtnorm’
      Please remove these calls from your code.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    winvar.Z: no visible global function definition for ‘qnorm’
    yuen.t.test.formula: no visible global function definition for ‘terms’
    yuen1.test: no visible global function definition for ‘qt’
    yuen1.test: no visible global function definition for ‘pt’
    yuen2.test: no visible global function definition for ‘complete.cases’
    yuen2.test: no visible global function definition for ‘qt’
    yuen2.test: no visible global function definition for ‘pt’
    yuenp.test: no visible global function definition for ‘complete.cases’
    yuenp.test: no visible global function definition for ‘qt’
    yuenp.test: no visible global function definition for ‘pt’
    summary,paired: no visible global function definition for ‘cor’
    Undefined global functions or variables:
      IQR complete.cases cor cor.test dnorm integrate mad median pchisq pf
      pnorm pt qchisq qf qnorm qt rbinom reorder sd t.test terms var
      wilcox.test
    Consider adding
      importFrom("stats", "IQR", "complete.cases", "cor", "cor.test",
                 "dnorm", "integrate", "mad", "median", "pchisq", "pf",
                 "pnorm", "pt", "qchisq", "qf", "qnorm", "qt", "rbinom",
                 "reorder", "sd", "t.test", "terms", "var", "wilcox.test")
    to your NAMESPACE file.
    ```

# paleofire

Version: 1.2.0

## Newly broken

*   checking whether package ‘paleofire’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::calc’ by ‘raster::calc’ when loading ‘paleofire’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/paleofire/new/paleofire.Rcheck/00install.out’ for details.
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    trying URL 'http://blarquez.com/public/data/data_cageo.zip'
    Content type 'application/zip' length 14857 bytes (14 KB)
    ==================================================
    downloaded 14 KB
    
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'paleofire-paper.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `elsarticle.cls' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: cls)
    
    ! Emergency stop.
    <read *> 
             
    l.4 ^^M
           
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# pandaR

Version: 1.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.3Mb
      sub-directories of 1Mb or more:
        data   9.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    dFunction: no visible global function definition for ‘cor’
    importPandaMatlab: no visible global function definition for
      ‘read.delim’
    panda: no visible global function definition for ‘cor’
    panda: no visible global function definition for ‘aggregate’
    plot.panda: no visible global function definition for ‘hist’
    plotCommunityDetection: no visible global function definition for
      ‘title’
    plotZbyTF: no visible global function definition for ‘aggregate’
    prepResult: no visible global function definition for ‘pnorm’
    Undefined global functions or variables:
      aggregate cor hist pnorm read.delim title
    Consider adding
      importFrom("graphics", "hist", "title")
      importFrom("stats", "aggregate", "cor", "pnorm")
      importFrom("utils", "read.delim")
    to your NAMESPACE file.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'plot.panda':
      ‘plot.panda’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# parallelDist

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        libs   4.8Mb
    ```

# parlitools

Version: 0.2.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13 marked UTF-8 strings
    ```

# parsemsf

Version: 0.1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘parsemsf-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: make_area_table
    > ### Title: Make a table of peptide areas
    > ### Aliases: make_area_table
    > 
    > ### ** Examples
    > 
    > make_area_table(parsemsf_example("test_db.msf"))
    Error: Condition message must be a string
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      13: glubort(NULL, "The {name} package is required to {reason}.", if (install) "\nPlease install it with `install.packages(\"{name}\")`")
      14: .abort(text)
      15: cnd_error(type, .msg = msg, .call = sys.call(-1))
      16: new_cnd(c(.type, "error"), ..., .msg = .msg)
      17: stop("Condition message must be a string", call. = FALSE)
      
      Error: Condition message must be a string
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 3
      1. Error: make_area_table creates a data frame with the correct column names (@test_make_area_table.R#16) 
      2. Error: make_pep_table creates a data frame with the correct column names (@test_make_pep_table.R#13) 
      3. Error: map_peptides creates a data frame with the correct column names (@test_map_peptides.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 20-25 (introduction.Rmd) 
    Error: processing vignette 'introduction.Rmd' failed with diagnostics:
    Condition message must be a string
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RSQLite’
      All declared Imports should be used.
    ```

# passport

Version: 0.1.1

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘passport-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: parse_country
    > ### Title: Parse country names to standardized form
    > ### Aliases: parse_country
    > 
    > ### ** Examples
    > 
    > parse_country(c("United States", "USA", "U.S.", "us", "United States of America"))
    [1] "US" "US" "US" "US" "US"
    > 
    > # Unicode support for parsing accented or non-Latin scripts
    > parse_country(c("\u65e5\u672c", "Japon", "\u0698\u0627\u067e\u0646"), how = "dstk")
    Error in open.connection(con, "rb") : 
      Timeout was reached: Connection timed out after 10001 milliseconds
    Calls: parse_country ... fromJSON_string -> parseJSON -> parse_con -> open -> open.connection
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      7: fromJSON(url)
      8: jsonlite::fromJSON(...)
      9: fromJSON_string(txt = txt, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame, 
             simplifyMatrix = simplifyMatrix, flatten = flatten, ...)
      10: parseJSON(txt, bigint_as_char)
      11: parse_con(txt, bigint_as_char)
      12: open(con, "rb")
      13: open.connection(con, "rb")
      
      testthat results ================================================================
      OK: 34 SKIPPED: 0 FAILED: 1
      1. Error: parsing country names with live geocoding APIs works (@test_parse_country.R#61) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# PathoStat

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning: Transformation introduced infinite values in discrete y-axis
    Error: processing vignette 'PathoStatAdvanced.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# pathVar

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotOneSample: no visible binding for global variable ‘Number_of_genes’
    plotTwoSamplesCont: no visible binding for global variable ‘PwayName’
    plotTwoSamplesCont: no visible binding for global variable
      ‘NumOfGenesFromDataSetInPathway’
    plotTwoSamplesCont: no visible binding for global variable ‘value’
    plotTwoSamplesCont: no visible binding for global variable
      ‘..density..’
    plotTwoSamplesCont: no visible binding for global variable ‘group’
    plotTwoSamplesDisc: no visible binding for global variable ‘Cluster’
    plotTwoSamplesDisc: no visible binding for global variable
      ‘Number_of_genes’
    sigOneSample: no visible binding for global variable ‘APval’
    sigOneSample: no visible binding for global variable ‘PwayName’
    sigTwoSamplesCont: no visible binding for global variable ‘APval’
    sigTwoSamplesCont: no visible binding for global variable ‘PwayName’
    sigTwoSamplesDisc: no visible binding for global variable ‘APval’
    sigTwoSamplesDisc: no visible binding for global variable ‘PwayName’
    Undefined global functions or variables:
      ..density.. APval Cluster NumOfGenesFromDataSetInPathway
      Number_of_genes PercOfGenesInPway PwayName avg cv group medAbsDev
      standDev value
    ```

# patPRO

Version: 1.1.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Checking should be performed on sources prepared by ‘R CMD build’.
    ```

# patternplot

Version: 0.2

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# pauwels2014

Version: 1.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking R code for possible problems ... NOTE
    ```
    add_noise: no visible global function definition for ‘rnorm’
    estimate_risk_dream6 : <anonymous>: no visible global function
      definition for ‘var’
    generate_sample: no visible global function definition for ‘rnorm’
    generate_sample: no visible global function definition for ‘runif’
    sample_function: no visible global function definition for ‘runif’
    sample_function : <anonymous>: no visible global function definition
      for ‘var’
    sample_function_multi_mod_weight: no visible global function definition
      for ‘runif’
    sample_function_multi_mod_weight : <anonymous>: no visible global
      function definition for ‘var’
    sample_function_single_mod: no visible global function definition for
      ‘runif’
    Undefined global functions or variables:
      rnorm runif var
    Consider adding
      importFrom("stats", "rnorm", "runif", "var")
    to your NAMESPACE file.
    ```

# PAWL

Version: 0.5

## In both

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘PAWL’ for: ‘show’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls to packages already attached by Depends:
      ‘foreach’ ‘ggplot2’ ‘reshape’
      Please remove these calls from your code.
    Packages in Depends field not imported from:
      ‘foreach’ ‘ggplot2’ ‘methods’ ‘mvtnorm’ ‘reshape’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    smcparameters.constructor: no visible global function definition for
      ‘new’
    systematic_resampling: no visible global function definition for
      ‘runif’
    target.constructor: no visible global function definition for ‘new’
    tuningparameters.constructor: no visible global function definition for
      ‘new’
    show,smcparameters: no visible global function definition for ‘tail’
    Undefined global functions or variables:
      %do% abline aes aes_string cov element_text facet_wrap foreach
      geom_density2d geom_line geom_point geom_step geom_vline ggplot hist
      labs mahalanobis melt new quantile rgamma rmultinom rmvnorm rnorm
      runif stat_bin2d tail theme xlab ylab ylim
    Consider adding
      importFrom("graphics", "abline", "hist")
      importFrom("methods", "new")
      importFrom("stats", "cov", "mahalanobis", "quantile", "rgamma",
                 "rmultinom", "rnorm", "runif")
      importFrom("utils", "tail")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# Pbase

Version: 0.16.0

## In both

*   checking examples ... ERROR
    ```
    ...
    
    Attaching package: 'AnnotationFilter'
    
    The following object is masked from 'package:Gviz':
    
        feature
    
    > library(EnsDb.Hsapiens.v86)
    > edb <- EnsDb.Hsapiens.v86
    > 
    > ## Define a filter to retrieve all genes from chromosome Y
    > sqnf <- SeqNameFilter("Y")
    > ## Retrieve the proteins without protein domains but specify to retrieve in
    > ## addition the transcript biotype for the encoding transcripts and the gene
    > ## names
    > prts <- Proteins(edb, filter = sqnf, loadProteinDomains = FALSE,
    +                  columns = c("tx_biotype", "gene_name"))
    Error in validObject(.Object) : 
      invalid class "AnnotationFilterList" object: superclass "vectorORfactor" not defined in the environment of the object's class
    Calls: Proteins ... .AnnotationFilterList -> new -> initialize -> initialize -> validObject
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      9: validObject(.Object)
      10: stop(msg, ": ", errors, domain = NA)
      
      testthat results ================================================================
      OK: 84 SKIPPED: 0 FAILED: 7
      1. Error: Proteins,EnsDb,missing constructor (@test_Proteins-ensembldb.R#9) 
      2. Error: Proteins,EnsDb,missing protein_id n:1 uniprot_id mapping (@test_Proteins-ensembldb.R#91) 
      3. Error: pfeatures (@test_Proteins-methods.R#185) 
      4. Error: .mapToGenome2 internal function (@test_mapToGenome-ensembldb.R#9) 
      5. Error: mapToGenome,Proteins,EnsDb (@test_mapToGenome-ensembldb.R#140) 
      6. Error: mapToGenome,Proteins,EnsDb with Uniprot IDs (@test_mapToGenome-ensembldb.R#183) 
      7. Error: .checkPcol (@test_utils.R#87) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: Rcpp
    Loading required package: Gviz
    Loading required package: S4Vectors
    Loading required package: stats4
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomicRanges
    Loading required package: GenomeInfoDb
    Loading required package: grid
    
    This is Pbase version 0.16.0
    
    Error: processing vignette 'Pbase-data.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Authors@R field gives more than one person with maintainer role:
      Laurent Gatto <lg390@cam.ac.uk> [aut, cre]
      Johannes Rainer <Johannes.Rainer@eurac.edu> [aut, cre]
      Sebastian Gibb <mail@sebastiangibb.de> [aut, cre]
    ```

# pbcmc

Version: 1.4.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
          pmin, pmin.int, rank, rbind, rowMeans, rowSums, rownames, sapply,
          setdiff, sort, table, tapply, union, unique, unsplit, which,
          which.max, which.min
      
      Welcome to Bioconductor
      
          Vignettes contain introductory material; view with
          'browseVignettes()'. To cite Bioconductor, see
          'citation("Biobase")', and for packages 'citation("pkgname")'.
      
      No methods found in "BiocGenerics" for requests: unlist
      Error in library("RUnit", quietly = TRUE) : 
        there is no package called 'RUnit'
      Calls: <Anonymous> -> library
      Execution halted
    ```

# pcadapt

Version: 3.0.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc       1.9Mb
        extdata   1.6Mb
        libs      1.5Mb
    ```

# PCADSC

Version: 0.8.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Matrix’ ‘pander’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘data.table’
    ```

# pcaExplorer

Version: 2.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following object is masked from 'package:base':
    
        apply
    
    Loading required package: AnnotationDbi
    
    'select()' returned 1:many mapping between keys and columns
    Warning in cor.test.default(x[, j], coldata[, i], method = "spearman") :
      Cannot compute exact p-value with ties
    Warning in cor.test.default(x[, j], coldata[, i], method = "spearman") :
      Cannot compute exact p-value with ties
    Warning in cor.test.default(x[, j], coldata[, i], method = "spearman") :
      Cannot compute exact p-value with ties
    Warning in cor.test.default(x[, j], coldata[, i], method = "spearman") :
      Cannot compute exact p-value with ties
    Warning: Removed 8 rows containing missing values (geom_text).
    Error: processing vignette 'pcaExplorer.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        doc   4.8Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    pcaExplorer: no visible binding for '<<-' assignment to
      ‘pcaexplorer_env’
    pcaExplorer : <anonymous>: no visible binding for global variable
      ‘airway’
    pcaExplorer : <anonymous>: no visible binding for global variable
      ‘pcaexplorer_env’
    Undefined global functions or variables:
      airway pcaexplorer_env
    ```

# pcaMethods

Version: 1.68.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    svdImpute: no visible global function definition for ‘prcomp’
    svdPca: no visible global function definition for ‘prcomp’
    plot,pcaRes: no visible global function definition for ‘gray’
    plot,pcaRes: no visible global function definition for ‘barplot’
    plot,pcaRes: no visible global function definition for ‘legend’
    slplot,pcaRes: no visible global function definition for ‘par’
    slplot,pcaRes: no visible global function definition for ‘layout’
    slplot,pcaRes: no visible global function definition for ‘abline’
    slplot,pcaRes: no visible global function definition for ‘lines’
    Undefined global functions or variables:
      abline barplot cor cov gray layout legend lines median na.omit pairs
      par points prcomp qf rnorm runif setTxtProgressBar text
      txtProgressBar
    Consider adding
      importFrom("grDevices", "gray")
      importFrom("graphics", "abline", "barplot", "layout", "legend",
                 "lines", "pairs", "par", "points", "text")
      importFrom("stats", "cor", "cov", "median", "na.omit", "prcomp", "qf",
                 "rnorm", "runif")
      importFrom("utils", "setTxtProgressBar", "txtProgressBar")
    to your NAMESPACE file.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ade4’
    ```

# PDN

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glmnet’
      All declared Imports should be used.
    ```

# pdp

Version: 0.6.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ICEbox’
    ```

# PedCNV

Version: 0.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘Rcpp’ ‘RcppArmadillo’ ‘ggplot2’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    Inde: no visible global function definition for ‘var’
    STE: no visible global function definition for ‘pchisq’
    STIM: no visible global function definition for ‘cor’
    STIM: no visible global function definition for ‘pchisq’
    plot.clust: no visible global function definition for ‘princomp’
    plot.clust: no visible global function definition for ‘loadings’
    plot.clust: no visible global function definition for ‘qplot’
    plot.clust: no visible global function definition for ‘geom_histogram’
    plot.clust: no visible global function definition for ‘aes_string’
    plot.clust: no visible global function definition for ‘barplot’
    plot.clust: no visible global function definition for ‘title’
    plot.clust: no visible global function definition for ‘mtext’
    plot.clust: no visible global function definition for ‘text’
    Undefined global functions or variables:
      aes_string barplot cor cov geom_histogram kmeans lm loadings mtext
      pchisq prcomp princomp qplot rbinom text title var
    Consider adding
      importFrom("graphics", "barplot", "mtext", "text", "title")
      importFrom("stats", "cor", "cov", "kmeans", "lm", "loadings", "pchisq",
                 "prcomp", "princomp", "rbinom", "var")
    to your NAMESPACE file.
    ```

# pepStat

Version: 1.10.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘pepStat’ for: ‘end’, ‘start’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    coerce,peptideSet-ExpressionSet: no visible global function definition
      for ‘annotation’
    end,peptideSet: no visible global function definition for ‘end’
    position,peptideSet: no visible global function definition for ‘start’
    position,peptideSet: no visible global function definition for ‘end’
    start,peptideSet: no visible global function definition for ‘start’
    write.pSet,peptideSet: no visible global function definition for
      ‘start’
    write.pSet,peptideSet: no visible global function definition for ‘end’
    write.pSet,peptideSet: no visible global function definition for
      ‘write.csv’
    Undefined global functions or variables:
      annotation dev.flush dev.hold dev.interactive devAskNewPage end
      lm.fit lm.wfit mcols mcols<- median read.csv sd start write.csv
    Consider adding
      importFrom("grDevices", "dev.flush", "dev.hold", "dev.interactive",
                 "devAskNewPage")
      importFrom("stats", "end", "lm.fit", "lm.wfit", "median", "sd",
                 "start")
      importFrom("utils", "read.csv", "write.csv")
    to your NAMESPACE file.
    ```

# perccalc

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘devtools’ ‘ggplot2’ ‘haven’ ‘tidyverse’
      All declared Imports should be used.
    ```

# perry

Version: 0.2.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    hasMethod: no visible global function definition for ‘getS3method’
    Undefined global functions or variables:
      getS3method
    Consider adding
      importFrom("utils", "getS3method")
    to your NAMESPACE file.
    ```

# petro.One

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rgraphviz’ ‘cluster’ ‘graph’
      All declared Imports should be used.
    ```

# PGA

Version: 1.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        extdata   1.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘biomaRt:::martBM’ ‘biomaRt:::martDataset’ ‘biomaRt:::martHost’
      ‘customProDB:::makeTranscriptDbFromBiomart_archive’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    reportSNV: no visible binding for global variable ‘isUnique’
    reportSNV: no visible binding for global variable ‘Index’
    reportSNV: no visible binding for global variable ‘aaref’
    reportSNV: no visible binding for global variable ‘aavar’
    reportSNV: no visible binding for global variable ‘genename’
    reportSNV: no visible binding for global variable ‘proname’
    reportSNV: no visible binding for global variable ‘.SD’
    reportSNV: no visible binding for global variable ‘ID’
    reportSNV: no visible binding for global variable ‘Change’
    reportSNV: no visible binding for global variable ‘aapos’
    reportSNV: no visible binding for global variable ‘abc’
    reportSNV: no visible binding for global variable ‘xyz’
    Undefined global functions or variables:
      . .I .N .SD CUFF_ID Change Class Evalue Frame Freq ID Index Mass
      MutNum Query Qvalue Strand Substring Type aapos aaref aavar abc
      alleleCount alleles charge chr chrom cumlen delta_da delta_ppm evalue
      gene_name genename genome<- id isSAP isUnique junType jun_type label
      miss mods mrnaAcc mz name output pep peptide pincoding position
      pro_name proname prot protAcc protein rbindlist readAAStringSet
      readDNAStringSet refbase rsid seqlengths seqlevels seqlevels<- subseq
      transcript tx_name txid txname varbase writeXStringSet x xyz y
    ```

# PGPC

Version: 1.4.0

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘EBImage’ ‘imageHTS’
    
    Depends: includes the non-default packages:
      ‘EBImage’ ‘imageHTS’ ‘SearchTrees’ ‘limma’ ‘RColorBrewer’ ‘gplots’
      ‘splots’ ‘ggplot2’ ‘geneplotter’ ‘ChemmineR’ ‘reshape2’ ‘plyr’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# PGRdup

Version: 0.2.3.2

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘RecordLinkage’
    ```

# PhaseType

Version: 0.1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      ‘ggplot2’ ‘reshape’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    plot.phtMCMC: no visible global function definition for ‘ggplot’
    plot.phtMCMC: no visible global function definition for ‘melt’
    plot.phtMCMC: no visible global function definition for ‘geom_line’
    plot.phtMCMC: no visible global function definition for ‘aes_string’
    plot.phtMCMC: no visible global function definition for ‘geom_smooth’
    plot.phtMCMC: no visible global function definition for ‘facet_wrap’
    plot.phtMCMC: no visible global function definition for ‘ggtitle’
    plot.phtMCMC: no visible global function definition for ‘xlab’
    plot.phtMCMC: no visible global function definition for ‘ylab’
    plot.phtMCMC: no visible global function definition for ‘geom_density’
    Undefined global functions or variables:
      aes_string facet_wrap geom_density geom_line geom_smooth ggplot
      ggtitle melt xlab ylab
    ```

*   checking compiled code ... NOTE
    ```
    File ‘PhaseType/libs/PhaseType.so’:
      Found ‘rand’, possibly from ‘rand’ (C)
        Object: ‘LJMA_arms.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# phenoTest

Version: 1.24.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘org.Ce.eg.db’ ‘org.Mm.eg.db’ ‘org.Rn.eg.db’ ‘org.Dm.eg.db’
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    Packages listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘annotate’ ‘GSEABase’
    A package should be listed in only one of these fields.
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘phenoTest’ for: ‘show’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘ggplot2’ which was already attached by Depends.
      Please remove these calls from your code.
    Namespace in Imports field not imported from: ‘annotate’
      All declared Imports should be used.
    Packages in Depends field not imported from:
      ‘BMA’ ‘Heatplus’ ‘annotate’ ‘ggplot2’ ‘methods’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘sortDragHtmlTable’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      gray hclust heatmap_plus image integrate kruskal.test layout legend
      lines lm median model.matrix new p.adjust pData par pc1 pc2 pchisq
      pdf png pnorm polygon prcomp predict qplot quantile rect rgb sd text
      theme wilcox.test write.csv y
    Consider adding
      importFrom("grDevices", "densCols", "dev.off", "gray", "pdf", "png",
                 "rgb")
      importFrom("graphics", "abline", "axTicks", "axis", "barplot",
                 "boxplot", "dotchart", "image", "layout", "legend", "lines",
                 "par", "polygon", "rect", "text")
      importFrom("methods", "new")
      importFrom("stats", "BIC", "anova", "approx", "approxfun",
                 "as.dendrogram", "as.dist", "binom.test", "chisq.test",
                 "coef", "coefficients", "complete.cases", "confint", "cor",
                 "cov", "cutree", "density", "dist", "hclust", "integrate",
                 "kruskal.test", "lm", "median", "model.matrix", "p.adjust",
                 "pchisq", "pnorm", "prcomp", "predict", "quantile", "sd",
                 "wilcox.test")
      importFrom("utils", "write.csv")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'plot.gseaData':
      ‘plot.gseaData’
    
    S3 methods shown with full name in documentation object 'plot.gseaSignatures':
      ‘plot.gseaSignaturesSign’
    
    S3 methods shown with full name in documentation object 'summary.gseaData':
      ‘summary.gseaData’
    
    S3 methods shown with full name in documentation object 'summary.gseaSignificance':
      ‘summary.gseaSignificanceSign’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# philr

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        intersect, setdiff, setequal, union
    
    Found more than one class "phylo" in cache; using the first, from namespace 'treeio'
    Also defined by 'phyloseq'
    Found more than one class "phylo" in cache; using the first, from namespace 'treeio'
    Also defined by 'phyloseq'
    
    Attaching package: 'tidyr'
    
    The following object is masked from 'package:ggtree':
    
        expand
    
    The following object is masked from 'package:Matrix':
    
        expand
    
    Error: processing vignette 'philr-intro.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    name.balance: no visible global function definition for ‘as’
    vote.annotation: no visible global function definition for ‘is’
    Undefined global functions or variables:
      as is
    Consider adding
      importFrom("methods", "as", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘compositions’
    ```

# PhyInformR

Version: 1.0

## In both

*   checking whether package ‘PhyInformR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/PhyInformR/new/PhyInformR.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘gplots’ ‘phytools’
      All declared Imports should be used.
    ```

# phyloseq

Version: 1.20.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘phyloseq-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: UniFrac
    > ### Title: Calculate weighted or unweighted (Fast) UniFrac distance for all
    > ###   sample pairs.
    > ### Aliases: UniFrac UniFrac,phyloseq-method
    > 
    > ### ** Examples
    > 
    > ################################################################################
    > # Perform UniFrac on esophagus data
    > ################################################################################
    > data("esophagus")
    > (y <- UniFrac(esophagus, TRUE))
    Error in .C(ape:::node_depth_edgelength, PACKAGE = "ape", as.integer(Ntip),  : 
      Incorrect number of arguments (7), expecting 5 for 'node_depth_edgelength'
    Calls: UniFrac ... UniFrac -> fastUniFrac -> ape_node_depth_edge_length -> .C
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat-phyloseq.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 541 SKIPPED: 0 FAILED: 62
      1. Failure: The specialized read_tree_greengenes function works: (@test-IO.R#323) 
      2. Error: UniFrac produces correct values on an example subset from Global Patterns. 'Correct' values are results from pyCogent (@test-distance.R#33) 
      3. Error: Check that regular-expression matching for unifrac method flag is working (@test-distance.R#53) 
      4. Failure: all 4 plot_ordination type options result in valid ggplot2 object (@test-plot.R#51) 
      5. Failure: all 4 plot_ordination type options result in valid ggplot2 object (@test-plot.R#52) 
      6. Failure: all 4 plot_ordination type options result in valid ggplot2 object (@test-plot.R#53) 
      7. Failure: all 4 plot_ordination type options result in valid ggplot2 object (@test-plot.R#54) 
      8. Failure: plot_ordination: When variables are present or not, color SampleType (@test-plot.R#83) 
      9. Failure: plot_ordination: When variables are present or not, color SampleType (@test-plot.R#84) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 181-183 (phyloseq-FAQ.Rmd) 
    Error: processing vignette 'phyloseq-FAQ.Rmd' failed with diagnostics:
    Incorrect number of arguments (7), expecting 5 for 'node_depth_edgelength'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘doParallel’
    ```

*   checking dependencies in R code ... NOTE
    ```
    ':::' calls which should be '::':
      ‘ape:::node_depth_edgelength’ ‘ape:::node_height’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking foreign function calls ... NOTE
    ```
    Foreign function calls to a different package:
      .C(ape:::node_depth_edgelength, ..., PACKAGE = "ape")
      .C(ape:::node_height, ..., PACKAGE = "ape")
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    merge_phyloseq_pair,sample_data-sample_data : <anonymous>: no visible
      binding for global variable ‘X0’
    merge_samples,sample_data: no visible global function definition for
      ‘aggregate’
    plot_phyloseq,phyloseq: no visible binding for global variable
      ‘esophagus’
    Undefined global functions or variables:
      #OTU ID .SD := Abundance Classification Consensus Lineage J OTU
      OTULabel SE.sim Sample V1 V2 X0 aggregate as.dist as.formula
      as.hclust axis capture.output combn complete.cases count cutree
      dcast.data.table download.file eigenvalue esophagus gap h.adj.index
      head i k label queryID queryString read read.table relevel se tail
      untar unzip value vmax vmin write.table x xdodge xend xfartiplab
      xleft xright y yend
    Consider adding
      importFrom("graphics", "axis")
      importFrom("stats", "aggregate", "as.dist", "as.formula", "as.hclust",
                 "complete.cases", "cutree", "relevel")
      importFrom("utils", "capture.output", "combn", "download.file", "head",
                 "read.table", "tail", "untar", "unzip", "write.table")
    to your NAMESPACE file.
    ```

# Pi

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.1Mb
      sub-directories of 1Mb or more:
        doc   5.2Mb
    ```

# pifpaf

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gridExtra’
      All declared Imports should be used.
    ```

# pitchRx

Version: 1.8.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘ggsubplot’
    ```

# pkggraph

Version: 0.2.2

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 34-58 (vignette_pkggraph.Rmd) 
    Error: processing vignette 'vignette_pkggraph.Rmd' failed with diagnostics:
    Columns `xend`, `yend`, `x`, `y` must be 1d atomic vectors or lists
    Execution halted
    ```

# PKgraph

Version: 1.7

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgetsRGtk2’
    
    Depends: includes the non-default packages:
      ‘RGtk2’ ‘gWidgetsRGtk2’ ‘cairoDevice’ ‘lattice’ ‘rggobi’ ‘ggplot2’
      ‘proto’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# PKPDmisc

Version: 2.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘purrr’
      All declared Imports should be used.
    ```

# PKreport

Version: 1.5

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    generate.plot: no visible global function definition for ‘dev.off’
    writeHTML: no visible global function definition for
      ‘packageDescription’
    writeLst: no visible global function definition for
      ‘packageDescription’
    writeLst.tab: no visible global function definition for
      ‘packageDescription’
    writeTable: no visible global function definition for
      ‘packageDescription’
    writeTable: no visible global function definition for ‘quantile’
    initialize,nonmem: no visible global function definition for
      ‘read.table’
    Undefined global functions or variables:
      bmp browseURL dev.cur dev.new dev.off formula heatmap jpeg
      packageDescription png quantile read.table tiff
    Consider adding
      importFrom("grDevices", "bmp", "dev.cur", "dev.new", "dev.off", "jpeg",
                 "png", "tiff")
      importFrom("stats", "formula", "heatmap", "quantile")
      importFrom("utils", "browseURL", "packageDescription", "read.table")
    to your NAMESPACE file.
    ```

# planar

Version: 1.6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.9Mb
      sub-directories of 1Mb or more:
        libs   5.4Mb
    ```

# platetools

Version: 0.0.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > ### ** Examples
    > 
    > df01 <- data.frame(well = num_to_well(1:96),
    +   vals = rnorm(96),
    +   plate = 1)
    > 
    > df02 <- data.frame(well = num_to_well(1:96),
    +   vals = rnorm(96),
    +   plate = 2)
    > 
    > df <- rbind(df01, df02)
    > 
    > b_grid(data = df$vals,
    +     well = df$well,
    +     plate_id = df$plate,
    +     plate = 96)
    Warning: `panel.margin.x` is deprecated. Please use `panel.spacing.x` property instead
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Error: Free scales are only supported with `coord_cartesian()` and `coord_flip()`
    Execution halted
    ```

# playwith

Version: 0.9-54

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgetsRGtk2’
    
    Package suggested but not available for checking: ‘latticist’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# plethy

Version: 1.14.0

## In both

*   checking examples ... ERROR
    ```
    ...
    > count = c(NA,900, NA,150, 150, 110)
    > measure_break = c(FALSE, FALSE, TRUE, FALSE, FALSE,FALSE)
    > table_break = c(TRUE, rep(FALSE, length(samples)-1))
    > phase = rep("D1", length(samples))
    >     
    > err.dta <- data.frame(samples=samples, count=count, measure_break=measure_break, table_break=table_break, phase=phase, stringsAsFactors=FALSE)
    >     
    > sim.bux.lines <- plethy:::generate.sample.buxco(err.dta)
    >     
    > temp.file <- tempfile()
    > temp.db.file <- tempfile()
    > write(sim.bux.lines, file=temp.file)
    > test.bux.db <- parse.buxco(file.name=temp.file, db.name=temp.db.file, chunk.size=10000)
    Processing /home/muelleki/tmp/RtmpbAgMTr/fileec16587710f9 in chunks of 10000
    Starting chunk 1
    Reached breakpoint change
    Processing breakpoint 1
    Starting sample sample_1
    Error in if (sum(which.gt) > 0) { : missing value where TRUE/FALSE needed
    Calls: parse.buxco ... write.sample.breaks -> write.sample.db -> sanity.check.time
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
      Test files with failing tests
      
         test_check_helpers.R 
           test.add.labels.by.sample 
           test.dbImport 
           test.get.err.breaks 
           test.summaryMeasures 
      
      
      Error in BiocGenerics:::testPackage("plethy") : 
        unit tests failed for package plethy
      In addition: Warning message:
      In .Internal(gc(verbose, reset)) :
        closing unused connection 3 (/home/muelleki/tmp/RtmpX9oekr/filef53251cebac7)
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘csv.to.table’ ‘find.break.ranges.integer’ ‘fix.time’ ‘multi.grep’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    mvtsplot.data.frame: no visible global function definition for ‘bxp’
    mvtsplot.data.frame : <anonymous>: no visible binding for global
      variable ‘median’
    mvtsplot.data.frame: no visible global function definition for ‘lines’
    mvtsplot.data.frame: no visible global function definition for ‘Axis’
    mvtsplot.data.frame: no visible global function definition for ‘legend’
    retrieveMatrix,BuxcoDB: no visible global function definition for
      ‘terms’
    tsplot,BuxcoDB: no visible binding for global variable ‘Days’
    tsplot,BuxcoDB: no visible binding for global variable ‘Value’
    tsplot,BuxcoDB: no visible binding for global variable ‘Sample_Name’
    Undefined global functions or variables:
      Axis Days Sample_Name Value abline bxp colors layout legend lines
      median mtext packageDescription par plot rnorm strwidth terms
    Consider adding
      importFrom("grDevices", "colors")
      importFrom("graphics", "Axis", "abline", "bxp", "layout", "legend",
                 "lines", "mtext", "par", "plot", "strwidth")
      importFrom("stats", "median", "rnorm", "terms")
      importFrom("utils", "packageDescription")
    to your NAMESPACE file.
    ```

# plotluck

Version: 1.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘hexbin’ ‘quantreg’
      All declared Imports should be used.
    ```

# plotly

Version: 4.7.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 1252 SKIPPED: 2 FAILED: 18
      1. Error: api() returns endpoints (@test-api.R#7) 
      2. Error: Can search with white-space (@test-api.R#16) 
      3. Error: Changing a filename works (@test-api.R#25) 
      4. Error: Downloading plots works (@test-api.R#35) 
      5. Error: Downloading grids works (@test-api.R#53) 
      6. Error: Creating produces a new file by default (@test-api.R#79) 
      7. Error: Can overwrite a grid (@test-api.R#95) 
      8. Error: Can overwrite a plot (@test-api.R#107) 
      9. Error: Can create plots with non-trivial src attributes (@test-api.R#123) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# politeness

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ROCR’
      All declared Imports should be used.
    ```

# popEpi

Version: 0.4.4

## In both

*   R CMD check timed out
    

# PopGenReport

Version: 3.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ecodist’
    ```

# popReconstruct

Version: 1.0-4

## In both

*   checking R code for possible problems ... NOTE
    ```
    estMod.rinvGamma.mar29: no visible global function definition for
      ‘rgamma’
    life.expectancy.stationary: no visible global function definition for
      ‘head’
    life.expectancy.stationary: no visible global function definition for
      ‘tail’
    log.lhood.mar29: no visible global function definition for ‘dnorm’
    log.post.mar29: no visible global function definition for ‘dnorm’
    popRecon.sampler: no visible global function definition for ‘rnorm’
    popRecon.sampler: no visible global function definition for ‘runif’
    Undefined global functions or variables:
      dnorm head rgamma rnorm runif tail
    Consider adding
      importFrom("stats", "dnorm", "rgamma", "rnorm", "runif")
      importFrom("utils", "head", "tail")
    to your NAMESPACE file.
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    
    The following object is masked from ‘package:base’:
    
        startsWith
    
    Calculating life expectancy at birth ...
    ... done
    Calculating net number of migrants ...
    ... done
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'burkina-faso-females.tex' failed.
    BibTeX errors:
    The top-level auxiliary file: burkina-faso-females.aux
    I couldn't open style file apa.bst
    ---line 19 of file burkina-faso-females.aux
     : \bibstyle{apa
     :              }
    I'm skipping whatever remains of this command
    I found no style file---while reading file burkina-faso-females.aux
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# PortfolioEffectHFT

Version: 1.8

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        java   5.1Mb
    ```

# POUMM

Version: 1.3.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# powerlmm

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘testthat’
      All declared Imports should be used.
    ```

# PPInfer

Version: 1.2.4

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘biomaRt’ ‘fgsea’ ‘kernlab’ ‘ggplot2’ ‘igraph’ ‘STRINGdb’
      ‘yeastExpData’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    ORA: no visible global function definition for ‘setTxtProgressBar’
    ORA: no visible global function definition for ‘p.adjust’
    ORA.dotplot: no visible global function definition for ‘p.adjust’
    enrich.net: no visible global function definition for ‘stack’
    enrich.net: no visible global function definition for ‘adjustcolor’
    enrich.net : <anonymous>: no visible global function definition for
      ‘adjustcolor’
    enrich.net: no visible binding for global variable ‘legend’
    net.infer: no visible global function definition for ‘na.omit’
    net.infer.ST: no visible global function definition for ‘na.omit’
    ppi.infer.human: no visible global function definition for ‘na.omit’
    ppi.infer.mouse: no visible global function definition for ‘na.omit’
    Undefined global functions or variables:
      adjustcolor fisher.test legend na.omit p.adjust setTxtProgressBar
      stack txtProgressBar
    Consider adding
      importFrom("grDevices", "adjustcolor")
      importFrom("graphics", "legend")
      importFrom("stats", "fisher.test", "na.omit", "p.adjust")
      importFrom("utils", "setTxtProgressBar", "stack", "txtProgressBar")
    to your NAMESPACE file.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' call not declared from: ‘httr’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
        as.data.frame, cbind, colMeans, colSums, colnames, do.call,
        duplicated, eval, evalq, get, grep, grepl, intersect, is.unsorted,
        lapply, lengths, mapply, match, mget, order, paste, pmax, pmax.int,
        pmin, pmin.int, rank, rbind, rowMeans, rowSums, rownames, sapply,
        setdiff, sort, table, tapply, union, unique, unsplit, which,
        which.max, which.min
    
    
    Attaching package: ‘graph’
    
    The following objects are masked from ‘package:igraph’:
    
        degree, edges, intersection
    
    Error in net.infer(names(V(sg))[1:50], K, top = 20) : 
      size of list is too large
    
    Error: processing vignette 'PPInfer.Rnw' failed with diagnostics:
     chunk 14 
    Error in library(httr) : there is no package called ‘httr’
    Execution halted
    ```

# PPtreeViz

Version: 2.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# pqsfinder

Version: 1.4.9

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: XVector
    
    Attaching package: 'Biostrings'
    
    The following object is masked from 'package:base':
    
        strsplit
    
    Loading required package: GenomicRanges
    Loading required package: GenomeInfoDb
    Loading required package: grid
    Loading required package: BSgenome
    Error: processing vignette 'pqsfinder.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking if this is a source package ... NOTE
    ```
    Found the following apparent object files/libraries:
      src/boost_regex/c_regex_traits.o src/boost_regex/cpp_regex_traits.o
      src/boost_regex/cregex.o src/boost_regex/fileiter.o
      src/boost_regex/icu.o src/boost_regex/instances.o
      src/boost_regex/posix_api.o src/boost_regex/regex.o
      src/boost_regex/regex_debug.o src/boost_regex/regex_raw_buffer.o
      src/boost_regex/regex_traits_defaults.o
      src/boost_regex/static_mutex.o src/boost_regex/usinstances.o
      src/boost_regex/w32_regex_traits.o src/boost_regex/wc_regex_traits.o
      src/boost_regex/wide_posix_api.o src/boost_regex/winstances.o
    Object files/libraries should not be included in a source package.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 24.9Mb
      sub-directories of 1Mb or more:
        lib   20.3Mb
        libs   4.0Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Versioned 'LinkingTo' value for ‘BH’ is only usable in R >= 3.0.2
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# precrec

Version: 0.9.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.4Mb
      sub-directories of 1Mb or more:
        doc    1.2Mb
        libs   7.4Mb
    ```

# predictmeans

Version: 0.99

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘predictmeans-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: covariatemeans
    > ### Title: Predicted Means of a Linear Model with Covariate Variable(s)
    > ### Aliases: covariatemeans
    > 
    > ### ** Examples
    > 
    >   library(predictmeans)
    >   data(Oats, package="nlme")
    >   fm <- lme(yield ~ nitro*Variety, random=~1|Block/Variety, data=Oats)
    > # library(lme4)
    > # fm <- lmer(yield ~ nitro*Variety+(1|Block/Variety), data=Oats)
    >   covariatemeans(fm, "Variety", covariate="nitro")
    dev.new(): using pdf(file="Rplots2.pdf")
    Error: Column `y` must be a 1d atomic vector or a list
    Execution halted
    ```

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    terms.gls: no visible global function definition for ‘model.frame’
    Undefined global functions or variables:
      abline anova as.formula axis box coef delete.response dev.new edit
      family fitted formula ftable identify image legend lines lm.influence
      loess.smooth model.frame model.matrix na.omit na.pass p.adjust par
      plot points ppoints predict pt ptukey qnorm qqline qqnorm qt resid
      residuals slot terms text title update vcov xtabs
    Consider adding
      importFrom("grDevices", "dev.new")
      importFrom("graphics", "abline", "axis", "box", "identify", "image",
                 "legend", "lines", "par", "plot", "points", "text", "title")
      importFrom("methods", "slot")
      importFrom("stats", "anova", "as.formula", "coef", "delete.response",
                 "family", "fitted", "formula", "ftable", "lm.influence",
                 "loess.smooth", "model.frame", "model.matrix", "na.omit",
                 "na.pass", "p.adjust", "ppoints", "predict", "pt", "ptukey",
                 "qnorm", "qqline", "qqnorm", "qt", "resid", "residuals",
                 "terms", "update", "vcov", "xtabs")
      importFrom("utils", "edit")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# PredictTestbench

Version: 1.1.3

## Newly broken

*   checking whether package ‘PredictTestbench’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘forecast::autolayer’ by ‘ggplot2::autolayer’ when loading ‘PredictTestbench’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/PredictTestbench/new/PredictTestbench.Rcheck/00install.out’ for details.
    ```

## In both

*   checking R code for possible problems ... NOTE
    ```
    prediction_errors: possible error in psf(data = dataIn1, n.ahead =
      nextVal): unused argument (n.ahead = nextVal)
    ```

# PReMiuM

Version: 3.1.6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.3Mb
      sub-directories of 1Mb or more:
        libs  13.1Mb
    ```

# preprosim

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘e1071’
      All declared Imports should be used.
    ```

# prevR

Version: 3.3

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘sparr’
    ```

# primerTree

Version: 1.0.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Error in .requirePackage(package) : 
        unable to find required package 'RCurl'
      Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
      Execution halted
    ```

# prism

Version: 0.0.7

## Newly broken

*   checking whether package ‘prism’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::calc’ by ‘raster::calc’ when loading ‘prism’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/prism/new/prism.Rcheck/00install.out’ for details.
    ```

# profr

Version: 0.3.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘ggplot2’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    ggplot.profr: no visible binding for global variable ‘start’
    ggplot.profr: no visible binding for global variable ‘end’
    ggplot.profr: no visible global function definition for ‘geom_text’
    ggplot.profr: no visible binding for global variable ‘time’
    ggplot.profr: no visible global function definition for
      ‘scale_y_continuous’
    ggplot.profr: no visible global function definition for
      ‘scale_x_continuous’
    plot.profr: no visible global function definition for ‘plot’
    plot.profr: no visible global function definition for ‘rect’
    plot.profr: no visible binding for global variable ‘time’
    plot.profr: no visible global function definition for ‘text’
    profr: no visible global function definition for ‘Rprof’
    Undefined global functions or variables:
      Rprof aes end geom_rect geom_text ggplot hist plot rect
      scale_x_continuous scale_y_continuous start text time
    Consider adding
      importFrom("graphics", "hist", "plot", "rect", "text")
      importFrom("stats", "end", "start", "time")
      importFrom("utils", "Rprof")
    to your NAMESPACE file.
    ```

# ProgGUIinR

Version: 0.0-4

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘qtbase’ ‘gWidgetsRGtk2’
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      definition for ‘apropos’
    showGtkWidgetInfo : getMethodForObject: no visible global function
      definition for ‘help’
    showGtkWidgetInfo: no visible global function definition for
      ‘gSignalConnect’
    Undefined global functions or variables:
      PangoStyle addSpring apropos gSignalConnect gbutton gframe ggroup
      gnotebook gpanedgroup gstatusbar gtable gtext gtkAction
      gtkCellRendererText gtkEntry gtkEntryCompletionNew gtkFrame gtkHBox
      gtkHBoxNew gtkHPaned gtkLabel gtkListStore gtkNotebook
      gtkScrolledWindow gtkSeparatorToolItem gtkStatusbar gtkTextBuffer
      gtkTextView gtkToolButton gtkToolbar gtkTreeView gtkTreeViewColumn
      gtkTypeGetSignals gtkVBox gtkVBoxNew gtkWindow gwindow help
      rGtkDataFrame svalue svalue<- tcl tclvalue tkbind tkconfigure
      tkdelete tkget tkgrid tkgrid.columnconfigure tkgrid.rowconfigure
      tkinsert tkmenu tkpack tksee tkset tktag.add tktag.configure
      tktag.remove tktext tktoplevel tkwm.title tkxview ttkbutton ttkframe
      ttklabel ttkpanedwindow ttkscrollbar ttktreeview visible<-
    Consider adding
      importFrom("utils", "apropos", "help")
    to your NAMESPACE file.
    ```

# projmanr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# pRoloc

Version: 1.16.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        nChrom
    
    Loading required package: cluster
    Warning: replacing previous import 'BiocGenerics::var' by 'stats::var' when loading 'MLInterfaces'
    
    This is pRoloc version 1.16.1 
      Read '?pRoloc' and references therein for information
      about the package and how to get started.
    
    
    This is pRolocdata version 1.14.0.
    Use 'pRolocdata()' to list available data sets.
    Loading required namespace: GO.db
    
    Loading required package: GO.db
    Retaining 84 out of 524 in GOAnnotations
    Retaining 79 out of 84 in GOAnnotations
    Error: processing vignette 'pRoloc-goannotations.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   3.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘MLInterfaces:::.macroF1’ ‘MLInterfaces:::.precision’
      ‘MLInterfaces:::.recall’ ‘MLInterfaces:::es2df’
      ‘caret:::predict.plsda’
      See the note in ?`:::` about the use of this operator.
    There are ::: calls to the package's namespace in its code. A package
      almost never needs to use ::: for its own objects:
      ‘checkSortedFeatureNames’ ‘opt’
    ```

*   checking R code for possible problems ... NOTE
    ```
    Found the following possibly unsafe calls:
    File ‘pRoloc/R/annotation.R’:
      unlockBinding("params", .pRolocEnv)
    ```

*   checking files in ‘vignettes’ ... NOTE
    ```
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

# pRolocGUI

Version: 1.10.0

## In both

*   checking whether package ‘pRolocGUI’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘lme4’ is not available and has been replaced
      Warning: namespace ‘MatrixModels’ is not available and has been replaced
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/pRolocGUI/new/pRolocGUI.Rcheck/00install.out’ for details.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Authors@R field gives more than one person with maintainer role:
      Lisa Breckels <lms79@cam.ac.uk> [aut, cre]
      Laurent Gatto <lg390@cam.ac.uk> [aut, cre]
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘pRoloc:::remap’
      See the note in ?`:::` about the use of this operator.
    ```

# prophet

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 22.9Mb
      sub-directories of 1Mb or more:
        libs  21.7Mb
    ```

# proportion

Version: 2.0.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘binom’, ‘PropCIs’, ‘BlakerCI’, ‘prevalence’
    ```

# propr

Version: 3.1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        libs   2.8Mb
    ```

# proteomics

Version: 0.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    addIonSatistics: no visible binding for global variable ‘median’
    adjustOne: no visible binding for global variable ‘median’
    attachModifications: no visible global function definition for
      ‘na.omit’
    avrgLoadingCalculation: no visible binding for global variable ‘median’
    channelResponses : estimate: no visible binding for global variable
      ‘var’
    responseStatisics : func: no visible global function definition for
      ‘aggregate’
    testing: no visible global function definition for ‘aov’
    testingOneshot: no visible global function definition for ‘anova’
    testingTukey: no visible global function definition for ‘TukeyHSD’
    Undefined global functions or variables:
      TukeyHSD aggregate anova aov median na.omit var
    Consider adding
      importFrom("stats", "TukeyHSD", "aggregate", "anova", "aov", "median",
                 "na.omit", "var")
    to your NAMESPACE file.
    ```

# proteoQC

Version: 1.12.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'proteoQC.Rmd' failed with diagnostics:
    there is no package called ‘prettydoc’
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        doc       3.2Mb
        extdata   4.0Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotMS2boxplot: no visible binding for global variable ‘techRep’
    plotMS2boxplot: no visible binding for global variable ‘fraction’
    plotMS2boxplot: no visible binding for global variable ‘MS2QC’
    plotSampleIDResultErrorBar: no visible binding for global variable
      ‘fraction’
    plotSampleIDResultErrorBar: no visible binding for global variable
      ‘val’
    plotSampleIDResultErrorBar: no visible binding for global variable ‘se’
    plotSampleVenn: no visible global function definition for ‘grid.draw’
    plotTechRepVenn : <anonymous>: no visible global function definition
      for ‘grid.draw’
    qcHist: no visible binding for global variable ‘error’
    qcHist: no visible binding for global variable ‘techRep’
    qcHist: no visible binding for global variable ‘bioRep’
    qcHist2: no visible binding for global variable ‘error’
    qcHist2: no visible binding for global variable ‘fractile’
    Undefined global functions or variables:
      ..count.. Intensity MS1QC MS2QC TMT10 TMT6 Tag V1 V2 V3 V4 V5 bioRep
      curenv delta error exprs fractile fraction grid.draw iTRAQ4 iTRAQ8
      label peplength peptide_summary precursorCharge quantify ratio
      readMgfData se techRep val x y
    ```

# PSAboot

Version: 1.1.4

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# pscore

Version: 0.1-2

## In both

*   checking R code for possible problems ... NOTE
    ```
    mahalanobisComposite: no visible global function definition for
      ‘princomp’
    prepareComposite: no visible global function definition for ‘cov’
    winsorizor : f: no visible global function definition for ‘quantile’
    Undefined global functions or variables:
      cov princomp quantile
    Consider adding
      importFrom("stats", "cov", "princomp", "quantile")
    to your NAMESPACE file.
    ```

# psd

Version: 1.0-1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘fftw’
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    psdcore.default: no visible global function definition for ‘ts’
    pspectrum.ts: no visible global function definition for ‘frequency’
    spec_confint.default: no visible global function definition for
      ‘pchisq’
    spec_confint.default: no visible global function definition for
      ‘qchisq’
    splineGrad.default: no visible global function definition for ‘par’
    splineGrad.default: no visible global function definition for ‘plot’
    splineGrad.default: no visible global function definition for ‘lines’
    Undefined global functions or variables:
      abline acf as.ts frequency head is.ts layout legend lines lm
      loess.control mtext par pchisq plot qchisq residuals spec.pgram start
      tail title ts var
    Consider adding
      importFrom("graphics", "abline", "layout", "legend", "lines", "mtext",
                 "par", "plot", "title")
      importFrom("stats", "acf", "as.ts", "frequency", "is.ts", "lm",
                 "loess.control", "pchisq", "qchisq", "residuals",
                 "spec.pgram", "start", "ts", "var")
      importFrom("utils", "head", "tail")
    to your NAMESPACE file.
    ```

# psda

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘raster’
      All declared Imports should be used.
    ```

# psychmeta

Version: 0.1.2

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
    ```

# psycho

Version: 0.0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘rtf’ ‘tidyverse’
      All declared Imports should be used.
    ```

# psygenet2r

Version: 1.9.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 2-32 (case_study.Rmd) 
    Error: processing vignette 'case_study.Rmd' failed with diagnostics:
    could not find function "doc_date"
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.8Mb
      sub-directories of 1Mb or more:
        doc   5.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    package 'methods' is used but not declared
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘Disease1’
    plot,JaccardIndexPsy-ANY: no visible binding for global variable
      ‘Disease2’
    plot,JaccardIndexPsy-ANY: no visible binding for global variable
      ‘JaccardIndex’
    plot,JaccardIndexPsy-ANY: no visible binding for global variable
      ‘value’
    plot,JaccardIndexPsy-ANY: no visible binding for global variable
      ‘variable’
    Undefined global functions or variables:
      Category Disease1 Disease2 JaccardIndex Var1 c0.Number_of_Abstracts
      c0.Score c1.Gene_Symbol c2.DiseaseName c2.Disease_code
      c2.PsychiatricDisorder category combn database diseases gene new perc
      phyper pie read.csv read.delim value variable
    Consider adding
      importFrom("graphics", "pie")
      importFrom("methods", "new")
      importFrom("stats", "phyper")
      importFrom("utils", "combn", "read.csv", "read.delim")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# PTXQC

Version: 0.92.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        doc        3.8Mb
        examples   2.3Mb
    ```

# PureCN

Version: 1.6.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc       1.6Mb
        extdata   2.7Mb
    ```

# pxweb

Version: 0.6.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      
      2. Failure: baseURL 1 (@test-get_pxweb_metadata.R#23) --------------------------
      dim(test_file) not equivalent to test$test_dims.
      1/2 mismatches
      [1] 17 - 16 == 1
      
      
      testthat results ================================================================
      OK: 120 SKIPPED: 10 FAILED: 2
      1. Failure: baseURL 1 (@test-get_pxweb_metadata.R#23) 
      2. Failure: baseURL 1 (@test-get_pxweb_metadata.R#23) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# qcmetrics

Version: 1.14.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    n15qc : <anonymous>: no visible global function definition for ‘median’
    n15qc : <anonymous>: no visible global function definition for ‘legend’
    n15qc : <anonymous>: no visible global function definition for ‘par’
    n15qc : <anonymous>: no visible global function definition for ‘layout’
    n15qc : <anonymous>: no visible global function definition for
      ‘barplot’
    n15qc: no visible global function definition for ‘fileNames’
    n15qc: no visible global function definition for ‘experimentData’
    rnadeg : <anonymous>: no visible global function definition for
      ‘legend’
    rnadeg : <anonymous>: no visible global function definition for ‘par’
    Undefined global functions or variables:
      abline aes barplot combineFeatures density dev.off experimentData
      exprs fData fData<- fileNames fvarLabels geom_boxplot geom_hline
      geom_jitter ggplot labs layout legend lines median par pdf png rect
    Consider adding
      importFrom("grDevices", "dev.off", "pdf", "png")
      importFrom("graphics", "abline", "barplot", "layout", "legend",
                 "lines", "par", "rect")
      importFrom("stats", "density", "median")
    to your NAMESPACE file.
    ```

# qdap

Version: 2.2.9

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gplots’
    ```

# qrqc

Version: 1.30.0

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘reshape’ ‘ggplot2’ ‘Biostrings’ ‘biovizBase’ ‘brew’ ‘xtable’
      ‘Rsamtools’ ‘testthat’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Packages listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘reshape’ ‘ggplot2’ ‘Biostrings’ ‘biovizBase’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Packages in Depends field not imported from:
      'Rsamtools' 'brew' 'testthat' 'xtable'
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    kmerKLPlot,SequenceSummary: no visible binding for global variable
      'kmer'
    kmerKLPlot,SequenceSummary: no visible binding for global variable
      'position'
    kmerKLPlot,SequenceSummary: no visible binding for global variable 'kl'
    kmerKLPlot,list : <anonymous>: no visible binding for global variable
      'kmer'
    kmerKLPlot,list: no visible binding for global variable 'position'
    kmerKLPlot,list: no visible binding for global variable 'kl'
    kmerKLPlot,list: no visible binding for global variable 'kmer'
    plotGC,SequenceSummary : <local>: no visible global function definition
      for 'aggregate'
    qualPlot,FASTQSummary: no visible binding for global variable
      'position'
    qualPlot,list: no visible binding for global variable 'position'
    Undefined global functions or variables:
      DNAStringSet aggregate base entropy kl kmer na.exclude position
      quantile write.XStringSet
    Consider adding
      importFrom("stats", "aggregate", "na.exclude", "quantile")
    to your NAMESPACE file.
    ```

# quadrupen

Version: 0.2-5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        libs   7.8Mb
    ```

# QualInt

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    powerval: no visible global function definition for ‘qnorm’
    powerval: no visible global function definition for ‘pnorm’
    print.expplot: no visible global function definition for ‘gray’
    print.expplot: no visible binding for global variable ‘coef’
    print.outplot: no visible global function definition for ‘gray’
    print.outplot: no visible binding for global variable ‘coef’
    qualint: no visible global function definition for ‘qnorm’
    qualint: no visible global function definition for ‘plot’
    qualval: no visible global function definition for ‘qnorm’
    qualval: no visible global function definition for ‘plot’
    surout: no visible global function definition for ‘tail’
    surout: no visible global function definition for ‘coef’
    Undefined global functions or variables:
      coef dbinom gray pchisq plot pnorm qnorm rnorm sd tail
    Consider adding
      importFrom("grDevices", "gray")
      importFrom("graphics", "plot")
      importFrom("stats", "coef", "dbinom", "pchisq", "pnorm", "qnorm",
                 "rnorm", "sd")
      importFrom("utils", "tail")
    to your NAMESPACE file.
    ```

# qualvar

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 92-106 (wilcox1973.Rmd) 
    Error: processing vignette 'wilcox1973.Rmd' failed with diagnostics:
    there is no package called 'webshot'
    Execution halted
    ```

# quanteda

Version: 0.99.22

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      all(plot_docnames[order(plot_docnames)] == plot_docnames) isn't false.
      
      
      2. Failure: test plot.kwic keeps order of keywords passed (@test-plots.R#81) ---
      as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$keyword)) not equal to c("people", "american").
      Lengths differ: 0 vs 2
      
      
      testthat results ================================================================
      OK: 1625 SKIPPED: 15 FAILED: 2
      1. Failure: test plot.kwic facet order parameter (@test-plots.R#71) 
      2. Failure: test plot.kwic keeps order of keywords passed (@test-plots.R#81) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 17.6Mb
      sub-directories of 1Mb or more:
        libs  15.4Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1415 marked UTF-8 strings
    ```

# quickPlot

Version: 0.1.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘fastshp’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rgdal’
      All declared Imports should be used.
    ```

# quickpsy

Version: 0.1.4

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘quickpsy-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plotcurves
    > ### Title: Plot the curves
    > ### Aliases: plotcurves
    > 
    > ### ** Examples
    > 
    > library(MPDiR) # contains the Vernier data
    > fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
    +                 grouping = .(Direction, WaveForm, TempFreq), B = 5)
    > plotcurves(fit)
    Warning: Ignoring unknown aesthetics: x
    Error: Aesthetics must be either length 1 or the same as the data (8): ymin, ymax, colour, x
    Execution halted
    ```

# quickReg

Version: 1.5.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘psych’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘PredictABEL’
    ```

# quokar

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MCMCpack’ ‘gridExtra’ ‘knitr’
      All declared Imports should be used.
    ```

# qvalue

Version: 2.8.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    lfdr: no visible global function definition for ‘qnorm’
    lfdr: no visible global function definition for ‘density’
    lfdr: no visible global function definition for ‘smooth.spline’
    lfdr: no visible global function definition for ‘predict’
    lfdr: no visible global function definition for ‘dnorm’
    pi0est: no visible global function definition for ‘smooth.spline’
    pi0est: no visible global function definition for ‘predict’
    pi0est: no visible global function definition for ‘quantile’
    plot.qvalue: no visible global function definition for ‘quantile’
    write.qvalue: no visible global function definition for ‘write.table’
    Undefined global functions or variables:
      density dnorm predict qnorm quantile smooth.spline write.table
    Consider adding
      importFrom("stats", "density", "dnorm", "predict", "qnorm", "quantile",
                 "smooth.spline")
      importFrom("utils", "write.table")
    to your NAMESPACE file.
    ```

# qwraps2

Version: 0.2.4

## In both

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'summary_table':
      ‘cbind.qwraps2_summary_table’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# r2glmm

Version: 0.1.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘dplyr’ ‘lmerTest’
      All declared Imports should be used.
    ```

# R3CPET

Version: 1.8.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        data      3.0Mb
        example   1.0Mb
        libs      1.7Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘methods’
    A package should be listed in only one of these fields.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: 'BiocGenerics'
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    .formatDAVIDResult: no visible global function definition for
      'formatList'
    .formatDAVIDResult: no visible global function definition for
      'formatGene2Gene'
    .formatDAVIDResult: no visible global function definition for
      'formatAnnotationReport'
    .get.NetworksGenes: no visible global function definition for
      'annotatePeakInBatch'
    EnsemblToHGNC: no visible global function definition for 'useMart'
    EnsemblToHGNC: no visible global function definition for 'useDataset'
    EnsemblToHGNC: no visible global function definition for 'getBM'
    EntrezToHGNC: no visible global function definition for 'useMart'
    EntrezToHGNC: no visible global function definition for 'useDataset'
    EntrezToHGNC: no visible global function definition for 'getBM'
    createServer,ChiapetExperimentData-NetworkCollection-ChromMaintainers:
      no visible global function definition for 'runApp'
    Undefined global functions or variables:
      TxDb.Hsapiens.UCSC.hg19.knownGene annotatePeakInBatch
      formatAnnotationReport formatGene2Gene formatGeneReport
      formatGeneReportFull formatList getBM org.Hs.eg.db org.Hs.egUCSCKG
      runApp select toTable useDataset useMart
    ```

*   checking compiled code ... NOTE
    ```
    File ‘R3CPET/libs/R3CPET.so’:
      Found ‘rand’, possibly from ‘rand’ (C)
        Object: ‘state.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# radiant.model

Version: 0.8.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# radiant.multivariate

Version: 0.8.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# rags2ridges

Version: 2.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        libs   3.7Mb
    ```

# raincpc

Version: 0.4

## In both

*   checking R code for possible problems ... NOTE
    ```
    cpc_get_rawdata: no visible global function definition for
      ‘download.file’
    Undefined global functions or variables:
      download.file
    Consider adding
      importFrom("utils", "download.file")
    to your NAMESPACE file.
    ```

# randomForestExplainer

Version: 0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘MASS’ ‘dtplyr’
      All declared Imports should be used.
    ```

# randomizeR

Version: 1.4

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: plotrix
    Warning: Computation failed in `stat_boxplot()`:
    there is no package called 'quantreg'
    Warning: Computation failed in `stat_boxplot()`:
    there is no package called 'quantreg'
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'comparison-example.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `biblatex.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.57 \bibliography
                      {vignette}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# randomUniformForest

Version: 1.1.5

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘rnorm’
    update.unsupervised: no visible global function definition for
      ‘predict’
    weightedVoteModel: no visible global function definition for ‘lm’
    which.is.duplicate: no visible global function definition for ‘na.omit’
    Undefined global functions or variables:
      abline aggregate barplot cor dev.new dev.off dist graphics.off grid
      head heat.colors kmeans legend lm median memory.limit model.frame
      model.matrix model.response mosaicplot na.omit par pbinom persp plot
      points predict qnorm quantile rnorm runif sd title
    Consider adding
      importFrom("grDevices", "dev.new", "dev.off", "graphics.off",
                 "heat.colors")
      importFrom("graphics", "abline", "barplot", "grid", "legend",
                 "mosaicplot", "par", "persp", "plot", "points", "title")
      importFrom("stats", "aggregate", "cor", "dist", "kmeans", "lm",
                 "median", "model.frame", "model.matrix", "model.response",
                 "na.omit", "pbinom", "predict", "qnorm", "quantile",
                 "rnorm", "runif", "sd")
      importFrom("utils", "head", "memory.limit")
    to your NAMESPACE file.
    ```

# rangeMapper

Version: 0.3-1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘snow’
    ```

# raptr

Version: 0.1.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      done.
      
      Building tree... 
      done.
      Ball query... 
      
      done.
      Requested probability quantile 0.500000, obtained 0.499612 - setting threshold value 0.084938.
       For a closer match, you can increase num.thresholds in hypervolume_threshold.
      testthat results ================================================================
      OK: 318 SKIPPED: 18 FAILED: 1
      1. Failure: gdal installation checks (@test-02-misc.R#6) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘gurobi’ ‘rgurobi’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        doc    1.4Mb
    ```

# Rariant

Version: 1.12.0

## In both

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: 'rbind_all' is deprecated.
    Deprecated functions may be defunct as soon as of the next release of
    R.
    See ?Deprecated.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        doc       2.3Mb
        extdata   5.2Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    tallyBamRegion: no visible global function definition for 'PileupParam'
    tallyBamRegion: no visible global function definition for
      'ScanBamParam'
    tallyBamRegion: no visible global function definition for 'pileup'
    Undefined global functions or variables:
      PileupParam ScanBamParam pileup
    ```

*   checking installed files from ‘inst/doc’ ... NOTE
    ```
    The following files should probably not be installed:
      ‘rariant-inspect-ci.png’, ‘rariant-inspect-shift.png’
    
    Consider the use of a .Rinstignore file: see ‘Writing R Extensions’,
    or move the vignette sources from ‘inst/doc’ to ‘vignettes’.
    ```

# rattle

Version: 5.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'rattle.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm2e.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.14 \usepackage
                    [^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘gWidgetsRGtk2’ ‘playwith’
    ```

*   checking dependencies in R code ... NOTE
    ```
    
    (R:36326): Gtk-WARNING **: gtk_disable_setlocale() must be called before gtk_init()
    ```

# rbefdata

Version: 0.3.5

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    bef.portal.get.attachments_for: no visible global function definition
      for ‘flush.console’
    bef.portal.get.categories_for: no visible binding for global variable
      ‘id’
    bef.portal.get.categories_for: no visible global function definition
      for ‘read.csv’
    bef.portal.get.dataset: no visible global function definition for
      ‘read.csv’
    bef.portal.get.dataset_by: no visible global function definition for
      ‘read.csv’
    bef.portal.get.datasets.for_keyword : <anonymous>: no visible global
      function definition for ‘read.csv’
    bef.portal.get.datasets.for_proposal: no visible global function
      definition for ‘read.csv’
    upload_file: no visible global function definition for ‘write.csv’
    Undefined global functions or variables:
      browseURL flush.console id read.csv write.csv
    Consider adding
      importFrom("utils", "browseURL", "flush.console", "read.csv",
                 "write.csv")
    to your NAMESPACE file.
    ```

# RBesT

Version: 1.2-3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 27.7Mb
      sub-directories of 1Mb or more:
        libs  26.4Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lme4’
      All declared Imports should be used.
    ```

# rbison

Version: 0.5.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      
      
      3. Failure: bison_tax returns the correct ... (@test-bison_tax.R#14) -----------
      out3$names$vernacularName not equal to "black bear".
      Lengths differ: 6 vs 1
      
      
      testthat results ================================================================
      OK: 37 SKIPPED: 0 FAILED: 3
      1. Failure: bison returns the correct value (@test-bison.R#16) 
      2. Failure: bison_tax returns the correct ... (@test-bison_tax.R#12) 
      3. Failure: bison_tax returns the correct ... (@test-bison_tax.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rbokeh

Version: 0.5.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘shiny’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# rCGH

Version: 1.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        data      2.8Mb
        extdata   1.2Mb
    ```

# RcmdrPlugin.FuzzyClust

Version: 1.1

## In both

*   R CMD check timed out
    

# RcmdrPlugin.KMggplot2

Version: 0.2-4

## In both

*   checking whether package ‘RcmdrPlugin.KMggplot2’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/RcmdrPlugin.KMggplot2/new/RcmdrPlugin.KMggplot2.Rcheck/00install.out’ for details.
    ```

# RcmdrPlugin.MA

Version: 0.0-2

## In both

*   checking whether package ‘RcmdrPlugin.MA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/RcmdrPlugin.MA/new/RcmdrPlugin.MA.Rcheck/00install.out’ for details.
    ```

# rcongresso

Version: 0.2.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(rcongresso)
      > 
      > test_check("rcongresso")
      Error: Column `id` must be a 1d atomic vector or a list
      In addition: Warning message:
      Unknown or uninitialised column: 'id'. 
      testthat results ================================================================
      OK: 18 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 45-50 (introducao-rcongresso.Rmd) 
    Error: processing vignette 'introducao-rcongresso.Rmd' failed with diagnostics:
    Column `id` must be a 1d atomic vector or a list
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# RDAVIDWebService

Version: 1.14.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 23.9Mb
      sub-directories of 1Mb or more:
        java  21.7Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘callNextMethod’
    terms,DAVIDGODag: no visible global function definition for ‘nodeData’
    terms,DAVIDGODag: no visible global function definition for ‘goDag’
    uniqueIds,DAVIDGenes: no visible global function definition for
      ‘validObject’
    upsideDown,graph: no visible global function definition for ‘nodes’
    upsideDown,graph: no visible global function definition for ‘edges’
    upsideDown,graph : <anonymous>: no visible global function definition
      for ‘edges’
    upsideDown,graph : <anonymous> : <anonymous>: no visible global
      function definition for ‘addEdge’
    Undefined global functions or variables:
      GOGraph Term addEdge callNextMethod edges fill getFromNamespace goDag
      inEdges na.omit nodeData nodeData<- nodeDataDefaults<- nodes read.csv
      removeNode type.convert validObject x y
    Consider adding
      importFrom("methods", "callNextMethod", "validObject")
      importFrom("stats", "na.omit")
      importFrom("utils", "getFromNamespace", "read.csv", "type.convert")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# rdpla

Version: 0.2.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 5 SKIPPED: 0 FAILED: 24
      1. Error: dpla_collections basic functionality works (@test-dpla_collections.R#6) 
      2. Error: dpla_items - pagination works (@test-dpla_collections.R#20) 
      3. Error: dpla_items - fields requests work (@test-dpla_collections.R#35) 
      4. Failure: dpla_items fails well (@test-dpla_collections.R#46) 
      5. Failure: dpla_items fails well (@test-dpla_collections.R#49) 
      6. Error: dpla_collections_ basic functionality works (@test-dpla_collections_.R#6) 
      7. Error: dpla_items - pagination works (@test-dpla_collections_.R#22) 
      8. Error: dpla_items - fields requests work (@test-dpla_collections_.R#37) 
      9. Failure: dpla_items fails well (@test-dpla_collections_.R#48) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# re2r

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.1Mb
      sub-directories of 1Mb or more:
        doc    1.5Mb
        libs  10.4Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# recoup

Version: 1.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
            • test.exons: A 'GRangesList' containing the exons of the 100
              above genes for use with 'recoup' RNA-Seq mode.
    
    _F_o_r_m_a_t:
    
         'data.frame' and 'list' objects whose format is accepted by
         recoup.
    
    _A_u_t_h_o_r(_s):
    
         Panagiotis Moulos
    
    _S_o_u_r_c_e:
    
         Personal communication with the Talianids lab at BSRC 'Alexander
         Fleming'. Unpublished data.
    
    
    Error: processing vignette 'recoup_intro.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    recoup : <anonymous>: no visible global function definition for
      ‘runValue’
    recoupCorrelation: no visible binding for global variable ‘Index’
    recoupCorrelation: no visible binding for global variable ‘Coverage’
    recoupCorrelation: no visible binding for global variable ‘Condition’
    recoupCorrelation: no visible binding for global variable ‘Design’
    recoupHeatmap : <anonymous>: no visible global function definition for
      ‘grid.text’
    recoupProfile: no visible binding for global variable ‘Signal’
    recoupProfile: no visible binding for global variable ‘Condition’
    recoupProfile: no visible binding for global variable ‘Design’
    reduceExons : <anonymous>: no visible global function definition for
      ‘DataFrame’
    splitVector: no visible global function definition for ‘Rle’
    Undefined global functions or variables:
      Condition Coverage DataFrame Design IRanges Index Rle ScanBamParam
      Seqinfo Signal alphabetFrequency bamWhich<- biocLite dbConnect
      dbDisconnect dbDriver dbGetQuery dbWriteTable flankedSexon gene
      genomeRanges getBSgenome grid.text indexBam installed.genomes
      mclapply mcmapply runValue seqlevels seqlevels<- sexon sortBam
      subjectHits
    ```

# rEDM

Version: 0.6.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning in ccm(cbind(thrips_block$Thrips_imaginis, surr_Rain[, i]), E = 8,  :
      Note: CCM results are typically interpreted in the opposite direction of causation. Please see 'Detecting causality in complex ecosystems' (Sugihara et al. 2012) for more details.
    Warning in model$run() :
      Found overlap between lib and pred. Enabling cross-validation with exclusion radius = 0.
    Warning in model$run() :
      lib size request was larger than maximum available; corrected
    Error: processing vignette 'rEDM-algorithms.ltx' failed with diagnostics:
    Running 'texi2dvi' on 'rEDM-algorithms.ltx' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.28 \usepackage
                    [noend]{algpseudocode}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 17.5Mb
      sub-directories of 1Mb or more:
        doc    2.1Mb
        libs  15.1Mb
    ```

# regionReport

Version: 1.10.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
      no date field in DESCRIPTION file of package 'BiocStyle'
    Warning in citation("GenomeInfoDb") :
      no date field in DESCRIPTION file of package 'GenomeInfoDb'
    Warning in citation("biovizBase") :
      no date field in DESCRIPTION file of package 'biovizBase'
    Warning in citation("TxDb.Hsapiens.UCSC.hg19.knownGene") :
      no date field in DESCRIPTION file of package 'TxDb.Hsapiens.UCSC.hg19.knownGene'
    Warning in `[.bibentry`(citation("edgeR"), 5) : subscript out of bounds
    Warning in citation("DEFormats") :
      no date field in DESCRIPTION file of package 'DEFormats'
    Writing 34 Bibtex entries ... OK
    Results written to file 'regionReportRef.bib'
    Warning in `[[.BibEntry`(bib, c("edgeR1", "edgeR2", "edgeR5", "edgeR6")) :
      subscript out of bounds
    Warning in `[[.BibEntry`(bib, c("edgeR1", "edgeR2", "edgeR5", "edgeR6")) :
      subscript out of bounds
    Warning in `[[.BibEntry`(bib, "mgcv") : subscript out of bounds
    Warning in `[[.BibEntry`(bib, "whisker") : subscript out of bounds
    Error: processing vignette 'regionReport.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported object imported by a ':::' call: ‘DESeq2:::pvalueAdjustment’
      See the note in ?`:::` about the use of this operator.
    ```

# ReinforcementLearning

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# repijson

Version: 0.1.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘geojsonio’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# replyr

Version: 0.9.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘magick’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RSQLite’ ‘dbplyr’
      All declared Imports should be used.
    ```

# ReportingTools

Version: 2.16.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
      ERROR in /home/muelleki/git/R/ggplot2/revdep/checks/ReportingTools/new/ReportingTools.Rcheck/ReportingTools/unitTests/test_DESeqResults.R: Error while sourcing  /home/muelleki/git/R/ggplot2/revdep/checks/ReportingTools/new/ReportingTools.Rcheck/ReportingTools/unitTests/test_DESeqResults.R : Error in .requirePackage(package) : 
        unable to find required package 'DESeq'
      
      Test files with failing tests
      
         test_DESeqDataSet.R 
           /home/muelleki/git/R/ggplot2/revdep/checks/ReportingTools/new/ReportingTools.Rcheck/ReportingTools/unitTests/test_DESeqDataSet.R 
      
         test_DESeqResults.R 
           /home/muelleki/git/R/ggplot2/revdep/checks/ReportingTools/new/ReportingTools.Rcheck/ReportingTools/unitTests/test_DESeqResults.R 
      
      
      Error in BiocGenerics:::testPackage("ReportingTools") : 
        unit tests failed for package ReportingTools
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      'fData'
    .marrayLM.to.html: no visible global function definition for
      'featureNames'
    .marrayLM.to.html: no visible global function definition for 'fData'
    check.ids: no visible binding for global variable 'org.Hs.eg.db'
    check.ids: no visible global function definition for 'keys'
    custHeaderPanel : <anonymous>: no visible binding for global variable
      'tags'
    custHeaderPanel : <anonymous>: no visible global function definition
      for 'HTML'
    custHeaderPanel: no visible global function definition for 'tagList'
    custHeaderPanel: no visible global function definition for 'tag'
    custHeaderPanel: no visible global function definition for 'div'
    custHeaderPanel: no visible global function definition for 'h1'
    publish,trellis-HTMLReport: no visible binding for global variable
      'htmlRep'
    toReportDF,DESeqDataSet: no visible global function definition for
      'mcols'
    Undefined global functions or variables:
      HTML columns description div exprs fData featureNames h1 htmlRep keys
      keytype mcols org.Hs.eg.db tag tagList tags
    ```

# rerddap

Version: 0.4.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 91 SKIPPED: 0 FAILED: 15
      1. Failure: ed_search_adv fails well (@test-ed_search_adv.R#58) 
      2. Failure: ed_search_adv fails well (@test-ed_search_adv.R#59) 
      3. Error: griddap returns the correct class (@test-griddap.r#6) 
      4. Error: griddap fixes incorrect user inputs (@test-griddap.r#24) 
      5. Error: griddap fields parameter works, and fails correctly (@test-griddap.r#65) 
      6. Failure: griddap fails well, in addition to above failure tests (@test-griddap.r#85) 
      7. Failure: griddap fails well, in addition to above failure tests (@test-griddap.r#86) 
      8. Failure: griddap fails well, in addition to above failure tests (@test-griddap.r#89) 
      9. Error: info returns the correct (@test-info.R#6) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘taxize’
    ```

# rfigshare

Version: 0.3.7

## In both

*   checking R code for possible problems ... NOTE
    ```
    fs_author_ids : <anonymous>: no visible global function definition for
      ‘select.list’
    fs_download : <anonymous>: no visible global function definition for
      ‘download.file’
    Undefined global functions or variables:
      download.file select.list
    Consider adding
      importFrom("utils", "download.file", "select.list")
    to your NAMESPACE file.
    ```

# RforProteomics

Version: 1.14.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    NetworkAnalysis, GraphsAndNetworks
    Warning: Dropping unknown biocViews terms:
    Cancer, StemCells, HIV
    Warning: Dropping unknown biocViews terms:
    NoViewProvided
    Warning: Dropping unknown biocViews terms:
    Statistics
    Warning: Dropping unknown biocViews terms:
    Statistical Method
    Warning: Dropping unknown biocViews terms:
    Regulation, HighThroughputSequencing, MultipleComparisons, Bioinformatics
    Warning: Dropping unknown biocViews terms:
    HighThroughputSequencing
    trying URL 'ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/PXD000001_mztab.txt'
    Content type 'unknown' length 864039 bytes (843 KB)
    ==================================================
    Warning: Version 0.9 is deprecated. Please see '?readMzTabData' and '?MzTab' for details.
    Quitting from lines 414-419 (RProtVis.Rmd) 
    Error: processing vignette 'RProtVis.Rmd' failed with diagnostics:
    there is no package called 'beanplot'
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        doc   7.1Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    RProtVis: no visible global function definition for ‘vignette’
    RProtViz: no visible global function definition for ‘vignette’
    RforProteomics: no visible global function definition for ‘vignette’
    downloadData: no visible global function definition for ‘download.file’
    getPackagesInBiocView: no visible global function definition for ‘data’
    shinyMA: no visible global function definition for ‘data’
    shinyMA : <anonymous>: no visible global function definition for ‘par’
    shinyMA : <anonymous>: no visible global function definition for ‘grid’
    shinyMA : <anonymous>: no visible global function definition for
      ‘abline’
    shinyMA : <anonymous>: no visible global function definition for
      ‘points’
    Undefined global functions or variables:
      abline data download.file grid par points vignette
    Consider adding
      importFrom("graphics", "abline", "grid", "par", "points")
      importFrom("utils", "data", "download.file", "vignette")
    to your NAMESPACE file.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    '::' or ':::' import not declared from: ‘AnnotationDbi’
    ```

# rfPermute

Version: 2.1.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# rgbif

Version: 0.9.9

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      8: dplyr::collect
      9: getExportedValue(pkg, name)
      10: asNamespace(ns)
      11: getNamespace(ns)
      12: tryCatch(loadNamespace(name), error = function(e) stop(e))
      13: tryCatchList(expr, classes, parentenv, handlers)
      14: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      15: value[[3L]](cond)
      
      testthat results ================================================================
      OK: 720 SKIPPED: 0 FAILED: 1
      1. Error: gbifmap (@test-gbifmap.r#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RGCCA

Version: 2.1.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    ! LaTeX Error: File `algorithm2e.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    Enter file name: 
    ! Emergency stop.
    <read *> 
             
    l.128 \usepackage
    
    pandoc: Error producing PDF
    Error: processing vignette 'vignette_RGCCA.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 43
    Execution halted
    ```

# RGraphics

Version: 2.0-14

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘gWidgetsRGtk2’ ‘iplots’ ‘playwith’ ‘pmg’ ‘RGraphics’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  9.7Mb
      sub-directories of 1Mb or more:
        extra   9.4Mb
    ```

# RiboProfiling

Version: 1.6.0

## In both

*   checking whether package ‘RiboProfiling’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘BiocGenerics::Position’ by ‘ggplot2::Position’ when loading ‘RiboProfiling’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/RiboProfiling/new/RiboProfiling.Rcheck/00install.out’ for details.
    ```

*   checking R code for possible problems ... NOTE
    ```
    applyShiftFeature: no visible global function definition for 'is'
    aroundPromoter: no visible global function definition for 'is'
    codonInfo: no visible global function definition for 'is'
    countShiftReads: no visible global function definition for 'is'
    histMatchLength: no visible global function definition for 'is'
    orfRelativePos: no visible global function definition for 'is'
    plotSummarizedCov: no visible global function definition for 'is'
    readStartCov: no visible global function definition for 'is'
    readsToStartOrEnd: no visible global function definition for 'is'
    riboSeqFromBAM: no visible global function definition for 'is'
    Undefined global functions or variables:
      is
    Consider adding
      importFrom("methods", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# riskRegression

Version: 1.4.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        libs   5.6Mb
    ```

# RITAN

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    summary.term_enrichment_by_subset: no visible global function
      definition for ‘head’
    term.test: no visible global function definition for ‘phyper’
    term_enrichment : process_source: no visible binding for global
      variable ‘active_genesets’
    term_enrichment : process_source: no visible global function definition
      for ‘head’
    write_simple_table: no visible global function definition for
      ‘write.table’
    Undefined global functions or variables:
      Var1 Var2 abline active_genesets all_net all_symbols box density e f
      geneset_list head hist mad median network_list p.adjust phyper plot
      polygon read.table rect setTxtProgressBar subSIF txtProgressBar
      write.table
    Consider adding
      importFrom("graphics", "abline", "box", "hist", "plot", "polygon",
                 "rect")
      importFrom("stats", "density", "mad", "median", "p.adjust", "phyper")
      importFrom("utils", "head", "read.table", "setTxtProgressBar",
                 "txtProgressBar", "write.table")
    to your NAMESPACE file.
    ```

# RIVER

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    SubjectID	GeneName	Feature1	Feature2	Feature3	Feature4	Feature5	Feature6	Feature7	Feature8	Feature9	Feature10	Feature11	Feature12	Feature13	Feature14	Feature15	Feature16	Feature17	Feature18	Zscore	N2pair
    indiv58	gene1614	0.489033798630696	-1.21431829385884	-0.501524068860469	-0.809388087647524	-0.191695708370831	-0.0661469835645102	0.199315319036506	-0.754425337309085	-1.35050355454646	-0.0347771492735548	2.08675351921196	0.728318277270296	0.150747102542626	1.51835790522656	0.222613351767026	1.88846321159354	-0.440823555356033	-0.45581108818362	2.83051056818124	NA
    Error: processing vignette 'RIVER.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘data.table’
    ```

# rmcfs

Version: 1.2.6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        java   4.5Mb
    ```

# rms

Version: 5.1-1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘multiwayvcov’, ‘treatSens’
    ```

# RnBeads

Version: 1.8.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/runTests.R’ failed.
    Last 13 lines of output:
      
          ozone
      
      The following object is masked from 'package:IRanges':
      
          desc
      
      The following object is masked from 'package:S4Vectors':
      
          rename
      
      Error in library("RUnit", quietly = TRUE) : 
        there is no package called 'RUnit'
      Calls: <Anonymous> -> library
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘BiocGenerics’ ‘S4Vectors’ ‘GenomicRanges’ ‘MASS’ ‘cluster’ ‘ff’
      ‘fields’ ‘ggplot2’ ‘gplots’ ‘gridExtra’ ‘limma’ ‘matrixStats’
      ‘illuminaio’ ‘methylumi’ ‘plyr’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        R     1.1Mb
        bin   1.0Mb
        doc   3.1Mb
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘RnBeads’ for: ‘samples’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      'Gviz:::.getBMFeatureMap' 'doParallel:::.options'
      'grDevices:::.smoothScatterCalcDensity'
      'minfi:::.default.450k.annotation' 'minfi:::.extractFromRGSet450k'
      'minfi:::.normalizeFunnorm450k'
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      BootRefFreeEwasModel ChrNumeric DataTrack Density Deviance Difference
      DoISVA Error EstDimRMT GenomeAxisTrack ID IdeogramTrack
      IlluminaHumanMethylation450kmanifest
      IlluminaHumanMethylationEPICmanifest Index Intensity Measure
      PairsBootRefFreeEwasModel Probe RGChannelSet RefFreeEwasModel SNP
      Sample Slide Target Term UcscTrack Value addSex as.profileCGH
      assayDataElement assayDataElementNames barcode bv chrom color
      combinedRank comma covgMedian covgPercLow covgPercUp cv.glmnet daglad
      diffmeth diffmeth.p.adj.fdr diffmeth.p.val dinucleotideFrequency
      expectedCounts featureData featureData<- featureNames featureNames<-
      foreach geneCounts genome<- getCN getDoParWorkers getGreen
      getManifest getMeth getRed getSex getUnmeth getVarCov glmnet
      grid.draw grid.newpage group group1 group2 i impute.knn intensities
      is.subsegmentation k letterFrequency lme mapToGenome mean.diff
      mean.g1 mean.g2 mean.mean.g1 mean.mean.g2 mean.quot.log2 melt muted
      n.sites num.sites numSites numeric.names oddsRatios pData
      percent_format phenoData phenoData<- plotOrder plotTracks
      preprocessSWAN pvalues refText reg.type region.size
      registerDoParallel relative.coord report samples seqlengths
      seqlevels<- sigCategories sites2ignore size solve.QP stopCluster sva
      target tsne type types universeCounts useMart v varLabels x y yint
    ```

# Rnits

Version: 1.10.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in block_exec(params) :
      failed to tidy R code in chunk <loaddata>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning in download.file(sprintf("https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/%s",  :
      URL https://ftp.ncbi.nlm.nih.gov/geo/series/GSE4nnn/GSE4158/matrix//geo/series/GSE4nnn/GSE4158/: cannot open destfile '/home/muelleki/tmp/RtmpMRn15c//geo/series/GSE4nnn/GSE4158/', reason 'No such file or directory'
    Warning in download.file(sprintf("https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/%s",  :
      download had nonzero exit status
    Warning in file(con, "r") :
      cannot open file '/home/muelleki/tmp/RtmpMRn15c//geo/series/GSE4nnn/GSE4158/': No such file or directory
    Quitting from lines 90-114 (Rnits-vignette.Rnw) 
    Error: processing vignette 'Rnits-vignette.Rnw' failed with diagnostics:
    cannot open the connection
    Execution halted
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘scale_color_brewer’
    plotResults,Rnits: no visible global function definition for
      ‘facet_wrap’
    plotResults,Rnits: no visible global function definition for ‘dev.off’
    summary,Rnits: no visible global function definition for ‘hist’
    timeAlign,Rnits: no visible global function definition for ‘quantile’
    timeAlign,Rnits: no visible global function definition for ‘mvfft’
    timeAlign,Rnits: no visible global function definition for ‘abline’
    Undefined global functions or variables:
      Sample Time abline aes dev.off facet_wrap gaussian geom_point
      geom_smooth glm hat hist kmeans mad median mvfft p.adjust par predict
      quantile rnorm scale_color_brewer sd setNames setTxtProgressBar
      smooth.spline theme theme_bw txtProgressBar value ylab
    Consider adding
      importFrom("grDevices", "dev.off")
      importFrom("graphics", "abline", "hist", "par")
      importFrom("stats", "gaussian", "glm", "hat", "kmeans", "mad",
                 "median", "mvfft", "p.adjust", "predict", "quantile",
                 "rnorm", "sd", "setNames", "smooth.spline")
      importFrom("utils", "setTxtProgressBar", "txtProgressBar")
    to your NAMESPACE file.
    ```

# rnoaa

Version: 0.7.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 163 SKIPPED: 1 FAILED: 17
      1. Failure: buoys fails well (@test-buoy.R#74) 
      2. Error: check_response returns an error (@test-check_response.r#7) 
      3. Error: check_response returns the correct error messages (@test-check_response.r#26) 
      4. Error: gefs time and ensemble selection returns correct indices. (@test-gefs.R#25) 
      5. Error: gefs_variables returns characters. (@test-gefs.R#52) 
      6. Error: gefs_latitudes returns numeric. (@test-gefs.R#63) 
      7. Error: gefs_longitudes returns numeric. (@test-gefs.R#73) 
      8. Error: gefs_dimensions returns character list. (@test-gefs.R#83) 
      9. Error: gefs_dimension_values returns numeric array. (@test-gefs.R#93) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RNOmni

Version: 0.1.4

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘cowplot’ ‘ggplot2’ ‘microbenchmark’ ‘reshape2’
      All declared Imports should be used.
    ```

# rnpn

Version: 0.1.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      bb$latitude inherits from `numeric` not `character`.
      
      
      testthat results ================================================================
      OK: 20 SKIPPED: 0 FAILED: 7
      1. Failure: npn_indsatstations works well (@test-npn_indsatstations.R#9) 
      2. Failure: npn_indspatstations works well (@test-npn_indspatstations.R#9) 
      3. Error: npn_obsspbyday works well (@test-npn_obsspbyday.R#6) 
      4. Error: when no match, returns empty data.frame (@test-npn_obsspbyday.R#20) 
      5. Failure: npn_stationsbystate works well (@test-npn_stationsbystate.R#11) 
      6. Failure: npn_stationswithspp works well (@test-npn_stationswithspp.R#10) 
      7. Failure: npn_stationswithspp works well (@test-npn_stationswithspp.R#15) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# robCompositions

Version: 2.0.6

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘mvoutlier’, ‘StatDA’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5701 marked UTF-8 strings
    ```

# robustbase

Version: 0.92-8

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘matrixStats’, ‘robustX’, ‘quantreg’, ‘Hmisc’
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error in re-building vignettes:
      ...
    Loading required package: robustbase
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'fastMcd-kmini.tex' failed.
    BibTeX errors:
    The top-level auxiliary file: fastMcd-kmini.aux
    I couldn't open style file chicago.bst
    ---line 43 of file fastMcd-kmini.aux
     : \bibstyle{chicago
     :                  }
    I'm skipping whatever remains of this command
    I found no style file---while reading file fastMcd-kmini.aux
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# robustHD

Version: 0.5.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        libs   5.9Mb
    ```

# rODE

Version: 0.99.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘data.table’
      All declared Imports should be used.
    ```

# Roleswitch

Version: 1.14.0

## In both

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘pracma’ ‘reshape’ ‘plotrix’ ‘microRNA’ ‘biomaRt’ ‘Biostrings’
      ‘Biobase’ ‘DBI’
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

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
    3: max(p.x-p.x.prev)=0.00010
    4: max(p.x-p.x.prev)=0.00000
    
    Start roleswitch with 365 miRNA and 11016 mRNA
    1: max(p.x-p.x.prev)=0.12564
    2: max(p.x-p.x.prev)=0.00008
    3: max(p.x-p.x.prev)=0.00003
    4: max(p.x-p.x.prev)=0.00000
    Some genes or miRNA are left out in calculation
    b/c they have zero target sites or targets!
    Their probabilities are set to zero in the output matrices
    Loading required package: ggplot2
    Loading required package: hgu95av2.db
    Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
      there is no package called ‘hgu95av2.db’
    
    Error: processing vignette 'Roleswitch.Rnw' failed with diagnostics:
     chunk 10 (label = eset) 
    Error in roleswitch(eset, mirna.expr) : 
      hgu95av2.db package must be installed
    Execution halted
    ```

# rolypoly

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘matrixcalc’
      All declared Imports should be used.
    ```

# rotations

Version: 1.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 10.3Mb
      sub-directories of 1Mb or more:
        data   2.3Mb
        libs   6.5Mb
    ```

# rpdo

Version: 0.2.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      15: check_values(data, values = values, unique = TRUE, nulls = FALSE, data_name = data_name) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-data3.R:33
      16: check_data_values(data, values, data_name) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-values.R:20
      17: vapply(column_names, FUN = check_data_values_column, logical(1), data = data, values = values, 
             data_name = data_name) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-data-values.R:20
      18: FUN(X[[i]], ...)
      19: check_vector_value_missing(vector, value, column_name, data_name) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-data-values.R:9
      20: error(name_info(column_name, data_name), " cannot include missing values") at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/check-vector-value.R:7
      21: stop(..., call. = FALSE) at /tmp/RtmpHtcmq3/R.INSTALL6285486ea706/datacheckr/R/utils.R:20
      
      testthat results ================================================================
      OK: 1 SKIPPED: 0 FAILED: 1
      1. Error: download_pdo (@test-download-pdo.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rpf

Version: 0.55

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.3Mb
      sub-directories of 1Mb or more:
        libs   8.5Mb
    ```

# RPPanalyzer

Version: 1.4.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        extdata   4.5Mb
    ```

# rPref

Version: 1.2

## In both

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# Rqc

Version: 1.10.2

## In both

*   checking R code for possible problems ... NOTE
    ```
    .ui.Rqc: no visible global function definition for 'packageVersion'
    rqc: no visible global function definition for 'browseURL'
    rqcCycleAverageQualityPcaCalc: no visible global function definition
      for 'prcomp'
    rqcFileHeatmap: no visible global function definition for 'hclust'
    stats4trim : <anonymous>: no visible global function definition for
      'head'
    Undefined global functions or variables:
      browseURL hclust head packageVersion prcomp
    Consider adding
      importFrom("stats", "hclust", "prcomp")
      importFrom("utils", "browseURL", "head", "packageVersion")
    to your NAMESPACE file.
    ```

# rrpack

Version: 0.1-6

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        libs   5.1Mb
    ```

# rrr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rcpp’
      All declared Imports should be used.
    ```

# RSA

Version: 0.9.11

## In both

*   checking whether package ‘RSA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
      Warning: loading Rplot failed
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/RSA/new/RSA.Rcheck/00install.out’ for details.
    ```

# rsMove

Version: 0.2.1

## Newly broken

*   checking whether package ‘rsMove’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::calc’ by ‘raster::calc’ when loading ‘rsMove’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/rsMove/new/rsMove.Rcheck/00install.out’ for details.
    ```

# rsoi

Version: 0.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘utils’
      All declared Imports should be used.
    ```

# rSPACE

Version: 1.2.0

## In both

*   checking whether package ‘rSPACE’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/rSPACE/new/rSPACE.Rcheck/00install.out’ for details.
    ```

# RSSL

Version: 0.6.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        libs   2.7Mb
    ```

# rstan

Version: 2.16.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 228.9Mb
      sub-directories of 1Mb or more:
        libs  226.6Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘coda’, ‘rstanarm’
    ```

# rstanarm

Version: 2.15.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        no valid constructor available for the argument list
      trying deprecated constructor; please alert package maintainer
      Error in new_CppObject_xp(fields$.module, fields$.pointer, ...) : 
        no valid constructor available for the argument list
      trying deprecated constructor; please alert package maintainer
      Error in new_CppObject_xp(fields$.module, fields$.pointer, ...) : 
        no valid constructor available for the argument list
      trying deprecated constructor; please alert package maintainer
      testthat results ================================================================
      OK: 1738 SKIPPED: 0 FAILED: 2
      1. Error: pp_check.stanreg creates ggplot object (@test_pp_check.R#48) 
      2. Error: the Stan equivalent of lme4's Z %*% b works (@test_stan_functions.R#408) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 58.3Mb
      sub-directories of 1Mb or more:
        doc    2.2Mb
        libs  55.0Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘gamm4’, ‘biglm’
    ```

# RStoolbox

Version: 0.1.10

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    49 LC82240632013167LGN00 LC82240632013167LGN00
                                                                   Download.Link
    6  http://earthexplorer.usgs.gov/download/options/4923/LC82240632015157LGN00
    27 http://earthexplorer.usgs.gov/download/options/4923/LC82240632014186LGN00
    40 http://earthexplorer.usgs.gov/download/options/4923/LC82240632013327LGN00
    46 http://earthexplorer.usgs.gov/download/options/4923/LC82240632013215LGN00
    49 http://earthexplorer.usgs.gov/download/options/4923/LC82240632013167LGN00
       Browse.Link       Date Doy Year Satellite Num
    6           NA 2015-06-06 157 2015       LS8   8
    27          NA 2014-07-05 186 2014       LS8   8
    40          NA 2013-11-23 327 2013       LS8   8
    46          NA 2013-08-03 215 2013       LS8   8
    49          NA 2013-06-16 167 2013       LS8   8
    > 
    > ## Available time-series
    > ggplot(ee) + 
    + 		geom_segment(aes(x = Date, xend = Date, y = 0, yend = 100 - Cloud.Cover, 
    +      col = as.factor(Year))) +
    + 		scale_y_continuous(name = "Scene quality (% clear sky)")
    Error: Columns `x`, `xend` are dates/times and must be stored as POSIXct, not POSIXlt
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        libs   4.3Mb
    ```

# rsunlight

Version: 0.4.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 8 SKIPPED: 0 FAILED: 30
      1. Error: returns the correct (@test-amendments.R#6) 
      2. Error: vectorizing works (@test-amendments.R#29) 
      3. Error: paging works (@test-amendments.R#37) 
      4. Error: fails well (@test-amendments.R#47) 
      5. Error: cg_bills returns the correct (@test-cg_bills.R#6) 
      6. Error: cg_bills vectorizing works (@test-cg_bills.R#25) 
      7. Error: cg_bills pagination works (@test-cg_bills.R#35) 
      8. Failure: cg_bills curl options work (@test-cg_bills.R#44) 
      9. Failure: cg_bills fails well (@test-cg_bills.R#50) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RSwissMaps

Version: 0.1.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 18627 marked UTF-8 strings
    ```

# RTCGA

Version: 1.6.0

## In both

*   checking examples ... ERROR
    ```
    ...
    + 	MET = `MET|4233`) %>%  
    + 	#cancer samples
    + 	filter(substr(bcr_patient_barcode, 14, 15) == "01") -> ACC_BLCA_BRCA_OV.rnaseq
    > 	
    > 
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "cohort", "MET")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "cohort", "log1p(MET)")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), median)", "log1p(MET)")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), max)", "log1p(MET)")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), median)", "log1p(MET)",
    + xlab = "Cohort Type", ylab = "Logarithm of MET")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), median)", "log1p(MET)", 
    + xlab = "Cohort Type", ylab = "Logarithm of MET", legend.title = "Cohorts")
    > boxplotTCGA(ACC_BLCA_BRCA_OV.rnaseq, "reorder(cohort,log1p(MET), median)", "log1p(MET)", 
    + xlab = "Cohort Type", ylab = "Logarithm of MET", legend.title = "Cohorts", legend = "bottom")
    > 
    > ## facet example
    > library(RTCGA.mutations)
    Error in library(RTCGA.mutations) : 
      there is no package called ‘RTCGA.mutations’
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘RTCGA.clinical’ ‘RTCGA.mutations’ ‘RTCGA.RPPA’ ‘RTCGA.mRNA’
      ‘RTCGA.miRNASeq’ ‘RTCGA.methylation’ ‘RTCGA.CNV’
    ```

*   checking R code for possible problems ... NOTE
    ```
    availableDates: no visible binding for global variable ‘.’
    downloadTCGA: no visible binding for global variable ‘.’
    ggbiplot: no visible binding for global variable ‘xvar’
    ggbiplot: no visible binding for global variable ‘yvar’
    ggbiplot: no visible global function definition for ‘muted’
    ggbiplot: no visible binding for global variable ‘varname’
    ggbiplot: no visible binding for global variable ‘angle’
    ggbiplot: no visible binding for global variable ‘hjust’
    read.mutations: no visible binding for global variable ‘.’
    read.rnaseq: no visible binding for global variable ‘.’
    survivalTCGA: no visible binding for global variable ‘times’
    whichDateToUse: no visible binding for global variable ‘.’
    Undefined global functions or variables:
      . angle hjust muted times varname xvar yvar
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘RTCGA.clinical’, ‘RTCGA.mutations’, ‘RTCGA.CNV’, ‘RTCGA.RPPA’, ‘RTCGA.mRNA’, ‘RTCGA.miRNASeq’, ‘RTCGA.methylation’
    ```

# rtf

Version: 0.4-11

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    .forest.plot.scale: no visible global function definition for ‘axTicks’
    .forest.plot.scale : <anonymous>: no visible global function definition
      for ‘lines’
    .forest.plot.scale: no visible global function definition for ‘lines’
    .forest.plot.scale: no visible global function definition for ‘text’
    .rtf.plot: no visible global function definition for ‘png’
    .rtf.plot: no visible global function definition for ‘dev.off’
    .rtf.trellis.object: no visible global function definition for ‘png’
    .rtf.trellis.object: no visible global function definition for
      ‘dev.off’
    addSessionInfo.RTF: no visible global function definition for
      ‘sessionInfo’
    Undefined global functions or variables:
      abline arrows axTicks dev.off lines par plot png points sessionInfo
      text
    Consider adding
      importFrom("grDevices", "dev.off", "png")
      importFrom("graphics", "abline", "arrows", "axTicks", "lines", "par",
                 "plot", "points", "text")
      importFrom("utils", "sessionInfo")
    to your NAMESPACE file.
    ```

# rtimes

Version: 0.5.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      6: Filter(Negate(is.null), x)
      7: unlist(lapply(x, f))
      8: lapply(x, f)
      9: check_key(key)
      10: stop("need an API key for ", y, call. = FALSE)
      
      testthat results ================================================================
      OK: 2 SKIPPED: 0 FAILED: 4
      1. Error: returns the correct stuff (@test-as_search.R#8) 
      2. Error: returns the correct stuff (@test-geo_search.R#8) 
      3. Failure: fails well (@test-geo_search.R#48) 
      4. Error: fails well (@test-geo_search.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rtimicropem

Version: 1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

# rtweet

Version: 0.6.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 113868 marked UTF-8 strings
    ```

# rvertnet

Version: 0.6.2

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      grepl("Bufo debilis", aa$data$scientificname) isn't true.
      
      
      3. Failure: vert_id works (@test-vert_id.R#33) ---------------------------------
      any(grepl("Bufo", aa$data$scientificname)) isn't true.
      
      
      testthat results ================================================================
      OK: 107 SKIPPED: 0 FAILED: 3
      1. Failure: searchbyterm works correctly (@test-searchbyterm.R#12) 
      2. Failure: vert_id works (@test-vert_id.R#15) 
      3. Failure: vert_id works (@test-vert_id.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rvinecopulib

Version: 0.2.3.1.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 49.2Mb
      sub-directories of 1Mb or more:
        libs  48.4Mb
    ```

# rWBclimate

Version: 0.1.3

## In both

*   checking R code for possible problems ... NOTE
    ```
    check_ISO_code: no visible binding for global variable ‘NoAm_country’
    check_ISO_code: no visible binding for global variable ‘SoAm_country’
    check_ISO_code: no visible binding for global variable ‘Oceana_country’
    check_ISO_code: no visible binding for global variable ‘Africa_country’
    check_ISO_code: no visible binding for global variable ‘Asia_country’
    check_ISO_code: no visible binding for global variable ‘Eur_country’
    climate_map: no visible binding for global variable ‘data’
    date_correct: no visible global function definition for ‘tail’
    Undefined global functions or variables:
      Africa_country Asia_country Eur_country NoAm_country Oceana_country
      SoAm_country data tail
    Consider adding
      importFrom("utils", "data", "tail")
    to your NAMESPACE file.
    ```

# Rz

Version: 0.9-1

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘vcd’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Packages in Depends field not imported from:
      ‘foreign’ ‘ggplot2’ ‘grid’ ‘psych’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    Unexported object imported by a ':::' call: ‘foreign:::adQuote’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      for ‘rgb’
    gtkColorSelectionWidgetNew: no visible global function definition for
      ‘colors’
    gtkFileChooserDialogFilteredActivate: no visible binding for global
      variable ‘theme_grey’
    localize: no visible global function definition for ‘localeToCharset’
    summary.CrossTable : twoDimTable: no visible global function definition
      for ‘addmargins’
    summary.CrossTable: no visible global function definition for
      ‘assocstats’
    write.spss: no visible global function definition for ‘write.table’
    write.stata: no visible global function definition for ‘write.table’
    Undefined global functions or variables:
      addmargins as.formula assocstats colors localeToCharset nclass.FD
      nclass.Sturges nclass.scott rgb theme_grey write.table
    Consider adding
      importFrom("grDevices", "colors", "nclass.FD", "nclass.Sturges",
                 "nclass.scott", "rgb")
      importFrom("stats", "addmargins", "as.formula")
      importFrom("utils", "localeToCharset", "write.table")
    to your NAMESPACE file.
    ```

# samExploreR

Version: 1.0.0

## In both

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      .BBSoptions
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    plotsamExplorer: no visible binding for global variable ‘f’
    plotsamExplorer: no visible binding for global variable ‘value’
    plotsamExplorer: no visible binding for global variable ‘group’
    Undefined global functions or variables:
      f group value
    ```

# sand

Version: 1.0.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6 marked UTF-8 strings
    ```

# savR

Version: 1.14.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    buildReports,savProject-character: no visible global function
      definition for ‘dev.off’
    qualityHeatmap,savProject-integer-integer-logical: no visible global
      function definition for ‘quantile’
    Undefined global functions or variables:
      dev.off quantile
    Consider adding
      importFrom("grDevices", "dev.off")
      importFrom("stats", "quantile")
    to your NAMESPACE file.
    ```

# SC3

Version: 1.4.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'my-vignette.Rmd' failed with diagnostics:
    there is no package called ‘BiocStyle’
    Execution halted
    ```

# scanstatistics

Version: 1.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        libs   6.3Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gamlss.dist’
      All declared Imports should be used.
    ```

# scater

Version: 1.4.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.7Mb
      sub-directories of 1Mb or more:
        doc   5.7Mb
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'arrange':
      ‘arrange.SCESet’
    
    S3 methods shown with full name in documentation object 'filter':
      ‘filter.SCESet’
    
    S3 methods shown with full name in documentation object 'mutate':
      ‘mutate.SCESet’
    
    S3 methods shown with full name in documentation object 'rename':
      ‘rename.SCESet’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# scDD

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘grDevices’ ‘graphics’ ‘stats’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    testZeroes: no visible global function definition for ‘anova’
    validation : <anonymous>: no visible global function definition for
      ‘var’
    validation: no visible binding for global variable ‘var’
    validation: no visible global function definition for ‘par’
    validation: no visible global function definition for ‘plot’
    validation: no visible global function definition for ‘abline’
    validation: no visible global function definition for ‘points’
    Undefined global functions or variables:
      abline anova axis binomial density dev.off fisher.test hcl hist
      ks.test lines lm model.matrix p.adjust par pdf plot points quantile
      rbinom rect residuals rnbinom rt runif t.test var
    Consider adding
      importFrom("grDevices", "dev.off", "hcl", "pdf")
      importFrom("graphics", "abline", "axis", "hist", "lines", "par",
                 "plot", "points", "rect")
      importFrom("stats", "anova", "binomial", "density", "fisher.test",
                 "ks.test", "lm", "model.matrix", "p.adjust", "quantile",
                 "rbinom", "residuals", "rnbinom", "rt", "runif", "t.test",
                 "var")
    to your NAMESPACE file.
    ```

# SciencesPo

Version: 1.4.1

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Title: Automatically enclose points in a polygon
    > ### Aliases: GeomSpotlight geom_spotlight
    > ### Keywords: datasets ggplot2
    > 
    > ### ** Examples
    > 
    > d <- data.frame(x=c(1,1,2),y=c(1,2,2)*100)
    > 
    > gg <- ggplot(d,aes(x,y))
    > gg <- gg + scale_x_continuous(expand=c(0.5,1))
    > gg <- gg + scale_y_continuous(expand=c(0.5,1))
    > gg + geom_spotlight(s_shape=1, expand=0) + geom_point()
    > 
    > 
    > gg <- ggplot(mpg, aes(displ, hwy))
    > ss <- subset(mpg,hwy>29 & displ<3)
    > gg + geom_spotlight(data=ss, colour="blue", s_shape=.8, expand=0) +
    + geom_point() + geom_point(data=ss, colour="blue")
    Error in loadNamespace(name) : there is no package called ‘dplyr’
    Calls: geom_spotlight ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning: `legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing
    Warning: New theme missing the following elements: axis.title.x.top, axis.title.y.right, axis.text.x.top, axis.text.y.right, axis.line.x, axis.line.y, legend.spacing.x, legend.spacing.y, legend.box.margin, legend.box.background, legend.box.spacing, panel.spacing.x, panel.spacing.y, panel.grid.major, panel.grid.minor, plot.subtitle, plot.caption, strip.placement
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Warning: `legend.margin` must be specified using `margin()`. For the old behavior use legend.spacing
    Warning: `panel.margin` is deprecated. Please use `panel.spacing` property instead
    Quitting from lines 1090-1091 (SciencesPo.Rmd) 
    Error: processing vignette 'SciencesPo.Rmd' failed with diagnostics:
    invalid 'times' argument
    Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gmodels’
    ```

# scone

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    Attaching package: 'matrixStats'
    
    The following objects are masked from 'package:Biobase':
    
        anyMissing, rowMedians
    
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following object is masked from 'package:base':
    
        apply
    
    Error: processing vignette 'sconeTutorial.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘EDASeq’
      All declared Imports should be used.
    ```

# SCORPIUS

Version: 1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Rcpp’ ‘testthat’
      All declared Imports should be used.
    ```

# scsR

Version: 1.12.0

## In both

*   checking whether package ‘scsR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/scsR/new/scsR.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Depends: includes the non-default packages:
      ‘STRINGdb’ ‘BiocGenerics’ ‘Biostrings’ ‘IRanges’ ‘plyr’ ‘tcltk’
    Adding so many packages to the search path is excessive and importing
    selectively is preferable.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    seed_correction_pooled: no visible global function definition for
      ‘txtProgressBar’
    seed_correction_pooled: no visible global function definition for
      ‘setTxtProgressBar’
    seed_removal: no visible global function definition for
      ‘txtProgressBar’
    seed_removal: no visible global function definition for
      ‘setTxtProgressBar’
    seeds_analysis : <anonymous>: no visible global function definition for
      ‘ks.test’
    seeds_analysis: no visible global function definition for ‘phyper’
    transcribe_seqs: no visible global function definition for
      ‘txtProgressBar’
    transcribe_seqs: no visible global function definition for
      ‘setTxtProgressBar’
    Undefined global functions or variables:
      cor.test heatmap.2 ks.test lm phyper setTxtProgressBar txtProgressBar
    Consider adding
      importFrom("stats", "cor.test", "ks.test", "lm", "phyper")
      importFrom("utils", "setTxtProgressBar", "txtProgressBar")
    to your NAMESPACE file.
    ```

# SDaA

Version: 0.1-3

## In both

*   checking R code for possible problems ... NOTE
    ```
    lahiri.design: no visible global function definition for ‘runif’
    Undefined global functions or variables:
      runif
    Consider adding
      importFrom("stats", "runif")
    to your NAMESPACE file.
    ```

# sdcMicro

Version: 5.0.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        libs   2.1Mb
    ```

# SDEFSR

Version: 0.7.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘grDevices’
      All declared Imports should be used.
    ```

# season

Version: 0.3-5

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    sinusoid: no visible global function definition for ‘par’
    sinusoid: no visible global function definition for ‘plot’
    sinusoid: no visible global function definition for ‘lines’
    sinusoid: no visible global function definition for ‘axis’
    summary.monthglm: no visible global function definition for ‘qnorm’
    wtest: no visible global function definition for ‘qchisq’
    wtest: no visible global function definition for ‘pchisq’
    Undefined global functions or variables:
      acf as.formula axis box cpgram fitted gaussian glm gray hist lines
      median model.frame na.omit par pchisq plot points polygon qchisq
      qnorm quantile relevel resid residuals rgamma rnorm rug runif sd text
      time
    Consider adding
      importFrom("grDevices", "gray")
      importFrom("graphics", "axis", "box", "hist", "lines", "par", "plot",
                 "points", "polygon", "rug", "text")
      importFrom("stats", "acf", "as.formula", "cpgram", "fitted",
                 "gaussian", "glm", "median", "model.frame", "na.omit",
                 "pchisq", "qchisq", "qnorm", "quantile", "relevel", "resid",
                 "residuals", "rgamma", "rnorm", "runif", "sd", "time")
    to your NAMESPACE file.
    ```

# seewave

Version: 2.0.5

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rpanel’
    ```

# selfea

Version: 1.0.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    get_statistics_from_file: no visible global function definition for
      ‘read.csv’
    glm_anova: no visible global function definition for ‘aov’
    glm_anova: no visible global function definition for ‘glm’
    glm_anova: no visible global function definition for ‘gaussian’
    glm_anova: no visible global function definition for ‘anova’
    glm_anova: no visible global function definition for ‘quasipoisson’
    glm_anova: no visible global function definition for ‘sd’
    glm_anova: no visible global function definition for ‘p.adjust’
    ttest_cohens_d: no visible global function definition for ‘t.test’
    ttest_cohens_d: no visible global function definition for ‘sd’
    Undefined global functions or variables:
      anova aov gaussian glm p.adjust quasipoisson read.csv sd t.test
    Consider adding
      importFrom("stats", "anova", "aov", "gaussian", "glm", "p.adjust",
                 "quasipoisson", "sd", "t.test")
      importFrom("utils", "read.csv")
    to your NAMESPACE file.
    ```

# sensitivity

Version: 1.15.0

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘fanovaGraph’
    ```

# SensusR

Version: 2.2.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    [1] "93% done merging data for SpeedDatum (15 of 16)."
    [1] "100% done merging data for SpeedDatum (16 of 16)."
    [1] "Creating data frame for SpeedDatum."
    [1] "100% done merging data for TelephonyDatum (1 of 1)."
    [1] "Creating data frame for TelephonyDatum."
    [1] "14% done merging data for WlanDatum (1 of 7)."
    [1] "28% done merging data for WlanDatum (2 of 7)."
    [1] "42% done merging data for WlanDatum (3 of 7)."
    [1] "57% done merging data for WlanDatum (4 of 7)."
    [1] "71% done merging data for WlanDatum (5 of 7)."
    [1] "85% done merging data for WlanDatum (6 of 7)."
    [1] "100% done merging data for WlanDatum (7 of 7)."
    [1] "Creating data frame for WlanDatum."
    > plot(data$LocationDatum)
    Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=38.0676352725243,-78.9510441850485&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
    Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=38.0676352725243,-78.9510441850485&sensor=false
    Warning: geocode failed with status OVER_QUERY_LIMIT, location = "38.0676352725243,-78.9510441850485"
    Error in data.frame(ll.lat = ll[1], ll.lon = ll[2], ur.lat = ur[1], ur.lon = ur[2]) : 
      arguments imply differing number of rows: 0, 1
    Calls: plot ... <Anonymous> -> ggmap -> get_map -> get_googlemap -> data.frame
    Execution halted
    ```

# SentimentAnalysis

Version: 1.3-0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘SnowballC’ ‘XML’ ‘mgcv’
      All declared Imports should be used.
    ```

# sentimentr

Version: 1.0.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘lexicon’ ‘syuzhet’
      All declared Imports should be used.
    ```

# sentometrics

Version: 0.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘compiler’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6339 marked UTF-8 strings
    ```

# SEPA

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      definition for ‘t.test’
    truetimepattern : <anonymous>: no visible global function definition
      for ‘p.adjust’
    truetimevisualize : <anonymous> : <anonymous>: no visible global
      function definition for ‘median’
    truetimevisualize: no visible binding for global variable ‘time’
    truetimevisualize: no visible binding for global variable ‘expmean’
    truetimevisualize: no visible binding for global variable ‘Gene’
    windowGOanalysis: no visible global function definition for ‘new’
    windowGOvisualize: no visible binding for global variable ‘Var1’
    windowGOvisualize: no visible binding for global variable ‘value’
    Undefined global functions or variables:
      Gene Var1 Var2 confint expmean fitted lines lm median new p.adjust
      pseudotime t.test time value xend yend
    Consider adding
      importFrom("graphics", "lines")
      importFrom("methods", "new")
      importFrom("stats", "confint", "fitted", "lm", "median", "p.adjust",
                 "t.test", "time")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# seqbias

Version: 1.24.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        libs   7.3Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Versioned 'LinkingTo' value for ‘Rsamtools’ is only usable in R >= 3.0.2
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘GenomicRanges’ which was already attached by Depends.
      Please remove these calls from your code.
    Packages in Depends field not imported from:
      ‘Biostrings’ ‘GenomicRanges’ ‘methods’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      function definition for ‘runif’
    random.intervals: no visible global function definition for ‘GRanges’
    random.intervals: no visible global function definition for ‘IRanges’
    seqbias.fit: no visible global function definition for ‘new’
    seqbias.load: no visible global function definition for ‘new’
    seqbias.predict: no visible global function definition for ‘is’
    seqbias.predict : <anonymous>: no visible global function definition
      for ‘seqnames’
    seqbias.predict : <anonymous>: no visible global function definition
      for ‘start’
    seqbias.predict : <anonymous>: no visible global function definition
      for ‘end’
    seqbias.predict : <anonymous>: no visible global function definition
      for ‘strand’
    Undefined global functions or variables:
      GRanges IRanges end is new runif seqnames start strand
    Consider adding
      importFrom("methods", "is", "new")
      importFrom("stats", "end", "runif", "start")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

*   checking compiled code ... NOTE
    ```
    File ‘seqbias/libs/seqbias.so’:
      Found ‘rand’, possibly from ‘rand’ (C)
        Objects: ‘sequencing_bias.o’, ‘twobitseq.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# SeqFeatR

Version: 0.2.4

## In both

*   checking whether package ‘SeqFeatR’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/SeqFeatR/new/SeqFeatR.Rcheck/00install.out’ for details.
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'SeqFeatR_tutorial.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm2e.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.6 ^^M
           
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# seqplots

Version: 1.14.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in rep("../", npos) : invalid 'times' argument
    Error: processing vignette 'QuickStart.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 13.7Mb
      sub-directories of 1Mb or more:
        doc        2.6Mb
        seqplots  10.3Mb
    ```

*   checking foreign function calls ... NOTE
    ```
    Foreign function call to a different package:
      .Call("BWGFile_summary", ..., PACKAGE = "rtracklayer")
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotHeatmap,list: no visible global function definition for ‘kmeans’
    plotHeatmap,list: no visible global function definition for ‘hclust’
    plotHeatmap,list: no visible global function definition for ‘dist’
    plotHeatmap,list: no visible global function definition for ‘cutree’
    plotHeatmap,list: no visible global function definition for
      ‘as.dendrogram’
    plotHeatmap,list: no visible global function definition for ‘title’
    Undefined global functions or variables:
      Var1 Var2 abline adjustcolor approx as.dendrogram axis box
      capture.output colorRampPalette cutree dist hclust image kmeans
      layout lines mtext par plot.new qt rainbow rect rgb text title value
    Consider adding
      importFrom("grDevices", "adjustcolor", "colorRampPalette", "rainbow",
                 "rgb")
      importFrom("graphics", "abline", "axis", "box", "image", "layout",
                 "lines", "mtext", "par", "plot.new", "rect", "text",
                 "title")
      importFrom("stats", "approx", "as.dendrogram", "cutree", "dist",
                 "hclust", "kmeans", "qt")
      importFrom("utils", "capture.output")
    to your NAMESPACE file.
    ```

# Seurat

Version: 2.1.0

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘Seurat-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DarkTheme
    > ### Title: Dark Theme
    > ### Aliases: DarkTheme
    > 
    > ### ** Examples
    > 
    > df <- data.frame(x = rnorm(n = 100, mean = 20, sd = 2), y = rbinom(n = 100, size = 100, prob = 0.2))
    > p <- ggplot(data = df, mapping = aes(x = x, y = y)) + geom_point(mapping = aes(color = 'red'))
    > p + DarkTheme(legend.position = 'none')
    Theme element panel.border missing
    Error in if (theme$panel.ontop) { : argument is of length zero
    Calls: <Anonymous> ... print.ggplot -> ggplot_gtable -> <Anonymous> -> f -> lapply -> FUN
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        |                                                                            
        |===================================================================   |  96%
        |                                                                            
        |====================================================================  |  98%
        |                                                                            
        |===================================================================== |  99%
        |                                                                            
        |======================================================================| 100%
      testthat results ================================================================
      OK: 210 SKIPPED: 0 FAILED: 2
      1. Failure: tSNE plots correctly (@test_seurat_object.R#115) 
      2. Error: tSNE plots correctly (@test_seurat_object.R#116) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.0Mb
      sub-directories of 1Mb or more:
        libs   5.0Mb
    ```

# sf

Version: 0.5-5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 194-197 (sf5.Rmd) 
    Error: processing vignette 'sf5.Rmd' failed with diagnostics:
    cannot open the connection
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 20.2Mb
      sub-directories of 1Mb or more:
        doc     10.7Mb
        libs     5.7Mb
        sqlite   1.5Mb
    ```

# sgd

Version: 1.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 13 SKIPPED: 0 FAILED: 10
      1.  Error: MSE converges for linear regression with lasso (@test-lasso.R#6) 
      2.  Failure: MSE converges for linear models (@test-linear.R#37) 
      3.  Failure: MSE converges for linear models (@test-linear.R#40) 
      4.  Failure: MSE converges for linear models (@test-linear.R#42) 
      5.  Failure: MSE converges for linear models (@test-linear.R#55) 
      6.  Failure: MSE converges for linear models (@test-linear.R#56) 
      7.  Failure: MSE converges for linear models (@test-linear.R#57) 
      8.  Failure: MSE converges for linear models (@test-linear.R#60) 
      9.  Failure: MSE converges for linear models (@test-linear.R#61) 
      10. Failure: MSE converges for linear models (@test-linear.R#62) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# sglr

Version: 0.7

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .computePStar: no visible global function definition for ‘uniroot’
    plotBoundary: no visible global function definition for
      ‘scale_y_continuous’
    Undefined global functions or variables:
      scale_y_continuous uniroot
    Consider adding
      importFrom("stats", "uniroot")
    to your NAMESPACE file.
    ```

# shazam

Version: 0.1.8

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# shiny

Version: 1.0.5

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.8Mb
      sub-directories of 1Mb or more:
        www   6.5Mb
    ```

# ShinyItemAnalysis

Version: 1.2.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘data.table’ ‘gridExtra’ ‘knitr’ ‘latticeExtra’ ‘msm’ ‘plotly’
      ‘xtable’
      All declared Imports should be used.
    ```

# shinyKGode

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘KGode’ ‘XML’ ‘ggplot2’ ‘gridExtra’ ‘mvtnorm’ ‘reshape2’ ‘shinyjs’
      ‘tools’
      All declared Imports should be used.
    ```

# SIBER

Version: 2.1.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘viridis’
      All declared Imports should be used.
    ```

# sicegar

Version: 0.2.2

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# SigFuge

Version: 1.14.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    SFfigure: no visible global function definition for ‘hcl’
    SFfigure: no visible binding for global variable ‘cluster’
    SFfigure: no visible binding for global variable ‘median’
    SFfigure: no visible binding for global variable ‘x’
    SFfigure: no visible binding for global variable ‘y1’
    SFfigure: no visible binding for global variable ‘label’
    SFfigure: no visible binding for global variable ‘value’
    SFfigure: no visible binding for global variable ‘Clusters’
    SFfigure: no visible binding for global variable ‘y2’
    SFfigure: no visible global function definition for ‘pdf’
    SFfigure: no visible global function definition for ‘dev.off’
    SFlabels: no visible global function definition for ‘kmeans’
    SFnormalize : <anonymous>: no visible global function definition for
      ‘median’
    Undefined global functions or variables:
      Clusters base cluster dev.off e hcl kmeans label median pdf rgb s
      value x y1 y2
    Consider adding
      importFrom("grDevices", "dev.off", "hcl", "pdf", "rgb")
      importFrom("stats", "kmeans", "median")
    to your NAMESPACE file.
    ```

# SIMAT

Version: 1.8.0

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Name: plotEIC
    > ### Title: Plotting EIC of one peak
    > ### Aliases: plotEIC
    > 
    > ### ** Examples
    > 
    >     # load an RData file including a single run data acquired by readCDF
    >     data("Run")
    >     
    >     # load targets information
    >     data(Targets)
    >     
    >     # get all the corresponding peaks of the target list
    >     runPeaks <- getPeak(Run = Run, Targets = Targets)
    >     
    >     # plot the EIC of the first target
    >     plotEIC(runPeaks[[1]])
    Error in if (ggplot_build(mainPlot)$layout$panel_ranges[[1]]$y.range[2] >  : 
      argument is of length zero
    Calls: plotEIC
    Execution halted
    ```

*   checking re-building of vignette outputs ... NOTE
    ```
    Error in re-building vignettes:
      ...
    Loading required package: Rcpp
    
    Error: processing vignette 'SIMAT-vignette.Rnw' failed with diagnostics:
     chunk 9 (label = plotEIC) 
    Error in if (ggplot_build(mainPlot)$layout$panel_ranges[[1]]$y.range[2] >  : 
      argument is of length zero
    Execution halted
    ```

## In both

*   checking R code for possible problems ... NOTE
    ```
    getRIStandard: no visible global function definition for ‘read.csv’
    plotEIC: no visible global function definition for ‘stack’
    plotEIC: no visible global function definition for ‘dnorm’
    plotEIC: no visible global function definition for ‘dev.copy’
    plotEIC: no visible binding for global variable ‘pdf’
    plotEIC: no visible global function definition for ‘dev.off’
    plotTIC: no visible binding for global variable ‘rt’
    Undefined global functions or variables:
      dev.copy dev.off dnorm pdf read.csv rt stack
    Consider adding
      importFrom("grDevices", "dev.copy", "dev.off", "pdf")
      importFrom("stats", "dnorm", "rt")
      importFrom("utils", "read.csv", "stack")
    to your NAMESPACE file.
    ```

# simcausal

Version: 0.5.4

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘tmlenet’
    ```

# SimMultiCorrData

Version: 0.2.1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘BinNonNor’, ‘PoisNor’, ‘PoisBinOrdNor’, ‘PoisBinOrdNonNor’
    ```

# simTool

Version: 1.0.3

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking R code for possible problems ... NOTE
    ```
    evalGrids: no visible global function definition for ‘sessionInfo’
    meanAndNormCI: no visible global function definition for ‘sd’
    Undefined global functions or variables:
      sd sessionInfo
    Consider adding
      importFrom("stats", "sd")
      importFrom("utils", "sessionInfo")
    to your NAMESPACE file.
    ```

# sincell

Version: 1.8.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    sc_InitializingSincellObject: no visible binding for global variable
      ‘var’
    sc_StatisticalSupportByGeneSubsampling: no visible global function
      definition for ‘cor’
    sc_StatisticalSupportByReplacementWithInSilicoCellsReplicates: no
      visible global function definition for ‘cor’
    sc_clusterObj: no visible global function definition for ‘hclust’
    sc_clusterObj: no visible global function definition for ‘cutree’
    sc_distanceObj: no visible global function definition for ‘cor’
    sc_marker2color: no visible global function definition for
      ‘colorRampPalette’
    Undefined global functions or variables:
      cmdscale colorRampPalette combn cor cutree hclust plot prcomp
      quantile rnbinom rnorm runif var
    Consider adding
      importFrom("grDevices", "colorRampPalette")
      importFrom("graphics", "plot")
      importFrom("stats", "cmdscale", "cor", "cutree", "hclust", "prcomp",
                 "quantile", "rnbinom", "rnorm", "runif", "var")
      importFrom("utils", "combn")
    to your NAMESPACE file.
    ```

*   checking compiled code ... NOTE
    ```
    File ‘sincell/libs/sincell.so’:
      Found ‘rand’, possibly from ‘rand’ (C)
        Objects: ‘pseudoreplicatesbymodel.o’, ‘pseudoreplicatesbynoise.o’,
          ‘pseudoreplicatesbynoise_cv2.o’
      Found ‘srand’, possibly from ‘srand’ (C)
        Objects: ‘pseudoreplicatesbymodel.o’, ‘pseudoreplicatesbynoise.o’,
          ‘pseudoreplicatesbynoise_cv2.o’
    
    Compiled code should not call entry points which might terminate R nor
    write to stdout/stderr instead of to the console, nor the system RNG.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# Single.mTEC.Transcriptomes

Version: 1.4.0

## In both

*   checking data for ASCII and uncompressed saves ... WARNING
    ```
      Warning: package needs dependence on R (>= 2.10)
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 911.9Mb
      sub-directories of 1Mb or more:
        data  895.1Mb
        doc    16.6Mb
    ```

# SISPA

Version: 1.6.0

## In both

*   checking files in ‘vignettes’ ... WARNING
    ```
    Files in the 'vignettes' directory but no files in 'inst/doc':
      ‘SISPA.Rmd’, ‘SISPA.md’, ‘SISPA_data.Rda’,
        ‘SISPA_files/figure-html/unnamed-chunk-2-1.png’,
        ‘SISPA_files/figure-html/unnamed-chunk-3-1.png’,
        ‘SISPA_files/figure-html/unnamed-chunk-4-1.png’,
        ‘SISPA_files/figure-html/unnamed-chunk-5-1.png’,
        ‘SISPA_files/figure-html/unnamed-chunk-6-1.png’,
        ‘SISPA_files/figure-html/unnamed-chunk-7-1.png’,
        ‘sispa_overview.png’
    The following directory looks like a leftover from 'knitr':
      ‘figure’
    Please remove from your package.
    ```

*   checking R code for possible problems ... NOTE
    ```
    cptSamples : cptsPlot: no visible global function definition for
      ‘abline’
    cptSamples : cptsPlot: no visible global function definition for ‘text’
    Undefined global functions or variables:
      abline text
    Consider adding
      importFrom("graphics", "abline", "text")
    to your NAMESPACE file.
    ```

# sjlabelled

Version: 1.0.5

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘sjPlot’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘sjPlot’
    ```

# sjmisc

Version: 2.6.2

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘sjPlot’
    ```

# sjPlot

Version: 2.4.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘prediction’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘plm’
    ```

# sjstats

Version: 0.12.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘sjPlot’, ‘MuMIn’, ‘piecewiseSEM’
    ```

# SmarterPoland

Version: 1.7

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.2Mb
      sub-directories of 1Mb or more:
        data   8.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1122 marked UTF-8 strings
    ```

# SMFI5

Version: 1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls to packages already attached by Depends:
      ‘ggplot2’ ‘reshape’
      Please remove these calls from your code.
    Packages in Depends field not imported from:
      ‘corpcor’ ‘ggplot2’ ‘reshape’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    sim.cir: no visible global function definition for ‘aes’
    sim.cir: no visible global function definition for ‘geom_line’
    sim.cir: no visible global function definition for ‘ggtitle’
    sim.n.chi2: no visible global function definition for ‘rchisq’
    sim.n.chi2: no visible global function definition for ‘rnorm’
    sim.n.chi2: no visible global function definition for ‘rpois’
    sim.vasicek: no visible global function definition for ‘rnorm’
    sim.vasicek: no visible global function definition for ‘melt’
    sim.vasicek: no visible global function definition for ‘ggplot’
    sim.vasicek: no visible global function definition for ‘aes’
    sim.vasicek: no visible global function definition for ‘geom_line’
    sim.vasicek: no visible global function definition for ‘ggtitle’
    Undefined global functions or variables:
      acf aes cor cov dchisq geom_line ggplot ggtitle head melt optim plot
      pseudoinverse qnorm rchisq rnorm rpois sd title
    Consider adding
      importFrom("graphics", "plot", "title")
      importFrom("stats", "acf", "cor", "cov", "dchisq", "optim", "qnorm",
                 "rchisq", "rnorm", "rpois", "sd")
      importFrom("utils", "head")
    to your NAMESPACE file.
    ```

# smoof

Version: 1.5.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rPython’
    ```

# snht

Version: 1.0.5

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'pairwiseSNHT.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm2e.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.63 \usepackage
                    {mathtools}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# SNPhood

Version: 1.6.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'IntroductionToSNPhood.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  8.1Mb
      sub-directories of 1Mb or more:
        data   3.8Mb
        doc    3.8Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    BugReports field is not a suitable URL but appears to contain an email address
      not specified by mailto: nor contained in < >
    ```

*   checking R code for possible problems ... NOTE
    ```
    .calcBinomTestVector: no visible binding for global variable ‘pp’
    Undefined global functions or variables:
      pp
    ```

# soc.ca

Version: 0.7.3

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 833 marked UTF-8 strings
    ```

# soGGi

Version: 1.8.0

## In both

*   checking whether package ‘soGGi’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::Position’ by ‘BiocGenerics::Position’ when loading ‘soGGi’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/soGGi/new/soGGi.Rcheck/00install.out’ for details.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    Codoc mismatches from documentation object 'c,ChIPprofile-method':
    \S4method{c}{ChIPprofile}
      Code: function(x, ...)
      Docs: function(x, ..., recursive = FALSE)
      Argument names in docs not in code:
        recursive
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
      definition for ‘weighted.mean’
    getShifts: no visible global function definition for
      ‘readGAlignmentsFromBam’
    getSummitScore: no visible global function definition for
      ‘readGAlignmentsFromBam’
    plotRegion.ChIPprofile: no visible global function definition for
      ‘formula’
    runFindSummit: no visible global function definition for
      ‘readGAlignmentsFromBam’
    runRegionPlot : <anonymous>: no visible global function definition for
      ‘spline’
    summitPipeline: no visible global function definition for
      ‘readGAlignmentsFromBam’
    plotRegion,ChIPprofile: no visible global function definition for
      ‘formula’
    Undefined global functions or variables:
      formula read.delim readGAlignmentsFromBam spline weighted.mean
    Consider adding
      importFrom("stats", "formula", "spline", "weighted.mean")
      importFrom("utils", "read.delim")
    to your NAMESPACE file.
    ```

# soilcarbon

Version: 1.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 145 marked UTF-8 strings
    ```

# SomaticCancerAlterations

Version: 1.12.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    .load_dataset: no visible global function definition for ‘data’
    .maf2gr: no visible binding for global variable ‘Chromosome’
    .maf2gr: no visible binding for global variable ‘Start_position’
    .maf2gr: no visible binding for global variable ‘End_position’
    .read_maf: no visible global function definition for ‘read.delim’
    hg2ncbi: no visible global function definition for ‘seqnameStyle<-’
    hg2ncbi: no visible global function definition for ‘genome<-’
    mutationDensity: no visible global function definition for
      ‘keepSeqlevels’
    mutationDensity: no visible global function definition for ‘as’
    ncbi2hg: no visible global function definition for ‘seqnameStyle<-’
    ncbi2hg: no visible global function definition for ‘genome<-’
    scaListDatasets: no visible global function definition for ‘data’
    Undefined global functions or variables:
      Chromosome End_position Start_position as data genome<- keepSeqlevels
      read.delim seqnameStyle<-
    Consider adding
      importFrom("methods", "as")
      importFrom("utils", "data", "read.delim")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# SomaticSignatures

Version: 2.12.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Package listed in more than one of Depends, Imports, Suggests, Enhances:
      ‘NMF’
    A package should be listed in only one of these fields.
    ```

# sorvi

Version: 0.7.26

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    regression_plot: no visible global function definition for
      ‘colorRampPalette’
    regression_plot: no visible global function definition for
      ‘loess.control’
    regression_plot: no visible global function definition for ‘predict’
    regression_plot : <anonymous>: no visible global function definition
      for ‘quantile’
    regression_plot : <anonymous>: no visible global function definition
      for ‘pnorm’
    regression_plot: no visible global function definition for
      ‘flush.console’
    regression_plot: no visible global function definition for ‘density’
    Undefined global functions or variables:
      colorRampPalette density flush.console loess loess.control pnorm
      predict quantile read.csv
    Consider adding
      importFrom("grDevices", "colorRampPalette")
      importFrom("stats", "density", "loess", "loess.control", "pnorm",
                 "predict", "quantile")
      importFrom("utils", "flush.console", "read.csv")
    to your NAMESPACE file.
    ```

# sparklyr

Version: 0.6.4

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > Sys.setenv("R_TESTS" = "")
      > library(testthat)
      > library(sparklyr)
      > 
      > if (identical(Sys.getenv("NOT_CRAN"), "true")) {
      +   test_check("sparklyr")
      +   on.exit({ spark_disconnect_all() ; livy_service_stop() })
      + }
      Error in if (is.na(a)) return(-1L) : argument is of length zero
      Calls: test_check ... spark_install_find -> spark_versions -> lapply -> FUN -> compareVersion
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

# SpatialEpiApp

Version: 0.3

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘INLA’
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RColorBrewer’ ‘SpatialEpi’ ‘dplyr’ ‘dygraphs’ ‘ggplot2’
      ‘htmlwidgets’ ‘knitr’ ‘leaflet’ ‘mapproj’ ‘maptools’ ‘rgdal’ ‘rgeos’
      ‘rmarkdown’ ‘shinyjs’ ‘spdep’ ‘xts’
      All declared Imports should be used.
    ```

# spatialwarnings

Version: 1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘moments’ ‘poweRlaw’
      All declared Imports should be used.
    ```

# specmine

Version: 1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘specmine-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: impute_nas_linapprox
    > ### Title: Impute missing values with linear approximation
    > ### Aliases: impute_nas_linapprox
    > ### Keywords: missing values
    > 
    > ### ** Examples
    > 
    >   ## Example of NA imputation with linear approximation
    >   data(propolis)
    >   dataset = impute_nas_linapprox(propolis)
    Error: 'spc.NA.linapprox' is not an exported object from 'namespace:hyperSpec'
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported objects:
      ‘ChemoSpec::readJDX’ ‘hyperSpec::spc.NA.linapprox’
    ```

# spew

Version: 1.3.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘maptools’
      All declared Imports should be used.
    ```

# SpidermiR

Version: 1.7.4

## In both

*   checking R code for possible problems ... NOTE
    ```
    .SpidermiRvisualize_gene: possible error in simpleNetwork(NetworkData,
      linkColour = "gray", textColour = "black", zoom = TRUE): unused
      argument (textColour = "black")
    SpidermiRvisualize_plot_target: no visible binding for global variable
      ‘miRNAs’
    SpidermiRvisualize_plot_target: no visible binding for global variable
      ‘mRNA_target’
    Undefined global functions or variables:
      mRNA_target miRNAs
    ```

# spikeSlabGAM

Version: 1.1-11

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
      Some 'x' values are beyond 'boundary.knots'; Linear extrapolation used.
    Warning in (function (xnew)  :
      predictions outside fitted range for lin(glucose).
    Warning in (function (xnew)  :
      predictions outside fitted range for sm(glucose).
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'UsingSpikeSlabGAM.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithmic.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.12 \usepackage
                    {algorithm}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# spind

Version: 2.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘sp’
      All declared Imports should be used.
    ```

# splatter

Version: 1.0.3

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'splatter.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# sppmix

Version: 1.0.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.3Mb
      sub-directories of 1Mb or more:
        data   5.4Mb
        libs   9.3Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 17 marked UTF-8 strings
    ```

# ss3sim

Version: 0.9.5

## In both

*   checking whether package ‘ss3sim’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/ss3sim/new/ss3sim.Rcheck/00install.out’ for details.
    ```

# ssviz

Version: 1.10.0

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

# stacomiR

Version: 0.5.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgetsRGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# starmie

Version: 0.1.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        doc       1.1Mb
        extdata   4.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘MCMCpack’
      All declared Imports should be used.
    ```

# statcheck

Version: 1.2.2

## In both

*   checking whether package ‘statcheck’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/statcheck/new/statcheck.Rcheck/00install.out’ for details.
    ```

# STATegRa

Version: 1.10.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning in cor.test.default(getDist(listDistW[[i]])[referenceFeatures, referenceFeatures],  :
      Cannot compute exact p-value with ties
    Warning in plot.window(...) :
      relative range of values =  18 * EPS, is small (axis 2)
    Warning in plot.window(...) :
      relative range of values =  18 * EPS, is small (axis 2)
    Warning in plot.window(...) :
      relative range of values =  18 * EPS, is small (axis 2)
    Warning in plot.window(...) :
      relative range of values =  18 * EPS, is small (axis 2)
    Warning in replayPlot(x) :
      relative range of values =  18 * EPS, is small (axis 2)
    Warning in replayPlot(x) :
      relative range of values =  18 * EPS, is small (axis 2)
    Warning in replayPlot(x) :
      relative range of values =  18 * EPS, is small (axis 2)
    Warning in replayPlot(x) :
      relative range of values =  18 * EPS, is small (axis 2)
    Error: processing vignette 'STATegRa.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        data   2.4Mb
        doc    2.4Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    biplotRes,caClass-character-numeric-character: no visible binding for
      global variable ‘values.1’
    biplotRes,caClass-character-numeric-character: no visible binding for
      global variable ‘values.2’
    biplotRes,caClass-character-numeric-character: no visible binding for
      global variable ‘color’
    plotVAF,caClass: no visible binding for global variable ‘comp’
    plotVAF,caClass: no visible binding for global variable ‘VAF’
    plotVAF,caClass: no visible binding for global variable ‘block’
    selectCommonComps,matrix-matrix-numeric: no visible binding for global
      variable ‘comps’
    selectCommonComps,matrix-matrix-numeric: no visible binding for global
      variable ‘block’
    selectCommonComps,matrix-matrix-numeric: no visible binding for global
      variable ‘comp’
    selectCommonComps,matrix-matrix-numeric: no visible binding for global
      variable ‘ratio’
    Undefined global functions or variables:
      VAF block color comp comps ratio values.1 values.2
    ```

# statisticalModeling

Version: 0.3.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(statisticalModeling)
      Loading required package: ggplot2
      > 
      > test_check("statisticalModeling")
      Error: gwm() has been removed from `mosaic'.  
          It will be replaced by better tools in `mosaicModel'.
      testthat results ================================================================
      OK: 21 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 303-306 (modeling.Rmd) 
    Error: processing vignette 'modeling.Rmd' failed with diagnostics:
    Invalid formula type for gf_point.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘magrittr’ ‘rpart’
      All declared Imports should be used.
    ```

# statsDK

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘stringr’
      All declared Imports should be used.
    ```

# strataG

Version: 2.0.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        libs   4.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘survival’
      All declared Imports should be used.
    ```

# StroupGLMM

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘car’ ‘lmerTest’ ‘pbkrtest’
      All declared Imports should be used.
    ```

# structSSI

Version: 1.1.1

## In both

*   checking R code for possible problems ... NOTE
    ```
    PlotHypTree: no visible global function definition for ‘browseURL’
    treePValues: no visible global function definition for ‘lm’
    treePValues: no visible global function definition for ‘pf’
    plot,GBH-ANY: no visible binding for global variable ‘sorted.hyp’
    plot,GBH-ANY: no visible binding for global variable ‘pval’
    plot,GBH-ANY: no visible binding for global variable ‘group’
    plot,GBH-ANY: no visible binding for global variable ‘type’
    Undefined global functions or variables:
      browseURL group lm pf pval sorted.hyp type
    Consider adding
      importFrom("stats", "lm", "pf")
      importFrom("utils", "browseURL")
    to your NAMESPACE file.
    ```

# subSeq

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    summary.subsamples: no visible binding for global variable ‘o.padj’
    summary.subsamples: no visible binding for global variable
      ‘significant’
    summary.subsamples: no visible binding for global variable ‘estFDP’
    summary.subsamples: no visible binding for global variable ‘rFDP’
    summary.subsamples: no visible binding for global variable ‘metric’
    summary.subsamples: no visible binding for global variable ‘value’
    summary.subsamples: no visible binding for global variable ‘percent’
    voomLimma: no visible global function definition for ‘model.matrix’
    Undefined global functions or variables:
      . ID average.depth average.value coefficient cor count cov depth
      estFDP method metric model.matrix o.coefficient o.lfdr o.padj
      p.adjust padj percent plot proportion pvalue rFDP rbinom replication
      selectMethod significant valid value var
    Consider adding
      importFrom("graphics", "plot")
      importFrom("methods", "selectMethod")
      importFrom("stats", "cor", "cov", "model.matrix", "p.adjust", "rbinom",
                 "var")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# subspaceMOA

Version: 0.6.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘fields’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘subspace’
    ```

# sure

Version: 0.2.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘truncdist’
    ```

# surveillance

Version: 1.15.0

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking: ‘hhh4contacts’ ‘INLA’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  7.3Mb
      sub-directories of 1Mb or more:
        doc    2.3Mb
        help   1.0Mb
        libs   1.7Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘coin’, ‘VGAM’
    ```

# survivALL

Version: 0.9.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Biobase’ ‘broom’ ‘data.table’ ‘magrittr’ ‘pander’ ‘png’ ‘readr’
      ‘survsim’ ‘testthat’
      All declared Imports should be used.
    ```

# survminer

Version: 0.4.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc   4.9Mb
    ```

# SWATH2stats

Version: 1.6.1

## In both

*   checking package dependencies ... NOTE
    ```
    Packages which this enhances but not available for checking:
      ‘imsbInfer’ ‘MSstats’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘MSstats’
    ```

# swfdr

Version: 1.0.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'swfdrTutorial.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# switchde

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following object is masked from 'package:Biobase':
    
        combine
    
    The following objects are masked from 'package:BiocGenerics':
    
        combine, intersect, setdiff, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Warning: Removed 68 rows containing missing values (geom_path).
    Error: processing vignette 'switchde_vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# synlet

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
    siRNAPlot: no visible global function definition for ‘pdf’
    siRNAPlot: no visible global function definition for ‘dev.off’
    tTest: no visible global function definition for ‘p.adjust’
    zFactor: no visible binding for global variable ‘condition’
    zFactor: no visible binding for global variable ‘sd’
    zFactor: no visible binding for global variable ‘median’
    zFactor: no visible global function definition for ‘complete.cases’
    Undefined global functions or variables:
      COL_NAME EXPERIMENT_MODIFICATION EXPERIMENT_TYPE MASTER_PLATE PLATE
      READOUT ROW_NAME Var1 WELL_CONTENT_NAME colorRampPalette
      complete.cases condition dev.off experiments is mad median medpolish
      p.adjust pdf phyper rainbow sd siRNA t.test value write.table
    Consider adding
      importFrom("grDevices", "colorRampPalette", "dev.off", "pdf",
                 "rainbow")
      importFrom("methods", "is")
      importFrom("stats", "complete.cases", "mad", "median", "medpolish",
                 "p.adjust", "phyper", "sd", "t.test")
      importFrom("utils", "write.table")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# synthpop

Version: 1.4-0

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    ...
      ...
    Loading required package: lattice
    Loading required package: MASS
    Loading required package: nnet
    Loading required package: ggplot2
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'inference.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.25 \usepackage
                    {algpseudocode}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# systemPipeR

Version: 1.10.2

## In both

*   checking package subdirectories ... WARNING
    ```
    Invalid citation information in ‘inst/CITATION’:
      Error in library(knitcitations): there is no package called ‘knitcitations’
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'systemPipeR.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking for hidden files and directories ... NOTE
    ```
    Found the following hidden files and directories:
      inst/extdata/.BatchJobs.R
    These were most likely included in error. See section ‘Package
    structure’ in the ‘Writing R Extensions’ manual.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        doc       3.0Mb
        extdata   1.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BiocGenerics’ ‘VariantAnnotation’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘close.screen’
    writeTargetsRef: no visible global function definition for ‘read.delim’
    show,INTERSECTset: no visible binding for global variable ‘vennset’
    Undefined global functions or variables:
      AllVariants Base Comparisons Counts Coverage Cycle DataFrame Feature
      Frequency IRanges Intersect_Sets Length Level Method Outliers Percent
      Quality RelDiv Rle Sample SampleMatch Strand Type aggregate alt
      altDepth<- asVCF boxplot chunk close.screen combn dev.off first
      import.bed last locateVariants low mid minQuality model.matrix
      na.omit pdf phyper plot predictCoding qwidth read.delim
      readGAlignmentPairs readGAlignments readVcf ref refDepth<- screen
      seqlengths seqlengths<- split.screen subsetByOverlaps
      summarizeOverlaps symbols test_sample text top tophatargs
      totalDepth<- vennset write.table writeVcf
    Consider adding
      importFrom("grDevices", "dev.off", "pdf")
      importFrom("graphics", "boxplot", "close.screen", "plot", "screen",
                 "split.screen", "symbols", "text")
      importFrom("stats", "aggregate", "model.matrix", "na.omit", "phyper")
      importFrom("utils", "combn", "read.delim", "write.table")
    to your NAMESPACE file.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' call not declared from: ‘GenomicAlignments’
    ```

# tadaatoolbox

Version: 0.14.0

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘rmdformats’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 3 marked UTF-8 strings
    ```

# taRifx

Version: 1.0.6

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Title field: should not end in a period.
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘gdata’ ‘ggplot2’ ‘grid’ ‘lattice’ ‘xtable’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    xtable.summary.lme: no visible global function definition for ‘align<-’
    xtable.summary.lme: no visible global function definition for
      ‘digits<-’
    xtable.summary.lme: no visible global function definition for
      ‘display<-’
    xtablelm: no visible global function definition for ‘xtable’
    xtablelm: no visible global function definition for ‘pf’
    Undefined global functions or variables:
      aes align<- barplot caption<- convertUnit coord_cartesian digits<-
      display<- ecdf ggplot gpar grid.layout grid.lines grid.newpage
      grid.points grid.polyline grid.rect grid.segments grid.text
      interleave label<- latticeParseFormula median na.omit opts
      panel.densityplot panel.lines panel.xyplot par pf plot.new
      popViewport pushViewport quantile sd seekViewport stat_summary terms
      text theme_text time unit upViewport viewport write.csv xtable
    Consider adding
      importFrom("graphics", "barplot", "par", "plot.new", "text")
      importFrom("stats", "ecdf", "median", "na.omit", "pf", "quantile",
                 "sd", "terms", "time")
      importFrom("utils", "write.csv")
    to your NAMESPACE file.
    ```

# TCGAbiolinks

Version: 2.5.9

## In both

*   checking examples ... ERROR
    ```
    ...
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    |NA                          |NA                                  |NA                   |NA                           |
    Error in checkProjectInput(project) : 
      Please set a valid project argument from the column id above. Project TCGA-ACC was not found.
    Calls: GDCquery -> checkProjectInput
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      testthat results ================================================================
      OK: 123 SKIPPED: 0 FAILED: 15
      1. Error: TCGAanalyze_survival creates pdf (@test-analyse.R#4) 
      2. Error: GDCdownload API method for two files is working  (@test-prepare-download.R#4) 
      3. Error: GDCdownload API method for one files is working  (@test-prepare-download.R#20) 
      4. Error: GDCprepare accepts more than one project (@test-prepare-download.R#50) 
      5. Error: Accecpts more than one platform (@test-prepare-download.R#68) 
      6. Error: GDCquery can filter by data.category (@test-query.R#5) 
      7. Error: GDCquery accepts more than one project (@test-query.R#11) 
      8. Error: GDCquery can filter by sample.type (@test-query.R#23) 
      9. Error: GDCquery can filter by barcode (@test-query.R#46) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:S4Vectors':
    
        first, intersect, rename, setdiff, setequal, union
    
    The following objects are masked from 'package:BiocGenerics':
    
        combine, intersect, setdiff, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 16-20 (clinical.Rmd) 
    Error: processing vignette 'clinical.Rmd' failed with diagnostics:
    there is no package called 'DT'
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 61.0Mb
      sub-directories of 1Mb or more:
        R      1.1Mb
        data   2.3Mb
        doc   57.4Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘limmacontrasts.fit’
    TCGAanalyze_analyseGRN: no visible global function definition for
      ‘knnmi.cross’
    TCGAanalyze_networkInference: no visible global function definition for
      ‘c3net’
    TCGAanalyze_networkInference: no visible global function definition for
      ‘minet’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetInduce’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dNetPipeline’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘dCommSignif’
    TCGAvisualize_SurvivalCoxNET: no visible global function definition for
      ‘visNet’
    TCGAvisualize_oncoprint: no visible binding for global variable ‘value’
    getTSS: no visible global function definition for ‘promoters’
    Undefined global functions or variables:
      c3net dCommSignif dNetInduce dNetPipeline knnmi.cross
      limmacontrasts.fit limmamakeContrasts minet portions promoters value
      visNet
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' call not declared from: ‘DT’
    ```

# TCGAbiolinksGUI

Version: 1.2.1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following objects are masked from 'package:S4Vectors':
    
        first, intersect, rename, setdiff, setequal, union
    
    The following objects are masked from 'package:BiocGenerics':
    
        combine, intersect, setdiff, union
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 11-15 (data.Rmd) 
    Error: processing vignette 'data.Rmd' failed with diagnostics:
    there is no package called 'DT'
    Execution halted
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 30.3Mb
      sub-directories of 1Mb or more:
        app   1.1Mb
        doc  28.9Mb
    ```

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking for unstated dependencies in vignettes ... NOTE
    ```
    'library' or 'require' calls not declared from:
      ‘DT’ ‘dplyr’
    ```

# tcR

Version: 2.2.1.11

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.6Mb
      sub-directories of 1Mb or more:
        data   1.2Mb
        doc    3.9Mb
        libs   1.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘scales’
      All declared Imports should be used.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'top.fun':
      ‘slice.fun’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# TCseq

Version: 1.0.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    DBresult: no visible global function definition for ‘as’
    DBresult.cluster: no visible global function definition for ‘as’
    TCA: no visible global function definition for ‘is’
    TCA: no visible global function definition for ‘as’
    TCAFromSummarizedExperiment: no visible global function definition for
      ‘is’
    TCAFromSummarizedExperiment: no visible global function definition for
      ‘as’
    countReads: no visible global function definition for
      ‘createAnnotationFile’
    countReads: no visible global function definition for ‘featureCounts’
    timeclustplot: no visible binding for global variable ‘group’
    timecourseTable: no visible global function definition for ‘as’
    Undefined global functions or variables:
      as createAnnotationFile featureCounts group is
    Consider adding
      importFrom("methods", "as", "is")
    to your NAMESPACE file (and ensure that your DESCRIPTION Imports field
    contains 'methods').
    ```

# tdr

Version: 0.11

## In both

*   checking R code for possible problems ... NOTE
    ```
    makeCircles: no visible global function definition for ‘quantile’
    targetDiagram: no visible global function definition for ‘as.formula’
    targetDiagram: no visible global function definition for ‘modifyList’
    targetDiagram: no visible global function definition for ‘extendrange’
    tdStats : sdo: no visible global function definition for ‘sd’
    tdStats : sdm: no visible global function definition for ‘sd’
    tdStats : r2: no visible global function definition for ‘cor’
    Undefined global functions or variables:
      as.formula cor extendrange modifyList quantile sd
    Consider adding
      importFrom("grDevices", "extendrange")
      importFrom("stats", "as.formula", "cor", "quantile", "sd")
      importFrom("utils", "modifyList")
    to your NAMESPACE file.
    ```

# teachingApps

Version: 1.0.2

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        apps   2.6Mb
        libs   2.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘data.table’ ‘datasets’ ‘stats’
      All declared Imports should be used.
    ```

# TeachingDemos

Version: 2.10

## In both

*   R CMD check timed out
    

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘R2wd’
    ```

# TELP

Version: 1.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘TELP-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: telp
    > ### Title: The Base Function of This Package The Free Evocation of Words
    > ###   Technique
    > ### Aliases: telp
    > ### Keywords: function
    > 
    > ### ** Examples
    > 
    > telp()
    Error in structure(.External(.C_dotTclObjv, objv), class = "tclObj") : 
      [tcl] invalid command name "toplevel".
    Calls: telp ... tktoplevel -> tkwidget -> tcl -> .Tcl.objv -> structure
    Execution halted
    ```

*   checking whether package ‘TELP’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/TELP/new/TELP.Rcheck/00install.out’ for details.
    ```

# texmexseq

Version: 0.3

## In both

*   checking examples ... ERROR
    ```
    ...
    > 
    > # make up some data
    > sim.data <- function() rpoilog(1000, 1.0, 1.0, condS=TRUE)
    > otu <- data.frame(sample0=sim.data())
    > for (i in 1:10) otu[[paste('sample', i, sep='')]] <- sim.data()
    > otu.ids <- paste('otu', seq(1:1000), sep='')
    > rownames(otu) <- otu.ids
    > z.table <- z.transform.table(otu)
    Warning in value[[3L]](cond) : fit 1 failed
    Warning in value[[3L]](cond) : fit 1 failed
    Warning in value[[3L]](cond) : fit 2 failed
    > 
    > # pull out a quad, imagining that samples 1 and 2 were the control samples
    > # and 3 and 4 were the treatment
    > q <- quad.table(z.table, 'sample1', 'sample2', 'sample3', 'sample4')
    > 
    > # plot it
    > p <- quad.plot(q)
    Error in get("d.control") : object 'd.control' not found
    Calls: quad.plot ... lapply -> FUN -> overscope_eval_next -> .Call -> get
    Execution halted
    ```

# tidyjson

Version: 0.2.2

## In both

*   checking examples ... ERROR
    ```
    ...
    > # companies[[1]] %>% prettify
    > 
    > # Get the key employees data
    > key_employees <- companies %>%
    +   spread_values(
    +     name = jstring("name")
    +   ) %>%
    +   mutate(
    +     company.sort_order = rank(name)
    +   ) %>%
    +   enter_object("relationships") %>%
    +   gather_array("relationship.index") %>%
    +   spread_values(
    +     is.past = jlogical("is_past"),
    +     name = jstring("person", "permalink"),
    +     title = jstring("title")
    +   )
    Error in eval(assertion, env) : 
      argument "json.column" is missing, with no default
    Calls: %>% ... tryCatchList -> tryCatchOne -> doTryCatch -> eval -> eval
    Execution halted
    ```

# tidyquant

Version: 0.5.3

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      
      testthat results ================================================================
      OK: 179 SKIPPED: 0 FAILED: 3
      1. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_key_stats.R#15) 
      2. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_key_stats.R#17) 
      3. Failure: Test returns tibble with correct rows and columns. (@test_tq_get_key_stats.R#19) 
      
      Error: testthat unit tests failed
      In addition: Warning messages:
      1: In download.file(url, destfile = tmp, quiet = TRUE) :
        cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
      2: x = 'AAPL', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
       
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Warning in download.file(url, destfile = tmp, quiet = TRUE) :
      cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
    Warning: x = 'AAPL', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
    
    Warning in download.file(url, destfile = tmp, quiet = TRUE) :
      cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
    Warning: x = 'AAPL', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=AAPL&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
     Removing AAPL.
    Warning in download.file(url, destfile = tmp, quiet = TRUE) :
      cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=FB&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
    Warning: x = 'FB', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=FB&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
     Removing FB.
    Warning in download.file(url, destfile = tmp, quiet = TRUE) :
      cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=GOOG&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv': HTTP status was '403 Forbidden'
    Warning: x = 'GOOG', get = 'key.stats': Error in download.file(url, destfile = tmp, quiet = TRUE): cannot open URL 'http://download.finance.yahoo.com/d/quotes.csv?s=GOOG&f=aa2a5bb4b6c1c4dd1ee7e8e9f6ghjj1j2j4j5j6kk3k4k5ll1mm3m4m5m6m7m8nopp2p5p6qrr1r5r6r7s6s7t8vwxy&e=.csv'
     Removing GOOG.
    Warning in value[[3L]](cond) : Returning as nested data frame.
    Quitting from lines 211-214 (TQ01-core-functions-in-tidyquant.Rmd) 
    Error: processing vignette 'TQ01-core-functions-in-tidyquant.Rmd' failed with diagnostics:
    object 'Ask' not found
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘broom’ ‘curl’ ‘devtools’ ‘rvest’ ‘timeSeries’ ‘tseries’ ‘zoo’
      All declared Imports should be used.
    ```

# tidyverse

Version: 1.2.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dbplyr’ ‘reprex’ ‘rlang’
      All declared Imports should be used.
    ```

# tikzDevice

Version: 0.10-1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 675-675 (tikzDevice.Rnw) 
    Error: processing vignette 'tikzDevice.Rnw' failed with diagnostics:
    there is no package called 'formatR'
    Execution halted
    ```

# timeline

Version: 0.9

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘shiny’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Package in Depends field not imported from: ‘ggplot2’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    timeline: no visible global function definition for ‘ggplot’
    timeline: no visible global function definition for ‘geom_segment’
    timeline: no visible global function definition for ‘aes_string’
    timeline: no visible global function definition for ‘geom_rect’
    timeline: no visible global function definition for ‘geom_text’
    timeline: no visible global function definition for ‘theme’
    timeline: no visible global function definition for ‘element_blank’
    timeline: no visible global function definition for ‘xlab’
    timeline: no visible global function definition for ‘ylab’
    timeline: no visible global function definition for ‘xlim’
    timeline: no visible global function definition for
      ‘scale_y_continuous’
    timeline: no visible global function definition for ‘geom_point’
    timeline: no visible global function definition for ‘scale_color_grey’
    timeline: no visible global function definition for ‘geom_hline’
    Undefined global functions or variables:
      aes_string element_blank geom_hline geom_point geom_rect geom_segment
      geom_text ggplot scale_color_grey scale_y_continuous theme xlab xlim
      ylab
    ```

# timelineS

Version: 0.1.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘base’
      All declared Imports should be used.
    ```

# TimeProjection

Version: 0.2.0

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘TimeProjection-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: projectDate
    > ### Title: Time Projection
    > ### Aliases: projectDate
    > 
    > ### ** Examples
    > 
    > dates = timeSequence(from = "2001-01-01", to = "2004-01-01", by = "day")
    >    projectDate(as.Date(dates))
    Error in if (all(diff(as.numeric(raw[, i])) == 0)) redundantCols[i] = T : 
      missing value where TRUE/FALSE needed
    Calls: projectDate
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls in package code:
      ‘ggplot2’ ‘plyr’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Packages in Depends field not imported from:
      ‘Matrix’ ‘lubridate’ ‘timeDate’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotCalendarHeatmap: no visible global function definition for ‘ddply’
    plotCalendarHeatmap: no visible global function definition for ‘.’
    plotCalendarHeatmap: no visible binding for global variable ‘year’
    plotCalendarHeatmap: no visible binding for global variable ‘month’
    plotCalendarHeatmap: no visible binding for global variable ‘week’
    plotCalendarHeatmap: no visible global function definition for ‘ggplot’
    plotCalendarHeatmap: no visible global function definition for ‘aes’
    plotCalendarHeatmap: no visible binding for global variable ‘monthweek’
    plotCalendarHeatmap: no visible binding for global variable ‘weekday’
    plotCalendarHeatmap: no visible global function definition for
      ‘geom_tile’
    plotCalendarHeatmap: no visible global function definition for
      ‘facet_grid’
    plotCalendarHeatmap: no visible global function definition for
      ‘scale_fill_gradientn’
    projectDate: no visible global function definition for ‘holidayNYSE’
    projectDate: no visible global function definition for
      ‘sparse.model.matrix’
    Undefined global functions or variables:
      . aes ddply facet_grid geom_tile ggplot holidayNYSE isWeekday month
      monthweek scale_fill_gradientn sparse.model.matrix week weekday year
    ```

# TimerQuant

Version: 1.6.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘rainbow’
    plotPrimordiumProfile: no visible binding for global variable ‘median’
    plotPrimordiumProfile: no visible binding for global variable ‘mad’
    plotPrimordiumProfile: no visible global function definition for ‘par’
    plotPrimordiumProfile: no visible global function definition for ‘plot’
    plotPrimordiumProfile: no visible global function definition for ‘axis’
    plotPrimordiumProfile: no visible global function definition for
      ‘points’
    plotPrimordiumProfile: no visible global function definition for
      ‘polygon’
    plotPrimordiumProfile: no visible global function definition for ‘rgb’
    simulatedRatio: no visible global function definition for ‘rnorm’
    Undefined global functions or variables:
      approxfun axis mad median optimize par plot points polygon predict
      rainbow rgb rnorm
    Consider adding
      importFrom("grDevices", "rainbow", "rgb")
      importFrom("graphics", "axis", "par", "plot", "points", "polygon")
      importFrom("stats", "approxfun", "mad", "median", "optimize",
                 "predict", "rnorm")
    to your NAMESPACE file.
    ```

# tis

Version: 1.32

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘zoo’
    ```

# tmap

Version: 1.10

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   1.5Mb
        doc    3.3Mb
    ```

# toaster

Version: 0.5.5

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      target is NULL, current is character
      
      
      2. Failure: format 'boxplot' with facets works (@test-showData.R#21) -----------
      nrow(p$layout$panel_layout) not equal to length(cols).
      target is NULL, current is numeric
      
      
      testthat results ================================================================
      OK: 496 SKIPPED: 0 FAILED: 2
      1. Failure: format 'boxplot' works (@test-showData.R#12) 
      2. Failure: format 'boxplot' with facets works (@test-showData.R#21) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘igraph’
    ```

# TOSTER

Version: 0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘jmvcore’
      All declared Imports should be used.
    ```

# tourrGui

Version: 0.4

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    License components which are templates and need '+ file LICENSE':
      MIT
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' calls to packages already attached by Depends:
      ‘Cairo’ ‘RGtk2’ ‘colorspace’ ‘gWidgets’ ‘tourr’
      Please remove these calls from your code.
    'library' or 'require' calls in package code:
      ‘TeachingDemos’ ‘ash’
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Packages in Depends field not imported from:
      ‘Cairo’ ‘RGtk2’ ‘colorspace’ ‘gWidgets’ ‘tourr’
      These packages need to be imported from (in the NAMESPACE file)
      for when this namespace is loaded but not attached.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘gmessage’
    gui_xy: no visible global function definition for ‘dev.cur’
    gui_xy: no visible global function definition for ‘CairoX11’
    gui_xy: no visible global function definition for ‘dev.list’
    gui_xy: no visible global function definition for ‘dev.new’
    gui_xy: no visible global function definition for ‘dev.control’
    gui_xy: no visible global function definition for ‘visible<-’
    Undefined global functions or variables:
      CairoX11 basis_init cmass dev.control dev.cur dev.list dev.new
      display_andrews display_dist display_faces display_image display_pcp
      display_scatmat display_stars display_stereo display_xy dispose
      find_platform flea gIdleAdd gbutton gcheckbox gcheckboxgroup
      gdroplist ggroup glayout gmessage gnotebook gradio grand_tour gslider
      gtable gtkIdleRemove guided_tour gwindow holes lda_pp little_tour
      local_tour ozone par pda_pp rainbow_hcl rescale rgb sphere svalue
      svalue<- tooltip<- visible<-
    Consider adding
      importFrom("grDevices", "dev.control", "dev.cur", "dev.list",
                 "dev.new", "rgb")
      importFrom("graphics", "par")
    to your NAMESPACE file.
    ```

# toxboot

Version: 0.1.1

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘rmongodb’
    ```

# toxplot

Version: 0.1.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Quitting from lines 48-52 (Using_ToxPlot_Package_to_Analyze_in_vitro_Screening_Data.Rmd) 
    Error: processing vignette 'Using_ToxPlot_Package_to_Analyze_in_vitro_Screening_Data.Rmd' failed with diagnostics:
    'roxygen2' >= 5.0.0 must be installed for this functionality.
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tidyr’
      All declared Imports should be used.
    ```

# TPP

Version: 3.4.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.3Mb
      sub-directories of 1Mb or more:
        data           1.9Mb
        example_data   8.0Mb
        test_data      1.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
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

# trackeR

Version: 0.0.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘colorspace’
      All declared Imports should be used.
    ```

# trapezoid

Version: 2.0-0

## In both

*   checking R code for possible problems ... NOTE
    ```
    rtrapezoid: no visible global function definition for ‘runif’
    Undefined global functions or variables:
      runif
    Consider adding
      importFrom("stats", "runif")
    to your NAMESPACE file.
    ```

# treeclim

Version: 2.0.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        libs   5.0Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘dplR’
    ```

# treeDA

Version: 0.0.2

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘treeDA-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_coefficients
    > ### Title: Plot the discriminating axes from treeda
    > ### Aliases: plot_coefficients
    > 
    > ### ** Examples
    > 
    > data(treeda_example)
    > out.treeda = treeda(response = treeda_example$response,
    +     predictors = treeda_example$predictors,
    +     tree = treeda_example$tree,
    +     p = 1)
    > plot_coefficients(out.treeda)
    Error in .C(ape:::node_depth_edgelength, PACKAGE = "ape", as.integer(Ntip),  : 
      Incorrect number of arguments (7), expecting 5 for 'node_depth_edgelength'
    Calls: plot_coefficients ... plot_tree -> tree_layout -> ape_node_depth_edge_length -> .C
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    Quitting from lines 190-191 (treeda-vignette.Rmd) 
    Error: processing vignette 'treeda-vignette.Rmd' failed with diagnostics:
    Incorrect number of arguments (7), expecting 5 for 'node_depth_edgelength'
    Execution halted
    ```

# treespace

Version: 1.0.0

## In both

*   checking whether package ‘treespace’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: namespace ‘DBI’ is not available and has been replaced
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/treespace/new/treespace.Rcheck/00install.out’ for details.
    ```

# TSCAN

Version: 1.14.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    ...
      ‘lm’
    exprmclust: no visible global function definition for ‘dist’
    plotmclust: no visible binding for global variable ‘pca_dim_1’
    plotmclust: no visible binding for global variable ‘pca_dim_2’
    plotmclust: no visible binding for global variable ‘sample_name’
    preprocess: no visible binding for global variable ‘sd’
    preprocess: no visible global function definition for ‘hclust’
    preprocess: no visible global function definition for ‘dist’
    preprocess: no visible global function definition for ‘cutree’
    preprocess: no visible global function definition for ‘aggregate’
    singlegeneplot: no visible global function definition for
      ‘fitted.values’
    singlegeneplot: no visible binding for global variable ‘predict’
    Undefined global functions or variables:
      aggregate cutree dist fitted.values hclust lm p.adjust pca_dim_1
      pca_dim_2 pchisq prcomp predict sample_name sd
    Consider adding
      importFrom("stats", "aggregate", "cutree", "dist", "fitted.values",
                 "hclust", "lm", "p.adjust", "pchisq", "prcomp", "predict",
                 "sd")
    to your NAMESPACE file.
    ```

# TSMining

Version: 1.0

## In both

*   checking R code for possible problems ... NOTE
    ```
    Func.SAX: no visible global function definition for ‘sd’
    Func.SAX: no visible global function definition for ‘qnorm’
    Func.matrix: no visible global function definition for ‘qnorm’
    Func.motif: no visible global function definition for ‘sd’
    Func.motif: no visible global function definition for ‘na.omit’
    Func.motif: no visible global function definition for ‘combn’
    Func.motif.multivariate: no visible global function definition for
      ‘txtProgressBar’
    Func.motif.multivariate: no visible global function definition for
      ‘setTxtProgressBar’
    Undefined global functions or variables:
      combn na.omit qnorm sd setTxtProgressBar txtProgressBar
    Consider adding
      importFrom("stats", "na.omit", "qnorm", "sd")
      importFrom("utils", "combn", "setTxtProgressBar", "txtProgressBar")
    to your NAMESPACE file.
    ```

# TSRchitect

Version: 1.2.0

## In both

*   checking top-level files ... NOTE
    ```
    File
      inst/LICENSE
    will install at top-level and is not mentioned in the DESCRIPTION file.
    ```

# TSS.RESTREND

Version: 0.1.02

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘gimms’
    ```

# tufterhandout

Version: 1.2.1

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

# TVTB

Version: 1.2.0

## In both

*   checking examples ... ERROR
    ```
    ...
    > # Pre-process variants
    > vcf <- VariantAnnotation::readVcf(
    +     vcfFile, param = tparam, colData = phenotypes)
    > vcf <- VariantAnnotation::expand(vcf, row.names = TRUE)
    > vcf <- addFrequencies(vcf, "super_pop")
    > 
    > # Example usage ----
    > 
    > if (requireNamespace("EnsDb.Hsapiens.v75")){
    +     plotInfo(
    +         vcf, "MAF",
    +         range(GenomicRanges::granges(vcf)),
    +         EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75,
    +         "super_pop"
    +     )
    + }
    Loading required namespace: EnsDb.Hsapiens.v75
    Error in validObject(.Object) : 
      invalid class "AnnotationFilterList" object: superclass "vectorORfactor" not defined in the environment of the object's class
    Calls: plotInfo ... .AnnotationFilterList -> new -> initialize -> initialize -> validObject
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        rs1426654 rs150379789 rs570906312 rs538198029 rs553496066 rs574775672 
             TRUE        TRUE       FALSE       FALSE        TRUE       FALSE 
      rs140666229 rs556950130 rs575303689 rs147513140 rs187525777 rs192454382 
             TRUE       FALSE        TRUE        TRUE       FALSE       FALSE 
      rs184818838 rs566886499 rs199924625 rs555872528 rs531820822 rs201353600 
            FALSE       FALSE       FALSE       FALSE       FALSE       FALSE 
      rs550201688 rs565338261 rs201239799 rs200461129 rs146726548 
            FALSE       FALSE       FALSE       FALSE        TRUE 
      testthat results ================================================================
      OK: 229 SKIPPED: 0 FAILED: 2
      1. Error: all signatures work to completion (@test_plotInfo-methods.R#25) 
      2. Error: invalid metric/phenotype combination is detected (@test_plotInfo-methods.R#50) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Overwriting INFO keys in data:
    - REF
    - HET
    - ALT
    - AAF
    - MAF
    Overwriting INFO keys in header:
    - REF
    - HET
    - ALT
    - AAF
    - MAF
    Quitting from lines 570-577 (Introduction.Rmd) 
    Error: processing vignette 'Introduction.Rmd' failed with diagnostics:
    invalid class "AnnotationFilterList" object: superclass "vectorORfactor" not defined in the environment of the object's class
    Execution halted
    ```

# twoddpcr

Version: 1.0.6

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'twoddpcr.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# ukbtools

Version: 0.9.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 5 marked UTF-8 strings
    ```

# ukgasapi

Version: 0.13

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ukgasapi-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: dataItemExplorer
    > ### Title: Data Item Explorer API
    > ### Aliases: dataItemExplorer
    > 
    > ### ** Examples
    > 
    > # Specify the data item(s) to enquire from API
    > dataitems <- c('Storage Injection, Actual',
    +                'Storage Withdrawal, Actual')
    > 
    > # Initialise API (requires internet connection for this step)
    > response <- dataItemExplorer(dataitems,
    +                              fromdate = '2013-10-01',
    +                              todate='2015-09-30')
    ```

# unvotes

Version: 0.2.0

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4494 marked UTF-8 strings
    ```

# UpSetR

Version: 1.3.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        doc   7.9Mb
    ```

# usmap

Version: 0.2.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.0Mb
      sub-directories of 1Mb or more:
        extdata   6.2Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘ggthemes’
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# uSORT

Version: 1.2.0

## In both

*   checking whether package ‘uSORT’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/uSORT/new/uSORT.Rcheck/00install.out’ for details.
    ```

# valr

Version: 0.3.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 15.5Mb
      sub-directories of 1Mb or more:
        libs  14.1Mb
    ```

# vanddraabe

Version: 1.0.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("vanddraabe")
      Loading required package: vanddraabe
      1. Failure: FileTimeStamp returns the correct formatted date and time (@test-utilities.R#87) 
      FileTimeStamp(current.time) not equal to "may042016_1234".
      1/1 mismatches
      x[1]: "mai042016_1234"
      y[1]: "may042016_1234"
      
      
      testthat results ================================================================
      OK: 64 SKIPPED: 0 FAILED: 1
      1. Failure: FileTimeStamp returns the correct formatted date and time (@test-utilities.R#87) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# variancePartition

Version: 1.6.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        anyMissing, rowMedians
    
    
    Attaching package: 'DelayedArray'
    
    The following objects are masked from 'package:matrixStats':
    
        colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
    
    The following object is masked from 'package:base':
    
        apply
    
    converting counts to integer mode
    Note: importing `abundance.h5` is typically faster than `abundance.tsv`
    reading in files with read_tsv
    1 Quitting from lines 529-565 (variancePartition.Rnw) 
    Error: processing vignette 'variancePartition.Rnw' failed with diagnostics:
    reading kallisto results from hdf5 files requires Bioconductor package `rhdf5`
    Execution halted
    ```

# VariantAnnotation

Version: 1.22.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘snpStats’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    Unexported objects imported by ':::' calls:
      ‘BiocGenerics:::replaceSlots’ ‘BiocGenerics:::testPackage’
      ‘Rsamtools:::.RsamtoolsFile’ ‘Rsamtools:::.RsamtoolsFileList’
      ‘Rsamtools:::.io_check_exists’ ‘S4Vectors:::expandByColumnSet’
      ‘S4Vectors:::labeledLine’ ‘S4Vectors:::recycleVector’
      ‘S4Vectors:::selectSome’
      ‘SummarizedExperiment:::.SummarizedExperiment.charbound’
      ‘SummarizedExperiment:::.cbind.DataFrame’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking R code for possible problems ... NOTE
    ```
    .predictCodingGRangesList: no visible binding for global variable
      ‘GENETIC_CODE’
    VRangesForMatching: no visible binding for global variable ‘REF’
    VRangesForMatching: no visible binding for global variable ‘ALT’
    probabilityToSnpMatrix: no visible global function definition for
      ‘post2g’
    import,VcfFile-ANY-ANY: no visible global function definition for
      ‘checkArgFormat’
    Undefined global functions or variables:
      ALT GENETIC_CODE REF checkArgFormat post2g
    ```

*   checking compiled code ... NOTE
    ```
    File ‘VariantAnnotation/libs/VariantAnnotation.so’:
      Found non-API calls to R: ‘R_GetConnection’, ‘R_WriteConnection’
    
    Compiled code should not call non-API entry points in R.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# vcdExtra

Version: 0.7-1

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘coin’, ‘alr3’, ‘Hmisc’
    ```

# vcfR

Version: 1.5.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.3Mb
      sub-directories of 1Mb or more:
        doc    3.0Mb
        libs   7.5Mb
    ```

# vdg

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
      Running 'texi2dvi' on 'vdg.tex' failed.
    LaTeX errors:
    ! LaTeX Error: File `algorithm.sty' not found.
    
    Type X to quit or <RETURN> to proceed,
    or enter new name. (Default extension: sty)
    
    ! Emergency stop.
    <read *> 
             
    l.75 \usepackage
                    {algorithmic}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    Calls: buildVignettes -> texi2pdf -> texi2dvi
    Execution halted
    ```

# vdmR

Version: 0.2.4

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘vdmR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: vhist
    > ### Title: Generate histogram with interactive functions
    > ### Aliases: vhist
    > 
    > ### ** Examples
    > 
    > data(vsfuk2012)
    > vhist(FertilityRate, vsfuk2012, "hist1", "vsfuk2012", fill=Type)
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    Error in histparam$xmax : $ operator is invalid for atomic vectors
    Calls: vhist -> unique
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/run-all.R’ failed.
    Last 13 lines of output:
      with 72 features
      It has 3 fields
      Integer64 fields read as strings:  CityCode 
      ..
      Failed -------------------------------------------------------------------------
      1. Error: check vhist (@test-vdmR.R#7) -----------------------------------------
      $ operator is invalid for atomic vectors
      1: vhist(Sepal.Length, iris, "hist01", "iris", fill = Species) at /home/muelleki/git/R/ggplot2/revdep/checks/vdmR/new/vdmR.Rcheck/vdmR/tests/test-vdmR.R:7
      2: unique(histparam$xmax)
      
      DONE ===========================================================================
      Error: Test failures
      In addition: Warning message:
      Placing tests in `inst/tests/` is deprecated. Please use `tests/testthat/` instead 
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    Quitting from lines 40-42 (vdmR-vignette.Rnw) 
    Error: processing vignette 'vdmR-vignette.Rnw' failed with diagnostics:
    $ operator is invalid for atomic vectors
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘maptools’ ‘rgeos’
      All declared Imports should be used.
    ```

# VetResearchLMM

Version: 0.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘nlme’
      All declared Imports should be used.
    ```

# viridis

Version: 0.4.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
        RGB
    
    The following object is masked from 'package:ggplot2':
    
        calc
    
    Loading required package: lattice
    Loading required package: latticeExtra
    Loading required package: RColorBrewer
    
    Attaching package: 'latticeExtra'
    
    The following object is masked from 'package:ggplot2':
    
        layer
    
    Quitting from lines 204-213 (intro-to-viridis.Rmd) 
    Error: processing vignette 'intro-to-viridis.Rmd' failed with diagnostics:
    Cannot create RasterLayer object from this file; perhaps you need to install rgdal first
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘stats’
      All declared Imports should be used.
    ```

# vmsbase

Version: 2.1.3

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘gWidgetsRGtk2’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# voxel

Version: 1.3.2

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > ### Name: plotGAMM
    > ### Title: GAMM plotting using ggplot2
    > ### Aliases: plotGAMM
    > 
    > ### ** Examples
    > 
    > 
    > set.seed(1)
    > data <- data.frame(x = (seq(.25,25, .25) +rnorm(100)), group = rep(1:2, 5), z=rnorm(100),
    +               index.rnorm = rep(rnorm(50, sd = 50), 2), index.var = rep(1:50, 2))
    > data$y <- (data$x)*data$group*10 + rnorm(100, sd = 700) + data$index.rnorm + data$z
    > data$group <- ordered(data$group)
    > gamm <- gamm4::gamm4(y ~ + s(x) + s(x, by=group) + z + group, data=data, random = ~ (1|index.var))
    > plot <- plotGAMM(gammFit <- gamm, smooth.cov <- "x", groupCovs = "group", 
    +                     plotCI <- T, rawOrFitted = "raw", grouping = "index.var")
    [1] "Working with a GAMM4 object"
    [1] "orderedAsFactor functionality temporally disabled"
    > plot
    Error: Column `x` must be a 1d atomic vector or a list
    Execution halted
    ```

# vqtl

Version: 1.2.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘iterators’ ‘knitr’ ‘testthat’
      All declared Imports should be used.
    ```

# vsn

Version: 3.44.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hexbin’
      All declared Imports should be used.
    ```

*   checking R code for possible problems ... NOTE
    ```
    meanSdPlot,matrix: no visible binding for global variable ‘y’
    Undefined global functions or variables:
      y
    ```

# walker

Version: 0.2.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 46.9Mb
      sub-directories of 1Mb or more:
        libs  46.2Mb
    ```

# walkr

Version: 0.3.4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        libs   5.0Mb
    ```

# walrus

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘WRS2’ ‘ggplot2’
      All declared Imports should be used.
    ```

# warbleR

Version: 1.1.9

## In both

*   checking package dependencies ... ERROR
    ```
    Packages required but not available: ‘seewave’ ‘fftw’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# Wats

Version: 0.10.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.3Mb
      sub-directories of 1Mb or more:
        doc  12.1Mb
    ```

# wavClusteR

Version: 2.10.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Error: processing vignette 'wavCluster_vignette.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘doMC’
    ```

*   checking dependencies in R code ... NOTE
    ```
    'library' or 'require' call to ‘doMC’ in package code.
      Please use :: or requireNamespace() instead.
      See section 'Suggested packages' in the 'Writing R Extensions' manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    plotSubstitutions: no visible global function definition for ‘barplot’
    processChunk: no visible global function definition for ‘extractAt’
    processMD: no visible global function definition for ‘registerDoMC’
    readSortedBam: no visible global function definition for ‘scanBamFlag’
    readSortedBam : <anonymous>: no visible binding for global variable
      ‘rname’
    readSortedBam : <anonymous>: no visible binding for global variable
      ‘qwidth’
    Undefined global functions or variables:
      Compartment DNAString DNAStringSet Percentage abline axis barplot
      dbinom extractAt grid hist legend lines pairs panel.smooth par
      polygon qwidth rect registerDoMC rname scanBamFlag seqlevels
      setTxtProgressBar strwidth text txtProgressBar write.table
    Consider adding
      importFrom("graphics", "abline", "axis", "barplot", "grid", "hist",
                 "legend", "lines", "pairs", "panel.smooth", "par",
                 "polygon", "rect", "strwidth", "text")
      importFrom("stats", "dbinom")
      importFrom("utils", "setTxtProgressBar", "txtProgressBar",
                 "write.table")
    to your NAMESPACE file.
    ```

# wbstats

Version: 0.1.1

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1371 marked UTF-8 strings
    ```

# wesanderson

Version: 0.3.2

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
    Malformed Description field: should contain one or more complete sentences.
    ```

*   checking R code for possible problems ... NOTE
    ```
    print.palette: no visible global function definition for ‘par’
    print.palette: no visible global function definition for ‘image’
    print.palette: no visible global function definition for ‘rect’
    print.palette: no visible global function definition for ‘rgb’
    print.palette: no visible global function definition for ‘text’
    wes_palette: no visible global function definition for
      ‘colorRampPalette’
    Undefined global functions or variables:
      colorRampPalette image par rect rgb text
    Consider adding
      importFrom("grDevices", "colorRampPalette", "rgb")
      importFrom("graphics", "image", "par", "rect", "text")
    to your NAMESPACE file.
    ```

# widyr

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# wiggleplotr

Version: 1.0.0

## In both

*   checking examples ... ERROR
    ```
    ...
    The following object is masked from 'package:base':
    
        expand.grid
    
    Loading required package: IRanges
    Loading required package: GenomeInfoDb
    Loading required package: GenomicFeatures
    Loading required package: AnnotationDbi
    Loading required package: Biobase
    Welcome to Bioconductor
    
        Vignettes contain introductory material; view with
        'browseVignettes()'. To cite Bioconductor, see
        'citation("Biobase")', and for packages 'citation("pkgname")'.
    
    Loading required package: AnnotationFilter
    > plotTranscriptsFromEnsembldb(EnsDb.Hsapiens.v86, "NCOA7", transcript_ids = c("ENST00000438495", "ENST00000392477"))
    Error in validObject(.Object) : 
      invalid class "AnnotationFilterList" object: superclass "vectorORfactor" not defined in the environment of the object's class
    Calls: plotTranscriptsFromEnsembldb ... .AnnotationFilterList -> new -> initialize -> initialize -> validObject
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 148-152 (wiggleplotr.Rmd) 
    Error: processing vignette 'wiggleplotr.Rmd' failed with diagnostics:
    invalid class "AnnotationFilterList" object: superclass "vectorORfactor" not defined in the environment of the object's class
    Execution halted
    ```

# windfarmGA

Version: 1.1.1

## In both

*   checking whether package ‘windfarmGA’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/windfarmGA/new/windfarmGA.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RandomFields’
      All declared Imports should be used.
    ```

# wordbankr

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 27-31 (wordbankr.Rmd) 
    Error: processing vignette 'wordbankr.Rmd' failed with diagnostics:
    Condition message must be a string
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RMySQL’
      All declared Imports should be used.
    ```

# wrswoR

Version: 1.0-1

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Quitting from lines 622-635 (wrswoR.Rmd) 
    Error: processing vignette 'wrswoR.Rmd' failed with diagnostics:
    
    TeX was unable to calculate metrics for the following string
    or character:
    
    	77
    
    Common reasons for failure include:
      * The string contains a character which is special to LaTeX unless
        escaped properly, such as % or $.
      * The string makes use of LaTeX commands provided by a package and
        the tikzDevice was not told to load the package.
    
    The contents of the LaTeX log of the aborted run have been printed above,
    it may contain additional details as to why the metric calculation failed.
    Execution halted
    ```

# WRTDStidal

Version: 1.1.0

## Newly broken

*   checking whether package ‘WRTDStidal’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘forecast::autolayer’ by ‘ggplot2::autolayer’ when loading ‘WRTDStidal’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/WRTDStidal/new/WRTDStidal.Rcheck/00install.out’ for details.
    ```

# WVPlots

Version: 0.2.6

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mgcv’
      All declared Imports should be used.
    ```

# XBSeq

Version: 1.6.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
      
      The following object is masked from 'package:base':
      
          apply
      
          Welcome to 'XBSeq'.
      > 
      > test_check("XBSeq")
      estimating parameters using MLE for group one 
      estimating parameters using MLE for group two 
      Error: XBplot(XB, Samplenum = "Sample_54_WT") did not throw an error.
      testthat results ================================================================
      OK: 0 SKIPPED: 0 FAILED: 0
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Attaching package: 'DESeq'
    
    The following object is masked from 'package:XBSeq':
    
        fitInfo
    
    The following objects are masked from 'package:DESeq2':
    
        estimateSizeFactorsForMatrix, getVarianceStabilizedData,
        varianceStabilizingTransformation
    
    Warning in block_exec(params) :
      failed to tidy R code in chunk <unnamed-chunk-19>
    reason: Error in loadNamespace(name) : there is no package called 'formatR'
    
    Warning: Transformation introduced infinite values in continuous x-axis
    Warning: Removed 17 rows containing missing values (geom_point).
    Warning: Removed 2 rows containing missing values (geom_point).
    Error: processing vignette 'XBSeq.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

*   checking whether the namespace can be loaded with stated dependencies ... NOTE
    ```
    Warning: no function found corresponding to methods exports from ‘XBSeq’ for: ‘conditions’, ‘conditions<-’, ‘dispTable’
    
    A namespace must be able to be loaded with just the base namespace
    loaded: otherwise if the namespace gets loaded by a saved object, the
    session will be unable to start.
    
    Probably some imports need to be declared in the NAMESPACE file.
    ```

*   checking R code for possible problems ... NOTE
    ```
    ...
    estimateRealCount,XBSeqDataSet: no visible global function definition
      for ‘assay’
    estimateRealCount,XBSeqDataSet: no visible global function definition
      for ‘assay<-’
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘conditions’
    estimateSCV,XBSeqDataSet: no visible global function definition for
      ‘dispTable<-’
    Undefined global functions or variables:
      ..count.. DataFrame Gamma Group Sample SummarizedExperiment assay
      assay<- assays baseMean coefficients complete.cases conditions cor
      data ddelap dispTable dispTable<- dnbinom dpois formula glm
      log2FoldChange median optim p.adjust pbeta predict qbeta quantile
      rnbinom scvBiasCorrectionFits
    Consider adding
      importFrom("stats", "Gamma", "coefficients", "complete.cases", "cor",
                 "dnbinom", "dpois", "formula", "glm", "median", "optim",
                 "p.adjust", "pbeta", "predict", "qbeta", "quantile",
                 "rnbinom")
      importFrom("utils", "data")
    to your NAMESPACE file.
    ```

# xgboost

Version: 0.6-4

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.9Mb
      sub-directories of 1Mb or more:
        libs  11.3Mb
    ```

# XLConnect

Version: 0.2-13

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.7Mb
      sub-directories of 1Mb or more:
        java   9.7Mb
    ```

# xpose

Version: 0.4.0

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
    Error in 1:page_tot : argument of length 0
    Calls: <Anonymous> -> print.xpose_plot
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: grid.draw(plot)
      4: grid.draw.ggplot(plot)
      5: print(x)
      6: print.xpose_plot(x)
      
      testthat results ================================================================
      OK: 520 SKIPPED: 0 FAILED: 5
      1. Error: warnings and errors are properly returned (@test-print_xpose_plots.R#19) 
      2. Error: properly creates the xpdb when using the file argument (@test-xpose_data.R#39) 
      3. Error: properly creates the xpdb when using the runno argument (@test-xpose_data.R#52) 
      4. Error: common graphical device work properly (@test-xpose_save.R#45) 
      5. Error: template filenames and auto file extension work properly (@test-xpose_save.R#67) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Returning parameter estimates from $prob no.1, subprob no.0, method foce
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    Filtering data by EVID == 0
    Quitting from lines 76-87 (customize_plots.Rmd) 
    Error: processing vignette 'customize_plots.Rmd' failed with diagnostics:
    argument is of length zero
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘gridExtra’
      All declared Imports should be used.
    ```

# xtractomatic

Version: 3.3.2

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      The vignette engine knitr::rmarkdown is not available, because the rmarkdown package is not installed. Please install it.
    date	lon	lat	lowLon	higLon	lowLat	higLat
    4/23/2003	203.899	19.664	203.899	203.899	19.664	19.664
    4/24/2003	204.151	19.821	203.912597	204.389403	18.78051934	20.86148066
    4/30/2003	203.919	20.351	203.6793669	204.1586331	18.79728188	21.90471812
    5/1/2003	204.229	20.305	203.9943343	204.4636657	18.90440013	21.70559987
    Quitting from lines 1044-1046 (Usingxtractomatic.Rmd) 
    Error: processing vignette 'Usingxtractomatic.Rmd' failed with diagnostics:
    there is no package called 'webshot'
    Execution halted
    ```

# xxIRT

Version: 2.0.3

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘graphics’
      All declared Imports should be used.
    ```

# YAPSA

Version: 1.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    
    The following object is masked from 'package:base':
    
        strsplit
    
    Loading required package: rtracklayer
    Warning in posthoc.kruskal.nemenyi.test.default(x = sig_exposures_vector,  :
      Ties are present, p-values are not corrected.
    Warning in posthoc.kruskal.nemenyi.test.default(x = sig_exposures_vector,  :
      Ties are present, p-values are not corrected.
    Warning in posthoc.kruskal.nemenyi.test.default(x = sig_exposures_vector,  :
      Ties are present, p-values are not corrected.
    Warning in posthoc.kruskal.nemenyi.test.default(x = sig_exposures_vector,  :
      Ties are present, p-values are not corrected.
    Warning in posthoc.kruskal.nemenyi.test.default(x = sig_exposures_vector,  :
      Ties are present, p-values are not corrected.
    Warning in posthoc.kruskal.nemenyi.test.default(x = sig_exposures_vector,  :
      Ties are present, p-values are not corrected.
    Error: processing vignette 'YAPSA.Rmd' failed with diagnostics:
    path for html_dependency not found: 
    Execution halted
    ```

# zenplots

Version: 0.0-1

## In both

*   checking whether package ‘zenplots’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/zenplots/new/zenplots.Rcheck/00install.out’ for details.
    ```

# zFactor

Version: 0.1.7

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rootSolve’
      All declared Imports should be used.
    ```

# zonator

Version: 0.5.8

## Newly broken

*   checking whether package ‘zonator’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘ggplot2::calc’ by ‘raster::calc’ when loading ‘zonator’
    See ‘/home/muelleki/git/R/ggplot2/revdep/checks/zonator/new/zonator.Rcheck/00install.out’ for details.
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘zdat’
    ```

# zoo

Version: 1.8-0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/vignette-zoo-quickref.R’ failed.
    Last 13 lines of output:
      
      > library("tseries")
      > online <- FALSE ## if set to FALSE the local copy of
      >                 ## is used instead of get.hist.quote()
      > options(prompt = "R> ")
      R> Sys.setenv(TZ = "GMT")
      R> 
      R> 
      R> ###################################################
      R> ### chunk number 2: read.zoo
      R> ###################################################
      R> inrusd <- read.zoo(system.file("doc", "demo1.txt", package = "zoo"), sep = "|", format="%d %b %Y")
      Error in read.zoo(system.file("doc", "demo1.txt", package = "zoo"), sep = "|",  : 
        index has bad entries at data rows: 14 15 16 17 18 19 20
      Execution halted
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘stinepack’
    ```

# zooaRchGUI

Version: 1.0.2

## In both

*   R CMD check timed out
    

# zoocat

Version: 0.2.0

## In both

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'merge':
      ‘cbind.zoocat’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# ztype

Version: 0.1.0

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘ggplot2’ ‘lubridate’
      All declared Imports should be used.
    ```

