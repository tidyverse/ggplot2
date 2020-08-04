# drugCombo

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/drugCombo
* Date/Publication: 2019-10-15 14:50:06 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "drugCombo")` for more info

</details>

## Newly broken

*   checking whether package ‘drugCombo’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/drugCombo/new/drugCombo.Rcheck/00install.out’ for details.
    ```

# frontiles

<details>

* Version: 1.2
* Source code: https://github.com/cran/frontiles
* Date/Publication: 2013-11-25 13:26:32
* Number of recursive dependencies: 78

Run `cloud_details(, "frontiles")` for more info

</details>

## Newly broken

*   checking whether package ‘frontiles’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/frontiles/new/frontiles.Rcheck/00install.out’ for details.
    ```

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

# GENEAsphere

<details>

* Version: 1.5.1
* Source code: https://github.com/cran/GENEAsphere
* Date/Publication: 2019-12-05 16:50:14 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "GENEAsphere")` for more info

</details>

## Newly broken

*   checking whether package ‘GENEAsphere’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/GENEAsphere/new/GENEAsphere.Rcheck/00install.out’ for details.
    ```

# ggdistribute

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/ggdistribute
* URL: https://github.com/iamamutt/ggdistribute
* Date/Publication: 2018-11-15 19:10:04 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "ggdistribute")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggdistribute-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: label_plot
    > ### Title: Add labels to existing plot
    > ### Aliases: label_plot
    > 
    > ### ** Examples
    > 
    > example_plot()
    Warning: `as.tibble()` is deprecated as of tibble 2.0.0.
    Please use `as_tibble()` instead.
    The signature and semantics have changed, see `?as_tibble`.
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    Error in rbindlist(l, use.names, fill, idcol) : 
      Class attribute on column 2 of item 2 does not match with column 2 of item 1.
    Calls: <Anonymous> ... eval -> eval -> rbind -> rbind -> <Anonymous> -> rbindlist
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘dplyr’
      All declared Imports should be used.
    ```

# gMOIP

<details>

* Version: 1.4.3
* Source code: https://github.com/cran/gMOIP
* URL: https://github.com/relund/gMOIP/
* BugReports: https://github.com/relund/gMOIP/issues
* Date/Publication: 2020-02-20 15:10:02 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "gMOIP")` for more info

</details>

## Newly broken

*   checking whether package ‘gMOIP’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/gMOIP/new/gMOIP.Rcheck/00install.out’ for details.
    ```

# helda

<details>

* Version: 1.1.2
* Source code: https://github.com/cran/helda
* URL: https://www.github.com/Redcart/helda
* BugReports: https://github.com/Redcart/helda/issues
* Date/Publication: 2020-06-07 11:20:03 UTC
* Number of recursive dependencies: 103

Run `cloud_details(, "helda")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 1. Failure: lift curve for titanic data set (@test-lift_curve.R#18)  ────────
      result$layers not equivalent to lift_curve_test$layers.
      Component 3: Component 3: Component 2: target, current do not match when deparsed
      
      ── 2. Failure: lift effect for titanic data set (@test-lift_effect.R#18)  ──────
      result$layers not equivalent to lift_effect_test$layers.
      Component 2: Component 3: Component 2: target, current do not match when deparsed
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 5 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 2 ]
      1. Failure: lift curve for titanic data set (@test-lift_curve.R#18) 
      2. Failure: lift effect for titanic data set (@test-lift_effect.R#18) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1 marked UTF-8 string
    ```

# lemon

<details>

* Version: 0.4.4
* Source code: https://github.com/cran/lemon
* URL: https://github.com/stefanedwards/lemon
* BugReports: https://github.com/stefanedwards/lemon/issues
* Date/Publication: 2020-04-03 16:00:18 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "lemon")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    The following objects are masked from ‘package:lemon’:
    
        CoordCartesian, element_render
    
    > dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
    > (d <- ggplot(dsamp, aes(carat, price)) +
    +  geom_point(aes(colour = clarity)))
    > 
    > reposition_legend(d + theme(legend.position='bottom'), 'bottom right')
    > 
    > # To change the orientation of the legend, use theme's descriptors.
    > reposition_legend(d + theme(legend.position='bottom'), 'top left')
    > 
    > # Use odd specifications, here offset the legend with half its height from the bottom.
    > reposition_legend(d + theme(legend.position='bottom'), x=0.3, y=0, just=c(0, -0.5))
    > 
    > # For using with facets:
    > reposition_legend(d + facet_grid(.~cut), 'top left', panel = 'panel-3-1')
    Error in reposition_legend(d + facet_grid(. ~ cut), "top left", panel = "panel-3-1") : 
      Could not find panel named `panel-3-1`.
    Execution halted
    ```

# metagen

<details>

* Version: 1.0
* Source code: https://github.com/cran/metagen
* URL: http://00tau.github.io/metagen/
* Date/Publication: 2014-05-27 15:43:54
* Number of recursive dependencies: 68

Run `cloud_details(, "metagen")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > bcg   <- bcgVaccineData()
    > bcg_y <- bcg$logrisk
    > bcg_d <- bcg$sdiv
    > bcg_s <- bcg$size
    > bcg_x <- cbind(1,bcg$x)
    > p <- plotStudyQfuncPfunc(y=bcg_y, d=bcg_d, x=bcg_x, n=500)
    > p[1] # plot of the q-function
    $plotQ
    Error: Aesthetics must be either length 1 or the same as the data (1): x and y
    Backtrace:
        █
     1. ├─(function (x, ...) ...
     2. └─ggplot2:::print.ggplot(x)
     3.   ├─ggplot2::ggplot_build(x)
     4.   └─ggplot2:::ggplot_build.ggplot(x)
     5.     └─ggplot2:::by_layer(function(l, d) l$compute_aesthetics(d, plot))
     6.       └─ggplot2:::f(l = layers[[i]], d = data[[i]])
     7.         └─l$compute_aesthetics(d, plot)
     8.           └─ggplot2:::f(..., self = self)
     9.             └─ggplot2:::check_aesthetics(evaled, n)
    Execution halted
    ```

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

# NeatMap

<details>

* Version: 0.3.6.2
* Source code: https://github.com/cran/NeatMap
* Date/Publication: 2014-07-29 08:05:12
* Number of recursive dependencies: 73

Run `cloud_details(, "NeatMap")` for more info

</details>

## Newly broken

*   checking whether package ‘NeatMap’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/NeatMap/new/NeatMap.Rcheck/00install.out’ for details.
    ```

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
    write to stdout/stderr instead of to the console, nor use Fortran I/O
    nor system RNGs.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
    ```

# PPQplan

<details>

* Version: 1.0.0
* Source code: https://github.com/cran/PPQplan
* URL: https://allenzhuaz.github.io/PPQplan/, https://github.com/allenzhuaz/PPQplan
* BugReports: https://github.com/allenzhuaz/PPQplan/issues
* Date/Publication: 2019-09-03 16:20:04 UTC
* Number of recursive dependencies: 89

Run `cloud_details(, "PPQplan")` for more info

</details>

## Newly broken

*   checking whether package ‘PPQplan’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/PPQplan/new/PPQplan.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 12.0Mb
      sub-directories of 1Mb or more:
        doc  11.8Mb
    ```

# predict3d

<details>

* Version: 0.1.3.3
* Source code: https://github.com/cran/predict3d
* URL: https://github.com/cardiomoon/predict3d
* BugReports: https://github.com/cardiomoon/predict3d/issues
* Date/Publication: 2019-09-03 13:00:02 UTC
* Number of recursive dependencies: 109

Run `cloud_details(, "predict3d")` for more info

</details>

## Newly broken

*   checking whether package ‘predict3d’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/predict3d/new/predict3d.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘TH.data’ ‘moonBook’
      All declared Imports should be used.
    ```

# ratPASTA

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/ratPASTA
* URL: https://github.com/ikodvanj/ratPASTA
* BugReports: https://github.com/ikodvanj/ratPASTA/issues
* Date/Publication: 2020-06-04 11:20:10 UTC
* Number of recursive dependencies: 117

Run `cloud_details(, "ratPASTA")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Component "xmin": Attributes: < Lengths: 1, 0 >
      Component "xmin": Attributes: < names for target but not for current >
      Component "xmin": Attributes: < current is not list-like >
      ...
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 18 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 5 ]
      1. Failure: Testing output of latency plot (@test-latency.R#20) 
      2. Failure: Testing output of startle plot (@test-startleplot.r#26) 
      3. Failure: Testing output of startle plot (@test-startleplot.r#40) 
      4. Failure: Testing output of startle plot (@test-startleplot.r#54) 
      5. Failure: Testing output of startle plot (@test-startleplot.r#82) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘hms’
      All declared Imports should be used.
    ```

# vmsbase

<details>

* Version: 2.2.1
* Source code: https://github.com/cran/vmsbase
* URL: https://github.com/d-lorenz/R-vmsbase
* Date/Publication: 2020-05-15 08:30:02 UTC
* Number of recursive dependencies: 114

Run `cloud_details(, "vmsbase")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    
    (R:15902): Gtk-WARNING **: 09:12:07.881: gtk_disable_setlocale() must be called before gtk_init()
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking replacement functions ... WARNING
    ```
    
    (R:15977): Gtk-WARNING **: 09:12:14.441: gtk_disable_setlocale() must be called before gtk_init()
    The argument of a replacement function which corresponds to the right
    hand side must be named ‘value’.
    ```

*   checking for missing documentation entries ... WARNING
    ```
    
    (R:16764): Gtk-WARNING **: 09:12:51.194: gtk_disable_setlocale() must be called before gtk_init()
    All user-level objects in a package should have documentation entries.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking for code/documentation mismatches ... WARNING
    ```
    
    (R:16839): Gtk-WARNING **: 09:12:55.395: gtk_disable_setlocale() must be called before gtk_init()
    
    (R:16914): Gtk-WARNING **: 09:12:59.497: gtk_disable_setlocale() must be called before gtk_init()
    
    (R:16989): Gtk-WARNING **: 09:13:03.694: gtk_disable_setlocale() must be called before gtk_init()
    ```

*   checking dependencies in R code ... NOTE
    ```
    
    (R:15827): Gtk-WARNING **: 09:12:02.946: gtk_disable_setlocale() must be called before gtk_init()
    ```

*   checking foreign function calls ... NOTE
    ```
    
    (R:16052): Gtk-WARNING **: 09:12:18.457: gtk_disable_setlocale() must be called before gtk_init()
    See chapter ‘System and foreign language interfaces’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    
    (R:16407): Gtk-WARNING **: 09:12:25.811: gtk_disable_setlocale() must be called before gtk_init()
    ```

*   checking Rd \usage sections ... NOTE
    ```
    
    (R:17136): Gtk-WARNING **: 09:13:08.358: gtk_disable_setlocale() must be called before gtk_init()
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

