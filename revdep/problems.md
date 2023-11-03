# jfa

<details>

* Version: 0.7.0
* GitHub: https://github.com/koenderks/jfa
* Source code: https://github.com/cran/jfa
* Date/Publication: 2023-10-04 14:50:02 UTC
* Number of recursive dependencies: 187

Run `cloud_details(, "jfa")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘algorithm-auditing.Rmd’ using rmarkdown
    --- finished re-building ‘algorithm-auditing.Rmd’
    
    --- re-building ‘audit-sampling.Rmd’ using rmarkdown
    --- finished re-building ‘audit-sampling.Rmd’
    
    --- re-building ‘bayesian-sampling-workflow.Rmd’ using rmarkdown
    --- finished re-building ‘bayesian-sampling-workflow.Rmd’
    
    ...
    --- finished re-building ‘sample-selection.Rmd’
    
    --- re-building ‘sampling-workflow.Rmd’ using rmarkdown
    --- finished re-building ‘sampling-workflow.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘data-auditing.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 120.4Mb
      sub-directories of 1Mb or more:
        doc     3.2Mb
        libs  115.7Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘RcppParallel’ ‘rstantools’
      All declared Imports should be used.
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

# SCORPIUS

<details>

* Version: 1.0.9
* GitHub: https://github.com/rcannood/SCORPIUS
* Source code: https://github.com/cran/SCORPIUS
* Date/Publication: 2023-08-07 17:30:05 UTC
* Number of recursive dependencies: 197

Run `cloud_details(, "SCORPIUS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(SCORPIUS)
      > library(dplyr)
      
      Attaching package: 'dplyr'
      
      The following objects are masked from 'package:stats':
      
    ...
       4.   │   └─base::eval(mc, parent.frame())
       5.   └─mclust::mclustBIC(data = `<dbl[,91]>`, verbose = verbose)
       6.     ├─mclust::me(...)
       7.     │ └─base::eval(mc, parent.frame())
       8.     │   └─base::eval(mc, parent.frame())
       9.     └─mclust::meEEE(...)
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 182 ]
      Error: Test failures
      Execution halted
    ```

