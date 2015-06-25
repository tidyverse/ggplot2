This submission of ggplot2 fixes \donttest{} examples.

## Test environments
* OS X, R 3.1.3
* OS X, R-devel
* Ubuntu 14.04, R 3.1.3
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs in R 3.1.3. On R-devel with --as-cran, there was also this NOTE:

Found the following (possibly) invalid URLs:
  URL: http://fueleconomy.gov
    From: man/mpg.Rd
    Status: 404
    Message: Not Found

This URL is accessible through a web browser, though apparently not via R CMD check.


On R-devel for Windows x64, the tests hung and did not complete. We encountered some similar strange errors in our local testing, until we rebuilt and reinstalled packages from source. After doing that, the R CMD CHECK passed as above without problems.

## Downstream dependencies
We did not run checks on downstream dependencies, because there were no behaviour modifying changes to the code.

