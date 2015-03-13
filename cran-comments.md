This submission of ggplot2 fixes \donttest{} examples.

## Test environments
* OS X, R 3.1.3
* OS X, R-devel
* Ubuntu 14.04, R 3.1.3
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. There was 1 NOTE:

* Possibly mis-spelled words in DESCRIPTION:
  ggplot (16:43)

  ggplot is, of course, spelled correctly.

On R-devel for Windows x64, the tests hung and did not complete. We encountered some similar strange errors in our local testing, until we rebuilt and reinstalled packages from source. After doing that, the R CMD CHECK passed as above without problems.

## Downstream dependencies
We did not run checks on downstream dependencies, because there were no behaviour modifying changes to the code.

