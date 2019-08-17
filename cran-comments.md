This is a patch release fixing the errors caused by changes to sf, as well as
fixing a few regressions. As all changes are internal and non-breaking a reverse
dependency check has not been performed

-------

## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
    installed size is  6.2Mb
    sub-directories of 1Mb or more:
      doc   3.4Mb
