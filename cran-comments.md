## Test environments
* OS X install: R 3.5.1
* win-builder: R-devel
* travis-ci: R 3.1, R 3.2, R 3.3, R 3.4, 3.5, R-devel

## R CMD check results

There were no ERRORs, WARNING or NOTEs

## Comments
This submission replaces the previous 3.2.0 submission of ggplot2. 

This release implements the minimal changes needed to work with the current and future grid unit specification.

We did not perform revdep checks because this is a minimal update with no side-effects. This is a subset of the previous submission that does not change ggplot2's external interface.
