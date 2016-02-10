This is a resubmission, using the canonical link to the proto package.

---

## Test environments
* OS X, R 3.2.2
* Ubuntu 14.04, R 3.2.2
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. I see one NOTE:

* Found the following (possibly) invalid URLs: 
  URL: http://fueleconomy.gov 
  From: man/mpg.Rd
  Status: 404 Message: Not Found
  
  I think the problem is a poorly configured webserver: 
  `curl http://fueleconomy.gov` works, but `curl -I http://fueleconomy.gov`
  (which sends a HEAD request) does not.
  
  I've also updated my email address to hadley@rstudio.com

## Downstream dependencies

* Maintainers were notified on Feb 10.
