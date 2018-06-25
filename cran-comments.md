## Test environments
* OS X install: R 3.4
* win-builder: R-devel
* travis-ci: R 3.1, R 3.2, R 3.3, R 3.4, R-devel

## R CMD check results

There were no ERRORs, WARNING or NOTEs

## revdepcheck results

We checked 2152 reverse dependencies (1774 from CRAN + 378 from BioConductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 81 new problems
 * We failed to check 32 packages

This is an unfortuantely high number of failures. The ggplot2 development team carefully analysed all revdep failures to confirm that they were due to deliberate API changes - unfortunately many downstream packages relied on internal, undocumented, features of ggplot2 objects, which changed in this version. We advised authors about problems on May 21 and again on June 18. I'm happy to run the checks again from my end and re-advise if that would be helpful to you.

### New problems
(This reports the first line of each new failure)

* bayesAB
  checking tests ...

* bayesplot
  checking tests ...

* benchmark
  checking whether package ‘benchmark’ can be installed ... WARNING

* benchr
  checking tests ...

* BioPET
  checking examples ... ERROR

* BrailleR
  checking examples ... ERROR
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* cellWise
  checking re-building of vignette outputs ... WARNING

* choroplethr
  checking examples ... ERROR

* civis
  checking tests ...

* clustree
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* colorplaner
  checking examples ... ERROR
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* cosinor2
  checking examples ... ERROR

* countytimezones
  checking re-building of vignette outputs ... WARNING

* cowplot
  checking tests ...

* cricketr
  checking whether package ‘cricketr’ can be installed ... WARNING

* DendroSync
  checking examples ... ERROR

* dextergui
  checking whether package ‘dextergui’ can be installed ... WARNING

* dgo
  checking tests ...

* dMod
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* docxtools
  checking tests ...

* emojifont
  checking re-building of vignette outputs ... WARNING

* extracat
  checking examples ... ERROR

* fastR2
  checking examples ... ERROR
  checking whether package ‘fastR2’ can be installed ... WARNING

* fiftystater
  checking re-building of vignette outputs ... WARNING

* forecast
  checking examples ... ERROR

* ForecastComb
  checking whether package ‘ForecastComb’ can be installed ... WARNING

* foreSIGHT
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* fpp2
  checking whether package ‘fpp2’ can be installed ... WARNING

* GeomComb
  checking whether package ‘GeomComb’ can be installed ... WARNING

* ggdistribute
  checking examples ... ERROR

* ggedit
  checking examples ... ERROR

* ggFacetSample
  checking examples ... ERROR

* ggforce
  checking for code/documentation mismatches ... WARNING

* ggformula
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* gggenes
  checking examples ... ERROR
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* gghighlight
  checking examples ... ERROR
  checking tests ...

* ggiraphExtra
  checking examples ... ERROR
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* ggmap
  checking examples ... ERROR

* ggmosaic
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING
  checking Rd \usage sections ... NOTE

* ggpol
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* ggpubr
  checking examples ... ERROR

* ggpval
  checking examples ... ERROR
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* ggraph
  checking examples ... ERROR
  checking S3 generic/method consistency ... WARNING
  checking re-building of vignette outputs ... WARNING
  checking Rd \usage sections ... NOTE

* ggstance
  checking tests ...

* healthcareai
  checking tests ...

* heatmaply
  checking tests ...

* heatwaveR
  checking tests ...

* hyperSpec
  checking examples ... ERROR

* imputeTestbench
  checking whether package ‘imputeTestbench’ can be installed ... WARNING

* INDperform
  checking tests ...

* jcolors
  checking examples ... ERROR

* jtools
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* lavaSearch2
  checking whether package ‘lavaSearch2’ can be installed ... WARNING

* mafs
  checking whether package ‘mafs’ can be installed ... WARNING

* malariaAtlas
  checking tests ...

* mosaic
  checking examples ... ERROR

* mosaicModel
  checking re-building of vignette outputs ... WARNING

* oddsratio
  checking examples ... ERROR
  checking tests ...
  checking re-building of vignette outputs ... WARNING

* optiRum
  checking tests ...

* otvPlots
  checking tests ...

* phyloseqGraphTest
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* platetools
  checking examples ... ERROR

* plotly
  checking tests ...

* plotROC
  checking re-building of vignette outputs ... WARNING

* reghelper
  checking tests ...

* robustbase
  checking running R code from vignettes ...

* RStoolbox
  checking examples ... ERROR

* scatterpie
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* SimDesign
  checking re-building of vignette outputs ... WARNING

* sugrrants
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* svdvis
  checking examples ... ERROR
  checking re-building of vignette outputs ... WARNING

* synthpop
  checking examples ... ERROR

* toaster
  checking tests ...

* trackr
  checking examples ... ERROR
  checking tests ...

* ukgasapi
  checking examples ... ERROR

* vdmR
  checking examples ... ERROR

* voxel
  checking examples ... ERROR

* WRTDStidal
  checking whether package ‘WRTDStidal’ can be installed ... WARNING

* xkcd
  checking examples ... ERROR

* XLConnect
  checking tests ...

* xpose
  checking examples ... ERROR
  checking tests ...
  checking re-building of vignette outputs ... WARNING

### Failed to check

* BACCT                  (failed to install)
* bamdit                 (failed to install)
* BayesRS                (failed to install)
* BNSP                   (failed to install)
* bsam                   (failed to install)
* BTSPAS                 (failed to install)
* classify               (failed to install)
* crmPack                (failed to install)
* dynr                   (failed to install)
* ewoc                   (failed to install)
* fingerPro              (failed to install)
* glmmTMB                (check timed out)
* HTSSIP                 (check timed out)
* magick                 (failed to install)
* mglR                   (check timed out)
* morse                  (failed to install)
* mwaved                 (failed to install)
* pcaPA                  (failed to install)
* phase1RMD              (failed to install)
* phylosim               (check timed out)
* RcmdrPlugin.FuzzyClust (check timed out)
* rpanel                 (failed to install)
* rstanarm               (check timed out)
* rsvg                   (failed to install)
* seewave                (failed to install)
* SeqFeatR               (failed to install)
* sf                     (failed to install)
* sgmcmc                 (check timed out)
* simulator              (check timed out)
* TeachingDemos          (check timed out)
* tricolore              (failed to install)
* zooaRchGUI             (failed to install)
