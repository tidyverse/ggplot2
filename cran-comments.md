This is a resubmission after fixing a bug that affect mlr and SciencePo.

---------------------------------

This is a resubmission, post-review of \dontrun{} usage. Now \dontrun{} is only used in two places: an example that fails on some platforms, and an ggsave() examples which save files on disk.

---------------------------------

## Test environments
* OS X, R 3.2.3
* Ubuntu 14.04, R 3.2.3
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

## Reverse dependencies

* I ran `R CMD check` on 674 all reverse dependencies
  (summary at https://github.com/hadley/ggplot2/blob/master/revdep/).
  I checked with the dev versions of ggplot2, scales, and gtable, in an
  effort to maximise problems found.
  
* Maintainers with NOTEs, WARNINGs, or ERRORS were notified on Feb 10, 
  and again on Feb 25. I have switched to a new automated system for 
  maintainer notification, which ensures each maintainer gets a personalised
  email, and hopefully is more likely to take action.
  
* I've also tried to give a more detailed breakdown of ERRORs/WARNINGs
  (but not NOTEs below). Please let me know if there's anything else I 
  can do to make this more helpful.
  
As far as I can tell, there are 3 failures related to changes to ggplot2:

* ggtern: checking examples ... ERROR
  Caused by change in API. Author is aware but hasn't fixed.

* plotly: checking tests ... ERROR
  Probably caused by changes to histogram API.

* precrec: checking tests ... ERROR
  Using a bad test for class inheritance + minor change in ggplot2.

There were a number of failures that don't appear to be related to changes in ggplot2.

* archivist: checking examples ... ERROR
  Looks like ggplot2 2.0.0 problem.

* BCEA: checking examples ... ERROR
  I failed to install R2jags.

* bcrm: checking examples ... ERROR
  Another JAGS problem

* dotwhisker: checking examples ... ERROR
  Needs to attach grid (ggplot2 2.0.0 problem)

* emil: checking examples ... ERROR
  Needs very latest version of nlme

* eyetrackingR: checking examples ... ERROR
  Needs suggested package (lme4) to run examples

* fuzzyforest: checking examples ... ERROR
  Needs GO.db

* ggthemes: checking tests ... ERROR
  Automated code style checking failure

* iNEXT: checking Rd cross-references ... WARNING
  ggplot2 2.0.0 problem: links to help topic that no longer exists

* metaheur: checking for missing documentation entries ... ERROR
  Needs very latest version of nlme

* mlr: checking examples ... ERROR
  Needs very latest version of nlme
  
* multitable: checking running R code from vignettes ... ERROR
  Needs lme4
  
* preproviz: checking tests ... ERROR
  Needs very latest version of nlme

* Rz: checking dependencies in R code ... WARNING
  Namespace issues

* sdmvspecies: checking for executable files ... WARNING
  Includes executable file

Additionally, I:

* Failed to install dependencies for: AFM, clusterfly, demi, ibmdbR, metaMix, 
  PKgraph, pmc, prcbench, SeqFeatR, SpaDES, specmine, toaster

* Failed to install: abd, alm, arqas, ARTool, BACA, bamdit, bdscale, bdvis, 
  benchmark, biomod2, brainGraph, brms, BTSPAS, capm, caret, caretEnsemble, 
  ChainLadder, classify, conformal, COPASutils, crmPack, extracat, FAOSTAT, 
  fheatmap, geneSLOPE, gettingtothebottom, gitter, granovaGG, HistDAWass, 
  hyperSpec, IntegratedJM, kobe, LANDD, localgauss, LOGIT, ltbayes, MergeGUI, 
  Methplot, micromap, MissingDataGUI, mizer, morse, MultiMeta, mwaved, ncappc, 
  NeuralNetTools, NMF, nparACT, npregfast, OriGen, partialAR, patPRO, Phxnlme, 
  playwith, PlotPrjNetworks, pomp, PPtreeViz, primerTree, quadrupen, 
  refund.shiny, RobustEM, RSA, RSDA, rstan, rstanarm, RStoolbox, rsvg, SCGLR, 
  seewave, sgd, simmr, sjPlot, snpEnrichment, sparkTable, spikeSlabGAM, 
  spoccutils, strvalidator, tadaatoolbox, tigerstats, tigris, treemap, 
  userfriendlyscience, vmsbase

I think the majority of these are because I'm not currently installing bioconductor packages. I hope to work on that in the future.
