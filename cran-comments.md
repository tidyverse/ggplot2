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

This version of ggplot2 contains substantial changes (hence the bump in the major version number), and I've done my best to get the reverse dependencies updated. I started the release process over two months ago, and I have notified maintainers three times (and I'll notify them again today). 

Unfortunately there are still widespread check failures (mostly new WARNINGS). They are mostly caused by three changes:

* ggplot2 now imports and exports `unit()` and `arrow()` from grid, and 
  `alpha()` from scales. Many packages seem to have unused unqualified import 
  statements which now generates a warning on package install. (You could argue
  that this is a false positive since the conflicting names both point to the 
  same object.). This affects:
  
  alm, arqas, benchmark, blowtorch, clhs, conformal, Deducer, DTRlearn, dynsim, 
  extracat, FAOSTAT, fheatmap, FinCal, gettingtothebottom, GGally, gwmw, 
  HistDAWass, hyperspec, kobe, likert, lsbclust, marketeR, Methplot, micromap, 
  MissingDataGUI, mizer, MultiMeta, ncappc, nparACT, optiRum, patPRO, 
  PortfolioEffectHFT, PPtreeViz, PReMiuM, prevR, primerTree, qicharts, 
  quadrupen, RAM, RDS, RFmarkerDetector, rfPermute, RobustEM, rotations, RSDA, 
  SCGLR, sjPlot, soc.ca, sparkTable, spikeSlabGAM, spoccutils, sprm, statebins, 
  strvalidator, survMisc, tcR, TreatmentSelection, TriMatch, UpSetR, useful, 
  varian, waffle

* ggplot2 makes extensive use of ..., and previously little checking was done
  on the contents. Now ggplot2 is much stricter about ... which causes a number
  of warnings, where previously the function silently did the wrong thing.
  This afffects:
  
  AmplicoDuo, chemosensors, clifro, dpcR, granovaGG, GraphPCA, HistDAWass, 
  knitrBootstrap, LOGIT, lsbclust, mlr, multitable, oaxaca, pAnalysis, PASWR2, 
  patPRO, PDQutils, planar, RAM, reproducer, rex, rfordummies, sdmvspecies, 
  SensMixed, simmer, SixSigma, sjPlot, spcosa, ss3sim, starma, survMisc, tcR, 
  treecm, userfriendlyscience, vcdExtra, vdmR, wq, xkcd

* A few packages linked to the ggplot2 documentation topic, which didn't 
  actualy contain anything useful. I removed the file, so a number of packges
  now give warnings about a missing link. This affects:
  
  PopED, precrec, RcmdrPlugin.KMggplot2points, simPH, statebins

Finally, there were a grab bag of other failures:

* CosmoPhotoz: uses a newly deprecated function
* cowplot: uses a newly deprecated function
* directlabels: ?? think this is long standing (??)
* ggswissmaps (looks like it was relying on an internal API that's changed)
* ggmcmc (from GGally error)
* metricsgraphics (relies on movies data which has moved to ggplot2movies)
* OutbreakTools: downstream error from ggmap
* plot2groups: used internal StatHline which is no longer exported
* plotly: used internal StatVline which is no longer exported
* precintcon: ?? not ggplot2 related?
* RCell: used internal functions where API has changed
* robustlmm: ?? not ggplot2 related?
* SWMPr: downstream error from ggmap
* synthpop:  ?? not ggplot2 related?
* TreatmentSelection: used internal stat_hline which is now longer exported
