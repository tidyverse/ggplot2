This is a resubmission fixing two minor issues.

---

## Test environments
* OS X, R 3.3.1
* Ubuntu 14.04, R 3.3.1
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs. I see two NOTEs:

* Found the following (possibly) invalid URLs: 
  URL: http://fueleconomy.gov 
  From: man/mpg.Rd
  Status: 404 Message: Not Found
  
  I think the problem is a poorly configured webserver: 
  `curl http://fueleconomy.gov` works, but `curl -I http://fueleconomy.gov`
  (which sends a HEAD request) does not.

* checking DESCRIPTION meta-information ... NOTE
  Authors@R field gives persons with non-standard roles:
  RStudio [cph, fnd]: fnd
  
  I would rely like to capture the signficant funders in the authors list
  so that can be correctly acknowledged.

## Reverse dependencies

* I ran `R CMD check` on 954 all reverse dependencies
  (summary at https://github.com/hadley/ggplot2/blob/master/revdep/).

* Maintainers with NOTEs, WARNINGs, or ERRORS were notified on Oct 6,
  Oct 25, and again today.
  
* There are a lot of WARNINGS/ERRORs, but I've carefully reviewed them
  all and as far as I can tell they are either deliberate changes to
  the ggplot2 API, or unrelated problems. I'm sorry there are so many -
  I don't know how to better encourage package authors to submit
  updates. Every maintainer has been personally emailed at least twice
  before today.

### ggproto mistake

I unfortunately made an implementation error in ggproto (the OO system that ggplot2 uses) that inadvertently introduced a build-time dependency on ggplot2. I've fixed the problem and added a clear error message, but to fix a number of downstream failures will require a rebuild of the following packages against the new version of ggplot2:

* ggrepel (GeomTextRepel)
* ggmap (GeomRasterAnn) 
* ggpmisc (statpeaks) 
* ggbeeswarm (PositionQuasirandom)

This problem affects the following downstream packgaes:

* archivist: checking examples ... ERROR
* clustrd: checking examples ... ERROR
* clifro: checking re-building of vignette outputs ... WARNING
* corrr: checking examples ... ERROR
* factoextra: checking examples ... ERROR
* ggnetwork: checking examples ... ERROR
* ggpmisc: checking examples ... ERROR
* ggpubr: checking examples ... ERROR
* idm: checking examples ... ERROR
* OutbreakTools: checking examples ... ERROR
* photobiologyInOut: checking re-building of vignette outputs ... WARNING
* photobiologyLamps: checking re-building of vignette outputs ... WARNING
* photobiologyLEDs: checking re-building of vignette outputs ... WARNING
* photobiologyPlants: checking re-building of vignette outputs ... WARNING
* SensusR: checking examples ... ERROR
* sjPlot: checking examples ... ERROR
* soc.ca: checking examples ... ERROR
* trackeR: checking re-building of vignette outputs ... WARNING
* vipor: checking re-building of vignette outputs ... WARNING

### ggplot2 changes

Need dev version of plotly: the maintainer is planning on submitting ASAP:

* eechidna: checking examples ... ERROR
* heatmaply: checking examples ... ERROR
* MendelianRandomization: checking examples ... ERROR
* plotly: checking examples ... ERROR

Errors that were silent, but now break thanks to stricter checks:  

* biogram: checking re-building of vignette outputs ... WARNING
* BlandAltmanLeh: checking re-building of vignette outputs ... WARNING
* brms: checking tests ... ERROR
* CopulaDTA: checking re-building of vignette outputs ... WARNING
* ddpcr: checking re-building of vignette outputs ... WARNING
* fheatmap: checking examples ... ERROR
* gapfill: checking examples ... ERROR
* ggExtra: checking examples ... ERROR
* iNEXT: checking re-building of vignette outputs ... WARNING
* largeVis: checking re-building of vignette outputs ... WARNING
* MEGENA: checking re-building of vignette outputs ... WARNING
* obAnalytics: checking examples ... ERROR
* oddsratio: checking examples ... ERROR
* onlineCPD: checking examples ... ERROR
* PGRdup: checking re-building of vignette outputs ... WARNING
* quickpsy: checking examples ... ERROR
* sparkTable: checking examples ... ERROR
* surveillance: checking re-building of vignette outputs ... WARNING
* survMisc: checking examples ... ERROR
* UpSetR: checking re-building of vignette outputs ... WARNING

Some packages rely on internal object definitions, and haven't yet been updated for this version:

* geomnet: checking examples ... ERROR
* GGally: checking examples ... ERROR
* ggalt: checking examples ... ERROR
* hyperSpec: checking examples ... ERROR
* manhattanly: checking re-building of vignette outputs ... WARNING
* mtconnectR: checking tests ... ERROR
* quanteda: checking tests ... ERROR
* rstanarm: checking examples ... ERROR
* tabplot: checking examples ... ERROR
* vdmR: checking examples ... ERROR

Two use a newly deprecated argument:

* plotluck: checking tests ... ERROR
* robCompositions: checking re-building of vignette outputs ... WARNING

### Problems unrelated to ggplot2

Failed to installed suggested package used in examples or vignettes:
  
* bcrm: checking examples ... ERROR
* BCEA: checking examples ... ERROR
* coloc: checking re-building of vignette outputs ... WARNING
* emojifont: checking re-building of vignette outputs ... WARNING
* fuzzyforest: checking examples ... ERROR
* glycanr: checking examples ... ERROR
* GenCAT: checking examples ... ERROR
* ie2miscdata: checking re-building of vignette outputs ... WARNING
* MixSIAR: checking examples ... ERROR
* slim: checking re-building of vignette outputs ... WARNING
* survminer: checking examples ... ERROR
* tcgsaseq: checking examples ... ERROR
* TELP: checking examples ... ERROR

Dependency of Hmisc on bleeding-edge survival:

* choroplethr: checking re-building of vignette outputs ... WARNING
* Greg: checking examples ... ERROR
* hdnom: checking examples ... ERROR
* pander: checking re-building of vignette outputs ... WARNING
* simcausal: checking re-building of vignette outputs ... WARNING

Needs java:

* mlr: checking examples ... ERROR
* prcbench: checking examples ... ERROR
* rfordummies: checking examples ... ERROR
* userfriendlyscience: checking examples ... ERROR

Other/unknown:

* BioPET: checking examples ... ERROR
  Calling mean on non-numeric object

* ecb: checking tests ... ERROR
  Seems to be some badness in tests of remote server

* fitbitScraper: checking re-building of vignette outputs ... WARNING
  Looks related to RCurl

* ggenealogy: checking re-building of vignette outputs ... WARNING
  Looks like encoding related issues in vignette

* ggfortify: checking tests ... ERROR
  Don't know what's causing these failures

* ParamHelpers: checking tests ... ERROR
  Failing test appears unrelated to ggplot2

* PredictTestbench: checking re-building of vignette outputs ... WARNING
  Missing `pred` arugment

* xtractomatic: checking re-building of vignette outputs ... WARNING
  Failed to open connection

Existing R CMD check failures:

* proportion: checking sizes of PDF files under ‘inst/doc’ ... WARNING
* Rz: checking dependencies in R code ... WARNING
* simmer: checking compiled code ... WARNING

Failed to install dependencies for: aop, BACA, cate, clusterfly, demi, gitter, 
  gridDebug, Hmisc, HTSSIP, ibmdbR, IntClust, LANDD, LDheatmap, MetaIntegrator, 
  metaMix, multiDimBio, myTAI, NFP, PKgraph, PlasmaMutationDetector, pRF, PSCBS, 
  raptr, rms, RPPanalyzer, scmamp, SeqFeatR, snpEnrichment, specmine, starmie, 
  structSSI, TcGSA, toaster

Failed to install: BACCT, backShift, bamdit, brainGraph, bsam, BTSPAS, 
  classify, CollapsABEL, crmPack, Crossover, Deducer, DiversityOccupancy, 
  DynNom, dynr, Fgmutils, funModeling, G2Sd, ggtern, greport, IAPWS95, 
  imageData, imager, InSilicoVA, llama, localgauss, ltbayes, magick, 
  MergeGUI, MissingDataGUI, morse, mrMLM, mwaved, npregfast, OpenStreetMap, 
  openVA, OriGen, pcaPA, pcrsim, playwith, PortfolioEffectHFT, qdap, ReporteRs, 
  RJafroc, rmcfs, rrepast, RSA, rstan, rsvg, SEERaBomb, seewave, simmr, spcosa, 
  StroupGLMM, strvalidator, subspaceMOA, UBL, UsingR, vmsbase, x.ent, XGR, 
  XLConnect

### False positives

These seem to be a couple of false positives: neither I nor the maintainers could reproduce outside of my revdep check setup:

* radiant.model: checking examples ... ERROR
* data.table: checking tests ... ERROR
