# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.2.1 (2015-06-18) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (0.99.720)           |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/Chicago              |
|date     |2015-10-15                   |

## Packages

|package  |*  |version |date       |source         |
|:--------|:--|:-------|:----------|:--------------|
|digest   |   |0.6.8   |2014-12-31 |CRAN (R 3.2.0) |
|gtable   |   |0.1.2   |2012-12-05 |CRAN (R 3.2.0) |
|hexbin   |   |1.27.1  |2015-08-19 |CRAN (R 3.2.0) |
|Hmisc    |   |3.17-0  |2015-09-21 |CRAN (R 3.2.0) |
|knitr    |   |1.10.5  |2015-05-06 |CRAN (R 3.2.0) |
|mapproj  |   |1.2-4   |2015-08-03 |CRAN (R 3.2.0) |
|maps     |   |3.0.0-2 |2015-10-02 |CRAN (R 3.2.0) |
|maptools |   |0.8-37  |2015-09-29 |CRAN (R 3.2.0) |
|multcomp |   |1.4-1   |2015-07-23 |CRAN (R 3.2.0) |
|plyr     |   |1.8.3   |2015-06-12 |CRAN (R 3.2.0) |
|quantreg |   |5.19    |2015-08-31 |CRAN (R 3.2.0) |
|reshape2 |   |1.4.1   |2014-12-06 |CRAN (R 3.2.0) |
|scales   |   |0.3.0   |2015-08-25 |CRAN (R 3.2.0) |
|testthat |*  |0.10.0  |2015-05-22 |CRAN (R 3.2.0) |

# Check results
556 checked out of 562 dependencies 

## abctools (1.0.3)
Maintainer: Matt Nunes <m.nunes@lancaster.ac.uk>  
Bug reports: http://github.com/dennisprangle/abctools/issues

```
checking whether package ‘abctools’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/abctools.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## abd (0.2-8)
Maintainer: Kevin M. Middleton <middletonk@missouri.edu>

__OK__

## ACDm (1.0.2)
Maintainer: Markus Belfrage <markus.belfrage@gmail.com>

```
checking whether package ‘ACDm’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ACDm.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## adegenet (2.0.0)
Maintainer: Thibaut Jombart <t.jombart@imperial.ac.uk>

__OK__

## alm (0.4.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/alm/issues

```
checking whether package ‘alm’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘alm’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/alm.Rcheck/00install.out’ for details.
```
```
checking files in ‘vignettes’ ... NOTE
The following directory looks like a leftover from 'knitr':
  ‘figure’
Please remove from your package.
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## alphahull (2.0)
Maintainer: Beatriz Pateiro-Lopez <beatriz.pateiro@usc.es>

```
checking whether package ‘alphahull’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/alphahull.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## AmpliconDuo (1.0)
Maintainer: Anja Lange <anja.lange@uni-due.de>

```
checking examples ... ERROR
Running examples in ‘AmpliconDuo-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: AmpliconDuo-package
> ### Title: Statistical Analysis Of Amplicon Data Of The Same Sample To
> ###   Identify Spurious Amplicons
> ### Aliases: AmpliconDuo-package AmpliconDuo
> ### Keywords: package
> 
> ### ** Examples
> 
> 
> ## load test amplicon frequency data ampliconfreqs and vector with sample names site.f
> data(ampliconfreqs)
> data(site.f)
> 
> ## generating ampliconduo data frames 
> ## depending on the size if the data sets, may take some time
> ampliconduoset <- ampliconduo(ampliconfreqs[,1:4], sample.names = site.f[1:2])
..> 
> ## plot amplicon read numbers of sample A  vs. amplicon read numbers of sample B,
> ## indicating amplicons with significant deviations in their occurence across samples
> plotAmpliconduo.set(ampliconduoset, nrow = 3)
> 
> ## calculate discordance between the two data sets of an ampliconduo
> discordance <- discordance.delta(ampliconduoset)
> 
> ## plot the odds ratio density of ampliconduo data
> plotORdensity(ampliconduoset)
Error: Unknown parameters: NA
Execution halted
```
```
DONE
Status: 1 ERROR
```

## ANOM (0.4.2)
Maintainer: Philip Pallmann <p.pallmann@lancaster.ac.uk>

__OK__

## aoristic (0.6)
Maintainer: George Kikuchi <gkikuchi@csufresno.edu>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 NOTE
```

## apsimr (1.2)
Maintainer: Bryan Stanfill <bstanfill2003@gmail.com>  
Bug reports: https://github.com/stanfill/apsimr/issues

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘APSIMBatch’
```
```
DONE
Status: 1 NOTE
```

## archetypes (2.2-0)
Maintainer: Manuel J. A. Eugster <manuel@mjae.net>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘MASS’ ‘TSP’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
ginv.zalphasfn: no visible global function definition for ‘ginv’
tspsimplex_projection: no visible global function definition for
  ‘solve_TSP’
tspsimplex_projection: no visible global function definition for ‘TSP’
```
```
DONE
Status: 2 NOTEs
```

## archivist (1.7)
Maintainer: Przemyslaw Biecek <przemyslaw.biecek@gmail.com>  
Bug reports: https://github.com/pbiecek/archivist/issues

__OK__

## ARPobservation (1.1)
Maintainer: James E. Pustejovsky <jepusto@gmail.com>

__OK__

## arqas (1.3)
Maintainer: Borja Varela <borja.varela.brea@gmail.com>

```
checking whether package ‘arqas’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘ggplot2::Geom’ when loading ‘arqas’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/arqas.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## asremlPlus (2.0-0)
Maintainer: Chris Brien <Chris.Brien@unisa.edu.au>

```
checking package dependencies ... NOTE
Package which this enhances but not available for checking: ‘asreml’
```
```
checking R code for possible problems ... NOTE
predictionplot.asreml: possible error in layer(geom = "bar", stat =
  "identity", fill = "grey50"): unused argument (fill = "grey50")
predictionplot.asreml: possible error in layer(geom = "bar", stat =
  "identity", fill = cbPalette[1]): unused argument (fill =
  cbPalette[1])
predictionplot.asreml: possible error in layer(geom = "point", shape =
  symb[7]): unused argument (shape = symb[7])
predictionplot.asreml: possible error in layer(geom = "line", colour =
  "black"): unused argument (colour = "black")
predictionplot.asreml: possible error in layer(geom = "line", colour =
  cbPalette[1]): unused argument (colour = cbPalette[1])
```
```
DONE
Status: 2 NOTEs
```

## asVPC (1.0.2)
Maintainer: Eun-Kyung Lee <lee.eunk@gmail.com>

__OK__

## automap (1.0-14)
Maintainer: Paul Hiemstra <paul@numbertheory.nl>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘ggplot2’ ‘gpclib’ ‘maptools’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
cv.compare.ggplot: no visible global function definition for ‘ggplot’
cv.compare.ggplot: no visible global function definition for
  ‘aes_string’
cv.compare.ggplot: no visible global function definition for
  ‘facet_wrap’
cv.compare.ggplot: no visible global function definition for
  ‘scale_x_continuous’
cv.compare.ggplot: no visible global function definition for
  ‘scale_y_continuous’
cv.compare.ggplot: no visible global function definition for
  ‘coord_equal’
cv.compare.ggplot: no visible global function definition for
  ‘scale_color_gradient2’
cv.compare.ggplot: no visible global function definition for
  ‘scale_size_continuous’
cv.compare.ggplot: no visible global function definition for ‘fortify’
cv.compare.ggplot: no visible global function definition for
  ‘geom_path’
cv.compare.ggplot: no visible global function definition for
  ‘geom_point’
```
```
DONE
Status: 2 NOTEs
```

## BACA (1.3)
Maintainer: Vittorio Fortino <vittorio.fortino@ttl.fi>

```
checking package dependencies ... ERROR
Package required but not available: ‘RDAVIDWebService’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## backShift (0.1.3)
Maintainer: Christina Heinze <heinze@stat.math.ethz.ch>  
Bug reports: https://github.com/christinaheinze/backShift/issues

```
checking whether package ‘backShift’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/backShift.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## bamdit (2.0.1)
Maintainer: Pablo Emilio Verde <pabloemilio.verde@hhu.de>

```
checking whether package ‘bamdit’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/bamdit.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## BBEST (0.1-5)
Maintainer: Anton Gagin <av.gagin@gmail.com>

__OK__

## bbmle (1.0.17)
Maintainer: Ben Bolker <bolker@mcmaster.ca>

```
checking R code for possible problems ... NOTE
splom.slice : up0: no visible global function definition for
  ‘panel.points’
splom.slice: no visible binding for global variable ‘diag.panel.splom’
xyplot.slice : pfun: no visible global function definition for
  ‘panel.xyplot’
xyplot.slice : pfun: no visible global function definition for
  ‘panel.abline’
xyplot.slice : pfun: no visible global function definition for
  ‘panel.number’
```
```
DONE
Status: 1 NOTE
```

## BCEA (2.1-1)
Maintainer: Gianluca Baio <gianluca@stats.ucl.ac.uk>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘INLA’
```
```
DONE
Status: 1 NOTE
```

## bcp (4.0.0)
Maintainer: John W. Emerson <john.emerson@yale.edu>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘DNAcopy’
```
```
checking whether package ‘bcp’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/bcp.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR, 1 NOTE
```

## bcrm (0.4.5)
Maintainer: Michael Sweeting <mjs212@medschl.cam.ac.uk>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘BRugs’
```
```
checking whether package ‘bcrm’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘bcrm’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘bcrm’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/bcrm.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘bcrm-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: bcrm
> ### Title: Bayesian Continual Reassessment Method for Phase I
> ###   Dose-Escalation Trials
> ### Aliases: bcrm
> 
> ### ** Examples
> 
> ## Dose-escalation cancer trial example as described in Neuenschwander et al 2008.
> ## Pre-defined doses
> dose<-c(1,2.5,5,10,15,20,25,30,40,50,75,100,150,200,250)
> ## Pre-specified probabilities of toxicity
> ## [dose levels 11-15 not specified in the paper, and are for illustration only]
> p.tox0<-c(0.010,0.015,0.020,0.025,0.030,0.040,0.050,0.100,0.170,0.300,0.400,0.500,0.650
+   ,0.800,0.900)
> ## Data from the first 5 cohorts of 18 patients
> data<-data.frame(patient=1:18,dose=rep(c(1:4,7),c(3,4,5,4,2)),tox=rep(0:1,c(16,2)))
> ## Target toxicity level
> target.tox<-0.30
> 
> ## A 1-parameter power model is used, with standardised doses calculated using 
> ## the plug-in prior median
> ## Prior for alpha is lognormal with mean 0 (on log scale) 
> ## and standard deviation 1.34 (on log scale)
> ## The recommended dose for the next cohort if posterior mean is used
> Power.LN.bcrm<-bcrm(stop=list(nmax=18),data=data,p.tox0=p.tox0,dose=dose
+   ,ff="power",prior.alpha=list(3,0,1.34^2),target.tox=target.tox,constrain=FALSE
+   ,sdose.calculate="median",pointest="mean")

 Stopping: Reached maximum sample size
> print(Power.LN.bcrm)
 Estimation method:  exact 

 Model:  1-parameter power 

 Prior:  Lognormal( Mean:0, Variance:1.7956) 

 Standardised doses (skeleton): 
    1   2.5     5    10    15    20    25    30    40    50    75   100   150 
0.010 0.015 0.020 0.025 0.030 0.040 0.050 0.100 0.170 0.300 0.400 0.500 0.650 
  200   250 
0.800 0.900 

 Unmodified (unconstrained) CRM used 

 Posterior mean estimate of probability of toxicity used to select next dose 

 Toxicities observed: 
            Doses
             1 2.5 5 10 15 20 25 30 40 50 75 100 150 200 250
  n          3   4 5  4  0  0  2  0  0  0  0   0   0   0   0
  Toxicities 0   0 0  0  0  0  2  0  0  0  0   0   0   0   0

 Posterior estimates of toxicity: 
        Doses
              1    2.5      5     10     15     20     25    30    40    50
  Mean   0.0702 0.0866 0.1010 0.1130 0.1250 0.1460 0.1650 0.244 0.333 0.467
  SD     0.0558 0.0630 0.0686 0.0731 0.0769 0.0831 0.0879 0.102 0.109 0.108
  Median 0.0561 0.0723 0.0865 0.0995 0.1110 0.1330 0.1530 0.237 0.330 0.471
        Doses
             75   100   150    200    250
  Mean   0.5580 0.641 0.757 0.8650 0.9330
  SD     0.0996 0.088 0.066 0.0398 0.0205
  Median 0.5640 0.648 0.764 0.8700 0.9360
         Doses
Quantiles       1     2.5      5     10     15     20     25     30    40    50
    2.5%  0.00493 0.00787 0.0110 0.0142 0.0175 0.0244 0.0316 0.0702 0.130 0.249
    25%   0.02860 0.03910 0.0488 0.0579 0.0667 0.0833 0.0990 0.1690 0.255 0.395
    50%   0.05610 0.07230 0.0865 0.0995 0.1110 0.1330 0.1530 0.2370 0.330 0.471
    75%   0.09710 0.11900 0.1380 0.1540 0.1690 0.1960 0.2190 0.3120 0.408 0.544
    97.5% 0.21300 0.24400 0.2690 0.2900 0.3080 0.3400 0.3660 0.4620 0.552 0.668
         Doses
Quantiles    75   100   150   200   250
    2.5%  0.347 0.450 0.608 0.773 0.886
    25%   0.493 0.586 0.717 0.842 0.922
    50%   0.564 0.648 0.764 0.870 0.936
    75%   0.629 0.704 0.804 0.893 0.948
    97.5% 0.735 0.792 0.865 0.928 0.965

 Next recommended dose:  40 
> plot(Power.LN.bcrm)
> 
> ## Simulate 10 replicate trials of size 36 (cohort size 3) using this design 
> ## with constraint (i.e. no dose-skipping) and starting at lowest dose
> ## True probabilities of toxicity are set to pre-specified probabilities (p.tox0) 
> Power.LN.bcrm.sim<-bcrm(stop=list(nmax=36),p.tox0=p.tox0,dose=dose,ff="power"
+   ,prior.alpha=list(3,0,1.34^2),target.tox=target.tox,constrain=TRUE
+   ,sdose.calculate="median",pointest="mean",start=1,simulate=TRUE,nsims=10,truep=p.tox0)
1 
2 
3 
4 
5 
6 
7 
8 
9 
10 
> print(Power.LN.bcrm.sim)
Operating characteristics based on  10  simulations: 
 
              
Sample size 36

                            Doses
                                  1    2.5      5     10     15     20     25
  Experimentation proportion 0.0833 0.0833 0.0833 0.0833 0.0833 0.0833 0.0833
  Recommendation proportion  0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000
                            Doses
                                 30    40    50    75    100 150 200 250
  Experimentation proportion 0.0833 0.108 0.133 0.075 0.0167   0   0   0
  Recommendation proportion  0.0000 0.100 0.500 0.200 0.2000   0   0   0

                            Probability of DLT
                             [0,0.2] (0.2,0.4] (0.4,0.6] (0.6,0.8] (0.8,1]
  Experimentation proportion   0.775     0.208    0.0167         0       0
  Recommendation proportion    0.100     0.700    0.2000         0       0
> plot(Power.LN.bcrm.sim)
Error: StatBin requires a continuous x variable the x variable is discrete. Perhaps you want stat="count"?
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE
```

## bde (1.0.1)
Maintainer: Guzman Santafe <guzman.santafe@unavarra.es>

__OK__

## bdscale (1.2)
Maintainer: Dave Mills <dave.a.mills@gmail.com>

```
checking whether package ‘bdscale’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘bdscale’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/bdscale.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## bdvis (0.1.0)
Maintainer: Vijay Barve <vijay.barve@gmail.com>

```
checking whether package ‘bdvis’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘bdvis’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘bdvis’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/bdvis.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## benchmark (0.3-6)
Maintainer: Manuel J. A. Eugster <manuel@mjae.net>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘Rgraphviz’
```
```
checking whether package ‘benchmark’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘benchmark’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/benchmark.Rcheck/00install.out’ for details.
```
```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘multcomp’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
boxplot.AlgorithmPerformance: no visible binding for global variable
  ‘algorithms’
boxplot.AlgorithmPerformance: no visible binding for global variable
  ‘value’
boxplot.AlgorithmPerformance: no visible binding for global variable
  ‘samples’
bsgraph0.dist: no visible global function definition for ‘addEdge’
bsgraph0.graphNEL: no visible global function definition for
  ‘getDefaultAttrs’
bsgraph0.graphNEL: no visible global function definition for ‘agopen’
densityplot.AlgorithmPerformance: no visible binding for global
  variable ‘value’
densityplot.AlgorithmPerformance: no visible binding for global
  variable ‘algorithms’
mi: no visible global function definition for ‘mi.plugin’
patch.relation_class_ids: no visible global function definition for
  ‘relation_is_strict_weak_order’
plot.DatasetCharacterization: no visible binding for global variable
  ‘characteristics’
plot.DatasetCharacterization: no visible binding for global variable
  ‘value’
plot.DatasetCharacterization: no visible binding for global variable
  ‘samples’
plot.DatasetCharacterization: no visible global function definition for
  ‘theme_text’
plot.TestResult: no visible binding for global variable ‘samples’
plot.TestResult: no visible binding for global variable ‘value’
stripchart.AlgorithmPerformance: no visible binding for global variable
  ‘algorithms’
stripchart.AlgorithmPerformance: no visible binding for global variable
  ‘value’
stripchart.AlgorithmPerformance: no visible binding for global variable
  ‘samples’
```
```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘graph’
```
```
DONE
Status: 1 WARNING, 4 NOTEs
```

## binom (1.1-1)
Maintainer: Sundar Dorai-Raj <sdorairaj@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘ggplot2’ ‘lattice’ ‘polynom’ ‘tcltk’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
binom.bayes.densityplot: no visible global function definition for
  ‘ggplot’
binom.bayes.densityplot: no visible global function definition for
  ‘aes_string’
binom.bayes.densityplot: no visible global function definition for
  ‘geom_polygon’
binom.bayes.densityplot: no visible global function definition for
  ‘facet_wrap’
binom.bayes.densityplot: no visible global function definition for
  ‘xlim’
binom.bayes.densityplot: no visible global function definition for
  ‘xlab’
binom.bayes.densityplot: no visible global function definition for
  ‘ylim’
binom.bayes.densityplot: no visible global function definition for
  ‘ylab’
binom.bayes.densityplot: no visible global function definition for
  ‘theme_bw’
binom.profile: no visible global function definition for ‘xyplot’
binom.profile : <anonymous>: no visible global function definition for
  ‘panel.xyplot’
binom.profile : <anonymous>: no visible global function definition for
  ‘llines’
binom.profile : <anonymous>: no visible global function definition for
  ‘ltext’
integrate.poly: no visible global function definition for ‘polynomial’
integrate.poly: no visible global function definition for ‘integral’
panel.binom.lrt: no visible global function definition for
  ‘panel.xyplot’
panel.binom.lrt: no visible global function definition for
  ‘current.panel.limits’
panel.binom.lrt: no visible global function definition for ‘llines’
panel.binom.lrt: no visible global function definition for ‘ltext’
panel.binom.lrt: no visible global function definition for ‘lpoints’
panel.binom.plot.levelplot: no visible global function definition for
  ‘panel.levelplot’
panel.binom.plot.levelplot: no visible global function definition for
  ‘lpolygon’
panel.binom.plot.xyplot: no visible global function definition for
  ‘panel.abline’
panel.binom.plot.xyplot: no visible global function definition for
  ‘panel.xyplot’
tkbinom.power: no visible global function definition for
  ‘trellis.par.set’
tkbinom.power: no visible global function definition for ‘col.whitebg’
tkbinom.power : <local>: no visible global function definition for
  ‘tclVar’
tkbinom.power : <local>: no visible binding for global variable
  ‘tclVar’
tkbinom.power : <local>: no visible global function definition for
  ‘trellis.par.get’
tkbinom.power : <local> : replot: no visible global function definition
  for ‘tclObj’
tkbinom.power : <local> : replot: no visible global function definition
  for ‘xyplot’
tkbinom.power : <local> : replot : <anonymous>: no visible global
  function definition for ‘llines’
tkbinom.power : <local> : replot : <anonymous>: no visible global
  function definition for ‘panel.abline’
tkbinom.power : <local> : replot : <anonymous>: no visible global
  function definition for ‘tclObj’
tkbinom.power : <local> : replot.maybe: no visible global function
  definition for ‘tclObj’
tkbinom.power : <local>: no visible global function definition for
  ‘tktoplevel’
tkbinom.power : <local>: no visible global function definition for
  ‘tkwm.title’
tkbinom.power : <local>: no visible global function definition for
  ‘tkframe’
tkbinom.power : <local>: no visible global function definition for
  ‘tkpack’
tkbinom.power : <local>: no visible global function definition for
  ‘tklabel’
tkbinom.power : <local>: no visible global function definition for
  ‘tkradiobutton’
tkbinom.power : <local>: no visible global function definition for
  ‘tkcheckbutton’
tkbinom.power : <local>: no visible global function definition for
  ‘tkscale’
tkbinom.power : <local>: no visible global function definition for
  ‘tkbutton’
tkbinom.power : <local> : <anonymous>: no visible global function
  definition for ‘tkdestroy’
tkbinom.power2: no visible global function definition for
  ‘trellis.par.set’
tkbinom.power2: no visible global function definition for ‘col.whitebg’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tclVar’
tkbinom.power2 : <local>: no visible binding for global variable
  ‘tclVar’
tkbinom.power2 : <local> : replot: no visible global function
  definition for ‘tclObj’
tkbinom.power2 : <local> : replot.maybe: no visible global function
  definition for ‘tclObj’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tktoplevel’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tkwm.title’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tkframe’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tkpack’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tklabel’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tkradiobutton’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tkcheckbutton’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tkscale’
tkbinom.power2 : <local>: no visible global function definition for
  ‘tkbutton’
tkbinom.power2 : <local> : <anonymous>: no visible global function
  definition for ‘tkdestroy’
```
```
DONE
Status: 3 NOTEs
```

## biogas (1.1.0)
Maintainer: Sasha D. Hafner <saha@kbm.sdu.dk>

__OK__

## biogram (1.2)
Maintainer: Michal Burdukiewicz <michalburdukiewicz@gmail.com>  
Bug reports: https://github.com/michbur/biogram/issues

__OK__

## biomod2 (3.1-64)
Maintainer: Damien Georges <damien.georges2@gmail.com>  
Bug reports: <https://r-forge.r-project.org/R/?group_id=302>

```
checking whether package ‘biomod2’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘randomForest::margin’ when loading ‘biomod2’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/biomod2.Rcheck/00install.out’ for details.
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘gam’ ‘mgcv’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## BioStatR (2.0.0)
Maintainer: Frederic Bertrand <frederic.bertrand@math.unistra.fr>

__OK__

## BlandAltmanLeh (0.1.0)
Maintainer: Bernhard Lehnert <bernhard.lehnert@uni-greifswald.de>

```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘ggplot2’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
bland.altman.ggplot2: no visible global function definition for
  ‘ggplot’
bland.altman.ggplot2: no visible global function definition for ‘aes’
bland.altman.ggplot2: no visible global function definition for
  ‘geom_point’
bland.altman.ggplot2: no visible global function definition for
  ‘geom_hline’
bland.altman.ggplot2: no visible global function definition for ‘xlab’
bland.altman.ggplot2: no visible global function definition for ‘ylab’
```
```
DONE
Status: 2 NOTEs
```

## blowtorch (1.0.2)
Maintainer: Steven Pollack <steven@pollackphoto.net>

```
checking whether package ‘blowtorch’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘blowtorch’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘blowtorch’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/blowtorch.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## bmmix (0.1-2)
Maintainer: Thibaut Jombart <t.jombart@imperial.ac.uk>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 NOTE
```

## bootnet (0.1)
Maintainer: Sacha Epskamp <mail@sachaepskamp.com>

__OK__

## breakpoint (1.1)
Maintainer: Priyadarshana W.J.R.M. <madawa.weerasinghe@mq.edu.au>

__OK__

## brms (0.5.0)
Maintainer: Paul-Christian Buerkner <paul.buerkner@gmail.com>  
Bug reports: http://github.com/paul-buerkner/brms/issues

__OK__

## broman (0.59-5)
Maintainer: Karl W Broman <kbroman@biostat.wisc.edu>

```
checking R code for possible problems ... NOTE
Found the following calls to attach():
File ‘broman/R/loadfile.R’:
  attach(file)
See section ‘Good practice’ in ‘?attach’.
```
```
DONE
Status: 1 NOTE
```

## broom (0.3.7)
Maintainer: David Robinson <admiral.david@gmail.com>  
Bug reports: http://github.com/dgrtwo/broom/issues

```
checking examples ... ERROR
Running examples in ‘broom-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: cch_tidiers
> ### Title: tidiers for case-cohort data
> ### Aliases: cch_tidiers glance.cch tidy.cch
> 
> ### ** Examples
> 
> if (require("survival", quietly = TRUE)) {
+     # examples come from cch documentation
+     subcoh <- nwtco$in.subcohort
+     selccoh <- with(nwtco, rel==1|subcoh==1)
+     ccoh.data <- nwtco[selccoh,]
+     ccoh.data$subcohort <- subcoh[selccoh]
+     ## central-lab histology
+     ccoh.data$histol <- factor(ccoh.data$histol,labels=c("FH","UH"))
+     ## tumour stage
+     ccoh.data$stage <- factor(ccoh.data$stage,labels=c("I","II","III" ,"IV"))
+     ccoh.data$age <- ccoh.data$age/12 # Age in years
+ 
+     fit.ccP <- cch(Surv(edrel, rel) ~ stage + histol + age, data = ccoh.data,
+                    subcoh = ~subcohort, id= ~seqno, cohort.size = 4028)
+ 
+     tidy(fit.ccP)
+ 
+     # coefficient plot
+     library(ggplot2)
+     ggplot(tidy(fit.ccP), aes(x = estimate, y = term)) + geom_point() +
+         geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
+         geom_vline()
+ 
+     # compare between methods
+     library(dplyr)
+     fits <- data_frame(method = c("Prentice", "SelfPrentice", "LinYing")) %>%
+         group_by(method) %>%
+         do(tidy(cch(Surv(edrel, rel) ~ stage + histol + age, data = ccoh.data,
+                     subcoh = ~subcohort, id= ~seqno, cohort.size = 4028,
+                     method = .$method)))
+ 
+     # coefficient plots comparing methods
+     ggplot(fits, aes(x = estimate, y = term, color = method)) + geom_point() +
+         geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
+         geom_vline()
+ }

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

Error: geom_vline requires the following missing aesthetics: xintercept
Execution halted
```
```
DONE
Status: 1 ERROR
```

## BTSPAS (2014.0901)
Maintainer: Carl J Schwarz <cschwarz@stat.sfu.ca>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘BRugs’
```
```
checking whether package ‘BTSPAS’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/BTSPAS.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR, 1 NOTE
```

## capm (0.8.0)
Maintainer: Oswaldo Santos Baquero <oswaldosant@gmail.com>

```
checking whether package ‘capm’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘capm’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘capm’
  Warning: replacing previous import by ‘sp::nowrapSpatialLines’ when loading ‘capm’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/capm.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## caret (6.0-57)
Maintainer: Max Kuhn <Max.Kuhn@pfizer.com>  
Bug reports: https://github.com/topepo/caret/issues

```
checking S3 generic/method consistency ... WARNING
ggplot:
  function(data, mapping, ..., environment)
ggplot.rfe:
  function(data, metric, output, ...)

ggplot:
  function(data, mapping, ..., environment)
ggplot.train:
  function(data, metric, plotType, output, nameInStrip, ...)

ggplot:
  function(data, mapping, ..., environment)
ggplot.train:
  function(data, metric, plotType, output, nameInStrip, ...)

ggplot:
  function(data, mapping, ..., environment)
ggplot.rfe:
  function(data, metric, output, ...)

See section ‘Generic functions and methods’ in the ‘Writing R
Extensions’ manual.
```
```
DONE
Status: 1 WARNING
```

## caretEnsemble (1.0.0)
Maintainer: Zachary A. Mayer <zach.mayer@gmail.com>  
Bug reports: https://github.com/zachmayer/caretEnsemble/issues

```
checking whether package ‘caretEnsemble’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘caretEnsemble’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘caretEnsemble’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/caretEnsemble.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## cate (1.0.4)
Maintainer: Qingyuan Zhao <qyzhao@stanford.edu>

```
checking package dependencies ... ERROR
Package required but not available: ‘sva’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## catenary (1.1)
Maintainer: Jonathan Tuke <simon.tuke@adelaide.edu.au>

```
checking dependencies in R code ... NOTE
Packages in Depends field not imported from:
  ‘boot’ ‘ggplot2’ ‘methods’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
getFunctionEnvelopeCat: no visible global function definition for
  ‘boot’
getFunctionEnvelopePara: no visible global function definition for
  ‘boot’
plot,catenary: no visible global function definition for ‘qplot’
plot,catenary: no visible global function definition for ‘geom_point’
plot,catenary: no visible global function definition for ‘aes’
plot,catenary: no visible global function definition for ‘labs’
```
```
DONE
Status: 2 NOTEs
```

## Causata (4.2-0)
Maintainer: Justin Hemann <justinh@causata.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
checking top-level files ... NOTE
Non-standard file/directory found at top level:
  ‘integration_tests’
```
```
checking R code for possible problems ... NOTE
GetMetadata.Connect: no visible global function definition for
  ‘dbGetQuery’
GetRawData.Connect: no visible global function definition for
  ‘dbGetQuery’
```
```
checking line endings in Makefiles ... NOTE
Found the following Makefile(s) without a final LF:
  inst/doc/Makefile
Some ‘make’ programs ignore lines not ending in LF.
```
```
checking files in ‘vignettes’ ... NOTE
The following files look like leftovers/mistakes:
  ‘Causata-vignette.log’
Please remove them from your package.
The following directory looks like a leftover from 'knitr':
  ‘figure’
Please remove from your package.
```
```
DONE
Status: 5 NOTEs
```

## cda (1.5.1)
Maintainer: Baptiste Auguie <baptiste.auguie@gmail.com>

```
checking whether package ‘cda’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/cda.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## ChainLadder (0.2.2)
Maintainer: Markus Gesmann <markus.gesmann@googlemail.com>  
Bug reports: https://github.com/mages/ChainLadder/issues

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...

Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
  Running 'texi2dvi' on 'ChainLadder.tex' failed.
LaTeX errors:
! Undefined control sequence.
l.40 !<a8>T<98><9f><e7><cd>^^[<da>m67<d9><dd><e5><e0><80><cd>M<84><e0><f9>s<ae>\<e1>
                                 <dd>;~<fd>bj*<c2><e9><ea>͊^^X<:<92><e2><cf>^^Py<c8><f3>^^P^^B))<97>{<95>=<9f>7<d5>r4...
The control sequence at the end of the top line
of your error message was never \def'ed. If you have
! Undefined control sequence.
l.81 ...U<d5>q^^\<c7>qTU<cd>f<b3>+++<aa><aa>r<ce>o޼yг<8d>.<b1>N<bd>r<a2><e4>E<d1>3<ca><fe>\XX
                                                  ^^H<82><e0><ea>ի<b7>nݺy<f3><e6><cc><cc>L^^P^^D^^...
The control sequence at the end of the top line
of your error message was never \def'ed. If you have
! LaTeX Error: Missing \begin{document}.

See the LaTeX manual or LaTeX Companion for explanation.
Type  H <return>  for immediate help.
 ...                                              
! Missing $ inserted.
<inserted text> 
                $
l.94 ...$ID<
Calls: buildVignettes -> texi2pdf -> texi2dvi
Execution halted

```
```
DONE
Status: 1 NOTE
```

## checkmate (1.6.2)
Maintainer: Michel Lang <michellang@gmail.com>  
Bug reports: https://github.com/mllg/checkmate/issues

__OK__

## chemosensors (0.7.8)
Maintainer: Andrey Ziyatdinov <andrey.ziyatdinov@upc.edu>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘doMC’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking examples ... ERROR
Running examples in ‘chemosensors-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: UNIMANsnoise
> ### Title: Dataset UNIMANsnoise.
> ### Aliases: UNIMANsnoise
> ### Keywords: data datasets
> 
> ### ** Examples
> 
> 
> data(UNIMANsnoise)
> 
> str(UNIMANsnoise, max.level = 2)
List of 1
 $ Bsd:List of 2
  ..$ SensorModel:List of 4
  ..$ Sensor     :List of 4
> 
> str(UNIMANsnoise$Bsd$Sensor, max.level = 1)
List of 4
 $ plsr        : num [1:3, 1:17] 0.04712 0.03075 0.00291 0.0363 0.02512 ...
 $ mvr         : num [1:3, 1:17] 0.04712 0.03075 0.00291 0.0363 0.02512 ...
 $ broken-stick: num [1:3, 1:17] 0.0831 0.01804 0.00212 0.06764 0.01457 ...
 $ ispline     : num [1:9, 1:17] 0.1816 0.1297 0.4216 0.0365 0.0272 ...
> 
> # SD parameters for a particular data model 'plsr'
> Bsd <- UNIMANsnoise$Bsd$Sensor$plsr
> 
> # plot #1
> df <- melt(Bsd, varnames = c("gas", "sensor"))
> 
> df <- mutate(df,
+   gas = LETTERS[gas], 
+   sensor = factor(paste("S", sensor, sep = ""), levels = paste("S", 1:17, sep = "")))
>   
> p1 <- qplot(sensor, value, data = df, geom = "bar") + 
+   facet_grid(gas ~ ., scales = "free_y") +
+   labs(x = "sensor", y = "sd parameter", title = "Sensor Noise in data model 'plsr'")
> p1  
Error: stat_count() must not be used with a y aesthetic.
Execution halted
```
```
DONE
Status: 1 ERROR, 2 NOTEs
```

## choroplethr (3.3.0)
Maintainer: Ari Lamstein <arilamstein@gmail.com>  
Bug reports: https://github.com/arilamstein/choroplethr/issues

__OK__

## choroplethrAdmin1 (1.0.0)
Maintainer: Ari Lamstein <arilamstein@gmail.com>

```
checking installed package size ... NOTE
  installed size is 17.9Mb
  sub-directories of 1Mb or more:
    data  17.9Mb
```
```
checking data for non-ASCII characters ... NOTE
  Note: found 1057225 marked UTF-8 strings
```
```
DONE
Status: 2 NOTEs
```

## choroplethrMaps (1.0)
Maintainer: Ari Lamstein <arilamstein@gmail.com>  
Bug reports: https://github.com/trulia/choroplethrMaps/issues

__OK__

## chron (2.3-47)
Maintainer: Kurt Hornik <Kurt.Hornik@R-project.org>

__OK__

## CINOEDV (2.0)
Maintainer: Junliang Shang <shangjunliang110@163.com>

__OK__

## cjoint (2.0.0)
Maintainer: Anton Strezhnev <astrezhnev@fas.harvard.edu>

__OK__

## classify (1.3)
Maintainer: Dr Chris Wheadon <chris.wheadon@gmail.com>

```
checking whether package ‘classify’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/classify.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## classyfire (0.1-2)
Maintainer: Eleni Chatzimichali <ea.chatzimichali@gmail.com>  
Bug reports: https://github.com/eaHat/classyfire/issues

__OK__

## clhs (0.5-4)
Maintainer: Pierre Roudier <roudierp@landcareresearch.co.nz>

```
checking whether package ‘clhs’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘clhs’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘clhs’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/clhs.Rcheck/00install.out’ for details.
```
```
checking R code for possible problems ... NOTE
plot.cLHS_result: no visible global function definition for ‘opts’
```
```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘vignette.Rnw’
  ...
> library(clhs)
Warning: replacing previous import by ‘grid::arrow’ when loading ‘clhs’
Warning: replacing previous import by ‘grid::unit’ when loading ‘clhs’

> res <- clhs(diamonds, size = 100, progress = FALSE, 
+     iter = 1000)

  When sourcing ‘vignette.R’:
Error: unable to find an inherited method for function ‘clhs’ for signature ‘"tbl_df"’
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Warning: replacing previous import by ‘grid::arrow’ when loading ‘clhs’
Warning: replacing previous import by ‘grid::unit’ when loading ‘clhs’

Error: processing vignette 'vignette.Rnw' failed with diagnostics:
 chunk 3 (label = simple_clhs) 
Error in (function (classes, fdef, mtable)  : 
  unable to find an inherited method for function ‘clhs’ for signature ‘"tbl_df"’
Execution halted

```
```
DONE
Status: 1 ERROR, 1 WARNING, 2 NOTEs
```

## clifro (2.4-0)
Maintainer: Blake Seers <blake.seers@gmail.com>  
Bug reports: https://github.com/ropensci/clifro/issues

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: ggplot2
Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Auckland&zoom=8&size=640x640&scale=2&maptype=hybrid&language=en-EN&sensor=false
Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Auckland&sensor=false
Quitting from lines 139-157 (cfStation.Rmd) 
Error: processing vignette 'cfStation.Rmd' failed with diagnostics:
unused argument (geom_params = list(raster = c("#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", 
"#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4C629A", "#4B6097", "#4B6097", "#4B6097", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#485D95", "#445A93", "#44588D", "#44588D", "#44588D", "#40558B", "#40558B", "#3A4D7C", "#3A4D7C", "#3B4B72", "#3B4B72", "#314361", "#3
Execution halted

```
```
DONE
Status: 1 NOTE
```

## ClimClass (2.0.1)
Maintainer: Emanuele Eccel <emanuele.eccel@fmach.it>

__OK__

## climwin (0.1.2)
Maintainer: Liam D. Bailey <liam.bailey@anu.edu.au>

__OK__

## clusterfly (0.4)
Maintainer: Hadley Wickham <h.wickham@gmail.com>

```
checking package dependencies ... ERROR
Package required but not available: ‘rggobi’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## clustrd (0.1.2)
Maintainer: Angelos Markos <amarkos@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 NOTE
```

## coefplot (1.2.0)
Maintainer: Jared P. Lander <packages@jaredlander.com>

```
checking whether package ‘coefplot’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/coefplot.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## coloc (2.3-1)
Maintainer: Chris Wallace <chris.wallace@cimr.cam.ac.uk>  
Bug reports: https://github.com/chr1swallace/coloc/issues

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘snpStats’
```
```
checking R code for possible problems ... NOTE
coloc.abf.snpStats: no visible global function definition for
  ‘single.snp.tests’
coloc.abf.snpStats: no visible global function definition for
  ‘col.summary’
fillin: no visible global function definition for ‘col.summary’
fillin: no visible global function definition for ‘snp.imputation’
fillin: no visible global function definition for ‘impute.snps’
```
```
checking running R code from vignettes ... WARNING
Errors in running code in vignettes:
when running code in ‘vignette.Rnw’
  ...
             values.against
values.for    c(-0.1, 1) c(0.9, 1.1)
  c(-0.1, 1)     1.00000  0.05150753
  c(0.9, 1.1)   19.41464  1.00000000

> library(snpStats)

  When sourcing ‘vignette.R’:
Error: there is no package called ‘snpStats’
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: colorspace
Loading required package: MASS
Loading required package: BMA
Loading required package: survival
Loading required package: leaps
Loading required package: robustbase

Attaching package: ‘robustbase’

The following object is masked from ‘package:survival’:

    heart

Loading required package: inline
Loading required package: rrcov
Scalable Robust Estimators with High Breakdown Point (version 1.3-8)


Error: processing vignette 'vignette.Rnw' failed with diagnostics:
 chunk 7 
Error in library(snpStats) : there is no package called ‘snpStats’
Execution halted

```
```
DONE
Status: 1 WARNING, 3 NOTEs
```

## CommT (0.1.1)
Maintainer: Michael Gruenstaeudl <mi.gruenstaeudl@gmail.com>

__OK__

## complmrob (0.5.8)
Maintainer: David Kepplinger <david.kepplinger@gmail.com>

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'predictdf.complmrob.part.Rd':
  ‘ggplot2’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
checking examples ... ERROR
Running examples in ‘complmrob-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot.complmrob
> ### Title: Diagnostic plots for the robust regression model with
> ###   compositional covariats
> ### Aliases: plot.complmrob
> 
> ### ** Examples
> 
> data <- data.frame(lifeExp = state.x77[, "Life Exp"], USArrests[ , -3])
> mUSArr <- complmrob(lifeExp ~ ., data = data)
> plot(mUSArr)
Warning: The plyr::rename operation has created duplicates for the following name(s): (`colour`, `size`, `alpha`, `shape`)
Error: Unknown parameters: complmrob.model, transform
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## confidence (1.1-0)
Maintainer: Dennis J. J. Walvoort <dennis.Walvoort@wur.nl>

__OK__

## conformal (0.1)
Maintainer: Isidro Cortes <isidrolauscher@gmail.com>

```
checking whether package ‘conformal’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘conformal’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘conformal’
  Warning: replacing previous import by ‘randomForest::margin’ when loading ‘conformal’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/conformal.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## contoureR (1.0.5)
Maintainer: Nicholas Hamilton <n.hamilton@unsw.edu.au>

__OK__

## cooccur (1.2)
Maintainer: Daniel M. Griffith <griffith.dan@gmail.com>

__OK__

## COPASutils (0.1.6)
Maintainer: Erik Andersen <erik.andersen@northwestern.edu>

```
checking whether package ‘COPASutils’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘kernlab::alpha’ when loading ‘COPASutils’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/COPASutils.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## cosinor (1.1)
Maintainer: Michael Sachs <sachsmc@gmail.com>

```
checking R code for possible problems ... NOTE
cosinor_analyzer: no visible binding for global variable ‘vitamind’
```
```
DONE
Status: 1 NOTE
```

## CosmoPhotoz (0.1)
Maintainer: Rafael S. de Souza <rafael.2706@gmail.com>

```
checking examples ... ERROR
Running examples in ‘CosmoPhotoz-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plotDiagPhotoZ
> ### Title: Plot diagnostics for photometric redshift estimations
> ### Aliases: plotDiagPhotoZ
> ### Keywords: hplot
> 
> ### ** Examples
> 
> # First, generate some mock data
> ppo <- runif(1000, min=0.1, max=2)
> ppo_ph <- rnorm(length(ppo), mean=ppo, sd=0.05)
> 
> # Then generate the plots
> plotDiagPhotoZ(ppo_ph, ppo, type="errordist")
Warning: `axis.ticks.margin` is deprecated. Please set `margin` property  of `axis.text` instead
Error in FUN(X[[i]], ...) : 
  Theme element 'text' has NULL property: margin, debug
Calls: print ... element_render -> calc_element -> lapply -> FUN -> lapply -> FUN
Execution halted
```
```
DONE
Status: 1 ERROR
```

## covmat (1.0)
Maintainer: Rohit Arora <emailrohitarora@gmail.com>

__OK__

## cowplot (0.5.0)
Maintainer: Claus O. Wilke <wilke@austin.utexas.edu>  
Bug reports: https://github.com/wilkelab/cowplot/issues

```
checking examples ... ERROR
Running examples in ‘cowplot-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: add_sub
> ### Title: Add annotation underneath a plot
> ### Aliases: add_sub
> 
> ### ** Examples
> 
> p1 <- ggplot(mtcars, aes(mpg, disp)) + geom_line(colour = "blue") + background_grid(minor='none')
> ggdraw(add_sub(p1, "This is an annotation.\nAnnotations can span multiple lines."))
Warning: `axis.ticks.margin` is deprecated. Please set `margin` property  of `axis.text` instead
Error in FUN(X[[i]], ...) : 
  Theme element 'text' has NULL property: margin, debug
Calls: ggdraw ... element_render -> calc_element -> lapply -> FUN -> lapply -> FUN
Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: cowplot
Loading required package: ggplot2

Attaching package: 'cowplot'

The following object is masked from 'package:ggplot2':

    ggsave

Loading required package: grid
Quitting from lines 17-26 (axis_position.Rmd) 
Error: processing vignette 'axis_position.Rmd' failed with diagnostics:
both operands must be units
Execution halted

```
```
DONE
Status: 1 ERROR, 1 NOTE
```

## cplm (0.7-4)
Maintainer: Yanwei (Wayne) Zhang <actuary_zhang@hotmail.com>

```
checking whether package ‘cplm’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/cplm.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## Crossover (0.1-15)
Maintainer: Kornelius Rohmeyer <rohmeyer@small-projects.de>  
Bug reports: https://github.com/kornl/Crossover/issues

```
checking whether package ‘Crossover’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/Crossover.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## cutoffR (1.0)
Maintainer: Lingbing Feng <fenglb88@gmail.com>

__OK__

## cvxclustr (1.1.1)
Maintainer: Eric C. Chi <ecchi1105@gmail.com>

```
checking whether package ‘cvxclustr’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/cvxclustr.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## dae (2.7-2)
Maintainer: Chris Brien <Chris.Brien@unisa.edu.au>

__OK__

## dams (0.1)
Maintainer: Gopi Goteti <my.ration.shop@gmail.com>

__OK__

## data.table (1.9.6)
Maintainer: Matt Dowle <mattjdowle@gmail.com>  
Bug reports: https://github.com/Rdatatable/data.table/issues

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘GenomicRanges’
```
```
DONE
Status: 1 NOTE
```

## dcmr (1.0)
Maintainer: Diane Losardo <dlosardo@amplify.com>

__OK__

## Deducer (0.7-7)
Maintainer: Ian Fellows <ian@fellstat.com>

```
checking whether package ‘Deducer’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘Deducer’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/Deducer.Rcheck/00install.out’ for details.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  as.matrix.cor.matrix plot.cor.matrix print.contin.table
  print.contin.tests print.contingency.tables print.cor.matrix
  print.freq.table print.multi.test sort.data.frame summary.lm
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
ggcorplot: possible error in layer(geom = "line", geom_params =
  list(colour = "red"), stat = "smooth", stat_params = list(method =
  line.method), data = z, mapping = aes(x = x_var, y = y_var)): unused
  arguments (geom_params = list(colour = "red"), stat_params =
  list(method = line.method))
ggcorplot: possible error in layer(geom = "ribbon", geom_params =
  list(fill = "green", alpha = 0.5), stat = "smooth", stat_params =
  list(method = line.method), data = z, mapping = aes(x = x_var, y =
  y_var)): unused arguments (geom_params = list(fill = "green", alpha =
  0.5), stat_params = list(method = line.method))
ggcorplot: possible error in layer(geom = "text", geom_params =
  list(size = var_text_size), data = diag, mapping = aes(x = y_mid, y =
  x_mid, label = x_label)): unused argument (geom_params = list(size =
  var_text_size))
```
```
checking examples ... ERROR
Running examples in ‘Deducer-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ggcorplot
> ### Title: Correlation matrix
> ### Aliases: ggcorplot
> 
> ### ** Examples
> 
> data(mtcars)
> corr.mat1<-cor.matrix(variables=d(mpg,carb,carb+rnorm(length(carb))),,
+ 	 data=mtcars,
+ 	 test=cor.test,
+ 	 method='spearman',
+ 	alternative="two.sided",exact=FALSE)
> 	
> p<-ggcorplot(corr.mat1,data = mtcars)
Error: Attempted to create layer with no stat.
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING, 2 NOTEs
```

## demi (1.1.2)
Maintainer: Sten Ilmjarv <sten.ilmjarv@gmail.com>

```
checking package dependencies ... ERROR
Packages required but not available: ‘affxparser’ ‘affy’ ‘oligo’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## dendextend (1.1.0)
Maintainer: Tal Galili <tal.galili@gmail.com>  
Bug reports: https://github.com/talgalili/dendextend/issues

```
checking package dependencies ... NOTE
Package which this enhances but not available for checking: ‘labeltodendro’
```
```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘WGCNA’, ‘moduleColor’
```
```
DONE
Status: 2 NOTEs
```

## DepthProc (1.0.3)
Maintainer: Zygmunt Zawadzki <zawadzkizygmunt@gmail.com>

```
checking whether package ‘DepthProc’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/DepthProc.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## DescribeDisplay (0.2.4)
Maintainer: Di Cook <dicook@iastate.edu>

```
checking whether package ‘DescribeDisplay’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/DescribeDisplay.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## dfexplore (0.2.1)
Maintainer: Joris Muller <joris.muller@etu.unistra.fr>

__OK__

## DFIT (1.0-2)
Maintainer: Victor H. Cervantes <vhcervantesb@unal.edu.co>

__OK__

## dielectric (0.2.3)
Maintainer: Baptiste Auguie <baptiste.auguie@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
checking dependencies in R code ... NOTE
Package in Depends field not imported from: ‘methods’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
L2eV: no visible binding for global variable ‘constants’
L2w: no visible binding for global variable ‘constants’
eV2L: no visible binding for global variable ‘constants’
t2eV: no visible binding for global variable ‘constants’
```
```
DONE
Status: 3 NOTEs
```

## diffeR (0.0-3)
Maintainer: Al� Santacruz <amsantac@unal.edu.co>

__OK__

## directlabels (2013.6.15)
Maintainer: Toby Dylan Hocking <toby@sg.cs.titech.ac.jp>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls to packages already attached by Depends:
  ‘grid’ ‘quadprog’
  Please remove these calls from your code.
'library' or 'require' calls in package code:
  ‘alphahull’ ‘ggplot2’ ‘inlinedocs’ ‘proto’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Packages in Depends field not imported from:
  ‘grid’ ‘quadprog’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
':::' calls which should be '::':
  ‘ggplot2:::Geom’ ‘ggplot2:::StatIdentity’
  See the note in ?`:::` about the use of this operator.
Missing objects imported by ':::' calls:
  ‘ggplot2:::aesdefaults’ ‘ggplot2:::coord_transform’
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  drawDetails.dlgrob
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
ahull.points: no visible global function definition for ‘ashape’
calc.boxes: no visible global function definition for
  ‘current.viewport’
direct.label.ggplot: no visible global function definition for
  ‘aes_string’
direct.label.ggplot: no visible global function definition for ‘guides’
dl.move : pf: no visible global function definition for ‘unit’
dl.move : pf: no visible global function definition for ‘convertUnit’
dlcompare: no visible global function definition for ‘grid.newpage’
dlcompare: no visible global function definition for ‘grid.layout’
dlcompare: no visible global function definition for ‘unit’
dlcompare: no visible global function definition for ‘pushViewport’
dlcompare: no visible global function definition for ‘viewport’
dlcompare: no visible global function definition for ‘grid.text’
dlcompare: no visible global function definition for ‘popViewport’
dlcompare: no visible global function definition for ‘grid.rect’
dldoc: no visible global function definition for ‘theme_set’
dldoc: no visible global function definition for ‘theme_grey’
dldoc : makehtml : <anonymous>: no visible global function definition
  for ‘grid.text’
dlgrob: no visible global function definition for ‘grob’
drawDetails.dlgrob: no visible global function definition for
  ‘convertX’
drawDetails.dlgrob: no visible global function definition for ‘unit’
drawDetails.dlgrob: no visible global function definition for
  ‘convertY’
drawDetails.dlgrob: no visible binding for global variable ‘gpar’
empty.grid: no visible global function definition for ‘convertX’
empty.grid: no visible global function definition for ‘unit’
empty.grid: no visible global function definition for ‘convertY’
empty.grid : draw : drawlines: no visible global function definition
  for ‘grid.segments’
empty.grid : draw : drawlines: no visible global function definition
  for ‘gpar’
extract.posfun: no visible global function definition for
  ‘extract.docs.file’
geom_dl: no visible global function definition for ‘proto’
geom_dl : default_aes: no visible global function definition for ‘aes’
panel.superpose.dl: no visible binding for global variable
  ‘panel.superpose’
panel.superpose.dl: no visible global function definition for
  ‘trellis.par.get’
panel.superpose.dl: no visible global function definition for
  ‘grid.draw’
project.onto.segments: no visible global function definition for
  ‘grid.segments’
qp.labels : <anonymous>: no visible global function definition for
  ‘solve.QP’
static.labels : <anonymous>: no visible global function definition for
  ‘convertX’
static.labels : <anonymous>: no visible global function definition for
  ‘unit’
static.labels : <anonymous>: no visible global function definition for
  ‘convertY’
xlimits: no visible global function definition for ‘convertX’
xlimits: no visible global function definition for ‘unit’
ylimits: no visible global function definition for ‘convertY’
ylimits: no visible global function definition for ‘unit’
```
```
checking examples ... ERROR
Running examples in ‘directlabels-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: direct.label
> ### Title: Direct labels for color decoding
> ### Aliases: direct.label
> 
> ### ** Examples
> 
> ## Add direct labels to a ggplot2 scatterplot, making sure that each
> ## label is close to its point cloud, and doesn't overlap points or
> ## other labels.
> library(ggplot2)
> scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
+                  main="Fuel efficiency depends on car size")
> print(direct.label(scatter))
Error in switch(geom, density = "top.bumptwice", line = { : 
  EXPR must be a length 1 vector
Calls: print ... direct.label.ggplot -> default.picker -> do.call -> <Anonymous>
Execution halted
```
```
DONE
Status: 1 ERROR, 3 NOTEs
```

## disclapmix (1.6.2)
Maintainer: Mikkel Meyer Andersen <mikl@math.aau.dk>

__OK__

## diveRsity (1.9.73)
Maintainer: Kevin Keenan <kkeenan02@qub.ac.uk>

```
checking whether package ‘diveRsity’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘diveRsity’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘diveRsity’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/diveRsity.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘parallel’ ‘plotrix’ ‘sendplot’ ‘xlsx’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
DONE
Status: 1 WARNING, 2 NOTEs
```

## dMod (0.1)
Maintainer: Daniel Kaschek <daniel.kaschek@physik.uni-freiburg.de>

__OK__

## doBy (4.5-13)
Maintainer: S�ren H�jsgaard <sorenh@math.aau.dk>

```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘pbkrtest’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  LSmatrix.default LSmeans.default LSmeans.lmerMod coef.lmBy
  coef.summary.lmBy esticon.coxph esticon.geeglm esticon.glm
  esticon.gls esticon.lm esticon.lme esticon.mer esticon.merMod
  firstobs.default firstobs.formula fitted.lmBy lastobs.default
  lastobs.formula linest.geeglm linest.glm linest.lm linest.lmerMod
  linest.merMod nullBasis.Matrix nullBasis.geeglm nullBasis.glm
  nullBasis.lm nullBasis.lmerMod nullBasis.matrix print.LSmatrix
  print.linearEstimate print.lmBy print.splitByData print.summary.lmBy
  residuals.lmBy summary.lmBy
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 2 NOTEs
```

## dotwhisker (0.2.0.1)
Maintainer: Yue Hu <yue-hu-1@uiowa.edu>  
Bug reports: https://github.com/fsolt/dotwhisker/issues

```
checking whether package ‘dotwhisker’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘dotwhisker’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘dotwhisker’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/dotwhisker.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## dplyr (0.4.3)
Maintainer: Hadley Wickham <hadley@rstudio.com>  
Bug reports: https://github.com/hadley/dplyr/issues

__OK__

## dslice (1.1.4)
Maintainer: Chao Ye <yechao1009@gmail.com>

```
checking whether package ‘dslice’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘dslice’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/dslice.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## dsm (2.2.9)
Maintainer: David Lawrence Miller <dave@ninepointeightone.net>

```
checking dependencies in R code ... NOTE
Unexported objects imported by ':::' calls:
  ‘mrds:::assign.par’ ‘mrds:::create.ddfobj’ ‘mrds:::detfct’
  ‘mrds:::process.data’
  See the note in ?`:::` about the use of this operator.
```
```
DONE
Status: 1 NOTE
```

## DTR (1.6)
Maintainer: Xinyu Tang <xtang@uams.edu>

```
checking whether package ‘DTR’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/DTR.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## dtwclust (1.1.0)
Maintainer: Alexis Sarda <alexis.sarda@gmail.com>

__OK__

## dtwSat (0.1.0)
Maintainer: Victor Maus <vwmaus1@gmail.com>

__OK__

## DVHmetrics (0.3.3)
Maintainer: Daniel Wollschlaeger <wollschlaeger@uni-mainz.de>

__OK__

## DynNom (1.0.1)
Maintainer: Amirhossein Jalali <a.jalali2@nuigalway.ie>

__OK__

## dynsim (1.2)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/dynsim/issues

```
checking whether package ‘dynsim’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘dynsim’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/dynsim.Rcheck/00install.out’ for details.
```
```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'dynsimGG.Rd':
  ‘ggplot2’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
DONE
Status: 2 WARNINGs
```

## dynsurv (0.2-2)
Maintainer: Jun Yan <jun.yan@uconn.edu>

__OK__

## earlywarnings (1.0.59)
Maintainer: Vasilis Dakos <vasilis.dakos@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking R code for possible problems ... NOTE
PlotPotential: no visible global function definition for ‘aes’
PlotPotential: no visible global function definition for ‘geom_tile’
PlotPotential: no visible global function definition for
  ‘scale_fill_gradient’
PlotPotential: no visible global function definition for ‘stat_contour’
PlotPotential: no visible global function definition for ‘xlab’
PlotPotential: no visible global function definition for ‘ylab’
PlotPotential: no visible global function definition for ‘labs’
```
```
DONE
Status: 2 NOTEs
```

## EasyHTMLReport (0.1.1)
Maintainer: Yohei Sato <yohei0511@gmail.com>

```
checking top-level files ... NOTE
Non-standard files/directories found at top level:
  ‘easy_html_report_tmp_1376284934.59207.tsv’
  ‘easy_html_report_tmp_1376284935.5951.tsv’
  ‘easy_html_report_tmp_1376284936.59848.tsv’
```
```
checking dependencies in R code ... NOTE
Namespaces in Imports field not imported from:
  ‘ggplot2’ ‘reshape2’ ‘scales’ ‘xtable’
  All declared Imports should be used.
Packages in Depends field not imported from:
  ‘base64enc’ ‘knitr’ ‘markdown’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
.file_attachment: no visible global function definition for
  ‘base64encode’
easyHtmlReport: no visible global function definition for ‘knit’
easyHtmlReport: no visible global function definition for
  ‘markdownToHTML’
```
```
DONE
Status: 3 NOTEs
```

## ecoengine (1.9.1)
Maintainer: Karthik Ram <karthik.ram@gmail.com>  
Bug reports: https://github.com/ropensci/ecoengine/issues

```
checking examples ... ERROR
Running examples in ‘ecoengine-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ee_photos
> ### Title: ee_photos
> ### Aliases: ee_photos
> 
> ### ** Examples
> 
> # Request all photos. This request will paginate.
> # merced <- ee_photos(county = "Merced County")
>  ee_photos(page_size = 10)
Search contains 74468 photos (downloading 1 of 1 pages 
)

  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |======================================================================| 100%Error in `$<-.data.frame`(`*tmp*`, "begin_date", value = numeric(0)) : 
  replacement has 0 rows, data has 1
Calls: ee_photos -> $<- -> $<-.data.frame
Execution halted
```
```
DONE
Status: 1 ERROR
```

## EcoGenetics (1.2.0-2)
Maintainer: Leandro Roser <learoser@gmail.com>

__OK__

## edgar (1.0.2)
Maintainer: Gunratan Lonare <lonare.gunratan@gmail.com>

__OK__

## eeptools (0.9.0)
Maintainer: Jared E. Knowles <jknowles@gmail.com>

```
checking whether package ‘eeptools’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘eeptools’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘eeptools’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/eeptools.Rcheck/00install.out’ for details.
```
```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'ggmapmerge.Rd':
  ‘ggplot2’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
checking examples ... ERROR
Running examples in ‘eeptools-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: autoplot.lm
> ### Title: A function to replicate the basic plot function for linear
> ###   models in ggplot2
> ### Aliases: autoplot.lm
> 
> ### ** Examples
> 
> # Univariate
> a <- runif(1000)
> b <- 7*a+rnorm(1)
> mymod <- lm(b~a)
> autoplot(mymod)
Error: geom_hline requires the following missing aesthetics: yintercept
Execution halted
```
```
checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  13: f(..., self = self) at /Users/hadley/Documents/ggplot/ggplot/R/ggproto.r:112
  14: check_required_aesthetics(self$geom$required_aes, c(names(data), names(self$aes_params)), 
         snake_class(self$geom)) at /Users/hadley/Documents/ggplot/ggplot/R/layer.r:194
  15: stop(name, " requires the following missing aesthetics: ", paste(missing_aes, collapse = ", "), 
         call. = FALSE) at /Users/hadley/Documents/ggplot/ggplot/R/utilities.r:33
  
  testthat results ================================================================
  OK: 503 SKIPPED: 0 FAILED: 2
  1. Error: It returns the correct data.frame 
  2. Error: Autoplot works as expected for linear models 
  
  Error: testthat unit tests failed
  Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: ggplot2
Warning: replacing previous import by 'grid::arrow' when loading 'eeptools'
Warning: replacing previous import by 'grid::unit' when loading 'eeptools'
Note: the specification for S3 class "family" in package 'MatrixModels' seems equivalent to one from package 'lme4': not turning on duplicate class definitions for this class.
Note: the specification for class "character or NULL" in package 'memisc' seems equivalent to one from package 'SparseM': not turning on duplicate class definitions for this class.
Quitting from lines 186-189 (intro.Rmd) 
Error: processing vignette 'intro.Rmd' failed with diagnostics:
geom_hline requires the following missing aesthetics: yintercept
Execution halted

```
```
DONE
Status: 2 ERRORs, 2 WARNINGs, 1 NOTE
```

## EFDR (0.1.1)
Maintainer: Andrew Zammit-Mangion <andrewzm@gmail.com>

__OK__

## EffectLiteR (0.3-3)
Maintainer: Axel Mayer <amayer2010@gmail.com>

__OK__

## ega (1.0.1)
Maintainer: Daniel Schmolze <ega@schmolze.com>

__OK__

## egcm (1.0.6)
Maintainer: Matthew Clegg <matthewcleggphd@gmail.com>

```
checking whether package ‘egcm’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘egcm’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘egcm’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/egcm.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## emil (2.2.2)
Maintainer: Christofer Backlin <emil@christofer.backlin.se>  
Bug reports: https://github.com/Molmed/emil/issues

```
checking examples ... ERROR
Running examples in ‘emil-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: subtree
> ### Title: Extract a subset of a tree of nested lists
> ### Aliases: subtree
> 
> ### ** Examples
> 
> l <- list(A=list(a=0:2, b=3:4, c=023-22030),
+           B=list(a=5:7, b=8:9))
> subtree(l, 1:2, "b")
     A B
[1,] 3 8
[2,] 4 9
> subtree(l, TRUE, mean, "a")
A B 
1 6 
> 
> # More practical examples
> x <- iris[-5]
> y <- iris$Species
> cv <- resample("crossvalidation", y, nfold=5, nrep=3)
> procedure <- modeling_procedure("pamr")
> 
> # To illustrate the error handling capacities of subtree we'll introduce some
> # spurious errors in the pre-processing function. By setting .return_error=TRUE
> # they wont break the execution, but will instead be return in the results.
> pre_error <- function(data, risk=.1){
+     if(runif(1) < risk)
+         stop("Oh no! Random error!")
+     data
+ }
> result <- evaluate(procedure, x, y, resample=cv,
+     .save=c(importance=TRUE), .return_error=TRUE,
+     pre_process = function(...){
+         pre_split(...) %>%
+             pre_error(risk=.3) %>%
+             pre_pamr
+     }
+ )
15 Oct 17:39  Evaluating modeling performance...
15 Oct 17:39    Repeat 1, fold 1
                Result size is 6.91 KiB.
                Estimated completion time is 17:39.
15 Oct 17:39    Repeat 1, fold 2
15 Oct 17:39    Repeat 1, fold 3
15 Oct 17:39    Repeat 1, fold 4
15 Oct 17:39    Repeat 1, fold 5
15 Oct 17:39    Repeat 2, fold 1
15 Oct 17:39    Repeat 2, fold 2
15 Oct 17:39    Repeat 2, fold 3
15 Oct 17:39    Repeat 2, fold 4
15 Oct 17:39    Repeat 2, fold 5
15 Oct 17:39    Repeat 3, fold 1
15 Oct 17:39    Repeat 3, fold 2
15 Oct 17:39    Repeat 3, fold 3
15 Oct 17:39    Repeat 3, fold 4
15 Oct 17:39    Repeat 3, fold 5
> message(sum(sapply(result, inherits, "error")),
+         " folds did not complete successfully!")
4 folds did not complete successfully!
> 
> # Extract error rates. Since some folds fail it will be an ugly list with both
> # numeric estimates and NULL values (for the failed folds).
> subtree(result, TRUE, "error")
$rep1fold1
[1] 0.03333333

$rep1fold2
[1] 0

$rep1fold3
NULL

$rep1fold4
NULL

$rep1fold5
[1] 0.03333333

$rep2fold1
NULL

$rep2fold2
[1] 0.1333333

$rep2fold3
[1] 0

$rep2fold4
[1] 0.06666667

$rep2fold5
[1] 0.03333333

$rep3fold1
[1] 0.03333333

$rep3fold2
[1] 0.1

$rep3fold3
[1] 0.1

$rep3fold4
NULL

$rep3fold5
[1] 0.1

> 
> # To put it on a more consistent form we can impute the missing error rates
> # with NA to allow automatic simplification into a vector (since it requires
> # all values to be on the same form, i.e. numeric(1) rather than a mix
> # between numeric(1) and NULL as in the previous example).
> subtree(result, TRUE, "error", error_value=as.numeric(NA), warn=-1)
 rep1fold1  rep1fold2  rep1fold3  rep1fold4  rep1fold5  rep2fold1  rep2fold2 
0.03333333 0.00000000         NA         NA 0.03333333         NA 0.13333333 
 rep2fold3  rep2fold4  rep2fold5  rep3fold1  rep3fold2  rep3fold3  rep3fold4 
0.00000000 0.06666667 0.03333333 0.03333333 0.10000000 0.10000000         NA 
 rep3fold5 
0.10000000 
> 
> # Sum up feature importance for all classes within each fold and extract.
> # Note that the lengths (= 4) must match between the folds for the automatic
> # simplification to work.
> subtree(result, TRUE, "importance", function(x){
+     if(is.null(x)){
+         rep(NA, 3)
+     } else {
+         colMeans(x[2:4])
+     }
+ })
            rep1fold1  rep1fold2 rep1fold3 rep1fold4  rep1fold5 rep2fold1
setosa     -1.2121196 -1.2276302        NA        NA -1.1767932        NA
versicolor  0.1523041  0.1544427        NA        NA  0.1348266        NA
virginica   1.0598154  1.0731875        NA        NA  1.0419666        NA
             rep2fold2  rep2fold3   rep2fold4  rep2fold5   rep3fold1
setosa     -1.09817907 -1.1841429 -1.16731960 -1.1835360 -1.10615510
versicolor  0.04930333  0.1306708  0.09546163  0.1095238  0.08193271
virginica   0.97373788  1.0534720  1.02425476  1.0500272  0.98331658
             rep3fold2  rep3fold3 rep3fold4  rep3fold5
setosa     -1.18271578 -1.2980377        NA -1.1779802
versicolor  0.07918483  0.1621244        NA  0.1292222
virginica   1.07987964  1.1359133        NA  1.0487580
> 
> # The equivalent 'select' command would be ...
> require(tidyr)
Loading required package: tidyr

Attaching package: ‘tidyr’

The following object is masked from ‘package:emil’:

    fill

> imp <- result %>% select(fold = TRUE, "importance", function(x){
+     if(is.null(x)) return(NULL)
+     x %>% gather(Species, Importance, -feature)
+ })
> require(ggplot2)
Loading required package: ggplot2
> ggplot(imp, aes(x=Species, y=Importance)) +
+     geom_abline(yintercept=0, slope=0, color="hotpink") +
+     geom_boxplot() + facet_wrap(~feature)
Error: Unknown parameters: yintercept
Execution halted
```
```
DONE
Status: 1 ERROR
```

## enigma (0.2.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropengov/enigma/issues

__OK__

## EpiDynamics (0.2)
Maintainer: Oswaldo Santos Baquero <oswaldosant@gmail.com>

```
checking whether package ‘EpiDynamics’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘EpiDynamics’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘EpiDynamics’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/EpiDynamics.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## episensr (0.7.1)
Maintainer: Denis Haine <denis.haine@gmail.com>  
Bug reports: https://github.com/dhaine/episensr/issues

__OK__

## erer (2.4)
Maintainer: Changyou Sun <cs258@msstate.edu>

__OK__

## ESGtoolkit (0.1)
Maintainer: Thierry Moudiki <thierry.moudiki@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
checking R code for possible problems ... NOTE
esgplotshocks: no visible global function definition for ‘geom_point’
esgplotshocks: no visible global function definition for ‘aes’
esgplotshocks: no visible global function definition for ‘theme’
esgplotshocks: no visible global function definition for
  ‘element_blank’
esgplotshocks: no visible global function definition for
  ‘scale_color_manual’
esgplotshocks: no visible global function definition for ‘geom_density’
esgplotshocks: no visible global function definition for
  ‘scale_fill_manual’
esgplotshocks: no visible global function definition for ‘coord_flip’
esgplotts: no visible global function definition for ‘xlab’
esgplotts: no visible global function definition for ‘ylab’
esgplotts: no visible global function definition for ‘theme’
```
```
DONE
Status: 2 NOTEs
```

## etm (0.6-2)
Maintainer: Arthur Allignol <arthur.allignol@uni-ulm.de>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 NOTE
```

## eurostat (1.0.16)
Maintainer: Lahti Leo <louhos@googlegroups.com>  
Bug reports: https://github.com/ropengov/eurostat/issues

```
checking files in ‘vignettes’ ... NOTE
The following directory looks like a leftover from 'knitr':
  ‘figure’
Please remove from your package.
```
```
DONE
Status: 1 NOTE
```

## evaluate (0.8)
Maintainer: Yihui Xie <xie@yihui.name>  
Bug reports: https://github.com/hadley/evaluate/issues

__OK__

## evolqg (0.1-9)
Maintainer: Diogo Melo <diogro@usp.br>  
Bug reports: https://github.com/lem-usp/evolqg/issues

```
checking examples ... ERROR
Running examples in ‘evolqg-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: Rarefaction
> ### Title: Rarefaction analysis via ressampling
> ### Aliases: PlotRarefaction Rarefaction
> ### Keywords: bootstap rarefaction repeatability
> 
> ### ** Examples
> 
> ind.data <- iris[1:50,1:4]
> 
> results.RS <- Rarefaction(ind.data, PCAsimilarity, num.reps = 5, iterations = 100)
> results.Mantel <- Rarefaction(ind.data, MatrixCor, correlation = TRUE,
+                               num.reps = 5, iterations = 100)
> results.KrzCov <- Rarefaction(ind.data, KrzCor, num.reps = 5, iterations = 100)
> results.PCA <- Rarefaction(ind.data, PCAsimilarity, num.reps = 5, iterations = 100)
> 
> #Multiple threads can be used with some foreach backend library, like doMC or doParallel
> #library(doParallel)
> ##Windows:
> #cl <- makeCluster(2)
> #registerDoParallel(cl)
> ##Mac and Linux:
> #registerDoParallel(cores = 2)
> #results.KrzCov <- Rarefaction(ind.data, KrzCor, num.reps = 5, parallel = TRUE)
> 
> #Easy access
> library(reshape2)
> melt(results.RS)
        value L1
1   0.8711419  1
2   0.9375862  1
3   0.1275785  1
4   0.9881679  1
5   0.7960694  1
6   0.9789168  2
7   0.9701507  2
8   0.7028587  2
9   0.9707332  2
10  0.9654603  2
11  0.5048638  3
12  0.5315258  3
13  0.9354292  3
14  0.9696264  3
15  0.6776366  3
16  0.9200138  4
17  0.8326970  4
18  0.9079901  4
19  0.9812691  4
20  0.8681309  4
21  0.9401457  5
22  0.9667612  5
23  0.9730222  5
24  0.9074775  5
25  0.7969072  5
26  0.9511478  6
27  0.8649195  6
28  0.8729060  6
29  0.9805820  6
30  0.9291425  6
31  0.8643381  7
32  0.9803881  7
33  0.9679119  7
34  0.9493757  7
35  0.9241382  7
36  0.9493848  8
37  0.9615401  8
38  0.9824975  8
39  0.9470300  8
40  0.9965838  8
41  0.9739871  9
42  0.8880790  9
43  0.9973845  9
44  0.9874541  9
45  0.9923719  9
46  0.9901877 10
47  0.9175344 10
48  0.9561266 10
49  0.9084606 10
50  0.9912193 10
51  0.9506067 11
52  0.9882129 11
53  0.9352957 11
54  0.9719490 11
55  0.9556577 11
56  0.9431721 12
57  0.9793228 12
58  0.9448131 12
59  0.9827488 12
60  0.9699028 12
61  0.9820660 13
62  0.9382103 13
63  0.9633454 13
64  0.9893409 13
65  0.9866354 13
66  0.9791086 14
67  0.9896877 14
68  0.9482763 14
69  0.9864740 14
70  0.9619848 14
71  0.9467598 15
72  0.9177348 15
73  0.9933600 15
74  0.9941690 15
75  0.9965563 15
76  0.9731473 16
77  0.9652501 16
78  0.9538527 16
79  0.9796818 16
80  0.9898807 16
81  0.9936495 17
82  0.9457624 17
83  0.9977365 17
84  0.9880955 17
85  0.9899556 17
86  0.9513739 18
87  0.9803870 18
88  0.9964493 18
89  0.9523389 18
90  0.9954350 18
91  0.9951494 19
92  0.9777309 19
93  0.9672977 19
94  0.9976506 19
95  0.9939515 19
96  0.9933019 20
97  0.9912479 20
98  0.9945743 20
99  0.9931095 20
100 0.9862204 20
101 0.9964639 21
102 0.9857692 21
103 0.9684279 21
104 0.9804915 21
105 0.9859946 21
106 0.9971380 22
107 0.9851838 22
108 0.9848418 22
109 0.9870261 22
110 0.9846950 22
111 0.9777007 23
112 0.9969293 23
113 0.9847680 23
114 0.9906356 23
115 0.9763132 23
116 0.9941621 24
117 0.9942443 24
118 0.9717165 24
119 0.9808515 24
120 0.9811217 24
121 0.9787336 25
122 0.9756102 25
123 0.9677741 25
124 0.9953170 25
125 0.9925051 25
126 0.9933557 26
127 0.9951159 26
128 0.9821851 26
129 0.9982798 26
130 0.9611614 26
131 0.9867807 27
132 0.9690558 27
133 0.9880727 27
134 0.9992711 27
135 0.9918338 27
136 0.9549263 28
137 0.9802328 28
138 0.9921057 28
139 0.9878432 28
140 0.9734731 28
141 0.9771753 29
142 0.9801565 29
143 0.9910663 29
144 0.9833305 29
145 0.9900968 29
146 0.9781557 30
147 0.9945877 30
148 0.9815174 30
149 0.9973128 30
150 0.9768159 30
151 0.9966064 31
152 0.9898946 31
153 0.9546872 31
154 0.9861624 31
155 0.9758398 31
156 0.9934624 32
157 0.9970472 32
158 0.9876583 32
159 0.9600919 32
160 0.9489179 32
161 0.9965148 33
162 0.9892347 33
163 0.9973581 33
164 0.9870640 33
165 0.9967527 33
166 0.9925772 34
167 0.9970558 34
168 0.9932622 34
169 0.9942743 34
170 0.9966920 34
171 0.9733735 35
172 0.9925029 35
173 0.9878932 35
174 0.9901267 35
175 0.9921262 35
176 0.9919480 36
177 0.9912185 36
178 0.9869917 36
179 0.9826715 36
180 0.9719686 36
181 0.9975697 37
182 0.9971541 37
183 0.9923497 37
184 0.9957337 37
185 0.9942649 37
186 0.9700449 38
187 0.9885127 38
188 0.9924232 38
189 0.9970670 38
190 0.9942835 38
191 0.9982293 39
192 0.9948001 39
193 0.9948088 39
194 0.9984600 39
195 0.9868021 39
196 0.9904519 40
197 0.9731091 40
198 0.9739364 40
199 0.9668586 40
200 0.9785129 40
201 0.9896844 41
202 0.9980030 41
203 0.9876827 41
204 0.9794370 41
205 0.9894081 41
206 0.9968275 42
207 0.9988710 42
208 0.9957601 42
209 0.9734128 42
210 0.9920208 42
211 0.9775573 43
212 0.9912456 43
213 0.9874790 43
214 0.9961684 43
215 0.9772753 43
216 0.9966432 44
217 0.9974951 44
218 0.9939085 44
219 0.9973505 44
220 0.9888212 44
221 0.9838261 45
222 0.9962038 45
223 0.9856424 45
224 0.9909363 45
225 0.9726848 45
226 0.9973134 46
227 0.9812429 46
228 0.9959824 46
229 0.9978854 46
230 0.9963704 46
231 0.9893706 47
232 0.9988725 47
233 0.9881617 47
234 0.9811206 47
235 0.9897931 47
236 0.9970695 48
237 0.9838576 48
238 0.9990941 48
239 0.9899728 48
240 0.9948575 48
241 0.9985418 49
242 0.9978309 49
243 0.9991817 49
244 0.9936696 49
245 0.9905422 49
> 
> #Plotting using ggplot2
> a <- PlotRarefaction(results.RS, "Random Skewers")
Error: Attempted to create layer with no stat.
Execution halted
```
```
checking tests ... ERROR
Running the tests in ‘tests/run-all.R’ failed.
Last 13 lines of output:
  Component "plots": Component 35: Component 8: Component 8: Attributes: < Component 1: Modes: character, numeric >
  Component "plots": Component 35: Component 8: Component 8: Attributes: < Component 1: target is character, current is numeric >
  Component "plots": Component 35: Component 8: Component 8: current is not list-like
  Component "plots": Component 36: Component 8: Component 7: Attributes: < Length mismatch: comparison on first 1 components >
  Component "plots": Component 36: Component 8: Component 8: Modes: list, numeric
  Component "plots": Component 36: Component 8: Component 8: Lengths: 10, 400
  Component "plots": Component 36: Component 8: Component 8: names for target but not for current
  Component "plots": Component 36: Component 8: Component 8: Attributes: < Names: 1 string mismatch >
  Component "plots": Component 36: Component 8: Component 8: Attributes: < Component 1: Modes: character, numeric >
  Component "plots": Component 36: Component 8: Component 8: Attributes: < Component 1: target is character, current is numeric >
  Component "plots": Component 36: Component 8: Component 8: current is not list-like
  Error: Test failures
  Execution halted
```
```
DONE
Status: 2 ERRORs
```

## ExtDist (0.6-3)
Maintainer: A. Jonathan R. Godfrey <a.j.godfrey@massey.ac.nz>

__OK__

## extracat (1.7-3)
Maintainer: Alexander Pilhoefer <alexander.pilhoefer@gmail.com>

```
checking whether package ‘extracat’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘ggplot2::unit’ when loading ‘extracat’
  Warning: replacing previous import by ‘ggplot2::arrow’ when loading ‘extracat’
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘extracat’
  Warning: replacing previous import by ‘data.table::melt’ when loading ‘extracat’
  Warning: replacing previous import by ‘data.table::dcast’ when loading ‘extracat’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/extracat.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘extracat-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ahist
> ### Title: Histogram using active bins
> ### Aliases: ahist
> 
> ### ** Examples
> 
> ahist(rnorm(100))
Error: Unknown parameters: binwidth
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## ez (4.2-2)
Maintainer: Michael A. Lawrence <mike.lwrnc@gmail.com>

```
checking whether package ‘ez’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ez.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## ezsim (0.5.5)
Maintainer: TszKin Julian Chan <ctszkin@gmail.com>  
Bug reports: TszKin Julian Chan <ctszkin@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘vignette.Rnw’
  ...
13   hat(mu) 60     1  0 -0.0177  0 -0.0177 0.1257 0.1270           -Inf
14   hat(mu) 60     1  2  1.9982  2 -0.0018 0.1325 0.1325        -0.0009
15   hat(mu) 60     3  0 -0.0137  0 -0.0137 0.3130 0.3133           -Inf
16   hat(mu) 60     3  2  2.0764  2  0.0764 0.3529 0.3611         0.0382

> print(plot(ezsim_basic, return_print = TRUE)[[1]])

  When sourcing ‘vignette.R’:
Error: argument "value" is missing, with no default
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...

Error: processing vignette 'vignette.Rnw' failed with diagnostics:
 chunk 4 
Error in paste(variable, value, sep = "==") : 
  argument "value" is missing, with no default
Execution halted

```
```
DONE
Status: 1 ERROR, 2 NOTEs
```

## FAOSTAT (2.0)
Maintainer: Filippo Gheri <filippo.gheri@fao.org>

```
checking whether package ‘FAOSTAT’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘FAOSTAT’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/FAOSTAT.Rcheck/00install.out’ for details.
```
```
checking data for non-ASCII characters ... NOTE
  Note: found 179 marked UTF-8 strings
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## fbroc (0.3.1)
Maintainer: Erik Peter <jerikpeter@googlemail.com>  
Bug reports: http://github.com/erikpeter/fbroc/issues

__OK__

## fermicatsR (1.3)
Maintainer: Pablo Saz Parkinson <sazpark2@gmail.com>

__OK__

## FField (0.1.0)
Maintainer: Grigori Kapoustin <gregk@alphabetaworks.com>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘ggplot2’ ‘gridExtra’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
FFieldPtRepDemo: no visible global function definition for ‘ggplot’
FFieldPtRepDemo: no visible global function definition for ‘aes’
FFieldPtRepDemo: no visible binding for global variable ‘mpg’
FFieldPtRepDemo: no visible global function definition for ‘geom_point’
FFieldPtRepDemo: no visible global function definition for ‘geom_text’
FFieldPtRepDemo: no visible global function definition for ‘ggtitle’
FFieldPtRepDemo: no visible global function definition for
  ‘geom_segment’
FFieldPtRepDemo: no visible global function definition for
  ‘grid.arrange’
```
```
DONE
Status: 2 NOTEs
```

## fheatmap (1.0.0)
Maintainer: Sivasish Sindiri<sentisci@gmail.com>

```
checking whether package ‘fheatmap’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘fheatmap’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘fheatmap’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/fheatmap.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘fheatmap-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name:  Column Annotations 
> ### Title: Annotations of columns in "fheatmap_data"
> ### Aliases: ' annotation_col '
> ### Keywords: annotation_col
> 
> ### ** Examples
> 
> data(annotation_col)
> fheatmap(fheatmap_data,annotation_col=annotation_col)
Warning: `axis.ticks.margin` is deprecated. Please set `margin` property  of `axis.text` instead
Warning: `axis.ticks.margin` is deprecated. Please set `margin` property  of `axis.text` instead
Error: Unknown parameters: hjust
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## FinCal (0.6)
Maintainer: Felix Yanhui Fan <nolanfyh@gmail.com>

```
checking whether package ‘FinCal’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘FinCal’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/FinCal.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## fishmove (0.3-3)
Maintainer: Johannes Radinger <jradinger@igb-berlin.de>

__OK__

## flowr (0.9.8.2)
Maintainer: Sahil Seth <me@sahilseth.com>  
Bug reports: https://github.com/sahilseth/flowr/issues

__OK__

## forestFloor (1.8.6)
Maintainer: Soeren Havelund Welling <SOWE@DTU.DK>

__OK__

## frailtySurv (1.2.2)
Maintainer: John V. Monaco <vincent@vmonaco.com>  
Bug reports: https://github.com/vmonaco/frailtySurv/issues

__OK__

## freqparcoord (1.0.0)
Maintainer: Norm Matloff <normmatloff@gmail.com>

```
checking dependencies in R code ... NOTE
Package in Depends field not imported from: ‘parallel’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
smoothz: no visible global function definition for ‘splitIndices’
smoothz: no visible global function definition for ‘clusterApply’
smoothzpred: no visible global function definition for ‘splitIndices’
smoothzpred: no visible global function definition for ‘clusterApply’
```
```
checking examples ... ERROR
Running examples in ‘freqparcoord-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: smoothz
> ### Title: Smoothing functions.
> ### Aliases: smoothz smoothzpred knnreg knndens
> 
> ### ** Examples
> 
> 
> # programmers and engineers in Silicon Valley, 2000 census, age 25-65
> data(prgeng)
> pg <- prgeng
> pg1 <- pg[pg$age >= 25 & pg$age <= 65,]
> estreg <- smoothz(pg1[,c(1,8)],sf=knnreg,k=100)
> age <- pg1[,1]
> p <- ggplot(data.frame(age,estreg))
> p + geom_smooth(aes(x=age,y=estreg))
Error in loadNamespace(name) : there is no package called ‘mgcv’
Calls: print ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
Execution halted
```
```
DONE
Status: 1 ERROR, 2 NOTEs
```

## freqweights (1.0.2)
Maintainer: Emilio Torres-Manzanera <torres@uniovi.es>

__OK__

## frontiles (1.2)
Maintainer: Thibault Laurent <thibault.laurent@univ-tlse1.fr>

```
checking whether package ‘frontiles’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/frontiles.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## fSRM (0.6.1)
Maintainer: Felix Schönbrodt <felix@nicebread.de>

```
checking whether package ‘fSRM’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘fSRM’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘fSRM’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/fSRM.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## G2Sd (2.1.4)
Maintainer: Regis K. Gallon <reg.gallon@gmail.com>

__OK__

## gapmap (0.0.2)
Maintainer: Ryo Sakai <ryo.sakai@esat.kuleuven.be>

```
checking whether package ‘gapmap’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘gapmap’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘gapmap’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/gapmap.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## gapminder (0.1.0)
Maintainer: Jennifer Bryan <jenny@stat.ubc.ca>  
Bug reports: https://github.com/jennybc/gapminder/issues

__OK__

## gcookbook (1.0)
Maintainer: Winston Chang <winston@stdout.org>

__OK__

## GDAdata (0.93)
Maintainer: Antony Unwin<unwin@math.uni-augsburg.de>

__OK__

## GenCAT (1.0.1)
Maintainer: Eric Reed <reeder@bu.edu>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘snpStats’
```
```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘snpStats’
```
```
checking data for non-ASCII characters ... NOTE
  Error in .requirePackage(package) : 
    unable to find required package 'snpStats'
  Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
  Execution halted
```
```
checking examples ... ERROR
Running examples in ‘GenCAT-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: GenCAT
> ### Title: Running GenCAT
> ### Aliases: GenCAT
> ### Keywords: ~kwd1 ~kwd2
> 
> ### ** Examples
> 
> 
> ###############
> #Running GenCAT
> ###############
> data("CardioMapped")
> 
> #Subset CardioMapped to decrease CPU time
> CardioMappedSub<-CardioMapped[CardioMapped$chr < 15,]
> set.seed(1)
> CardioMappedSub<-CardioMappedSub[sample(1:nrow(CardioMappedSub), 100),]
> 
> print(head(CardioMappedSub))
            SNP effect_allele other_allele   testStat chr position class
17232 rs9572807             C            T -0.3014771  13 72423959 DACH1
24151 rs4389009             G            A -1.4443860  13 99193519 STK24
37178 rs7151730             T            A  0.1387317  14 33531585 NPAS3
58941 rs1076958             G            A  2.0597766  14 91131857 TTC7B
13089 rs7987481             G            A -1.0024210  13 47318950 LRCH1
58302  rs411064             G            A  0.2132412  14 90008860 FOXN3
> 
> library(snpStats)
Error in library(snpStats) : there is no package called ‘snpStats’
Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: dplyr

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Loading required package: doParallel
Loading required package: foreach
Loading required package: iterators
Loading required package: parallel
Loading required package: ggplot2
Quitting from lines 62-69 (GenCAT-vignette.Rmd) 
Error: processing vignette 'GenCAT-vignette.Rmd' failed with diagnostics:
unable to find required package 'snpStats'
Execution halted

```
```
DONE
Status: 1 ERROR, 4 NOTEs
```

## gender (0.5.1)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/gender/issues

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘genderdata’
```
```
DONE
Status: 1 NOTE
```

## GERGM (0.3.2)
Maintainer: Matthew J. Denny <mzd5530@psu.edu>

__OK__

## gettingtothebottom (3.2)
Maintainer: Jocelyn T. Chi <jocelynchi@alum.berkeley.edu>

```
checking whether package ‘gettingtothebottom’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘gettingtothebottom’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘gettingtothebottom’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/gettingtothebottom.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## gfcanalysis (1.2)
Maintainer: Alex Zvoleff <azvoleff@conservation.org>  
Bug reports: https://github.com/azvoleff/gfcanalysis/issues

```
checking whether package ‘gfcanalysis’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘gfcanalysis’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/gfcanalysis.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## GGally (0.5.0)
Maintainer: Barret Schloerke <schloerke@gmail.com>

```
checking whether package ‘GGally’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘GGally’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘GGally’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/GGally.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
checking examples ... ERROR
Running examples in ‘GGally-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: getPlot
> ### Title: getPlot
> ### Aliases: getPlot
> ### Keywords: hplot
> 
> ### ** Examples
> 
> data(tips, package = "reshape")
>  plotMatrix2 <- ggpairs(tips[,3:2], upper = list(combo = "denstrip"))
>  getPlot(plotMatrix2, 1, 2)
Error in eval(expr, envir, enclos) : object 'density' not found
Calls: print ... unique -> unlist -> lapply -> lapply -> FUN -> eval
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE
```

## ggdendro (0.1-17)
Maintainer: Andrie de Vries <apdevries@gmail.com>  
Bug reports: https://github.com/andrie/ggdendro/issues

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'ggdendro-package.Rd':
  ‘[ggplot2]{ggplot2}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
DONE
Status: 1 WARNING
```

## ggenealogy (0.1.0)
Maintainer: Lindsay Rutter <lrutter@iastate.edu>

__OK__

## ggExtra (0.3.0)
Maintainer: Dean Attali <daattali@gmail.com>  
Bug reports: https://github.com/daattali/ggExtra/issues

__OK__

## ggfortify (0.0.4)
Maintainer: Masaaki Horikoshi <sinhrks@gmail.com>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking: ‘dlm’ ‘MSwM’ ‘lfda’
```
```
checking whether package ‘ggfortify’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ggfortify.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR, 1 NOTE
```

## ggmap (2.5.2)
Maintainer: David Kahle <david.kahle@gmail.com>  
Bug reports: https://github.com/dkahle/ggmap/issues

```
checking whether package ‘ggmap’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ggmap.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## ggmcmc (0.7.2)
Maintainer: Xavier Fernández i Marín <xavier.fim@gmail.com>  
Bug reports: https://github.com/xfim/ggmcmc/issues

```
checking examples ... ERROR
Running examples in ‘ggmcmc-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ggs_pairs
> ### Title: Create a plot matrix of posterior simulations
> ### Aliases: ggs_pairs
> 
> ### ** Examples
> 
> data(linear)
> 
> # default ggpairs plot
> ggs_pairs(ggs(s))
> 
> # change alpha transparency of points
> ggs_pairs(ggs(s), lower=list(params=c(alpha=.2)))
> 
> # with too many points, try contours instead
> ggs_pairs(ggs(s), lower=list(continuous="density"))
> 
> # histograms instead of univariate densities on diagonal
> ggs_pairs(ggs(s), diag=list(continuous="bar"))
Error in eval(expr, envir, enclos) : object 'density' not found
Calls: print ... unique -> unlist -> lapply -> lapply -> FUN -> eval
Execution halted
```
```
DONE
Status: 1 ERROR
```

## ggparallel (0.1.2)
Maintainer: Heike Hofmann <hofmann@iastate.edu>

__OK__

## ggRandomForests (1.1.4)
Maintainer: John Ehrlinger <john.ehrlinger@gmail.com>  
Bug reports: https://github.com/ehrlinger/ggRandomForests/issues

```
checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    data   2.6Mb
    doc    3.2Mb
```
```
checking examples ... ERROR
Running examples in ‘ggRandomForests-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: gg_minimal_vimp
> ### Title: Minimal depth vs VIMP camparison by variable rankings.
> ### Aliases: gg_minimal_vimp
> 
> ### ** Examples
> 
> ## Examples from RFSRC package...
> ## ------------------------------------------------------------
> ## classification example
> ## ------------------------------------------------------------
> ## -------- iris data
> ## You can build a randomForest
> # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
> # varsel_iris <- var.select(rfsrc_iris)
> # ... or load a cached randomForestSRC object
> data(varsel_iris, package="ggRandomForests")
> 
> # Get a data.frame containing minimaldepth measures
> gg_dta<- gg_minimal_vimp(varsel_iris)
> 
> # Plot the gg_minimal_depth object
> plot(gg_dta)
Error: Unknown parameters: xintercept
Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union


 randomForestSRC 1.6.1 
 
 Type rfsrc.news() to see new features, changes, and bug fixes. 
 

Quitting from lines 246-256 (randomForestSRC-Regression.Rnw) 
Error: processing vignette 'randomForestSRC-Regression.Rnw' failed with diagnostics:
subscript out of bounds
Execution halted

```
```
DONE
Status: 1 ERROR, 2 NOTEs
```

## ggROC (1.0)
Maintainer: Honglong Wu <wuhonglong.china@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
Package listed in more than one of Depends, Imports, Suggests, Enhances:
  ‘ggplot2’
A package should be listed in only one of these fields.
```
```
checking dependencies in R code ... NOTE
Package in Depends field not imported from: ‘ggplot2’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
ggroc: no visible global function definition for ‘ggplot’
ggroc: no visible global function definition for ‘aes’
ggroc: no visible global function definition for ‘geom_point’
ggroc: no visible global function definition for ‘geom_line’
ggroc: no visible global function definition for ‘theme’
ggroc: no visible global function definition for ‘element_text’
ggroc: no visible global function definition for ‘labs’
ggroc: no visible global function definition for ‘ggsave’
```
```
DONE
Status: 3 NOTEs
```

## ggsn (0.2.0)
Maintainer: Oswaldo Santos Baquero <oswaldosant@gmail.com>

```
checking whether package ‘ggsn’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘ggsn’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘ggsn’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ggsn.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘ggsn-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: blank
> ### Title: Blank theme
> ### Aliases: blank
> 
> ### ** Examples
> 
> library(rgdal)
Loading required package: sp
rgdal: version: 1.0-4, (SVN revision 548)
 Geospatial Data Abstraction Library extensions to R successfully loaded
 Loaded GDAL runtime: GDAL 1.10.1, released 2013/08/26
 Path to GDAL shared files: /usr/local/Cellar/gdal/1.10.1_1/share/gdal
 Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March 2012, [PJ_VERSION: 480]
 Path to PROJ.4 shared files: (autodetected)
 Linking to sp version: 1.1-1 
> dsn <- system.file('extdata', package = 'ggsn')
> map <- readOGR(dsn, 'sp')
OGR data source with driver: ESRI Shapefile 
Source: "/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ggsn.Rcheck/ggsn/extdata", layer: "sp"
with 96 features
It has 2 fields
> map@data$id <- 1:nrow(map@data)
> map.ff <- fortify(map, region = 'id')
Error in get("rgeos", envir = .MAPTOOLS_CACHE) : object 'rgeos' not found
Calls: fortify ... fortify.SpatialPolygonsDataFrame -> <Anonymous> -> rgeosStatus -> get
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## ggsubplot (0.3.2)
Maintainer: Garrett Grolemund <garrett@rstudio.com>

```
checking whether package ‘ggsubplot’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ggsubplot.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## ggswissmaps (0.0.2)
Maintainer: Sandro Petrillo Burri <gibo.gaf@gmail.com>

```
checking examples ... ERROR
Running examples in ‘ggswissmaps-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: maps
> ### Title: A list with 8 maps of Switzerland's territory at various levels.
> ### Aliases: maps
> 
> ### ** Examples
> 
> data(maps)
> maps[[1]]
Error in match.fun(FUN) : object 'scale_clone' not found
Calls: print ... ggplot_build -> plot_clone -> <Anonymous> -> lapply -> match.fun
Execution halted
```
```
DONE
Status: 1 ERROR
```

## ggtern (1.0.6.1)
Maintainer: Nicholas Hamilton <nick@ggtern.com>

```
checking whether package ‘ggtern’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ggtern.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## ggthemes (2.2.1)
Maintainer: Jeffrey B. Arnold <jeffrey.arnold@gmail.com>  
Bug reports: http://github.com/jrnold/ggthemes

```
checking whether package ‘ggthemes’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ggthemes.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## gitter (1.1.1)
Maintainer: Omar Wagih <wagih@ebi.ac.uk>

```
checking package dependencies ... ERROR
Package required but not available: ‘EBImage’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## glinternet (1.0.0)
Maintainer: Michael Lim <michael626@gmail.com>

__OK__

## glycanr (0.2.0)
Maintainer: Ivo Ugrina <ivo@iugrina.com>  
Bug reports: https://github.com/iugrina/glycanr/issues

__OK__

## gmum.r (0.2.1)
Maintainer: Stanislaw Jastrzebski <staszek.jastrzebski@gmail.com>  
Bug reports: https://github.com/gmum/gmum.r/issues

```
checking whether package ‘gmum.r’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/gmum.r.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## googlesheets (0.1.0)
Maintainer: Jennifer Bryan <jenny@stat.ubc.ca>  
Bug reports: https://github.com/jennybc/googlesheets/issues

__OK__

## GOplot (1.0.1)
Maintainer: Wencke Walter <wencke.walter@arcor.de>  
Bug reports: https://github.com/wencke/wencke.github.io/issues

__OK__

## gpmap (0.1.1)
Maintainer: Arne B. Gjuvsland <arne.gjuvsland@nmbu.no>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking dependencies in R code ... NOTE
Packages in Depends field not imported from:
  ‘foreach’ ‘ggplot2’ ‘isotone’ ‘plyr’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
decompose_monotone: no visible global function definition for ‘%dopar%’
decompose_monotone: no visible global function definition for ‘foreach’
decompose_monotone: no visible global function definition for ‘%do%’
decompose_monotone_single: no visible global function definition for
  ‘%dopar%’
decompose_monotone_single: no visible global function definition for
  ‘foreach’
decompose_monotone_single: no visible global function definition for
  ‘laply’
degree_of_monotonicity: no visible global function definition for
  ‘%do%’
degree_of_monotonicity: no visible global function definition for
  ‘foreach’
degree_of_monotonicity_single: no visible global function definition
  for ‘aaply’
monotone_regression: no visible global function definition for
  ‘activeSet’
partial_genotype_order: no visible global function definition for
  ‘aaply’
plot1_dec: no visible global function definition for ‘%do%’
plot1_dec: no visible global function definition for ‘foreach’
plot1_dec: no visible global function definition for ‘ggplot’
plot1_dec: no visible global function definition for ‘aes_string’
plot1_dec: no visible global function definition for ‘geom_point’
plot1_dec: no visible global function definition for ‘facet_wrap’
plot1_dec: no visible global function definition for ‘labs’
plot1_orig: no visible global function definition for ‘%do%’
plot1_orig: no visible global function definition for ‘foreach’
plot1_orig: no visible global function definition for ‘ggplot’
plot1_orig: no visible global function definition for ‘aes_string’
plot1_orig: no visible global function definition for ‘geom_line’
plot1_orig: no visible global function definition for ‘labs’
plot2_dec: no visible global function definition for ‘%do%’
plot2_dec: no visible global function definition for ‘foreach’
plot2_dec: no visible global function definition for ‘ggplot’
plot2_dec: no visible global function definition for ‘aes_string’
plot2_dec: no visible global function definition for ‘geom_line’
plot2_dec: no visible global function definition for ‘facet_wrap’
plot2_dec: no visible global function definition for ‘labs’
plot2_orig: no visible global function definition for ‘%do%’
plot2_orig: no visible global function definition for ‘foreach’
plot2_orig: no visible global function definition for ‘ggplot’
plot2_orig: no visible global function definition for ‘aes_string’
plot2_orig: no visible global function definition for ‘geom_line’
plot2_orig: no visible global function definition for ‘labs’
plot3_dec: no visible global function definition for ‘%do%’
plot3_dec: no visible global function definition for ‘foreach’
plot3_dec: no visible global function definition for ‘ggplot’
plot3_dec: no visible global function definition for ‘aes_string’
plot3_dec: no visible global function definition for ‘geom_line’
plot3_dec: no visible global function definition for ‘facet_grid’
plot3_dec: no visible global function definition for ‘labs’
plot3_orig: no visible global function definition for ‘%do%’
plot3_orig: no visible global function definition for ‘foreach’
plot3_orig: no visible global function definition for ‘ggplot’
plot3_orig: no visible global function definition for ‘aes_string’
plot3_orig: no visible global function definition for ‘geom_line’
plot3_orig: no visible global function definition for ‘facet_wrap’
plot3_orig: no visible global function definition for ‘labs’
```
```
DONE
Status: 3 NOTEs
```

## granovaGG (1.3)
Maintainer: Brian A. Danielak <brian@briandk.com>

```
checking whether package ‘granovaGG’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘granovaGG’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘granovaGG’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/granovaGG.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘granovaGG-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: granovagg.ds
> ### Title: Elemental Graphic for Display of Dependent Sample Data
> ### Aliases: granovagg.ds
> 
> ### ** Examples
> 
> ### Using granovagg.ds to examine trends or effects for repeated measures data.
> 
> # This example corresponds to case 1b in Pruzek and Helmreich (2009). In this
> # graphic we're looking for the effect of treatment on patients with anorexia.
> 
> data(anorexia.sub)
> granovagg.ds(anorexia.sub,
+              revc = TRUE,
+              main = "Assessment Plot for weights to assess\
+                      Family Therapy treatment for Anorexia Patients",
+              xlab = "Weight after therapy (lbs.)",
+              ylab = "Weight before therapy (lbs.)"
+ )
                              Summary Statistics
n                                         17.000
Postwt mean                               90.494
Prewt mean                                83.229
mean(D = Postwt - Prewt)                   7.265
SD(D)                                      7.157
Effect Size                                1.015
r(Postwt, Prewt)                           0.538
r(Postwt + Prewt, D)                       0.546
Lower 95% Confidence Interval              3.585
Upper 95% Confidence Interval             10.945
t (D-bar)                                  4.185
df.t                                      16.000
p-value (t-statistic)                      0.001
Error: Unknown parameters: lpha
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## GraphPCA (1.0)
Maintainer: Brahim Brahim <brahim.brahim@bigdatavisualizations.com>

```
checking whether package ‘GraphPCA’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘GraphPCA’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/GraphPCA.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘GraphPCA-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: Visu
> ### Title: Visu
> ### Aliases: Visu
> 
> ### ** Examples
> 
> data(Hist1)
> data(Hist2)
> data(Hist3)
> data(Hist4)
> data(Hist5)
> data(Hist6)
> 
> 
> PC_example1=HistPCA(Variable=
+ list(Hist1,Hist2,Hist3,Hist4,
+ Hist5,Hist6),axes=c(1,2),
+ Row.names=paste('Year',
+ 70:82,sep='-'),Col.names=
+ c('mpg','cylinders', 
+ 'displacement',
+ 'horsepower',
+ 'weight',
+ 'acceleration'
+ ))$PCinterval
dev.new(): using pdf(file="Rplots8.pdf")
dev.new(): using pdf(file="Rplots9.pdf")
> 
> 
> Visu(PC_example1, axes=c(1,2),
+ Row.names=rownames(PC_example1))
Error: Unknown parameters: shape
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## greport (0.5-3)
Maintainer: Frank E Harrell Jr <f.harrell@vanderbilt.edu>

```
checking R code for possible problems ... NOTE
accrualReport: multiple local function definitions for ‘g’ with
  different formal arguments
```
```
DONE
Status: 1 NOTE
```

## gridDebug (0.4-0)
Maintainer: Paul Murrell <p.murrell@auckland.ac.nz>

```
checking whether package ‘gridDebug’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/gridDebug.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## gridExtra (2.0.0)
Maintainer: Baptiste Auguie <baptiste.auguie@gmail.com>

__OK__

## growcurves (0.2.3.9)
Maintainer: "terrance savitsky" <tds151@gmail.com>

```
checking whether package ‘growcurves’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/growcurves.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## growfunctions (0.11)
Maintainer: Terrance Savitsky <tds151@gmail.com>

```
checking whether package ‘growfunctions’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/growfunctions.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## gsDesign (2.9-3)
Maintainer: Keaven Anderson <keaven_anderson@merck.com>

```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  plot.binomialSPRT plot.gsBinomialExact plot.gsDesign
  plot.gsProbability plot.ssrCP print.eEvents print.gsBoundSummary
  print.gsDesign print.gsProbability print.gsSurv print.nSurv
  print.nSurvival summary.gsDesign summary.spendfn xtable.gsSurv
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
plotgsCP: no visible global function definition for ‘opts’
plotgsPower: no visible global function definition for ‘opts’
qplotit: no visible global function definition for ‘opts’
```
```
checking line endings in Makefiles ... NOTE
Found the following Makefile(s) without a final LF:
  inst/unitTests/Makefile
Some ‘make’ programs ignore lines not ending in LF.
```
```
DONE
Status: 3 NOTEs
```

## GSE (3.2.2)
Maintainer: Andy Leung <andy.leung@stat.ubc.ca>

```
checking whether package ‘GSE’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/GSE.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## hazus (0.1)
Maintainer: Gopi Goteti <my.ration.shop@gmail.com>

__OK__

## hierarchicalDS (2.9)
Maintainer: Paul B Conn <paul.conn@noaa.gov>

__OK__

## HighDimOut (1.0.0)
Maintainer: Cheng Fan <raja8885@hotmail.com>

__OK__

## HistData (0.7-5)
Maintainer: Michael Friendly <friendly@yorku.ca>

```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘Guerry’, ‘alr3’, ‘agridat’
```
```
DONE
Status: 1 NOTE
```

## HistDAWass (0.1.3)
Maintainer: Antonio Irpino <antonio.irpino@unina2.it>

```
checking whether package ‘HistDAWass’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘HistDAWass’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘HistDAWass’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/HistDAWass.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘HistDAWass-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot-HTS
> ### Title: Method plot for a histogram time series
> ### Aliases: plot,HTS-method plot-HTS
> 
> ### ** Examples
> 
> plot(subsetHTS(RetHTS,from=1,to=40)) #plots RetHTS dataset
Error: Unknown parameters: aplha
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## historydata (0.1)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/historydata/issues

```
checking examples ... ERROR
Running examples in ‘historydata-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: early_colleges
> ### Title: Early colleges in the United States
> ### Aliases: early_colleges
> ### Keywords: datasets
> 
> ### ** Examples
> 
> head(early_colleges)
                 college         original_name         city state established
1                Harvard                  <NA>    Cambridge    MA        1636
2       William and Mary                  <NA> Williamsburg    VA        1693
3                   Yale                  <NA>    New Haven    CT        1701
4 Pennsylvania, Univ. of                  <NA> Philadelphia    PA        1740
5              Princeton College of New Jersey    Princeton    NJ        1746
6               Columbia        King's College     New York    NY        1754
                           sponsorship
1 Congregational; after 1805 Unitarian
2                             Anglican
3                       Congregational
4                    Nondenominational
5                         Presbyterian
6                             Anglican
> if(require(ggplot2)) {
+   ggplot(early_colleges, aes(x = established)) + geom_bar(binwidth = 5) +
+   ggtitle("Founding Dates of Early American Colleges")
+ }
Loading required package: ggplot2
Error: Unknown parameters: binwidth
Execution halted
```
```
DONE
Status: 1 ERROR
```

## HiveR (0.2.44)
Maintainer: Bryan A. Hanson <hanson@depauw.edu>  
Bug reports: https://github.com/bryanhanson/HiveR/issues

__OK__

## HLMdiag (0.3.0)
Maintainer: Adam Loy <loyad01@gmail.com>

```
checking whether package ‘HLMdiag’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/HLMdiag.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## Hmisc (3.17-0)
Maintainer: Frank E Harrell Jr <f.harrell@vanderbilt.edu>

```
checking whether package ‘Hmisc’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/Hmisc.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## httk (1.3)
Maintainer: John Wambaugh <wambaugh.john@epa.gov>

__OK__

## hyperSpec (0.98-20150304)
Maintainer: Claudia Beleites <chemometrie@beleites.de>

```
checking whether package ‘hyperSpec’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘hyperSpec’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/hyperSpec.Rcheck/00install.out’ for details.
```
```
checking R code for possible problems ... NOTE
Warning: local assignments to syntactic functions: ~
Warning: local assignments to syntactic functions: ~
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## IAT (0.2)
Maintainer: Dan Martin <dpmartin42@gmail.com>

__OK__

## icd9 (1.3)
Maintainer: Jack O. Wasey <jack@jackwasey.com>  
Bug reports: https://github.com/jackwasey/icd9/issues

```
checking data for non-ASCII characters ... NOTE
  Note: found 14 marked Latin-1 strings
  Note: found 39 marked UTF-8 strings
```
```
DONE
Status: 1 NOTE
```

## idm (1.2)
Maintainer: Angelos Markos <amarkos@gmail.com>

__OK__

## ifaTools (0.6)
Maintainer: Joshua N. Pritikin <jpritikin@pobox.com>

__OK__

## iNEXT (2.0.5)
Maintainer: T. C. Hsieh <euler96@gmail.com>  
Bug reports: https://github.com/JohnsonHsieh/iNEXT/issues

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'ggiNEXT.Rd':
  ‘ggplot2’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
DONE
Status: 1 WARNING
```

## Information (0.0.5)
Maintainer: Larsen Kim <kblarsen4@gmail.com>

```
checking whether package ‘Information’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘Information’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘Information’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/Information.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## InformationValue (1.1.1)
Maintainer: Selva Prabhakaran <selva86@gmail.com>  
Bug reports: https://github.com/selva86/InformationValue/issues License:
        GPL (>= 2)

__OK__

## IntegratedJM (1.1)
Maintainer: Rudradev Sengupta <rudradev.sengupta@uhasselt.be>

```
checking package dependencies ... ERROR
Package required but not available: ‘Biobase’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## interplot (0.1.0.2)
Maintainer: Yue Hu <yue-hu-1@uiowa.edu>

__OK__

## intsvy (1.7)
Maintainer: Daniel Caro <daniel.caro@education.ox.ac.uk>  
Bug reports: https://github.com/eldafani/intsvy/issues

__OK__

## ITEMAN (1.0)
Maintainer: Cengiz Zopluoglu <c.zopluoglu@miami.edu>

__OK__

## JacobiEigen (0.1)
Maintainer: Bill Venables <Bill.Venables@gmail.com>

__OK__

## kdetrees (0.1.5)
Maintainer: Grady Weyenberg <grady.weyenberg@uky.edu>

__OK__

## kfigr (1.2)
Maintainer: Michael C Koohafkan <michael.koohafkan@gmail.com>  
Bug reports: https://github.com/mkoohafkan/kfigr/issues

__OK__

## Kmisc (0.5.0)
Maintainer: Kevin Ushey <kevinushey@gmail.com>  
Bug reports: https://github.com/kevinushey/Kmisc/issues

__OK__

## knitrBootstrap (0.9.0)
Maintainer: Jim Hester <james.f.hester@gmail.com>  
Bug reports: https://github.com/jimhester/knitrBootstrap/issues

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 44-48 (cars.Rmd) 
Error: processing vignette 'cars.Rmd' failed with diagnostics:
Unknown parameters: method, formula
Execution halted

```
```
DONE
Status: 2 NOTEs
```

## kobe (1.3.2)
Maintainer: Laurence Kell <laurie.kell@iccat.int>

```
checking whether package ‘kobe’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘kobe’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/kobe.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## Lahman (4.0-1)
Maintainer: Chris Dalzell <cdalzell@gmail.com>

```
checking installed package size ... NOTE
  installed size is  7.5Mb
  sub-directories of 1Mb or more:
    data   7.2Mb
```
```
DONE
Status: 1 NOTE
```

## LANDD (1.0.0)
Maintainer: Shangzhao Qiu <qsz1328@gmail.com>

```
checking package dependencies ... ERROR
Packages required but not available: ‘GOstats’ ‘GOSemSim’ ‘modeest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## latex2exp (0.3.3)
Maintainer: Stefano Meschiari <stefano.meschiari@gmail.com>  
Bug reports: https://github.com/stefano-meschiari/latex2exp/issues

__OK__

## lda (1.3.2)
Maintainer: Jonathan Chang <jonchang@fb.com>

```
checking whether package ‘lda’ can be installed ... WARNING
Found the following significant warnings:
  gibbs.c:26:1: warning: control may reach end of non-void function [-Wreturn-type]
Found the following additional warnings:
  gibbs.c:26:1: warning: control may reach end of non-void function [-Wreturn-type]
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/lda.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
checking top-level files ... NOTE
Non-standard files/directories found at top level:
  ‘DESCRIPTION.orig’ ‘DESCRIPTION.rej’
```
```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘penalized’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Missing or unexported object: ‘Matrix::xtabs’
```
```
checking R code for possible problems ... NOTE
slda.em : estimate.params: no visible global function definition for
  ‘penalized’
```
```
checking Rd line widths ... NOTE
Rd file 'nubbi.collapsed.gibbs.sampler.Rd':
  \usage lines wider than 90 characters:
     nubbi.collapsed.gibbs.sampler(contexts, pair.contexts, pairs, K.individual, K.pair, vocab, num.iterations, alpha, eta, xi)

Rd file 'rtm.collapsed.gibbs.sampler.Rd':
  \usage lines wider than 90 characters:
             alpha, eta, lambda = sum(sapply(links, length))/(length(links) * (length(links) -1)/2),

These lines will be truncated in the PDF manual.
```
```
DONE
Status: 1 WARNING, 5 NOTEs
```

## ldatuning (0.1.0)
Maintainer: Murzintcev Nikita <nikita@lreis.ac.cn>  
Bug reports: https://github.com/nikita-moor/ldatuning/issues

__OK__

## LDheatmap (0.99-1)
Maintainer: Brad McNeney <mcneney@sfu.ca>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘rtracklayer’ ‘GenomicRanges’ ‘chopsticks’
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘GenomicRanges’ ‘chopsticks’ ‘rtracklayer’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Package in Depends field not imported from: ‘grid’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
LDheatmap : makeImageRect: no visible global function definition for
  ‘rectGrob’
LDheatmap : makeImageRect: no visible global function definition for
  ‘gpar’
LDheatmap : makeImageText: no visible global function definition for
  ‘textGrob’
LDheatmap : makeImageText: no visible global function definition for
  ‘gpar’
LDheatmap : LDheatmap.Legend.add: no visible global function definition
  for ‘viewport’
LDheatmap : LDheatmap.Legend.add: no visible global function definition
  for ‘textGrob’
LDheatmap : LDheatmap.Legend.add: no visible global function definition
  for ‘gpar’
LDheatmap : LDheatmap.Legend.add: no visible global function definition
  for ‘segmentsGrob’
LDheatmap : LDheatmap.Legend.add: no visible global function definition
  for ‘linesGrob’
LDheatmap : LDheatmap.Legend.add: no visible global function definition
  for ‘gTree’
LDheatmap : LDheatmap.Legend.add: no visible global function definition
  for ‘gList’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘linesGrob’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘gpar’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘segmentsGrob’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘textGrob’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘gTree’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘gList’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘pointsGrob’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘convertWidth’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘grobWidth’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘editGrob’
LDheatmap : LDheatmap.Map.add: no visible global function definition
  for ‘unit’
LDheatmap: no visible global function definition for ‘ld.snp’
LDheatmap: no visible global function definition for ‘viewport’
LDheatmap: no visible global function definition for ‘unit’
LDheatmap: no visible global function definition for ‘grid.newpage’
LDheatmap: no visible global function definition for ‘textGrob’
LDheatmap: no visible global function definition for ‘gpar’
LDheatmap: no visible global function definition for ‘editGrob’
LDheatmap: no visible global function definition for ‘gTree’
LDheatmap: no visible global function definition for ‘gList’
LDheatmap: no visible global function definition for ‘grid.draw’
LDheatmap: no visible global function definition for ‘downViewport’
LDheatmap: no visible global function definition for ‘popViewport’
LDheatmap.addGenes: no visible global function definition for
  ‘pushViewport’
LDheatmap.addGenes: no visible global function definition for ‘vpStack’
LDheatmap.addGenes: no visible global function definition for
  ‘editGrob’
LDheatmap.addGenes: no visible global function definition for ‘addGrob’
LDheatmap.addGrob: no visible global function definition for ‘unit’
LDheatmap.addGrob: no visible global function definition for ‘vpStack’
LDheatmap.addGrob: no visible global function definition for ‘gTree’
LDheatmap.addGrob: no visible global function definition for ‘gList’
LDheatmap.addGrob: no visible global function definition for ‘addGrob’
LDheatmap.addRecombRate: no visible global function definition for
  ‘unit’
LDheatmap.addRecombRate: no visible global function definition for
  ‘pushViewport’
LDheatmap.addRecombRate: no visible global function definition for
  ‘vpStack’
LDheatmap.addRecombRate: no visible global function definition for
  ‘editGrob’
LDheatmap.addRecombRate: no visible global function definition for
  ‘addGrob’
LDheatmap.addScatterplot: no visible global function definition for
  ‘unit’
LDheatmap.addScatterplot: no visible global function definition for
  ‘linesGrob’
LDheatmap.addScatterplot: no visible global function definition for
  ‘yaxisGrob’
LDheatmap.addScatterplot: no visible global function definition for
  ‘gpar’
LDheatmap.addScatterplot: no visible global function definition for
  ‘textGrob’
LDheatmap.addScatterplot: no visible global function definition for
  ‘vpStack’
LDheatmap.addScatterplot: no visible global function definition for
  ‘gTree’
LDheatmap.addScatterplot: no visible global function definition for
  ‘gList’
LDheatmap.addScatterplot: no visible global function definition for
  ‘pointsGrob’
LDheatmap.addScatterplot: no visible global function definition for
  ‘addGrob’
LDheatmap.highlight: no visible global function definition for
  ‘current.vpTree’
LDheatmap.highlight: no visible global function definition for
  ‘seekViewport’
LDheatmap.highlight: no visible global function definition for
  ‘pushViewport’
LDheatmap.highlight: no visible global function definition for
  ‘polygonGrob’
LDheatmap.highlight: no visible global function definition for ‘gpar’
LDheatmap.highlight: no visible global function definition for
  ‘grid.draw’
LDheatmap.highlight: no visible global function definition for
  ‘upViewport’
LDheatmap.highlight: no visible global function definition for
  ‘popViewport’
LDheatmap.marks: no visible global function definition for ‘gpar’
LDheatmap.marks: no visible global function definition for
  ‘current.vpTree’
LDheatmap.marks: no visible global function definition for
  ‘seekViewport’
LDheatmap.marks: no visible global function definition for
  ‘pushViewport’
LDheatmap.marks: no visible global function definition for ‘pointsGrob’
LDheatmap.marks: no visible global function definition for ‘gTree’
LDheatmap.marks: no visible global function definition for ‘gList’
LDheatmap.marks: no visible global function definition for ‘grid.draw’
LDheatmap.marks: no visible global function definition for ‘upViewport’
LDheatmap.marks: no visible global function definition for
  ‘popViewport’
constructVP: no visible global function definition for ‘convertX’
constructVP: no visible global function definition for ‘getGrob’
constructVP: no visible global function definition for ‘viewport’
drawLDheatmapGrob: no visible global function definition for ‘convertY’
drawLDheatmapGrob: no visible global function definition for ‘getGrob’
drawLDheatmapGrob: no visible global function definition for ‘viewport’
drawLDheatmapGrob: no visible global function definition for ‘gpar’
drawLDheatmapGrob: no visible global function definition for
  ‘grid.newpage’
drawLDheatmapGrob: no visible global function definition for
  ‘pushViewport’
drawLDheatmapGrob: no visible global function definition for
  ‘grid.draw’
drawLDheatmapGrob: no visible global function definition for
  ‘popViewport’
moveTitles: no visible global function definition for ‘convertX’
moveTitles: no visible global function definition for ‘getGrob’
moveTitles: no visible global function definition for ‘editGrob’
moveTitles: no visible global function definition for ‘unit’
moveTitles: no visible global function definition for ‘grid.newpage’
moveTitles: no visible global function definition for ‘grid.draw’
moveTitles: no visible global function definition for ‘pushViewport’
moveTitles: no visible global function definition for
  ‘current.transform’
moveTitles: no visible global function definition for ‘upViewport’
moveTitles: no visible global function definition for ‘convertY’
moveTitles: no visible global function definition for ‘convertHeight’
moveTitles: no visible global function definition for ‘grobHeight’
plotGenes: no visible global function definition for ‘viewport’
plotGenes: no visible global function definition for ‘convertX’
plotGenes: no visible global function definition for ‘browserSession’
plotGenes: no visible global function definition for ‘genome<-’
plotGenes: no visible global function definition for ‘ucscTableQuery’
plotGenes: no visible global function definition for
  ‘GRangesForUCSCGenome’
plotGenes: no visible global function definition for ‘IRanges’
plotGenes: no visible global function definition for ‘getTable’
plotGenes: no visible global function definition for ‘tableNames’
plotGenes: no visible global function definition for ‘convertWidth’
plotGenes: no visible global function definition for ‘grobWidth’
plotGenes: no visible global function definition for ‘textGrob’
plotGenes: no visible global function definition for ‘gpar’
plotGenes: no visible global function definition for ‘unit’
plotGenes: no visible global function definition for ‘gTree’
plotGenes: no visible global function definition for ‘gList’
plotGenes: no visible global function definition for ‘linesGrob’
plotGenes: no visible global function definition for ‘segmentsGrob’
plotGenes: no visible global function definition for ‘polygonGrob’
plotGenes: no visible global function definition for ‘unit.c’
plotGenes: no visible global function definition for ‘addGrob’
plotGenes: no visible global function definition for ‘grid.draw’
postDrawDetails.ldheatmap: no visible global function definition for
  ‘popViewport’
postDrawDetails.symbols: no visible global function definition for
  ‘popViewport’
preDrawDetails.ldheatmap: no visible global function definition for
  ‘convertX’
preDrawDetails.ldheatmap: no visible global function definition for
  ‘unit’
preDrawDetails.ldheatmap: no visible global function definition for
  ‘rectGrob’
preDrawDetails.ldheatmap: no visible global function definition for
  ‘pushViewport’
preDrawDetails.ldheatmap: no visible global function definition for
  ‘viewport’
preDrawDetails.ldheatmap: no visible global function definition for
  ‘gpar’
preDrawDetails.symbols: no visible global function definition for
  ‘convertX’
preDrawDetails.symbols: no visible global function definition for
  ‘unit’
preDrawDetails.symbols: no visible global function definition for
  ‘rectGrob’
preDrawDetails.symbols: no visible global function definition for
  ‘pushViewport’
preDrawDetails.symbols: no visible global function definition for
  ‘viewport’
preDrawDetails.symbols: no visible global function definition for
  ‘gpar’
recombRate: no visible global function definition for ‘viewport’
recombRate: no visible global function definition for ‘convertX’
recombRate: no visible global function definition for ‘browserSession’
recombRate: no visible global function definition for ‘genome<-’
recombRate: no visible global function definition for ‘ucscTableQuery’
recombRate: no visible global function definition for
  ‘GRangesForUCSCGenome’
recombRate: no visible global function definition for ‘IRanges’
recombRate: no visible global function definition for ‘getTable’
recombRate: no visible global function definition for ‘textGrob’
recombRate: no visible global function definition for ‘gpar’
recombRate: no visible global function definition for ‘unit’
recombRate: no visible global function definition for ‘editGrob’
recombRate: no visible global function definition for ‘convertWidth’
recombRate: no visible global function definition for ‘grobWidth’
recombRate: no visible global function definition for ‘gTree’
recombRate: no visible global function definition for ‘gList’
recombRate: no visible global function definition for ‘rectGrob’
recombRate: no visible global function definition for ‘addGrob’
recombRate: no visible global function definition for ‘grid.draw’
```
```
checking examples ... ERROR
Running examples in ‘LDheatmap-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: GIMAP5.CEU
> ### Title: Example data set for LDheatmap
> ### Aliases: GIMAP5.CEU
> ### Keywords: datasets
> 
> ### ** Examples
>  
> data(GIMAP5.CEU) 
> LDheatmap(GIMAP5.CEU$snp.data,GIMAP5.CEU$snp.support$Position)
Loading required package: chopsticks
Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
  there is no package called ‘chopsticks’
Error in LDheatmap(GIMAP5.CEU$snp.data, GIMAP5.CEU$snp.support$Position) : 
  could not find function "ld.snp"
Execution halted
```
```
checking running R code from vignettes ... WARNING
Errors in running code in vignettes:
when running code in ‘addTracks.Rnw’
  ...

> ll <- LDheatmap(GIMAP5.CEU$snp.data, GIMAP5.CEU$snp.support$Position, 
+     flip = TRUE)
Loading required package: chopsticks
Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
  there is no package called ‘chopsticks’

  When sourcing ‘addTracks.R’:
Error: could not find function "ld.snp"
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: grid
Loading required package: chopsticks
Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
  there is no package called ‘chopsticks’

Error: processing vignette 'addTracks.Rnw' failed with diagnostics:
 chunk 3 (label = fig1com) 
Error in LDheatmap(GIMAP5.CEU$snp.data, GIMAP5.CEU$snp.support$Position,  : 
  could not find function "ld.snp"
Execution halted

```
```
DONE
Status: 1 ERROR, 1 WARNING, 4 NOTEs
```

## learnstats (0.1.1)
Maintainer: Daniel Walter <dswalter@gmail.com>

__OK__

## likeLTD (5.5.0)
Maintainer: Christopher Steele <c.steele.11@ucl.ac.uk>

__OK__

## likert (1.2)
Maintainer: Jason Bryer <jason@bryer.org>  
Bug reports: https://github.com/jbryer/likert/issues

```
checking whether package ‘likert’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘likert’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘likert’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/likert.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘shiny’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
likert.bar.plot: warning in scale_y_continuous(label = abs_formatter,
  limits = c(ymin - ybuffer, ymax + ybuffer)): partial argument match
  of 'label' to 'labels'
likert.histogram.plot: warning in scale_y_continuous(label =
  abs_formatter): partial argument match of 'label' to 'labels'
```
```
checking data for non-ASCII characters ... NOTE
  Note: found 7 marked UTF-8 strings
```
```
DONE
Status: 1 WARNING, 4 NOTEs
```

## llama (0.8.1)
Maintainer: Lars Kotthoff <lars.kotthoff@insight-centre.org>

__OK__

## lme4 (1.1-10)
Maintainer: Ben Bolker <bbolker+lme4@gmail.com>  
Bug reports: https://github.com/lme4/lme4/issues

```
checking installed package size ... NOTE
  installed size is  5.9Mb
  sub-directories of 1Mb or more:
    doc        1.8Mb
    testdata   1.5Mb
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Computing bootstrap confidence intervals ...
Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  : 
  Running 'texi2dvi' on 'lmer.tex' failed.
LaTeX errors:
! Undefined control sequence.
l.15 x<9c><ed>\Mh
           ^^TK^^P<ae><97>(q$<ee>؈!.L6^^L/($oYX^^Hx^^H<eb>!<a0>'o<de><<eb>ٓ^^A/<a2>O<f0>]ċ^^G<f5>^^]<84><e7>A...
The control sequence at the end of the top line
of your error message was never \def'ed. If you have
! LaTeX Error: Missing \begin{document}.

See the LaTeX manual or LaTeX Companion for explanation.
Type  H <return>  for immediate help.
 ...                                              
! Missing $ inserted.
<inserted text> 
                $
l.16 ...F<a0><ba>v<ed>Zggg<ad>V{<fd><fa>5^^Q9<8e>388866<a6><94><8a><e3>^^X<fd>^^]<c7><c1><83><ae>^
                                                  <bd><9a>J<a5>l<db>v^^\^^G<9f>J<a9><87>^^O^^_<c6>q|...
! Extra }, or forgotten $.
l.17 Ԓ<da>[}
          <b8>ӄ<f6><a0><9c>0^^L<89>(<9b><cd><e2>^^A<98><ce>0^^L<85>^^BOm<b9>\^^^^^Z^^Z<8a><a2><88><a7>?<9b><cd>"@a<ce>0<8e>eYP{<a1>P<c0>O<<c8><e3>...
I've deleted a group-closing 
Calls: buildVignettes -> texi2pdf -> texi2dvi
Execution halted

```
```
DONE
Status: 2 NOTEs
```

## lmerTest (2.0-29)
Maintainer: Alexandra Kuznetsova <alku@dtu.dk>

__OK__

## lmms (1.3)
Maintainer: Jasmin Straube <j.straube@qfab.org>

__OK__

## localgauss (0.34)
Maintainer: Tore Selland Kleppe <tore.kleppe@uis.no>

```
checking whether package ‘localgauss’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/localgauss.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## LocFDRPois (1.0.0)
Maintainer: Kris Sankaran <kriss1@stanford.edu>

__OK__

## logisticPCA (0.1)
Maintainer: Andrew J. Landgraf <andland@gmail.com>

__OK__

## LOGIT (1.1)
Maintainer: Rafael S. de Souza <rafael.2706@gmail.com>

```
checking examples ... ERROR
Running examples in ‘LOGIT-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ROCtest
> ### Title: Display ROC curve and related AUC statistic, or
> ###   sensitivity-specificity plot of glm with binomial family.
> ### Aliases: ROCtest
> ### Keywords: models
> 
> ### ** Examples
> 
> library(MASS)
>  library(LOGIT)
>  data(R84)
>  R84$cage <- R84$age - mean(R84$age)
>  R84$cdoc <- R84$docvis - mean(R84$docvis)
>  mylogit <- glm(outwork ~ cdoc + female + kids + cage + factor(edlevel),
+  family=binomial, data=R84)
>  summary(mylogit)

Call:
glm(formula = outwork ~ cdoc + female + kids + cage + factor(edlevel), 
    family = binomial, data = R84)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3615  -0.6807  -0.4320   0.8541   2.8220  

Coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -2.022806   0.085790 -23.579  < 2e-16 ***
cdoc              0.024480   0.006255   3.914 9.08e-05 ***
female            2.268467   0.084039  26.993  < 2e-16 ***
kids              0.373053   0.090586   4.118 3.82e-05 ***
cage              0.054355   0.004207  12.921  < 2e-16 ***
factor(edlevel)2  0.009293   0.173172   0.054 0.957201    
factor(edlevel)3  0.478016   0.153786   3.108 0.001882 ** 
factor(edlevel)4 -0.830282   0.214336  -3.874 0.000107 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 5091.1  on 3873  degrees of freedom
Residual deviance: 3889.9  on 3866  degrees of freedom
AIC: 3905.9

Number of Fisher Scoring iterations: 4

>  ROCtest(mylogit, fold=10, type="Sensitivity")
Error: Unknown parameters: name
Execution halted
```
```
DONE
Status: 1 ERROR
```

## lsbclust (1.0.3)
Maintainer: Pieter Schoonees <schoonees@gmail.com>

```
checking whether package ‘lsbclust’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘lsbclust’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘lsbclust’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/lsbclust.Rcheck/00install.out’ for details.
```
```
checking R code for possible problems ... NOTE
plot.ovl.kmeans: possible error in position_dodge(height = 1): unused
  argument (height = 1)
```
```
checking examples ... ERROR
Running examples in ‘lsbclust-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot.col.kmeans
> ### Title: Plot method for class 'col.kmeans'
> ### Aliases: plot.col.kmeans
> ### Keywords: hplot
> 
> ### ** Examples
> 
> data("dcars")
> m <- orc.lsbclust(data = dcars, margin = 3, delta = c(1,1,1,1), nclust = 5, type = "columns")
K-means on column margins...	DONE
> plot(m)
$plot1
Error: geom_vline requires the following missing aesthetics: xintercept
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE
```

## lsl (0.5.0)
Maintainer: Po-Hsien Huang <psyphh@gmail.com>

```
checking examples ... ERROR
Running examples in ‘lsl-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: lslSEM-class
> ### Title: A Reference Class for Learning a SEM model via penalized
> ###   likelihood.
> ### Aliases: lslSEM lslSEM-class
> 
> ### ** Examples
> 
> #create a P x M population factor loading matrix#
> Ld0 <- diag(1, 9) %x% matrix(c(.8,.75,.8), 3, 1)
> 
> #create a M x M population path coefficients matrix#
> Bt0 <- matrix(0, 9, 9)
> Bt0[2, 1] = Bt0[3, 2] = Bt0[5, 4] = Bt0[6, 5] = Bt0[8, 7] = Bt0[9, 8] =.45
> Bt0[4, 1] = Bt0[5, 2] = Bt0[6, 3] = Bt0[7, 4] = Bt0[8, 5] = Bt0[9, 6]= .55
> Bt0iv <- solve(diag(1, 9) - Bt0)
> 
> #create a M x M population residual covariance matrix#
> Ph0 <- diag(0, 9)
> Ph0[1, 1] <- 1
> for (m in 2:9) {Ph0[m, m] <- (1 - sum((Bt0iv[m,] ^ 2) * diag(Ph0)))}
> 
> #create a P x P population measurement error matrix#
> Ps0 <- diag(c(.36, 0.4375, .36), 27)
> #create a P x M population covariance matrix#
> Sg0 <- Ld0 %*% Bt0iv %*% Ph0 %*% t(Bt0iv) %*% t(Ld0) + Ps0
> 
> #create a P x M pattern matrix for factor loadings#
> Ldp <- (Ld0 != 0)
> Ldp[1, 1] = Ldp[4, 2] = Ldp[7, 3] = Ldp[10, 4] = Ldp[13, 5] = 0
> Ldp[16, 6] = Ldp[19, 7] = Ldp[22, 8] = Ldp[25, 9] = 0
> 
> #create a P x M pattern matrix for path coefficients#
> Btp <- matrix(0, 9, 9)
> Btp[lower.tri(Btp)] <- -1
> 
> #specify field pattern, value, and penalty#
> pattern <- list(Ldp = Ldp, Btp = Btp)
> value <- list(Ld = Ld0, Bt = Bt0)
> penalty <- list(type = "mcp", gm_all = seq(0.025, .15, .025), dt_all = 2)
> 
> #generate data with N = 400 and P = 27#
> Z <- matrix(rnorm(400 * 27, 0, 1), 400, 27)
> Y <- Z %*% eigen(Sg0)$vectors %*% diag(sqrt(eigen(Sg0)$values)) %*% t(eigen(Sg0)$vectors)
> Y <- as.data.frame(Y)
> 
> #create lslSEM object#
> rc_sem <- lsl:::lslSEM(data = Y, pattern = pattern, value = value, penalty = penalty)
> 
> #check the specification through method check()#
> rc_sem$check()
***check for the field data***
field data is OK
***check for the field pattern***
warning: pattern$Psp is not specified and its value is substituted by the default.
warning: pattern$Php is not specified and its value is substituted by the default.
warning: pattern$nup is not specified and its value is substituted by the default.
warning: pattern$app is not specified and its value is substituted by the default.
4 warning(s) or error(s) is generated when checking the field pattern.
***check for the field value***
warning: value$Ps is not specified and its value is substituted by the default.
warning: value$Ph is not specified and its value is substituted by the default.
warning: value$nu is not specified and its value is substituted by the default.
warning: value$ap is not specified and its value is substituted by the default.
4 warning(s) or error(s) is generated when checking the field value.
***check for the field penalty***
field penalty is OK
***check for the field control***
warning: control$itmax is not specified and its value is substituted by the default.
warning: control$eps is not specified and its value is substituted by the default.
2 warning(s) or error(s) is generated when checking the field control.
> 
> #obtain the estimates under each pair of gamma and dt through method learn()#
> rc_sem$learn()
> 
> #obtain the final model based on bic through method fit()#
> rc_sem$fit(criterion = "bic")
> 
> #see overall model information and fit indices of final model#
> rc_sem$fit_summary
                                     value
gamma                           0.15000000
delta                           2.00000000
penalized ml discrepancy        1.15001857
ml discrepancy                  0.88000944
number of non-zero parameters  93.00000000
degree of freedom             312.00000000
number of iterations           33.00000000
lrt statistic                 352.00377530
srmr                            0.03239601
rmsea                           0.01790372
centrality index                0.95122494
gamma hat                       0.99264637
cfi                             0.99282283
nnfi                            0.99192569
bl89                            0.99287270
rni                             0.99282283
aic                             1.34500944
bic                             2.27302495
> 
> #see estimated Bt of final model#
> rc_sem$fit_value$Bt
           [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
 [1,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [2,] 0.5431272 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [3,] 0.0000000 0.4004454 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [4,] 0.5503286 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000
 [5,] 0.0000000 0.5120892 0.0000000 0.3776610 0.0000000 0.0000000 0.0000000
 [6,] 0.0000000 0.0000000 0.6449089 0.0000000 0.5520001 0.0000000 0.0000000
 [7,] 0.0000000 0.0000000 0.0000000 0.5782412 0.0000000 0.0000000 0.0000000
 [8,] 0.0000000 0.0000000 0.0000000 0.0000000 0.6319933 0.0000000 0.3981297
 [9,] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.5602191 0.0000000
           [,8] [,9]
 [1,] 0.0000000    0
 [2,] 0.0000000    0
 [3,] 0.0000000    0
 [4,] 0.0000000    0
 [5,] 0.0000000    0
 [6,] 0.0000000    0
 [7,] 0.0000000    0
 [8,] 0.0000000    0
 [9,] 0.4580425    0
> 
> #see the solution path parameters in Bt#
> rc_sem$plot_path(mat_name = "Bt")
Error in paste("delta = ", as.character(value), sep = "") : 
  argument "value" is missing, with no default
Calls: <Anonymous> ... facet_strips.grid -> build_strip -> lapply -> labeller -> paste
Execution halted
```
```
DONE
Status: 1 ERROR
```

## ltbayes (0.3)
Maintainer: Timothy R. Johnson <trjohns@uidaho.edu>

```
checking whether package ‘ltbayes’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ltbayes.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## MAc (1.1)
Maintainer: AC Del Re <acdelre@gmail.com>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘R2wd’

Package which this enhances but not available for checking: ‘irr’
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘R2wd’ ‘ggplot2’ ‘metafor’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  mareg.default print.icclist print.macat print.mareg print.omni
  print.summary.mareg r2.mareg summary.mareg wd.default wd.macat
  wd.mareg wd.omni
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
CatModf: warning in pchisq(out$Q, out$df, lower = FALSE): partial
  argument match of 'lower' to 'lower.tail'
CatModr: warning in pchisq(out$Q, out$df, lower = FALSE): partial
  argument match of 'lower' to 'lower.tail'
OmnibusES: warning in pchisq(Q, df, lower = FALSE): partial argument
  match of 'lower' to 'lower.tail'
macat: warning in pchisq(temp$Q, temp$df, lower = FALSE): partial
  argument match of 'lower' to 'lower.tail'
macat: warning in pchisq(out$Q, out$df, lower = FALSE): partial
  argument match of 'lower' to 'lower.tail'
omni: warning in pchisq(Q, df, lower = FALSE): partial argument match
  of 'lower' to 'lower.tail'
CatModGraph: no visible global function definition for ‘ggplot’
CatModGraph: no visible global function definition for ‘aes’
CatModGraph: no visible binding for global variable ‘z’
CatModGraph: no visible binding for global variable ‘wi’
CatModGraph: no visible global function definition for ‘geom_boxplot’
CatModGraph: no visible global function definition for ‘geom_jitter’
CatModGraph: no visible binding for global variable ‘wi.tau’
CatModGraph: no visible global function definition for ‘xlab’
CatModGraph: no visible global function definition for ‘ylab’
CatModGraph: no visible global function definition for ‘opts’
ForestPlot: no visible global function definition for ‘ggplot’
ForestPlot: no visible global function definition for ‘aes’
ForestPlot: no visible binding for global variable ‘id’
ForestPlot: no visible binding for global variable ‘r’
ForestPlot: no visible global function definition for ‘geom_vline’
ForestPlot: no visible global function definition for ‘geom_point’
ForestPlot: no visible global function definition for ‘opts’
ForestPlot: no visible global function definition for ‘geom_errorbarh’
ForestPlot: no visible binding for global variable ‘l.ci95’
ForestPlot: no visible binding for global variable ‘u.ci95’
ForestPlot: no visible global function definition for ‘xlim’
ForestPlot: no visible global function definition for ‘xlab’
ForestPlot: no visible global function definition for ‘ylab’
FunnelPlot: no visible global function definition for ‘ggplot’
FunnelPlot: no visible global function definition for ‘aes’
FunnelPlot: no visible binding for global variable ‘se.z’
FunnelPlot: no visible binding for global variable ‘z’
FunnelPlot: no visible global function definition for ‘geom_vline’
FunnelPlot: no visible global function definition for ‘opts’
FunnelPlot: no visible global function definition for ‘xlim’
FunnelPlot: no visible global function definition for ‘ylim’
FunnelPlot: no visible global function definition for ‘xlab’
FunnelPlot: no visible global function definition for ‘ylab’
FunnelPlot: no visible global function definition for ‘stat_abline’
FunnelPlot: no visible global function definition for
  ‘scale_y_continuous’
FunnelPlot: no visible binding for global variable ‘se.z.tau’
MAregGraph: no visible global function definition for ‘ggplot’
MAregGraph: no visible global function definition for ‘aes’
MAregGraph: no visible binding for global variable ‘z’
MAregGraph: no visible binding for global variable ‘wi’
MAregGraph: no visible global function definition for ‘geom_point’
MAregGraph: no visible global function definition for ‘geom_smooth’
MAregGraph: no visible global function definition for ‘xlab’
MAregGraph: no visible global function definition for ‘ylab’
MAregGraph: no visible global function definition for ‘opts’
MAregGraph: no visible binding for global variable ‘wi.tau’
MultiModGraph: no visible global function definition for ‘ggplot’
MultiModGraph: no visible global function definition for ‘aes’
MultiModGraph: no visible binding for global variable ‘z’
MultiModGraph: no visible binding for global variable ‘wi’
MultiModGraph: no visible global function definition for ‘opts’
MultiModGraph: no visible global function definition for ‘facet_wrap’
MultiModGraph: no visible global function definition for ‘geom_point’
MultiModGraph: no visible global function definition for ‘geom_smooth’
MultiModGraph: no visible global function definition for ‘ylab’
MultiModGraph: no visible global function definition for ‘xlab’
MultiModGraph: no visible binding for global variable ‘wi.tau’
mareg.default: no visible global function definition for ‘rma’
omni: no visible binding for global variable ‘g’
omni: no visible binding for global variable ‘var.g’
plotcat: no visible global function definition for ‘ggplot’
plotcat: no visible global function definition for ‘aes’
plotcat: no visible binding for global variable ‘wi’
plotcat: no visible global function definition for ‘geom_boxplot’
plotcat: no visible global function definition for ‘geom_jitter’
plotcat: no visible binding for global variable ‘wi.tau’
plotcat: no visible global function definition for ‘xlab’
plotcat: no visible global function definition for ‘ylab’
plotcat: no visible global function definition for ‘opts’
plotcon: no visible global function definition for ‘ggplot’
plotcon: no visible global function definition for ‘aes’
plotcon: no visible binding for global variable ‘wi’
plotcon: no visible global function definition for ‘geom_point’
plotcon: no visible global function definition for ‘geom_smooth’
plotcon: no visible global function definition for ‘xlab’
plotcon: no visible global function definition for ‘ylab’
plotcon: no visible global function definition for ‘opts’
plotcon: no visible binding for global variable ‘wi.tau’
stat_sum_single1: no visible binding for global variable ‘wi’
stat_sum_single1: no visible global function definition for
  ‘stat_summary’
stat_sum_single2: no visible binding for global variable ‘wi.tau’
stat_sum_single2: no visible global function definition for
  ‘stat_summary’
wd.default: no visible global function definition for ‘wdGet’
wd.default: no visible global function definition for ‘wdNewDoc’
wd.default: no visible global function definition for ‘wdHeading’
wd.default: no visible global function definition for ‘wdTable’
wd.macat: no visible global function definition for ‘wdGet’
wd.macat: no visible global function definition for ‘wdNewDoc’
wd.macat: no visible global function definition for ‘wdHeading’
wd.macat: no visible global function definition for ‘wdTable’
wd.mareg: no visible global function definition for ‘wdGet’
wd.mareg: no visible global function definition for ‘wdNewDoc’
wd.mareg: no visible global function definition for ‘wdHeading’
wd.mareg: no visible global function definition for ‘wdTable’
wd.omni: no visible global function definition for ‘wdGet’
wd.omni: no visible global function definition for ‘wdNewDoc’
wd.omni: no visible global function definition for ‘wdHeading’
wd.omni: no visible global function definition for ‘wdTable’
```
```
DONE
Status: 4 NOTEs
```

## MAd (0.8-2)
Maintainer: AC Del Re <acdelre@gmail.com>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘R2wd’

Package which this enhances but not available for checking: ‘irr’
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘R2wd’ ‘ggplot2’ ‘metafor’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  mareg.default print.icclist print.macat print.mareg print.omni
  print.summary.mareg r2.mareg summary.mareg wd.default wd.macat
  wd.mareg wd.omni
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 3 NOTEs
```

## mapDK (0.3.0)
Maintainer: Sebastian Barfort <sebastianbarfort@gmail.com>

__OK__

## marked (1.1.10)
Maintainer: Jeff Laake <Jeff.Laake@noaa.gov>

```
checking whether package ‘marked’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/marked.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## marketeR (0.1.0)
Maintainer: Felix MIKAELIAN <felix.mikaelian@essec.edu>  
Bug reports: https://github.com/fmikaelian/marketeR/issues

```
checking whether package ‘marketeR’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘marketeR’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘marketeR’
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘marketeR’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/marketeR.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## marmap (0.9.3)
Maintainer: Eric Pante <pante.eric@gmail.com>

__OK__

## matrixStats (0.14.2)
Maintainer: Henrik Bengtsson <henrikb@braju.com>  
Bug reports: https://github.com/HenrikBengtsson/matrixStats/issues

__OK__

## MAVIS (1.1.1)
Maintainer: William Kyle Hamilton <kyle.hamilton@gmail.com>

__OK__

## MaxentVariableSelection (1.0-0)
Maintainer: "Alexander Jueterbock" <Alexander-Jueterbock@web.de>

__OK__

## MCMC.OTU (1.0.8)
Maintainer: Mikhail V. Matz <matz@utexas.edu>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking R code for possible problems ... NOTE
otuByAutocorr: no visible global function definition for ‘autocorr’
```
```
DONE
Status: 2 NOTEs
```

## MCMC.qpcr (1.2)
Maintainer: Mikhail V. Matz <matz@utexas.edu>

__OK__

## mcprofile (0.2-1)
Maintainer: Daniel Gerhard <00gerhard@gmail.com>

__OK__

## MergeGUI (0.2-1)
Maintainer: Xiaoyue Cheng <xycheng@iastate.edu>

```
checking R code for possible problems ... NOTE
MergeGUI : mergefunc : undo: no visible global function definition for
  ‘gmessage’
MergeGUI : mergefunc : redo: no visible global function definition for
  ‘gmessage’
MergeGUI : mergefunc : VariableOptions: no visible global function
  definition for ‘gwindow’
MergeGUI : mergefunc : VariableOptions: no visible global function
  definition for ‘ggroup’
MergeGUI : mergefunc : VariableOptions: no visible global function
  definition for ‘glabel’
MergeGUI : mergefunc : VariableOptions: no visible global function
  definition for ‘gedit’
MergeGUI : mergefunc : VariableOptions: no visible global function
  definition for ‘gcombobox’
MergeGUI : mergefunc : VariableOptions: no visible global function
  definition for ‘gbutton’
MergeGUI : mergefunc : VariableOptions : <anonymous>: no visible global
  function definition for ‘gmessage’
MergeGUI : mergefunc : smmry: no visible global function definition for
  ‘gmessage’
MergeGUI : mergefunc : smmry: no visible global function definition for
  ‘ggroup’
MergeGUI : mergefunc : smmry: no visible global function definition for
  ‘glayout’
MergeGUI : mergefunc : smmry: no visible global function definition for
  ‘glabel’
MergeGUI : mergefunc : smmry: no visible global function definition for
  ‘gtable’
MergeGUI : mergefunc : graph: no visible global function definition for
  ‘ggroup’
MergeGUI : mergefunc : graph: no visible global function definition for
  ‘glayout’
MergeGUI : mergefunc : graph: no visible global function definition for
  ‘gmessage’
MergeGUI : mergefunc : graph: no visible global function definition for
  ‘ggraphics’
MergeGUI : mergefunc : graph: no visible global function definition for
  ‘ggpcp’
MergeGUI : mergefunc : dict: no visible global function definition for
  ‘ggroup’
MergeGUI : mergefunc : dict: no visible global function definition for
  ‘glayout’
MergeGUI : mergefunc : dict: no visible global function definition for
  ‘gtext’
MergeGUI : mergefunc : dict: no visible global function definition for
  ‘gmessage’
MergeGUI : mergefunc : changetest: no visible global function
  definition for ‘gtable’
MergeGUI : mergefunc : changetest: no visible global function
  definition for ‘gmessage’
MergeGUI : mergefunc : changematching: no visible global function
  definition for ‘gmessage’
MergeGUI : mergefunc : watchdatafunc: no visible global function
  definition for ‘gmessage’
MergeGUI : mergefunc : watchdatafunc: no visible global function
  definition for ‘gfile’
MergeGUI : mergefunc: no visible global function definition for
  ‘gwindow’
MergeGUI : mergefunc: no visible global function definition for
  ‘gnotebook’
MergeGUI : mergefunc: no visible global function definition for
  ‘ggroup’
MergeGUI : mergefunc: no visible global function definition for
  ‘gframe’
MergeGUI : mergefunc: no visible global function definition for
  ‘gradio’
MergeGUI : mergefunc: no visible global function definition for
  ‘glabel’
MergeGUI : mergefunc: no visible global function definition for ‘gedit’
MergeGUI : mergefunc: no visible global function definition for
  ‘gcheckboxgroup’
MergeGUI : mergefunc: no visible global function definition for
  ‘gtable’
MergeGUI : mergefunc: no visible global function definition for
  ‘addHandlerKeystroke’
MergeGUI : mergefunc: no visible global function definition for
  ‘gbutton’
MergeGUI : mergeID : watchIDfunc: no visible global function definition
  for ‘gmessage’
MergeGUI : mergeID : watchIDfunc: no visible global function definition
  for ‘gfile’
MergeGUI : mergeID: no visible global function definition for ‘gwindow’
MergeGUI : mergeID: no visible global function definition for ‘ggroup’
MergeGUI : mergeID: no visible global function definition for ‘glabel’
MergeGUI : mergeID: no visible global function definition for
  ‘gcombobox’
MergeGUI : mergeID: no visible global function definition for ‘gbutton’
MergeGUI: no visible global function definition for ‘gwindow’
MergeGUI: no visible global function definition for ‘ggroup’
MergeGUI: no visible global function definition for ‘gtable’
MergeGUI: no visible global function definition for ‘gbutton’
MergeGUI : <anonymous>: no visible global function definition for
  ‘gfile’
```
```
DONE
Status: 1 NOTE
```

## merTools (0.1.0)
Maintainer: Jared E. Knowles <jknowles@gmail.com>

__OK__

## metagen (1.0)
Maintainer: Thomas W. D. Möbius <kontakt@thomasmoebius.de>

__OK__

## metaMix (0.2)
Maintainer: Sofia Morfopoulou <sofia.morfopoulou.10@ucl.ac.uk>

```
checking package dependencies ... ERROR
Package required but not available: ‘Rmpi’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## meteogRam (1.0)
Maintainer: Bogdan Bochenek <bogdan.bochenek@uj.edu.pl>

__OK__

## Methplot (1.0)
Maintainer: Xin Yang <xin.yang@cimr.cam.ac.uk>

```
checking whether package ‘Methplot’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘Methplot’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘Methplot’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/Methplot.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## metricsgraphics (0.8.5)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/metricsgraphics/issues

```
checking examples ... ERROR
Running examples in ‘metricsgraphics-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: mjs_histogram
> ### Title: Plot Histograms with MetrisGraphics
> ### Aliases: mjs_histogram
> 
> ### ** Examples
> 
> movies <- ggplot2::movies[sample(nrow(ggplot2::movies), 1000), ]
Error: 'movies' is not an exported object from 'namespace:ggplot2'
Execution halted
```
```
checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  R is a collaborative project with many contributors.
  Type 'contributors()' for more information and
  'citation()' on how to cite R or R packages in publications.
  
  Type 'demo()' for some demos, 'help()' for on-line help, or
  'help.start()' for an HTML browser interface to help.
  Type 'q()' to quit R.
  
  > library(testthat)
  > test_check("metricsgraphics")
  Loading required package: metricsgraphics
  Error: 'movies' is not an exported object from 'namespace:ggplot2'
  Execution halted
```
```
DONE
Status: 2 ERRORs
```

## microbenchmark (1.4-2)
Maintainer: Olaf Mersmann <olafm@p-value.net>

```
checking for GNU extensions in Makefiles ... WARNING
Found the following file(s) containing GNU extensions:
  src/Makevars
Portable Makefiles do not use GNU extensions such as +=, :=, $(shell),
$(wildcard), ifeq ... endif. See section ‘Writing portable packages’ in
the ‘Writing R Extensions’ manual.
```
```
DONE
Status: 1 WARNING
```

## micromap (1.9.2)
Maintainer: Tom Kincaid <Kincaid.Tom@epa.gov>

```
checking whether package ‘micromap’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘sp::nowrapSpatialLines’ when loading ‘micromap’
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘micromap’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘micromap’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/micromap.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## MissingDataGUI (0.2-2)
Maintainer: Xiaoyue Cheng <xycheng@iastate.edu>

```
checking whether package ‘MissingDataGUI’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘MissingDataGUI’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘MissingDataGUI’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/MissingDataGUI.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## mistat (1.0-3)
Maintainer: Daniele Amberti <daniele.amberti@gmail.com>

__OK__

## MIXFIM (1.0)
Maintainer: Marie-Karelle Riviere-Jourdan <eldamjh@gmail.com>

__OK__

## mixOmics (5.1.2)
Maintainer: Kim-Anh Le Cao <k.lecao@uq.edu.au>  
Bug reports: mixomics@math.univ-toulouse.fr or
        https://bitbucket.org/klecao/package-mixomics/issues

```
checking examples ... ERROR
Running examples in ‘mixOmics-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plotIndiv
> ### Title: Plot of Individuals (Experimental Units)
> ### Aliases: plotIndiv
> ### Keywords: multivariate hplot dplot
> 
> ### ** Examples
> 
> ## plot of individuals for objects of class 'rcc' 
> # ----------------------------------------------------
> data(nutrimouse)
> X <- nutrimouse$lipid
> Y <- nutrimouse$gene
> nutri.res <- rcc(X, Y, ncomp = 3, lambda1 = 0.064, lambda2 = 0.008)
> 
> # default, only in the X space
> plotIndiv(nutri.res) 
> #changing the colors with argument col and ellipse will be plotted according to the color
> plotIndiv(nutri.res, col= as.numeric(nutrimouse$diet), plot.ellipse = TRUE)
> 
> # or we can specify the argument group for plotting the ellipse according to group
> plotIndiv(nutri.res, col= as.numeric(nutrimouse$diet), 
+           plot.ellipse = TRUE, group = nutrimouse$genotype)
> 
> 
> # plotting the samples in the XY space, with names indicating genotype
> plotIndiv(nutri.res, rep.space= 'XY-variate', plot.ellipse = TRUE, ellipse.level = 0.9, 
+           group = nutrimouse$genotype, ind.names = nutrimouse$genotype)
> 
> # ellipse with respect to genotype in the XY space, with legend according to group argument
> plotIndiv(nutri.res, rep.space= 'XY-variate', group = nutrimouse$genotype, add.legend = TRUE)
> 
> 
> # lattice style, with legend according to group argument
> plotIndiv(nutri.res, rep.space= 'XY-variate', group = nutrimouse$genotype, 
+           style = 'lattice')
> 
> # classic style, in the Y space
> plotIndiv(nutri.res, rep.space= 'Y-variate', group = nutrimouse$genotype, 
+           style = 'graphics')
> 
> 
> ## plot of individuals for objects of class 'pls' or 'spls'  
> # ----------------------------------------------------   
> data(liver.toxicity)
> X <- liver.toxicity$gene
> Y <- liver.toxicity$clinic
> toxicity.spls <- spls(X, Y, ncomp = 3, keepX = c(50, 50, 50), 
+                       keepY = c(10, 10, 10))
> 
> #default
> plotIndiv(toxicity.spls)
> 
> # in the Y space, colors indicate time of necropsy, text is the dose
> plotIndiv(toxicity.spls, rep.space= 'Y-variate', group = liver.toxicity$treatment[, 'Time.Group'], 
+           ind.names = liver.toxicity$treatment[, 'Dose.Group'], add.legend = TRUE)
> 
> ## Not run: 
> ##D # in the Y space, colors indicate time of necropsy, text is the dose, 
> ##D # changing the color per group, ellipse plots
> ##D plotIndiv(toxicity.spls, rep.space= 'Y-variate', group = liver.toxicity$treatment[, 'Time.Group'], 
> ##D           ind.names = liver.toxicity$treatment[, 'Dose.Group'], add.legend = TRUE,
> ##D           col.per.group = c(1:4), plot.ellipse = TRUE)
> ## End(Not run)
> 
> ## plot of individuals for objects of class 'plsda' or 'splsda'  
> # ----------------------------------------------------   
> data(breast.tumors)
> X <- breast.tumors$gene.exp
> Y <- breast.tumors$sample$treatment
> 
> splsda.breast <- splsda(X, Y,keepX=c(10,10),ncomp=2)
> 
> # default option: note the outcome color is included by default as it is a supervised approach
> plotIndiv(splsda.breast)
> 
> # default option with no ind name: pch and color are set automatically
> plotIndiv(splsda.breast, ind.names = FALSE, comp = c(1, 2))
> 
> # default option with no ind name: pch and color are set automatically, with legend
> plotIndiv(splsda.breast, ind.names = FALSE, comp = c(1, 2), add.legend = TRUE)
> 
> ## Not run: 
> ##D # playing with style
> ##D plotIndiv(splsda.breast, ind.names = TRUE, comp = c(1, 2), plot.indiv = FALSE, 
> ##D plot.ellipse = TRUE, style = "ggplot2", cex = c(1, 1))
> ##D plotIndiv(splsda.breast, ind.names = TRUE, comp = c(1, 2), plot.indiv = FALSE, 
> ##D plot.ellipse = TRUE, style = "lattice", cex = c(1, 1))
> ## End(Not run)
> 
> 
> ## plot of individuals for objects of class 'sgcca' (or 'rgcca')
> # ----------------------------------------------------
> data(nutrimouse)
> Y = unmap(nutrimouse$diet)
> data = list(gene = nutrimouse$gene, lipid = nutrimouse$lipid, Y = Y)
> design1 = matrix(c(0,1,1,1,0,1,1,1,0), ncol = 3, nrow = 3, byrow = TRUE)
> nutrimouse.sgcca <- wrapper.sgcca(blocks = data,
+                                   design = design1,
+                                   penalty = c(0.3, 0.5, 1),
+                                   ncomp = c(2, 2, 3),
+                                   scheme = "centroid",
+                                   verbose = FALSE, 
+                                   bias = FALSE)
> 
> # default style: one panel for each block
> plotIndiv(nutrimouse.sgcca)
Error: Aesthetics must be either length 1 or the same as the data (120): colour, size
Execution halted
```
```
DONE
Status: 1 ERROR
```

## mizer (0.2)
Maintainer: Finlay Scott <finlay.scott@jrc.ec.europa.eu>

```
checking whether package ‘mizer’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘mizer’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘mizer’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/mizer.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## mlr (2.4)
Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
Bug reports: https://github.com/berndbischl/mlr/issues

```
checking tests ... ERROR
Running the tests in ‘tests/run-base.R’ failed.
Last 13 lines of output:
     60L, 61L, 62L, 63L, 64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 
     77L, 78L, 79L, 80L, 101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 
     112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 124L, 125L, 
     126L, 127L, 128L, 129L, 130L)), method = character(0), thresh = 0.95, pcaComp = NULL, 
         na.remove = TRUE, k = 5L, fudge = 0.2, numUnique = 3L)
  19: pre_process_options(method, column_types)
  
  testthat results ================================================================
  OK: 1098 SKIPPED: 0 FAILED: 1
  1. Error: basic PreprocWrapperCaret works 
  
  Error: testthat unit tests failed
  Execution halted
```
```
DONE
Status: 1 ERROR
```

## mlxR (2.2.0)
Maintainer: Marc Lavielle <Marc.Lavielle@inria.fr>  
Bug reports: https://github.com/MarcLavielle/mlxR/issues

__OK__

## Mobilize (2.16-4)
Maintainer: Jeroen Ooms <jeroen.ooms@stat.ucla.edu>

```
checking R code for possible problems ... NOTE
biplot.character: no visible global function definition for ‘opts’
biplot.character: no visible global function definition for
  ‘theme_blank’
distributionplot.factor: no visible global function definition for
  ‘opts’
distributionplot.factor: no visible global function definition for
  ‘theme_text’
scatterplot: no visible global function definition for ‘opts’
scatterplot: no visible global function definition for ‘theme_text’
timeplot: no visible global function definition for ‘opts’
```
```
DONE
Status: 1 NOTE
```

## moonBook (0.1.3)
Maintainer: Keon-Woong Moon <cardiomoon@gmail.com>

```
checking files in ‘vignettes’ ... NOTE
The following directory looks like a leftover from 'knitr':
  ‘figure’
Please remove from your package.
```
```
DONE
Status: 1 NOTE
```

## morse (2.0.0)
Maintainer: Philippe Ruiz <philippe.ruiz@univ-lyon1.fr>

```
checking whether package ‘morse’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/morse.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## mosaic (0.12)
Maintainer: Randall Pruim <rpruim@calvin.edu>

```
checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    R     1.8Mb
    doc   2.8Mb
```
```
DONE
Status: 1 NOTE
```

## MRMR (0.1.3)
Maintainer: Brian A. Fannin <BFannin@RedwoodsGroup.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking R code for possible problems ... NOTE
PlotResiduals: no visible global function definition for ‘.’
```
```
DONE
Status: 2 NOTEs
```

## MSG (0.2.2)
Maintainer: Yihui Xie <xie@yihui.name>  
Bug reports: https://github.com/yihui/MSG/issues

```
checking Rd line widths ... NOTE
Rd file 'ChinaLifeEdu.Rd':
  \examples lines wider than 100 characters:
     contour(est$x1, est$x2, est$fhat, nlevels = 15, col = "darkgreen", add = TRUE, vfont = c("sans serif", 

Rd file 'andrews_curve.Rd':
  \usage lines wider than 90 characters:
     andrews_curve(x, n = 101, type = "l", lty = 1, lwd = 1, pch = NA, xlab = "t", ylab = "f(t)", 

These lines will be truncated in the PDF manual.
```
```
DONE
Status: 1 NOTE
```

## multiDimBio (0.3.3)
Maintainer: Samuel V. Scarpino <scarpino@utexas.edu>

```
checking package dependencies ... ERROR
Package required but not available: ‘pcaMethods’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## multilevelPSA (1.2.2)
Maintainer: Jason Bryer <jason@bryer.org>  
Bug reports: https://github.com/jbryer/multilevelPSA/issues

```
checking whether package ‘multilevelPSA’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/multilevelPSA.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## MultiMeta (0.1)
Maintainer: Dragana Vuckovic <dragana.vuckovic@burlo.trieste.it>

```
checking whether package ‘MultiMeta’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘MultiMeta’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘MultiMeta’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/MultiMeta.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## multitable (1.6)
Maintainer: Steve C Walker <steve.walker@utoronto.ca>

```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘multitable.Rnw’
  ...
> p <- p + geom_point(aes(x = width, y = abundance, 
+     shape = life.history))

> p <- p + stat_smooth(aes(x = width, y = abundance), 
+     se = FALSE, method = "bayesglm", family = poisson, form = y ~ 
+         x + I(x^2), colo .... [TRUNCATED] 

  When sourcing ‘multitable.R’:
Error: Unknown parameters: family
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
The following object is masked from ‘package:arm’:

    rescale

dimids automatically generated
dimids automatically generated
omitting width because it is not replicated along MARGIN
omitting temp because it is not replicated along MARGIN
omitting depth because it is not replicated along MARGIN
omitting velocity because it is not replicated along MARGIN
omitting substrate because it is not replicated along MARGIN
omitting habitat because it is not replicated along MARGIN
omitting trophic because of the following error:
 Error in median.default(newX[, i], ...) : need numeric data

omitting life.history because of the following error:
 Error in median.default(newX[, i], ...) : need numeric data

omitting trophic because it is not replicated along MARGIN
omitting life.history because it is not replicated along MARGIN

Error: processing vignette 'multitable.Rnw' failed with diagnostics:
 chunk 57 (label = a faceted ggplot scatterplot from a data list) 
Error : Unknown parameters: family
Execution halted

```
```
DONE
Status: 1 ERROR, 1 NOTE
```

## munsell (0.4.2)
Maintainer: Charlotte Wickham <cwickham@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘ggplot2’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
chroma_slice: no visible global function definition for ‘ggplot’
chroma_slice: no visible global function definition for ‘aes’
chroma_slice: no visible binding for global variable ‘hue’
chroma_slice: no visible binding for global variable ‘value’
chroma_slice: no visible binding for global variable ‘chroma’
chroma_slice: no visible global function definition for ‘geom_tile’
chroma_slice: no visible global function definition for ‘geom_text’
chroma_slice: no visible binding for global variable ‘name’
chroma_slice: no visible global function definition for
  ‘scale_colour_identity’
chroma_slice: no visible global function definition for
  ‘scale_x_discrete’
chroma_slice: no visible global function definition for
  ‘scale_y_continuous’
chroma_slice: no visible global function definition for ‘coord_fixed’
chroma_slice: no visible global function definition for ‘facet_wrap’
chroma_slice: no visible global function definition for
  ‘scale_fill_identity’
complement_slice: no visible global function definition for ‘ggplot’
complement_slice: no visible global function definition for ‘aes’
complement_slice: no visible binding for global variable ‘value’
complement_slice: no visible global function definition for ‘geom_tile’
complement_slice: no visible global function definition for ‘geom_text’
complement_slice: no visible binding for global variable ‘name’
complement_slice: no visible global function definition for
  ‘scale_fill_identity’
complement_slice: no visible global function definition for
  ‘scale_colour_identity’
complement_slice: no visible global function definition for
  ‘scale_x_continuous’
complement_slice: no visible global function definition for
  ‘scale_y_continuous’
complement_slice: no visible global function definition for
  ‘facet_grid’
complement_slice: no visible global function definition for
  ‘coord_fixed’
hue_slice: no visible global function definition for ‘ggplot’
hue_slice: no visible global function definition for ‘aes’
hue_slice: no visible binding for global variable ‘chroma’
hue_slice: no visible binding for global variable ‘value’
hue_slice: no visible global function definition for ‘geom_tile’
hue_slice: no visible global function definition for ‘facet_wrap’
hue_slice: no visible global function definition for ‘scale_x_discrete’
hue_slice: no visible global function definition for ‘coord_fixed’
hue_slice: no visible global function definition for ‘scale_y_discrete’
hue_slice: no visible global function definition for
  ‘scale_fill_identity’
hue_slice: no visible binding for global variable ‘hue’
hue_slice: no visible global function definition for ‘geom_text’
hue_slice: no visible binding for global variable ‘name’
hue_slice: no visible global function definition for
  ‘scale_colour_identity’
plot_closest: no visible global function definition for ‘ggplot’
plot_closest: no visible global function definition for ‘aes’
plot_closest: no visible binding for global variable ‘x’
plot_closest: no visible binding for global variable ‘y’
plot_closest: no visible global function definition for ‘geom_tile’
plot_closest: no visible global function definition for ‘geom_text’
plot_closest: no visible binding for global variable ‘name’
plot_closest: no visible binding for global variable ‘text.colour’
plot_closest: no visible global function definition for
  ‘scale_colour_identity’
plot_closest: no visible global function definition for ‘coord_fixed’
plot_closest: no visible global function definition for
  ‘scale_fill_identity’
plot_closest: no visible global function definition for ‘facet_wrap’
plot_hex: no visible global function definition for ‘geom_text’
plot_hex: no visible global function definition for ‘aes’
plot_hex: no visible global function definition for ‘facet_wrap’
plot_hex: no visible global function definition for ‘ggplot’
plot_hex: no visible binding for global variable ‘x’
plot_hex: no visible binding for global variable ‘y’
plot_hex: no visible global function definition for ‘geom_tile’
plot_hex: no visible binding for global variable ‘colour’
plot_hex: no visible global function definition for
  ‘scale_fill_identity’
plot_hex: no visible global function definition for
  ‘scale_x_continuous’
plot_hex: no visible global function definition for
  ‘scale_y_continuous’
plot_hex: no visible global function definition for ‘coord_fixed’
plot_mnsl: no visible global function definition for ‘geom_text’
plot_mnsl: no visible global function definition for ‘aes’
plot_mnsl: no visible global function definition for
  ‘scale_colour_identity’
plot_mnsl: no visible global function definition for ‘facet_wrap’
plot_mnsl: no visible global function definition for ‘ggplot’
plot_mnsl: no visible binding for global variable ‘x’
plot_mnsl: no visible binding for global variable ‘y’
plot_mnsl: no visible global function definition for ‘geom_tile’
plot_mnsl: no visible global function definition for
  ‘scale_x_continuous’
plot_mnsl: no visible global function definition for
  ‘scale_y_continuous’
plot_mnsl: no visible global function definition for ‘coord_fixed’
plot_mnsl: no visible global function definition for
  ‘scale_fill_identity’
theme_munsell: no visible global function definition for ‘theme’
theme_munsell: no visible global function definition for ‘element_line’
theme_munsell: no visible global function definition for ‘element_rect’
theme_munsell: no visible global function definition for
  ‘element_blank’
theme_munsell: no visible global function definition for ‘element_text’
value_slice: no visible global function definition for ‘ggplot’
value_slice: no visible global function definition for ‘aes’
value_slice: no visible binding for global variable ‘hue’
value_slice: no visible binding for global variable ‘chroma’
value_slice: no visible binding for global variable ‘value’
value_slice: no visible global function definition for ‘geom_tile’
value_slice: no visible global function definition for ‘coord_polar’
value_slice: no visible global function definition for
  ‘scale_x_discrete’
value_slice: no visible global function definition for
  ‘scale_y_discrete’
value_slice: no visible global function definition for ‘facet_wrap’
value_slice: no visible global function definition for
  ‘scale_fill_identity’
```
```
DONE
Status: 3 NOTEs
```

## mvtboost (0.3)
Maintainer: Patrick Miller <patrick.mil10@gmail.com>  
Bug reports: https://github.com/patr1ckm/mvtboost/issues

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 31-41 (mvtboost_vignette.Rmd) 
Error: processing vignette 'mvtboost_vignette.Rmd' failed with diagnostics:
variable 1: manufacturer is not of type numeric, ordered, or factor.
Execution halted

```
```
DONE
Status: 1 NOTE
```

## mwaved (1.1.1)
Maintainer: Justin Rory Wishart <j.wishart@unsw.edu.au>  
Bug reports: https://github.com/jrwishart/mwaved/issues

```
checking whether package ‘mwaved’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/mwaved.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## myTAI (0.3.0)
Maintainer: Hajk-Georg Drost <hajk-georg.drost@informatik.uni-halle.de>  
Bug reports: https://github.com/HajkD/myTAI/issues

```
checking package dependencies ... ERROR
Package required but not available: ‘edgeR’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## ncappc (0.2)
Maintainer: Chayan Acharya <chayan.acharya@farmbio.uu.se>

```
checking whether package ‘ncappc’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘ncappc’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ncappc.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## NeatMap (0.3.6.2)
Maintainer: Satwik Rajaram <satwik@gmail.com>

```
checking compiled code ... NOTE
File ‘NeatMap/libs/NeatMap.so’:
  Found ‘_rand’, possibly from ‘rand’ (C)
    Object: ‘nMDS_R.o’
  Found ‘_srand’, possibly from ‘srand’ (C)
    Object: ‘nMDS_R.o’

Compiled code should not call entry points which might terminate R nor
write to stdout/stderr instead of to the console, nor the system RNG.

See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```
```
DONE
Status: 1 NOTE
```

## networkreporting (0.0.1)
Maintainer: Dennis Feehan <dfeehan@princeton.edu>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
DONE
Status: 1 NOTE
```

## NeuralNetTools (1.3.1)
Maintainer: Marcus W. Beck <mbafs2012@gmail.com>  
Bug reports: https://github.com/fawda123/NeuralNetTools/issues

```
checking whether package ‘NeuralNetTools’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘NeuralNetTools’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/NeuralNetTools.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## ngramr (1.4.5)
Maintainer: Sean Carmody <seancarmody@gmail.com>

```
checking whether package ‘ngramr’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘ngramr’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/ngramr.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## NlsyLinks (2.0.1)
Maintainer: Will Beasley <wibeasley@hotmail.com>  
Bug reports: https://github.com/LiveOak/NlsyLinks/issues

```
checking installed package size ... NOTE
  installed size is  6.3Mb
  sub-directories of 1Mb or more:
    data   4.3Mb
    doc    1.0Mb
```
```
DONE
Status: 1 NOTE
```

## NMF (0.20.6)
Maintainer: Renaud Gaujoux <renaud@tx.technion.ac.il>  
Bug reports: http://github.com/renozao/NMF/issues

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘RcppOctave’ ‘doMPI’ ‘Biobase’
```
```
checking whether package ‘NMF’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘ggplot2::unit’ when loading ‘NMF’
  Warning: replacing previous import by ‘ggplot2::arrow’ when loading ‘NMF’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/NMF.Rcheck/00install.out’ for details.
```
```
checking R code for possible problems ... NOTE
.wrapResult: no visible global function definition for ‘exprs’
```
```
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: ‘RcppOctave’, ‘Biobase’
```
```
checking data for non-ASCII characters ... NOTE
  Error in .requirePackage(package) : 
    unable to find required package 'Biobase'
  Calls: <Anonymous> ... .extendsForS3 -> extends -> getClassDef -> .requirePackage
  Execution halted
```
```
checking examples ... ERROR
Running examples in ‘NMF-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: nmfModel
> ### Title: Factory Methods NMF Models
> ### Aliases: nmfModel nmfModel,data.frame,data.frame-method
> ###   nmfModel,formula,ANY-method nmfModel,matrix,ANY-method
> ###   nmfModel,matrix,matrix-method nmfModel-methods
> ###   nmfModel,missing,ANY-method nmfModel,missing,missing-method
> ###   nmfModel,NULL,ANY-method nmfModel,numeric,matrix-method
> ###   nmfModel,numeric,missing-method nmfModel,numeric,numeric-method
> ###   nmfModels
> ### Keywords: methods
> 
> ### ** Examples
> 
> ## Don't show: 
> # roxygen generated flag
> options(R_CHECK_RUNNING_EXAMPLES_=TRUE)
> ## End(Don't show)
> 
> #----------
> # nmfModel,numeric,numeric-method
> #----------
> # data
> n <- 20; r <- 3; p <- 10
> V <- rmatrix(n, p) # some target matrix
> 
> # create a r-ranked NMF model with a given target dimensions n x p as a 2-length vector
> nmfModel(r, c(n,p)) # directly
<Object of class:NMFstd>
features: 20 
basis/rank: 3 
samples: 10 
> nmfModel(r, dim(V)) # or from an existing matrix <=> nmfModel(r, V)
<Object of class:NMFstd>
features: 20 
basis/rank: 3 
samples: 10 
> # or alternatively passing each dimension separately
> nmfModel(r, n, p)
<Object of class:NMFstd>
features: 20 
basis/rank: 3 
samples: 10 
> 
> # trying to create a NMF object based on incompatible matrices generates an error
> w <- rmatrix(n, r)
> h <- rmatrix(r+1, p)
> try( new('NMFstd', W=w, H=h) )
Error in validObject(.Object) : 
  invalid class “NMFstd” object: Dimensions of W and H are not compatible [ncol(W)= 3 != nrow(H)= 4 ]
> try( nmfModel(w, h) )
Error in .local(rank, target, ...) : 
  nmfModel - Invalid number of columns in the basis matrix [3]: it should match the number of rows in the mixture coefficient matrix [4]
> try( nmfModel(r+1, W=w, H=h) )
Error in .local(rank, target, ...) : 
  nmfModel - Objective rank [4] is greater than the number of columns in W [3]
> # The factory method can be force the model to match some target dimensions
> # but warnings are thrown
> nmfModel(r, W=w, H=h)
Warning in .local(rank, target, ...) :
  nmfModel - Objective rank [3] is lower than the number of rows in H [4]: only the first 3 rows of H  will be used
<Object of class:NMFstd>
features: 20 
basis/rank: 3 
samples: 10 
> nmfModel(r, n-1, W=w, H=h)
Warning in .local(rank, target, ...) :
  nmfModel - Number of rows in target is lower than the number of rows in W [20]: only the first 19 rows of W will be used
Warning in .local(rank, target, ...) :
  nmfModel - Objective rank [3] is lower than the number of rows in H [4]: only the first 3 rows of H  will be used
<Object of class:NMFstd>
features: 19 
basis/rank: 3 
samples: 10 
> 
> #----------
> # nmfModel,numeric,missing-method
> #----------
> ## Empty model of given rank
> nmfModel(3)
<Object of class:NMFstd>
features: 0 
basis/rank: 3 
samples: 0 
> 
> #----------
> # nmfModel,missing,ANY-method
> #----------
> nmfModel(target=10) #square
<Object of class:NMFstd>
features: 10 
basis/rank: 0 
samples: 10 
> nmfModel(target=c(10, 5))
<Object of class:NMFstd>
features: 10 
basis/rank: 0 
samples: 5 
> 
> #----------
> # nmfModel,missing,missing-method
> #----------
> # Build an empty NMF model
> nmfModel()
<Object of class:NMFstd>
features: 0 
basis/rank: 0 
samples: 0 
> 
> # create a NMF object based on one random matrix: the missing matrix is deduced
> # Note this only works when using factory method NMF
> n <- 50; r <- 3;
> w <- rmatrix(n, r)
> nmfModel(W=w)
<Object of class:NMFstd>
features: 50 
basis/rank: 3 
samples: 0 
> 
> # create a NMF object based on random (compatible) matrices
> p <- 20
> h <- rmatrix(r, p)
> nmfModel(H=h)
<Object of class:NMFstd>
features: 0 
basis/rank: 3 
samples: 20 
> 
> # specifies two compatible matrices
> nmfModel(W=w, H=h)
<Object of class:NMFstd>
features: 50 
basis/rank: 3 
samples: 20 
> # error if not compatible
> try( nmfModel(W=w, H=h[-1,]) )
Error in .local(rank, target, ...) : 
  nmfModel - Invalid number of columns in the basis matrix [3]: it should match the number of rows in the mixture coefficient matrix [2]
> 
> #----------
> # nmfModel,numeric,matrix-method
> #----------
> # create a r-ranked NMF model compatible with a given target matrix
> obj <- nmfModel(r, V)
> all(is.na(basis(obj)))
[1] TRUE
> 
> #----------
> # nmfModel,matrix,matrix-method
> #----------
> ## From two existing factors
> 
> # allows a convenient call without argument names
> w <- rmatrix(n, 3); h <- rmatrix(3, p)
> nmfModel(w, h)
<Object of class:NMFstd>
features: 50 
basis/rank: 3 
samples: 20 
> 
> # Specify the type of NMF model (e.g. 'NMFns' for non-smooth NMF)
> mod <- nmfModel(w, h, model='NMFns')
> mod
<Object of class:NMFns>
features: 50 
basis/rank: 3 
samples: 20 
theta: 0.5 
> 
> # One can use such an NMF model as a seed when fitting a target matrix with nmf()
> V <- rmatrix(mod)
> res <- nmf(V, mod)
> nmf.equal(res, nmf(V, mod))
[1] TRUE
> 
> # NB: when called only with such a seed, the rank and the NMF algorithm
> # are selected based on the input NMF model.
> # e.g. here rank was 3 and the algorithm "nsNMF" is used, because it is the default
> # algorithm to fit "NMFns" models (See ?nmf).
> 
> #----------
> # nmfModel,matrix,ANY-method
> #----------
> ## swapped arguments `rank` and `target`
> V <- rmatrix(20, 10)
> nmfModel(V) # equivalent to nmfModel(target=V)
<Object of class:NMFstd>
features: 20 
basis/rank: 0 
samples: 10 
> nmfModel(V, 3) # equivalent to nmfModel(3, V)
<Object of class:NMFstd>
features: 20 
basis/rank: 3 
samples: 10 
> 
> #----------
> # nmfModel,formula,ANY-method
> #----------
> # empty 3-rank model
> nmfModel(~ 3)
<Object of class:NMFstd>
features: 0 
basis/rank: 3 
samples: 0 
> 
> # 3-rank model that fits a given data matrix
> x <- rmatrix(20,10)
> nmfModel(x ~ 3)
<Object of class:NMFstd>
features: 20 
basis/rank: 3 
samples: 10 
> 
> # add fixed coefficient term defined by a factor
> gr <- gl(2, 5)
> nmfModel(x ~ 3 + gr)
<Object of class:NMFstd>
features: 20 
basis/rank: 5 
samples: 10 
fixed coef [2]:
  gr = <1, 2>
> 
> # add fixed coefficient term defined by a numeric covariate
> nmfModel(x ~ 3 + gr + b, data=list(b=runif(10)))
<Object of class:NMFstd>
features: 20 
basis/rank: 6 
samples: 10 
fixed coef [3]:
  gr = <1, 2>
  b = 0.0101301828399301, 0.21454192395322, ..., 0.767450851621106
> 
> # 3-rank model that fits a given ExpressionSet (with fixed coef terms)
> e <- ExpressionSet(x)
Error: could not find function "ExpressionSet"
Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 385-398 (NMF-vignette.Rnw) 
Error: processing vignette 'NMF-vignette.Rnw' failed with diagnostics:
unable to find required package 'Biobase'
Execution halted

```
```
DONE
Status: 1 ERROR, 1 WARNING, 5 NOTEs
```

## NNTbiomarker (0.29.11)
Maintainer: Roger Day <day01@pitt.edu>

__OK__

## NORRRM (1.0.0)
Maintainer: Renee Gonzalez Guzman <rguzman@cicese.edu.mx>

__OK__

## nullabor (0.3.1)
Maintainer: Di Cook <dicook@iastate.edu>

__OK__

## oapackage (2.0.23)
Maintainer: Pieter Thijs Eendebak <pieter.eendebak@gmail.com>

__OK__

## oaxaca (0.1.2)
Maintainer: Marek Hlavac <hlavac@fas.harvard.edu>

```
checking examples ... ERROR
Running examples in ‘oaxaca-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: oaxaca
> ### Title: Blinder-Oaxaca Decomposition
> ### Aliases: oaxaca summary.oaxaca
> ### Keywords: decomposition regression linear
> 
> ### ** Examples
> 
> # set random seed
> set.seed(03104)
> 
> # load data set of Hispanic workers in Chicago
> data("chicago")
> 
> # perform Blinder-Oaxaca Decomposition:
> # explain differences in log real wages across native and foreign-born groups
> oaxaca.results.1 <- oaxaca(ln.real.wage ~ age + female + LTHS + some.college + 
+                                           college + advanced.degree | foreign.born, 
+                            data = chicago, R = 30)
oaxaca: oaxaca() performing analysis. Please wait.

Bootstrapping standard errors:
1 / 30 (3.33%)
3 / 30 (10%)
6 / 30 (20%)
9 / 30 (30%)
12 / 30 (40%)
15 / 30 (50%)
18 / 30 (60%)
21 / 30 (70%)
24 / 30 (80%)
27 / 30 (90%)
30 / 30 (100%)
> 
> # print the results
> print(oaxaca.results.1)
$beta
$beta$beta.A
    (Intercept)             age          female            LTHS    some.college 
     2.23253751      0.01345495     -0.29547808     -0.18054263      0.14264418 
        college advanced.degree 
     0.51160587      0.70198200 

$beta$beta.B
    (Intercept)             age          female            LTHS    some.college 
    2.400531738     0.005594233    -0.196041450    -0.168690180     0.156513512 
        college advanced.degree 
    0.219690157     0.931336335 

$beta$beta.diff
    (Intercept)             age          female            LTHS    some.college 
   -0.167994227     0.007860715    -0.099436633    -0.011852452    -0.013869333 
        college advanced.degree 
    0.291915711    -0.229354332 

$beta$beta.R
                (Intercept)         age     female       LTHS some.college
[1,]  0.0000000    2.400532 0.005594233 -0.1960415 -0.1686902    0.1565135
[2,]  1.0000000    2.232538 0.013454948 -0.2954781 -0.1805426    0.1426442
[3,]  0.5000000    2.316535 0.009524591 -0.2457598 -0.1746164    0.1495788
[4,]  0.5690691    2.304931 0.010067523 -0.2526278 -0.1754350    0.1486209
[5,] -1.0000000    2.346364 0.008143107 -0.2348752 -0.1988700    0.1563418
[6,] -2.0000000    2.376670 0.008926525 -0.2382057 -0.1834587    0.1288051
       college advanced.degree
[1,] 0.2196902       0.9313363
[2,] 0.5116059       0.7019820
[3,] 0.3656480       0.8166592
[4,] 0.3858104       0.8008179
[5,] 0.3891707       0.8111406
[6,] 0.3723389       0.7880131


$call
oaxaca(formula = ln.real.wage ~ age + female + LTHS + some.college + 
    college + advanced.degree | foreign.born, data = chicago, 
    R = 30)

$n
$n$n.A
[1] 287

$n$n.B
[1] 379

$n$n.pooled
[1] 666


$R
[1] 30

$reg
$reg$reg.A

Call:
NULL

Coefficients:
    (Intercept)              age           female             LTHS  
        2.23254          0.01345         -0.29548         -0.18054  
   some.college          college  advanced.degree  
        0.14264          0.51161          0.70198  


$reg$reg.B

Call:
NULL

Coefficients:
    (Intercept)              age           female             LTHS  
        2.23254          0.01345         -0.29548         -0.18054  
   some.college          college  advanced.degree  
        0.14264          0.51161          0.70198  


$reg$reg.pooled.1

Call:
NULL

Coefficients:
    (Intercept)              age           female             LTHS  
       2.346364         0.008143        -0.234875        -0.198870  
   some.college          college  advanced.degree  
       0.156342         0.389171         0.811141  


$reg$reg.pooled.2

Call:
NULL

Coefficients:
    (Intercept)              age           female             LTHS  
       2.376670         0.008927        -0.238206        -0.183459  
   some.college          college  advanced.degree     foreign.born  
       0.128805         0.372339         0.788013        -0.093352  



$threefold
$threefold$overall
  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients) 
        0.07694359         0.02665455         0.12222967         0.04807357 
 coef(interaction)    se(interaction) 
       -0.05580754         0.04493710 

$threefold$variables
                coef(endowments) se(endowments) coef(coefficients)
(Intercept)           0.00000000    0.000000000       -0.167994227
age                  -0.03706421    0.011913651        0.319427085
female               -0.01667487    0.008702664       -0.039354868
LTHS                  0.04544444    0.013942140       -0.004597125
some.college          0.03947192    0.015595726       -0.001866322
college               0.01035304    0.007245263        0.023877011
advanced.degree       0.03541327    0.016943848       -0.007261879
                se(coefficients) coef(interaction) se(interaction)
(Intercept)          0.102573757       0.000000000     0.000000000
age                  0.117093344      -0.052080622     0.023442687
female               0.020700727      -0.008457869     0.007004911
LTHS                 0.033593755       0.003193002     0.023911364
some.college         0.012304004      -0.003497776     0.023917546
college              0.008747167       0.013756722     0.011351569
advanced.degree      0.006782704      -0.008721002     0.009747714


$twofold
$twofold$overall
         weight coef(explained) se(explained) coef(unexplained) se(unexplained)
[1,]  0.0000000      0.07694359    0.02665455        0.06642213      0.03859534
[2,]  1.0000000      0.02113605    0.03465667        0.12222967      0.04807357
[3,]  0.5000000      0.04903982    0.02123533        0.09432590      0.03735637
[4,]  0.5690691      0.04518525    0.02223644        0.09818048      0.03823441
[5,] -1.0000000      0.06825666    0.02068619        0.07510906      0.02755265
[6,] -2.0000000      0.05001392    0.01984619        0.09335181      0.03468036
     coef(unexplained A) se(unexplained A) coef(unexplained B)
[1,]        6.642213e-02      3.859534e-02          0.00000000
[2,]        0.000000e+00      0.000000e+00          0.12222967
[3,]        3.321107e-02      1.929767e-02          0.06111484
[4,]        2.862335e-02      1.663193e-02          0.06955713
[5,]        4.274224e-02      1.578190e-02          0.03236682
[6,]       -2.169272e-15      4.345303e-15          0.09335181
     se(unexplained B)
[1,]        0.00000000
[2,]        0.04807357
[3,]        0.02403678
[4,]        0.02735718
[5,]        0.01195835
[6,]        0.03468036

$twofold$variables
$twofold$variables[[1]]
                weight coef(explained) se(explained) coef(unexplained)
(Intercept)          0      0.00000000   0.000000000      -0.167994227
age                  0     -0.03706421   0.011913651       0.267346463
female               0     -0.01667487   0.008702664      -0.047812737
LTHS                 0      0.04544444   0.013942140      -0.001404123
some.college         0      0.03947192   0.015595726      -0.005364098
college              0      0.01035304   0.007245263       0.037633733
advanced.degree      0      0.03541327   0.016943848      -0.015982880
                se(unexplained) coef(unexplained A) se(unexplained A)
(Intercept)         0.102573757        -0.167994227       0.102573757
age                 0.095012790         0.267346463       0.095012790
female              0.026283859        -0.047812737       0.026283859
LTHS                0.009871699        -0.001404123       0.009871699
some.college        0.035944512        -0.005364098       0.035944512
college             0.016807066         0.037633733       0.016807066
advanced.degree     0.015576195        -0.015982880       0.015576195
                coef(unexplained B) se(unexplained B)
(Intercept)                       0                 0
age                               0                 0
female                            0                 0
LTHS                              0                 0
some.college                      0                 0
college                           0                 0
advanced.degree                   0                 0

$twofold$variables[[2]]
                weight coef(explained) se(explained) coef(unexplained)
(Intercept)          1      0.00000000    0.00000000      -0.167994227
age                  1     -0.08914483    0.02436475       0.319427085
female               1     -0.02513274    0.01434089      -0.039354868
LTHS                 1      0.04863744    0.02248472      -0.004597125
some.college         1      0.03597414    0.01709530      -0.001866322
college              1      0.02410977    0.01485710       0.023877011
advanced.degree      1      0.02669226    0.01066619      -0.007261879
                se(unexplained) coef(unexplained A) se(unexplained A)
(Intercept)         0.102573757                   0                 0
age                 0.117093344                   0                 0
female              0.020700727                   0                 0
LTHS                0.033593755                   0                 0
some.college        0.012304004                   0                 0
college             0.008747167                   0                 0
advanced.degree     0.006782704                   0                 0
                coef(unexplained B) se(unexplained B)
(Intercept)            -0.167994227       0.102573757
age                     0.319427085       0.117093344
female                 -0.039354868       0.020700727
LTHS                   -0.004597125       0.033593755
some.college           -0.001866322       0.012304004
college                 0.023877011       0.008747167
advanced.degree        -0.007261879       0.006782704

$twofold$variables[[3]]
                weight coef(explained) se(explained) coef(unexplained)
(Intercept)        0.5      0.00000000    0.00000000      -0.167994227
age                0.5     -0.06310452    0.01517887       0.293386774
female             0.5     -0.02090380    0.01133277      -0.043583802
LTHS               0.5      0.04704094    0.01438870      -0.003000624
some.college       0.5      0.03772303    0.01116807      -0.003615210
college            0.5      0.01723141    0.01021758       0.030755372
advanced.degree    0.5      0.03105276    0.01329196      -0.011622379
                se(unexplained) coef(unexplained A) se(unexplained A)
(Intercept)          0.10257376       -0.0839971136       0.051286879
age                  0.10597995        0.1336732317       0.047506395
female               0.02339687       -0.0239063683       0.013141929
LTHS                 0.02168080       -0.0007020616       0.004935849
some.college         0.02405589       -0.0026820488       0.017972256
college              0.01213592        0.0188168664       0.008403533
advanced.degree      0.01097984       -0.0079914401       0.007788098
                coef(unexplained B) se(unexplained B)
(Intercept)            -0.083997114       0.051286879
age                     0.159713543       0.058546672
female                 -0.019677434       0.010350363
LTHS                   -0.002298563       0.016796877
some.college           -0.000933161       0.006152002
college                 0.011938505       0.004373583
advanced.degree        -0.003630939       0.003391352

$twofold$variables[[4]]
                   weight coef(explained) se(explained) coef(unexplained)
(Intercept)     0.5690691      0.00000000    0.00000000      -0.167994227
age             0.5690691     -0.06670168    0.01625480       0.296983934
female          0.5690691     -0.02148798    0.01173199      -0.042999625
LTHS            0.5690691      0.04726148    0.01520707      -0.003221162
some.college    0.5690691      0.03748144    0.01143856      -0.003373622
college         0.5690691      0.01818157    0.01079972       0.029805208
advanced.degree 0.5690691      0.03045041    0.01285138      -0.011020028
                se(unexplained) coef(unexplained A) se(unexplained A)
(Intercept)          0.10257376       -0.0723939087       0.044202205
age                  0.10750745        0.1152078604       0.040943950
female               0.02301150       -0.0206039871       0.011326528
LTHS                 0.02332383       -0.0006050801       0.004254020
some.college         0.02241949       -0.0023115556       0.015489602
college              0.01156156        0.0162175395       0.007242685
advanced.degree      0.01036486       -0.0068875175       0.006712264
                coef(unexplained B) se(unexplained B)
(Intercept)            -0.095600319       0.058371552
age                     0.181776074       0.066634200
female                 -0.022395638       0.011780143
LTHS                   -0.002616082       0.019117167
some.college           -0.001062066       0.007001828
college                 0.013587668       0.004977742
advanced.degree        -0.004132510       0.003859827

$twofold$variables[[5]]
                weight coef(explained) se(explained) coef(unexplained)
(Intercept)         -1      0.00000000    0.00000000      -0.167994227
age                 -1     -0.05395159    0.01164617       0.284233848
female              -1     -0.01997799    0.01059458      -0.044509618
LTHS                -1      0.05357477    0.01511313      -0.009534452
some.college        -1      0.03942862    0.01070448      -0.005320798
college             -1      0.01833993    0.01180974       0.029646849
advanced.degree     -1      0.03084293    0.01213337      -0.011412542
                se(unexplained) coef(unexplained A) se(unexplained A)
(Intercept)         0.102573757        -0.113826779       0.065427620
age                 0.101999521         0.180658127       0.067133394
female              0.023931840        -0.029140051       0.015789132
LTHS                0.015616635         0.002171190       0.007550534
some.college        0.022541487        -0.005297694       0.016625556
college             0.011451449         0.015784323       0.007123078
advanced.degree     0.009989864        -0.007606873       0.006261995
                coef(unexplained B) se(unexplained B)
(Intercept)           -5.416745e-02       0.043693327
age                    1.035757e-01       0.042310179
female                -1.536957e-02       0.009680798
LTHS                  -1.170564e-02       0.009463445
some.college          -2.310375e-05       0.007161898
college                1.386253e-02       0.004799840
advanced.degree       -3.805669e-03       0.003971440

$twofold$variables[[6]]
                weight coef(explained) se(explained) coef(unexplained)
(Intercept)         -2      0.00000000    0.00000000      -0.167994227
age                 -2     -0.05914207    0.01319394       0.289424330
female              -2     -0.02026127    0.01089181      -0.044226331
LTHS                -2      0.04942303    0.01377796      -0.005382714
some.college        -2      0.03248399    0.01059997       0.001623828
college             -2      0.01754672    0.01135918       0.030440058
advanced.degree     -2      0.02996352    0.01161304      -0.010533138
                se(unexplained) coef(unexplained A) se(unexplained A)
(Intercept)         0.102573757       -0.1441320477       0.066534228
age                 0.103507863        0.1540137226       0.060361530
female              0.023742941       -0.0275386157       0.014834656
LTHS                0.015270649        0.0003454627       0.007592458
some.college        0.022261483        0.0053524036       0.015796062
college             0.011475934        0.0179542742       0.007052683
advanced.degree     0.009705778       -0.0059951998       0.005754838
                coef(unexplained B) se(unexplained B)
(Intercept)            -0.023862180       0.049407378
age                     0.135410608       0.046541114
female                 -0.016687716       0.009871269
LTHS                   -0.005728176       0.008439051
some.college           -0.003728575       0.006952928
college                 0.012485784       0.004779231
advanced.degree        -0.004537938       0.004107819



$x
$x$x.mean.A
    (Intercept)             age          female            LTHS    some.college 
     1.00000000     34.01045296      0.48083624      0.11846690      0.38675958 
        college advanced.degree 
     0.12891986      0.06968641 

$x$x.mean.B
    (Intercept)             age          female            LTHS    some.college 
     1.00000000     40.63588391      0.39577836      0.38786280      0.13456464 
        college advanced.degree 
     0.08179420      0.03166227 

$x$x.mean.diff
    (Intercept)             age          female            LTHS    some.college 
     0.00000000     -6.62543094      0.08505787     -0.26939590      0.25219494 
        college advanced.degree 
     0.04712567      0.03802414 


$y
$y$y.A
[1] 2.696725

$y$y.B
[1] 2.55336

$y$y.diff
[1] 0.1433657


attr(,"class")
[1] "oaxaca"
> 
> # Next:
> # - adjust gender and education dummy variable coefficients to make results
> #   invariant to the choice of omitted baseline (reference category)
> # - include additional weights for the twofold decomposition that give
> #   weights of 0.2 and 0.4 to Group A relative to Group B in the choice
> #   of reference coefficients
> 
> oaxaca.results.2 <- oaxaca(ln.real.wage ~ age + female + LTHS + some.college + 
+                                           college + advanced.degree | foreign.born |
+                                           LTHS + some.college + college + advanced.degree,
+                            data = chicago, weights = c(0.2, 0.4), R = 30)
oaxaca: oaxaca() performing analysis. Please wait.

Bootstrapping standard errors:
1 / 30 (3.33%)
3 / 30 (10%)
6 / 30 (20%)
9 / 30 (30%)
12 / 30 (40%)
15 / 30 (50%)
18 / 30 (60%)
21 / 30 (70%)
24 / 30 (80%)
27 / 30 (90%)
30 / 30 (100%)
> 
> # plot the results
> plot(oaxaca.results.2)
Error: Unknown parameters: xintercept
Execution halted
```
```
DONE
Status: 1 ERROR
```

## OpasnetUtils (1.2.0)
Maintainer: Teemu Rintala <teemu.rintala.a@gmail.com>

__OK__

## OpenStreetMap (0.3.2)
Maintainer: Ian Fellows <ian@fellstat.com>

__OK__

## optiRum (0.35)
Maintainer: Stephanie Locke <stephanie.locke@optimumcredit.co.uk>  
Bug reports: https://github.com/stephlocke/optiRum/issues

```
checking whether package ‘optiRum’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘optiRum’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘optiRum’
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘optiRum’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/optiRum.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘optiRum-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: giniChart
> ### Title: Produce a ROC curve with gini coefficient title
> ### Aliases: giniChart
> ### Keywords: AUROC gini roc
> 
> ### ** Examples
> 
> sampledata<- data.frame(val= rnorm(100) , outcome=rbinom(100,1,.8))
>   giniChart(sampledata$val,sampledata$outcome)
Warning in e1[n] <- e2[n] :
  number of items to replace is not a multiple of replacement length
Error in if (debug) { : argument is not interpretable as logical
Calls: print ... element_grob -> element_grob.element_text -> titleGrob
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## orderedLasso (1.7)
Maintainer: Xiaotong Suo <xiaotong@stanford.edu>

```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  predict.orderedLasso predict.orderedLasso.path predict.timeLagLasso
  predict.timeLagLasso.path
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 NOTE
```

## orgR (0.9.0)
Maintainer: Yi Tang <yi.tang.uk@me.com>

__OK__

## OriGen (1.3.1)
Maintainer: John Michael O. Ranola <ranolaj@uw.edu>

```
checking whether package ‘OriGen’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/OriGen.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## OutbreakTools (0.1-13)
Maintainer: Thibaut Jombart <t.jombart@imperial.ac.uk>

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'plotggphy.Rd':
  ‘ggplot2’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
checking examples ... ERROR
Running examples in ‘OutbreakTools-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ToyOutbreakRaw
> ### Title: Raw simulated outbreak dataset
> ### Aliases: ToyOutbreakRaw
> ### Keywords: dataset
> 
> ### ** Examples
> 
> ## Load data ##
> data(ToyOutbreakRaw)
> attach(ToyOutbreakRaw)
The following object is masked from package:datasets:

    trees

> 
> ## Constructing an obkData object ##
> x <- new ("obkData", individuals=individuals, records=records,
+           contacts=contacts, contacts.start=contacts.start,
+           contacts.end=contacts.end, dna=dna,
+           dna.individualID=dna.info$individualID,
+           dna.date=dna.info$date, sample=dna.info$sample, trees=trees)
> 
> detach(ToyOutbreakRaw)
> 
> 
> ## Examining the object ##
> summary(x)
Dataset of 418 individuals with...
== @individuals ==
individuals information
  418 entries
  recorded fields are:
  <infector> class: numeric,  mean: 84.26139,  sd:67.48384, range: [1;245],  1 NAs
  <DateInfected> class: character,  10 unique values,  frequency range: [1;173],  0 NAs
  <Sex> class: character,  2 unique values,  frequency range: [192;226],  0 NAs
  <Age> class: numeric,  mean: 35.09809,  sd:6.10833, range: [19;56],  0 NAs
  <lat> class: numeric,  mean: 51.51644,  sd:0.00304656, range: [51.50711;51.52625],  0 NAs
  <lon> class: numeric,  mean: -0.1711455,  sd:0.01051185, range: [-0.2013245;-0.140349],  0 NAs

== @records ==
records on:  Fever 
$Fever
  418 entries,  418 individuals, from 2000-01-03 to 2000-01-17
  recorded fields are:
  <temperature> class: numeric,  mean: 39.48541,  sd:0.5310073, range: [38;40.9],  0 NAs

== @dna ==
836 sequences across 2 loci, 418 individuals, from 2000-01-01 to 2000-01-10
length of concatenated alignment: 1600 nucleotides
Attached meta data:
  836 entries,  418 individuals, from 2000-01-01 to 2000-01-10
  recorded fields are:
  <locus> class: character,  2 unique values,  frequency range: [418;418],  0 NAs
  <sample> class: character,  418 unique values,  frequency range: [2;2],  0 NAs

== @contacts ==
19 contacts between 20 individuals

== @trees ==
1 phylogenetic trees with 418 tips

> 
> head(x@individuals)
  infector DateInfected Sex Age      lat        lon
1       NA   2000-01-01   M  33 51.52152 -0.1805272
2        1   2000-01-02   F  42 51.51502 -0.1770907
3        2   2000-01-03   F  44 51.51885 -0.1614321
4        2   2000-01-03   M  49 51.51672 -0.1706063
5        2   2000-01-03   M  34 51.51797 -0.1685206
6        2   2000-01-03   M  31 51.51401 -0.1662320
> head(x@records)
$Fever
    individualID       date temperature
1              1 2000-01-03        39.1
2              2 2000-01-03        40.4
3              3 2000-01-07        40.0
4              4 2000-01-08        39.8
5              5 2000-01-04        39.4
6              6 2000-01-06        39.3
7              7 2000-01-13        39.3
8              8 2000-01-07        39.4
9              9 2000-01-06        38.0
10            10 2000-01-07        39.5
11            11 2000-01-09        39.5
12            12 2000-01-09        39.3
13            13 2000-01-10        39.8
14            14 2000-01-09        38.6
15            15 2000-01-09        39.5
16            16 2000-01-05        39.0
17            17 2000-01-06        39.0
18            18 2000-01-09        39.6
19            19 2000-01-07        38.9
20            20 2000-01-08        39.5
21            21 2000-01-11        39.7
22            22 2000-01-08        39.6
23            23 2000-01-08        39.9
24            24 2000-01-10        39.5
25            25 2000-01-08        39.6
26            26 2000-01-10        40.6
27            27 2000-01-10        39.9
28            28 2000-01-07        39.1
29            29 2000-01-09        38.8
30            30 2000-01-09        39.5
31            31 2000-01-10        39.6
32            32 2000-01-10        39.5
33            33 2000-01-11        39.2
34            34 2000-01-10        40.0
35            35 2000-01-06        39.3
36            36 2000-01-10        39.1
37            37 2000-01-10        38.8
38            38 2000-01-10        39.8
39            39 2000-01-10        39.3
40            40 2000-01-08        38.9
41            41 2000-01-08        39.4
42            42 2000-01-11        39.0
43            43 2000-01-10        39.7
44            44 2000-01-07        39.1
45            45 2000-01-10        39.5
46            46 2000-01-10        40.1
47            47 2000-01-10        39.6
48            48 2000-01-08        38.7
49            49 2000-01-09        39.7
50            50 2000-01-08        39.6
51            51 2000-01-08        39.2
52            52 2000-01-10        39.9
53            53 2000-01-09        39.9
54            54 2000-01-08        40.1
55            55 2000-01-09        39.2
56            56 2000-01-11        39.7
57            57 2000-01-11        40.2
58            58 2000-01-10        40.5
59            59 2000-01-10        38.9
60            60 2000-01-10        39.2
61            61 2000-01-11        39.1
62            62 2000-01-09        39.7
63            63 2000-01-07        39.4
64            64 2000-01-08        39.6
65            65 2000-01-13        39.0
66            66 2000-01-12        39.0
67            67 2000-01-07        39.5
68            68 2000-01-08        39.6
69            69 2000-01-07        39.8
70            70 2000-01-09        40.3
71            71 2000-01-11        39.7
72            72 2000-01-12        39.6
73            73 2000-01-09        40.1
74            74 2000-01-11        39.8
75            75 2000-01-09        39.9
76            76 2000-01-09        40.0
77            77 2000-01-07        40.0
78            78 2000-01-10        38.9
79            79 2000-01-11        39.4
80            80 2000-01-09        39.3
81            81 2000-01-11        38.8
82            82 2000-01-11        38.7
83            83 2000-01-09        40.3
84            84 2000-01-12        38.5
85            85 2000-01-09        39.4
86            86 2000-01-11        39.7
87            87 2000-01-09        39.6
88            88 2000-01-10        38.0
89            89 2000-01-10        39.1
90            90 2000-01-09        38.7
91            91 2000-01-09        39.5
92            92 2000-01-11        39.7
93            93 2000-01-10        39.3
94            94 2000-01-12        39.7
95            95 2000-01-13        39.0
96            96 2000-01-10        39.6
97            97 2000-01-11        39.9
98            98 2000-01-08        40.7
99            99 2000-01-10        40.8
100          100 2000-01-12        39.4
101          101 2000-01-13        39.8
102          102 2000-01-11        39.5
103          103 2000-01-12        39.2
104          104 2000-01-09        40.1
105          105 2000-01-12        39.2
106          106 2000-01-10        39.9
107          107 2000-01-10        39.3
108          108 2000-01-08        40.1
109          109 2000-01-12        39.4
110          110 2000-01-09        38.8
111          111 2000-01-09        39.3
112          112 2000-01-10        39.4
113          113 2000-01-09        39.5
114          114 2000-01-10        40.7
115          115 2000-01-11        39.5
116          116 2000-01-10        38.6
117          117 2000-01-11        38.9
118          118 2000-01-10        39.5
119          119 2000-01-12        38.5
120          120 2000-01-10        38.5
121          121 2000-01-09        39.0
122          122 2000-01-12        40.2
123          123 2000-01-11        39.8
124          124 2000-01-11        39.8
125          125 2000-01-10        39.0
126          126 2000-01-13        38.7
127          127 2000-01-10        40.2
128          128 2000-01-10        39.9
129          129 2000-01-09        39.7
130          130 2000-01-09        39.1
131          131 2000-01-13        39.6
132          132 2000-01-12        40.3
133          133 2000-01-08        38.6
134          134 2000-01-10        39.3
135          135 2000-01-08        39.7
136          136 2000-01-11        39.3
137          137 2000-01-10        39.0
138          138 2000-01-10        39.9
139          139 2000-01-14        39.0
140          140 2000-01-13        38.6
141          141 2000-01-15        38.8
142          142 2000-01-11        40.3
143          143 2000-01-10        39.4
144          144 2000-01-12        39.0
145          145 2000-01-11        40.1
146          146 2000-01-11        39.5
147          147 2000-01-11        39.9
148          148 2000-01-13        40.2
149          149 2000-01-10        39.7
150          150 2000-01-12        39.9
151          151 2000-01-10        40.0
152          152 2000-01-12        39.8
153          153 2000-01-11        38.8
154          154 2000-01-09        39.6
155          155 2000-01-11        39.1
156          156 2000-01-10        39.2
157          157 2000-01-09        38.1
158          158 2000-01-11        40.5
159          159 2000-01-13        39.5
160          160 2000-01-11        40.7
161          161 2000-01-12        39.3
162          162 2000-01-11        38.9
163          163 2000-01-11        39.5
164          164 2000-01-13        39.1
165          165 2000-01-13        39.8
166          166 2000-01-12        40.0
167          167 2000-01-14        38.8
168          168 2000-01-15        39.7
169          169 2000-01-13        40.2
170          170 2000-01-12        39.5
171          171 2000-01-13        38.7
172          172 2000-01-11        40.1
173          173 2000-01-10        38.8
174          174 2000-01-10        39.7
175          175 2000-01-13        40.4
176          176 2000-01-10        39.5
177          177 2000-01-11        40.0
178          178 2000-01-14        39.2
179          179 2000-01-10        39.0
180          180 2000-01-13        38.7
181          181 2000-01-10        39.5
182          182 2000-01-14        39.5
183          183 2000-01-10        39.5
184          184 2000-01-13        39.8
185          185 2000-01-10        39.9
186          186 2000-01-09        39.4
187          187 2000-01-12        39.6
188          188 2000-01-13        39.2
189          189 2000-01-11        38.9
190          190 2000-01-12        39.3
191          191 2000-01-14        39.5
192          192 2000-01-11        39.3
193          193 2000-01-12        39.6
194          194 2000-01-12        39.6
195          195 2000-01-11        39.9
196          196 2000-01-11        39.7
197          197 2000-01-11        39.7
198          198 2000-01-12        39.9
199          199 2000-01-11        39.2
200          200 2000-01-11        38.8
201          201 2000-01-11        39.6
202          202 2000-01-09        38.7
203          203 2000-01-11        39.4
204          204 2000-01-11        39.1
205          205 2000-01-11        40.7
206          206 2000-01-10        39.4
207          207 2000-01-11        39.0
208          208 2000-01-12        38.9
209          209 2000-01-11        39.7
210          210 2000-01-12        38.6
211          211 2000-01-10        39.7
212          212 2000-01-10        39.1
213          213 2000-01-11        38.6
214          214 2000-01-10        39.9
215          215 2000-01-13        39.4
216          216 2000-01-12        39.7
217          217 2000-01-11        39.2
218          218 2000-01-14        39.4
219          219 2000-01-14        40.3
220          220 2000-01-14        38.7
221          221 2000-01-11        38.9
222          222 2000-01-13        38.7
223          223 2000-01-12        40.2
224          224 2000-01-14        39.7
225          225 2000-01-15        39.2
226          226 2000-01-12        39.5
227          227 2000-01-12        40.0
228          228 2000-01-11        39.2
229          229 2000-01-15        39.4
230          230 2000-01-12        39.1
231          231 2000-01-11        38.5
232          232 2000-01-12        38.7
233          233 2000-01-11        39.1
234          234 2000-01-12        39.4
235          235 2000-01-11        39.5
236          236 2000-01-09        38.5
237          237 2000-01-11        39.4
238          238 2000-01-13        39.6
239          239 2000-01-12        39.6
240          240 2000-01-10        38.9
241          241 2000-01-12        39.9
242          242 2000-01-13        38.6
243          243 2000-01-11        40.1
244          244 2000-01-15        39.2
245          245 2000-01-10        39.5
246          246 2000-01-15        39.4
247          247 2000-01-11        38.5
248          248 2000-01-14        39.9
249          249 2000-01-12        39.3
250          250 2000-01-12        38.8
251          251 2000-01-12        39.0
252          252 2000-01-15        40.3
253          253 2000-01-11        40.0
254          254 2000-01-12        39.4
255          255 2000-01-11        39.7
256          256 2000-01-14        40.2
257          257 2000-01-13        39.8
258          258 2000-01-13        39.6
259          259 2000-01-13        38.7
260          260 2000-01-12        39.2
261          261 2000-01-15        39.5
262          262 2000-01-15        38.7
263          263 2000-01-12        39.3
264          264 2000-01-12        39.4
265          265 2000-01-13        39.0
266          266 2000-01-13        39.5
267          267 2000-01-16        38.8
268          268 2000-01-11        39.2
269          269 2000-01-12        39.3
270          270 2000-01-12        38.5
271          271 2000-01-12        39.9
272          272 2000-01-12        38.5
273          273 2000-01-13        39.4
274          274 2000-01-12        39.4
275          275 2000-01-11        39.9
276          276 2000-01-14        38.6
277          277 2000-01-16        39.6
278          278 2000-01-11        39.3
279          279 2000-01-13        39.3
280          280 2000-01-11        39.7
281          281 2000-01-13        38.6
282          282 2000-01-12        39.8
283          283 2000-01-13        38.5
284          284 2000-01-12        39.7
285          285 2000-01-14        39.5
286          286 2000-01-12        38.8
287          287 2000-01-13        39.7
288          288 2000-01-14        39.9
289          289 2000-01-13        39.7
290          290 2000-01-12        40.3
291          291 2000-01-13        40.1
292          292 2000-01-13        38.6
293          293 2000-01-13        40.2
294          294 2000-01-12        39.9
295          295 2000-01-11        40.4
296          296 2000-01-13        39.7
297          297 2000-01-13        39.9
298          298 2000-01-14        39.7
299          299 2000-01-11        40.2
300          300 2000-01-13        39.2
301          301 2000-01-14        39.0
302          302 2000-01-13        39.5
303          303 2000-01-12        40.5
304          304 2000-01-11        40.1
305          305 2000-01-13        38.7
306          306 2000-01-13        39.8
307          307 2000-01-11        38.6
308          308 2000-01-13        38.8
309          309 2000-01-14        38.9
310          310 2000-01-12        39.2
311          311 2000-01-11        39.6
312          312 2000-01-13        39.8
313          313 2000-01-14        40.4
314          314 2000-01-14        39.6
315          315 2000-01-13        39.9
316          316 2000-01-16        39.7
317          317 2000-01-12        40.0
318          318 2000-01-10        39.5
319          319 2000-01-13        39.5
320          320 2000-01-12        39.3
321          321 2000-01-11        40.2
322          322 2000-01-13        39.5
323          323 2000-01-11        40.2
324          324 2000-01-15        39.3
325          325 2000-01-14        39.5
326          326 2000-01-13        39.9
327          327 2000-01-12        40.2
328          328 2000-01-13        40.7
329          329 2000-01-10        39.5
330          330 2000-01-11        39.9
331          331 2000-01-12        39.0
332          332 2000-01-11        39.5
333          333 2000-01-13        39.9
334          334 2000-01-12        39.8
335          335 2000-01-13        39.9
336          336 2000-01-14        39.4
337          337 2000-01-10        40.1
338          338 2000-01-11        39.3
339          339 2000-01-16        38.5
340          340 2000-01-13        39.2
341          341 2000-01-12        40.8
342          342 2000-01-12        39.4
343          343 2000-01-10        39.7
344          344 2000-01-15        39.7
345          345 2000-01-10        39.7
346          346 2000-01-12        39.8
347          347 2000-01-15        39.5
348          348 2000-01-11        39.6
349          349 2000-01-12        40.8
350          350 2000-01-15        39.6
351          351 2000-01-17        39.9
352          352 2000-01-12        40.4
353          353 2000-01-16        39.0
354          354 2000-01-14        39.9
355          355 2000-01-12        39.9
356          356 2000-01-11        38.8
357          357 2000-01-13        39.9
358          358 2000-01-15        39.3
359          359 2000-01-15        39.8
360          360 2000-01-13        39.2
361          361 2000-01-11        39.0
362          362 2000-01-10        38.7
363          363 2000-01-14        39.9
364          364 2000-01-11        39.0
365          365 2000-01-11        39.2
366          366 2000-01-12        39.0
367          367 2000-01-16        39.4
368          368 2000-01-15        39.5
369          369 2000-01-12        38.7
370          370 2000-01-12        39.3
371          371 2000-01-13        39.9
372          372 2000-01-11        38.6
373          373 2000-01-12        38.6
374          374 2000-01-13        39.0
375          375 2000-01-17        39.2
376          376 2000-01-13        40.1
377          377 2000-01-15        39.2
378          378 2000-01-11        38.6
379          379 2000-01-12        39.1
380          380 2000-01-13        38.9
381          381 2000-01-13        39.6
382          382 2000-01-13        39.6
383          383 2000-01-14        39.2
384          384 2000-01-11        38.8
385          385 2000-01-15        40.3
386          386 2000-01-14        39.9
387          387 2000-01-15        39.5
388          388 2000-01-13        39.6
389          389 2000-01-12        39.9
390          390 2000-01-10        39.8
391          391 2000-01-13        39.4
392          392 2000-01-13        39.6
393          393 2000-01-12        39.8
394          394 2000-01-12        39.5
395          395 2000-01-13        39.9
396          396 2000-01-10        39.6
397          397 2000-01-13        38.7
398          398 2000-01-14        40.9
399          399 2000-01-12        40.1
400          400 2000-01-11        40.1
401          401 2000-01-10        40.1
402          402 2000-01-11        39.6
403          403 2000-01-13        40.3
404          404 2000-01-11        39.4
405          405 2000-01-15        39.9
406          406 2000-01-15        39.6
407          407 2000-01-12        40.5
408          408 2000-01-12        39.2
409          409 2000-01-12        40.8
410          410 2000-01-14        39.4
411          411 2000-01-16        40.4
412          412 2000-01-12        39.0
413          413 2000-01-10        39.5
414          414 2000-01-14        40.1
415          415 2000-01-13        40.5
416          416 2000-01-13        39.4
417          417 2000-01-13        39.0
418          418 2000-01-13        39.3

> names(x@records)
[1] "Fever"
> head(x@records$Fever)
  individualID       date temperature
1            1 2000-01-03        39.1
2            2 2000-01-03        40.4
3            3 2000-01-07        40.0
4            4 2000-01-08        39.8
5            5 2000-01-04        39.4
6            6 2000-01-06        39.3
> x@contacts
 Number of individuals = 20
 Number of contacts = 19
 Contacts = dynamic
NetworkDynamic properties:
  distinct change times: 5 
  maximal time range: 0 until  4 

 Network attributes:
  vertices = 20 
  directed = FALSE 
  hyper = FALSE 
  loops = FALSE 
  multiple = TRUE 
  bipartite = FALSE 
  total edges= 19 
    missing edges= 0 
    non-missing edges= 19 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names: 
    active 

Date of origin: [1] "2000-01-01"
> x@dna
= @dna =
[ 836 DNA sequences in 2 loci ]
$gene1
418 DNA sequences in binary format stored in a matrix.

All sequences of same length: 600 

Labels: 1 2 3 4 5 6 ...

Base composition:
    a     c     g     t 
0.237 0.248 0.252 0.263 

$gene2
418 DNA sequences in binary format stored in a matrix.

All sequences of same length: 1000 

Labels: 419 420 421 422 423 424 ...

Base composition:
    a     c     g     t 
0.223 0.243 0.257 0.276 


= @meta =
[ meta information on the sequences ]
  individualID       date locus sample
1            1 2000-01-01 gene1      1
2            2 2000-01-02 gene1      2
3            3 2000-01-03 gene1      3
4            4 2000-01-03 gene1      4

...
    individualID       date locus sample
833          415 2000-01-10 gene2    415
834          416 2000-01-10 gene2    416
835          417 2000-01-10 gene2    417
836          418 2000-01-10 gene2    418
> x@trees
1 phylogenetic trees
> 
> ## Plotting the dynamic contact network ##
> par(mfrow=c(2,2))
> plot(get.contacts(x),main="Contact network - days 0-3",displaylabels=TRUE)
> plot(get.contacts(x, from=0, to=1.1), main="Contact network - days 0-1",
+      displaylabels=TRUE)
> plot(get.contacts(x, from=2, to=2.1), main="Contact network - day 2",
+      displaylabels=TRUE)
> plot(get.contacts(x, from=3, to=3.1), main="Contact network - day 3",
+      displaylabels=TRUE)
> 
> ## Mapping the outbreak (by sex) ##
> plot(x,'geo',location=c('lon','lat'),isLonLat=TRUE,zoom=13,colorBy='Sex')
Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=51.516439,-0.171146&zoom=13&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
Error in layer(mapping = NULL, data = NULL, stat = "identity", geom = <environment>,  : 
  unused argument (geom_params = list(raster = c("#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#EEEAE4", "#FEFEFE", "#F4F2ED", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#ECE8E1", "#F9F4F2", "#FEFEFE", "#F9F8F4", "#ECE8E1", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#FEFEFE", "#EEEDE5", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", 
"#E9E5DC", "#E9E5DC", "#E9E5DC", "#EBE8E0", "#EBE8E0", "#F9F9F8", "#F2F0EA", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#EEEAE4", "#F4F2ED", "#F4F2ED", "#F4F2ED", "#8F7B77", "#674C44", "#F4F2ED", "#EEEAE4", "#674C44", "#836B63", "#F9F8F4", "#FEFEFE", "#FEFEFE", "#FEFEFE", "#FEFEFE", "#FEFEFE", "#EEEAE4", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5DC", "#E9E5D
Calls: plot ... inset_raster -> <Anonymous> -> <Anonymous> -> do.call -> layer
Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Warning: replacing previous import by 'grid::unit' when loading 'ggmap'
Warning: replacing previous import by 'scales::alpha' when loading 'ggmap'
Warning in .local(x, ...) :
  The following sequence IDs are not in the dataset: 311, 222
Warning in .local(x, ...) :
  The following sequence IDs are not in the dataset: 311, 222
Warning in (if (out_format(c("latex", "sweave", "listings"))) sanitize_fn else paste0)(path,  :
  replaced special characters in figure filename "figs/OutbreakTools-out.width==".8\\textwidth"" -> "figs/OutbreakTools-out.width___.8\\textwidth_"
Warning in (if (out_format(c("latex", "sweave", "listings"))) sanitize_fn else paste0)(path,  :
  dots in figure paths replaced with _ ("figs/OutbreakTools-out_width____8//textwidth_")
Warning: The shape palette can deal with a maximum of 6 discrete values because
more than 6 becomes difficult to discriminate; you have 11. Consider
specifying shapes manually if you must have them.
Warning: Removed 58 rows containing missing values (geom_point).
Warning: The shape palette can deal with a maximum of 6 discrete values because
more than 6 becomes difficult to discriminate; you have 11. Consider
specifying shapes manually if you must have them.
Warning: `show_guide` has been deprecated. Please use `show.legend` instead.
Warning: `show_guide` has been deprecated. Please use `show.legend` instead.
Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=51.516439,-0.171146&zoom=14&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
Quitting from lines 1021-1022 (OutbreakTools.Rnw) 
Error: processing vignette 'OutbreakTools.Rnw' failed with diagnostics:
unused argument (geom_params = list(raster = c("#E4E0D7", "#E5E1D9", "#E9E5DD", "#F1F0EA", "#FEFEFE", "#FEFEFE", "#E8E4DB", "#E9E5DD", "#ECE8E0", "#EEEAE4", "#EFEEE6", "#F4F2ED", "#F4F2ED", "#F9F8F5", "#F9F8F5", "#FCFCF9", "#FEFEFE", "#FEFEFE", "#FEFEFE", "#FEFEFE", "#FEFEFE", "#FEFEFE", "#FCFCF9", "#F9F8F5", "#F5F5F2", "#F4F2ED", "#F3F1EC", "#EEEAE4", "#EEEAE4", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", 
"#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E9E5DD", "#E8E4DB", "#E8E4DB", "#E8E4DB", "#E8E4DB", "#E8E4DB", "#E8E4DB", "#E8E4DB", "#E8E4DB", "#E8E4DB", "#E8E4DB",
Execution halted

```
```
DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE
```

## P2C2M (0.7.6)
Maintainer: Michael Gruenstaeudl <mi.gruenstaeudl@gmail.com>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘genealogicalSorting’ ‘phybase’ ‘Rmpi’
```
```
checking data for non-ASCII characters ... NOTE
  Note: found 490 marked UTF-8 strings
```
```
DONE
Status: 2 NOTEs
```

## pa (1.2-1)
Maintainer: Yang Lu <yang.lu2014@gmail.com>

```
checking dependencies in R code ... NOTE
Package in Depends field not imported from: ‘grid’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
DONE
Status: 1 NOTE
```

## packcircles (0.1.1)
Maintainer: Michael Bedward <michael.bedward@gmail.com>

__OK__

## PairedData (1.0.1)
Maintainer: Stephane Champely <champely@univ-lyon1.fr>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls to packages already attached by Depends:
  ‘MASS’ ‘gld’ ‘mvtnorm’
  Please remove these calls from your code.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  bonettseier.var.test.default bonettseier.var.test.paired
  grambsch.var.test.default grambsch.var.test.paired
  imam.var.test.default imam.var.test.paired levene.var.test.default
  levene.var.test.paired mcculloch.var.test.default
  mcculloch.var.test.paired sandvikolsson.var.test.default
  sandvikolsson.var.test.paired t.test.paired t.test.paired
  var.test.default var.test.paired wilcox.test.paired
  winsor.cor.test.default winsor.cor.test.paired yuen.t.test.default
  yuen.t.test.formula yuen.t.test.paired
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking Rd line widths ... NOTE
Rd file 'mcculloch.var.test.Rd':
  \usage lines wider than 90 characters:
            alternative = c("two.sided", "less", "greater"),method= c("spearman","pearson", "kendall"),

Rd file 'plot.Rd':
  \usage lines wider than 90 characters:
     plot(x, groups=NULL,subjects=NULL, facet=TRUE,type=c("correlation","BA","McNeil","profile"),...)

These lines will be truncated in the PDF manual.
```
```
DONE
Status: 3 NOTEs
```

## paleofire (1.1.6)
Maintainer: Olivier Blarquez <blarquez@gmail.com>

__OK__

## palettetown (0.1.0)
Maintainer: Tim Lucas <timcdlucas@gmail.com>

__OK__

## pander (0.5.2)
Maintainer: Gergely Daróczi <daroczig@rapporter.net>  
Bug reports: https://github.com/rapporter/pander/issues

```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  pander.option pander.return
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking Rd \usage sections ... NOTE
S3 methods shown with full name in documentation object 'pander.option':
  ‘pander.option’

S3 methods shown with full name in documentation object 'pander.return':
  ‘pander.return’

The \usage entries for S3 methods should use the \method markup and not
their full name.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
```
```
DONE
Status: 2 NOTEs
```

## partialAR (1.0.5)
Maintainer: Matthew Clegg <matthewcleggphd@gmail.com>

```
checking whether package ‘partialAR’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘partialAR’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘partialAR’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/partialAR.Rcheck/00install.out’ for details.
```
```
checking R code for possible problems ... NOTE
Warning: replacing previous import by ‘grid::arrow’ when loading ‘partialAR’
Warning: replacing previous import by ‘grid::unit’ when loading ‘partialAR’
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## PASWR2 (1.0)
Maintainer: Alan T. Arnholt <arnholtat@appstate.edu>

```
checking whether package ‘PASWR2’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘PASWR2’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘PASWR2’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/PASWR2.Rcheck/00install.out’ for details.
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking examples ... ERROR
Running examples in ‘PASWR2-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: APTSIZE
> ### Title: Apartment Size
> ### Aliases: APTSIZE
> ### Keywords: datasets
> 
> ### ** Examples
> 
> p <- ggplot(data = APTSIZE, aes(x = location, y = size, fill = location)) +
+ labs(x = "", y = "Apartment size (square meters)") +
+ scale_x_discrete(breaks = c("Mendebaldea", "SanJorge"),
+ labels =c("Mendebaldea", "San Jorge")) + scale_fill_brewer()
> p + geom_boxplot()
> # remove the legend
> p + geom_boxplot() + guides(fill = FALSE)
> # violin plot
> p + geom_violin(scale = 'equal') + guides(fill = FALSE)
Error in eval(expr, envir, enclos) : object 'violinwidth' not found
Calls: print ... f -> transform -> transform.data.frame -> eval -> eval
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE
```

## patPRO (1.0.0)
Maintainer: Geoffrey D Hannigan <ghanni@upenn.edu>

```
checking whether package ‘patPRO’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘patPRO’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘patPRO’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/patPRO.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘patPRO-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: patproPlotThree
> ### Title: Generate Patient Profile of Three Plots.
> ### Aliases: patproPlotThree
> ### Keywords: package
> 
> ### ** Examples
> 
> # Plot individual patient
> data("PatProAlphaDiv",package="patPRO")
> data("PatProMap",package="patPRO")
> data("PatProOTU",package="patPRO")
> data("PatProBacLoad",package="patPRO")
> # Alpha Diversity
> mergedMapAlpha <- mergeMapMetaData(map.file=PatProMap, 
+   merging.file=PatProAlphaDiv, 
+   map.sub.id="SubjectID", 
+   map.tmpt="Time_point", 
+   map.smpl.id="SampleID", 
+   sample.id.col="SampleID")
> testNormAlphaDiv <- normalizeAlphaDiv(mergedMapAlpha, c("chao1","shannon"), 1)
No id variables; using all as measure variables
> alphaDivPlot <- plotNormalizedAlphaDiv(testNormAlphaDiv, 
+   c("chao1","shannon"), 
+   plot.title="Subject One Diversity", 
+   color.brewer.set="Set2", 
+   legend.text.size = 12)
> # Bacterial Load
> mergedMapBacLoad <- mergeMapMetaData(map.file=PatProMap, 
+   merging.file=PatProBacLoad, 
+   map.sub.id="SubjectID", 
+   map.tmpt="Time_point", 
+   map.smpl.id="SampleID", 
+   sample.id.col="SampleID")
> bacLoad <- plotBacterialLoad(mergedMapBacLoad, 
+   1, 
+   bac.load.col="Num_Bacteria", 
+   plot.title="Subject One Bacterial Load")
> # Taxa Relative Abundance
> transTestRelAbund <- transposeRelAbund(PatProOTU)
> mergedMapTransRA <- mergeMapMetaData(map.file=PatProMap, 
+   merging.file=transTestRelAbund, 
+   map.sub.id="SubjectID", 
+   map.tmpt="Time_point", 
+   map.smpl.id="SampleID", 
+   sample.id.col="SampleID")
> top5RelAbund <- topRelAbundDataFrame(x=mergedMapTransRA, top.taxa.num=5)
Using SampleID as id variables
> topTaxa <- plotTopTaxa(top5RelAbund, 
+   1, 
+   plot.title="Subject One Taxonomy", 
+   color.brewer.set="Set2", 
+   mark.events=TRUE, 
+   mark.times=c(2,6), 
+   mark.text="Surgery", 
+   legend.text.size=8)
Error: Unknown parameters: width
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## pauwels2014 (1.0)
Maintainer: Edouard Pauwels <pauwelsed@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
DONE
Status: 1 NOTE
```

## PAWL (0.5)
Maintainer: Pierre Jacob <pierre.jacob.work@gmail.com>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls to packages already attached by Depends:
  ‘foreach’ ‘ggplot2’ ‘reshape’
  Please remove these calls from your code.
Packages in Depends field not imported from:
  ‘foreach’ ‘ggplot2’ ‘methods’ ‘mvtnorm’ ‘reshape’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
ConvertResults: no visible global function definition for ‘%do%’
ConvertResults: no visible global function definition for ‘foreach’
PlotAllVar: no visible global function definition for ‘melt’
PlotAllVar: no visible global function definition for ‘ggplot’
PlotAllVar: no visible global function definition for ‘aes’
PlotAllVar: no visible global function definition for ‘geom_line’
PlotAllVar: no visible global function definition for ‘facet_wrap’
PlotAllVar: no visible global function definition for ‘theme’
PlotAllVar: no visible global function definition for ‘element_text’
PlotAllVar: no visible global function definition for ‘labs’
PlotComp1vsComp2: no visible global function definition for ‘ggplot’
PlotComp1vsComp2: no visible global function definition for
  ‘aes_string’
PlotComp1vsComp2: no visible global function definition for
  ‘geom_point’
PlotComp1vsComp2: no visible global function definition for ‘aes’
PlotComp1vsComp2: no visible global function definition for ‘xlab’
PlotComp1vsComp2: no visible global function definition for ‘ylab’
PlotComp1vsComp2: no visible global function definition for ‘labs’
PlotDensComp1vsComp2: no visible global function definition for
  ‘ggplot’
PlotDensComp1vsComp2: no visible global function definition for
  ‘aes_string’
PlotDensComp1vsComp2: no visible global function definition for
  ‘stat_bin2d’
PlotDensComp1vsComp2: no visible global function definition for
  ‘geom_density2d’
PlotDensComp1vsComp2: no visible global function definition for ‘theme’
PlotFH: no visible global function definition for ‘ggplot’
PlotFH: no visible global function definition for ‘aes’
PlotFH: no visible global function definition for ‘geom_step’
PlotFH: no visible global function definition for ‘xlab’
PlotFH: no visible global function definition for ‘ylab’
PlotFH: no visible global function definition for ‘theme’
PlotLogTheta: no visible global function definition for ‘%do%’
PlotLogTheta: no visible global function definition for ‘foreach’
PlotLogTheta: no visible global function definition for ‘melt’
PlotLogTheta: no visible global function definition for ‘ggplot’
PlotLogTheta: no visible global function definition for ‘aes’
PlotLogTheta: no visible global function definition for ‘geom_line’
PlotLogTheta: no visible global function definition for ‘geom_vline’
PlotLogTheta: no visible global function definition for ‘theme’
PlotNbins: no visible global function definition for ‘ggplot’
PlotNbins: no visible global function definition for ‘aes_string’
PlotNbins: no visible global function definition for ‘geom_step’
PlotNbins: no visible global function definition for ‘ylim’
PlotNbins: no visible global function definition for ‘ylab’
PlotNbins: no visible global function definition for ‘xlab’
PlotNbins: no visible global function definition for ‘theme’
createTrimodalTarget : generate: no visible global function definition
  for ‘rmvnorm’
```
```
checking Rd line widths ... NOTE
Rd file 'createAdaptiveRandomWalkProposal.Rd':
  \usage lines wider than 90 characters:
         createAdaptiveRandomWalkProposal(nchains, targetdimension, adaptiveproposal, adaptationrate, sigma_init)

These lines will be truncated in the PDF manual.
```
```
DONE
Status: 3 NOTEs
```

## pbdPROF (0.2-3)
Maintainer: Wei-Chen Chen <wccsnow@gmail.com>  
Bug reports: http://group.r-pbd.org/

```
checking whether package ‘pbdPROF’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/pbdPROF.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## PBImisc (0.999)
Maintainer: Przemyslaw Biecek <przemyslaw.biecek@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking dependencies in R code ... NOTE
Packages in Depends field not imported from:
  ‘MASS’ ‘Matrix’ ‘lme4’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
groupDisp: no visible global function definition for ‘lmer’
groupDisp : <anonymous>: no visible global function definition for
  ‘lmer’
groupDisp : <anonymous>: no visible global function definition for
  ‘fixef’
groupDisp : <anonymous>: no visible global function definition for
  ‘VarCorr’
obsDisp: no visible global function definition for ‘lmer’
obsDisp : <anonymous>: no visible global function definition for ‘lmer’
obsDisp : <anonymous>: no visible global function definition for
  ‘fixef’
obsDisp : <anonymous>: no visible global function definition for
  ‘VarCorr’
recalculateLogLik: no visible global function definition for ‘VarCorr’
recalculateLogLik: no visible global function definition for ‘getME’
recalculateLogLik: no visible global function definition for ‘Matrix’
recalculateLogLik: no visible global function definition for ‘Diagonal’
```
```
DONE
Status: 3 NOTEs
```

## PDQutils (0.1.2)
Maintainer: Steven E. Pav <shabbychef@gmail.com>  
Bug reports: https://github.com/shabbychef/PDQutils/issues

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Warning in (function (source = "clipboard", comment = getOption("formatR.comment",  :
  The argument 'keep.blank.line' is deprecated; please use 'blank'
Warning in (function (source = "clipboard", comment = getOption("formatR.comment",  :
  The argument 'keep.blank.line' is deprecated; please use 'blank'
Warning in (function (source = "clipboard", comment = getOption("formatR.comment",  :
  The argument 'keep.blank.line' is deprecated; please use 'blank'
Warning in (function (source = "clipboard", comment = getOption("formatR.comment",  :
  The argument 'keep.blank.line' is deprecated; please use 'blank'
Warning in (function (source = "clipboard", comment = getOption("formatR.comment",  :
  The argument 'keep.blank.line' is deprecated; please use 'blank'
Warning in (function (source = "clipboard", comment = getOption("formatR.comment",  :
  The argument 'keep.blank.line' is deprecated; please use 'blank'
large z may result in inaccurate quantiles
Warning in (function (source = "clipboard", comment = getOption("formatR.comment",  :
  The argument 'keep.blank.line' is deprecated; please use 'blank'
Loading required package: polynom
Warning: `stat` is deprecated
Quitting from lines 844-849 (PDQutils.Rnw) 
Error: processing vignette 'PDQutils.Rnw' failed with diagnostics:
geom_step requires the following missing aesthetics: y
Execution halted

```
```
DONE
Status: 1 NOTE
```

## PedCNV (0.1)
Maintainer: Meiling Liu <meiling.sta@gmail.com>

```
checking whether package ‘PedCNV’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/PedCNV.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## peptider (0.2.2)
Maintainer: Eric Hare <erichare@iastate.edu>  
Bug reports: https://github.com/heike/peptider/issues

__OK__

## pequod (0.0-4)
Maintainer: Alberto Mirisola <alberto.mirisola@gmail.com>

__OK__

## performanceEstimation (1.0.2)
Maintainer: Luis Torgo <ltorgo@dcc.fc.up.pt>  
Bug reports: https://github.com/ltorgo/performanceEstimation/issues

__OK__

## perry (0.2.0)
Maintainer: Andreas Alfons <alfons@ese.eur.nl>

__OK__

## perspectev (1.1)
Maintainer: Kenneth B. Hoehn <perspectev@gmail.com>

__OK__

## PhaseType (0.1.3)
Maintainer: Louis Aslett <louis@maths.tcd.ie>

```
checking whether package ‘PhaseType’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/PhaseType.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## physiology (0.2.2)
Maintainer: Jack O. Wasey <jack@jackwasey.com>  
Bug reports: https://github.com/jackwasey/physiology/issues

__OK__

## pid (0.36)
Maintainer: Kevin Dunn <kevin.dunn@mcmaster.ca>  
Bug reports: https://bitbucket.org/kevindunn/r-pid

__OK__

## pipe.design (0.3)
Maintainer: Michael Sweeting <mjs212@medschl.cam.ac.uk>

__OK__

## pitchRx (1.8)
Maintainer: Carson Sievert <sievert@iastate.edu>  
Bug reports: http://github.com/cpsievert/pitchRx/issues

__OK__

## PKgraph (1.7)
Maintainer: Xiaoyong Sun <johnsunx1@gmail.com>

```
checking package dependencies ... ERROR
Package required but not available: ‘rggobi’

Depends: includes the non-default packages:
  ‘RGtk2’ ‘gWidgetsRGtk2’ ‘cairoDevice’ ‘lattice’ ‘rggobi’ ‘ggplot2’
  ‘proto’
Adding so many packages to the search path is excessive and importing
selectively is preferable.

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## PKreport (1.5)
Maintainer: Xiaoyong Sun <johnsunx1@gmail.com>

__OK__

## planar (1.5.2)
Maintainer: Baptiste Auguie <baptiste.auguie@gmail.com>

```
checking whether package ‘planar’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/planar.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## playwith (0.9-54)
Maintainer: Felix Andrews <felix@nfrac.org>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘latticist’
```
```
checking dependencies in R code ... WARNING
'library' or 'require' call to ‘lattice’ which was already attached by Depends.
  Please remove these calls from your code.
'library' or 'require' call to ‘latticist’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Packages in Depends field not imported from:
  ‘cairoDevice’ ‘gWidgetsRGtk2’ ‘grid’ ‘lattice’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
Unexported objects imported by ':::' calls:
  ‘lattice:::lattice.getStatus’ ‘stats:::biplot.prcomp’
  See the note in ?`:::` about the use of this operator.
  Including base/recommended package(s):
  ‘stats’ ‘lattice’
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  plotCoords.biplot plotCoords.plot
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
Found an obsolete/platform-specific call in the following functions:
  ‘copy_handler’ ‘print_handler’
Found the platform-specific devices:
  ‘win.metafile’ ‘win.print’
dev.new() is the preferred way to open a new device, in the unlikely
event one is needed.
.defaultPlaywithOptions: no visible global function definition for
  ‘trellis.par.set’
.defaultPlaywithOptions: no visible global function definition for
  ‘col.whitebg’
.defaultPlaywithOptions: no visible global function definition for
  ‘standard.theme’
.defaultPlaywithOptions: no visible global function definition for
  ‘custom.theme’
.defaultPlaywithOptions: no visible global function definition for
  ‘custom.theme.2’
.defaultPlaywithOptions: no visible global function definition for
  ‘custom.theme.black’
.defaultPlaywithOptions: no visible global function definition for
  ‘brewer.pal’
.defaultPlaywithOptions: no visible global function definition for
  ‘lattice.options’
annotateCore: no visible global function definition for
  ‘trellis.par.get’
annotateCore : annot_handler: no visible global function definition for
  ‘convertWidth’
annotateCore : annot_handler: no visible global function definition for
  ‘unit’
annotateCore : annot_handler: no visible global function definition for
  ‘convertHeight’
annotateCore : annot_handler: no visible global function definition for
  ‘trellis.par.set’
autoplay: no visible global function definition for ‘lattice.options’
convertFromDevicePixels: no visible global function definition for
  ‘current.transform’
convertFromDevicePixels: no visible global function definition for
  ‘unit’
convertFromDevicePixels: no visible global function definition for
  ‘convertX’
convertFromDevicePixels: no visible global function definition for
  ‘convertY’
convertToDevicePixels: no visible global function definition for
  ‘is.unit’
convertToDevicePixels: no visible global function definition for ‘unit’
convertToDevicePixels: no visible global function definition for
  ‘convertX’
convertToDevicePixels: no visible global function definition for
  ‘convertY’
convertToDevicePixels: no visible global function definition for
  ‘current.transform’
copy_handler: no visible binding for global variable ‘Cairo_png’
createStyleActions : do.theme_handler: no visible global function
  definition for ‘trellis.par.set’
current.brush.line: no visible global function definition for
  ‘trellis.par.get’
current.brush.symbol: no visible global function definition for
  ‘trellis.par.get’
current.user.text: no visible global function definition for
  ‘trellis.par.get’
decr.font_handler: no visible global function definition for
  ‘trellis.par.set’
doPlayReplot: no visible global function definition for ‘grid.newpage’
doPlayReplot: no visible global function definition for
  ‘trellis.currentLayout’
doPlayReplot: no visible global function definition for
  ‘trellis.par.get’
doPlayReplot: no visible global function definition for
  ‘trellis.par.set’
doPlayReplot: no visible global function definition for ‘grid.ls’
expand_handler: no visible global function definition for
  ‘trellis.focus’
expand_handler: no visible global function definition for
  ‘packet.number’
generateSpaces: no visible global function definition for ‘upViewport’
generateSpaces: no visible global function definition for ‘grid.ls’
generateSpaces: no visible global function definition for
  ‘downViewport’
generateSpaces: no visible global function definition for ‘popViewport’
generateSpaces: no visible global function definition for ‘viewport’
generateSpaces: no visible global function definition for
  ‘pushViewport’
generateSpaces: no visible global function definition for
  ‘trellis.vpname’
generateSpaces: no visible global function definition for ‘unit’
grob.inspector_handler: no visible global function definition for
  ‘grid.refresh’
grob.inspector_handler : <anonymous>: no visible global function
  definition for ‘grid.remove’
grob.inspector_handler : <anonymous>: no visible global function
  definition for ‘grid.get’
grobBBDevicePixels: no visible global function definition for
  ‘current.vpPath’
grobBBDevicePixels: no visible global function definition for
  ‘upViewport’
grobBBDevicePixels: no visible global function definition for
  ‘downViewport’
grobBBDevicePixels: no visible global function definition for ‘unit.c’
grobBBDevicePixels: no visible global function definition for ‘grobX’
grobBBDevicePixels: no visible global function definition for ‘grobY’
grobBoundingBoxes: no visible global function definition for ‘gpar’
grobBoundingBoxes: no visible global function definition for
  ‘current.vpPath’
grobBoundingBoxes: no visible global function definition for
  ‘upViewport’
grobBoundingBoxes: no visible global function definition for
  ‘downViewport’
grobBoundingBoxes: no visible global function definition for ‘grid.ls’
grobBoundingBoxes: no visible binding for global variable ‘type’
grobBoundingBoxes: no visible global function definition for ‘grid.get’
grobBoundingBoxes: no visible global function definition for
  ‘grid.rect’
grobBoundingBoxes: no visible global function definition for
  ‘grid.text’
grobBoundingBoxes: no visible global function definition for
  ‘grid.remove’
identifyCore: no visible global function definition for ‘convertX’
identifyCore: no visible global function definition for ‘unit’
identifyCore: no visible global function definition for ‘convertY’
identifyGrob: no visible global function definition for ‘grid.locator’
inViewport: no visible global function definition for ‘current.vpPath’
inViewport: no visible global function definition for ‘upViewport’
inViewport: no visible global function definition for ‘downViewport’
inViewport: no visible global function definition for ‘unit’
incr.font_handler: no visible global function definition for
  ‘trellis.par.set’
panel.brushlines: no visible global function definition for
  ‘panel.lines’
panel.brushpoints: no visible global function definition for
  ‘panel.points’
panel.usertext: no visible global function definition for ‘panel.text’
playDo: no visible global function definition for ‘packet.number’
playDo: no visible global function definition for ‘trellis.vpname’
playDo: no visible global function definition for ‘current.vpPath’
playDo: no visible global function definition for ‘upViewport’
playDo: no visible global function definition for ‘downViewport’
playDo: no visible global function definition for ‘seekViewport’
playLineInput: no visible global function definition for
  ‘current.vpPath’
playLineInput: no visible global function definition for ‘upViewport’
playLineInput: no visible global function definition for ‘downViewport’
playLineInput: no visible global function definition for ‘grid.locator’
playPointInput: no visible global function definition for
  ‘current.vpPath’
playPointInput: no visible global function definition for ‘upViewport’
playPointInput: no visible global function definition for
  ‘downViewport’
playPointInput: no visible global function definition for
  ‘grid.locator’
playPointInput: no visible global function definition for ‘convertX’
playPointInput: no visible global function definition for ‘convertY’
playRectInput: no visible global function definition for
  ‘current.vpPath’
playRectInput: no visible global function definition for ‘upViewport’
playRectInput: no visible global function definition for ‘downViewport’
playRectInput: no visible global function definition for ‘grid.locator’
playwith: no visible global function definition for ‘asCairoDevice’
playwith.trellis: no visible binding for global variable
  ‘packet.panel.default’
plotCoords.cloud : <anonymous>: no visible global function definition
  for ‘ltransform3dto3d’
plotCoords.plot.SpatialPoints: no visible global function definition
  for ‘coordinates’
plotCoords.plot.SpatialPointsDataFrame: no visible global function
  definition for ‘coordinates’
plotCoords.splom: no visible global function definition for
  ‘current.vpPath’
plotCoords.splom: no visible global function definition for
  ‘upViewport’
plotCoords.splom: no visible global function definition for
  ‘downViewport’
plotCoords.splom: no visible global function definition for
  ‘trellis.vpname’
plotCoords.splom: no visible global function definition for ‘convertX’
plotCoords.splom: no visible global function definition for ‘unit’
plotCoords.splom: no visible global function definition for ‘convertY’
plotOnePage : packet.panel.pageN: no visible global function definition
  for ‘packet.panel.default’
rawXYLim: no visible global function definition for ‘packet.number’
rawXYLim: no visible global function definition for ‘convertX’
rawXYLim: no visible global function definition for ‘unit’
rawXYLim: no visible global function definition for ‘convertY’
rotate3DCore: no visible global function definition for
  ‘ltransform3dMatrix’
set.arrow.style_handler: no visible global function definition for
  ‘trellis.par.get’
set.arrow.style_handler : ok_handler: no visible global function
  definition for ‘trellis.par.set’
set.brush.style_handler : ok_handler: no visible global function
  definition for ‘trellis.par.set’
set.default.theme_handler: no visible global function definition for
  ‘trellis.par.set’
set.default.theme_handler: no visible global function definition for
  ‘standard.theme’
set.label.style_handler : ok_handler: no visible global function
  definition for ‘trellis.par.set’
set.point.line.style_handler: no visible global function definition for
  ‘trellis.par.get’
set.point.line.style_handler : ok_handler: no visible global function
  definition for ‘trellis.par.set’
style.settings_handler: no visible global function definition for
  ‘latticeStyleGUI’
style.solid.points_handler: no visible global function definition for
  ‘trellis.par.set’
style.solid.points_handler: no visible global function definition for
  ‘simpleTheme’
style.thick.lines_handler: no visible global function definition for
  ‘trellis.par.set’
style.trans.points_handler: no visible global function definition for
  ‘trellis.par.set’
style.trans.points_handler: no visible global function definition for
  ‘simpleTheme’
time.mode_entry_handler: no visible global function definition for
  ‘as.yearmon’
time.mode_entry_handler: no visible global function definition for
  ‘as.yearqtr’
time.mode_update: no visible global function definition for
  ‘as.yearmon’
updateGrobActions: no visible global function definition for ‘grid.ls’
xyData: no visible global function definition for ‘packet.number’
xyData: no visible global function definition for ‘trellis.panelArgs’

Found the following assignments to the global environment:
File ‘playwith/R/uiIdentifyActions.R’:
  assign(name, playGetIDs(playState), globalenv())
```
```
checking Rd \usage sections ... NOTE
S3 methods shown with full name in documentation object 'plotCoords':
  ‘plotCoords.plot’ ‘plotCoords.biplot’

The \usage entries for S3 methods should use the \method markup and not
their full name.
See chapter ‘Writing R documentation files’ in the ‘Writing R
Extensions’ manual.
```
```
DONE
Status: 1 WARNING, 4 NOTEs
```

## plot2groups (0.10)
Maintainer: Fuquan Zhang <zfq777@gmail.com>

```
checking examples ... ERROR
Running examples in ‘plot2groups-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot2
> ### Title: A function to plot scatter points for two groups of values.
> ### Aliases: plot2
> 
> ### ** Examples
> 
> data(drd3)
>  plot2(drd3, test = "wilcox")
Warning in wilcox.test.default(x = c(-0.2, 1.2, -2.1, -8.7, -2.7, 1.5, -0.1,  :
  cannot compute exact p-value with ties
Error: No stat called StatHline.
Execution halted
```
```
DONE
Status: 1 ERROR
```

## PlotPrjNetworks (1.0.0)
Maintainer: Joaquin Bienvenido Ordieres Mere <j.ordieres@upm.es>

```
checking whether package ‘PlotPrjNetworks’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘PlotPrjNetworks’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘PlotPrjNetworks’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/PlotPrjNetworks.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## plotROC (1.3.3)
Maintainer: Michael C Sachs <sachsmc@gmail.com>  
Bug reports: http://github.com/sachsmc/plotROC/issues

__OK__

## plspm (0.4.7)
Maintainer: Gaston Sanchez <gaston.stat@gmail.com>  
Bug reports: https://github.com/gastonstat/plspm/issues

__OK__

## pmc (1.0.1)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/cboettig/pmc/issues

```
checking package dependencies ... ERROR
Package required but not available: ‘ouch’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## pogit (1.0.1)
Maintainer: Michaela Dvorzak <m.dvorzak@gmx.at>

__OK__

## pointRes (1.1.0)
Maintainer: Marieke van der Maaten-Theunissen <marieketheunissen@gmail.com>

__OK__

## pollstR (1.2.1)
Maintainer: Jeffrey B. Arnold <jeffrey.arnold@gmail.com>  
Bug reports: https://github.com/rOpenGov/pollstR/issues

__OK__

## pomp (1.2.1.1)
Maintainer: Aaron A. King <kingaa@umich.edu>  
Bug reports: http://github.com/kingaa/pomp/issues

```
checking whether package ‘pomp’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/pomp.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## PopED (0.2.0)
Maintainer: Andrew C. Hooker <andrew.hooker@farmbio.uu.se>  
Bug reports: https://github.com/andrewhooker/PopED/issues

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'plot_efficiency_of_windows.Rd':
  ‘[ggplot2]{ggplot2}’

Missing link or links in documentation object 'plot_model_prediction.Rd':
  ‘[ggplot2]{ggplot2}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
DONE
Status: 1 WARNING
```

## popEpi (0.2.1)
Maintainer: Joonas Miettinen <joonas.miettinen@cancer.fi>  
Bug reports: https://github.com/WetRobot/popEpi/issues

__OK__

## PopGenReport (2.2)
Maintainer: Bernd Gruber <bernd.gruber@canberra.edu.au>

__OK__

## popgraph (1.4)
Maintainer: Rodney J. Dyer <rjdyer@vcu.edu>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
DONE
Status: 1 NOTE
```

## poppr (2.0.2)
Maintainer: Zhian N. Kamvar <kamvarz@science.oregonstate.edu>  
Bug reports: https://github.com/grunwaldlab/poppr/issues

__OK__

## popReconstruct (1.0-4)
Maintainer: "Mark C. Wheldon" <mwheldon@aut.ac.nz>

__OK__

## PortfolioEffectHFT (1.2)
Maintainer: Aleksey Zemnitskiy <aleksey.zemnitskiy@portfolioeffect.com>

```
checking whether package ‘PortfolioEffectHFT’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘PortfolioEffectHFT’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘PortfolioEffectHFT’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/PortfolioEffectHFT.Rcheck/00install.out’ for details.
```
```
checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    java   5.0Mb
```
```
checking examples ... ERROR
Running examples in ‘PortfolioEffectHFT-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: aapl.data
> ### Title: Sample Price Data
> ### Aliases: aapl.data goog.data spy.data
> ### Keywords: PortfolioEffectHFT aapl.data
> 
> ### ** Examples
> 
> data(aapl.data) 
> data(goog.data) 
> data(spy.data) 
> util_plot2d(aapl.data,title="Price",Legend="AAPL")+
+         util_line2d(goog.data,Legend="GOOG")+
+         util_line2d(spy.data,Legend="SPY")
Error: Unknown parameters: fill
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE
```

## PPtreeViz (1.3.0)
Maintainer: Eun-Kyung Lee <lee.eunk@gmail.com>

```
checking whether package ‘PPtreeViz’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘PPtreeViz’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘PPtreeViz’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/PPtreeViz.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## precintcon (2.1)
Maintainer: Lucas Venezian Povoa <lucasvenez@gmail.com>  
Bug reports: https://github.com/lucasvenez/precintcon/issues

```
checking examples ... ERROR
Running examples in ‘precintcon-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: pplot.rai
> ### Title: Plot Rainfall Anomaly Index
> ### Aliases: pplot.rai precintcon.plot.rai
> ### Keywords: anomaly precipitation rainfall
> 
> ### ** Examples
> 
> ##
> # Loading the daily precipitation serie.
> data(daily)
> 
> ##
> # Performing the a set of statistical analysis
> pplot.rai(daily, granularity = "m")
Error in strsplit(unitspec, " ") : non-character argument
Calls: pplot.rai ... fullseq.Date -> seq -> floor_date -> parse_unit_spec -> strsplit
Execution halted
```
```
DONE
Status: 1 ERROR
```

## predictmeans (0.99)
Maintainer: Dongwen Luo <dongwen.luo@agresearch.co.nz>

__OK__

## PReMiuM (3.1.2)
Maintainer: Silvia Liverani <liveranis@gmail.com>

```
checking whether package ‘PReMiuM’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘ggplot2::unit’ when loading ‘PReMiuM’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/PReMiuM.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## prevR (3.1)
Maintainer: Joseph Larmarange <joseph.larmarange@ird.fr>

```
checking whether package ‘prevR’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘prevR’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/prevR.Rcheck/00install.out’ for details.
```
```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘sparr’
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## pRF (1.0)
Maintainer: Ankur Chakravarthy <ankur.chakravarthy.10@ucl.ac.uk>

```
checking package dependencies ... ERROR
Package required but not available: ‘multtest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## primerTree (1.0.1)
Maintainer: Jim Hester <james.f.hester@gmail.com>

```
checking whether package ‘primerTree’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘primerTree’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘primerTree’
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘primerTree’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/primerTree.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘primerTree-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot.primerTree
> ### Title: plot function for a primerTree object, calls plot_tree_ranks
> ### Aliases: plot.primerTree
> 
> ### ** Examples
> 
> library(gridExtra)
> library(directlabels)
> #plot with all common ranks
> plot(mammals_16S)
Error: Unknown parameters: vjust
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## PRISMA (0.2-5)
Maintainer: Tammo Krueger <tammokrueger@googlemail.com>

__OK__

## profileR (0.3)
Maintainer: Christopher David Desjardins <cddesjardins@gmail.com>

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'profileplot.Rd':
  ‘[ggplot2]{ggplot2}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
DONE
Status: 1 WARNING
```

## profr (0.3.1)
Maintainer: Hadley Wickham <h.wickham@gmail.com>  
Bug reports: https://github.com/hadley/profr/issues

```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘ggplot2’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
ggplot.profr: no visible global function definition for ‘ggplot’
ggplot.profr: no visible global function definition for ‘geom_rect’
ggplot.profr: no visible global function definition for ‘aes’
ggplot.profr: no visible global function definition for ‘geom_text’
ggplot.profr: no visible global function definition for
  ‘scale_y_continuous’
ggplot.profr: no visible global function definition for
  ‘scale_x_continuous’
```
```
DONE
Status: 2 NOTEs
```

## ProgGUIinR (0.0-4)
Maintainer: John Verzani <verzani@math.csi.cuny.edu>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘qtbase’
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking R code for possible problems ... NOTE
browseRGtk2Files: no visible global function definition for ‘gtkWindow’
browseRGtk2Files: no visible global function definition for ‘gtkVBox’
browseRGtk2Files: no visible global function definition for
  ‘gtkToolbar’
browseRGtk2Files: no visible global function definition for ‘gtkHPaned’
browseRGtk2Files: no visible global function definition for ‘gtkFrame’
browseRGtk2Files: no visible global function definition for
  ‘gtkTextView’
browseRGtk2Files: no visible global function definition for
  ‘gtkScrolledWindow’
browseRGtk2Files: no visible global function definition for
  ‘gtkStatusbar’
browseRGtk2Files: no visible global function definition for
  ‘rGtkDataFrame’
browseRGtk2Files: no visible global function definition for
  ‘gtkTreeView’
browseRGtk2Files: no visible global function definition for
  ‘gtkTreeViewColumn’
browseRGtk2Files: no visible global function definition for
  ‘gtkCellRendererText’
browseRGtk2Files: no visible binding for global variable ‘PangoStyle’
browseRGtk2Files: no visible global function definition for ‘gtkAction’
browseRGtk2Files: no visible global function definition for
  ‘gSignalConnect’
browseRGtk2Files: no visible global function definition for
  ‘gtkToolButton’
browseRGtk2Files: no visible global function definition for
  ‘gtkSeparatorToolItem’
browseTclTkFiles : addScrollbars: no visible global function definition
  for ‘ttkscrollbar’
browseTclTkFiles : addScrollbars : <anonymous>: no visible global
  function definition for ‘tkxview’
browseTclTkFiles : addScrollbars: no visible global function definition
  for ‘tkconfigure’
browseTclTkFiles : addScrollbars : <anonymous>: no visible global
  function definition for ‘tkset’
browseTclTkFiles : addScrollbars: no visible global function definition
  for ‘tkgrid’
browseTclTkFiles : addScrollbars: no visible global function definition
  for ‘tkgrid.columnconfigure’
browseTclTkFiles : addScrollbars: no visible global function definition
  for ‘tkgrid.rowconfigure’
browseTclTkFiles: no visible global function definition for
  ‘tktoplevel’
browseTclTkFiles: no visible global function definition for
  ‘tkwm.title’
browseTclTkFiles: no visible global function definition for ‘ttkframe’
browseTclTkFiles: no visible global function definition for ‘tkpack’
browseTclTkFiles: no visible global function definition for ‘tkmenu’
browseTclTkFiles: no visible global function definition for
  ‘tkconfigure’
browseTclTkFiles: no visible global function definition for
  ‘ttkpanedwindow’
browseTclTkFiles: no visible global function definition for ‘tcl’
browseTclTkFiles: no visible global function definition for
  ‘ttktreeview’
browseTclTkFiles: no visible global function definition for ‘tktext’
browseTclTkFiles: no visible global function definition for ‘ttklabel’
browseTclTkFiles: no visible global function definition for
  ‘tktag.configure’
browseTclTkFiles : showFileInTextBuffer: no visible global function
  definition for ‘tkdelete’
browseTclTkFiles : showFileInTextBuffer: no visible global function
  definition for ‘tkinsert’
browseTclTkFiles : showFileInTextBuffer: no visible global function
  definition for ‘tksee’
browseTclTkFiles: no visible global function definition for ‘tkbind’
browseTclTkFiles : <anonymous>: no visible global function definition
  for ‘tcl’
browseTclTkFiles : evalBuffer: no visible global function definition
  for ‘tclvalue’
browseTclTkFiles : evalBuffer: no visible global function definition
  for ‘tkget’
browseTclTkFiles : evalRegion: no visible global function definition
  for ‘tclvalue’
browseTclTkFiles : evalRegion: no visible global function definition
  for ‘tkget’
browseTclTkFiles : evalSelection: no visible global function definition
  for ‘tclvalue’
browseTclTkFiles : evalSelection: no visible global function definition
  for ‘tkget’
browseTclTkFiles : evalLine: no visible global function definition for
  ‘tclvalue’
browseTclTkFiles : evalLine: no visible global function definition for
  ‘tkget’
browseTclTkFiles: no visible global function definition for ‘ttkbutton’
browseTclTkFiles : <anonymous>: no visible global function definition
  for ‘tclvalue’
browseTclTkFiles : <anonymous>: no visible global function definition
  for ‘tkget’
browseTclTkFiles : <anonymous>: no visible global function definition
  for ‘tkconfigure’
browseTclTkFiles : tagRegion: no visible global function definition for
  ‘tktag.configure’
browseTclTkFiles : tagRegion: no visible global function definition for
  ‘tktag.remove’
browseTclTkFiles : tagRegion : getLine: no visible global function
  definition for ‘tclvalue’
browseTclTkFiles : tagRegion : getLine: no visible global function
  definition for ‘tkget’
browseTclTkFiles : tagRegion : getPosFromIndex: no visible global
  function definition for ‘tclvalue’
browseTclTkFiles : tagRegion : getPosFromIndex: no visible global
  function definition for ‘tcl’
browseTclTkFiles : tagRegion: no visible global function definition for
  ‘tktag.add’
browsegWidgetsFiles: no visible global function definition for
  ‘gwindow’
browsegWidgetsFiles: no visible global function definition for
  ‘gstatusbar’
browsegWidgetsFiles: no visible global function definition for
  ‘gpanedgroup’
browsegWidgetsFiles: no visible global function definition for ‘gframe’
browsegWidgetsFiles: no visible global function definition for ‘gtable’
browsegWidgetsFiles : <anonymous>: no visible global function
  definition for ‘svalue’
browsegWidgetsFiles : <anonymous>: no visible global function
  definition for ‘svalue<-’
browsegWidgetsFiles : <anonymous>: no visible global function
  definition for ‘gtext’
browsegWidgetsFiles: no visible global function definition for ‘ggroup’
browsegWidgetsFiles: no visible global function definition for
  ‘addSpring’
browsegWidgetsFiles: no visible global function definition for
  ‘gbutton’
browsegWidgetsFiles: no visible global function definition for
  ‘gnotebook’
browsegWidgetsFiles: no visible global function definition for
  ‘visible<-’
browsegWidgetsFiles: no visible global function definition for
  ‘svalue<-’
showGtkWidgetInfo: no visible global function definition for ‘gtkEntry’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkNotebook’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkTextBuffer’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkWindow’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkVBoxNew’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkStatusbar’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkHBoxNew’
showGtkWidgetInfo: no visible global function definition for ‘gtkHBox’
showGtkWidgetInfo: no visible global function definition for ‘gtkLabel’
showGtkWidgetInfo : <anonymous>: no visible global function definition
  for ‘gtkTextView’
showGtkWidgetInfo : <anonymous>: no visible global function definition
  for ‘gtkScrolledWindow’
showGtkWidgetInfo : <anonymous>: no visible global function definition
  for ‘gtkLabel’
showGtkWidgetInfo: no visible global function definition for ‘gtkVBox’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkEntryCompletionNew’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkListStore’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkTextView’
showGtkWidgetInfo: no visible global function definition for
  ‘gtkScrolledWindow’
showGtkWidgetInfo : getSignalText: no visible global function
  definition for ‘gtkTypeGetSignals’
showGtkWidgetInfo: no visible global function definition for
  ‘gSignalConnect’
```
```
DONE
Status: 3 NOTEs
```

## ProjectTemplate (0.6)
Maintainer: Kirill Mueller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/johnmyleswhite/ProjectTemplate/issues

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘RODBC’
```
```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
DONE
Status: 2 NOTEs
```

## proteomics (0.2)
Maintainer: Thomas W. D. Möbius <kontakt@thomasmoebius.de>

__OK__

## PRROC (1.1)
Maintainer: Jan Grau <grau@informatik.uni-halle.de>

__OK__

## PSAboot (1.1)
Maintainer: Jason Bryer <jason@bryer.org>  
Bug reports: https://github.com/jbryer/PSAboot/issues

```
checking whether package ‘PSAboot’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/PSAboot.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## PSCBS (0.50.0)
Maintainer: Henrik Bengtsson <henrikb@braju.com>  
Bug reports: https://github.com/HenrikBengtsson/PSCBS/issues

```
checking package dependencies ... ERROR
Package required but not available: ‘DNAcopy’

Package suggested but not available for checking: ‘aroma.light’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## pscore (0.1-2)
Maintainer: Joshua F. Wiley <jwiley.psych@gmail.com>

__OK__

## psd (1.0-1)
Maintainer: Andrew J. Barbour <andy.barbour@gmail.com>  
Bug reports: https://github.com/abarbour/psd/issues

```
checking whether package ‘psd’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/psd.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## pxweb (0.5.57)
Maintainer: Mans Magnusson <mons.magnusson@gmail.com>  
Bug reports: https://github.com/rOpenGov/pxweb/issues

__OK__

## QCAtools (0.1)
Maintainer: Jirka Lewandowski <jirka.lewandowski@wzb.eu>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  plot.qca
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 2 NOTEs
```

## qdap (2.2.4)
Maintainer: Tyler Rinker <tyler.rinker@gmail.com>  
Bug reports: http://github.com/trinker/qdap/issues

__OK__

## qgraph (1.3.1)
Maintainer: Sacha Epskamp <mail@sachaepskamp.com>

__OK__

## quadrupen (0.2-4)
Maintainer: Julien Chiquet <julien.chiquet@genopole.cnrs.fr>

```
checking whether package ‘quadrupen’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/quadrupen.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## QualInt (1.0.0)
Maintainer: Lixi Yu <lixi-yu@uiowa.edu>

__OK__

## qualvar (0.1.0)
Maintainer: Joel Gombin <joel.gombin@gmail.com>

__OK__

## quanteda (0.8.4-2)
Maintainer: Kenneth Benoit <kbenoit@lse.ac.uk>  
Bug reports: https://github.com/kbenoit/quanteda/issues

```
checking data for non-ASCII characters ... NOTE
  Note: found 1 marked Latin-1 string
  Note: found 750 marked UTF-8 strings
  Note: found 7 strings marked as "bytes"
```
```
DONE
Status: 1 NOTE
```

## QuantumClone (0.9.15)
Maintainer: Paul Deveau <paul.deveau@curie.fr>

__OK__

## quickpsy (0.1.0)
Maintainer: Linares Daniel <danilinares@gmail.com>

```
checking whether package ‘quickpsy’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘tidyr::%>%’ when loading ‘quickpsy’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/quickpsy.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## qwraps2 (0.1.1)
Maintainer: Peter DeWitt <dewittpe@gmail.com>

__OK__

## R2admb (0.7.13)
Maintainer: Ben Bolker <bolker@mcmaster.ca>

```
checking R code for possible problems ... NOTE
Found the following calls to attach():
File ‘R2admb/R/check_section.R’:
  attach(R_list, name = "R_list", warn.conflicts = FALSE)
See section ‘Good practice’ in ‘?attach’.
```
```
DONE
Status: 1 NOTE
```

## R6 (2.1.1)
Maintainer: Winston Chang <winston@stdout.org>

__OK__

## radiant (0.1.83)
Maintainer: Vincent Nijs <radiant@rady.ucsd.edu>  
Bug reports: https://github.com/vnijs/radiant/issues

```
checking examples ... ERROR
Running examples in ‘radiant-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot.cross_tabs
> ### Title: Plot method for the cross_tabs function
> ### Aliases: plot.cross_tabs
> 
> ### ** Examples
> 
> result <- cross_tabs("newspaper", "Income", "Newspaper")
> plot(result, ct_check = c("observed","expected","chi_sq"))
Error in gather_.data.frame(., "variable", "values") : 
  argument "gather_cols" is missing, with no default
Calls: plot ... _fseq -> freduce -> withVisible -> <Anonymous> -> sshhr
Execution halted
```
```
DONE
Status: 1 ERROR
```

## rags2ridges (1.4)
Maintainer: Carel F.W. Peeters <cf.peeters@vumc.nl>

__OK__

## raincpc (0.4)
Maintainer: Gopi Goteti <my.ration.shop@gmail.com>

__OK__

## rAltmetric (0.6)
Maintainer: Karthik Ram <karthik.ram@gmail.com>  
Bug reports: https://github.com/ropensci/rAltmetric/issues/

__OK__

## RAM (1.2.1)
Maintainer: Wen Chen <Wen.Chen@agr.gc.ca>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘Heatplus’
```
```
checking whether package ‘RAM’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘RAM’
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘RAM’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/RAM.Rcheck/00install.out’ for details.
```
```
checking Rd cross-references ... WARNING
Package unavailable to check Rd xrefs: ‘Heatplus’
Missing link or links in documentation object 'RAM-package.Rd':
  ‘[ggplot2:ggplot2-package]{ggplot2}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
checking examples ... ERROR
Running examples in ‘RAM-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: group.rich
> ### Title: Barplot Of Richness For Each Level Of A Given Metadata Variable
> ### Aliases: group.rich
> ### Keywords: hplot
> 
> ### ** Examples
> 
> data(ITS1, meta)
> 
> group.rich(ITS1, meta, "Crop")
Using ind as id variables
Error: Unknown parameters: shape
Execution halted
```
```
DONE
Status: 1 ERROR, 2 WARNINGs, 1 NOTE
```

## randomUniformForest (1.1.5)
Maintainer: Saip Ciss <saip.ciss@wanadoo.fr>

__OK__

## rasterVis (0.37)
Maintainer: Oscar Perpinan Lamigueiro <oscar.perpinan@gmail.com>  
Bug reports: https://github.com/oscarperpinan/rastervis/issues

__OK__

## rattle (3.5.0)
Maintainer: Graham Williams <Graham.Williams@togaware.com>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘graph’ ‘RBGL’ ‘rggobi’ ‘RODBC’ ‘pkgDepTools’ ‘Rgraphviz’
```
```
checking installed package size ... NOTE
  installed size is  7.8Mb
  sub-directories of 1Mb or more:
    data   2.3Mb
    etc    3.1Mb
    po     1.2Mb
```
```
checking R code for possible problems ... NOTE
Found an obsolete/platform-specific call in the following functions:
  ‘openMyDevice’ ‘printPlot’ ‘savePlotToFile’
Found the platform-specific devices:
  ‘win.metafile’ ‘win.print’
dev.new() is the preferred way to open a new device, in the unlikely
event one is needed.
```
```
DONE
Status: 3 NOTEs
```

## rbefdata (0.3.5)
Maintainer: Claas-Thido Pfaff <claas-thido.pfaff@idiv-biodiversity.de>

```
checking R code for possible problems ... NOTE
bef.get.categories_for: no visible binding for global variable ‘id’
bef.portal.get.categories_for: no visible binding for global variable
  ‘id’
```
```
DONE
Status: 1 NOTE
```

## rbison (0.4.8)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rbison/issues

__OK__

## Rcell (1.3-2)
Maintainer: Alan Bush <abush@fbmc.fcen.uba.ar>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘EBImage’
```
```
checking dependencies in R code ... NOTE
':::' calls which should be '::':
  ‘ggplot2:::Geom’ ‘ggplot2:::Position’ ‘ggplot2:::Stat’
  See the note in ?`:::` about the use of this operator.
```
```
checking examples ... ERROR
Running examples in ‘Rcell-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: QC.filter
> ### Title: Quality Control Filter
> ### Aliases: QC.filter QC.reset QC.undo QC.execute
> ### Keywords: manip
> 
> ### ** Examples
> 
> 
> if(require(RcellData)){
+ 
+   #load example dataset
+   data(ACL394filtered)
+   
+   #resetting all the filters
+   X<-QC.reset(X)
+   
+   #filtering by fft.stat
+   cplot(X,~fft.stat) #see what cut to use
+   X<-QC.filter(X,fft.stat < 0.5) #apply the cut
+   
+   #filtering by the total number of frames in which a cell appears
+   cplot(X,cellID~t.frame,fill=f.tot.y,geom="tile",facets=~pos) 
+   X<-update_n.tot(X) #updating n.tot variable
+   cplot(X,~n.tot) #define where to apply the cut
+   X<-QC.filter(X,n.tot==14) #keep cells that appear in all t.frames
+   
+   #exclude cells by ucid (Unique Cell ID)
+   cplot(X,f.total.y~time.min,facets=~AF.nM,size=0.3,geom="jitter") 
+   #selecting cells that don't respond
+   c1=select.cells(X,f.total.y<10e4&t.frame>3,n.tot.subset=n.tot>=8)	
+   X<-QC.filter(X,!ucid %in% c1)
+   
+   #undoing the last filter
+   X<-QC.undo(X)
+ 
+ }
Loading required package: RcellData
resetting all filters
Warning in is.na(object) :
  is.na() applied to non-(list or vector) of type 'NULL'
Error in (function (g, s, ps)  : attempt to apply non-function
Calls: cplot -> mapply -> <Anonymous>
Execution halted
```
```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘Rcell.Rnw’
  ...
  all, CFP, YFP

> cplot(X, f.tot.y ~ t.frame, subset = pos == 1)
Warning in is.na(object) :
  is.na() applied to non-(list or vector) of type 'NULL'
treating pos as factor

  When sourcing ‘Rcell.R’:
Error: attempt to apply non-function
Execution halted
when running code in ‘cimage.Rnw’
  ...

> cplot(X, f.tot.y ~ fft.stat, subset = t.frame == 11 & 
+     pos %in% c(1, 8, 15, 22, 29))
Warning in is.na(object) :
  is.na() applied to non-(list or vector) of type 'NULL'
treating pos as factor

  When sourcing ‘cimage.R’:
Error: attempt to apply non-function
Execution halted
when running code in ‘cplot.Rnw’
  ...
> X$images$path <- factor(system.file("img", package = "RcellData"))

> cplot(X, x = f.tot.y, y = f.tot.c, subset = t.frame == 
+     13)
Warning in is.na(object) :
  is.na() applied to non-(list or vector) of type 'NULL'

  When sourcing ‘cplot.R’:
Error: attempt to apply non-function
Execution halted
when running code in ‘transform.Rnw’
  ...

> print(cplot(X, f.tot.y ~ t.frame, group = ucid, geom = "line", 
+     subset = pos == 29), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:10))
Warning in is.na(object) :
  is.na() applied to non-(list or vector) of type 'NULL'
treating pos, ucid as factor

  When sourcing ‘transform.R’:
Error: attempt to apply non-function
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Attaching package: ‘reshape’

The following objects are masked from ‘package:plyr’:

    rename, round_any

Loading required package: ggplot2
Loading required package: grid

Attaching package: ‘Rcell’

The following object is masked from ‘package:stats’:

    reshape

Warning in is.na(object) :
  is.na() applied to non-(list or vector) of type 'NULL'
Warning in is.na(object) :
  is.na() applied to non-(list or vector) of type 'NULL'
treating pos as factor

Error: processing vignette 'Rcell.Rnw' failed with diagnostics:
 chunk 10 (label = fig01) 
Error in (function (g, s, ps)  : attempt to apply non-function
Execution halted

```
```
DONE
Status: 2 ERRORs, 3 NOTEs
```

## rclinicaltrials (1.4.1)
Maintainer: Michael C Sachs <sachsmc@gmail.com>

__OK__

## RcmdrPlugin.KMggplot2 (0.2-0)
Maintainer: Triad sou. <triadsou@gmail.com>

```
checking dependencies in R code ... NOTE
Packages in Depends field not imported from:
  ‘ggplot2’ ‘grid’ ‘methods’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
Missing or unexported object: ‘ggplot2::digest.ggplot’
```
```
checking R code for possible problems ... NOTE
ggsaveKmg2: no visible global function definition for ‘last_plot’
theme_natrisk: no visible global function definition for ‘%+replace%’
theme_natrisk: no visible global function definition for
  ‘element_blank’
theme_natrisk: no visible global function definition for ‘element_rect’
theme_natrisk21: no visible global function definition for ‘%+replace%’
theme_natrisk21: no visible global function definition for
  ‘element_blank’
theme_natrisk21: no visible global function definition for
  ‘element_rect’
theme_natrisk21: no visible global function definition for ‘unit’
theme_natriskbg: no visible global function definition for ‘%+replace%’
theme_natriskbg: no visible global function definition for
  ‘element_blank’
theme_natriskbg: no visible global function definition for
  ‘element_rect’
theme_natriskbg: no visible global function definition for ‘unit’
theme_simple: no visible global function definition for ‘%+replace%’
theme_simple: no visible global function definition for ‘theme_bw’
theme_simple: no visible global function definition for ‘element_rect’
theme_simple: no visible global function definition for ‘element_blank’
```
```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'package-RcmdrPlugin.KMggplot2.Rd':
  ‘[ggplot2:ggplot2-package]{ggplot2}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
DONE
Status: 1 WARNING, 2 NOTEs
```

## RcmdrPlugin.MA (0.0-2)
Maintainer: A. C. Del Re <acdelre@gmail.com>

__OK__

## RCMIP5 (1.1)
Maintainer: Kathe Todd-Brown <ktoddbrown@gmail.com>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘ggplot2’ ‘ncdf’ ‘ncdf4’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
DONE
Status: 1 NOTE
```

## rddtools (0.4.0)
Maintainer: Bastiaan Quast <bquast@gmail.com>  
Bug reports: https://github.com/bquast/RDDtools/issues

__OK__

## RDML (0.9-1)
Maintainer: Konstantin A. Blagodatskikh <k.blag@yandex.ru>

__OK__

## RDS (0.7-3)
Maintainer: Mark S. Handcock <handcock@stat.ucla.edu>

```
checking whether package ‘RDS’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘RDS’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/RDS.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘RDS-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: convergence.plot
> ### Title: Convergence Plots
> ### Aliases: convergence.plot
> 
> ### ** Examples
> 
> data(faux)
> convergence.plot(faux,c("X","Y"))
Error in plot_clone(plot) : attempt to apply non-function
Calls: convergence.plot ... print -> print.ggplot -> ggplot_build -> plot_clone
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## refund (0.1-13)
Maintainer: Lei Huang <huangracer@gmail.com>

__OK__

## refund.shiny (0.1)
Maintainer: Julia Wrobel <jw3134@cumc.columbia.edu>

__OK__

## repijson (0.1.0)
Maintainer: Andy South <southandy@gmail.com>

__OK__

## ReporteRs (0.8.2)
Maintainer: David Gohel <david.gohel@lysis-consultants.fr>  
Bug reports: https://github.com/davidgohel/ReporteRs/issues

__OK__

## repra (0.4.4)
Maintainer: Eduardo Ibanez <edu.ibanez@gmail.com>  
Bug reports: https://github.com/NREL/repra/issues

```
checking examples ... ERROR
Running examples in ‘repra-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: calculate_elcc
> ### Title: Calculate effective load carrying capability (ELCC)
> ### Aliases: calculate_elcc
> 
> ### ** Examples
> 
> # Create outage table with 200 5-MW units
> gens <- data.frame(Capacity = rep(5, 200),
+                    EFOR = rep(0.08, 200))
> out.table <- outage_table(gens)
> 
> # Create random load and wind data and format
> tdata <- data.frame(Time = 1:8760,
+                     Load = runif(8760, 450, 850),
+                     Wind = runif(8760, 0, 100),
+                     Wind2 = runif(8760, 0, 200))
> td <- format_timedata(tdata)
Error in `[.data.frame`(data3, , list(Value = sum(Value)), by = cols.temp) : 
  unused argument (by = cols.temp)
Calls: format_timedata -> [ -> [.data.frame
Execution halted
```
```
checking tests ... ERROR
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  11: mode(current)
  12: format_timedata(tdata)
  13: data3[, list(Value = sum(Value)), by = cols.temp]
  14: `[.data.frame`(data3, , list(Value = sum(Value)), by = cols.temp)
  
  testthat results ================================================================
  OK: 9 SKIPPED: 0 FAILED: 3
  1. Error: timedata format 
  2. Error: timedata column names 
  3. Error: WinProb value set to 1 
  
  Error: testthat unit tests failed
  Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...

Attaching package: 'dplyr'

The following objects are masked from 'package:stats':

    filter, lag

The following objects are masked from 'package:base':

    intersect, setdiff, setequal, union

Quitting from lines 61-65 (repra.Rmd) 
Error: processing vignette 'repra.Rmd' failed with diagnostics:
unused argument (by = cols.temp)
Execution halted

```
```
DONE
Status: 2 ERRORs, 1 NOTE
```

## reproducer (0.1.3)
Maintainer: Lech Madeyski <lech.madeyski@gmail.com>

```
checking examples ... ERROR
Running examples in ‘reproducer-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: boxplotAndDensityCurveOnHistogram
> ### Title: boxplotAndDensityCurveOnHistogram
> ### Aliases: boxplotAndDensityCurveOnHistogram
> 
> ### ** Examples
> 
> library(ggplot2)
> library(grid)
> library(gridExtra)
> boxplotAndDensityCurveOnHistogram(Madeyski15EISEJ.PropProjects, "STUD", 0, 100)
Error: Unknown parameters: environment
Execution halted
```
```
DONE
Status: 1 ERROR
```

## reval (2.0.0)
Maintainer: Michael C Koohafkan <michael.koohafkan@gmail.com>  
Bug reports: https://github.com/mkoohafkan/reval/issues

__OK__

## rex (1.0.1)
Maintainer: Jim Hester <james.f.hester@gmail.com>  
Bug reports: https://github.com/kevinushey/rex/issues

```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Read 1000 items
Quitting from lines 69-71 (log_parsing.Rmd) 
Error: processing vignette 'log_parsing.Rmd' failed with diagnostics:
StatBin requires a continuous x variable the x variable is discrete. Perhaps you want stat="count"?
Execution halted

```
```
DONE
Status: 1 NOTE
```

## rfigshare (0.3.7)
Maintainer: Carl Boettiger <cboettig@gmail.com>  
Bug reports: https://github.com/ropensci/rfigshare/issues

__OK__

## rfisheries (0.1)
Maintainer: Karthik Ram <karthik.ram@gmail.com>  
Bug reports: http://www.github.com/ropensci/rfisheries/issues/new

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking R code for possible problems ... NOTE
fish_plot: no visible binding for global variable ‘species_code_data’
fish_plot: no visible binding for global variable ‘country_code_data’
```
```
DONE
Status: 2 NOTEs
```

## RFmarkerDetector (1.0)
Maintainer: Piergiorgio Palla <piergiorgio.palla@diee.unica.it>

```
checking whether package ‘RFmarkerDetector’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘randomForest::margin’ when loading ‘RFmarkerDetector’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/RFmarkerDetector.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## rfordummies (0.1.1)
Maintainer: Andrie de Vries <apdevries@gmail.com>  
Bug reports: https://github.com/andrie/rfordummies/issues

```
checking examples ... ERROR
Running examples in ‘rfordummies-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: ch13
> ### Title: Print examples of chapter 13 of 'R for Dummies'.
> ### Aliases: ch13
> 
> ### ** Examples
> 
> # C hapter 13 - Manipulating and Processing Data
> 
> # Deciding on the Most Appropriate Data Structure
> 
> # Creating Subsets of Your Data
> 
> ## Understanding the three subset operators
> ## Understanding the five ways of specifying the subset
> 
> str(islands)
 Named num [1:48] 11506 5500 16988 2968 16 ...
 - attr(*, "names")= chr [1:48] "Africa" "Antarctica" "Asia" "Australia" ...
> islands[]
          Africa       Antarctica             Asia        Australia 
           11506             5500            16988             2968 
    Axel Heiberg           Baffin            Banks           Borneo 
              16              184               23              280 
         Britain          Celebes            Celon             Cuba 
              84               73               25               43 
           Devon        Ellesmere           Europe        Greenland 
              21               82             3745              840 
          Hainan       Hispaniola         Hokkaido           Honshu 
              13               30               30               89 
         Iceland          Ireland             Java           Kyushu 
              40               33               49               14 
           Luzon       Madagascar         Melville         Mindanao 
              42              227               16               36 
        Moluccas      New Britain       New Guinea  New Zealand (N) 
              29               15              306               44 
 New Zealand (S)     Newfoundland    North America    Novaya Zemlya 
              58               43             9390               32 
 Prince of Wales         Sakhalin    South America      Southampton 
              13               29             6795               16 
     Spitsbergen          Sumatra           Taiwan         Tasmania 
              15              183               14               26 
Tierra del Fuego            Timor        Vancouver         Victoria 
              19               13               12               82 
> islands[c(8, 1, 1, 42)]
 Borneo  Africa  Africa Sumatra 
    280   11506   11506     183 
> islands[-(3:46)]
    Africa Antarctica  Vancouver   Victoria 
     11506       5500         12         82 
> islands[islands < 20]
    Axel Heiberg           Hainan           Kyushu         Melville 
              16               13               14               16 
     New Britain  Prince of Wales      Southampton      Spitsbergen 
              15               13               16               15 
          Taiwan Tierra del Fuego            Timor        Vancouver 
              14               19               13               12 
> islands[c("Madagascar", "Cuba")]
Madagascar       Cuba 
       227         43 
> 
> ## Subsetting data frames
> 
> str(iris)
'data.frame':	150 obs. of  5 variables:
 $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
 $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
 $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
 $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
 $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
> iris[1:5, ]
  Sepal.Length Sepal.Width Petal.Length Petal.Width Species
1          5.1         3.5          1.4         0.2  setosa
2          4.9         3.0          1.4         0.2  setosa
3          4.7         3.2          1.3         0.2  setosa
4          4.6         3.1          1.5         0.2  setosa
5          5.0         3.6          1.4         0.2  setosa
> iris[, c("Sepal.Length", "Sepal.Width")]
    Sepal.Length Sepal.Width
1            5.1         3.5
2            4.9         3.0
3            4.7         3.2
4            4.6         3.1
5            5.0         3.6
6            5.4         3.9
7            4.6         3.4
8            5.0         3.4
9            4.4         2.9
10           4.9         3.1
11           5.4         3.7
12           4.8         3.4
13           4.8         3.0
14           4.3         3.0
15           5.8         4.0
16           5.7         4.4
17           5.4         3.9
18           5.1         3.5
19           5.7         3.8
20           5.1         3.8
21           5.4         3.4
22           5.1         3.7
23           4.6         3.6
24           5.1         3.3
25           4.8         3.4
26           5.0         3.0
27           5.0         3.4
28           5.2         3.5
29           5.2         3.4
30           4.7         3.2
31           4.8         3.1
32           5.4         3.4
33           5.2         4.1
34           5.5         4.2
35           4.9         3.1
36           5.0         3.2
37           5.5         3.5
38           4.9         3.6
39           4.4         3.0
40           5.1         3.4
41           5.0         3.5
42           4.5         2.3
43           4.4         3.2
44           5.0         3.5
45           5.1         3.8
46           4.8         3.0
47           5.1         3.8
48           4.6         3.2
49           5.3         3.7
50           5.0         3.3
51           7.0         3.2
52           6.4         3.2
53           6.9         3.1
54           5.5         2.3
55           6.5         2.8
56           5.7         2.8
57           6.3         3.3
58           4.9         2.4
59           6.6         2.9
60           5.2         2.7
61           5.0         2.0
62           5.9         3.0
63           6.0         2.2
64           6.1         2.9
65           5.6         2.9
66           6.7         3.1
67           5.6         3.0
68           5.8         2.7
69           6.2         2.2
70           5.6         2.5
71           5.9         3.2
72           6.1         2.8
73           6.3         2.5
74           6.1         2.8
75           6.4         2.9
76           6.6         3.0
77           6.8         2.8
78           6.7         3.0
79           6.0         2.9
80           5.7         2.6
81           5.5         2.4
82           5.5         2.4
83           5.8         2.7
84           6.0         2.7
85           5.4         3.0
86           6.0         3.4
87           6.7         3.1
88           6.3         2.3
89           5.6         3.0
90           5.5         2.5
91           5.5         2.6
92           6.1         3.0
93           5.8         2.6
94           5.0         2.3
95           5.6         2.7
96           5.7         3.0
97           5.7         2.9
98           6.2         2.9
99           5.1         2.5
100          5.7         2.8
101          6.3         3.3
102          5.8         2.7
103          7.1         3.0
104          6.3         2.9
105          6.5         3.0
106          7.6         3.0
107          4.9         2.5
108          7.3         2.9
109          6.7         2.5
110          7.2         3.6
111          6.5         3.2
112          6.4         2.7
113          6.8         3.0
114          5.7         2.5
115          5.8         2.8
116          6.4         3.2
117          6.5         3.0
118          7.7         3.8
119          7.7         2.6
120          6.0         2.2
121          6.9         3.2
122          5.6         2.8
123          7.7         2.8
124          6.3         2.7
125          6.7         3.3
126          7.2         3.2
127          6.2         2.8
128          6.1         3.0
129          6.4         2.8
130          7.2         3.0
131          7.4         2.8
132          7.9         3.8
133          6.4         2.8
134          6.3         2.8
135          6.1         2.6
136          7.7         3.0
137          6.3         3.4
138          6.4         3.1
139          6.0         3.0
140          6.9         3.1
141          6.7         3.1
142          6.9         3.1
143          5.8         2.7
144          6.8         3.2
145          6.7         3.3
146          6.7         3.0
147          6.3         2.5
148          6.5         3.0
149          6.2         3.4
150          5.9         3.0
> iris[, 'Sepal.Length']
  [1] 5.1 4.9 4.7 4.6 5.0 5.4 4.6 5.0 4.4 4.9 5.4 4.8 4.8 4.3 5.8 5.7 5.4 5.1
 [19] 5.7 5.1 5.4 5.1 4.6 5.1 4.8 5.0 5.0 5.2 5.2 4.7 4.8 5.4 5.2 5.5 4.9 5.0
 [37] 5.5 4.9 4.4 5.1 5.0 4.5 4.4 5.0 5.1 4.8 5.1 4.6 5.3 5.0 7.0 6.4 6.9 5.5
 [55] 6.5 5.7 6.3 4.9 6.6 5.2 5.0 5.9 6.0 6.1 5.6 6.7 5.6 5.8 6.2 5.6 5.9 6.1
 [73] 6.3 6.1 6.4 6.6 6.8 6.7 6.0 5.7 5.5 5.5 5.8 6.0 5.4 6.0 6.7 6.3 5.6 5.5
 [91] 5.5 6.1 5.8 5.0 5.6 5.7 5.7 6.2 5.1 5.7 6.3 5.8 7.1 6.3 6.5 7.6 4.9 7.3
[109] 6.7 7.2 6.5 6.4 6.8 5.7 5.8 6.4 6.5 7.7 7.7 6.0 6.9 5.6 7.7 6.3 6.7 7.2
[127] 6.2 6.1 6.4 7.2 7.4 7.9 6.4 6.3 6.1 7.7 6.3 6.4 6.0 6.9 6.7 6.9 5.8 6.8
[145] 6.7 6.7 6.3 6.5 6.2 5.9
> iris[, 'Sepal.Length', drop=FALSE]
    Sepal.Length
1            5.1
2            4.9
3            4.7
4            4.6
5            5.0
6            5.4
7            4.6
8            5.0
9            4.4
10           4.9
11           5.4
12           4.8
13           4.8
14           4.3
15           5.8
16           5.7
17           5.4
18           5.1
19           5.7
20           5.1
21           5.4
22           5.1
23           4.6
24           5.1
25           4.8
26           5.0
27           5.0
28           5.2
29           5.2
30           4.7
31           4.8
32           5.4
33           5.2
34           5.5
35           4.9
36           5.0
37           5.5
38           4.9
39           4.4
40           5.1
41           5.0
42           4.5
43           4.4
44           5.0
45           5.1
46           4.8
47           5.1
48           4.6
49           5.3
50           5.0
51           7.0
52           6.4
53           6.9
54           5.5
55           6.5
56           5.7
57           6.3
58           4.9
59           6.6
60           5.2
61           5.0
62           5.9
63           6.0
64           6.1
65           5.6
66           6.7
67           5.6
68           5.8
69           6.2
70           5.6
71           5.9
72           6.1
73           6.3
74           6.1
75           6.4
76           6.6
77           6.8
78           6.7
79           6.0
80           5.7
81           5.5
82           5.5
83           5.8
84           6.0
85           5.4
86           6.0
87           6.7
88           6.3
89           5.6
90           5.5
91           5.5
92           6.1
93           5.8
94           5.0
95           5.6
96           5.7
97           5.7
98           6.2
99           5.1
100          5.7
101          6.3
102          5.8
103          7.1
104          6.3
105          6.5
106          7.6
107          4.9
108          7.3
109          6.7
110          7.2
111          6.5
112          6.4
113          6.8
114          5.7
115          5.8
116          6.4
117          6.5
118          7.7
119          7.7
120          6.0
121          6.9
122          5.6
123          7.7
124          6.3
125          6.7
126          7.2
127          6.2
128          6.1
129          6.4
130          7.2
131          7.4
132          7.9
133          6.4
134          6.3
135          6.1
136          7.7
137          6.3
138          6.4
139          6.0
140          6.9
141          6.7
142          6.9
143          5.8
144          6.8
145          6.7
146          6.7
147          6.3
148          6.5
149          6.2
150          5.9
> iris['Sepal.Length']
    Sepal.Length
1            5.1
2            4.9
3            4.7
4            4.6
5            5.0
6            5.4
7            4.6
8            5.0
9            4.4
10           4.9
11           5.4
12           4.8
13           4.8
14           4.3
15           5.8
16           5.7
17           5.4
18           5.1
19           5.7
20           5.1
21           5.4
22           5.1
23           4.6
24           5.1
25           4.8
26           5.0
27           5.0
28           5.2
29           5.2
30           4.7
31           4.8
32           5.4
33           5.2
34           5.5
35           4.9
36           5.0
37           5.5
38           4.9
39           4.4
40           5.1
41           5.0
42           4.5
43           4.4
44           5.0
45           5.1
46           4.8
47           5.1
48           4.6
49           5.3
50           5.0
51           7.0
52           6.4
53           6.9
54           5.5
55           6.5
56           5.7
57           6.3
58           4.9
59           6.6
60           5.2
61           5.0
62           5.9
63           6.0
64           6.1
65           5.6
66           6.7
67           5.6
68           5.8
69           6.2
70           5.6
71           5.9
72           6.1
73           6.3
74           6.1
75           6.4
76           6.6
77           6.8
78           6.7
79           6.0
80           5.7
81           5.5
82           5.5
83           5.8
84           6.0
85           5.4
86           6.0
87           6.7
88           6.3
89           5.6
90           5.5
91           5.5
92           6.1
93           5.8
94           5.0
95           5.6
96           5.7
97           5.7
98           6.2
99           5.1
100          5.7
101          6.3
102          5.8
103          7.1
104          6.3
105          6.5
106          7.6
107          4.9
108          7.3
109          6.7
110          7.2
111          6.5
112          6.4
113          6.8
114          5.7
115          5.8
116          6.4
117          6.5
118          7.7
119          7.7
120          6.0
121          6.9
122          5.6
123          7.7
124          6.3
125          6.7
126          7.2
127          6.2
128          6.1
129          6.4
130          7.2
131          7.4
132          7.9
133          6.4
134          6.3
135          6.1
136          7.7
137          6.3
138          6.4
139          6.0
140          6.9
141          6.7
142          6.9
143          5.8
144          6.8
145          6.7
146          6.7
147          6.3
148          6.5
149          6.2
150          5.9
> iris[1:5, c("Sepal.Length", "Sepal.Width")]
  Sepal.Length Sepal.Width
1          5.1         3.5
2          4.9         3.0
3          4.7         3.2
4          4.6         3.1
5          5.0         3.6
> 
> ### Taking samples from data
> 
> sample(1:6, 10, replace=TRUE)
 [1] 2 3 4 6 2 6 6 4 4 1
> 
> set.seed(1)
> sample(1:6, 10, replace=TRUE)
 [1] 2 3 4 6 2 6 6 4 4 1
> sample(1:6, 10, replace=TRUE)
 [1] 2 2 5 3 5 3 5 6 3 5
> 
> set.seed(1)
> sample(1:6, 10, replace=TRUE)
 [1] 2 3 4 6 2 6 6 4 4 1
> 
> set.seed(123)
> index <- sample(1:nrow(iris), 5)
> index
[1]  44 118  61 130 138
> iris[index, ]
    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
44           5.0         3.5          1.6         0.6     setosa
118          7.7         3.8          6.7         2.2  virginica
61           5.0         2.0          3.5         1.0 versicolor
130          7.2         3.0          5.8         1.6  virginica
138          6.4         3.1          5.5         1.8  virginica
> 
> ### Removing duplicate data
> 
> duplicated(c(1,2,1,3,1,4))
[1] FALSE FALSE  TRUE FALSE  TRUE FALSE
> duplicated(iris)
  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 [49] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 [73] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 [85] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 [97] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
[109] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
[121] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
[133] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
[145] FALSE FALSE FALSE FALSE FALSE FALSE
> which(duplicated(iris))
[1] 143
> iris[!duplicated(iris), ]
    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
1            5.1         3.5          1.4         0.2     setosa
2            4.9         3.0          1.4         0.2     setosa
3            4.7         3.2          1.3         0.2     setosa
4            4.6         3.1          1.5         0.2     setosa
5            5.0         3.6          1.4         0.2     setosa
6            5.4         3.9          1.7         0.4     setosa
7            4.6         3.4          1.4         0.3     setosa
8            5.0         3.4          1.5         0.2     setosa
9            4.4         2.9          1.4         0.2     setosa
10           4.9         3.1          1.5         0.1     setosa
11           5.4         3.7          1.5         0.2     setosa
12           4.8         3.4          1.6         0.2     setosa
13           4.8         3.0          1.4         0.1     setosa
14           4.3         3.0          1.1         0.1     setosa
15           5.8         4.0          1.2         0.2     setosa
16           5.7         4.4          1.5         0.4     setosa
17           5.4         3.9          1.3         0.4     setosa
18           5.1         3.5          1.4         0.3     setosa
19           5.7         3.8          1.7         0.3     setosa
20           5.1         3.8          1.5         0.3     setosa
21           5.4         3.4          1.7         0.2     setosa
22           5.1         3.7          1.5         0.4     setosa
23           4.6         3.6          1.0         0.2     setosa
24           5.1         3.3          1.7         0.5     setosa
25           4.8         3.4          1.9         0.2     setosa
26           5.0         3.0          1.6         0.2     setosa
27           5.0         3.4          1.6         0.4     setosa
28           5.2         3.5          1.5         0.2     setosa
29           5.2         3.4          1.4         0.2     setosa
30           4.7         3.2          1.6         0.2     setosa
31           4.8         3.1          1.6         0.2     setosa
32           5.4         3.4          1.5         0.4     setosa
33           5.2         4.1          1.5         0.1     setosa
34           5.5         4.2          1.4         0.2     setosa
35           4.9         3.1          1.5         0.2     setosa
36           5.0         3.2          1.2         0.2     setosa
37           5.5         3.5          1.3         0.2     setosa
38           4.9         3.6          1.4         0.1     setosa
39           4.4         3.0          1.3         0.2     setosa
40           5.1         3.4          1.5         0.2     setosa
41           5.0         3.5          1.3         0.3     setosa
42           4.5         2.3          1.3         0.3     setosa
43           4.4         3.2          1.3         0.2     setosa
44           5.0         3.5          1.6         0.6     setosa
45           5.1         3.8          1.9         0.4     setosa
46           4.8         3.0          1.4         0.3     setosa
47           5.1         3.8          1.6         0.2     setosa
48           4.6         3.2          1.4         0.2     setosa
49           5.3         3.7          1.5         0.2     setosa
50           5.0         3.3          1.4         0.2     setosa
51           7.0         3.2          4.7         1.4 versicolor
52           6.4         3.2          4.5         1.5 versicolor
53           6.9         3.1          4.9         1.5 versicolor
54           5.5         2.3          4.0         1.3 versicolor
55           6.5         2.8          4.6         1.5 versicolor
56           5.7         2.8          4.5         1.3 versicolor
57           6.3         3.3          4.7         1.6 versicolor
58           4.9         2.4          3.3         1.0 versicolor
59           6.6         2.9          4.6         1.3 versicolor
60           5.2         2.7          3.9         1.4 versicolor
61           5.0         2.0          3.5         1.0 versicolor
62           5.9         3.0          4.2         1.5 versicolor
63           6.0         2.2          4.0         1.0 versicolor
64           6.1         2.9          4.7         1.4 versicolor
65           5.6         2.9          3.6         1.3 versicolor
66           6.7         3.1          4.4         1.4 versicolor
67           5.6         3.0          4.5         1.5 versicolor
68           5.8         2.7          4.1         1.0 versicolor
69           6.2         2.2          4.5         1.5 versicolor
70           5.6         2.5          3.9         1.1 versicolor
71           5.9         3.2          4.8         1.8 versicolor
72           6.1         2.8          4.0         1.3 versicolor
73           6.3         2.5          4.9         1.5 versicolor
74           6.1         2.8          4.7         1.2 versicolor
75           6.4         2.9          4.3         1.3 versicolor
76           6.6         3.0          4.4         1.4 versicolor
77           6.8         2.8          4.8         1.4 versicolor
78           6.7         3.0          5.0         1.7 versicolor
79           6.0         2.9          4.5         1.5 versicolor
80           5.7         2.6          3.5         1.0 versicolor
81           5.5         2.4          3.8         1.1 versicolor
82           5.5         2.4          3.7         1.0 versicolor
83           5.8         2.7          3.9         1.2 versicolor
84           6.0         2.7          5.1         1.6 versicolor
85           5.4         3.0          4.5         1.5 versicolor
86           6.0         3.4          4.5         1.6 versicolor
87           6.7         3.1          4.7         1.5 versicolor
88           6.3         2.3          4.4         1.3 versicolor
89           5.6         3.0          4.1         1.3 versicolor
90           5.5         2.5          4.0         1.3 versicolor
91           5.5         2.6          4.4         1.2 versicolor
92           6.1         3.0          4.6         1.4 versicolor
93           5.8         2.6          4.0         1.2 versicolor
94           5.0         2.3          3.3         1.0 versicolor
95           5.6         2.7          4.2         1.3 versicolor
96           5.7         3.0          4.2         1.2 versicolor
97           5.7         2.9          4.2         1.3 versicolor
98           6.2         2.9          4.3         1.3 versicolor
99           5.1         2.5          3.0         1.1 versicolor
100          5.7         2.8          4.1         1.3 versicolor
101          6.3         3.3          6.0         2.5  virginica
102          5.8         2.7          5.1         1.9  virginica
103          7.1         3.0          5.9         2.1  virginica
104          6.3         2.9          5.6         1.8  virginica
105          6.5         3.0          5.8         2.2  virginica
106          7.6         3.0          6.6         2.1  virginica
107          4.9         2.5          4.5         1.7  virginica
108          7.3         2.9          6.3         1.8  virginica
109          6.7         2.5          5.8         1.8  virginica
110          7.2         3.6          6.1         2.5  virginica
111          6.5         3.2          5.1         2.0  virginica
112          6.4         2.7          5.3         1.9  virginica
113          6.8         3.0          5.5         2.1  virginica
114          5.7         2.5          5.0         2.0  virginica
115          5.8         2.8          5.1         2.4  virginica
116          6.4         3.2          5.3         2.3  virginica
117          6.5         3.0          5.5         1.8  virginica
118          7.7         3.8          6.7         2.2  virginica
119          7.7         2.6          6.9         2.3  virginica
120          6.0         2.2          5.0         1.5  virginica
121          6.9         3.2          5.7         2.3  virginica
122          5.6         2.8          4.9         2.0  virginica
123          7.7         2.8          6.7         2.0  virginica
124          6.3         2.7          4.9         1.8  virginica
125          6.7         3.3          5.7         2.1  virginica
126          7.2         3.2          6.0         1.8  virginica
127          6.2         2.8          4.8         1.8  virginica
128          6.1         3.0          4.9         1.8  virginica
129          6.4         2.8          5.6         2.1  virginica
130          7.2         3.0          5.8         1.6  virginica
131          7.4         2.8          6.1         1.9  virginica
132          7.9         3.8          6.4         2.0  virginica
133          6.4         2.8          5.6         2.2  virginica
134          6.3         2.8          5.1         1.5  virginica
135          6.1         2.6          5.6         1.4  virginica
136          7.7         3.0          6.1         2.3  virginica
137          6.3         3.4          5.6         2.4  virginica
138          6.4         3.1          5.5         1.8  virginica
139          6.0         3.0          4.8         1.8  virginica
140          6.9         3.1          5.4         2.1  virginica
141          6.7         3.1          5.6         2.4  virginica
142          6.9         3.1          5.1         2.3  virginica
144          6.8         3.2          5.9         2.3  virginica
145          6.7         3.3          5.7         2.5  virginica
146          6.7         3.0          5.2         2.3  virginica
147          6.3         2.5          5.0         1.9  virginica
148          6.5         3.0          5.2         2.0  virginica
149          6.2         3.4          5.4         2.3  virginica
150          5.9         3.0          5.1         1.8  virginica
> 
> index <- which(duplicated(iris))
> iris[-index, ]
    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
1            5.1         3.5          1.4         0.2     setosa
2            4.9         3.0          1.4         0.2     setosa
3            4.7         3.2          1.3         0.2     setosa
4            4.6         3.1          1.5         0.2     setosa
5            5.0         3.6          1.4         0.2     setosa
6            5.4         3.9          1.7         0.4     setosa
7            4.6         3.4          1.4         0.3     setosa
8            5.0         3.4          1.5         0.2     setosa
9            4.4         2.9          1.4         0.2     setosa
10           4.9         3.1          1.5         0.1     setosa
11           5.4         3.7          1.5         0.2     setosa
12           4.8         3.4          1.6         0.2     setosa
13           4.8         3.0          1.4         0.1     setosa
14           4.3         3.0          1.1         0.1     setosa
15           5.8         4.0          1.2         0.2     setosa
16           5.7         4.4          1.5         0.4     setosa
17           5.4         3.9          1.3         0.4     setosa
18           5.1         3.5          1.4         0.3     setosa
19           5.7         3.8          1.7         0.3     setosa
20           5.1         3.8          1.5         0.3     setosa
21           5.4         3.4          1.7         0.2     setosa
22           5.1         3.7          1.5         0.4     setosa
23           4.6         3.6          1.0         0.2     setosa
24           5.1         3.3          1.7         0.5     setosa
25           4.8         3.4          1.9         0.2     setosa
26           5.0         3.0          1.6         0.2     setosa
27           5.0         3.4          1.6         0.4     setosa
28           5.2         3.5          1.5         0.2     setosa
29           5.2         3.4          1.4         0.2     setosa
30           4.7         3.2          1.6         0.2     setosa
31           4.8         3.1          1.6         0.2     setosa
32           5.4         3.4          1.5         0.4     setosa
33           5.2         4.1          1.5         0.1     setosa
34           5.5         4.2          1.4         0.2     setosa
35           4.9         3.1          1.5         0.2     setosa
36           5.0         3.2          1.2         0.2     setosa
37           5.5         3.5          1.3         0.2     setosa
38           4.9         3.6          1.4         0.1     setosa
39           4.4         3.0          1.3         0.2     setosa
40           5.1         3.4          1.5         0.2     setosa
41           5.0         3.5          1.3         0.3     setosa
42           4.5         2.3          1.3         0.3     setosa
43           4.4         3.2          1.3         0.2     setosa
44           5.0         3.5          1.6         0.6     setosa
45           5.1         3.8          1.9         0.4     setosa
46           4.8         3.0          1.4         0.3     setosa
47           5.1         3.8          1.6         0.2     setosa
48           4.6         3.2          1.4         0.2     setosa
49           5.3         3.7          1.5         0.2     setosa
50           5.0         3.3          1.4         0.2     setosa
51           7.0         3.2          4.7         1.4 versicolor
52           6.4         3.2          4.5         1.5 versicolor
53           6.9         3.1          4.9         1.5 versicolor
54           5.5         2.3          4.0         1.3 versicolor
55           6.5         2.8          4.6         1.5 versicolor
56           5.7         2.8          4.5         1.3 versicolor
57           6.3         3.3          4.7         1.6 versicolor
58           4.9         2.4          3.3         1.0 versicolor
59           6.6         2.9          4.6         1.3 versicolor
60           5.2         2.7          3.9         1.4 versicolor
61           5.0         2.0          3.5         1.0 versicolor
62           5.9         3.0          4.2         1.5 versicolor
63           6.0         2.2          4.0         1.0 versicolor
64           6.1         2.9          4.7         1.4 versicolor
65           5.6         2.9          3.6         1.3 versicolor
66           6.7         3.1          4.4         1.4 versicolor
67           5.6         3.0          4.5         1.5 versicolor
68           5.8         2.7          4.1         1.0 versicolor
69           6.2         2.2          4.5         1.5 versicolor
70           5.6         2.5          3.9         1.1 versicolor
71           5.9         3.2          4.8         1.8 versicolor
72           6.1         2.8          4.0         1.3 versicolor
73           6.3         2.5          4.9         1.5 versicolor
74           6.1         2.8          4.7         1.2 versicolor
75           6.4         2.9          4.3         1.3 versicolor
76           6.6         3.0          4.4         1.4 versicolor
77           6.8         2.8          4.8         1.4 versicolor
78           6.7         3.0          5.0         1.7 versicolor
79           6.0         2.9          4.5         1.5 versicolor
80           5.7         2.6          3.5         1.0 versicolor
81           5.5         2.4          3.8         1.1 versicolor
82           5.5         2.4          3.7         1.0 versicolor
83           5.8         2.7          3.9         1.2 versicolor
84           6.0         2.7          5.1         1.6 versicolor
85           5.4         3.0          4.5         1.5 versicolor
86           6.0         3.4          4.5         1.6 versicolor
87           6.7         3.1          4.7         1.5 versicolor
88           6.3         2.3          4.4         1.3 versicolor
89           5.6         3.0          4.1         1.3 versicolor
90           5.5         2.5          4.0         1.3 versicolor
91           5.5         2.6          4.4         1.2 versicolor
92           6.1         3.0          4.6         1.4 versicolor
93           5.8         2.6          4.0         1.2 versicolor
94           5.0         2.3          3.3         1.0 versicolor
95           5.6         2.7          4.2         1.3 versicolor
96           5.7         3.0          4.2         1.2 versicolor
97           5.7         2.9          4.2         1.3 versicolor
98           6.2         2.9          4.3         1.3 versicolor
99           5.1         2.5          3.0         1.1 versicolor
100          5.7         2.8          4.1         1.3 versicolor
101          6.3         3.3          6.0         2.5  virginica
102          5.8         2.7          5.1         1.9  virginica
103          7.1         3.0          5.9         2.1  virginica
104          6.3         2.9          5.6         1.8  virginica
105          6.5         3.0          5.8         2.2  virginica
106          7.6         3.0          6.6         2.1  virginica
107          4.9         2.5          4.5         1.7  virginica
108          7.3         2.9          6.3         1.8  virginica
109          6.7         2.5          5.8         1.8  virginica
110          7.2         3.6          6.1         2.5  virginica
111          6.5         3.2          5.1         2.0  virginica
112          6.4         2.7          5.3         1.9  virginica
113          6.8         3.0          5.5         2.1  virginica
114          5.7         2.5          5.0         2.0  virginica
115          5.8         2.8          5.1         2.4  virginica
116          6.4         3.2          5.3         2.3  virginica
117          6.5         3.0          5.5         1.8  virginica
118          7.7         3.8          6.7         2.2  virginica
119          7.7         2.6          6.9         2.3  virginica
120          6.0         2.2          5.0         1.5  virginica
121          6.9         3.2          5.7         2.3  virginica
122          5.6         2.8          4.9         2.0  virginica
123          7.7         2.8          6.7         2.0  virginica
124          6.3         2.7          4.9         1.8  virginica
125          6.7         3.3          5.7         2.1  virginica
126          7.2         3.2          6.0         1.8  virginica
127          6.2         2.8          4.8         1.8  virginica
128          6.1         3.0          4.9         1.8  virginica
129          6.4         2.8          5.6         2.1  virginica
130          7.2         3.0          5.8         1.6  virginica
131          7.4         2.8          6.1         1.9  virginica
132          7.9         3.8          6.4         2.0  virginica
133          6.4         2.8          5.6         2.2  virginica
134          6.3         2.8          5.1         1.5  virginica
135          6.1         2.6          5.6         1.4  virginica
136          7.7         3.0          6.1         2.3  virginica
137          6.3         3.4          5.6         2.4  virginica
138          6.4         3.1          5.5         1.8  virginica
139          6.0         3.0          4.8         1.8  virginica
140          6.9         3.1          5.4         2.1  virginica
141          6.7         3.1          5.6         2.4  virginica
142          6.9         3.1          5.1         2.3  virginica
144          6.8         3.2          5.9         2.3  virginica
145          6.7         3.3          5.7         2.5  virginica
146          6.7         3.0          5.2         2.3  virginica
147          6.3         2.5          5.0         1.9  virginica
148          6.5         3.0          5.2         2.0  virginica
149          6.2         3.4          5.4         2.3  virginica
150          5.9         3.0          5.1         1.8  virginica
> 
> ### Removing rows with missing data
> 
> str(airquality)
'data.frame':	153 obs. of  6 variables:
 $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
 $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
 $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
 $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
 $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
 $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...
> complete.cases(airquality)
  [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE
 [13]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
 [25] FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
 [37] FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE
 [49]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
 [61] FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
 [73]  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
 [85]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
 [97] FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE
[109]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE
[121]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
[133]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
[145]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
> 
> x <- airquality[complete.cases(airquality), ]
> str(x)
'data.frame':	111 obs. of  6 variables:
 $ Ozone  : int  41 36 12 18 23 19 8 16 11 14 ...
 $ Solar.R: int  190 118 149 313 299 99 19 256 290 274 ...
 $ Wind   : num  7.4 8 12.6 11.5 8.6 13.8 20.1 9.7 9.2 10.9 ...
 $ Temp   : int  67 72 74 62 65 59 61 69 66 68 ...
 $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
 $ Day    : int  1 2 3 4 7 8 9 12 13 14 ...
> x <- na.omit(airquality)
> 
> 
> 
> # Adding Calculated Fields to Data
> 
> ## Doing arithmetic on columns of a data frame
> 
> x <- iris$Sepal.Length / iris$Sepal.Width
> head(x)
[1] 1.457143 1.633333 1.468750 1.483871 1.388889 1.384615
> 
> ## Using with and within to improve code readability
> 
> y <- with(iris, Sepal.Length / Sepal.Width)
> head(y)
[1] 1.457143 1.633333 1.468750 1.483871 1.388889 1.384615
> identical(x, y)
[1] TRUE
> 
> iris$ratio <- iris$Sepal.Length / iris$Sepal.Width
> iris <- within(iris, ratio <- Sepal.Length / Sepal.Width)
> head(iris$ratio)
[1] 1.457143 1.633333 1.468750 1.483871 1.388889 1.384615
> 
> ## Creating subgroups or bins of data
> 
> ### Using cut to create a fixed number of subgroups
> 
> head(state.x77)
           Population Income Illiteracy Life Exp Murder HS Grad Frost   Area
Alabama          3615   3624        2.1    69.05   15.1    41.3    20  50708
Alaska            365   6315        1.5    69.31   11.3    66.7   152 566432
Arizona          2212   4530        1.8    70.55    7.8    58.1    15 113417
Arkansas         2110   3378        1.9    70.66   10.1    39.9    65  51945
California      21198   5114        1.1    71.71   10.3    62.6    20 156361
Colorado         2541   4884        0.7    72.06    6.8    63.9   166 103766
> frost <- state.x77[, "Frost"]
> head(frost, 5)
   Alabama     Alaska    Arizona   Arkansas California 
        20        152         15         65         20 
> cut(frost, 3, include.lowest=TRUE)
 [1] [-0.188,62.7] (125,188]     [-0.188,62.7] (62.7,125]    [-0.188,62.7]
 [6] (125,188]     (125,188]     (62.7,125]    [-0.188,62.7] [-0.188,62.7]
[11] [-0.188,62.7] (125,188]     (125,188]     (62.7,125]    (125,188]    
[16] (62.7,125]    (62.7,125]    [-0.188,62.7] (125,188]     (62.7,125]   
[21] (62.7,125]    (62.7,125]    (125,188]     [-0.188,62.7] (62.7,125]   
[26] (125,188]     (125,188]     (125,188]     (125,188]     (62.7,125]   
[31] (62.7,125]    (62.7,125]    (62.7,125]    (125,188]     (62.7,125]   
[36] (62.7,125]    [-0.188,62.7] (125,188]     (125,188]     (62.7,125]   
[41] (125,188]     (62.7,125]    [-0.188,62.7] (125,188]     (125,188]    
[46] (62.7,125]    [-0.188,62.7] (62.7,125]    (125,188]     (125,188]    
Levels: [-0.188,62.7] (62.7,125] (125,188]
> 
> ### Adding labels to cut
> 
> cut(frost, 3, include.lowest=TRUE, labels=c("Low", "Med", "High"))
 [1] Low  High Low  Med  Low  High High Med  Low  Low  Low  High High Med  High
[16] Med  Med  Low  High Med  Med  Med  High Low  Med  High High High High Med 
[31] Med  Med  Med  High Med  Med  Low  High High Med  High Med  Low  High High
[46] Med  Low  Med  High High
Levels: Low Med High
> 
> ### Using table to count the number of observations
> 
> x <- cut(frost, 3, include.lowest=TRUE, labels=c("Low", "Med", "High"))
> table(x)
x
 Low  Med High 
  11   19   20 
> x
 [1] Low  High Low  Med  Low  High High Med  Low  Low  Low  High High Med  High
[16] Med  Med  Low  High Med  Med  Med  High Low  Med  High High High High Med 
[31] Med  Med  Med  High Med  Med  Low  High High Med  High Med  Low  High High
[46] Med  Low  Med  High High
Levels: Low Med High
> 
> 
> # Combining and Merging Data Sets
> 
> ## Creating sample data to illustrate merging
> 
> all.states <- as.data.frame(state.x77)
> all.states$Name <- rownames(state.x77)
> rownames(all.states) <- NULL
> str(all.states)
'data.frame':	50 obs. of  9 variables:
 $ Population: num  3615 365 2212 2110 21198 ...
 $ Income    : num  3624 6315 4530 3378 5114 ...
 $ Illiteracy: num  2.1 1.5 1.8 1.9 1.1 0.7 1.1 0.9 1.3 2 ...
 $ Life Exp  : num  69 69.3 70.5 70.7 71.7 ...
 $ Murder    : num  15.1 11.3 7.8 10.1 10.3 6.8 3.1 6.2 10.7 13.9 ...
 $ HS Grad   : num  41.3 66.7 58.1 39.9 62.6 63.9 56 54.6 52.6 40.6 ...
 $ Frost     : num  20 152 15 65 20 166 139 103 11 60 ...
 $ Area      : num  50708 566432 113417 51945 156361 ...
 $ Name      : chr  "Alabama" "Alaska" "Arizona" "Arkansas" ...
> 
> ### Creating a subset of cold states
> 
> cold.states <- all.states[all.states$Frost>150, c("Name", "Frost")]
> cold.states
            Name Frost
2         Alaska   152
6       Colorado   166
19         Maine   161
23     Minnesota   160
26       Montana   155
28        Nevada   188
29 New Hampshire   174
34  North Dakota   186
41  South Dakota   172
45       Vermont   168
50       Wyoming   173
> 
> ### Creating a subset of large states
> 
> large.states <- all.states[all.states$Area>=100000, c("Name", "Area")]
> large.states
         Name   Area
2      Alaska 566432
3     Arizona 113417
5  California 156361
6    Colorado 103766
26    Montana 145587
28     Nevada 109889
31 New Mexico 121412
43      Texas 262134
> 
> ## Using the merge() function
> 
> ### Using merge to find the intersection of data
> 
> merge(cold.states, large.states)
      Name Frost   Area
1   Alaska   152 566432
2 Colorado   166 103766
3  Montana   155 145587
4   Nevada   188 109889
> 
> ### Understanding the different types of merge
> 
> merge(cold.states, large.states, all=TRUE)
            Name Frost   Area
1         Alaska   152 566432
2        Arizona    NA 113417
3     California    NA 156361
4       Colorado   166 103766
5          Maine   161     NA
6      Minnesota   160     NA
7        Montana   155 145587
8         Nevada   188 109889
9  New Hampshire   174     NA
10    New Mexico    NA 121412
11  North Dakota   186     NA
12  South Dakota   172     NA
13         Texas    NA 262134
14       Vermont   168     NA
15       Wyoming   173     NA
> 
> 
> ## Working with lookup tables
> 
> ### Finding a match
> 
> index <- match(cold.states$Name, large.states$Name)
> index
 [1]  1  4 NA NA  5  6 NA NA NA NA NA
> 
> large.states[na.omit(index), ]
       Name   Area
2    Alaska 566432
6  Colorado 103766
26  Montana 145587
28   Nevada 109889
> 
> ### Making sense of %in%
> 
> index <- cold.states$Name %in% large.states$Name
> index
 [1]  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
> !is.na(match(cold.states$Name,large.states$Name))
 [1]  TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
> cold.states[index, ]
       Name Frost
2    Alaska   152
6  Colorado   166
26  Montana   155
28   Nevada   188
> 
> # Sorting and Ordering Data
> 
> some.states <- data.frame(
+      Region = state.region,
+      state.x77)
> 
> some.states <- some.states[1:10, 1:3]
> some.states
               Region Population Income
Alabama         South       3615   3624
Alaska           West        365   6315
Arizona          West       2212   4530
Arkansas        South       2110   3378
California       West      21198   5114
Colorado         West       2541   4884
Connecticut Northeast       3100   5348
Delaware        South        579   4809
Florida         South       8277   4815
Georgia         South       4931   4091
> 
> ## Sorting vectors
> 
> ### Sorting a vector in ascending order
> 
> sort(some.states$Population)
 [1]   365   579  2110  2212  2541  3100  3615  4931  8277 21198
> 
> ### Sorting a vector in decreasing order
> 
> sort(some.states$Population, decreasing=TRUE)
 [1] 21198  8277  4931  3615  3100  2541  2212  2110   579   365
> 
> ## Sorting data frames
> 
> ### Getting the order
> 
> order.pop <- order(some.states$Population)
> order.pop
 [1]  2  8  4  3  6  7  1 10  9  5
> 
> some.states$Population[order.pop]
 [1]   365   579  2110  2212  2541  3100  3615  4931  8277 21198
> 
> ## Sorting a data frame in ascending order
> 
> some.states[order.pop, ]
               Region Population Income
Alaska           West        365   6315
Delaware        South        579   4809
Arkansas        South       2110   3378
Arizona          West       2212   4530
Colorado         West       2541   4884
Connecticut Northeast       3100   5348
Alabama         South       3615   3624
Georgia         South       4931   4091
Florida         South       8277   4815
California       West      21198   5114
> order(some.states$Population)
 [1]  2  8  4  3  6  7  1 10  9  5
> order(some.states$Population, decreasing=TRUE)
 [1]  5  9 10  1  7  6  3  4  8  2
> 
> some.states[order(some.states$Population, decreasing=TRUE), ]
               Region Population Income
California       West      21198   5114
Florida         South       8277   4815
Georgia         South       4931   4091
Alabama         South       3615   3624
Connecticut Northeast       3100   5348
Colorado         West       2541   4884
Arizona          West       2212   4530
Arkansas        South       2110   3378
Delaware        South        579   4809
Alaska           West        365   6315
> 
> ### Sorting on more than one column
> 
> index <- with(some.states, order(Region, Population))
> some.states[index, ]
               Region Population Income
Connecticut Northeast       3100   5348
Delaware        South        579   4809
Arkansas        South       2110   3378
Alabama         South       3615   3624
Georgia         South       4931   4091
Florida         South       8277   4815
Alaska           West        365   6315
Arizona          West       2212   4530
Colorado         West       2541   4884
California       West      21198   5114
> 
> ### Sorting multiple columns in mixed order
> index <- order(-xtfrm(some.states$Region), some.states$Population)
> some.states[index, ]
               Region Population Income
Alaska           West        365   6315
Arizona          West       2212   4530
Colorado         West       2541   4884
California       West      21198   5114
Delaware        South        579   4809
Arkansas        South       2110   3378
Alabama         South       3615   3624
Georgia         South       4931   4091
Florida         South       8277   4815
Connecticut Northeast       3100   5348
> 
> # Traversing Your Data with the Apply Functions
> 
> ## Using the apply() function to summarize arrays
> 
> str(Titanic)
 table [1:4, 1:2, 1:2, 1:2] 0 0 35 0 0 0 17 0 118 154 ...
 - attr(*, "dimnames")=List of 4
  ..$ Class   : chr [1:4] "1st" "2nd" "3rd" "Crew"
  ..$ Sex     : chr [1:2] "Male" "Female"
  ..$ Age     : chr [1:2] "Child" "Adult"
  ..$ Survived: chr [1:2] "No" "Yes"
> apply(Titanic, 1, sum)
 1st  2nd  3rd Crew 
 325  285  706  885 
> apply(Titanic, 3, sum)
Child Adult 
  109  2092 
> apply(Titanic, c(3, 4), sum)
       Survived
Age       No Yes
  Child   52  57
  Adult 1438 654
> 
> ## Using lapply() and sapply() to traverse a list or data frame
> 
> lapply(iris, class)
$Sepal.Length
[1] "numeric"

$Sepal.Width
[1] "numeric"

$Petal.Length
[1] "numeric"

$Petal.Width
[1] "numeric"

$Species
[1] "factor"

$ratio
[1] "numeric"

> sapply(iris, class)
Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species        ratio 
   "numeric"    "numeric"    "numeric"    "numeric"     "factor"    "numeric" 
> sapply(iris, mean)
Warning in mean.default(X[[i]], ...) :
  argument is not numeric or logical: returning NA
Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species        ratio 
    5.843333     3.057333     3.758000     1.199333           NA     1.953681 
> sapply(iris, function(x) ifelse(is.numeric(x), mean(x), NA))
Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species        ratio 
    5.843333     3.057333     3.758000     1.199333           NA     1.953681 
> 
> ## Using tapply() to create tabular summaries
> 
> tapply(iris$Sepal.Length, iris$Species, mean)
    setosa versicolor  virginica 
     5.006      5.936      6.588 
> with(iris, tapply(Sepal.Length, Species, mean))
    setosa versicolor  virginica 
     5.006      5.936      6.588 
> 
> ### Using tapply() to create higher-dimensional tables
> 
> str(mtcars)
'data.frame':	32 obs. of  11 variables:
 $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
 $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
 $ disp: num  160 160 108 258 360 ...
 $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
 $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
 $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
 $ qsec: num  16.5 17 18.6 19.4 17 ...
 $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
 $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
 $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
 $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
> cars <- within(mtcars,
+     am <- factor(am, levels=0:1, labels=c("Automatic", "Manual"))
+ )
> 
> with(cars, tapply(mpg, am, mean))
Automatic    Manual 
 17.14737  24.39231 
> with(cars, tapply(mpg, list(gear, am), mean))
  Automatic Manual
3  16.10667     NA
4  21.05000 26.275
5        NA 21.380
> 
> ### Using aggregate()
> 
> with(cars, aggregate(mpg, list(gear=gear, am=am), mean))
  gear        am        x
1    3 Automatic 16.10667
2    4 Automatic 21.05000
3    4    Manual 26.27500
4    5    Manual 21.38000
> 
> # Getting to Know the Formula Interface
> 
> 
> aggregate(mpg ~ gear + am, data=cars, mean)
  gear        am      mpg
1    3 Automatic 16.10667
2    4 Automatic 21.05000
3    4    Manual 26.27500
4    5    Manual 21.38000
> 
> aov(mpg ~ gear + am, data=cars)
Call:
   aov(formula = mpg ~ gear + am, data = cars)

Terms:
                    gear       am Residuals
Sum of Squares  259.7492 145.4497  720.8483
Deg. of Freedom        1        1        29

Residual standard error: 4.985663
Estimated effects may be unbalanced
> 
> library(lattice)
> xyplot(mpg ~ gear + am, data=cars)
> 
> 
> # Whipping Your Data into Shape
> 
> 
> ## Understanding data in long and wide format
> 
> 
> ## Getting started with the reshape2 package
> 
> ## Not run: 
> ##D install.packages("reshape2")
> ## End(Not run)
> library("reshape2")
> 
> goals <- data.frame(
+     Game = c("1st", "2nd", "3rd", "4th"),
+     Venue = c("Bruges", "Ghent", "Ghent", "Bruges"),
+     Granny = c(12, 4, 5, 6),
+     Geraldine = c(5, 4, 2, 4),
+     Gertrude = c(11, 5, 6, 7)
+ )
> 
> ## Melting data to long format
> 
> mgoals <- melt(goals)
Using Game, Venue as id variables
> mgoals <- melt(goals, id.vars=c("Game", "Venue"))
> mgoals
   Game  Venue  variable value
1   1st Bruges    Granny    12
2   2nd  Ghent    Granny     4
3   3rd  Ghent    Granny     5
4   4th Bruges    Granny     6
5   1st Bruges Geraldine     5
6   2nd  Ghent Geraldine     4
7   3rd  Ghent Geraldine     2
8   4th Bruges Geraldine     4
9   1st Bruges  Gertrude    11
10  2nd  Ghent  Gertrude     5
11  3rd  Ghent  Gertrude     6
12  4th Bruges  Gertrude     7
> 
> ## Casting data to wide format
> 
> dcast(mgoals,  Venue + Game ~ variable, sum)
   Venue Game Granny Geraldine Gertrude
1 Bruges  1st     12         5       11
2 Bruges  4th      6         4        7
3  Ghent  2nd      4         4        5
4  Ghent  3rd      5         2        6
> dcast(mgoals, variable ~ Venue , sum)
   variable Bruges Ghent
1    Granny     18     9
2 Geraldine      9     6
3  Gertrude     18    11
> dcast(mgoals,  Venue ~ variable , sum)
   Venue Granny Geraldine Gertrude
1 Bruges     18         9       18
2  Ghent      9         6       11
> 
> dcast(mgoals,  Venue + variable ~ Game , sum)
   Venue  variable 1st 2nd 3rd 4th
1 Bruges    Granny  12   0   0   6
2 Bruges Geraldine   5   0   0   4
3 Bruges  Gertrude  11   0   0   7
4  Ghent    Granny   0   4   5   0
5  Ghent Geraldine   0   4   2   0
6  Ghent  Gertrude   0   5   6   0
> 
> library(ggplot2)
> ggplot(mgoals, aes(x=variable, y=value, fill=Game)) + geom_bar()
Error: stat_count() must not be used with a y aesthetic.
Execution halted
```
```
DONE
Status: 1 ERROR
```

## rfPermute (1.9.2)
Maintainer: Eric Archer <eric.archer@noaa.gov>

```
checking whether package ‘rfPermute’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘rfPermute’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘rfPermute’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/rfPermute.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## rgbif (0.8.9)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rgbif/issues

__OK__

## RGraphics (2.0-12)
Maintainer: Paul Murrell <paul@stat.auckland.ac.nz>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘graph’ ‘hyperdraw’ ‘hypergraph’ ‘rggobi’ ‘Rgraphviz’ ‘SVGAnnotation’
```
```
checking whether package ‘RGraphics’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘ggplot2::unit’ when loading ‘RGraphics’
  Warning: replacing previous import by ‘ggplot2::arrow’ when loading ‘RGraphics’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/RGraphics.Rcheck/00install.out’ for details.
```
```
checking installed package size ... NOTE
  installed size is  9.6Mb
  sub-directories of 1Mb or more:
    extra   9.4Mb
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘Hmisc’ ‘MASS’ ‘Rcmdr’ ‘Rgraphviz’ ‘SVGAnnotation’ ‘TeachingDemos’
  ‘agricolae’ ‘animation’ ‘circular’ ‘cluster’ ‘colorspace’ ‘diagram’
  ‘gWidgetsRGtk2’ ‘gplots’ ‘grImport’ ‘graph’ ‘gridBase’ ‘gridExtra’
  ‘hexbin’ ‘hyperdraw’ ‘hypergraph’ ‘igraph’ ‘iplots’ ‘ipred’
  ‘latticeExtra’ ‘mapdata’ ‘maps’ ‘maptools’ ‘misc3d’ ‘network’
  ‘openair’ ‘oz’ ‘party’ ‘pixmap’ ‘playwith’ ‘plotrix’ ‘pmg’ ‘png’
  ‘quantmod’ ‘raster’ ‘rgdal’ ‘rggobi’ ‘rgl’ ‘scatterplot3d’
  ‘soiltexture’ ‘symbols’ ‘vcd’ ‘vcdExtra’ ‘venneuler’ ‘vrmlgen’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Missing or unexported objects:
  ‘gridExtra::grid.barbed’ ‘gridExtra::grid.pattern’
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  plot.newclass
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
figure5.14: no visible global function definition for ‘opts’
figure5.14: no visible global function definition for ‘theme_text’
figure5.14: no visible global function definition for ‘theme_blank’
figure5.4: no visible global function definition for ‘opts’
```
```
DONE
Status: 1 WARNING, 5 NOTEs
```

## RIGHT (0.2.0)
Maintainer: Jonghyun Bae <bnbbkr@gmail.com>  
Bug reports: https://groups.google.com/forum/?hl=en#!forum/right-user

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'createGgplot.Rd':
  ‘ggplot2’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
checking tests ... ERROR
Running the tests in ‘tests/testAll.R’ failed.
Last 13 lines of output:
  4: createQplot(x = conc, y = Time, data = Theoph, geom = "point") at testQplot_RIGHT.R:18
  5: ggplot_RIGHT(obj)
  
  4. Error: Check color option: --------------------------------------------------
  argument is of length zero
  1: withCallingHandlers(eval(code, new_test_environment), error = capture_calls, message = function(c) invokeRestart("muffleMessage"), 
         warning = function(c) invokeRestart("muffleWarning"))
  2: eval(code, new_test_environment)
  3: eval(expr, envir, enclos)
  4: createQplot(x = conc, y = Time, data = Theoph, colour = Subject, geom = "point") at testQplot_RIGHT.R:84
  5: ggplot_RIGHT(obj)
  Error: Test failures
  Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## rinat (0.1.4)
Maintainer: Edmund Hart <edmund.m.hart@gmail.com>

__OK__

## rivr (1.0)
Maintainer: Michael C Koohafkan <michael.koohafkan@gmail.com>  
Bug reports: https://github.com/mkoohafkan/rivr/issues

__OK__

## RJafroc (0.1.1)
Maintainer: Xuetong Zhai <xuetong.zhai@gmail.com>

__OK__

## rms (4.4-0)
Maintainer: Frank E Harrell Jr <f.harrell@vanderbilt.edu>

```
checking whether package ‘rms’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/rms.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## rnoaa (0.4.2)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: http://www.github.com/ropensci/rnoaa/issues

__OK__

## robustbase (0.92-5)
Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>

```
checking whether package ‘robustbase’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/robustbase.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## RobustEM (1.0)
Maintainer: Aishat Aloba <adetokaloba@gmail.com>

```
checking whether package ‘RobustEM’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/RobustEM.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## robustHD (0.5.0)
Maintainer: Andreas Alfons <alfons@ese.eur.nl>

```
checking whether package ‘robustHD’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/robustHD.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## robustlmm (1.7-6)
Maintainer: Manuel Koller <koller.manuel@gmail.com>

```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘rlmer.Rnw’
  ...
+     plate <- reorder(plate, diameter)
+     attr(plate, "scores") <- NULL
+ })

> print(ggplot(Penicillin, aes(plate, diameter, color = sample)) + 
+     geom_point() + geom_line(aes(as.numeric(plate))) + scale_colour_brewer("Samp ..." ... [TRUNCATED] 

  When sourcing ‘rlmer.R’:
Error: attempt to replicate an object of type 'language'
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: ggplot2
Loading required package: reshape2
Loading required package: grid
Loading required package: xtable
Loading required package: robustlmm
Loading required package: lme4
Loading required package: Matrix
Loading required package: robustbase

Attaching package: ‘robustbase’

The following object is masked from ‘package:lme4’:

    sigma


Error: processing vignette 'rlmer.Rnw' failed with diagnostics:
 chunk 3 (label = penicillin-raw) 
Error in rep(value[[k]], length.out = n) : 
  attempt to replicate an object of type 'language'
Execution halted

```
```
DONE
Status: 1 ERROR, 1 NOTE
```

## rollply (0.4.2)
Maintainer: Alexandre Genin <alex@lecairn.org>  
Bug reports: https://github.com/alexgenin/rollply

__OK__

## rorutadis (0.1.3)
Maintainer: Krzysztof Ciomek <k.ciomek@gmail.com>

__OK__

## rotations (1.4)
Maintainer: Bryan Stanfill <bstanfill2003@gmail.com>  
Bug reports: https://github.com/stanfill/rotationsC/issues

```
checking whether package ‘rotations’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/rotations.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## rpf (0.48)
Maintainer: Joshua Pritikin <jpritikin@pobox.com>

```
checking whether package ‘rpf’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/rpf.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## rplexos (1.1.4)
Maintainer: Eduardo Ibanez <edu.ibanez@gmail.com>  
Bug reports: https://github.com/NREL/rplexos/issues

__OK__

## rplos (0.5.4)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rplos/issues

__OK__

## RPPanalyzer (1.4.1)
Maintainer: Astrid Wachter <astrid.wachter@med.uni-goettingen.de>

```
checking package dependencies ... ERROR
Packages required but not available: ‘limma’ ‘Biobase’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## RSA (0.9.8)
Maintainer: Felix Schönbrodt <felix@nicebread.de>

```
checking whether package ‘RSA’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/RSA.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## RSDA (1.2)
Maintainer: Oldemar Rodriguez <oldemar.rodriguez@ucr.ac.cr>

```
checking whether package ‘RSDA’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘RSDA’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/RSDA.Rcheck/00install.out’ for details.
```
```
checking R code for possible problems ... NOTE
classic.to.sym: no visible binding for global variable ‘fn’
process.continuum.variable: no visible binding for global variable ‘fn’
process.histogram.variable: no visible binding for global variable ‘fn’
process.interval.variable: no visible binding for global variable ‘fn’
process.set.variable: no visible binding for global variable ‘fn’
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## rSPACE (1.1.1)
Maintainer: Martha Ellis <martha.ellis@gmail.com>  
Bug reports: http://github.com/mmellis/rSPACE/issues

__OK__

## rstan (2.8.0)
Maintainer: Ben Goodrich <benjamin.goodrich@columbia.edu>

```
checking installed package size ... NOTE
  installed size is  8.2Mb
  sub-directories of 1Mb or more:
    libs   5.8Mb
```
```
DONE
Status: 1 NOTE
```

## RStoolbox (0.1.1)
Maintainer: Benjamin Leutner <benjamin.leutner@uni-wuerzburg.de>  
Bug reports: https://github.com/bleutner/RStoolbox/issues

__OK__

## rtematres (0.2)
Maintainer: Claas-Thido Pfaff <claas-thido.pfaff@idiv-biodiversity.de>

__OK__

## rtf (0.4-11)
Maintainer: Michael E. Schaffer <mschaff@gmail.com>

```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  addHeader.RTF addNewLine.RTF addPageBreak.RTF addParagraph.RTF
  addPlot.RTF addPng.RTF addSessionInfo.RTF addTOC.RTF addTable.RTF
  addText.RTF addTrellisObject.RTF decreaseIndent.RTF done.RTF
  endParagraph.RTF increaseIndent.RTF setFontSize.RTF
  startParagraph.RTF view.RTF
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 NOTE
```

## rtimes (0.3.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropengov/rtimes/issues

__OK__

## rvertnet (0.3.4)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/rvertnet/issues

__OK__

## rWBclimate (0.1.3)
Maintainer: Edmund Hart <edmund.m.hart@gmail.com>  
Bug reports: http://github.com/ropensci/rWBclimate/issues

```
checking R code for possible problems ... NOTE
check_ISO_code: no visible binding for global variable ‘NoAm_country’
check_ISO_code: no visible binding for global variable ‘SoAm_country’
check_ISO_code: no visible binding for global variable ‘Oceana_country’
check_ISO_code: no visible binding for global variable ‘Africa_country’
check_ISO_code: no visible binding for global variable ‘Asia_country’
check_ISO_code: no visible binding for global variable ‘Eur_country’
```
```
DONE
Status: 1 NOTE
```

## rwirelesscom (1.4.2)
Maintainer: Alberto Gutierrez <algutier1@gmail.com>

__OK__

## Rz (0.9-1)
Maintainer: Masahiro Hayashi <rinm884@gmail.com>

```
checking dependencies in R code ... WARNING
'library' or 'require' call to ‘vcd’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Packages in Depends field not imported from:
  ‘foreign’ ‘ggplot2’ ‘grid’ ‘psych’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
Unexported object imported by a ':::' call: ‘foreign:::adQuote’
  See the note in ?`:::` about the use of this operator.
  Including base/recommended package(s):
  ‘foreign’
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  summary.CrossTable
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
gtkFileChooserDialogFilteredActivate: no visible binding for global
  variable ‘theme_grey’
summary.CrossTable: no visible global function definition for
  ‘assocstats’
```
```
DONE
Status: 1 WARNING, 2 NOTEs
```

## sadists (0.2.1)
Maintainer: Steven E. Pav <shabbychef@gmail.com>  
Bug reports: https://github.com/shabbychef/sadists/issues

__OK__

## saeSim (0.7.0)
Maintainer: Sebastian Warnholz <Sebastian.Warnholz@fu-berlin.de>  
Bug reports: https://github.com/wahani/saeSim/issues

__OK__

## SamplerCompare (1.2.7)
Maintainer: Madeleine Thompson <madeleineth@gmail.com>

```
checking whether package ‘SamplerCompare’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/SamplerCompare.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## sand (1.0.2)
Maintainer: Gabor Csardi <csardi.gabor@gmail.com>  
Bug reports: https://github.com/kolaczyk/sand/issues

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘GO.db’ ‘GOstats’ ‘org.Sc.sgd.db’
```
```
checking data for non-ASCII characters ... NOTE
  Note: found 6 marked UTF-8 strings
```
```
DONE
Status: 2 NOTEs
```

## SCGLR (2.0.2)
Maintainer: Guillaume Cornu <gcornu@cirad.fr>

```
checking whether package ‘SCGLR’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘SCGLR’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘SCGLR’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/SCGLR.Rcheck/00install.out’ for details.
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 242-243 (scglrVignettes.rnw) 
Error: processing vignette 'scglrVignettes.rnw' failed with diagnostics:
stat_count() must not be used with a y aesthetic.
Execution halted

```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## SciencesPo (1.3.7)
Maintainer: Daniel Marcelino <dmarcelino@live.com>  
Bug reports: http://github.com/danielmarcelino/SciencesPo

__OK__

## scmamp (0.2.3)
Maintainer: Borja Calvo <borja.calvo@ehu.eus>  
Bug reports: https://github.com/b0rxa/scmamp/issues

```
checking package dependencies ... ERROR
Packages required but not available: ‘graph’ ‘Rgraphviz’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## SDaA (0.1-3)
Maintainer: Tobias Verbeke <tobias.verbeke@openanalytics.eu>

__OK__

## sdcMicro (4.6.0)
Maintainer: Matthias Templ <matthias.templ@gmail.com>

__OK__

## sdmvspecies (0.3.1)
Maintainer: Howl Anderson <u1mail2me@gmail.com>

```
checking for executable files ... WARNING
Found the following executable file:
  inst/external/env/bio14.bil
Source packages should not contain undeclared executable files.
See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
```
```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘sdmvspecies.Rnw’
  ...

> plot.data <- data.frame(environment = env, suitability = suitability)

> ggplot() + theme_bw() + theme(axis.line = element_blank(), 
+     axis.text.x = element_blank(), axis.text.y = element_blank(), 
+     axis.ticks =  .... [TRUNCATED] 

  When sourcing ‘sdmvspecies.R’:
Error: Attempted to create layer with no stat.
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: GPArotation
Loading required package: sp
[1] "/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sdmvspecies.Rcheck/sdmvspecies/external/env/bio1.bil" 
[2] "/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sdmvspecies.Rcheck/sdmvspecies/external/env/bio11.bil"
[3] "/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sdmvspecies.Rcheck/sdmvspecies/external/env/bio12.bil"
[4] "/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sdmvspecies.Rcheck/sdmvspecies/external/env/bio14.bil"
[5] "/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sdmvspecies.Rcheck/sdmvspecies/external/env/bio16.bil"
[6] "/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sdmvspecies.Rcheck/sdmvspecies/external/env/bio5.bil" 
[7] "/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sdmvspecies.Rcheck/sdmvspecies/external/env/bio7.bil" 

Error: processing vignette 'sdmvspecies.Rnw' failed with diagnostics:
 chunk 3 (label = bell_shaped_response_function) 
Error : Attempted to create layer with no stat.
Execution halted

```
```
DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE
```

## season (0.3-5)
Maintainer: Adrian Barnett <a.barnett@qut.edu.au>

__OK__

## SEERaBomb (2015.2)
Maintainer: Tomas Radivoyevitch <radivot@ccf.org>

__OK__

## seewave (2.0.2)
Maintainer: Jerome Sueur <sueur@mnhn.fr>

```
checking whether package ‘seewave’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/seewave.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## selfea (1.0.1)
Maintainer: Lang Ho Lee <langholee@gmail.com>

__OK__

## SemiParBIVProbit (3.6)
Maintainer: Giampiero Marra <giampiero.marra@ucl.ac.uk>

__OK__

## sensitivity (1.11.1)
Maintainer: Bertrand Iooss <biooss@yahoo.fr>

```
checking whether package ‘sensitivity’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sensitivity.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## SensMixed (2.0-8)
Maintainer: Alexandra Kuznetsova <alku@dtu.dk>

```
checking R code for possible problems ... NOTE
.plotSensMixed: possible error in position_dodge(width = 0.9, height =
  10): unused argument (height = 10)
```
```
checking examples ... ERROR
Running examples in ‘SensMixed-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: plot
> ### Title: function creates plots for the sensmixed object
> ### Aliases: plot.sensmixed
> 
> ### ** Examples
> 
> res <- sensmixed(c("Coloursaturation", "Colourbalance"),
+                   Prod_effects=c("TVset"), 
+                   individual="Assessor", data=TVbo, MAM=TRUE, 
+                   reduce.random=FALSE)

  |                                                                            
  |                                                                      |   0%fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

  |                                                                            
  |===================================                                   |  50%fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

  |                                                                            
  |======================================================================| 100%
> plot(res)
Using effs, effs_short, abbreffs as id variables
Using effs as id variables
Error in position_dodge(width = 0.9, height = 10) : 
  unused argument (height = 10)
Calls: plot ... .plotSensMixed -> geom_point -> layer -> position_dodge
Execution halted
```
```
DONE
Status: 1 ERROR, 1 NOTE
```

## sgd (1.0)
Maintainer: Dustin Tran <dtran@g.harvard.edu>  
Bug reports: https://github.com/airoldilab/sgd/issues

```
checking whether package ‘sgd’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sgd.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## sglr (0.7)
Maintainer: Balasubramanian Narasimhan <naras@stat.stanford.edu>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
checking R code for possible problems ... NOTE
plotBoundary: no visible global function definition for
  ‘scale_y_continuous’
```
```
DONE
Status: 2 NOTEs
```

## shiny (0.12.2)
Maintainer: Winston Chang <winston@rstudio.com>  
Bug reports: https://github.com/rstudio/shiny/issues

```
checking installed package size ... NOTE
  installed size is  5.1Mb
  sub-directories of 1Mb or more:
    www   4.1Mb
```
```
DONE
Status: 1 NOTE
```

## shinystan (2.0.1)
Maintainer: Jonah Gabry <jsg2201@columbia.edu>  
Bug reports: https://github.com/stan-dev/shinystan/issues

__OK__

## sidier (3.0.1)
Maintainer: A.J. Muñoz-Pajares <ajesusmp@ugr.es>

__OK__

## simcausal (0.4.0)
Maintainer: Oleg Sofrygin <oleg.sofrygin@gmail.com>  
Bug reports: https://github.com/osofr/simcausal/issues

__OK__

## simmr (0.2)
Maintainer: Andrew Parnell <andrew.parnell@ucd.ie>

```
checking whether package ‘simmr’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/simmr.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## simPH (1.3.4)
Maintainer: Christopher Gandrud <christopher.gandrud@gmail.com>  
Bug reports: https://github.com/christophergandrud/simPH/issues

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'ggfitStrata.Rd':
  ‘ggplot2’

Missing link or links in documentation object 'simGG.siminteract.Rd':
  ‘ggplot2’

Missing link or links in documentation object 'simGG.simlinear.Rd':
  ‘ggplot2’

Missing link or links in documentation object 'simGG.simpoly.Rd':
  ‘ggplot2’

Missing link or links in documentation object 'simGG.simspline.Rd':
  ‘ggplot2’

Missing link or links in documentation object 'simGG.simtvc.Rd':
  ‘ggplot2’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
DONE
Status: 1 WARNING
```

## SimReg (1.2)
Maintainer: Daniel Greene <dg333@cam.ac.uk>

```
checking whether package ‘SimReg’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/SimReg.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## simTool (1.0.3)
Maintainer: Marsel Scheer <scheer@freescience.de>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
DONE
Status: 1 NOTE
```

## sinaplot (0.1.2)
Maintainer: Nikos Sidiropoulos <nikos.sidiro@gmail.com>

__OK__

## SixSigma (0.8-1)
Maintainer: Emilio L. Cano <emilio.lopez@urjc.es>

```
checking whether package ‘SixSigma’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘SixSigma’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘SixSigma’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/SixSigma.Rcheck/00install.out’ for details.
```
```
checking R code for possible problems ... NOTE
ss.rr: no visible binding for global variable ‘ss.data.rr’
```
```
checking examples ... ERROR
Running examples in ‘SixSigma-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: SixSigma
> ### Title: Six Sigma Tools for Quality and Process Improvement
> ### Aliases: SixSigma SixSigma-package
> ### Keywords: SixSigma package quality
> 
> ### ** Examples
> 
> example(ss.ci)

ss.ci> ss.ci(len, data=ss.data.strings, alpha = 0.05,
ss.ci+   sub = "Guitar Strings Test | String Length",
ss.ci+   xname = "Length")
	Mean = 950.016; sd = 0.267
	95% Confidence Interval= 949.967 to 950.064

Warning: `stat` is deprecated
Error: Unknown parameters: binwidth
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE
```

## sjPlot (1.8.4)
Maintainer: Daniel Lüdecke <d.luedecke@uke.de>  
Bug reports: https://github.com/sjPlot/devel/issues

```
checking whether package ‘sjPlot’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘sjPlot’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sjPlot.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘sjPlot-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: adjust_plot_range
> ### Title: Adjust y range of ggplot-objects
> ### Aliases: adjust_plot_range
> 
> ### ** Examples
> 
> # sample data set
> library(sjmisc)
> data(efc)
> # show frequencies of relationship-variable and
> # retrieve plot object
> gp <- sjp.frq(efc$e15relat, printPlot = FALSE)
> # show current plot
> plot(gp$plot)
Error: Aesthetics must be either length 1 or the same as the data (8): fill
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## slackr (1.2)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/slackr/issues

__OK__

## SmarterPoland (1.5)
Maintainer: Przemyslaw Biecek <przemyslaw.biecek@gmail.com>

```
checking data for non-ASCII characters ... NOTE
  Note: found 1122 marked UTF-8 strings
```
```
DONE
Status: 1 NOTE
```

## SMFI5 (1.0)
Maintainer: Bruno Remillard <bruno.remillard@hec.ca>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls to packages already attached by Depends:
  ‘ggplot2’ ‘reshape’
  Please remove these calls from your code.
Packages in Depends field not imported from:
  ‘corpcor’ ‘ggplot2’ ‘reshape’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
bond.cir: no visible global function definition for ‘melt’
bond.cir: no visible global function definition for ‘ggplot’
bond.cir: no visible global function definition for ‘aes’
bond.cir: no visible global function definition for ‘geom_line’
bond.cir: no visible global function definition for ‘ggtitle’
bond.vasicek: no visible global function definition for ‘melt’
bond.vasicek: no visible global function definition for ‘ggplot’
bond.vasicek: no visible global function definition for ‘aes’
bond.vasicek: no visible global function definition for ‘geom_line’
bond.vasicek: no visible global function definition for ‘ggtitle’
est.cir: no visible global function definition for ‘melt’
est.cir: no visible global function definition for ‘ggplot’
est.cir: no visible global function definition for ‘aes’
est.cir: no visible global function definition for ‘geom_line’
est.cir: no visible global function definition for ‘ggtitle’
est.cir: no visible global function definition for ‘pseudoinverse’
est.feller: no visible global function definition for ‘pseudoinverse’
est.ou: no visible global function definition for ‘pseudoinverse’
est.vasicek: no visible global function definition for ‘melt’
est.vasicek: no visible global function definition for ‘ggplot’
est.vasicek: no visible global function definition for ‘aes’
est.vasicek: no visible global function definition for ‘geom_line’
est.vasicek: no visible global function definition for ‘ggtitle’
est.vasicek: no visible global function definition for ‘pseudoinverse’
sim.cir: no visible global function definition for ‘melt’
sim.cir: no visible global function definition for ‘ggplot’
sim.cir: no visible global function definition for ‘aes’
sim.cir: no visible global function definition for ‘geom_line’
sim.cir: no visible global function definition for ‘ggtitle’
sim.vasicek: no visible global function definition for ‘melt’
sim.vasicek: no visible global function definition for ‘ggplot’
sim.vasicek: no visible global function definition for ‘aes’
sim.vasicek: no visible global function definition for ‘geom_line’
sim.vasicek: no visible global function definition for ‘ggtitle’
```
```
DONE
Status: 2 NOTEs
```

## smoof (1.0)
Maintainer: Jakob Bossek <j.bossek@gmail.com>  
Bug reports: https://github.com/jakobbossek/smoof/issues

__OK__

## snht (1.0.2)
Maintainer: Josh Browning <jbrownin@mines.edu>

```
checking files in ‘vignettes’ ... NOTE
The following directory looks like a leftover from 'knitr':
  ‘figure’
Please remove from your package.
```
```
DONE
Status: 1 NOTE
```

## snpEnrichment (1.7.0)
Maintainer: Mickael Canouil <mickael.canouil@cnrs.fr>  
Bug reports: https://github.com/mcanouil/snpEnrichment/issues

```
checking package dependencies ... ERROR
Package required but not available: ‘snpStats’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## soc.ca (0.7.2)
Maintainer: Anton Grau Larsen <alar@soc.ku.dk>

```
checking whether package ‘soc.ca’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘soc.ca’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘soc.ca’
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘soc.ca’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/soc.ca.Rcheck/00install.out’ for details.
```
```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'map.density.Rd':
  ‘ggplot2’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
checking data for non-ASCII characters ... NOTE
  Note: found 833 marked UTF-8 strings
```
```
checking examples ... ERROR
Running examples in ‘soc.ca-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: map.density
> ### Title: Density plot for the cloud of individuals
> ### Aliases: map.density
> 
> ### ** Examples
> 
> example(soc.ca)

soc.ca> data(taste)

soc.ca> # Create a data frame of factors containing all the active variables
soc.ca> taste          <- taste[which(taste$Isup == 'Active'), ]

soc.ca> attach(taste)

soc.ca> active         <- data.frame(TV, Film, Art, Eat)

soc.ca> sup            <- data.frame(Gender, Age, Income)

soc.ca> detach(taste)

soc.ca> # Runs the analysis
soc.ca> result         <- soc.mca(active, sup)
> map.density(result, map.ind(result, dim = 2:3, point.alpha = 0.2))
> map.density(result, map.ind(result, legend = TRUE, point.alpha = 0.2),
+  group = duplicated(active), color = duplicated(active),
+  linetype = duplicated(active))
> map.density(result, map.ctr(result))
> map.density(result, bins = 50)
Error: Unknown parameters: bins
Execution halted
```
```
DONE
Status: 1 ERROR, 2 WARNINGs, 1 NOTE
```

## Sofi (0.0.26)
Maintainer: Jose D. Loera <jose.loera@inegi.org.mx>  
Bug reports: https://github.com/loerasg/Sofi/issues

__OK__

## solarius (0.2.3)
Maintainer: Andrey Ziyatdinov <andrey.ziyatdinov@upc.edu>

__OK__

## sorvi (0.7.26)
Maintainer: Leo Lahti <louhos@googlegroups.com>  
Bug reports: https://github.com/ropengov/sorvi/issues

__OK__

## sotkanet (0.9.21)
Maintainer: Leo Lahti <louhos@googlegroups.com>  
Bug reports: https://github.com/ropengov/sotkanet/issues

__OK__

## SpaDES (1.0.1)
Maintainer: Alex M Chubaty <achubaty@NRCan.gc.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

```
checking package dependencies ... ERROR
Package required but not available: ‘secr’

Package suggested but not available for checking: ‘fastshp’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## sparkTable (1.0.0)
Maintainer: Alexander Kowarik <alexander.kowarik@statistik.gv.at>

```
checking whether package ‘sparkTable’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘sparkTable’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘sparkTable’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sparkTable.Rcheck/00install.out’ for details.
```
```
checking R code for possible problems ... NOTE
checkerplot: no visible global function definition for ‘pixmapGrob’
checkerplot: possible error in scale_y_continuous(limits = c(yLim_min,
  yLim_max), breaks = ybreaks, labels = ylabels, formatter =
  formatter): unused argument (formatter = formatter)
```
```
DONE
Status: 1 WARNING, 1 NOTE
```

## SparseFactorAnalysis (1.0)
Maintainer: Marc Ratkovic <ratkovic@princeton.edu>

```
checking whether package ‘SparseFactorAnalysis’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/SparseFactorAnalysis.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## sparsereg (1.1)
Maintainer: Marc Ratkovic <ratkovic@princeton.edu>

```
checking whether package ‘sparsereg’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sparsereg.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## spcosa (0.3-5)
Maintainer: Dennis Walvoort <dennis.Walvoort@wur.nl>

```
checking dependencies in R code ... NOTE
Packages in Depends field not imported from:
  ‘ggplot2’ ‘methods’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
':::' call which should be '::': ‘utils:::packageDescription’
  See the note in ?`:::` about the use of this operator.
```
```
checking R code for possible problems ... NOTE
coerce,CompactStratification-SpatialPixels: no visible global function
  definition for ‘gridded<-’
coerce,CompactStratification-SpatialPixelsDataFrame: no visible global
  function definition for ‘SpatialPixelsDataFrame’
coerce,SamplingPatternRandomComposite-SpatialPointsDataFrame: no
  visible global function definition for ‘SpatialPointsDataFrame’
plot,CompactStratification-SamplingPattern: no visible global function
  definition for ‘coordinates’
plot,CompactStratification-SamplingPattern: no visible global function
  definition for ‘geom_point’
plot,CompactStratification-SamplingPattern: no visible global function
  definition for ‘aes_string’
plot,CompactStratification-SamplingPatternPriorPoints: no visible
  global function definition for ‘coordinates’
plot,CompactStratification-SamplingPatternPriorPoints: no visible
  global function definition for ‘geom_point’
plot,CompactStratification-SamplingPatternPriorPoints: no visible
  global function definition for ‘aes_string’
plot,CompactStratification-SamplingPatternPriorPoints: no visible
  global function definition for ‘theme’
plot,CompactStratification-SamplingPatternRandomComposite: no visible
  global function definition for ‘coordinates’
plot,CompactStratification-SamplingPatternRandomComposite: no visible
  global function definition for ‘geom_point’
plot,CompactStratification-SamplingPatternRandomComposite: no visible
  global function definition for ‘aes_string’
plot,CompactStratification-SamplingPatternRandomComposite: no visible
  global function definition for ‘theme’
plot,CompactStratification-missing: no visible global function
  definition for ‘getGridTopology’
plot,CompactStratification-missing: no visible global function
  definition for ‘SpatialGridDataFrame’
plot,CompactStratification-missing: no visible global function
  definition for ‘coordnames’
plot,CompactStratification-missing: no visible global function
  definition for ‘coordinates’
plot,CompactStratification-missing: no visible global function
  definition for ‘ggplot’
plot,CompactStratification-missing: no visible global function
  definition for ‘geom_raster’
plot,CompactStratification-missing: no visible global function
  definition for ‘aes_string’
plot,CompactStratification-missing: no visible global function
  definition for ‘coord_fixed’
plot,CompactStratification-missing: no visible global function
  definition for ‘scale_x_continuous’
plot,CompactStratification-missing: no visible global function
  definition for ‘scale_y_continuous’
plot,CompactStratification-missing: no visible global function
  definition for ‘geom_segment’
plot,CompactStratification-missing: no visible global function
  definition for ‘theme’
plot,SamplingPattern-missing: no visible global function definition for
  ‘coordinates’
plot,SamplingPattern-missing: no visible global function definition for
  ‘ggplot’
plot,SamplingPattern-missing: no visible global function definition for
  ‘geom_point’
plot,SamplingPattern-missing: no visible global function definition for
  ‘aes_string’
plot,SamplingPattern-missing: no visible global function definition for
  ‘coord_fixed’
plot,SamplingPattern-missing: no visible global function definition for
  ‘scale_x_continuous’
plot,SamplingPattern-missing: no visible global function definition for
  ‘scale_y_continuous’
plot,SamplingPatternPriorPoints-missing: no visible global function
  definition for ‘coordinates’
plot,SamplingPatternPriorPoints-missing: no visible global function
  definition for ‘ggplot’
plot,SamplingPatternPriorPoints-missing: no visible global function
  definition for ‘geom_point’
plot,SamplingPatternPriorPoints-missing: no visible global function
  definition for ‘aes_string’
plot,SamplingPatternPriorPoints-missing: no visible global function
  definition for ‘coord_fixed’
plot,SamplingPatternPriorPoints-missing: no visible global function
  definition for ‘scale_x_continuous’
plot,SamplingPatternPriorPoints-missing: no visible global function
  definition for ‘scale_y_continuous’
plot,SamplingPatternPriorPoints-missing: no visible global function
  definition for ‘theme’
plot,SamplingPatternRandomComposite-missing: no visible global function
  definition for ‘coordinates’
plot,SamplingPatternRandomComposite-missing: no visible global function
  definition for ‘ggplot’
plot,SamplingPatternRandomComposite-missing: no visible global function
  definition for ‘geom_point’
plot,SamplingPatternRandomComposite-missing: no visible global function
  definition for ‘aes_string’
plot,SamplingPatternRandomComposite-missing: no visible global function
  definition for ‘coord_fixed’
plot,SamplingPatternRandomComposite-missing: no visible global function
  definition for ‘scale_x_continuous’
plot,SamplingPatternRandomComposite-missing: no visible global function
  definition for ‘scale_y_continuous’
spsample,CompactStratification-missing-missing: no visible global
  function definition for ‘coordinates’
spsample,CompactStratification-missing-missing: no visible global
  function definition for ‘%over%’
spsample,CompactStratification-missing-missing: no visible global
  function definition for ‘geometry’
spsample,CompactStratification-missing-missing: no visible global
  function definition for ‘coordinates<-’
spsample,CompactStratification-numeric-missing: no visible global
  function definition for ‘coordinates’
spsample,CompactStratification-numeric-missing: no visible global
  function definition for ‘SpatialPoints’
spsample,CompactStratificationPriorPoints-missing-missing: no visible
  global function definition for ‘coordinates’
spsample,CompactStratificationPriorPoints-missing-missing: no visible
  global function definition for ‘SpatialPoints’
```
```
checking examples ... ERROR
Running examples in ‘spcosa-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: estimate-methods
> ### Title: Estimating Statistics
> ### Aliases: estimate-methods estimate
> ###   estimate,character,CompactStratification,SamplingPatternRandomSamplingUnits,data.frame-method
> ###   estimate,character,CompactStratificationEqualArea,SamplingPatternRandomComposite,data.frame-method
> ###   estimate,SamplingVariance,CompactStratification,SamplingPatternRandomSamplingUnits,data.frame-method
> ###   estimate,SamplingVariance,CompactStratificationEqualArea,SamplingPatternRandomComposite,data.frame-method
> ###   estimate,SpatialCumulativeDistributionFunction,CompactStratification,SamplingPatternRandomSamplingUnits,data.frame-method
> ###   estimate,SpatialMean,CompactStratification,SamplingPatternRandomSamplingUnits,data.frame-method
> ###   estimate,SpatialMean,CompactStratificationEqualArea,SamplingPatternRandomComposite,data.frame-method
> ###   estimate,SpatialVariance,CompactStratification,SamplingPatternRandomSamplingUnits,data.frame-method
> ###   estimate,StandardError,CompactStratification,SamplingPatternRandomSamplingUnits,data.frame-method
> ### Keywords: methods
> 
> ### ** Examples
> 
> 
> # Note: the example below requires the 'rgdal'-package.
> # You may consider the 'maptools'-package as an alternative
> if (require(rgdal)) {
+     # read vector representation of the "Mijdrecht" area
+     shp <- readOGR(
+         dsn = system.file("maps", package = "spcosa"),
+         layer = "mijdrecht"
+     )
+ 
+     # stratify  into 30 strata
+     myStratification <- stratify(shp, nStrata = 30, nTry = 10, verbose = TRUE)
+ 
+     # random sampling of two sampling units per stratum
+     mySamplingPattern <- spsample(myStratification, n = 2)
+ 
+     # plot sampling pattern
+     plot(myStratification, mySamplingPattern)
+ 
+     # simulate data
+     # (in real world cases these data have to be obtained by field work etc.)
+     myData <- as(mySamplingPattern, "data.frame")
+     myData$observation <- rnorm(n = nrow(myData), mean = 10, sd = 1)
+ 
+     # design-based inference
+     estimate("spatial mean", myStratification, mySamplingPattern, myData["observation"])
+     estimate("sampling variance", myStratification, mySamplingPattern, myData["observation"])
+     estimate("standard error", myStratification, mySamplingPattern, myData["observation"])
+     estimate("spatial variance", myStratification, mySamplingPattern, myData["observation"])
+     estimate("scdf", myStratification, mySamplingPattern, myData["observation"])
+ }
Loading required package: rgdal
Loading required package: sp
rgdal: version: 1.0-4, (SVN revision 548)
 Geospatial Data Abstraction Library extensions to R successfully loaded
 Loaded GDAL runtime: GDAL 1.10.1, released 2013/08/26
 Path to GDAL shared files: /usr/local/Cellar/gdal/1.10.1_1/share/gdal
 Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March 2012, [PJ_VERSION: 480]
 Path to PROJ.4 shared files: (autodetected)
 Linking to sp version: 1.1-1 
OGR data source with driver: ESRI Shapefile 
Source: "/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/spcosa.Rcheck/spcosa/maps", layer: "mijdrecht"
with 1 features
It has 2 fields
2015-10-15 21:30:24 | optimizing configuration 1 
2015-10-15 21:30:24 |    current objective function value: 50485.85 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
2015-10-15 21:30:24 | optimizing configuration 2 
2015-10-15 21:30:24 |    current objective function value: 51048.45 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
2015-10-15 21:30:24 | optimizing configuration 3 
2015-10-15 21:30:24 |    current objective function value: 50670.82 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
2015-10-15 21:30:24 | optimizing configuration 4 
2015-10-15 21:30:24 |    current objective function value: 52082.42 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
2015-10-15 21:30:24 | optimizing configuration 5 
2015-10-15 21:30:24 |    current objective function value: 53706.91 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
2015-10-15 21:30:24 | optimizing configuration 6 
2015-10-15 21:30:24 |    current objective function value: 51047.8 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
2015-10-15 21:30:24 | optimizing configuration 7 
2015-10-15 21:30:24 |    current objective function value: 51216.01 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
2015-10-15 21:30:24 | optimizing configuration 8 
2015-10-15 21:30:24 |    current objective function value: 50634.7 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
2015-10-15 21:30:24 | optimizing configuration 9 
2015-10-15 21:30:24 |    current objective function value: 52254.34 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
2015-10-15 21:30:24 | optimizing configuration 10 
2015-10-15 21:30:24 |    current objective function value: 60713.64 
2015-10-15 21:30:24 |    minimum objective function value: 50485.85 
Error: Unknown parameters: colour
Execution halted
```
```
DONE
Status: 1 ERROR, 2 NOTEs
```

## spikeSlabGAM (1.1-9)
Maintainer: Fabian Scheipl
 <fabian.scheipl@stat.uni-muenchen.de>

```
checking whether package ‘spikeSlabGAM’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/spikeSlabGAM.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## spoccutils (0.1.0)
Maintainer: Scott Chamberlain <myrmecocystus@gmail.com>  
Bug reports: https://github.com/ropensci/spoccutils/issues

```
checking whether package ‘spoccutils’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘spoccutils’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/spoccutils.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## SPOT (1.0.5543)
Maintainer: Martin Zaefferer <martin.zaefferer@gmx.de>

__OK__

## sprm (1.1)
Maintainer: Irene Hoffmann <irene.hoffmann@tuwien.ac.at>

```
checking whether package ‘sprm’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘sprm’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘sprm’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/sprm.Rcheck/00install.out’ for details.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  biplot.prm biplot.sprm plot.prm plot.sprm predict.prm predict.sprm
  print.prm print.sprm summary.prm summary.sprm
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking examples ... ERROR
Running examples in ‘sprm-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: biplot.prm
> ### Title: Biplot for prm objects
> ### Aliases: biplot.prm
> 
> ### ** Examples
> 
> set.seed(5023)
> U <- c(rep(3,20), rep(4,30))
> X <- replicate(6, U+rnorm(50))
> beta <- c(rep(1, 3), rep(-1,3))
> e <- c(rnorm(45,0,1.5),rnorm(5,-20,1))
> y <- X%*%beta + e
> d <- as.data.frame(X)
> d$y <- y
> mod <- prms(y~., data=d, a=2, fun="Hampel")
> biplot(mod, comps = c(1, 2))
Error: Unknown parameters: environment
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING, 1 NOTE
```

## ss3sim (0.8.2)
Maintainer: Sean Anderson <sean@seananderson.ca>

```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘foreach’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
run_ss3sim: no visible global function definition for ‘%dopar%’
run_ss3sim: no visible global function definition for ‘foreach’
setup_parallel: no visible global function definition for
  ‘getDoParWorkers’
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 792-798 (ss3sim-vignette.Rnw) 
Error: processing vignette 'ss3sim-vignette.Rnw' failed with diagnostics:
Unknown parameters: width
Execution halted

```
```
DONE
Status: 3 NOTEs
```

## starma (1.1)
Maintainer: Felix Cheysson <felix@cheysson.fr>

```
checking whether package ‘starma’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/starma.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## statar (0.3.0)
Maintainer: Matthieu Gomez <mattg@princeton.edu>  
Bug reports: https://github.com/matthieugomez/statar/issues

```
checking whether package ‘statar’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/statar.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## statebins (1.0)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/statebins/issues

```
checking whether package ‘statebins’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘statebins’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘statebins’
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘statebins’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/statebins.Rcheck/00install.out’ for details.
```
```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'statebins.Rd':
  ‘ggplot2’

Missing link or links in documentation object 'statebins_continuous.Rd':
  ‘ggplot2’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```
```
DONE
Status: 2 WARNINGs
```

## StatRank (0.0.6)
Maintainer: Hossein Azari Soufiani <azari.hossein@gmail.com>

__OK__

## stcm (0.1.1)
Maintainer: Chris Krogslund <ckrogslund@berkeley.edu>

__OK__

## structSSI (1.1.1)
Maintainer: Kris Sankaran <kriss1@stanford.edu>

```
checking package dependencies ... ERROR
Package required but not available: ‘multtest’

Package suggested but not available for checking: ‘phyloseq’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## strvalidator (1.5.2)
Maintainer: Oskar Hansson <oskar.hansson@fhi.no>  
Bug reports: https://github.com/OskarHansson/strvalidator/issues

```
checking whether package ‘strvalidator’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::unit’ when loading ‘strvalidator’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/strvalidator.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## SuperLearner (2.0-15)
Maintainer: Eric Polley <eric.polley@nih.gov>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking: ‘genefilter’ ‘sva’
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘ROCR’ ‘cvAUC’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  predict.SL.bayesglm predict.SL.caret predict.SL.cforest
  predict.SL.earth predict.SL.gam predict.SL.gbm predict.SL.glm
  predict.SL.glmnet predict.SL.ipredbagg predict.SL.knn
  predict.SL.leekasso predict.SL.loess predict.SL.logreg
  predict.SL.mean predict.SL.nnet predict.SL.polymars
  predict.SL.randomForest predict.SL.ridge predict.SL.rpart
  predict.SL.step predict.SL.stepAIC predict.SL.svm predict.SL.template
  print.summary.CV.SuperLearner
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
CV.SuperLearner: no visible global function definition for ‘mclapply’
CV.SuperLearner: no visible global function definition for ‘parLapply’
SL.bayesglm: no visible global function definition for ‘bayesglm’
SL.caret: no visible global function definition for ‘trainControl’
SL.cforest: no visible global function definition for ‘cforest’
SL.cforest: no visible global function definition for
  ‘cforest_unbiased’
SL.gbm: no visible global function definition for ‘gbm’
SL.gbm: no visible global function definition for ‘gbm.perf’
SL.glmnet: no visible global function definition for ‘cv.glmnet’
SL.ipredbagg: no visible global function definition for ‘rpart.control’
SL.ipredbagg: no visible global function definition for ‘ipredbagg’
SL.knn: no visible global function definition for ‘knn’
SL.leekasso: no visible global function definition for ‘f.pvalue’
SL.logreg: no visible global function definition for ‘logreg’
SL.nnet: no visible global function definition for ‘nnet’
SL.polymars: no visible global function definition for ‘polymars’
SL.polymars: no visible global function definition for ‘polyclass’
SL.polymars: no visible global function definition for ‘ppolyclass’
SL.randomForest: no visible global function definition for
  ‘randomForest’
SL.ridge: no visible global function definition for ‘lm.ridge’
SL.rpart: no visible global function definition for ‘rpart’
SL.rpart: no visible global function definition for ‘rpart.control’
SL.rpartPrune: no visible global function definition for ‘rpart’
SL.rpartPrune: no visible global function definition for
  ‘rpart.control’
SL.rpartPrune: no visible global function definition for ‘prune’
SL.stepAIC: no visible global function definition for ‘stepAIC’
SL.svm: no visible global function definition for ‘svm’
mcSuperLearner: no visible global function definition for ‘mclapply’
method.AUC : <anonymous> : .cvRisk_AUC: no visible global function
  definition for ‘cvAUC’
method.AUC : <anonymous> : <anonymous>: no visible global function
  definition for ‘cvAUC’
method.CC_LS : computeCoef : compute: no visible global function
  definition for ‘solve.QP’
method.CC_nloglik : computeCoef: no visible global function definition
  for ‘nloptr’
method.NNLS2 : <anonymous> : .NNLS: no visible global function
  definition for ‘solve.QP’
plot.CV.SuperLearner: no visible global function definition for
  ‘dotplot’
plot.CV.SuperLearner : <anonymous>: no visible global function
  definition for ‘panel.xyplot’
plot.CV.SuperLearner : <anonymous>: no visible global function
  definition for ‘panel.segments’
plot.CV.SuperLearner: no visible global function definition for
  ‘ggplot’
plot.CV.SuperLearner: no visible global function definition for
  ‘aes_string’
plot.CV.SuperLearner: no visible global function definition for
  ‘geom_pointrange’
plot.CV.SuperLearner: no visible global function definition for
  ‘coord_flip’
plot.CV.SuperLearner: no visible global function definition for ‘ylab’
plot.CV.SuperLearner: no visible global function definition for ‘xlab’
predict.SL.knn: no visible global function definition for ‘knn’
predict.SL.polymars: no visible global function definition for
  ‘ppolyclass’
screen.SIS: no visible global function definition for ‘SIS’
screen.glmnet: no visible global function definition for ‘cv.glmnet’
screen.randomForest: no visible global function definition for
  ‘randomForest’
screen.ttest: no visible global function definition for ‘colttests’
snowSuperLearner: no visible global function definition for ‘parLapply’
snowSuperLearner: no visible global function definition for ‘parSapply’
summary.CV.SuperLearner: no visible global function definition for
  ‘cvAUC’
summary.CV.SuperLearner : <anonymous>: no visible global function
  definition for ‘cvAUC’
```
```
DONE
Status: 4 NOTEs
```

## survMisc (0.4.6)
Maintainer: Chris Dardis <christopherdardis@gmail.com>

```
checking whether package ‘survMisc’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘survMisc’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/survMisc.Rcheck/00install.out’ for details.
```
```
checking examples ... ERROR
Running examples in ‘survMisc-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: autoplot.rpart
> ### Title: Generate a 'ggplot' for an 'rpart' object
> ### Aliases: autoplot autoplot.rpart
> ### Keywords: graphs
> 
> ### ** Examples
> 
> data("cu.summary", package="rpart")
> fit <- rpart::rpart(Price ~ Mileage + Type + Country, cu.summary)
> autoplot(fit)
$plot

Attaching package: ‘ggplot2’

The following object is masked from ‘package:survMisc’:

    autoplot


$segments
        x         y ind  xend      yend
 1: 2.875 0.7494783  V1 2.875 1.0000000
 2: 2.875 1.0000000  V1 6.250 1.0000000
 3: 6.250 1.0000000  V1 6.250 0.7494783
 4: 1.750 0.6011194  V2 1.750 0.7494783
 5: 1.750 0.7494783  V2 4.000 0.7494783
 6: 4.000 0.7494783  V2 4.000 0.6011194
 7: 1.000 0.5383016  V3 1.000 0.6011194
 8: 1.000 0.6011194  V3 2.500 0.6011194
 9: 2.500 0.6011194  V3 2.500 0.5383016
10: 2.000 0.5277827  V4 2.000 0.5383016
11: 2.000 0.5383016  V4 3.000 0.5383016
12: 3.000 0.5383016  V4 3.000 0.5277827
13: 5.500 0.6618244  V5 5.500 0.7494783
14: 5.500 0.7494783  V5 7.000 0.7494783
15: 7.000 0.7494783  V5 7.000 0.6618244
16: 5.000 0.6515165  V6 5.000 0.6618244
17: 5.000 0.6618244  V6 6.000 0.6618244
18: 6.000 0.6618244  V6 6.000 0.6515165

$nodes
         x         y            node   n   isP lC rC   isL     resp       yNU
 1: 4.5625 1.0000000            root 117  TRUE  2  9 FALSE 15743.46 1.0094443
 2: 2.8750 0.7494783       Type=adef  80  TRUE  3  8 FALSE 13035.01 0.7589226
 3: 1.7500 0.6011194 Country=acefghj  69  TRUE  4  5 FALSE 11555.16 0.6105637
 4: 1.0000 0.5383016          Type=d  21 FALSE NA NA  TRUE 7629.048 0.5477460
 5: 2.5000 0.5383016        Type=aef  48  TRUE  6  7 FALSE 13272.83 0.5477460
 6: 2.0000 0.5277827     Country=fhj  29 FALSE NA NA  TRUE 12241.55 0.5372271
 7: 3.0000 0.5277827      Country=ce  19 FALSE NA NA  TRUE 14846.89 0.5372271
 8: 4.0000 0.6011194      Country=di  11 FALSE NA NA  TRUE 22317.73 0.6105637
 9: 6.2500 0.7494783         Type=bc  37  TRUE 10 13 FALSE 21599.57 0.7589226
10: 5.5000 0.6618244     Country=cgj  25  TRUE 11 12 FALSE 18697.28 0.6712687
11: 5.0000 0.6515165          Type=c  18 FALSE NA NA  TRUE 17607.44 0.6609609
12: 6.0000 0.6515165          Type=b   7 FALSE NA NA  TRUE 21499.71 0.6609609
13: 7.0000 0.6618244    Country=bdei  12 FALSE NA NA  TRUE    27646 0.6712687
          yND       yLD
 1: 0.9905557 0.9905557
 2: 0.7400339 0.7400339
 3: 0.5916750 0.5916750
 4: 0.5288573 0.5288573
 5: 0.5288573 0.5288573
 6: 0.5183384 0.5183384
 7: 0.5183384 0.5183384
 8: 0.5916750 0.5916750
 9: 0.7400339 0.7400339
10: 0.6523800 0.6523800
11: 0.6420722 0.6420722
12: 0.6420722 0.6420722
13: 0.6523800 0.6523800

> data("stagec", package="rpart")
> progstat <- factor(stagec$pgstat, levels = 0:1, labels = c("No", "Prog"))
> cfit <- rpart::rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,
+                      data = stagec, method = 'class')
> autoplot(cfit)
Error: Objects of type rpart not supported by autoplot.
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING
```

## SurvRank (0.1)
Maintainer: Michael Laimighofer <michael.laimighofer@helmholtz-muenchen.de>

__OK__

## sValues (0.1.1)
Maintainer: Carlos Cinelli <carloscinelli@hotmail.com>  
Bug reports: https://github.com/carloscinelli/sValues

__OK__

## SWMPr (2.1.4)
Maintainer: Marcus W. Beck <mbafs2012@gmail.com>  
Bug reports: https://github.com/fawda123/SWMPr/issues

```
checking examples ... ERROR
Running examples in ‘SWMPr-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: map_reserve
> ### Title: Map a reserve
> ### Aliases: map_reserve
> 
> ### ** Examples
> 
> ## defaults
> 
> map_reserve('jac')
Error in layer(mapping = NULL, data = NULL, stat = "identity", geom = <environment>,  : 
  unused argument (geom_params = list(raster = c("#CADDA8", "#CADFAA", "#C7DDA6", "#C7DDA6", "#CADDA8", "#CADDA8", "#CADDA8", "#CADDA8", "#C7DDA6", "#CADFAA", "#CADDA8", "#CADDA8", "#CADDA8", "#C7DDA6", "#CADFAA", "#CADFAA", "#CEE2AF", "#CCDFAD", "#CADFAA", "#CEE2AF", "#CADFAA", "#CCE2AD", "#CADFAA", "#CADFAA", "#CADFAA", "#CADFAA", "#CADFAA", "#CCE2AD", "#C7DDA6", "#CADDA8", "#CCDFAD", "#CADDA8", "#CADFAA", "#CEE2AF", "#CCDFAD", "#CADFAA", "#CADFAA", "#CCDFAD", "#CCE2AD", "#CEE2AF", "#CADFAA", "#CADFAA", 
"#CADFAA", "#CADFAA", "#CADFAA", "#CADFAA", "#CCDFAD", "#CADFAA", "#CADFAA", "#CADFAA", "#C7DDA6", "#CADDA8", "#CEE2AF", "#CCDFAD", "#CADFAA", "#CADFAA", "#CADFAA", "#CADFAA", "#CADFAA", "#CADFAA", "#CADFAA", "#CCDFAD", "#CADFAA", "#CEE2AF", "#CCE2AD", "#CEE2AF", "#CCDFAD", "#CADFAA", "#CADFAA", "#CADFAA", "#CCDFAD", "#CCDFAD", "#CADFAA", "#CADFAA", "#CADFAA", "#CADFAA", "#CADFAA", "#C7DDA6", "#C5DAA6", "#C5DAA6", "#CADDA8", "#CADDA8", "#CADFAA", "#CADFAA", "#CCDFAD", "#CADFA
Calls: map_reserve ... inset_raster -> <Anonymous> -> <Anonymous> -> do.call -> layer
Execution halted
```
```
DONE
Status: 1 ERROR
```

## synthpop (1.1-1)
Maintainer: Beata Nowok <beata.nowok@gmail.com>

```
checking dependencies in R code ... NOTE
Unexported object imported by a ':::' call: ‘coefplot:::position_dodgev’
  See the note in ?`:::` about the use of this operator.
```
```
checking examples ... ERROR
Running examples in ‘synthpop-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: compare.fit.synds
> ### Title: Compare model estimates based on synthesised and observed data
> ### Aliases: compare.fit.synds print.compare.fit.synds
> 
> ### ** Examples
> 
> ods <- SD2011[,c("sex","age","edu","smoke")]
> s1 <- syn(ods, m = 5)
No seed has been provided and it has been set to 266.

syn  variables
1    sex age edu smoke
2    sex age edu smoke
3    sex age edu smoke
4    sex age edu smoke
5    sex age edu smoke
> f1 <- glm.synds(smoke ~ sex + age + edu, data = s1, family = "binomial")
> compare(f1, ods) 
Error in get(x, envir = this, inherits = inh)(this, ...) : 
  attempt to apply non-function
Calls: compare ... geom_errorbarh -> layer -> <Anonymous> -> <Anonymous> -> <Anonymous>
Execution halted
```
```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘synthpop.Rnw’
  ...
eduVOCATIONAL/GRAMMAR        0.14027     0.267637
eduSECONDARY                 0.06621     0.275793
eduPOST-SECONDARY OR HIGHER -0.17350     0.312116
log(income)                 -0.00493     0.130486

> compare(model.sds, ods)

  When sourcing 'synthpop.R':
Error: attempt to apply non-function
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: lattice
Loading required package: MASS
Loading required package: nnet
Loading required package: ggplot2
Loading required package: coefplot
Warning: replacing previous import by 'scales::alpha' when loading 'coefplot'
Warning: replacing previous import by 'grid::arrow' when loading 'useful'
Warning: replacing previous import by 'grid::unit' when loading 'useful'
Warning: replacing previous import by 'scales::alpha' when loading 'useful'

Error: processing vignette 'synthpop.Rnw' failed with diagnostics:
 chunk 27 
Error in get(x, envir = this, inherits = inh)(this, ...) : 
  attempt to apply non-function
Execution halted

```
```
DONE
Status: 2 ERRORs, 2 NOTEs
```

## tableone (0.7.1)
Maintainer: Kazuki Yoshida <kazukiyoshida@mail.harvard.edu>

__OK__

## tabplot (1.1)
Maintainer: Martijn Tennekes <mtennekes@gmail.com>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘classInt’ ‘shiny’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
binRanges: no visible binding for global variable ‘ri’
bin_data: no visible global function definition for ‘vw<-’
bin_data: no visible global function definition for ‘chunk’
bin_data: no visible global function definition for ‘ff’
bin_data: no visible global function definition for ‘physical’
bin_data : <anonymous>: no visible global function definition for
  ‘vmode’
bin_data : <anonymous>: no visible global function definition for
  ‘is.factor.ff’
bin_hcc_data : <anonymous>: no visible global function definition for
  ‘ff’
num2fac: no visible global function definition for ‘classIntervals’
plot.tabplot: no visible global function definition for ‘p’
subset_data: no visible global function definition for ‘clone’
subset_data: no visible global function definition for ‘nrow<-’
subset_data: no visible global function definition for ‘fforder’
tablePrepare: no visible global function definition for ‘as.ffdf’
tablePrepare: no visible global function definition for ‘physical’
tablePrepare: no visible binding for global variable ‘is.factor.ff’
tablePrepare: no visible global function definition for ‘ff’
tablePrepare: no visible global function definition for ‘chunk’
tablePrepare: no visible global function definition for ‘fforder’
tablePrepare: no visible binding for global variable ‘fforder’
tablePrepare: no visible binding for global variable ‘ffdf’
tableplot: no visible global function definition for ‘vmode’
tableplot: no visible global function definition for ‘physical’
tableplot : <anonymous>: no visible global function definition for
  ‘is.factor.ff’
tableplot : <anonymous>: no visible global function definition for
  ‘vmode’
```
```
checking files in ‘vignettes’ ... NOTE
The following directory looks like a leftover from 'knitr':
  ‘figure’
Please remove from your package.
```
```
DONE
Status: 3 NOTEs
```

## taRifx (1.0.6)
Maintainer: Ari B. Friedman <abfriedman@gmail.com>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘gdata’ ‘ggplot2’ ‘grid’ ‘lattice’ ‘xtable’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  as.matrix.by stack.list
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
autoplot.microbenchmark: no visible global function definition for
  ‘ggplot’
autoplot.microbenchmark: no visible global function definition for
  ‘aes’
autoplot.microbenchmark: no visible global function definition for
  ‘coord_cartesian’
autoplot.microbenchmark: no visible global function definition for
  ‘stat_summary’
autoplot.microbenchmark: no visible global function definition for
  ‘opts’
autoplot.microbenchmark: no visible global function definition for
  ‘theme_text’
compareplot: no visible global function definition for ‘grid.newpage’
compareplot: no visible global function definition for
  ‘latticeParseFormula’
compareplot: no visible global function definition for ‘pushViewport’
compareplot: no visible global function definition for ‘viewport’
compareplot: no visible global function definition for ‘grid.layout’
compareplot: no visible global function definition for ‘unit’
compareplot: no visible global function definition for ‘seekViewport’
compareplot: no visible global function definition for ‘grid.rect’
compareplot : makeNat: no visible global function definition for
  ‘convertUnit’
compareplot: no visible global function definition for ‘grid.text’
compareplot: no visible global function definition for ‘gpar’
compareplot: no visible global function definition for ‘grid.lines’
compareplot: no visible global function definition for ‘grid.points’
compareplot: no visible global function definition for ‘grid.polyline’
compareplot: no visible global function definition for ‘upViewport’
compareplot: no visible global function definition for ‘convertUnit’
compareplot: no visible global function definition for ‘popViewport’
hist_horiz: no visible global function definition for
  ‘latticeParseFormula’
latex.table.by: no visible global function definition for ‘xtable’
panel.densityplot.enhanced: no visible global function definition for
  ‘panel.densityplot’
panel.densityplot.enhanced: no visible global function definition for
  ‘unit’
panel.densityplot.enhanced: no visible global function definition for
  ‘grid.text’
panel.densityplot.enhanced: no visible global function definition for
  ‘gpar’
panel.ecdf: no visible global function definition for ‘panel.xyplot’
panel.ecdf: no visible global function definition for ‘panel.lines’
panel.xyplot_rug: no visible global function definition for
  ‘panel.xyplot’
panel.xyplot_rug: no visible global function definition for
  ‘grid.segments’
panel.xyplot_rug: no visible global function definition for ‘unit’
panel.xyplot_rug: no visible global function definition for ‘gpar’
searchPattern: no visible global function definition for ‘interleave’
xtable.CrossTable: no visible global function definition for
  ‘caption<-’
xtable.CrossTable: no visible global function definition for ‘label<-’
xtable.CrossTable: no visible global function definition for ‘align<-’
xtable.CrossTable: no visible global function definition for ‘digits<-’
xtable.CrossTable: no visible global function definition for
  ‘display<-’
xtable.summary.lme: no visible global function definition for
  ‘caption<-’
xtable.summary.lme: no visible global function definition for ‘label<-’
xtable.summary.lme: no visible global function definition for ‘align<-’
xtable.summary.lme: no visible global function definition for
  ‘digits<-’
xtable.summary.lme: no visible global function definition for
  ‘display<-’
xtablelm: no visible global function definition for ‘xtable’
```
```
DONE
Status: 4 NOTEs
```

## TcGSA (0.9.8)
Maintainer: Boris P. Hejblum <boris.hejblum@isped.u-bordeaux2.fr>

```
checking package dependencies ... ERROR
Package required but not available: ‘multtest’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```
```
DONE
Status: 1 ERROR
```

## tcR (2.1.1)
Maintainer: Vadim Nazarov <vdm.nazarov@gmail.com>  
Bug reports: https://github.com/imminfo/tcr/issues

```
checking whether package ‘tcR’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘data.table::melt’ when loading ‘tcR’
  Warning: replacing previous import by ‘data.table::dcast’ when loading ‘tcR’
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘tcR’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘tcR’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/tcR.Rcheck/00install.out’ for details.
```
```
checking installed package size ... NOTE
  installed size is  5.5Mb
  sub-directories of 1Mb or more:
    data   1.2Mb
    doc    3.9Mb
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...

Warning: replacing previous import by 'data.table::melt' when loading 'tcR'
Warning: replacing previous import by 'data.table::dcast' when loading 'tcR'
Warning: replacing previous import by 'grid::arrow' when loading 'tcR'
Warning: replacing previous import by 'grid::unit' when loading 'tcR'

Attaching package: 'tcR'

The following object is masked from 'package:igraph':

    diversity

Using People as id variables
Using Gene as id variables
Using Gene as id variables
Using Gene as id variables
Warning: Removed 4 rows containing missing values (geom_text).
Warning: Removed 20 rows containing missing values (geom_point).
Warning: Removed 20 rows containing missing values (geom_point).
Warning: Removed 20 rows containing missing values (geom_point).
Warning: Removed 20 rows containing missing values (geom_point).
Quitting from lines 488-490 (tcrvignette.Rmd) 
Error: processing vignette 'tcrvignette.Rmd' failed with diagnostics:
Unknown parameters: binwidth, bins, origin, right
Execution halted

```
```
DONE
Status: 1 WARNING, 2 NOTEs
```

## tdr (0.11)
Maintainer: Oscar Perpinan Lamigueiro <oscar.perpinan@gmail.com>  
Bug reports: https://github.com/oscarperpinan/tdr/issues

__OK__

## TeachingDemos (2.9)
Maintainer: Greg Snow <greg.snow@imail.org>

```
checking package dependencies ... NOTE
Packages suggested but not available for checking: ‘R2wd’ ‘EBImage’
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘R2wd’ ‘lattice’ ‘logspline’ ‘rgl’ ‘tcltk’ ‘tcltk2’ ‘tkrplot’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
checking R code for possible problems ... NOTE
Found an obsolete/platform-specific call in the following function:
  ‘triplot’
Found the platform-specific device:
  ‘win.graph’
dev.new() is the preferred way to open a new device, in the unlikely
event one is needed.
HTKidentify: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/HWidentify.R:71)
HTKidentify: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/HWidentify.R:72)
HTKidentify: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/HWidentify.R:73)
HTKidentify: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/HWidentify.R:74)
HTKidentify: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/HWidentify.R:74)
HTKidentify: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/HWidentify.R:75)
HTKidentify: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/HWidentify.R:75)
HTKidentify : mm: no visible global function definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/HWidentify.R:87)
HTKidentify: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/HWidentify.R:90)
TkApprox: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:8)
TkApprox: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:9)
TkApprox: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:26)
TkApprox: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:27)
TkApprox : replot: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:43-56)
TkApprox: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:66)
TkApprox: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:67)
TkApprox: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:69)
TkApprox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:70)
TkApprox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:72)
TkApprox: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:72)
TkApprox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:74-76)
TkApprox: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:74-76)
TkApprox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:77-78)
TkApprox: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:77-78)
TkApprox : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:77-78)
TkApprox: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:81)
TkApprox: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:81)
TkApprox: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:82)
TkApprox: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:82)
TkApprox : mouse.move: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:90-92)
TkApprox : mouse.move: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:97)
TkApprox: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:113)
TkApprox: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:114)
TkApprox: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:115)
TkApprox: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkApprox.R:118)
TkBuildDist : <anonymous>: no visible global function definition for
  ‘oldlogspline’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:23)
TkBuildDist : <anonymous>: no visible global function definition for
  ‘doldlogspline’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:24)
TkBuildDist : <anonymous>: no visible global function definition for
  ‘dlogspline’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:35)
TkBuildDist: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:54)
TkBuildDist: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:55)
TkBuildDist: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:57)
TkBuildDist: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:58)
TkBuildDist: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:60-61)
TkBuildDist: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:60-61)
TkBuildDist : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:60-61)
TkBuildDist: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:63)
TkBuildDist: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:63)
TkBuildDist : mouse1.down: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:69)
TkBuildDist : mouse2.down: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:78)
TkBuildDist: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:82)
TkBuildDist: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:83)
TkBuildDist: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:84)
TkBuildDist: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:86)
TkBuildDist: no visible global function definition for ‘oldlogspline’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:92)
TkBuildDist2 : <anonymous>: no visible global function definition for
  ‘oldlogspline’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:128)
TkBuildDist2 : <anonymous>: no visible global function definition for
  ‘doldlogspline’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:129)
TkBuildDist2: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:154)
TkBuildDist2: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:155)
TkBuildDist2: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:157)
TkBuildDist2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:158)
TkBuildDist2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:160-161)
TkBuildDist2: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:160-161)
TkBuildDist2 : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:160-161)
TkBuildDist2: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:163)
TkBuildDist2: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:163)
TkBuildDist2: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:164)
TkBuildDist2: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:164)
TkBuildDist2 : mouse.move: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:181)
TkBuildDist2: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:195)
TkBuildDist2: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:196)
TkBuildDist2: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:197)
TkBuildDist2: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:199)
TkBuildDist2: no visible global function definition for ‘oldlogspline’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkBuildDist.R:204)
TkIdentify: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:119)
TkIdentify: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:120)
TkIdentify: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:122)
TkIdentify: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:123)
TkIdentify: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:125-126)
TkIdentify: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:125-126)
TkIdentify : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:125-126)
TkIdentify: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:133)
TkIdentify: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:133)
TkIdentify: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:134)
TkIdentify: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:134)
TkIdentify : mouse.move: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:166)
TkIdentify: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:183)
TkIdentify: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:184)
TkIdentify: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:185)
TkIdentify: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/dynIdentify.R:187)
TkListView: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:9)
TkListView: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:10)
TkListView: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:12)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:13)
TkListView: no visible global function definition for ‘tkscrollbar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:15)
TkListView : <anonymous>: no visible global function definition for
  ‘tkyview’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:15)
TkListView: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:17)
TkListView : <anonymous>: no visible global function definition for
  ‘tkset’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:17)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:19)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:20)
TkListView: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:23)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:24)
TkListView: no visible global function definition for ‘tktext’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:26)
TkListView: no visible global function definition for ‘tkscrollbar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:27)
TkListView : <anonymous>: no visible global function definition for
  ‘tkyview’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:27)
TkListView: no visible global function definition for ‘tkscrollbar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:28)
TkListView : <anonymous>: no visible global function definition for
  ‘tkxview’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:28)
TkListView: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:29-30)
TkListView : <anonymous>: no visible global function definition for
  ‘tkset’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:29-30)
TkListView: no visible global function definition for ‘tkgrid’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:32)
TkListView: no visible global function definition for ‘tkgrid’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:33)
TkListView: no visible global function definition for
  ‘tkgrid.columnconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:34)
TkListView: no visible global function definition for
  ‘tkgrid.rowconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:35)
TkListView : buildtree: no visible global function definition for
  ‘tkinsert’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:50)
TkListView : buildtree: no visible global function definition for
  ‘tkinsert’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:54)
TkListView : buildtree: no visible global function definition for
  ‘tkinsert’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:61)
TkListView : getx: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:70)
TkListView : getx: no visible global function definition for ‘tkselect’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:70)
TkListView: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:94-99)
TkListView : <anonymous>: no visible global function definition for
  ‘tkdelete’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:96)
TkListView : <anonymous>: no visible global function definition for
  ‘tkinsert’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:97)
TkListView: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:100-105)
TkListView : <anonymous>: no visible global function definition for
  ‘tkdelete’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:102)
TkListView : <anonymous>: no visible global function definition for
  ‘tkinsert’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:103)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:107)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:108)
TkListView: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:110)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:111)
TkListView: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:113)
TkListView: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:114)
TkListView: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:115-120)
TkListView : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:116)
TkListView : <anonymous>: no visible global function definition for
  ‘tkdelete’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:117)
TkListView : <anonymous>: no visible global function definition for
  ‘tkinsert’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:118)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:122)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:123)
TkListView: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:125-126)
TkListView: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:125-126)
TkListView : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkListView.R:125-126)
TkSpline: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:10)
TkSpline: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:11)
TkSpline: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:12)
TkSpline: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:13)
TkSpline: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:14)
TkSpline: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:15)
TkSpline: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:16)
TkSpline: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:17)
TkSpline: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:31)
TkSpline: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:32)
TkSpline : replot: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:46-48)
TkSpline : replot: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:49-52)
TkSpline : replot: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:53-56)
TkSpline : replot: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:58-59)
TkSpline: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:71)
TkSpline: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:72)
TkSpline: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:74)
TkSpline: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:75)
TkSpline: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:77)
TkSpline: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:77)
TkSpline: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:78-80)
TkSpline: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:78-80)
TkSpline : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:78-80)
TkSpline: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:81-83)
TkSpline: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:81-83)
TkSpline : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:81-83)
TkSpline: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:84-86)
TkSpline: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:84-86)
TkSpline : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:84-86)
TkSpline: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:90)
TkSpline: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:90)
TkSpline: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:92-94)
TkSpline: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:92-94)
TkSpline: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:96-97)
TkSpline: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:96-97)
TkSpline : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:96-97)
TkSpline: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:100)
TkSpline: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:100)
TkSpline: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:101)
TkSpline: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:101)
TkSpline : mouse.move: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:110-112)
TkSpline : mouse.move: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:119)
TkSpline: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:132)
TkSpline: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:133)
TkSpline: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:134)
TkSpline: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/TkSpline.R:137)
flip.rgl.coin: no visible global function definition for
  ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/flip.rgl.coin.R:4)
have.ttk: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tdspinner.R:13)
lattice.demo: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:9)
lattice.demo: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:10)
lattice.demo: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:12)
lattice.demo : lattice.refresh: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:15)
lattice.demo : lattice.refresh: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:16)
lattice.demo : lattice.refresh: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:18)
lattice.demo : lattice.refresh: no visible global function definition
  for ‘xyplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:27-31)
lattice.demo : lattice.refresh: no visible global function definition
  for ‘strip.custom’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:27-31)
lattice.demo : lattice.refresh: no visible global function definition
  for ‘cloud’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:33-40)
lattice.demo : lattice.refresh : <anonymous>: no visible global
  function definition for ‘panel.cloud’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:34-40)
lattice.demo : lattice.refresh : <anonymous> : <anonymous>: no visible
  global function definition for ‘panel.3dscatter’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:35-37)
lattice.demo : lattice.refresh : <anonymous> : <anonymous>: no visible
  global function definition for ‘panel.3dwire’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:38-39)
lattice.demo : lattice.refresh: no visible global function definition
  for ‘xyplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:43-48)
lattice.demo : lattice.refresh: no visible global function definition
  for ‘strip.custom’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:43-48)
lattice.demo: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:54)
lattice.demo: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:55)
lattice.demo: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:56)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:59)
lattice.demo: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:59)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:60)
lattice.demo: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:60)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:61-64)
lattice.demo: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:61-64)
lattice.demo: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:67)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:70)
lattice.demo: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:70)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:71)
lattice.demo: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:71)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:72-75)
lattice.demo: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:72-75)
lattice.demo: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:77)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:80)
lattice.demo: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:80)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:81-82)
lattice.demo: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:81-82)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:83-84)
lattice.demo: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:83-84)
lattice.demo: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:86)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:88)
lattice.demo: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:88)
lattice.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:90-91)
lattice.demo: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:90-91)
lattice.demo : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/lattice.demo.R:90-91)
mle.demo: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:10)
mle.demo: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:11)
mle.demo : mle.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:16)
mle.demo : mle.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:17)
mle.demo: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:51)
mle.demo: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:52)
mle.demo: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:53)
mle.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:56)
mle.demo: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:56)
mle.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:57)
mle.demo: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:57)
mle.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:62-66)
mle.demo: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:62-66)
mle.demo: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:68)
mle.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:71)
mle.demo: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:71)
mle.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:72)
mle.demo: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:72)
mle.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:76-80)
mle.demo: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:76-80)
mle.demo: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:82)
mle.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:86)
mle.demo: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:86)
mle.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:87-88)
mle.demo: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:87-88)
mle.demo : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/mle.demo.R:87-88)
panel.dice: no visible global function definition for ‘llines’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/panel.dice.R:14)
panel.dice: no visible global function definition for ‘lpoints’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/panel.dice.R:26)
panel.my.symbols : <anonymous>: no visible binding for global variable
  ‘lpolygon’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/panel.mysymbols.R:37)
panel.my.symbols : <anonymous>: no visible binding for global variable
  ‘llines’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/panel.mysymbols.R:39)
panel.my.symbols : <anonymous>: no visible binding for global variable
  ‘lpolygon’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/panel.mysymbols.R:53)
panel.my.symbols : <anonymous>: no visible binding for global variable
  ‘llines’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/panel.mysymbols.R:55)
plot.dice: no visible global function definition for ‘trellis.par.get’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.dice.R:5)
plot.dice: no visible global function definition for ‘trellis.par.set’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.dice.R:6)
plot.dice: no visible global function definition for ‘trellis.par.set’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.dice.R:7)
plot.dice: no visible global function definition for ‘col.whitebg’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.dice.R:7)
plot.dice: no visible global function definition for ‘xyplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.dice.R:16-19)
plotFagan: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:110)
plotFagan: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:111)
plotFagan: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:112)
plotFagan: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:113)
plotFagan: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:114)
plotFagan: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:115)
plotFagan: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:117)
plotFagan: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:118)
plotFagan: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:119)
plotFagan: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:120)
plotFagan : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:123)
plotFagan : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:124)
plotFagan : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:125)
plotFagan: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:129)
plotFagan: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:130)
plotFagan: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:132)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:133)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:135)
plotFagan: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:135)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:136)
plotFagan: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:136)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:137-141)
plotFagan: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:137-141)
plotFagan : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:137-141)
plotFagan : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:137-141)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:143)
plotFagan: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:143)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:144)
plotFagan: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:144)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:145-149)
plotFagan: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:145-149)
plotFagan : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:145-149)
plotFagan : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:145-149)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:151)
plotFagan: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:151)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:152-158)
plotFagan: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:152-158)
plotFagan : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:152-158)
plotFagan : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:152-158)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:160)
plotFagan: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:160)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:161-164)
plotFagan: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:161-164)
plotFagan : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:161-164)
plotFagan : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:161-164)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:166-167)
plotFagan: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:166-167)
plotFagan : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:166-167)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:170)
plotFagan: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:170)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:171)
plotFagan: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:171)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:172)
plotFagan: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:172)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:173)
plotFagan: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:173)
plotFagan: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:174)
plotFagan: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:174)
plotFagan: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:177)
plotFagan: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:178-181)
plotFagan2: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:195)
plotFagan2: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:196)
plotFagan2: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:197)
plotFagan2: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:198)
plotFagan2: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:199)
plotFagan2: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:200)
plotFagan2: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:201)
plotFagan2: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:202)
plotFagan2: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:204)
plotFagan2: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:205)
plotFagan2: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:206)
plotFagan2: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:207)
plotFagan2 : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:210)
plotFagan2 : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:211)
plotFagan2 : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:212)
plotFagan2 : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:213)
plotFagan2: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:217)
plotFagan2: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:218)
plotFagan2: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:220)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:221)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:223)
plotFagan2: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:223)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:224)
plotFagan2: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:224)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:225-229)
plotFagan2: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:225-229)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:225-229)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:225-229)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:231)
plotFagan2: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:231)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:232)
plotFagan2: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:232)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:233-237)
plotFagan2: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:233-237)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:233-237)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:233-237)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:239)
plotFagan2: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:239)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:240)
plotFagan2: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:240)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:241-245)
plotFagan2: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:241-245)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:241-245)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:241-245)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:247)
plotFagan2: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:247)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:248-254)
plotFagan2: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:248-254)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:248-254)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:248-254)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:256)
plotFagan2: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:256)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:257-260)
plotFagan2: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:257-260)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:257-260)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:257-260)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:262-263)
plotFagan2: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:262-263)
plotFagan2 : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:262-263)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:266)
plotFagan2: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:266)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:267)
plotFagan2: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:267)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:268)
plotFagan2: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:268)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:269)
plotFagan2: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:269)
plotFagan2: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:270)
plotFagan2: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:270)
plotFagan2: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:273)
plotFagan2: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/fagan.R:274-278)
rgl.Map : <anonymous>: no visible global function definition for
  ‘rgl.lines’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rgl.Map.R:22)
rgl.coin: no visible binding for global variable ‘coin.faces’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.coin.R:5)
rgl.coin: no visible global function definition for ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.coin.R:7)
rgl.coin: no visible global function definition for ‘rgl.triangles’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.coin.R:10-12)
rgl.coin: no visible global function definition for ‘rgl.triangles’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.coin.R:13-15)
rgl.coin: no visible global function definition for ‘rgl.quads’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.coin.R:16-21)
rgl.coin: no visible global function definition for ‘rgl.lines’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.coin.R:27-28)
rgl.coin: no visible global function definition for ‘rgl.lines’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.coin.R:33-34)
rgl.die: no visible global function definition for ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:5)
rgl.die: no visible global function definition for ‘rgl.quads’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:24)
rgl.die: no visible global function definition for ‘rgl.quads’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:25)
rgl.die: no visible global function definition for ‘rgl.quads’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:26)
rgl.die: no visible global function definition for ‘rgl.quads’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:27)
rgl.die: no visible global function definition for ‘rgl.quads’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:28)
rgl.die: no visible global function definition for ‘rgl.quads’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:29)
rgl.die: no visible global function definition for ‘rgl.triangles’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:34)
rgl.die: no visible global function definition for ‘rgl.triangles’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:40)
rgl.die: no visible global function definition for ‘rgl.triangles’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:46)
rgl.die: no visible global function definition for ‘rgl.triangles’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:52)
rgl.die: no visible global function definition for ‘rgl.triangles’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:58)
rgl.die: no visible global function definition for ‘rgl.triangles’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/plot.rgl.die.R:64)
roc.demo: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:10)
roc.demo : roc.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:19)
roc.demo: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:72)
roc.demo: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:73)
roc.demo: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:74)
roc.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:77)
roc.demo: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:77)
roc.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:78)
roc.demo: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:78)
roc.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:80-84)
roc.demo: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:80-84)
roc.demo: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:87)
roc.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:91)
roc.demo: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:91)
roc.demo: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:92-93)
roc.demo: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:92-93)
roc.demo : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roc.demo.R:92-93)
roll.rgl.die: no visible global function definition for ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roll.rgl.die.R:3)
roll.rgl.die: no visible global function definition for ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roll.rgl.die.R:11)
roll.rgl.die: no visible global function definition for ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roll.rgl.die.R:17)
roll.rgl.die: no visible global function definition for ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roll.rgl.die.R:21)
roll.rgl.die: no visible global function definition for ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roll.rgl.die.R:24)
roll.rgl.die: no visible global function definition for ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roll.rgl.die.R:30)
roll.rgl.die: no visible global function definition for ‘rgl.viewpoint’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/roll.rgl.die.R:34)
rotate.cloud: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:7)
rotate.cloud: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:8)
rotate.cloud: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:9)
rotate.cloud: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:11)
rotate.cloud: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:12)
rotate.cloud: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:13)
rotate.cloud : cloud.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:19)
rotate.cloud : cloud.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:20)
rotate.cloud : cloud.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:21)
rotate.cloud : cloud.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:23)
rotate.cloud : cloud.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:24)
rotate.cloud : cloud.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:25)
rotate.cloud: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:39)
rotate.cloud: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:40)
rotate.cloud: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:41)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:44)
rotate.cloud: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:44)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:45)
rotate.cloud: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:45)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:46-49)
rotate.cloud: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:46-49)
rotate.cloud: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:52)
rotate.cloud: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:54)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:57)
rotate.cloud: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:57)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:58)
rotate.cloud: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:58)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:59-62)
rotate.cloud: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:59-62)
rotate.cloud: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:65)
rotate.cloud: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:67)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:70)
rotate.cloud: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:70)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:71)
rotate.cloud: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:71)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:72-75)
rotate.cloud: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:72-75)
rotate.cloud: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:78)
rotate.cloud: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:80)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:82)
rotate.cloud: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:82)
rotate.cloud: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:84-85)
rotate.cloud: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:84-85)
rotate.cloud : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.cloud.R:84-85)
rotate.wireframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:7)
rotate.wireframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:8)
rotate.wireframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:9)
rotate.wireframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:11)
rotate.wireframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:12)
rotate.wireframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:13)
rotate.wireframe : wire.refresh: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:19)
rotate.wireframe : wire.refresh: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:20)
rotate.wireframe : wire.refresh: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:21)
rotate.wireframe : wire.refresh: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:23)
rotate.wireframe : wire.refresh: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:24)
rotate.wireframe : wire.refresh: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:25)
rotate.wireframe: no visible global function definition for
  ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:39)
rotate.wireframe: no visible global function definition for
  ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:40)
rotate.wireframe: no visible global function definition for
  ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:41)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:44)
rotate.wireframe: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:44)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:45)
rotate.wireframe: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:45)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:46-49)
rotate.wireframe: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:46-49)
rotate.wireframe: no visible global function definition for
  ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:52)
rotate.wireframe: no visible global function definition for
  ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:54)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:57)
rotate.wireframe: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:57)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:58)
rotate.wireframe: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:58)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:59-62)
rotate.wireframe: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:59-62)
rotate.wireframe: no visible global function definition for
  ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:65)
rotate.wireframe: no visible global function definition for
  ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:67)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:70)
rotate.wireframe: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:70)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:71)
rotate.wireframe: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:71)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:72-75)
rotate.wireframe: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:72-75)
rotate.wireframe: no visible global function definition for
  ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:78)
rotate.wireframe: no visible global function definition for
  ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:80)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:82)
rotate.wireframe: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:82)
rotate.wireframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:84-85)
rotate.wireframe: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:84-85)
rotate.wireframe : <anonymous>: no visible global function definition
  for ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/rotate.wireframe.R:84-85)
run.cor.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:40)
run.cor.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:41)
run.cor.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:42)
run.cor.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:43)
run.cor.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:45)
run.cor.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:46)
run.cor.examp : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:49)
run.cor.examp: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:63)
run.cor.examp: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:64)
run.cor.examp: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:66)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:67)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:70)
run.cor.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:70)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:71)
run.cor.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:71)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:72-77)
run.cor.examp: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:72-77)
run.cor.examp : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:72-77)
run.cor.examp : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:72-77)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:80)
run.cor.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:80)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:81-84)
run.cor.examp: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:81-84)
run.cor.examp : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:81-84)
run.cor.examp : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:81-84)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:85-86)
run.cor.examp: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:85-86)
run.cor.examp : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:85-86)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:88)
run.cor.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:88)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:89)
run.cor.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:89)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:90)
run.cor.examp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:90)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:91)
run.cor.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:91)
run.cor.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:92)
run.cor.examp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:92)
run.cor.examp: no visible global function definition for
  ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:95)
run.cor.examp: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor.examp.R:96)
run.cor2.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:60)
run.cor2.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:61)
run.cor2.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:62)
run.cor2.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:63)
run.cor2.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:65)
run.cor2.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:66)
run.cor2.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:67)
run.cor2.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:68)
run.cor2.examp : update.r: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:71)
run.cor2.examp : update.r: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:72)
run.cor2.examp : update.r: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:73)
run.cor2.examp : update.r: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:74-76)
run.cor2.examp : update.r: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:74-76)
run.cor2.examp : update.r2: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:79)
run.cor2.examp : update.r2: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:80)
run.cor2.examp : update.r2: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:81-83)
run.cor2.examp : update.r2: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:81-83)
run.cor2.examp : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:87)
run.cor2.examp: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:102)
run.cor2.examp: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:103)
run.cor2.examp: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:105)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:106)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:108)
run.cor2.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:108)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:109)
run.cor2.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:109)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:110)
run.cor2.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:110)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:111-113)
run.cor2.examp: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:111-113)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:114)
run.cor2.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:114)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:115)
run.cor2.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:115)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:116-118)
run.cor2.examp: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:116-118)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:120)
run.cor2.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:120)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:121-124)
run.cor2.examp: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:121-124)
run.cor2.examp : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:121-124)
run.cor2.examp : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:121-124)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:125-126)
run.cor2.examp: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:125-126)
run.cor2.examp : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:125-126)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:128)
run.cor2.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:128)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:129)
run.cor2.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:129)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:130)
run.cor2.examp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:130)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:131)
run.cor2.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:131)
run.cor2.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:132)
run.cor2.examp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:132)
run.cor2.examp: no visible global function definition for
  ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:135)
run.cor2.examp: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.cor2.examp.R:136)
run.power.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:17)
run.power.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:18)
run.power.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:19)
run.power.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:20)
run.power.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:21)
run.power.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:22)
run.power.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:24)
run.power.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:25)
run.power.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:26)
run.power.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:27)
run.power.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:28)
run.power.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:29)
run.power.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:31)
run.power.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:32)
run.power.examp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:33)
run.power.examp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:34)
run.power.examp : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:39-44)
run.power.examp: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:47)
run.power.examp: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:48)
run.power.examp: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:50)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:51)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:53)
run.power.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:53)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:54)
run.power.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:54)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:55-60)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:55-60)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:55-60)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:62)
run.power.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:62)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:63)
run.power.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:63)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:64-68)
run.power.examp: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:64-68)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:64-68)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:64-68)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:70)
run.power.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:70)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:71)
run.power.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:71)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:72-76)
run.power.examp: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:72-76)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:72-76)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:72-76)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:78)
run.power.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:78)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:79)
run.power.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:79)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:80-84)
run.power.examp: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:80-84)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:80-84)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:80-84)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:88)
run.power.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:88)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:89)
run.power.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:89)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:90)
run.power.examp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:90)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:91)
run.power.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:91)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:92)
run.power.examp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:92)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:96)
run.power.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:96)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:97)
run.power.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:97)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:98)
run.power.examp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:98)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:99)
run.power.examp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:99)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:100)
run.power.examp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:100)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:102)
run.power.examp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:102)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:103-106)
run.power.examp: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:103-106)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:103-106)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:103-106)
run.power.examp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:108-109)
run.power.examp: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:108-109)
run.power.examp : <anonymous>: no visible global function definition
  for ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:108-109)
run.power.examp: no visible global function definition for
  ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:112)
run.power.examp: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/run.power.examp.R:113-118)
slider: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:5-7)
slider: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:24)
slider: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:25)
slider: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:26)
slider: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:36)
slider: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:36)
slider: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:37)
slider: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:38-39)
slider: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:40)
slider: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:50)
slider: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:53)
slider: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:53)
slider: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:54-56)
slider: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:54-56)
slider : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:54-56)
slider: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:61-66)
slider: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:61-66)
slider: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:77-78)
slider: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/slider.R:77-78)
sliderv: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:5-6)
sliderv: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:16)
sliderv: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:16)
sliderv: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:16)
sliderv: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:20)
sliderv: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:20)
sliderv: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:21)
sliderv: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:22-23)
sliderv: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:24)
sliderv: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:29)
sliderv: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:29)
sliderv: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:30-31)
sliderv: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:30-31)
sliderv : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:30-31)
sliderv: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:32-36)
sliderv: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/sliderv.R:32-36)
tdspinner: no visible global function definition for ‘tkwidget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tdspinner.R:6)
tkBrush: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:46)
tkBrush: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:47)
tkBrush: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:81)
tkBrush: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:82)
tkBrush: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:84)
tkBrush: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:86)
tkBrush: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:89)
tkBrush: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:89)
tkBrush: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:90)
tkBrush: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:90)
tkBrush: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:93)
tkBrush: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:93)
tkBrush: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:94)
tkBrush: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:94)
tkBrush: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:97-98)
tkBrush: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:97-98)
tkBrush : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:97-98)
tkBrush: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:100)
tkBrush: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:100)
tkBrush: no visible global function definition for ‘tcl’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:101)
tkBrush: no visible global function definition for ‘tkcget’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:101)
tkBrush : mm: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:118)
tkBrush : mm: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:124)
tkBrush : mm: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:125)
tkBrush : mm: no visible global function definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:133)
tkBrush : mmm: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:140)
tkBrush : mmm: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:146)
tkBrush : mmm: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:147)
tkBrush : mmm: no visible global function definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:155)
tkBrush: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:158)
tkBrush: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:159)
tkBrush: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:160)
tkBrush: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:161)
tkBrush: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:162)
tkBrush: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:163)
tkBrush: no visible global function definition for ‘tkbind’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:164)
tkBrush: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkBrush.R:167)
tkexamp: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:33)
tkexamp: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:34)
tkexamp: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:36)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:37)
tkexamp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:39)
tkexamp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:40)
tkexamp: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:41)
tkexamp: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:42)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:50)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:51)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:53)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:53)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:59)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:59)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:60-61)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:60-61)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:62)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:63)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:68)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:78)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:78)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:79-80)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:79-80)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:81)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:82)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:87)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:97)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:97)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:98)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:98)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:99)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:100-104)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:105-108)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:105-108)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:112)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:122)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:122)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:123)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:123)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:124)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:125-129)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:130-133)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:130-133)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:137)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:147)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:147)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:148-149)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:148-149)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:150)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:151-155)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:156)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:157-160)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:157-160)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:164)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:171)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:175)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:176-178)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:179-183)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:179-183)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:191)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:202)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:202)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:203-204)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:203-204)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:205)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:206-208)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:217)
tkexamp : fillframe: no visible global function definition for
  ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:218)
tkexamp : fillframe: no visible global function definition for
  ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:219)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:229-230)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:229-230)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:231-232)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:231-232)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:233)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:234-238)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:243-246)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:243-246)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:249-250)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:262)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:262)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:263)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:263)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:264)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:265-269)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:270-273)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:270-273)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:281)
tkexamp : fillframe : tmp.tke.an: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:315)
tkexamp : fillframe : tmp.tke.an: no visible global function definition
  for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:316-317)
tkexamp : fillframe : tmp.tke.an: no visible global function definition
  for ‘tclAfter’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:326)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:332-333)
tkexamp : fillframe: no visible global function definition for
  ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:332-333)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:341)
tkexamp : fillframe: no visible global function definition for
  ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:341)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:342)
tkexamp : fillframe: no visible global function definition for
  ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:342)
tkexamp : fillframe: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:343)
tkexamp : fillframe: no visible global function definition for
  ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:344-348)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:349-352)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:349-352)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:361)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:372-377)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:373)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:373)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:374-376)
tkexamp : fillframe : <anonymous>: no visible global function
  definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:374-376)
tkexamp : fillframe: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:380-382)
tkexamp : fillframe: no visible global function definition for
  ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:380-382)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:395)
tkexamp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:395)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:397-400)
tkexamp: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:397-400)
tkexamp : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:397-399)
tkexamp : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:397-399)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:401-407)
tkexamp: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:401-407)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:408-409)
tkexamp: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:408-409)
tkexamp : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:408-409)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:412)
tkexamp: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:412)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:413)
tkexamp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:413)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:414)
tkexamp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:414)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:415)
tkexamp: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:415)
tkexamp: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:416)
tkexamp: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:416)
tkexamp: no visible global function definition for ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:420-421)
tkexamp: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:420-421)
tkexamp: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/tkexamp.R:424)
vis.binom: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:9)
vis.binom: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:10)
vis.binom: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:12)
vis.binom: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:13)
vis.binom : binom.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:17)
vis.binom : binom.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:18)
vis.binom : binom.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:20)
vis.binom : binom.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:21)
vis.binom: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:59)
vis.binom: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:60)
vis.binom: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:61)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:64)
vis.binom: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:64)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:65)
vis.binom: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:65)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:66-69)
vis.binom: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:66-69)
vis.binom: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:71)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:74)
vis.binom: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:74)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:75)
vis.binom: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:75)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:76-79)
vis.binom: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:76-79)
vis.binom: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:81)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:84)
vis.binom: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:84)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:85-86)
vis.binom: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:85-86)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:87-88)
vis.binom: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:87-88)
vis.binom: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:90)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:93)
vis.binom: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:93)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:94-95)
vis.binom: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:94-95)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:96-97)
vis.binom: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:96-97)
vis.binom: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:99)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:101)
vis.binom: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:101)
vis.binom: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:102-103)
vis.binom: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:102-103)
vis.binom : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.binom.R:102-103)
vis.boxcox: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:70)
vis.boxcox: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:71)
vis.boxcox: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:72)
vis.boxcox: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:73)
vis.boxcox: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:74)
vis.boxcox: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:75)
vis.boxcox : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:78)
vis.boxcox: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:95)
vis.boxcox: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:96)
vis.boxcox: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:98)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:99)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:101)
vis.boxcox: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:101)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:102)
vis.boxcox: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:102)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:103-107)
vis.boxcox: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:103-107)
vis.boxcox : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:103-107)
vis.boxcox : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:103-107)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:109)
vis.boxcox: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:109)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:110-113)
vis.boxcox: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:110-113)
vis.boxcox : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:110-113)
vis.boxcox : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:110-113)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:115-116)
vis.boxcox: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:115-116)
vis.boxcox : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:115-116)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:118)
vis.boxcox: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:118)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:119)
vis.boxcox: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:119)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:120)
vis.boxcox: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:120)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:121)
vis.boxcox: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:121)
vis.boxcox: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:122)
vis.boxcox: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:122)
vis.boxcox: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:125)
vis.boxcox: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:126-129)
vis.boxcox.old: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:18)
vis.boxcox.old : bc.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:21)
vis.boxcox.old: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:39)
vis.boxcox.old: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:40)
vis.boxcox.old: no visible global function definition for
  ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:41)
vis.boxcox.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:43)
vis.boxcox.old: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:43)
vis.boxcox.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:44)
vis.boxcox.old: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:44)
vis.boxcox.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:45-47)
vis.boxcox.old: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:45-47)
vis.boxcox.old: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:49)
vis.boxcox.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:51)
vis.boxcox.old: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:51)
vis.boxcox.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:52-53)
vis.boxcox.old: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:52-53)
vis.boxcox.old : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcox.R:52-53)
vis.boxcoxu: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:78)
vis.boxcoxu: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:79)
vis.boxcoxu: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:80)
vis.boxcoxu: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:81)
vis.boxcoxu: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:82)
vis.boxcoxu: no visible global function definition for ‘tclvalue<-’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:83)
vis.boxcoxu : replot: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:86)
vis.boxcoxu: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:105)
vis.boxcoxu: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:106)
vis.boxcoxu: no visible global function definition for ‘tkrplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:108)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:109)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:111)
vis.boxcoxu: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:111)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:112)
vis.boxcoxu: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:112)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:113-117)
vis.boxcoxu: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:113-117)
vis.boxcoxu : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:113-117)
vis.boxcoxu : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:113-117)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:119)
vis.boxcoxu: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:119)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:120-123)
vis.boxcoxu: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:120-123)
vis.boxcoxu : <anonymous>: no visible global function definition for
  ‘tkrreplot’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:120-123)
vis.boxcoxu : <anonymous>: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:120-123)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:125-126)
vis.boxcoxu: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:125-126)
vis.boxcoxu : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:125-126)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:128)
vis.boxcoxu: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:128)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:129)
vis.boxcoxu: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:129)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:130)
vis.boxcoxu: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:130)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:131)
vis.boxcoxu: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:131)
vis.boxcoxu: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:132)
vis.boxcoxu: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:132)
vis.boxcoxu: no visible global function definition for ‘tkwait.window’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:135)
vis.boxcoxu: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:136-138)
vis.boxcoxu.old: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:18)
vis.boxcoxu.old : bc.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:21)
vis.boxcoxu.old: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:41)
vis.boxcoxu.old: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:42)
vis.boxcoxu.old: no visible global function definition for
  ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:43)
vis.boxcoxu.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:45)
vis.boxcoxu.old: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:45)
vis.boxcoxu.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:46)
vis.boxcoxu.old: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:46)
vis.boxcoxu.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:47-49)
vis.boxcoxu.old: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:47-49)
vis.boxcoxu.old: no visible global function definition for
  ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:51)
vis.boxcoxu.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:53)
vis.boxcoxu.old: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:53)
vis.boxcoxu.old: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:54-55)
vis.boxcoxu.old: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:54-55)
vis.boxcoxu.old : <anonymous>: no visible global function definition
  for ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.boxcoxu.R:54-55)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:9)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:10)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:11)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:12)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:13)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:15)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:16)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:17)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:19)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:20)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:21)
vis.gamma: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:22)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:33)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:34)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:35)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:36)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:37)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:122)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:123)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:124)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:126)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:127)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:128)
vis.gamma : gamma.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:129)
vis.gamma: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:162)
vis.gamma: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:163)
vis.gamma: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:164)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:167)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:167)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:168)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:168)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:169-172)
vis.gamma: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:169-172)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:174)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:177)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:177)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:178)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:178)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:179-182)
vis.gamma: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:179-182)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:184)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:187)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:187)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:188)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:188)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:189-192)
vis.gamma: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:189-192)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:194)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:197)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:197)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:198)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:198)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:199-202)
vis.gamma: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:199-202)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:204)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:207)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:207)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:208)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:208)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:209-212)
vis.gamma: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:209-212)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:214)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:218)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:218)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:219-220)
vis.gamma: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:219-220)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:221-222)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:221-222)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:224)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:227)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:227)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:228-229)
vis.gamma: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:228-229)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:230-231)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:230-231)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:233)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:236)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:236)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:237-238)
vis.gamma: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:237-238)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:239-240)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:239-240)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:242)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:246)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:246)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:247)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:247)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:248)
vis.gamma: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:248)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:250)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:253)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:253)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:254)
vis.gamma: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:254)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:256)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:259)
vis.gamma: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:259)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:260)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:260)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:261)
vis.gamma: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:261)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:263)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:266)
vis.gamma: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:266)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:267)
vis.gamma: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:267)
vis.gamma: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:269)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:272)
vis.gamma: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:272)
vis.gamma: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:274-275)
vis.gamma: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:274-275)
vis.gamma : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.gamma.R:274-275)
vis.normal: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:10)
vis.normal: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:11)
vis.normal: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:12)
vis.normal: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:13)
vis.normal: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:14)
vis.normal: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:15)
vis.normal: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:16)
vis.normal : norm.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:23)
vis.normal : norm.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:24)
vis.normal : norm.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:25)
vis.normal : norm.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:35)
vis.normal : norm.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:42)
vis.normal : norm.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:43)
vis.normal : norm.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:44)
vis.normal : norm.refresh: no visible global function definition for
  ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:45)
vis.normal: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:58)
vis.normal: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:59)
vis.normal: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:60)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:63)
vis.normal: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:63)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:64)
vis.normal: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:64)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:65-68)
vis.normal: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:65-68)
vis.normal: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:70)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:73)
vis.normal: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:73)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:74)
vis.normal: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:74)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:75-78)
vis.normal: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:75-78)
vis.normal: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:80)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:83)
vis.normal: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:83)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:84)
vis.normal: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:84)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:85-88)
vis.normal: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:85-88)
vis.normal: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:90)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:94)
vis.normal: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:94)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:95)
vis.normal: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:95)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:96)
vis.normal: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:96)
vis.normal: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:98)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:101)
vis.normal: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:101)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:102)
vis.normal: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:102)
vis.normal: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:104)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:107)
vis.normal: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:107)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:108)
vis.normal: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:108)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:109)
vis.normal: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:109)
vis.normal: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:111)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:114)
vis.normal: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:114)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:115)
vis.normal: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:115)
vis.normal: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:117)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:120)
vis.normal: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:120)
vis.normal: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:122-123)
vis.normal: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:122-123)
vis.normal : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.normal.R:122-123)
vis.t: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:8)
vis.t: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:9)
vis.t: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:11)
vis.t: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:12)
vis.t: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:13)
vis.t: no visible global function definition for ‘tclVar’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:14)
vis.t : t.refresh: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:20)
vis.t : t.refresh: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:21)
vis.t : t.refresh: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:23)
vis.t : t.refresh: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:24)
vis.t : t.refresh: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:25)
vis.t : t.refresh: no visible global function definition for ‘tclvalue’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:26)
vis.t: no visible global function definition for ‘tktoplevel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:44)
vis.t: no visible global function definition for ‘tkwm.title’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:45)
vis.t: no visible global function definition for ‘tkwm.geometry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:46)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:49)
vis.t: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:49)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:50)
vis.t: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:50)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:51-54)
vis.t: no visible global function definition for ‘tkscale’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:51-54)
vis.t: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:56)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:59)
vis.t: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:59)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:60)
vis.t: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:60)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:61-62)
vis.t: no visible global function definition for ‘tkcheckbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:61-62)
vis.t: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:64)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:68)
vis.t: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:68)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:69)
vis.t: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:69)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:70)
vis.t: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:70)
vis.t: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:72)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:75)
vis.t: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:75)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:76)
vis.t: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:76)
vis.t: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:78)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:81)
vis.t: no visible global function definition for ‘tkframe’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:81)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:82)
vis.t: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:82)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:83)
vis.t: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:83)
vis.t: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:85)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:88)
vis.t: no visible global function definition for ‘tklabel’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:88)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:89)
vis.t: no visible global function definition for ‘tkentry’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:89)
vis.t: no visible global function definition for ‘tkconfigure’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:91)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:94)
vis.t: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:94)
vis.t: no visible global function definition for ‘tkpack’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:96-97)
vis.t: no visible global function definition for ‘tkbutton’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:96-97)
vis.t : <anonymous>: no visible global function definition for
  ‘tkdestroy’
  (/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TeachingDemos.Rcheck/00_pkg_src/TeachingDemos/R/vis.t.R:96-97)
```
```
checking Rd line widths ... NOTE
Rd file 'HWidentify.Rd':
  \usage lines wider than 90 characters:
     pt.col="red", adj=c(0,0), xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), ...)

Rd file 'TkApprox.Rd':
  \usage lines wider than 90 characters:
     TkApprox(x, y, type = "b", snap.to.x = FALSE, digits = 4, cols = c("red", "#009900", "blue"), xlab = deparse(substitute(x)), ylab = dep ... [TRUNCATED]

Rd file 'TkBuildDist.Rd':
  \usage lines wider than 90 characters:
     TkBuildDist(x = seq(min + (max - min)/nbin/2, max - (max - min)/nbin/2, length.out = nbin), min = 0, max = 10, nbin = 10, logspline = T ... [TRUNCATED]

Rd file 'ci.examp.Rd':
  \usage lines wider than 90 characters:
     ci.examp(mean.sim = 100, sd = 10, n = 25, reps = 50, conf.level = 0.95, method = "z", lower.conf = (1 - conf.level)/2, upper.conf = 1 - ... [TRUNCATED]

Rd file 'correct.Rd':
  \usage lines wider than 90 characters:
     cor.rect.plot(x, y, corr = TRUE, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), col = c("#ff000055", "#0000ff55"), ...)

Rd file 'dynIdentify.Rd':
  \usage lines wider than 90 characters:
     dynIdentify(x, y, labels = seq_along(x), corners = cbind(c(-1, 0, 1, -1, 1, -1, 0, 1), c(1, 1, 1, 0, 0, -1, -1, -1)), ...)
     TkIdentify(x, y, labels=seq_along(x), hscale=1.75, vscale=1.75, corners = cbind( c(-1,0,1,-1,1,-1,0,1), c(1,1,1,0,0,-1,-1,-1) ),...)

Rd file 'gp.open.Rd':
  \usage lines wider than 90 characters:
     gp.splot(x,y,z, add=FALSE, title=deparse(substitute(z)), pipe=gpenv$gp, datafile=tempfile())

Rd file 'loess.demo.Rd':
  \usage lines wider than 90 characters:
     loess.demo(x, y, span = 2/3, degree = 1, nearest = FALSE, xlim = numeric(0), ylim = numeric(0), verbose = FALSE)

Rd file 'mle.demo.Rd':
  \usage lines wider than 90 characters:
     mle.demo(x = rnorm(10, 10, 2), start.mean = mean(x) - start.sd, start.sd = 1.2 * sqrt(var(x)))

Rd file 'shadowtext.Rd':
  \usage lines wider than 90 characters:
     shadowtext(x, y = NULL, labels, col = "white", bg = "black", theta = seq(pi/4, 2 * pi, length.out = 8), r = 0.1, ...)

Rd file 'sigma.test.Rd':
  \usage lines wider than 90 characters:
     sigma.test(x, sigma = 1, sigmasq = sigma^2, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, ...)

Rd file 'slider.Rd':
  \usage lines wider than 90 characters:
     slider(sl.functions, sl.names, sl.mins, sl.maxs, sl.deltas, sl.defaults, but.functions, but.names, no, set.no.value, obj.name, obj.valu ... [TRUNCATED]

Rd file 'sliderv.Rd':
  \usage lines wider than 90 characters:
     sliderv(refresh.code, names, minima, maxima, resolutions, starts, title = "control", no = 0, set.no.value = 0)

Rd file 'triplot.Rd':
  \usage lines wider than 90 characters:
     dimnames(x)[[1]], legend = NULL, legend.split = NULL, inner = TRUE, inner.col = c("lightblue", "pink"), inner.lty = c(2, 3), add = FALS ... [TRUNCATED]

Rd file 'z.test.Rd':
  \usage lines wider than 90 characters:
     z.test(x, mu = 0, stdev, alternative = c("two.sided", "less", "greater"), sd = stdev, conf.level = 0.95, ...)

These lines will be truncated in the PDF manual.
```
```
DONE
Status: 4 NOTEs
```

## tidyjson (0.2.1)
Maintainer: Jeremy Stanley <jeremy.stanley@gmail.com>

__OK__

## tigerstats (0.2.7)
Maintainer: Homer White <hwhite0@georgetowncollege.edu>

__OK__

## tigris (0.1)
Maintainer: Kyle Walker <kyle.walker@tcu.edu>

```
checking whether package ‘tigris’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘sp::nowrapSpatialLines’ when loading ‘tigris’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/tigris.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## tikzDevice (0.8.1)
Maintainer: Kirill Müller <krlmlr+r@mailbox.org>  
Bug reports: https://github.com/yihui/tikzDevice/issues

__OK__

## timeit (0.2.1)
Maintainer: Kevin Ushey <kevinushey@gmail.com>

```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘ggplot2’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Package in Depends field not imported from: ‘microbenchmark’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
determine_replications: no visible global function definition for
  ‘microbenchmark’
plot.timeit: no visible global function definition for ‘ggplot’
plot.timeit: no visible global function definition for ‘aes’
plot.timeit: no visible global function definition for ‘geom_boxplot’
plot.timeit: no visible global function definition for ‘geom_point’
plot.timeit: no visible global function definition for ‘xlab’
plot.timeit: no visible global function definition for ‘ylab’
plot.timeit: no visible global function definition for ‘ggtitle’
plot.timeit: no visible global function definition for ‘coord_flip’
```
```
DONE
Status: 2 NOTEs
```

## timeline (0.9)
Maintainer: Jason Bryer <jason@bryer.org>  
Bug reports: https://github.com/jbryer/timeline/issues

```
checking dependencies in R code ... NOTE
'library' or 'require' call to ‘shiny’ in package code.
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Package in Depends field not imported from: ‘ggplot2’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
timeline: no visible global function definition for ‘ggplot’
timeline: no visible global function definition for ‘geom_segment’
timeline: no visible global function definition for ‘aes_string’
timeline: no visible global function definition for ‘geom_rect’
timeline: no visible global function definition for ‘geom_text’
timeline: no visible global function definition for ‘theme’
timeline: no visible global function definition for ‘element_blank’
timeline: no visible global function definition for ‘xlab’
timeline: no visible global function definition for ‘ylab’
timeline: no visible global function definition for ‘xlim’
timeline: no visible global function definition for
  ‘scale_y_continuous’
timeline: no visible global function definition for ‘geom_point’
timeline: no visible global function definition for ‘scale_color_grey’
timeline: no visible global function definition for ‘geom_hline’
```
```
DONE
Status: 2 NOTEs
```

## TimeProjection (0.2.0)
Maintainer: Jeffrey Wong <jeff.ct.wong@gmail.com>

```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘ggplot2’ ‘plyr’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Packages in Depends field not imported from:
  ‘Matrix’ ‘lubridate’ ‘timeDate’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
is.Bizday: no visible global function definition for ‘isWeekday’
plotCalendarHeatmap: no visible global function definition for ‘ddply’
plotCalendarHeatmap: no visible global function definition for ‘.’
plotCalendarHeatmap: no visible binding for global variable ‘year’
plotCalendarHeatmap: no visible binding for global variable ‘month’
plotCalendarHeatmap: no visible binding for global variable ‘week’
plotCalendarHeatmap: no visible global function definition for ‘ggplot’
plotCalendarHeatmap: no visible global function definition for ‘aes’
plotCalendarHeatmap: no visible binding for global variable ‘monthweek’
plotCalendarHeatmap: no visible binding for global variable ‘weekday’
plotCalendarHeatmap: no visible global function definition for
  ‘geom_tile’
plotCalendarHeatmap: no visible global function definition for
  ‘facet_grid’
plotCalendarHeatmap: no visible global function definition for
  ‘scale_fill_gradientn’
projectDate: no visible global function definition for ‘holidayNYSE’
projectDate: no visible global function definition for
  ‘sparse.model.matrix’
```
```
DONE
Status: 2 NOTEs
```

## tourr (0.5.4)
Maintainer: Hadley Wickham <h.wickham@gmail.com>  
Bug reports: https://github.com/ggobi/tourr/

```
checking dependencies in R code ... NOTE
'library' or 'require' calls in package code:
  ‘TeachingDemos’ ‘ash’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
```
```
DONE
Status: 1 NOTE
```

## tourrGui (0.4)
Maintainer: Dianne Cook <dicook@iastate.edu>

```
checking package dependencies ... NOTE
Package suggested but not available for checking: ‘rggobi’
```
```
checking DESCRIPTION meta-information ... NOTE
License components which are templates and need '+ file LICENSE':
  MIT
```
```
checking dependencies in R code ... NOTE
'library' or 'require' calls to packages already attached by Depends:
  ‘Cairo’ ‘RGtk2’ ‘colorspace’ ‘gWidgets’ ‘tourr’
  Please remove these calls from your code.
'library' or 'require' calls in package code:
  ‘TeachingDemos’ ‘ash’
  Please use :: or requireNamespace() instead.
  See section 'Suggested packages' in the 'Writing R Extensions' manual.
Packages in Depends field not imported from:
  ‘Cairo’ ‘RGtk2’ ‘colorspace’ ‘gWidgets’ ‘tourr’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.
```
```
checking R code for possible problems ... NOTE
.create_1d_tour: no visible global function definition for ‘gmessage’
.create_1d_tour: no visible global function definition for
  ‘display_dist’
.create_1d_tour: no visible global function definition for ‘grand_tour’
.create_1d_tour: no visible global function definition for
  ‘little_tour’
.create_1d_tour: no visible global function definition for
  ‘guided_tour’
.create_1d_tour: no visible binding for global variable ‘holes’
.create_1d_tour: no visible binding for global variable ‘cmass’
.create_1d_tour: no visible global function definition for ‘lda_pp’
.create_1d_tour: no visible global function definition for ‘pda_pp’
.create_1d_tour: no visible global function definition for ‘local_tour’
.create_1d_tour: no visible global function definition for ‘basis_init’
.create_1d_tour: no visible global function definition for ‘sphere’
.create_1d_tour: no visible global function definition for ‘rescale’
.create_andrews_tour: no visible global function definition for
  ‘gmessage’
.create_andrews_tour: no visible global function definition for
  ‘rainbow_hcl’
.create_andrews_tour: no visible global function definition for
  ‘display_andrews’
.create_andrews_tour: no visible global function definition for
  ‘grand_tour’
.create_andrews_tour: no visible global function definition for
  ‘little_tour’
.create_andrews_tour: no visible global function definition for
  ‘guided_tour’
.create_andrews_tour: no visible binding for global variable ‘holes’
.create_andrews_tour: no visible binding for global variable ‘cmass’
.create_andrews_tour: no visible global function definition for
  ‘lda_pp’
.create_andrews_tour: no visible global function definition for
  ‘pda_pp’
.create_andrews_tour: no visible global function definition for
  ‘local_tour’
.create_andrews_tour: no visible global function definition for
  ‘basis_init’
.create_andrews_tour: no visible global function definition for
  ‘sphere’
.create_andrews_tour: no visible global function definition for
  ‘rescale’
.create_face_tour: no visible global function definition for ‘gmessage’
.create_face_tour: no visible global function definition for
  ‘display_faces’
.create_face_tour: no visible global function definition for
  ‘grand_tour’
.create_face_tour: no visible global function definition for
  ‘little_tour’
.create_face_tour: no visible global function definition for
  ‘guided_tour’
.create_face_tour: no visible binding for global variable ‘holes’
.create_face_tour: no visible binding for global variable ‘cmass’
.create_face_tour: no visible global function definition for ‘lda_pp’
.create_face_tour: no visible global function definition for ‘pda_pp’
.create_face_tour: no visible global function definition for
  ‘local_tour’
.create_face_tour: no visible global function definition for
  ‘basis_init’
.create_face_tour: no visible global function definition for ‘sphere’
.create_face_tour: no visible global function definition for ‘rescale’
.create_image_tour: no visible global function definition for
  ‘gmessage’
.create_image_tour: no visible global function definition for
  ‘display_image’
.create_image_tour: no visible global function definition for
  ‘grand_tour’
.create_image_tour: no visible global function definition for
  ‘little_tour’
.create_image_tour: no visible global function definition for
  ‘local_tour’
.create_image_tour: no visible global function definition for
  ‘guided_tour’
.create_image_tour: no visible binding for global variable ‘holes’
.create_image_tour: no visible binding for global variable ‘cmass’
.create_image_tour: no visible global function definition for ‘rescale’
.create_mat_tour: no visible global function definition for ‘gmessage’
.create_mat_tour: no visible global function definition for
  ‘display_scatmat’
.create_mat_tour: no visible global function definition for
  ‘grand_tour’
.create_mat_tour: no visible global function definition for
  ‘little_tour’
.create_mat_tour: no visible global function definition for
  ‘guided_tour’
.create_mat_tour: no visible binding for global variable ‘holes’
.create_mat_tour: no visible binding for global variable ‘cmass’
.create_mat_tour: no visible global function definition for ‘lda_pp’
.create_mat_tour: no visible global function definition for ‘pda_pp’
.create_mat_tour: no visible global function definition for
  ‘local_tour’
.create_mat_tour: no visible global function definition for
  ‘basis_init’
.create_mat_tour: no visible global function definition for ‘sphere’
.create_mat_tour: no visible global function definition for ‘rescale’
.create_pcp_tour: no visible global function definition for ‘gmessage’
.create_pcp_tour: no visible global function definition for
  ‘display_pcp’
.create_pcp_tour: no visible global function definition for
  ‘grand_tour’
.create_pcp_tour: no visible global function definition for
  ‘little_tour’
.create_pcp_tour: no visible global function definition for
  ‘guided_tour’
.create_pcp_tour: no visible binding for global variable ‘holes’
.create_pcp_tour: no visible binding for global variable ‘cmass’
.create_pcp_tour: no visible global function definition for ‘lda_pp’
.create_pcp_tour: no visible global function definition for ‘pda_pp’
.create_pcp_tour: no visible global function definition for
  ‘local_tour’
.create_pcp_tour: no visible global function definition for
  ‘basis_init’
.create_pcp_tour: no visible global function definition for ‘sphere’
.create_pcp_tour: no visible global function definition for ‘rescale’
.create_stars_tour: no visible global function definition for
  ‘gmessage’
.create_stars_tour: no visible global function definition for
  ‘display_stars’
.create_stars_tour: no visible global function definition for
  ‘grand_tour’
.create_stars_tour: no visible global function definition for
  ‘little_tour’
.create_stars_tour: no visible global function definition for
  ‘guided_tour’
.create_stars_tour: no visible binding for global variable ‘holes’
.create_stars_tour: no visible binding for global variable ‘cmass’
.create_stars_tour: no visible global function definition for ‘lda_pp’
.create_stars_tour: no visible global function definition for ‘pda_pp’
.create_stars_tour: no visible global function definition for
  ‘local_tour’
.create_stars_tour: no visible global function definition for
  ‘basis_init’
.create_stars_tour: no visible global function definition for ‘sphere’
.create_stars_tour: no visible global function definition for ‘rescale’
.create_stereo_tour: no visible global function definition for
  ‘gmessage’
.create_stereo_tour: no visible global function definition for
  ‘display_stereo’
.create_stereo_tour: no visible global function definition for
  ‘grand_tour’
.create_stereo_tour: no visible global function definition for
  ‘little_tour’
.create_stereo_tour: no visible global function definition for
  ‘guided_tour’
.create_stereo_tour: no visible binding for global variable ‘holes’
.create_stereo_tour: no visible binding for global variable ‘cmass’
.create_stereo_tour: no visible global function definition for ‘lda_pp’
.create_stereo_tour: no visible global function definition for ‘pda_pp’
.create_stereo_tour: no visible global function definition for
  ‘local_tour’
.create_stereo_tour: no visible global function definition for ‘sphere’
.create_stereo_tour: no visible global function definition for
  ‘rescale’
.create_xy_tour: no visible global function definition for ‘gmessage’
.create_xy_tour: no visible global function definition for
  ‘rainbow_hcl’
.create_xy_tour: no visible global function definition for ‘display_xy’
.create_xy_tour: no visible global function definition for ‘grand_tour’
.create_xy_tour: no visible global function definition for
  ‘little_tour’
.create_xy_tour: no visible global function definition for
  ‘guided_tour’
.create_xy_tour: no visible binding for global variable ‘holes’
.create_xy_tour: no visible binding for global variable ‘cmass’
.create_xy_tour: no visible global function definition for ‘lda_pp’
.create_xy_tour: no visible global function definition for ‘pda_pp’
.create_xy_tour: no visible global function definition for ‘local_tour’
.create_xy_tour: no visible global function definition for ‘basis_init’
.create_xy_tour: no visible global function definition for ‘sphere’
.create_xy_tour: no visible global function definition for ‘rescale’
.interface_andrews : update_tour_andrews: no visible global function
  definition for ‘svalue’
.interface_andrews : draw_frame_andrews: no visible global function
  definition for ‘svalue’
.interface_andrews : draw_frame_andrews: no visible global function
  definition for ‘find_platform’
.interface_andrews: no visible global function definition for ‘glayout’
.interface_andrews: no visible global function definition for
  ‘gcheckboxgroup’
.interface_andrews: no visible global function definition for
  ‘tooltip<-’
.interface_andrews: no visible global function definition for ‘gtable’
.interface_andrews: no visible global function definition for ‘gradio’
.interface_andrews: no visible global function definition for
  ‘gdroplist’
.interface_andrews: no visible global function definition for ‘gslider’
.interface_andrews: no visible global function definition for
  ‘gcheckbox’
.interface_andrews : <anonymous>: no visible global function definition
  for ‘svalue’
.interface_andrews : pause_andrews: no visible global function
  definition for ‘svalue<-’
.interface_andrews : pause_andrews: no visible global function
  definition for ‘gtkIdleRemove’
.interface_andrews : pause_andrews: no visible global function
  definition for ‘gIdleAdd’
.interface_andrews: no visible global function definition for ‘ggroup’
.interface_andrews: no visible global function definition for ‘gbutton’
.interface_andrews : <anonymous>: no visible global function definition
  for ‘dispose’
.interface_density : update_tour_density: no visible global function
  definition for ‘svalue’
.interface_density : draw_frame_density: no visible global function
  definition for ‘svalue’
.interface_density : draw_frame_density: no visible global function
  definition for ‘find_platform’
.interface_density: no visible global function definition for ‘glayout’
.interface_density: no visible global function definition for
  ‘gcheckboxgroup’
.interface_density: no visible global function definition for
  ‘tooltip<-’
.interface_density: no visible global function definition for ‘gtable’
.interface_density: no visible global function definition for ‘gradio’
.interface_density: no visible global function definition for
  ‘gdroplist’
.interface_density: no visible global function definition for ‘gslider’
.interface_density: no visible global function definition for
  ‘gcheckbox’
.interface_density : <anonymous>: no visible global function definition
  for ‘svalue’
.interface_density : pause_density: no visible global function
  definition for ‘svalue<-’
.interface_density : pause_density: no visible global function
  definition for ‘gtkIdleRemove’
.interface_density : pause_density: no visible global function
  definition for ‘gIdleAdd’
.interface_density: no visible global function definition for ‘ggroup’
.interface_density: no visible global function definition for ‘gbutton’
.interface_density : <anonymous>: no visible global function definition
  for ‘dispose’
.interface_faces : update_tour_faces: no visible global function
  definition for ‘svalue’
.interface_faces : draw_frame_faces: no visible global function
  definition for ‘svalue’
.interface_faces : draw_frame_faces: no visible global function
  definition for ‘find_platform’
.interface_faces: no visible global function definition for ‘glayout’
.interface_faces: no visible global function definition for
  ‘gcheckboxgroup’
.interface_faces: no visible global function definition for ‘tooltip<-’
.interface_faces: no visible global function definition for ‘gtable’
.interface_faces: no visible global function definition for ‘gradio’
.interface_faces: no visible global function definition for ‘gdroplist’
.interface_faces: no visible global function definition for ‘gslider’
.interface_faces: no visible global function definition for ‘gcheckbox’
.interface_faces : <anonymous>: no visible global function definition
  for ‘svalue’
.interface_faces : pause_faces: no visible global function definition
  for ‘svalue<-’
.interface_faces : pause_faces: no visible global function definition
  for ‘gtkIdleRemove’
.interface_faces : pause_faces: no visible global function definition
  for ‘gIdleAdd’
.interface_faces: no visible global function definition for ‘ggroup’
.interface_faces: no visible global function definition for ‘gbutton’
.interface_faces : <anonymous>: no visible global function definition
  for ‘dispose’
.interface_pcp : update_tour_pcp: no visible global function definition
  for ‘svalue’
.interface_pcp : draw_frame_pcp: no visible global function definition
  for ‘svalue’
.interface_pcp : draw_frame_pcp: no visible global function definition
  for ‘find_platform’
.interface_pcp: no visible global function definition for ‘glayout’
.interface_pcp: no visible global function definition for
  ‘gcheckboxgroup’
.interface_pcp: no visible global function definition for ‘tooltip<-’
.interface_pcp: no visible global function definition for ‘gtable’
.interface_pcp: no visible global function definition for ‘gradio’
.interface_pcp: no visible global function definition for ‘gdroplist’
.interface_pcp: no visible global function definition for ‘gslider’
.interface_pcp: no visible global function definition for ‘gcheckbox’
.interface_pcp : <anonymous>: no visible global function definition for
  ‘svalue’
.interface_pcp : pause_pcp: no visible global function definition for
  ‘svalue<-’
.interface_pcp : pause_pcp: no visible global function definition for
  ‘gtkIdleRemove’
.interface_pcp : pause_pcp: no visible global function definition for
  ‘gIdleAdd’
.interface_pcp: no visible global function definition for ‘ggroup’
.interface_pcp: no visible global function definition for ‘gbutton’
.interface_pcp : <anonymous>: no visible global function definition for
  ‘dispose’
.interface_scatmat : update_tour_scatmat: no visible global function
  definition for ‘svalue’
.interface_scatmat : draw_frame_scatmat: no visible global function
  definition for ‘svalue’
.interface_scatmat : draw_frame_scatmat: no visible global function
  definition for ‘find_platform’
.interface_scatmat: no visible global function definition for ‘glayout’
.interface_scatmat: no visible global function definition for
  ‘gcheckboxgroup’
.interface_scatmat: no visible global function definition for
  ‘tooltip<-’
.interface_scatmat: no visible global function definition for ‘gtable’
.interface_scatmat: no visible global function definition for ‘gradio’
.interface_scatmat: no visible global function definition for
  ‘gdroplist’
.interface_scatmat: no visible global function definition for ‘gslider’
.interface_scatmat: no visible global function definition for
  ‘gcheckbox’
.interface_scatmat : <anonymous>: no visible global function definition
  for ‘svalue’
.interface_scatmat : pause_scatmat: no visible global function
  definition for ‘svalue<-’
.interface_scatmat : pause_scatmat: no visible global function
  definition for ‘gtkIdleRemove’
.interface_scatmat : pause_scatmat: no visible global function
  definition for ‘gIdleAdd’
.interface_scatmat: no visible global function definition for ‘ggroup’
.interface_scatmat: no visible global function definition for ‘gbutton’
.interface_scatmat : <anonymous>: no visible global function definition
  for ‘dispose’
.interface_stars : update_tour_stars: no visible global function
  definition for ‘svalue’
.interface_stars : draw_frame_stars: no visible global function
  definition for ‘svalue’
.interface_stars : draw_frame_stars: no visible global function
  definition for ‘find_platform’
.interface_stars: no visible global function definition for ‘glayout’
.interface_stars: no visible global function definition for
  ‘gcheckboxgroup’
.interface_stars: no visible global function definition for ‘tooltip<-’
.interface_stars: no visible global function definition for ‘gtable’
.interface_stars: no visible global function definition for ‘gradio’
.interface_stars: no visible global function definition for ‘gdroplist’
.interface_stars: no visible global function definition for ‘gslider’
.interface_stars: no visible global function definition for ‘gcheckbox’
.interface_stars : <anonymous>: no visible global function definition
  for ‘svalue’
.interface_stars : pause_stars: no visible global function definition
  for ‘svalue<-’
.interface_stars : pause_stars: no visible global function definition
  for ‘gtkIdleRemove’
.interface_stars : pause_stars: no visible global function definition
  for ‘gIdleAdd’
.interface_stars: no visible global function definition for ‘ggroup’
.interface_stars: no visible global function definition for ‘gbutton’
.interface_stars : <anonymous>: no visible global function definition
  for ‘dispose’
.interface_stereo : update_tour_stereo: no visible global function
  definition for ‘svalue’
.interface_stereo : draw_frame_stereo: no visible global function
  definition for ‘svalue’
.interface_stereo : draw_frame_stereo: no visible global function
  definition for ‘find_platform’
.interface_stereo: no visible global function definition for ‘glayout’
.interface_stereo: no visible global function definition for
  ‘gcheckboxgroup’
.interface_stereo: no visible global function definition for
  ‘tooltip<-’
.interface_stereo: no visible global function definition for ‘gtable’
.interface_stereo: no visible global function definition for ‘gradio’
.interface_stereo: no visible global function definition for
  ‘gdroplist’
.interface_stereo: no visible global function definition for ‘gslider’
.interface_stereo: no visible global function definition for
  ‘gcheckbox’
.interface_stereo : <anonymous>: no visible global function definition
  for ‘svalue’
.interface_stereo : pause_stereo: no visible global function definition
  for ‘svalue<-’
.interface_stereo : pause_stereo: no visible global function definition
  for ‘gtkIdleRemove’
.interface_stereo : pause_stereo: no visible global function definition
  for ‘gIdleAdd’
.interface_stereo: no visible global function definition for ‘ggroup’
.interface_stereo: no visible global function definition for ‘gbutton’
.interface_stereo : <anonymous>: no visible global function definition
  for ‘dispose’
.interface_xy : update_tour_xy: no visible global function definition
  for ‘svalue’
.interface_xy : draw_frame_xy: no visible global function definition
  for ‘svalue’
.interface_xy: no visible global function definition for
  ‘find_platform’
.interface_xy: no visible global function definition for ‘glayout’
.interface_xy: no visible global function definition for
  ‘gcheckboxgroup’
.interface_xy: no visible global function definition for ‘tooltip<-’
.interface_xy: no visible global function definition for ‘gtable’
.interface_xy: no visible global function definition for ‘gradio’
.interface_xy: no visible global function definition for ‘gdroplist’
.interface_xy: no visible global function definition for ‘gslider’
.interface_xy: no visible global function definition for ‘gcheckbox’
.interface_xy : <anonymous>: no visible global function definition for
  ‘svalue’
.interface_xy : pause_xy: no visible global function definition for
  ‘svalue<-’
.interface_xy : pause_xy: no visible global function definition for
  ‘gtkIdleRemove’
.interface_xy : pause_xy: no visible global function definition for
  ‘gIdleAdd’
.interface_xy: no visible global function definition for ‘ggroup’
.interface_xy: no visible global function definition for ‘gbutton’
.interface_xy : <anonymous>: no visible global function definition for
  ‘dispose’
.interface_xy : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_andrews: no visible binding for global variable ‘flea’
gui_andrews: no visible global function definition for ‘find_platform’
gui_andrews : update_tour: no visible global function definition for
  ‘svalue’
gui_andrews : draw_frame: no visible global function definition for
  ‘svalue’
gui_andrews: no visible global function definition for ‘gwindow’
gui_andrews: no visible global function definition for ‘glayout’
gui_andrews: no visible global function definition for ‘gcheckboxgroup’
gui_andrews: no visible global function definition for ‘tooltip<-’
gui_andrews: no visible global function definition for ‘gtable’
gui_andrews: no visible global function definition for ‘gradio’
gui_andrews: no visible global function definition for ‘gdroplist’
gui_andrews: no visible global function definition for ‘gslider’
gui_andrews: no visible global function definition for ‘gcheckbox’
gui_andrews : <anonymous>: no visible global function definition for
  ‘svalue’
gui_andrews : pause: no visible global function definition for
  ‘svalue<-’
gui_andrews : pause: no visible global function definition for
  ‘gtkIdleRemove’
gui_andrews : pause: no visible global function definition for
  ‘gIdleAdd’
gui_andrews: no visible global function definition for ‘ggroup’
gui_andrews: no visible global function definition for ‘gbutton’
gui_andrews : <anonymous>: no visible global function definition for
  ‘dispose’
gui_andrews : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_andrews: no visible global function definition for ‘CairoX11’
gui_andrews: no visible global function definition for ‘visible<-’
gui_density: no visible binding for global variable ‘flea’
gui_density: no visible global function definition for ‘find_platform’
gui_density : update_tour: no visible global function definition for
  ‘svalue’
gui_density : draw_frame: no visible global function definition for
  ‘svalue’
gui_density: no visible global function definition for ‘gwindow’
gui_density: no visible global function definition for ‘glayout’
gui_density: no visible global function definition for ‘gcheckboxgroup’
gui_density: no visible global function definition for ‘tooltip<-’
gui_density: no visible global function definition for ‘gtable’
gui_density: no visible global function definition for ‘gradio’
gui_density: no visible global function definition for ‘gdroplist’
gui_density: no visible global function definition for ‘gslider’
gui_density: no visible global function definition for ‘gcheckbox’
gui_density : <anonymous>: no visible global function definition for
  ‘svalue’
gui_density : pause: no visible global function definition for
  ‘svalue<-’
gui_density : pause: no visible global function definition for
  ‘gtkIdleRemove’
gui_density : pause: no visible global function definition for
  ‘gIdleAdd’
gui_density: no visible global function definition for ‘ggroup’
gui_density: no visible global function definition for ‘gbutton’
gui_density : <anonymous>: no visible global function definition for
  ‘dispose’
gui_density : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_density: no visible global function definition for ‘CairoX11’
gui_density: no visible global function definition for ‘visible<-’
gui_faces: no visible binding for global variable ‘flea’
gui_faces: no visible global function definition for ‘find_platform’
gui_faces : update_tour: no visible global function definition for
  ‘svalue’
gui_faces : draw_frame: no visible global function definition for
  ‘svalue’
gui_faces: no visible global function definition for ‘gwindow’
gui_faces: no visible global function definition for ‘glayout’
gui_faces: no visible global function definition for ‘gcheckboxgroup’
gui_faces: no visible global function definition for ‘tooltip<-’
gui_faces: no visible global function definition for ‘gtable’
gui_faces: no visible global function definition for ‘gradio’
gui_faces: no visible global function definition for ‘gdroplist’
gui_faces: no visible global function definition for ‘gslider’
gui_faces: no visible global function definition for ‘gcheckbox’
gui_faces : <anonymous>: no visible global function definition for
  ‘svalue’
gui_faces : pause: no visible global function definition for ‘svalue<-’
gui_faces : pause: no visible global function definition for
  ‘gtkIdleRemove’
gui_faces : pause: no visible global function definition for ‘gIdleAdd’
gui_faces: no visible global function definition for ‘ggroup’
gui_faces: no visible global function definition for ‘gbutton’
gui_faces : <anonymous>: no visible global function definition for
  ‘dispose’
gui_faces : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_faces: no visible global function definition for ‘CairoX11’
gui_faces: no visible global function definition for ‘visible<-’
gui_image: no visible binding for global variable ‘ozone’
gui_image: no visible global function definition for ‘find_platform’
gui_image : update_tour: no visible global function definition for
  ‘svalue’
gui_image : draw_frame: no visible global function definition for
  ‘svalue’
gui_image: no visible global function definition for ‘gwindow’
gui_image: no visible global function definition for ‘glayout’
gui_image: no visible global function definition for ‘gradio’
gui_image: no visible global function definition for ‘tooltip<-’
gui_image: no visible global function definition for ‘gdroplist’
gui_image: no visible global function definition for ‘gslider’
gui_image: no visible global function definition for ‘gcheckbox’
gui_image : <anonymous>: no visible global function definition for
  ‘svalue’
gui_image : pause: no visible global function definition for ‘svalue<-’
gui_image : pause: no visible global function definition for
  ‘gtkIdleRemove’
gui_image : pause: no visible global function definition for ‘gIdleAdd’
gui_image: no visible global function definition for ‘ggroup’
gui_image: no visible global function definition for ‘gbutton’
gui_image : <anonymous>: no visible global function definition for
  ‘dispose’
gui_image : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_image: no visible global function definition for ‘CairoX11’
gui_image: no visible global function definition for ‘visible<-’
gui_pcp: no visible binding for global variable ‘flea’
gui_pcp: no visible global function definition for ‘find_platform’
gui_pcp : update_tour: no visible global function definition for
  ‘svalue’
gui_pcp : draw_frame: no visible global function definition for
  ‘svalue’
gui_pcp: no visible global function definition for ‘gwindow’
gui_pcp: no visible global function definition for ‘glayout’
gui_pcp: no visible global function definition for ‘gcheckboxgroup’
gui_pcp: no visible global function definition for ‘tooltip<-’
gui_pcp: no visible global function definition for ‘gtable’
gui_pcp: no visible global function definition for ‘gradio’
gui_pcp: no visible global function definition for ‘gdroplist’
gui_pcp: no visible global function definition for ‘gslider’
gui_pcp: no visible global function definition for ‘gcheckbox’
gui_pcp : <anonymous>: no visible global function definition for
  ‘svalue’
gui_pcp : pause: no visible global function definition for ‘svalue<-’
gui_pcp : pause: no visible global function definition for
  ‘gtkIdleRemove’
gui_pcp : pause: no visible global function definition for ‘gIdleAdd’
gui_pcp: no visible global function definition for ‘ggroup’
gui_pcp: no visible global function definition for ‘gbutton’
gui_pcp : <anonymous>: no visible global function definition for
  ‘dispose’
gui_pcp : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_pcp: no visible global function definition for ‘CairoX11’
gui_pcp: no visible global function definition for ‘visible<-’
gui_scatmat: no visible binding for global variable ‘flea’
gui_scatmat: no visible global function definition for ‘find_platform’
gui_scatmat : update_tour: no visible global function definition for
  ‘svalue’
gui_scatmat : draw_frame: no visible global function definition for
  ‘svalue’
gui_scatmat: no visible global function definition for ‘gwindow’
gui_scatmat: no visible global function definition for ‘glayout’
gui_scatmat: no visible global function definition for ‘gcheckboxgroup’
gui_scatmat: no visible global function definition for ‘tooltip<-’
gui_scatmat: no visible global function definition for ‘gtable’
gui_scatmat: no visible global function definition for ‘gradio’
gui_scatmat: no visible global function definition for ‘gdroplist’
gui_scatmat: no visible global function definition for ‘gslider’
gui_scatmat: no visible global function definition for ‘gcheckbox’
gui_scatmat : <anonymous>: no visible global function definition for
  ‘svalue’
gui_scatmat : pause: no visible global function definition for
  ‘svalue<-’
gui_scatmat : pause: no visible global function definition for
  ‘gtkIdleRemove’
gui_scatmat : pause: no visible global function definition for
  ‘gIdleAdd’
gui_scatmat: no visible global function definition for ‘ggroup’
gui_scatmat: no visible global function definition for ‘gbutton’
gui_scatmat : <anonymous>: no visible global function definition for
  ‘dispose’
gui_scatmat : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_scatmat: no visible global function definition for ‘CairoX11’
gui_scatmat: no visible global function definition for ‘visible<-’
gui_stars: no visible binding for global variable ‘flea’
gui_stars: no visible global function definition for ‘find_platform’
gui_stars : update_tour: no visible global function definition for
  ‘svalue’
gui_stars : draw_frame: no visible global function definition for
  ‘svalue’
gui_stars: no visible global function definition for ‘gwindow’
gui_stars: no visible global function definition for ‘glayout’
gui_stars: no visible global function definition for ‘gcheckboxgroup’
gui_stars: no visible global function definition for ‘tooltip<-’
gui_stars: no visible global function definition for ‘gtable’
gui_stars: no visible global function definition for ‘gradio’
gui_stars: no visible global function definition for ‘gdroplist’
gui_stars: no visible global function definition for ‘gslider’
gui_stars: no visible global function definition for ‘gcheckbox’
gui_stars : <anonymous>: no visible global function definition for
  ‘svalue’
gui_stars : pause: no visible global function definition for ‘svalue<-’
gui_stars : pause: no visible global function definition for
  ‘gtkIdleRemove’
gui_stars : pause: no visible global function definition for ‘gIdleAdd’
gui_stars: no visible global function definition for ‘ggroup’
gui_stars: no visible global function definition for ‘gbutton’
gui_stars : <anonymous>: no visible global function definition for
  ‘dispose’
gui_stars : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_stars: no visible global function definition for ‘CairoX11’
gui_stars: no visible global function definition for ‘visible<-’
gui_stereo: no visible binding for global variable ‘flea’
gui_stereo: no visible global function definition for ‘find_platform’
gui_stereo : update_tour: no visible global function definition for
  ‘svalue’
gui_stereo : draw_frame: no visible global function definition for
  ‘svalue’
gui_stereo: no visible global function definition for ‘gwindow’
gui_stereo: no visible global function definition for ‘glayout’
gui_stereo: no visible global function definition for ‘gcheckboxgroup’
gui_stereo: no visible global function definition for ‘tooltip<-’
gui_stereo: no visible global function definition for ‘gtable’
gui_stereo: no visible global function definition for ‘gradio’
gui_stereo: no visible global function definition for ‘gdroplist’
gui_stereo: no visible global function definition for ‘gslider’
gui_stereo: no visible global function definition for ‘gcheckbox’
gui_stereo : <anonymous>: no visible global function definition for
  ‘svalue’
gui_stereo : pause: no visible global function definition for
  ‘svalue<-’
gui_stereo : pause: no visible global function definition for
  ‘gtkIdleRemove’
gui_stereo : pause: no visible global function definition for
  ‘gIdleAdd’
gui_stereo: no visible global function definition for ‘ggroup’
gui_stereo: no visible global function definition for ‘gbutton’
gui_stereo : <anonymous>: no visible global function definition for
  ‘dispose’
gui_stereo : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_stereo: no visible global function definition for ‘CairoX11’
gui_stereo: no visible global function definition for ‘visible<-’
gui_tour: no visible binding for global variable ‘flea’
gui_tour: no visible global function definition for ‘find_platform’
gui_tour: no visible global function definition for ‘gwindow’
gui_tour: no visible global function definition for ‘gnotebook’
gui_tour: no visible global function definition for ‘ggroup’
gui_tour: no visible global function definition for ‘CairoX11’
gui_tour: no visible global function definition for ‘visible<-’
gui_xy: no visible binding for global variable ‘flea’
gui_xy: no visible global function definition for ‘find_platform’
gui_xy : update_tour: no visible global function definition for
  ‘svalue’
gui_xy : draw_frame: no visible global function definition for ‘svalue’
gui_xy: no visible global function definition for ‘gwindow’
gui_xy: no visible global function definition for ‘glayout’
gui_xy: no visible global function definition for ‘gcheckboxgroup’
gui_xy: no visible global function definition for ‘tooltip<-’
gui_xy: no visible global function definition for ‘gtable’
gui_xy: no visible global function definition for ‘gradio’
gui_xy: no visible global function definition for ‘gdroplist’
gui_xy: no visible global function definition for ‘gslider’
gui_xy: no visible global function definition for ‘gcheckbox’
gui_xy : <anonymous>: no visible global function definition for
  ‘svalue’
gui_xy : pause: no visible global function definition for ‘svalue<-’
gui_xy : pause: no visible global function definition for
  ‘gtkIdleRemove’
gui_xy : pause: no visible global function definition for ‘gIdleAdd’
gui_xy: no visible global function definition for ‘ggroup’
gui_xy: no visible global function definition for ‘gbutton’
gui_xy : <anonymous>: no visible global function definition for
  ‘dispose’
gui_xy : <anonymous>: no visible global function definition for
  ‘gmessage’
gui_xy: no visible global function definition for ‘CairoX11’
gui_xy: no visible global function definition for ‘visible<-’
```
```
DONE
Status: 4 NOTEs
```

## trapezoid (2.0-0)
Maintainer: Jeremy Thoms Hetzel <jthetzel@gmail.com>

__OK__

## TreatmentSelection (1.1.2)
Maintainer: Marshall Brown <mdbrown@fhcrc.org>

```
checking whether package ‘TreatmentSelection’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘TreatmentSelection’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘TreatmentSelection’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TreatmentSelection.Rcheck/00install.out’ for details.
```
```
checking S3 generic/method consistency ... NOTE
Found the following apparent S3 methods exported but not registered:
  plot.trtsel print.calibrate.trtsel print.compare.trtsel
  print.eval.trtsel print.trtsel
See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
manual.
```
```
checking R code for possible problems ... NOTE
trteffectPLOT_gg_disc: no visible global function definition for
  ‘stat_hline’
trteffectPLOTcompare_gg_disc: no visible global function definition for
  ‘stat_hline’
```
```
checking examples ... ERROR
Running examples in ‘TreatmentSelection-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: compare.trtsel
> ### Title: compare the performance of two treatment selection markers
> ### Aliases: compare.trtsel
> 
> ### ** Examples
> 
> 
> data(tsdata)
> 
> ###########################
> ## Create trtsel objects 
> ###########################
> 
> trtsel.Y1 <- trtsel( event = "event",
+                      trt = "trt",
+                      marker = "Y1",
+                      data = tsdata,
+                      study.design = "randomized cohort")
> trtsel.Y1
Study design: randomized cohort 

Model Fit:

 Link function: logit 

 Coefficients: 
               Estimate  Std. Error    z value     Pr(>|z|)
(Intercept) -2.51814383 0.235642511 -10.686288 1.179991e-26
trt          0.48938620 0.311762857   1.569739 1.164759e-01
marker       0.04760056 0.006453791   7.375597 1.636104e-13
trt:marker  -0.02318881 0.008324063  -2.785756 5.340300e-03


Derived Data: (first ten rows)

   trt event  marker fittedrisk.t0 fittedrisk.t1    trt.effect marker.neg
1    1     1 39.9120    0.35016583     0.2583742  0.0917916549          0
2    1     0  6.6820    0.09974358     0.1340472 -0.0343036269          1
3    1     0  6.5820    0.09931697     0.1337641 -0.0344471266          1
4    0     0  1.3581    0.07918316     0.1196652 -0.0404820847          1
5    0     0  7.6820    0.10410005     0.1369063 -0.0328062456          1
6    0     0 41.1720    0.36393311     0.2643117  0.0996213622          0
7    1     0 19.4920    0.16933976     0.1746644 -0.0053246137          1
8    1     1 20.8220    0.17843231     0.1793943 -0.0009620341          1
9    0     0  6.9620    0.10094678     0.1348426 -0.0338958439          1
10   0     0  2.5020    0.08324538     0.1226384 -0.0393929781          1

> 
> trtsel.Y2 <- trtsel( event = "event",
+                      trt = "trt",
+                      marker = "Y2",
+                      data = tsdata,
+                      study.design = "randomized cohort")
> trtsel.Y2
Study design: randomized cohort 

Model Fit:

 Link function: logit 

 Coefficients: 
              Estimate Std. Error    z value     Pr(>|z|)
(Intercept) -1.2107912  0.1131642 -10.699416 1.024216e-26
trt         -0.5169008  0.1863643  -2.773604 5.543912e-03
marker       0.5779172  0.1148643   5.031305 4.871514e-07
trt:marker  -2.0455033  0.2064547  -9.907756 3.851994e-23


Derived Data: (first ten rows)

   trt event  marker fittedrisk.t0 fittedrisk.t1   trt.effect marker.neg
1    1     1 -0.8535     0.1539379    0.38340813 -0.229470242          1
2    1     0  0.2905     0.2605896    0.10395563  0.156633982          0
3    1     0  0.0800     0.2378401    0.13644937  0.101390712          0
4    0     0  1.1925     0.3724723    0.02995087  0.342521474          0
5    0     0 -0.2070     0.2090899    0.19405065  0.015039232          0
6    0     0 -0.0880     0.2206903    0.16818515  0.052505186          0
7    1     0  0.1670     0.2470740    0.12209072  0.124983277          0
8    1     1 -1.0485     0.1398258    0.45290799 -0.313082172          1
9    0     0 -0.2435     0.2056229    0.20256576  0.003057187          0
10   0     0  0.2030     0.2509647    0.11653995  0.134424710          0

> 
> #discrete markers
> trtsel.Y1_disc <- trtsel( event ="event", trt = "trt", marker = "Y1_disc", 
+                           data =  tsdata,
+                           study.design = "randomized cohort")
>                           
> trtsel.Y2_disc <- trtsel( event ="event", trt = "trt", marker = "Y2_disc",
+                           data = tsdata,
+                           study.design = "randomized cohort")
>                           
> 
> ###############################
> ## Compare marker performance
> ###############################
> 
> 
> # Plot treatment effect curves with pointwise confidence intervals
> ## use more bootstraps in practice
> compare.trtsel(trtsel1 = trtsel.Y1, trtsel2 = trtsel.Y2,
+                                 bootstraps = 10, plot = TRUE,      
+                                 ci = "horizontal",  conf.bands = TRUE) 
                      Summary Measure Estimates 
                    (with  95 % confidence intervals) 

               marker 1    |    marker 2    |   difference    (p-value)
 ------------------------------------------------------------------------

Decrease in event rate under marker-based treatment (Theta)
 Empirical:     0.013      |     0.090     |     -0.076         (< 0.1)
            (-0.007,0.131) | (0.079,0.236) | (-0.092,-0.083) 
 Model Based:   0.010      |     0.099      |     -0.088         (< 0.1)
            (0.002,0.043)  | (0.081,0.110)  | (-0.102,-0.055) 

Proportion marker negative:
                0.461      |     0.377      |     0.084         (0.4)
            (0.341,0.677)  | (0.338,0.487)  | (-0.055,0.202) 
Proportion marker positive:
                0.539      |     0.623      |     -0.084         (0.3)
            (0.324,0.659)  | (0.513,0.662)  | (-0.202,0.055) 

Average benefit of no treatment among marker-negatives (B.neg)
 Empirical:     0.029      |     0.238     |     -0.209         (< 0.1)
            (-0.017,0.069) | (0.208,0.255) | (-0.250,-0.157) 
 Model Based:   0.023      |     0.262      |     -0.239         (< 0.1)
            (0.007,0.057)  | (0.226,0.289)  | (-0.265,-0.202) 

Average benefit of treatment among marker-positives (B.pos)
 Empirical:     0.089      |     0.203     |     -0.114         (< 0.1)
            (0.013,0.146) | (0.177,0.234) | (-0.186,-0.075) 
 Model Based:   0.098      |     0.211      |     -0.113         (< 0.1)
            (0.046,0.131)  | (0.187,0.236)  | (-0.151,-0.083) 


Variance in estimated treatment effect : 
                0.007      |     0.080      |     -0.073         (< 0.1)
            (0.002,0.014)  | (0.067,0.101)  | (-0.094,-0.056) 

Total Gain: 
                0.066      |     0.224      |     -0.158         (< 0.1)
            (0.032,0.093)  | (0.202,0.254)  | (-0.186,-0.119) 

>                                 
> #compare discrete markers, plots are different                  
> compare.trtsel(trtsel1 = trtsel.Y1_disc, trtsel2 = trtsel.Y2_disc, ci = "vertical" , 
+                bootstraps = 10, plot = TRUE, offset = .1)                
Error in trteffectPLOTcompare_gg_disc(x1 = ts1, x2 = ts2, ci.bounds = myconf.ints,  : 
  could not find function "stat_hline"
Calls: compare.trtsel ... myplotcompare.trtsel_disc -> trteffectPLOTcompare_gg_disc
Execution halted
```
```
DONE
Status: 1 ERROR, 1 WARNING, 2 NOTEs
```

## treeclim (1.0.11)
Maintainer: Christian Zang <christian.zang@wzw.tum.de>

```
checking whether package ‘treeclim’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/treeclim.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## treecm (1.2.1)
Maintainer: Marco Bascietto <marco.bascietto@cnr.it>

```
checking DESCRIPTION meta-information ... NOTE
Malformed Title field: should not end in a period.
Malformed Description field: should contain one or more complete sentences.
```
```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘treecm.Rnw’
  ...
6547.653 3993.114 

> library(ggplot2)

> ggplot(biomass, aes(x = code, y = biomass)) + geom_bar(aes(fill = allometry), 
+     position = "dodge")

  When sourcing ‘treecm.R’:
Error: stat_count() must not be used with a y aesthetic.
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Error: processing vignette 'treecm.Rnw' failed with diagnostics:
stat_count() must not be used with a y aesthetic.
Execution halted

```
```
DONE
Status: 1 ERROR, 2 NOTEs
```

## treemap (2.4)
Maintainer: Martijn Tennekes <mtennekes@gmail.com>

```
checking whether package ‘treemap’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘treemap’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘treemap’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/treemap.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## treescape (1.0.0)
Maintainer: Michelle Kendall <m.kendall@imperial.ac.uk>

__OK__

## TriMatch (0.9.4)
Maintainer: Jason Bryer <jason@bryer.org>  
Bug reports: https://github.com/jbryer/TriMatch/issues

```
checking whether package ‘TriMatch’ can be installed ... ERROR
Installation failed.
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/TriMatch.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 ERROR
```

## TripleR (1.4.1)
Maintainer: Felix Schönbrodt <felix.schoenbrodt@psy.lmu.de>

__OK__

## TSMining (1.0)
Maintainer: Cheng Fan <raja8885@hotmail.com>

__OK__

## tspmeta (1.2)
Maintainer: Bernd Bischl <bernd_bischl@gmx.net>  
Bug reports: https://github.com/berndbischl/tspmeta/issues

__OK__

## tufterhandout (1.2.1)
Maintainer: Michael C Sachs <sachsmc@gmail.com>  
Bug reports: http://github.com/sachsmc/tufterhandout/issues

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 NOTE
```

## tvm (0.3.0)
Maintainer: Juan Manuel Truppia <jmtruppia@gmail.com>

__OK__

## UpSetR (0.0.5)
Maintainer: Jake Conway <jake_conway@student.uml.edu>  
Bug reports: http://github.com/hms-dbmi/UpSetR/issues

```
checking whether package ‘UpSetR’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘UpSetR’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘UpSetR’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/UpSetR.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## USAboundaries (0.1.1)
Maintainer: Lincoln Mullen <lincoln@lincolnmullen.com>  
Bug reports: https://github.com/ropensci/USAboundaries/issues

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  'citation()' on how to cite R or R packages in publications.
  
  Type 'demo()' for some demos, 'help()' for on-line help, or
  'help.start()' for an HTML browser interface to help.
  Type 'q()' to quit R.
  
  > library(testthat)
  > library(USAboundaries)
  > 
  > test_check("USAboundaries")
  Error in get("rgeos", envir = .MAPTOOLS_CACHE) : object 'rgeos' not found
  Calls: test_check ... fortify.SpatialPolygonsDataFrame -> <Anonymous> -> rgeosStatus -> get
  Execution halted
```
```
DONE
Status: 1 ERROR
```

## useful (1.1.9)
Maintainer: Jared P. Lander <packages@jaredlander.com>

```
checking whether package ‘useful’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘useful’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘useful’
  Warning: replacing previous import by ‘scales::alpha’ when loading ‘useful’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/useful.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## userfriendlyscience (0.3-0)
Maintainer: Gjalt-Jorn Peters <gjalt-jorn@userfriendlyscience.com>

```
checking examples ... ERROR
Running examples in ‘userfriendlyscience-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: meanDiff.multi
> ### Title: meanDiff.multi
> ### Aliases: meanDiff.multi
> ### Keywords: utilities
> 
> ### ** Examples
> 
> ### Create simple dataset
> dat <- data.frame(x1 = factor(rep(c(0,1), 20)),
+                   x2 = factor(c(rep(0, 20), rep(1, 20))),
+                   y=rep(c(4,5), 20) + rnorm(40));
> ### Compute mean difference and show it
> meanDiff.multi(dat, x=c('x1', 'x2'), y='y', var.equal="yes");
Warning: `show_guide` has been deprecated. Please use `show.legend` instead.
Error: Unknown parameters: size
Execution halted
```
```
DONE
Status: 1 ERROR
```

## UsingR (2.0-5)
Maintainer: John Verzani <verzani@math.csi.cuny.edu>

__OK__

## varbvs (1.0)
Maintainer: Peter Carbonetto <pcarbo@uchicago.edu>

```
checking foreign function calls ... NOTE
Calls with DUP:
   .C("varbvsbinupdateR", n = as.integer(n), m = as.integer(m), 
       X = X, sa = as.double(sa), logodds = as.double(logodds), 
       u = as.double(stats$u), xy = as.double(stats$xy), xu = as.double(stats$xu), 
       d = as.double(stats$d), alpha = as.double(alpha0), mu = as.double(mu0), 
       Xr = as.double(Xr0), S = as.integer(S - 1), DUP = FALSE)
   .C("varbvsupdateR", n = as.integer(n), m = as.integer(m), X = X, 
       sigma = as.double(sigma), sa = as.double(sa), logodds = as.double(logodds), 
       xy = as.double(xy), d = as.double(d), alpha = as.double(alpha0), 
       mu = as.double(mu0), Xr = as.double(Xr0), S = as.integer(S - 
           1), DUP = FALSE)
DUP is no longer supported and will be ignored.
```
```
DONE
Status: 1 NOTE
```

## varian (0.2.1)
Maintainer: Joshua F. Wiley <josh@elkhartgroup.com>  
Bug reports: https://github.com/ElkhartGroup/varian/issues

```
checking whether package ‘varian’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘varian’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘varian’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/varian.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## vcdExtra (0.6-11)
Maintainer: Michael Friendly <friendly@yorku.ca>

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘alr3’
```
```
checking examples ... ERROR
Running examples in ‘vcdExtra-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: Donner
> ### Title: Survival in the Donner Party
> ### Aliases: Donner
> ### Keywords: datasets
> 
> ### ** Examples
> 
> # conditional density plots
> op <- par(mfrow=c(1,2), cex.lab=1.5)
> cdplot(factor(survived) ~ age, subset=sex=='Male', data=Donner, 
+     main="Donner party: Males", ylevels=2:1, ylab="Survived", yaxlabels=c("yes", "no"))
> with(Donner, rug(jitter(age[sex=="Male"]), col="white", quiet=TRUE))
> cdplot(factor(survived) ~ age, subset=sex=='Female', data=Donner, 
+     main="Donner party: Females", ylevels=2:1, ylab="Survived", yaxlabels=c("yes", "no"))
> with(Donner, rug(jitter(age[sex=="Female"]), col="white", quiet=TRUE))
> par(op)
> 
> 
> # fit some models
> (mod1 <- glm(survived ~ age + sex, data=Donner, family=binomial))

Call:  glm(formula = survived ~ age + sex, family = binomial, data = Donner)

Coefficients:
(Intercept)          age      sexMale  
     1.5992      -0.0338      -1.2068  

Degrees of Freedom: 89 Total (i.e. Null);  87 Residual
Null Deviance:	    124.4 
Residual Deviance: 111.1 	AIC: 117.1
> (mod2 <- glm(survived ~ age * sex, data=Donner, family=binomial))

Call:  glm(formula = survived ~ age * sex, family = binomial, data = Donner)

Coefficients:
(Intercept)          age      sexMale  age:sexMale  
    1.85515     -0.04565     -1.62177      0.01957  

Degrees of Freedom: 89 Total (i.e. Null);  86 Residual
Null Deviance:	    124.4 
Residual Deviance: 110.7 	AIC: 118.7
> anova(mod2, test="Chisq")
Analysis of Deviance Table

Model: binomial, link: logit

Response: survived

Terms added sequentially (first to last)


        Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
NULL                       89     124.37            
age      1   6.5113        88     117.86 0.010719 * 
sex      1   6.7274        87     111.13 0.009494 **
age:sex  1   0.4003        86     110.73 0.526922   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> (mod3 <- glm(survived ~ poly(age,2) * sex, data=Donner, family=binomial))

Call:  glm(formula = survived ~ poly(age, 2) * sex, family = binomial, 
    data = Donner)

Coefficients:
          (Intercept)          poly(age, 2)1          poly(age, 2)2  
               0.7622               -26.9689               -30.5626  
              sexMale  poly(age, 2)1:sexMale  poly(age, 2)2:sexMale  
              -1.0996                22.7211                28.8976  

Degrees of Freedom: 89 Total (i.e. Null);  84 Residual
Null Deviance:	    124.4 
Residual Deviance: 97.8 	AIC: 109.8
> anova(mod3, test="Chisq")
Analysis of Deviance Table

Model: binomial, link: logit

Response: survived

Terms added sequentially (first to last)


                 Df Deviance Resid. Df Resid. Dev Pr(>Chi)   
NULL                                89    124.366            
poly(age, 2)      2   9.5441        87    114.822 0.008463 **
sex               1   8.0908        86    106.731 0.004449 **
poly(age, 2):sex  2   8.9321        84     97.799 0.011492 * 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> LRstats(glmlist(mod1, mod2, mod3))
Likelihood summary table:
        AIC    BIC LR Chisq Df Pr(>Chisq)  
mod1 117.13 124.63  111.128 87    0.04159 *
mod2 118.73 128.73  110.727 86    0.03755 *
mod3 109.80 124.80   97.799 84    0.14408  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> # plot fitted probabilities from mod2 and mod3
> # idea from: http://www.ling.upenn.edu/~joseff/rstudy/summer2010_ggplot2_intro.html
> library(ggplot2)
> 
> # separate linear fits on age for M/F
> ggplot(Donner, aes(age, survived, color = sex)) +
+   geom_point(position = position_jitter(height = 0.02, width = 0)) +
+   stat_smooth(method = "glm", family = binomial, formula = y ~ x,
+            alpha = 0.2, size=2, aes(fill = sex))
Error: Unknown parameters: family
Execution halted
```
```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘vcd-tutorial.Rnw’
  ...
 $ survived: int  0 1 1 1 1 1 1 1 1 1 ...
 $ death   : POSIXct, format: "1846-12-29" NA ...

> ggplot(Donner, aes(age, survived, color = sex)) + 
+     geom_point(position = position_jitter(height = 0.02, width = 0)) + 
+     stat_smooth(metho .... [TRUNCATED] 

  When sourcing ‘vcd-tutorial.R’:
Error: Unknown parameters: family
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: vcd
Loading required package: grid
Loading required package: gnm
The following objects are masked _by_ .GlobalEnv:

    A, B, C


Error: processing vignette 'vcd-tutorial.Rnw' failed with diagnostics:
 chunk 93 (label = donner3a) 
Error : Unknown parameters: family
Execution halted

```
```
DONE
Status: 2 ERRORs, 2 NOTEs
```

## vdmR (0.1.1)
Maintainer: Tomokazu Fujino <fujino@fwu.ac.jp>

```
checking examples ... ERROR
Running examples in ‘vdmR-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: vcmap
> ### Title: Generate choropleth map with interactive functions
> ### Aliases: vcmap
> 
> ### ** Examples
> 
> data(vsfuk2012)
> shp.path <- file.path(system.file(package="vdmR"), "etc/shapes/kitakyu2012.shp")
> kk2012 <- dplyr::filter(vsfuk2012, CityCode<40110&CityCode>40100)
> vcmap(shp.path, kk2012, "CityCode", "CityCode", "map1", "kk2012")
Error in get("rgeos", envir = .MAPTOOLS_CACHE) : object 'rgeos' not found
Calls: vcmap ... fortify.SpatialPolygonsDataFrame -> <Anonymous> -> rgeosStatus -> get
Execution halted
```
```
checking tests ... ERROR
Running the tests in ‘tests/run-all.R’ failed.
Last 13 lines of output:
  1: withCallingHandlers(eval(code, new_test_environment), error = capture_calls, message = function(c) invokeRestart("muffleMessage"), 
         warning = function(c) invokeRestart("muffleWarning"))
  2: eval(code, new_test_environment)
  3: eval(expr, envir, enclos)
  4: vcmap(shp.path, vsfuk2012, "CityCode", "CityCode", "map01", "vsfuk2012", fill = FertilityRate, 
         ggscale = frcol) at test-vdmR.R:20
  5: ggplot2::fortify(spdf, region = mid)
  6: fortify.SpatialPolygonsDataFrame(spdf, region = mid) at /Users/hadley/Documents/ggplot/ggplot/R/fortify.r:12
  7: maptools::unionSpatialPolygons(cp, attr[, region]) at /Users/hadley/Documents/ggplot/ggplot/R/fortify-spatial.r:33
  8: rgeosStatus()
  9: get("rgeos", envir = .MAPTOOLS_CACHE)
  Error: Test failures
  Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 40-42 (vdmR-vignette.Rnw) 
Error: processing vignette 'vdmR-vignette.Rnw' failed with diagnostics:
unused argument (geom_params = params)
Execution halted

```
```
DONE
Status: 2 ERRORs, 1 NOTE
```

## viridis (0.3.1)
Maintainer: Simon Garnier <garnier@njit.edu>  
Bug reports: https://github.com/sjmgarnier/viridis/issues

__OK__

## vmsbase (2.0)
Maintainer: Lorenzo D'Andrea <support@vmsbase.org>

__OK__

## waffle (0.3.1)
Maintainer: Bob Rudis <bob@rudis.net>  
Bug reports: https://github.com/hrbrmstr/waffle/issues

```
checking whether package ‘waffle’ can be installed ... WARNING
Found the following significant warnings:
  Warning: replacing previous import by ‘grid::arrow’ when loading ‘waffle’
  Warning: replacing previous import by ‘grid::unit’ when loading ‘waffle’
See ‘/private/tmp/RtmpvpynWs/check_cranf5d4a6d9419/waffle.Rcheck/00install.out’ for details.
```
```
DONE
Status: 1 WARNING
```

## walkr (0.3.3)
Maintainer: Andy Yao <andy.yao17@gmail.com>  
Bug reports: https://github.com/andyyao95/walkr/issues

__OK__

## Wats (0.10.1)
Maintainer: Will Beasley <wibeasley@hotmail.com>

```
checking examples ... ERROR
Running examples in ‘Wats-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: CartesianPeriodic
> ### Title: Linear Plot with Periodic Elements
> ### Aliases: CartesianPeriodic
> ### Keywords: Cartesian
> 
> ### ** Examples
> 
> library(Wats) #Load the package
> changeMonth <- base::as.Date("1996-02-15")
> dsLinear <- CountyMonthBirthRate2005Version
> dsLinear <- dsLinear[dsLinear$CountyName=="oklahoma", ]
> dsLinear <- AugmentYearDataWithMonthResolution(dsLinear=dsLinear, dateName="Date")
> hSpread <- function( scores ) { return( quantile(x=scores, probs=c(.25, .75)) ) }
> portfolio <- AnnotateData(
+     dsLinear,
+     dvName = "BirthRate",
+     centerFunction = median,
+     spreadFunction = hSpread
+ )
> 
> CartesianPeriodic(
+   portfolio$dsLinear,
+   portfolio$dsPeriodic,
+   xName = "Date",
+   yName = "BirthRate",
+   stageIDName = "StageID",
+   changePoints = changeMonth,
+   changePointLabels = "Bombing Effect"
+ )
Error: Unknown parameters: x
Execution halted
```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Quitting from lines 103-124 (MbrFigures.Rmd) 
Error: processing vignette 'MbrFigures.Rmd' failed with diagnostics:
Unknown parameters: x
Execution halted

```
```
DONE
Status: 1 ERROR, 1 NOTE
```

## wesanderson (0.3.2)
Maintainer: Karthik Ram <karthik.ram@gmail.com>  
Bug reports: https://github.com/karthik/wesanderson/issues

```
checking DESCRIPTION meta-information ... NOTE
Malformed Description field: should contain one or more complete sentences.
```
```
DONE
Status: 1 NOTE
```

## wikipediatrend (1.1.7)
Maintainer: Peter Meissner <retep.meissner@gmail.com>  
Bug reports: https://github.com/petermeissner/wikipediatrend/issues

```
checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘AnomalyDetection’ ‘BreakoutDetection’
```
```
DONE
Status: 1 NOTE
```

## wppExplorer (1.7-1)
Maintainer: Hana Sevcikova <hanas@uw.edu>

__OK__

## wq (0.4.4)
Maintainer: Alan Jassby <wq@fastmail.net>

```
checking examples ... ERROR
Running examples in ‘wq-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: eofPlot
> ### Title: Plot results of an EOF analysis
> ### Aliases: eofPlot
> ### Keywords: Graphics
> 
> ### ** Examples
> 
> # Create an annual matrix time series
> chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)
> chla1 <- chla1[, 1:12]  # remove stations with missing years
> 
> # eofNum (see examples) suggests n = 1
> e1 <- eof(chla1, n = 1)
> eofPlot(e1, type = 'coef')
> eofPlot(e1, type = 'amp')
Error: geom_hline requires the following missing aesthetics: yintercept
Execution halted
```
```
checking running R code from vignettes ... ERROR
Errors in running code in vignettes:
when running code in ‘wq-package.Rnw’
  ...

$variance
[1] 78.4


> print(eofPlot(e1, type = "amp"))

  When sourcing ‘wq-package.R’:
Error: geom_hline requires the following missing aesthetics: yintercept
Execution halted

```
```
checking re-building of vignette outputs ... NOTE
Error in re-building vignettes:
  ...
Loading required package: zoo

Attaching package: ‘zoo’

The following objects are masked from ‘package:base’:

    as.Date, as.Date.numeric

Warning: Removed 1410 rows containing missing values (geom_point).
Warning in window.default(x, ...) : 'end' value not changed

Error: processing vignette 'wq-package.Rnw' failed with diagnostics:
 chunk 32 
Error : geom_hline requires the following missing aesthetics: yintercept
Execution halted

```
```
DONE
Status: 2 ERRORs, 1 NOTE
```

## x.ent (1.1.2)
Maintainer: Tien T. Phan <phantien84@gmail.com>  
Bug reports: https://github.com/tienpt/x.ent/issues

__OK__

## xgboost (0.4-2)
Maintainer: Tong He <hetong007@gmail.com>  
Bug reports: https://github.com/dmlc/xgboost/issues

__OK__

## xkcd (0.0.4)
Maintainer: Emilio Torres-Manzanera <torres@uniovi.es>

```
checking examples ... ERROR
Running examples in ‘xkcd-Ex.R’ failed
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: xkcdaxis
> ### Title: Plot the axis
> ### Aliases: xkcdaxis
> 
> ### ** Examples
> 
> xrange <- range(mtcars$mpg)
> yrange <- range(mtcars$wt)
> p <- ggplot() +
+      geom_point(aes(mpg, wt), data=mtcars) +
+      xkcdaxis(xrange,yrange)
Error: Unknown parameters: yjitteramount
Execution halted
```
```
DONE
Status: 1 ERROR
```

## XLConnect (0.2-11)
Maintainer: Martin Studer <martin.studer@mirai-solutions.com>  
Bug reports: https://github.com/miraisolutions/xlconnect/issues

```
checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    java        4.1Mb
    unitTests   1.0Mb
```
```
DONE
Status: 1 NOTE
```

## zoo (1.7-12)
Maintainer: Achim Zeileis <Achim.Zeileis@R-project.org>

```
checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘stinepack’
```
```
DONE
Status: 1 NOTE
```

## zooaRch (1.1)
Maintainer: Erik Otarola-Castillo <eotarolacastillo@fas.harvard.edu>

__OK__

