**NEWS `[4 March 2015]`**

Release of `ChainLadder 0.2.0`, adding new functions to estimate the one year claims development result.

**NEWS `[20 February 2015]`**

The source code development of `ChainLadder` moved to [GitHub](https://github.com/mages/ChainLadder).

Should you find any issues or bugs with this version, then please drop us a line or add them to our [issues list](https://github.com/mages/ChainLadder/issues).




Please join the [R-SIG-insurance mailinglist](https://stat.ethz.ch/mailman/listinfo/r-sig-insurance),  the Special Interest Group on using R in actuarial science and insurance.
# Overview #
`ChainLadder` is an R package providing methods and models which are typically used in insurance claims reserving, including:

  * Mack chain-ladder, Munich chain-ladder and Bootstrap models
  * General multivariate chain ladder-models
  * Loss development factor fitting and Cape Cod models
  * Generalized linear models
  * One year claims development result functions
  * Utility functions to:
    * convert tables into triangles and triangles into tables
    * convert cumulative into incremental and incremental into cumulative triangles
    * visualise triangles

# Installation #
### From  [CRAN](http://cran.r-project.org/web/packages/ChainLadder/) ###
Start R on your computer and type:
```
 install.packages('ChainLadder')
```
Of course, don't forget the [R Installation and Administration](http://cran.r-project.org/manuals.html) manual.
## From Google Code ##
### R under Windows ###
  * Go to the "Downloads" tab on this site and download the most recent zip-file (Windows binary) to your hard drive.
  * Open R and select from the "Packages" menu at the top of the R console "Install package(s) from local zip files...".
  * Navigate to the folder where you saved the zip-file, select the zip-file and click open.
### R under Linux / Mac OS X ###
  * Go to the "Downloads" tab on this site and download the most recent tar.gz-file (source code) to your hard drive.
  * Open a terminal window and navigate to the folder where you saved the tar.gz-file and type: `R CMD INSTALL ChainLadder_x.x.x.tar.gz` (substitute x.x.x with the version number)
## Using ChainLadder ##
After you installed the package type into R:
```
 library('ChainLadder')
```
Type `library(help='ChainLadder')` or `?ChainLadder`
to see overall documentation.

Type `demo(ChainLadder)` to get an idea of the functionality of this package.

See `demo(package='ChainLadder')` for a list of more demos.
## Documentation ##
  * [Vignette: Claims reserving with ChainLadder in R](http://cran.r-project.org/web/packages/ChainLadder/vignettes/ChainLadder.pdf)
  * [ChainLadder-documentation](http://cran.r-project.org/web/packages/ChainLadder/ChainLadder.pdf)
# Examples #

  * To get an idea of the `ChainLadder` package check out some examples [here](http://code.google.com/p/chainladder/wiki/Examples).
  * Chain ladder age-to-age factors can be regarded as weighted linear regression through the origin, see  [here](http://code.google.com/p/chainladder/wiki/LinerRegression) for the underlying concept.
  * The `ChainLadder` package provides also an example spreadsheet, which shows you how you can access the `ChainLadder` R functions from **Excel** via the [RExcel-Addin](http://cran.r-project.org/web/packages/RExcelInstaller/). After you installed the package you will find the spreadsheet in the ChainLadder library folder. The R command `system.file("Excel", package="ChainLadder")` tells you the path to the Excel-folder; alternatively you can download it [here](http://chainladder.googlecode.com/svn/trunk/inst/Excel/ChainLadder_in_Excel.xls).
  * Using the `rcom` package it is possible to put your R output directly into **PowerPoint** or **Word**, see [here](PowerPoint.md) for an example.

# Presentations #
&lt;wiki:gadget url="http://dl.dropbox.com/u/7586336/ChainLadder\_R\_in\_Finance.xml" frameborder="0" width="410" height="340" /&gt;
  * [Claims reserving in R with ChainLadder, R in Finance, Chicago, 11 May 2012](https://docs.google.com/presentation/d/1BldxefDGgKINSMWm9achuQs6HC0whguVA3btMBviODg/edit)
  * [Bayesian Hierarchical Models in Property-Casualty Insurance](http://www.actuaryzhang.com/seminar/seminar.html)
  * [ChainLadder at the Predictive Modelling Seminar, Institute of Actuaries, November 2010](http://chainladder.googlecode.com/files/ChainLadder_Markus_20010Nov10.pdf)
  * [Reserve variability calculations](http://chainladder.googlecode.com/files/CAS-Spring-Meeting-2010-C21.pdf), CAS spring meeting 2010, San Diego
  * [How to integrate R into MS Office](http://code.google.com/p/chainladder/downloads/detail?name=R_and_MS_Office-MG-20100504.pdf), presentation at the LondonR user group meeting, 4 May 2010
  * [The ChainLadder package, working with databases and MS Office interfaces](http://chainladder.googlecode.com/files/ChainLadder_Markus_20090724.pdf), presentation at the "R you ready?" workshop, Institute of Actuaries, 24 July 2009
  * [The ChainLadder package at the London R user group meeting 31 March 2009](http://chainladder.googlecode.com/files/ChainLadder_at_London_R_User_Group_20090331.pdf)
  * [Introduction to R](http://chainladder.googlecode.com/files/Introduction_to_R_20081203.pdf),  [Loss Reserving with R](http://chainladder.googlecode.com/files/Loss_Reserving_in_R_20081203.pdf)  at the Stochastic Reserving and Modelling Seminar, 2 -- 3 December 2008, Institute of Actuaries, London
  * [Loss Reserving with R](http://chainladder.googlecode.com/files/LossReserving%20with%20R%2020081113.pdf), together with Vincent Goulet and Daniel Murphy at the annual CAS meeting in Seattle November 2008
  * [ChainLadder at R-user conference Dortmund August 2008](http://chainladder.googlecode.com/files/ChainLadderPackage_Dortmund_2008.pdf)
  * [Screen cast: How to install R and Rtools under Windows](http://toolkit.pbwiki.com/BuildingPackages)
# Useful links #
  * [Introduction to R for Actuaries](http://toolkit.pbwiki.com/f/R%20Examples%20for%20Actuaries%20v0.1-1.pdf) by Nigel de Silva
  * [An Interactive Introduction To R](http://www.slideshare.net/dataspora/an-interactive-introduction-to-r-programming-language-for-statistics) by Michael Driscoll and Dan Murphy
  * [cplm](http://code.google.com/p/cplm/) - Monte Carlo EM algorithms and Bayesian methods for fitting Tweedie compound Poisson linear models.
  * [actuar](http://cran.r-project.org/web/packages/actuar/index.html) - Loss distributions modelling, risk theory (including ruin theory), simulation of compound hierarchical models and credibility theory.
  * [fitdistrplus](http://cran.r-project.org/web/packages/fitdistrplus/index.html) - Help to fit of a parametric distribution to non-censored or censored data
  * [lossDev](http://lossdev.r-forge.r-project.org) - A Bayesian time series loss development model. Features include skewed-t distribution with time-varying scale parameter, Reversible Jump MCMC for determining the functional form of the consumption path, and a structural break in this path; by Christopher W. Laws and Frank A. Schmid
  * If have trouble with your IT department to get R on your machine [this document](http://freespace.virgin.net/markus.gesmann/publications/GIROPaper2006.pdf) might help you to put some good arguments forward. The report "An Actuarial Toolkit" was presented at the GIRO convention 2006 in Vienna.
  * The [RToolkit](http://toolkit.pbwiki.com/RToolkit) web page has a lot of examples, tips and tricks with R in an actuarial context.
  * [Here](http://toolkit.pbworks.com/BuildingPackages) is a video showing how to build R packages under MS Windows
  * There is a half hour long video demo about using R via Excel (RExcel) at [http://rcom.univie.ac.at/RExcelDemo/](http://rcom.univie.ac.at/RExcelDemo/)
  * [R-SIG-insurance -- Special Interest Group on using R in actuarial science and insurance ](https://stat.ethz.ch/mailman/listinfo/r-sig-insurance)
  * [favir](http://www.favir.net/): Formatted Actuarial Vignettes in R. FAViR lowers the learning curve of the R environment. It is a series of peer-reviewed Sweave papers that use a consistent style.
  * [mondate:](http://cran.r-project.org/web/packages/mondate/index.html) R packackge to keep track of dates in terms of months

# News #
```
Version 0.2.0 [2015-03-04]
--------------------------

NEW FEATURES

  o New generic function CDR to estimate the one year claims development
    result. S3 methods for the Mack and bootstrap model have been 
    added already:
      - CDR.MackChainLadder to estimate the one year claims 
        development result of the Mack model without tail factor, 
        based on papers by Merz & Wuthrich (2008, 2014)
      - CDR.BootChainLadder to estimate the one year claims 
        development result of the bootstrap model, using ideas and code 
        by Giuseppe Crupi.
        
  o New function tweedieReserve to estimate reserves in a GLM framework,
    including the one year claims development result.

  o Package vignette has new chapter 'One Year Claims Development
    Result'. 

  o New example data MW2008 and MW2014 form the Merz & Wuthrich (2008, 2014) 
    papers
  
Changes

  o Source code development moved from Google Code to GitHub:
    https://github.com/mages/ChainLadder
  
  o as.data.frame.triangle now gives warning message when dev. period 
    is a character  

  o Alessandro Carrato, Giuseppe Crupi and Mario Wuthrich have been 
    added as authors, thanks to their major contribution to code and
    documentation
  
  o Christophe Dutang, Arnaud Lacoume and Arthur Charpentier have been 
    added as contributors, thanks to their feedback, guidance and 
    code contribution   


Version 0.1.9 [2014-12-20]
--------------------------

Changes

  o Updated README and DESCRIPTION file to comply with changes of 
    CRAN policy.



Version 0.1.8 [2014-08-18]
--------------------------

Changes

  o plot.MackChainLadder gained arguments xlab and ylab.
  
Bug Fixes
  
  o BootChainLadder produced warnings for triangles that had static 
    developments when the argument process.distr was set to "od.pois".
  o as.triangle.data.frame didn't work for a data.frame less than three rows
  o Arguments xlab and ylab were not passed through in plot.triangle, when
    lattice=TRUE


Version 0.1.7 [2013-09-28]
--------------------------

Changes
  
  o The glmReserve function currently doesn't allow the parameter var.power
    to be set to NULL, which would have called the cpglm function of the 
    cplm package. The cplm package is due to dependency issues with lme4 
    no longer available via CRAN.


Version 0.1.6 [2013-08-09]
--------------------------

NEW FEATURES

  o A new function, CLFMdelta, finds the value of 
    delta such that the model coefficients resulting from 
    the 'chainladder' function with that value for argument delta are 
    consistent with an input vector of 'selected' age-to-age factors,
    subject to restrictions on the 'selected' factors relative to
    the input 'Triangle'. See the paper 
    "A Family of Chain-Ladder Factor Models for Selected Link Ratios"
    by Bardis, Majidi, Murphy:
    http://www.variancejournal.org/issues/?fa=article&abstrID=6943
  
  o A new 'coef' method returns the age-to-age factor coefficients of 
    the regression models estimated by the 'chainladder' function.
    
  o Exports a function "LRfunction" that calculates a Triangle's 
    link ratio function and can be used to plot the space of 
    "reasonable link ratio selections" per the CLFM paper.
    
Changes

   o Removed some package dependencies in DESCRIPTION and moved them 
     to Imports.


Version 0.1.5-6 [2013-03-16]
----------------------------

NEW FEATURES

  o The list output of the MackChainLadder function now includes
    the parameter risk and process risk breakdowns of the total risk
    estimate for the sum of projected losses across all origin years
    by development age.
  o The Mack Method's recursive parameter risk calculation now enables 
    Dr. Mack's original two-term formula (the default) and optionally
    the three-term formula found in Murphy's 1994 paper and in the 
    2006 paper by Buchwalder, Buhlmann, Merz, and Wuthrich.
  o A few more Mack Method examples.


Version 0.1.5-5 [2013-02-13]
----------------------------

Bug Fixes

  o The phi-scaling factor in BootChainLadder was incorrect. 
    Instead of calculating the number of data items in the upper left
    triangle as n*(n+1)/2, n*(n-1)/2 was used. Thanks to Thomas
    Girodot for reporting this bug.


Version 0.1.5-4 [2012-11-10]
----------------------------

NEW FEATURES

  o The function "getLatestCumulative" adds attributes to 
    the result
    - names = origin (rownames) from the Triangle
    - rowsname = name of row dimension of Triangle
    - colnames = dev (colnames) from Triangle
    - colsname = name of the column dimension of Triangle
    The function has an additional argument, na.values, a 
    vector of values (e.g., zero) that are synonymous with NA 
    when searching for the rightmost non-NA value
  o as.triangle.data.frame now aggregates multiple data.frame
    records when more than one (origin, dev) observation is
    found (the previous version took the first observation).

Changes

   o The vignette has been updated with sections on Multivariate
     chain-ladder, Clark's method and Generalised linear model methods 
   o MunichChainLadder no longer accepts triangles with more rows than
     columns as the function is not laid out for such data sets
     yet. Thanks to Ben Escoto for highlighting this issue. 


Version 0.1.5-3 [2012-08-10]
----------------------------

NEW FEATURES

   o The function "glmReserve" now simulates predictive distributions
     of the loss reserves when bootstrapping is used.
   o "glmReserve" allows the variance function of the compound Poisson 
     distribution to be estimated from the data, using the estimation 
     method provided by the "cplm" package. 
   o We offer a new function "MultiChainLadder2" to fit several commonly 
     used multivariate chain ladder models, which is much easier to use. 

Changes

   o The output from "glmReserve" is made to be of class "glmReserve", instead 
     of class "glm" used in previous versions. 
   o Fix bugs when exposure is included in "glmReserve". Thanks to
     Alessandro Carrato for reporting this bug. 
   o The "mse.method" argument in "glmReserve" supports partial match.
   o Dramatic improvement on the documentation of "MultiChainLadder".
   o Complete the sections of "MultiChainLadder" and "glmReserve" in
     the vignettes.  


Version 0.1.5-2 [2012-03-25]
----------------------------

NEW FEATURES

  o We started writing a vignette. The current version is still draft
    and far from complete. Feedback will be much appreciated.

Changes

  o Removed .Internal call to make ChainLadder compliant with R 2.15.0 
  o Changed argument "t" in plot.triangle to "type" in order to be
    consistent with plot.default

Bug Fixes

  o as.triangle() gave triangles back, with development periods not 
    ordered, when the input data frame had unordered development 
    periods in different units, e.g. dev=c(1,100,10)
    Thanks to Ben Escoto for reporting this issue.


Version 0.1.5-1 [2011-11-12]
----------------------------

Changes

  o Internal changes to plot.MackChainLadder to pass new checks
    introduced by R 2.14.0.
  o Commented out unnecessary creation of 'io' matrix in ClarkCapeCod
    function. Allows for analysis of very large matrices for CapeCod
    without running out of RAM. 'io' matrix is an integral part of
    ClarkLDF, and so remains in that function.
  o plot.clark method
     - Removed "conclusion" stated in QQplot of clark methods.
     - Restore 'par' settings upon exit
     - Slight change to the title
  o Reduced the minimum 'theta' boundary for weibull growth function
  o Added warnings to as.triangle if origin dev. period are not numeric
   

Version 0.1.5-0 [2011-08-29]
----------------------------

NEW FEATURES

  o New function glmReserve, which implements loss reserving models
    within the generalized linear model framework following a paper by
    England P. and Verrall R. (1999) 


Version 0.1.4-4 [2011-03-27]
----------------------------

Changes

 o Minor changes to reflect a more rigours package build process for
   R >= 2.14.0
 o Start up message uses now packageStartupMessage rather than cat to
   allow the message to be suppressed.


Version 0.1.4-3 [2011-01-18]
----------------------------

NEW FEATURES

  o ClarkLDF and ClarkCapeCod functions were reorganized to clarify
    the delivery and presentation of the methods' results
    - Individual components now contain distinct values within
      Clark's methodologies 
    - 'summary' methods produce "reports" that display results in
      the form of typical loss development and Bornhuetter-Ferguson
      exhibits
    - "Table" functions now produce the results as shown in the
      tables on pp. 64, 65 and 68 of Clark's paper
    - A 'vcov' method produces the covariance matrix of the
      estimated parameters
  o An 'ata' function exists to calculate the "age-to-age" development
    factors of a loss "triangle", as well as the simple and volume
    weighted averages


Version 0.1.4-2 [2011-01-03]
----------------------------

BUG FIXES

  o The TruncatedGrowth function value under the Clark Cape Cod method
    was incorrectly printed in the Table68 data.frame when the
    calculations were to be based on the average date of loss (argument
    adol=TRUE). The underlying calculations used the correct adol
    adjustment, only the printed output was incorrect.


Version 0.1.4-1 [2010-12-1]
---------------------------

NEW FEATURES

  o ClarkLDF and ClarkCapeCod functions: additional functionality
	- Clark's methods now work for "one-row triangles" -- i.e.,
	  loss experience from only one origin period
	- Clark's methods work for "phase-shifted" triangles -- i.e.,
	  triangles whose first age does not coincide with the end of
	  the origin period. Example: accident year origin periods with
	  September 30th evaluation dates.

  o A 'vcov' method now exists to produce the covariance matrix of the
    estimated parameters using the approach in Clark's paper

  o Additional values (in lists) returned by Clark's methods:
	- FI = Fisher Information matrix as Clark defines it in his
	  paper (i.e., without the sigma^2 value)
	- dR = the gradient of the reserves function evaluated at the
	  optimal parameter values
	- value = value of the loglikelihood function at the solution
	- counts = number of evaluations of the loglikelihood and its
	  derivative before convergence

  o Fine-tuning of maximum likelihood numerical algorithm's control
    parameters
	- Enable more consistent convergence properties between R's
   	  32-bit and 64-bit environments
  	- Initial starting values for the weibull function were
    	  adjusted for successful convergence across a wider set of
    	  triangles
  	- Upper bounds introduced for "L-BFGS-B" maximum likelihood
	  method to bound weibull away from unity at too early an age

  o If the solution is found at the boundary of the parameter region,
    it is conceivable that a "more optimal" solution might exist if the
    boundary constraints were not as conservative, so a warning is given

BUG FIXES

  o The parameters returned by the methods were the scaled versions;
    they now at their original scales.


  o The loss development factor (LDF) being returned by ClarkCapeCod
    was not documented


Version 0.1.4-0 [2010-11-11]
----------------------------

NEW FEATURES

  o New implementation of the methods in David Clark's "LDF Curve
    Fitting" paper in the 2003 Forum by Daniel Murphy.

	- Includes LDF and CapeCod methods (functions 'ClarkLDF' and
          'ClarkCapeCod', respectively)
	- Programmed to handle loglogistic and weibull growth functions
	- Printing an object returned by the function results in a
          table similar to that on p. 65 of the paper
	- Plotting such an object results in four residual plots,
          including a Q-Q plot with the results of the Shapiro-Wilk
          test


Version 0.1.3-4 [2010-10-19]
----------------------------
   
BUG FIXES

  o 'residuals.MackChainLadder': Zero weights applied to
    MackChainLadder caused an error. Thanks to Ernesto Schirmacher for
    reporting this bug.  


Version 0.1.3-3 [2010-05-16]
----------------------------

NEW FEATURES

  o New multivariate chain ladder function 'MultiChainLadder' by Wayne
    (Yanwei) Zhang  <actuaryzhang@uchicago.edu>
  o New function 'getLatestCumulative' available. It returns for a
    given triangle the most recent values for each origin period.
  o New demos! Type demo(package='ChainLadder') for more information.
  o Demos exist for the following topics: ChainLadder,
    MackChainLadder, DatabaseExamples, MSOffice, MultiChainLadder
  o New SWord example file ChainLadder_SWord_Example.doc, which
    demonstrates how R code snippets can be integrated into a Word
    file.
    The following R command system.file("SWord",
    package="ChainLadder") will show the directory of the file.


USER-VISIBLE CHANGES

  o The examples in MackChainLadder and ChainLadder-package have been
    shortened and demo files have been created instead.
    The examples focus on the syntax of the function calls, while the
    demos give more detailed information on how you might want to use
    the functions in a business context.
   
BUG FIXES

  o 'plot.MunichChainLadder':  The labels of the axis of the residuals
    plots where the mixed up. 
    Thanks to Ben Escoto for reporting this issue.
  o 'estimate.sigma' didn't check for sigma>0 before applying a
    log-linear regression. Thanks to Dan Murphy reporting this bug.


Version 0.1.2-13 [2009-11-24]
-----------------------------

USER-VISIBLE CHANGES

  o 'MackChainLadder' has new argument 'alpha' as an additional
     weighting parameter. As a result, the argument 'weights' is now
     just that, weights should be between 0 and 1.
     The argument 'alpha' describes the different chain ladder
     age-to-age factors:
     The default for alpha for all development periods is 1. See
     Mack's 1999 paper: 
     alpha=1 gives the historical chain ladder age-to-age factors, 
     alpha=0 gives the straight average of the observed individual
      	     development factors and 
     alpha=2 is the result of an ordinary regression with intercept 0. 
  
  o Basic 'chainladder' function now available using linear
    models. See ?chainladder for more information.

  o More examples for 'MackChainLadder' demonstrate how to apply the
    MackChainLadder over several triangles in 'one-line'.

  o 'as.data.frame.triangle' has new argument 'lob' (e.g. line of
    business) which allows to set an additional label column in the
    data frame output.

BUG FIXES

  o 'MackChainLadder':  Latest position of incomplete triangles were
    in some cases not returned correctly. Thanks to Ben Escoto for
    reporting and providing a patch.

  o 'MackChainLadder': 
    - Mack.S.E was not correctly calculated for non-standard chain
      ladder age-to-age factors (e.g. straight averages or ordinary
      regression through the origin) due the missing argument for 'alpha'. 
    - Chain ladder age-to-age factors were always applied to diagonal
      elements to calculate forecasts, although data in sub-diagonal
      triangle could exist. Many thanks to Przemyslaw Sloma for
      reporting those issues. 
  

Version 0.1.2-12 [2009-02-01]
-----------------------------

NEW FEATURES

 o New triangle class with S3 methods for plot, print and conversion
   from triangles to data.frames and vis versa

 o New utility functions 'incr2cum' and 'cum2incr' to convert
   incremental triangles into  cumulative triangles and vis
   versa. Thanks to Chritophe Dutang.
 o New logical argument lattice for plot.MackChainLadder (and
   plot.triangle), which allows to plot developments by origin period
   in separate panels.  

BUG FIXES

  o 'MunichChainLadder': tail factors were not accepted. Thanks to
    Stefan Pohl for reporting this issue.


Version 0.1.2-11 [2009-03-28]
-----------------------------

BUG FIXES

  o 'MackChainLadder': 'F.se'[ultimate] was calculated of the ultimate
    column instead of the latest paid. 


Version 0.1.2-10 [2009-03-27]
-----------------------------

USER-VISIBLE CHANGES

  o 'MackChainLadder' has new arguments 'tail.sigma' and 'tail.se' to
    provide estimates of the variability for a given tail factor.
 
BUG FIXES

  o 'MackChainLadder': calculation of 'Mack.S.E' did not use an
    ultimate sigma factor to estimate 'Mack.S.E' when a tail factor >
    1 was provided (Thanks to Mark Hoffmann for reporting this issue).


Version 0.1.2-9 [2009-02-01]
----------------------------

USER-VISIBLE CHANGES

  o Updated documentation to work with new Rd-file parser 
    (R version >= 2.9.0) 
  o Updated documentation for 'ABC' data (Thanks to Glen Barnett)


Version 0.1.2-8 [2008-11-03]
----------------------------

USER-VISIBLE CHANGES

  o Updated documentation for 'MackChainLadder' (Thanks to Daniel Murphy)


Version 0.1.2-7 [2008-10-24]
----------------------------

USER-VISIBLE CHANGES 

  o 'MackChainLadder' gives two more elements back: 'Mack.ProcessRisk'
    and 'Mack.ParameterRisk' for the process and parameter risk error
    (Thanks to Daniel Murphy)
  o In the summary output of'MackChainLadder' the label 'CV'  changed
    to 'CV(IBNR)' to clarify that we show the coefficient of variance
    of the IBNR.
  o 'MackChainLadder' provides new example plots for CV(IBNR)
    vs. origin period and CV(Ultimate) vs. origin period  
  o Updated documentation


Version 0.1.2-6 [2008-10-14]
----------------------------

USER-VISIBLE CHANGES

  o Updated documentation


Version 0.1.2-5 [2008-10-13]
----------------------------

NEW FEATURES

  o New function 'BootChainLadder', based on papers by England and Verrall, 
    and Barnett and Zehnwirth
  o 'MackChainLadder' and 'MunichChainLadder' allow for tail factors
  o 'MackChainLadder' estimates the overall standard error for the total IBNR	
  o New arguments 'tail' and 'est.sigma' for MackChainLadder, to control 
    the tail factor and the estimation of sigma_{n-1}
  o New arguments 'tailP', 'tailI' and 'est.sigmaP', 'est.sigmaI' for
    'MunichChainLadder', which are passed on to 'MackChainLadder' to control 
    the tail factor and the estimation of sigma_{n-1} for the Paid and 
    Incurred triangle
  o 'Mack-, 'Munich-, and 'BootChainLadder' accept (mxn) matrices with m>=n,
    e.g more accident years than development years
  o New example data sets: 'ABC' (annual run-off triangle of a worker's 
    compensation portfolio of a large company), 'qpaid', 'qincurred' ('made-up'
    data of a quarterly development triangle of annual origin period)
  o Triangles with higher development period frequency (e.g quarterly) than 
    origin period frequency (e.g annual) can be used after being 'blown-up'
    to a common period frequency, see the help of 'qpaid'
  o 'Mack-, 'Munich- and 'BootChainLadder' accept 'blown-up' triangles of 
    higher development period frequency than origin period frequency filled 
    with 'NA', see the help of 'qpaid'

USER-VISIBLE CHANGES

  o summary functions for 'Mack-, 'Munich-, 'BootChainLadder' give all a list 
    back with two elements: 'ByOrigin' and 'Totals'	
  o Change of labels: origin years -> origin period and development years ->
    development origin
  o Coefficient of Variance is abbreviate with 'CV' instead of 'CoV'
  o The example spreadsheet 'ChainLadder_in_Excel.xls' has new examples, 
    including 'BootChainLadder'
  o New greeting message after the R-call 'library(ChainLadder)'
  o Improved documentation

BUG FIXES

  o 'MunichChainLadder': calculation of 'lambdaP' and 'lambdI' was incorrect. 
    Thanks to Beat Huggler for reporting this issue.


Version 0.1.2-4 [2009-09-23]
----------------------------

  o R/BootstrapReserve.R Included all the functions for the BootChainLadder
    function. The BootChainLadder procedure provides a predictive
    distribution of reserves for a cumulative claims development
    triangle.
  o R/BootstrapReserve.R, MackChainLadder.R, MunichChainLadder The summary
    methods for MackChainLadder, MunichChainLadder, BootChainLadder
    give a list back with two elements "ByOrigin" and "Totals"
  o R/zzz.R Included a .onLoad function to produce a little message
    after the ChainLadder package is loaded.
  o Excel/ChainLadder_in_Excel.xls Added new examples for
    BootChainLadder and how to use Rapply to call functions from the
    ChainLadder package.


Version 0.1.2-2 [2008-09-18]
----------------------------

  o R/MackChainLadder.R Included tail factor estimation. The
    function MackChainLadder has a new argument "tail" to either
    estimate the tail factor via a log-linear regression or to set it manually.
  o data/qpaid.RData, qincurred.RData Added examples of quarterly
    development triangles 


Version 0.1.2-0 [2008-09-08]
----------------------------

  o R/MackChainLadder.R Prepared the functions Mack.S.E and 
    Total.Mack.S.E to accept triangles with rows full of NA values.
    This might be useful for non quadratic triangles


Version 0.1.1-5 [2008-05-19]
----------------------------

  o R/MackChainLadder.R Bug fix: Function Mack.S.E did not give F.se back, 
    which is needed by TotalMack.S.E. 
    Many thanks to Florian Leitenstorfer for reporting this issue.


Version 0.1.1-4 [2008-05-16]
----------------------------

  o inst/Excel/ChainLadder_in_Excel.xls uses now dynamic functions 
    and shows how to call 'plot' from Excel 
  o R/MackChainLadderFunctions.R: Changed labels Reserving to IBNR 
    (=Incurred But Not Reported)


Version 0.1.1-3 [2008-02-20]
----------------------------

  o R/MackChainLadderFunctions.R: Mack.S.E checks now which sigma>0 before log
    linear regression of sigma to estimate sigma[n-1]


Version 0.1.1-2 [2008-02-07]
----------------------------

  o R/MackChainLadderFunctions.R: added function
    TotalMack.S.E function to estimate the overall standard error for
    the reserve. MackChainLadder gives now also the
    Total.Mack.S.E. back plus the estimate standard error for all
    individual age-to-age factors F.se.


Version 0.1.1-1 [2007-12-07]
----------------------------

  o First release on CRAN  
```