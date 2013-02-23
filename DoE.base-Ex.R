pkgname <- "DoE.base"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('DoE.base')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("SN")
### * SN

flush(stderr()); flush(stdout())

### Name: SN
### Title: Function for the signal-to-noise ratio 10 * log10(mean^2/var)
### Aliases: SN
### Keywords: array design

### ** Examples

x <- rexp(10)
SN(x)
10 * log10(mean(x)^2/var(x))
20 * log10(mean(x)/sd(x))



cleanEx()
nameEx("add.response")
### * add.response

flush(stderr()); flush(stdout())

### Name: add.response
### Title: Function to add response values to an experimental design
### Aliases: add.response
### Keywords: array design

### ** Examples

       plan <- fac.design(nlevels=c(2,3,2,4))
       result <- rnorm(2*3*2*4)
       add.response(plan,response=result)
       ## direct use of rnorm() is also possible, but looks better with 48 
       add.response(plan,response=rnorm(48))
       
   ## Not run: 
##D    
##D        export.design(path="c:/projectA/experiments",plan)
##D        ## open exported file c:/projectA/experiments/plan.html
##D        ##        with Excel
##D        ## carry out the experiment, input data in Excel or elsewhere
##D        ##        store as csv file with the same name (or a different one, just use 
##D        ##        the correct storage name later in R), after deleting 
##D        ##        the legend portion to the right of the data area
##D        ##        (alternatively, input data by typing them in in R (function fix or R-commander)
##D        add.response(design="plan",response="c:/projectA/experiments/plan.csv",
##D                  rdapath="c:/projectA/experiments/plan.rda")
##D        ## plan is the name of the design in the workspace stored in rdapath
##D        ## assuming only responses were typed in
##D        ## should work on your computer regardless of system, 
##D        ##         if you adapt the path names accordingly
##D    
## End(Not run)



cleanEx()
nameEx("class-design-methods")
### * class-design-methods

flush(stderr()); flush(stdout())

### Name: Methods for class design objects
### Title: Methods for class design objects
### Aliases: [.design aggregate.design print.design showData
###   showData.default showData.design summary.design plot.design
###   class-design-methods
### Keywords: array design

### ** Examples

oa12 <- oa.design(nlevels=c(2,2,6))
#### Examples for extractor function
  ## subsetting to half the runs drops all attributes per default
  oa12[1:6,]
  ## keep the attributes (usually not reasonable, but ...)
  oa12[1:6, drop.attr=FALSE]
  ## reshuffling a design
  ## (re-)randomize
  oa12[sample(12),]
  ## add repeated measurements
  oa12[rep(1:12,each=3),]
  ## add a proper replication 
  ## (does not work for blocked designs)
  oa12[c(sample(12),sample(12)),]
  ## subsetting and rbinding to loose also contrasts of factors
  str(rbind(oa12[1:2,],oa12[3:12]))
  ## keeping all non-design-related attributes like the contrasts
  str(undesign(oa12))

#### Examples for plotting designs
  ## plotting a design without response (uses function mosaic from package vcd)
  plot(oa12)
  ## equivalent to mosaic(~A+B+C, oa12)
  ## alternative order:
  mosaic(~C+A+B, oa12)
  ## using the select function: the plots show that the projection for factors 
  ## C, D and E (columns 3, 14 and 15 of the array) is a full factorial, 
  ## while A, D and E does (columns 1, 14, and 15 of the array) do not occur in 
  ## all combinations
  plot(oa.design(L24.2.13.3.1.4.1,nlevels=c(2,2,2,3,4)),select=c("E","D","A"))
  plot(oa.design(L24.2.13.3.1.4.1,nlevels=c(2,2,2,3,4)),select=c("E","D","C"))
  
  ## plotting a design with response
  y=rnorm(12)
  plot(oa12, y)
  ## plot design with a response included
  oa12.r <- add.response(oa12,y)
  plot(oa12.r)
  ## plotting a numeric design (with or without response, 
  ##   does not make statistical sense here, for demo only)
  noa12 <- qua.design(oa12, quantitative="all")
  plot(noa12, y, main="Scatter Plot Matrix")

#### Examples print and summary
  ## rename factors and relabel levels of first two factors
  namen <- c(rep(list(c("current","new")),2),list(""))
  names(namen) <- c("First.Factor", "Second.Factor", "Third.Factor")
  factor.names(oa12) <- namen
  oa12   ### printed with the print method!

  ## add a few variables to oa12
  responses <- cbind(temp=sample(23:34),y1=rexp(12),y2=runif(12))
  oa12 <- add.response(oa12, responses)
  response.names(oa12)
  ## temp (for temperature) is not meant to be a response 
  ## --> drop it from responselist but not from data
  response.names(oa12) <- c("y1","y2")

## print design
  oa12
## look at design-specific summary
  summary(oa12)
## look at data frame style summary instead
  summary.data.frame(oa12)
  
## aggregation examples
  plan <- oa.design(nlevels=c(2,6,2), replications=2, repeat.only=TRUE)
  y <- rnorm(24)
  z <- rexp(24)
  plan <- add.response(plan, cbind(y=y,z=z))
  plan <- reptowide(plan)
  plan.mean <- aggregate(plan)
  plan.mean
  aggregate(plan, response="z")
  aggregate(plan, FUN=sd)
  aggregate(plan, FUN = function(obj) max(obj) - min(obj), postfix="range")
  ## several aggregates: add standard deviations to plan with means
  plan.mean.sd <- aggregate(plan.mean, FUN=sd)
  plan.mean.sd
  response.names(plan.mean.sd)
  ## change response.names element of design.info back to y.mean and z.mean
  ## may be needed for automatic analysis routines that have not been 
  ## created yet
  plan.mean.sd <- aggregate(plan.mean.sd, FUN=mean)
  plan.mean.sd
  response.names(plan.mean.sd)



cleanEx()
nameEx("class-design")
### * class-design

flush(stderr()); flush(stdout())

### Name: Class design and accessors
### Title: Class design and its accessor functions
### Aliases: design undesign redesign desnum desnum<- run.order run.order<-
###   design.info design.info<- factor.names factor.names<- response.names
###   response.names<- col.remove ord
### Keywords: array design

### ** Examples

oa12 <- oa.design(nlevels=c(2,2,6))


#### Examples for factor.names and response.names
  factor.names(oa12)
  ## rename factors
  factor.names(oa12) <- c("First.Factor", "Second.Factor", "Third.Factor")
  ## rename factors and relabel levels of first two factors
  namen <- c(rep(list(c("current","new")),2),list(""))
  names(namen) <- c("First.Factor", "Second.Factor", "Third.Factor")
  factor.names(oa12) <- namen
  oa12

  ## add a few variables to oa12
  responses <- cbind(temp=sample(23:34),y1=rexp(12),y2=runif(12))
  oa12 <- add.response(oa12, responses)
  response.names(oa12)
  ## temp (for temperature) is not meant to be a response 
  ## --> drop it from responselist but not from data
  response.names(oa12) <- c("y1","y2")

## looking at attributes of the design
  desnum(oa12)
  run.order(oa12)
  design.info(oa12)

## undesign and redesign
  u.oa12 <- undesign(oa12)
  str(u.oa12)  
  u.oa12$new <- rnorm(12)
  r.oa12 <- redesign(oa12, u.oa12)
## make known that new is also a response
  response.names(r.oa12) <- c(response.names(r.oa12), "new") 
## look at design-specific summary
  summary(r.oa12)
## look at data frame style summary instead
  summary.data.frame(r.oa12)




cleanEx()
nameEx("contr.FrF2")
### * contr.FrF2

flush(stderr()); flush(stdout())

### Name: contr.FrF2
### Title: Contrasts for orthogonal Fractional Factorial 2-level designs
### Aliases: contr.FrF2
### Keywords: array design

### ** Examples

## assign contr.FrF2 contrasts to a factor
status <- as.factor(rep(c("current","new"),4))
contrasts(status) <- contr.FrF2(2)
contrasts(status)



cleanEx()
nameEx("cross.design")
### * cross.design

flush(stderr()); flush(stdout())

### Name: cross.design
### Title: Function to cross several designs
### Aliases: cross.design
### Keywords: array design

### ** Examples

   ## creating a Taguchi-style inner-outer array design
   ## with proper randomization
   ##   function param.design would generate such a design with all outer array runs 
   ##     for each inner array run conducted in sequence
   ##   alternatively, a split-plot approach can also handle control and noise factor 
   ##     designs without necessarily crossing two separate designs
   des.control <- oa.design(ID=L18)
   des.noise <- oa.design(ID=L4.2.3,nlevels=2,factor.names=c("N1","N2","N3"))
   crossed <- cross.design(des.control, des.noise)
   crossed
   summary(crossed)



cleanEx()
nameEx("export.design")
### * export.design

flush(stderr()); flush(stdout())

### Name: export.design
### Title: Function for exporting a design object
### Aliases: export.design html html.data.frame
### Keywords: array design

### ** Examples

  ## six 2-level factors
  test <- oa.design(nlevels=c(2,3,3,3))
  ## export an html file with legend and two responses
  ## files test.rda and test.html will be written to the current working directory, 
  ##     if they do not exist yet
  export.design(test, response.names=c("pressure", "temperature"))



cleanEx()
nameEx("fac.design")
### * fac.design

flush(stderr()); flush(stdout())

### Name: fac.design
### Title: Function for full factorial designs
### Aliases: fac.design
### Keywords: array design

### ** Examples

  ## only specify level combination 
  fac.design(nlevels=c(4,3,3,2))
  ## design requested via factor.names
  fac.design(factor.names=list(one=c("a","b","c"), two=c(125,275), three=c("old","new"), four=c(-1,1), five=c("min","medium","max")))
  ## design requested via character factor.names and nlevels (with a little German lesson for one two three)
  fac.design(factor.names=c("eins","zwei","drei"),nlevels=c(2,3,2))
  
  ### blocking designs
  fac.design(nlevels=c(2,2,3,3,6), blocks=6, seed=12345)
  ## the same design, now unnecessarily constructed via option block.gen
  ## preparation: look at the numbers of levels of pseudo factors
  ## (in this order)
  unlist(factorize(c(2,2,3,3,6)))
  ## or, for more annotation, factorize the unblocked design
  factorize(fac.design(nlevels=c(2,2,3,3,6)))
  ## positions 1 2 5 are 2-level pseudo factors
  ## positions 3 4 6 are 4-level pseudo factors
  ## blocking with highest possible interactions
  G <- rbind(two=c(1,1,0,0,1,0),three=c(0,0,1,1,0,1))
  plan.6blocks <- fac.design(nlevels=c(2,2,3,3,6), blocks=6, block.gen=G, seed=12345)
  plan.6blocks
  
  ## two blocks, default design, but unnecessarily constructed via block.gen
  fac.design(nlevels=c(2,2,3,3,6), blocks=2, block.gen=c(1,1,0,0,1,0), seed=12345)
  
  ## three blocks, default design, but unnecessarily constructed via block.gen
  fac.design(nlevels=c(2,2,3,3,6), blocks=3, block.gen=c(0,0,1,1,0,1), seed=12345)
  
  ## nine blocks
  ## confounding two-factor interactions cannot be avoided
  ## there are warnings to that effect
  G <- rbind(CD=c(0,0,1,1,0,0),CE2=c(0,0,1,0,0,1))
  plan.9blocks <- fac.design(nlevels=c(2,2,3,3,6), blocks=9, block.gen=G, seed=12345)

  ## further automatic designs 
  fac.design(nlevels=c(2,2,3,3,6), blocks=4, seed=12345)
  fac.design(nlevels=c(2,2,3,3,6), blocks=9, seed=12345)
  fac.design(nlevels=c(2,2,3,3,6), blocks=36, seed=12345)
  fac.design(nlevels=c(3,5,6,10), blocks=15, seed=12345)
  
  ## independently check aliasing
  ## model with block main effects and all two-factor interactions
  ## 6 factors: not aliased
  summary(plan.6blocks)
  alias(lm(1:nrow(plan.6blocks)~Blocks+(A+B+C+D+E)^2,plan.6blocks))
  ## 9 factors: aliased
  summary(plan.9blocks)
  alias(lm(1:nrow(plan.9blocks)~Blocks+(A+B+C+D+E)^2,plan.9blocks))



cleanEx()
nameEx("factorize")
### * factorize

flush(stderr()); flush(stdout())

### Name: factorize
### Title: Factorize integer numbers and factors
### Aliases: factorize.factor factorize.design factorize.data.frame
### Keywords: array design

### ** Examples

factorize(12)
factorize(c(2,2,3,3,6))
factorize(fac.design(nlevels=c(2,2,3,3,6)))
unlist(factorize(c(2,2,3,3,6)))
factorize(undesign(fac.design(nlevels=c(2,2,3,3,6))))



cleanEx()
nameEx("formula.design")
### * formula.design

flush(stderr()); flush(stdout())

### Name: formula.design
### Title: Function to change the default formula for a data frame of class
###   design to involve the correct factors with the desired effects and
###   responses
### Aliases: formula.design
### Keywords: array design

### ** Examples

  ## indirect usage via function lm.design is much more interesting
  ## cf help for lm design!

   my.L18 <- oa.design(ID=L18, 
       factor.names = c("one","two","three","four","five","six","seven"), 
       nlevels=c(3,3,3,2,3,3,3))
   y <- rnorm(18)
   my.L18 <- add.response(my.L18, y)
   formula(my.L18)
   lm(my.L18)



cleanEx()
nameEx("generalized.word.length")
### * generalized.word.length

flush(stderr()); flush(stdout())

### Name: generalized.word.length
### Title: Functions for calculating the generalized word length pattern,
###   projection frequencies or optimizing column selection within an array
### Aliases: generalized.word.length length2 length3 length4 length5
###   lengths contr.XuWu P3.3 P4.4 GR oa.min3 oa.min34 oa.max3 oa.max4
###   oa.maxGR oa.minRelProjAberr nchoosek
### Keywords: design array

### ** Examples

   ## check a small design 
   oa12 <- oa.design(nlevels=c(2,2,6))
   length3(oa12)
   ## length4 is of course 0, because there are only 3 factors
   P3.3(oa12)

   ## the results need not be an integer
   oa12 <- oa.design(L12.2.11,columns=1:6)
   length3(oa12)
   length4(oa12)
   P3.3(oa12)  ## all projections have the same pattern
             ## which is known to be true for the complete L12.2.11 as well
   P3.3(L18)   ## this is the pattern of the Taguchi L18
             ## also published by Schoen 2009
   P3.3(L18[,-2])  ## without the 2nd column (= the 1st 3-level column)
   P3.3(L18[,-2], rela=TRUE)  ## relative pattern, divided by theoretical upper 
                              ## bound for each 3-factor projection
   
   ## choosing among different assignment possibilities
   ## for two 2-level factors and one 3- and 4-level factor each
   show.oas(nlevels=c(2,2,3,4))
   ## default allocation: first two columns for the 2-level factors
   oa24.bad <- oa.design(L24.2.13.3.1.4.1, columns=c(1,2,14,15))
   length3(oa24.bad)
   ## much better: columns 3 and 10
   oa24.good <- oa.design(L24.2.13.3.1.4.1, columns=c(3,10,14,15))
   length3(oa24.good)
   length4(oa24.good)  ## there are several variants, 
                       ## which produce the same pattern for lengths 3 and 4
                       
   ## the difference matters
   plot(oa24.bad, select=c(2,3,4))
   plot(oa24.good, select=c(2,3,4))
   
   ## generalized resolution differs as well (resolution is III in both cases)
   GR(oa24.bad)
   GR(oa24.good)

   ## choices for columns can be explored with functions oa.min3, oa.min34 or oa.max3
   oa.min3(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   oa.min34(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   ## columns for designs with maximum generalized resolution 
   ##    (can take very long, if all designs have worst-case aliasing) 
      ## then optimize these for overall relative number of words of length 3
      ##     and in addition absolute number of words of length 4 
   mGR <- oa.maxGR(L18, c(2,3,3,3,3,3,3))
   oa.minRelProjAberr(L18, c(2,3,3,3,3,3,3), maxGR=mGR)
   
   oa.max3(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))    ## this is not for finding 
                                                    ## a good design!!!
                                                    
   ## Not run: 
##D    ## play with selection of optimum design
##D    ## somewhat experimental at present
##D    oa.min3(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4))
##D    best3 <- oa.min3(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), rela=TRUE)
##D    oa.min34(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4))
##D    oa.min34(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), min3=best3)
##D    
##D    ## generalized resolution according to Groemping 2011, manually
##D    best3GR <- oa.min3(L36.2.11.3.12, c(rep(2,3),rep(3,3)), rela=TRUE, crit="worst")
##D       ## optimum GR is 3.59
##D    ## subsequent optimization w.r.t. rA3
##D    best3reltot.GR <- oa.min3(L36.2.11.3.12, c(rep(2,3),rep(3,3)), rela=TRUE, 
##D            variants=best3GR$column.variants)
##D       ## optimum rA3 is 0.5069
##D    ## (note: different from first optimizing rA3 (0.3611) and then GR (3.5))
##D    ## remaining nine designs: optimize RPFTs
##D    L36 <- oa.design(L36.2.11.3.12, randomize=FALSE)
##D    lapply(1:9, function(obj) P3.3(L36[,best3reltot.GR$column.variants[obj,]]))
##D       ## all identical
##D    oa.min34(L36, nlevels=c(rep(2,3),rep(3,3)), min3=best3reltot.GR)
##D       ## still all identical
##D    
## End(Not run)

   ## select among column variants with projection frequencies 
   ## here, all variants have identical projection frequencies
   ## for larger problems, this may sometimes be relevant
   variants <- oa.min34(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   for (i in 1:nrow(variants$column.variants)){
      cat("variant ", i, "\n")
      print(P3.3(oa.design(L24.2.13.3.1.4.1, columns=variants$column.variants[i,])))
      }
   
   ## automatic optimization is possible, but can be time-consuming
   ## (cf. help for oa.design)
   plan <- oa.design(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4), columns="min3")
   length3(plan)
   length4(plan)
   plan <- oa.design(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4), columns="min34")
   length3(plan)
   length4(plan)

   ## Not run: 
##D    ## blocked design from FrF2
##D    ## the design is of resolution IV
##D    ## there is one (generalized) 4-letter word that does not involve the block factor
##D    ## there are four more 4-letter words involving the block factor
##D    ## all this and more can also be learnt from design.info(plan)
##D    require(FrF2)
##D    plan <- FrF2(32,6,blocks=4)
##D    length3(plan)
##D    length3(plan, with.blocks=TRUE)
##D    length4(plan)
##D    length4(plan, with.blocks=TRUE)
##D    design.info(plan)
##D    
## End(Not run)




cleanEx()
nameEx("getblock")
### * getblock

flush(stderr()); flush(stdout())

### Name: getblock
### Title: Functions to extract a block factor from a class design object
###   or to rerandomize a class design object
### Aliases: getblock rerandomize.design
### Keywords: array design

### ** Examples

   ## a blocked full factorial design
   ff <- fac.design(nlevels=c(2,2,2,3,3,3), blocks=6, bbrep=2, wbrep=2, repeat.only=FALSE)
   getblock(ff)
   getblock(ff, combine=TRUE)
   rerandomize.design(ff)
   ff <- fac.design(nlevels=c(2,2,2,3,3,3), replications=2, repeat.only=FALSE)
   getblock(ff)  
   ff <- fac.design(nlevels=c(2,2,2,3,3,3), replications=2, repeat.only=FALSE)
   try(getblock(ff))



cleanEx()
nameEx("iscube")
### * iscube

flush(stderr()); flush(stdout())

### Name: iscube
### Title: Functions to isolate cube points from 2-level fractional
###   factorial design with center and / or star points
### Aliases: iscube isstar pickcube
### Keywords: design array

### ** Examples

  ## purely technical example, not run because FrF2 not loaded
  ## Not run: 
##D   plan <- FrF2(16,5, factor.names=c("one","two","three","four","five"), ncenter=4)
##D   iscube(plan)
##D   plan2 <- ccd.augment(plan)
##D   iscube(plan2)
##D   isstar(plan2)
##D   pickcube(plan2)
##D   
## End(Not run)
   


cleanEx()
nameEx("lm.design")
### * lm.design

flush(stderr()); flush(stdout())

### Name: lm and aov method for class design objects
### Title: lm and aov methods for class design objects
### Aliases: lm lm.default lm.design coef.lm.design aov aov.default
###   aov.design print.summary.lm.design print.lm.design summary.lm.design
###   print.summary.aov.design print.aov.design summary.aov.design
### Keywords: array design

### ** Examples

  oa12 <- oa.design(nlevels=c(2,2,6))
  ## add a few variables to oa12
  responses <- cbind(y=rexp(12),z=runif(12))
  oa12 <- add.response(oa12, responses)
  ## want treatment contrasts rather than the default
  ## polynomial contrasts for the factors 
  oa12 <- change.contr(oa12, "contr.treatment")
  linmod.y <- lm(oa12)
  linmod.z <- lm(oa12, response="z")
  linmod.y
  linmod.z
  summary(linmod.y)
  summary(linmod.z)
  
## examples with aggregation
  plan <- oa.design(nlevels=c(2,6,2), replications=2, repeat.only=TRUE)
  y <- rnorm(24)
  z <- rexp(24)
  plan <- add.response(plan, cbind(y=y,z=z))
  lm(plan)
  lm(plan, response="z")
  lm(plan, FUN=sd)
  ## wide format
  plan <- reptowide(plan)
  plan
  design.info(plan)$responselist
  ## default: aggregate variables for first column of responselist
  lm(plan)
  ## request z variables instead (z is the column name of response list)
  lm(plan, response="z") 
  ## force analysis of first z measurement only
  lm(plan, response="z.1")
  ## use almost all options 
  ## (option use.center can only be used with center point designs 
  ##          from package FrF2)
  summary(lm(plan, response="z", degree=2, FUN=sd))




cleanEx()
nameEx("oa.design")
### * oa.design

flush(stderr()); flush(stdout())

### Name: oa.design
### Title: Function for accessing orthogonal arrays
### Aliases: oa.design origin oa
### Keywords: array design

### ** Examples

  ## smallest available array for 6 factors with 3 levels each
  oa.design(nfactors=6,nlevels=3)
  ## level combination for which only a full factorial is (currently) found
  oa.design(nlevels=c(4,3,3,2))
  ## array requested via factor.names
  oa.design(factor.names=list(one=c("a","b","c"), two=c(125,275), three=c("old","new"), four=c(-1,1), five=c("min","medium","max")))
  ## array requested via character factor.names and nlevels (with a little German lesson for one two three four five)
  oa.design(factor.names=c("eins","zwei","drei","vier","fuenf"),nlevels=c(2,2,2,3,7))
  ## array requested via explicit name, Taguchi L18
  oa.design(ID=L18)
  ## array requested via explicit name, with column selection
  oa.design(ID=L18.3.6.6.1,columns=c(2,3,7))
  ## array requested with nruns, not very reasonable
  oa.design(nruns=12, nfactors=3, nlevels=2)
  ## array requested with min.residual.df
  oa.design(nfactors=3, nlevels=2, min.residual.df=12)
  
  ## examples showing alias structures and their improvment with option columns
  plan <- oa.design(nfactors=6,nlevels=3)
  plan
     ## generalized word length pattern
     length3(plan)
     ## length3 (first element of GWP) can be slightly improved by columns="min3"
     plan <- oa.design(nfactors=6,nlevels=3,columns="min3")
     summary(plan)  ## the first 3-level column of the array is not used
     length3(plan)
  plan <- oa.design(nlevels=c(2,2,2,6))
     length3(plan)
  plan.opt <- oa.design(nlevels=c(2,2,2,6),columns="min3") ## substantial improvement
     length3(plan.opt)   
     length4(plan.opt)   
  ## visualize practical relevance of improvement:
     ## for optimal plan, all 3-dimensional projections are full factorials
  plot(plan, select=1:3)
  plot(plan, select=c(1,2,4))
  plot(plan, select=c(1,3,4))
  plot(plan, select=2:4)
  plot(plan.opt, select=1:3)
  plot(plan.opt, select=c(1,2,4))
  plot(plan.opt, select=c(1,3,4))
  plot(plan.opt, select=2:4)
  

  ## The last example:
  ## generate an orthogonal array equivalent to Taguchi's L18
  ## by combining L18.3.6.6.1 with a full factorial in 2 and 3 levels
  show.oas(nruns=18, parents.only=FALSE)
       ## lineage entry leads the way:
           ## start from L18.3.6.6.1 
           ## insert L6.2.1.3.1 for the 6 level factor
  ## prepare the parent 
   parent.des <- L18.3.6.6.1
   colnames(parent.des) <- c(Letters[3:8], "comb")   
       ## column comb will create the first two columns of the target design
  ## 6-level design can be created by fac.design or expand.grid
   nest.des <- as.matrix(expand.grid(1:3,1:2))[c(1:3,5,6,4),c(2,1)]  
        ## want first column to change most slowly
        ## want resulting design to be easily transformable into Taguchi L18
        ## see mathematical comments in section Details
   colnames(nest.des) <- c("A","B")
  ## do the expansion (see mathematical comments in section Details)
  L18.2.1.3.7.manual <- cbind(nest.des[parent.des[,"comb"],], parent.des)[,-9]
  L18.2.1.3.7.manual <- L18.2.1.3.7.manual[ord(L18.2.1.3.7.manual),]  ## sort array
      rownames(L18.2.1.3.7.manual) <- 1:18
        ## (ordering is not necessary, just **tidy**)
  ## prepare for using it with function oa.design
  attr(L18.2.1.3.7.manual, "origin") <- 
      c(show.oas(name="L18.2.1.3.7", parents.only=FALSE,show=0)$lineage, 
        "unconventional order")
  class(L18.2.1.3.7.manual) <- c("oa", "matrix")
  comment(L18.2.1.3.7.manual) <- "Interaction of first two factors estimable"
     ## indicates that first two factors are full factorial from 6-level factor
  origin(L18.2.1.3.7.manual)
  comment(L18.2.1.3.7.manual)
  L18  ## Taguchi array
  L18.2.1.3.7.manual  ## manually expanded array
  oa.design(L18.2.1.3.7, randomize=FALSE)
        ## automatically expanded array
  P3.3(L18.2.1.3.7.manual)  ## length 3 pattern of 3 factor projections
                  ## this also identifies the array as isomorphic to L18
                  ## according to Schoen 2009
  ## the array can now be used in oa.design, like the built-in arrays
  oa.design(ID=L18.2.1.3.7.manual,nfactors=7,nlevels=3)



cleanEx()
nameEx("oacat")
### * oacat

flush(stderr()); flush(stdout())

### Name: oacat
### Title: data frame that lists available orthogonal arrays, mostly from
###   the Kuhfeld collection
### Aliases: oacat
### Keywords: array design

### ** Examples

   head(oacat)



cleanEx()
nameEx("param.design")
### * param.design

flush(stderr()); flush(stdout())

### Name: param.design
### Title: Function to generate Taguchi style parameter designs
### Aliases: param.design paramtowide
### Keywords: design array

### ** Examples

## It is recommended to use param.design particularly with FrF2 designs. 
## For the examples to run without package FrF2 loaded, 
## oa.design designs are used here.

## quick preliminary checks to try out possibilities
control <- oa.design(L18, columns=1:4, factor.names=paste("C",1:4,sep=""))
noise <- oa.design(L4.2.3, columns=1:3, factor.names=paste("N",1:3,sep=""))
## long
long <- param.design(control,noise)
## wide
wide <- param.design(control,noise,direction="wide")
wide
long

## use proper labelled factors
## should of course be as meaningful as possible for your data
fnc <- c(list(c("current","new")),rep(list(c("type1", "type2","type3")),3))
names(fnc) <- paste("C", 1:4, sep="")
control <- oa.design(L18, factor.names=fnc)
fnn <- rep(list(c("low","high")),3)
names(fnn) <- paste("N",1:3,sep="")
noise <- oa.design(L4.2.3, factor.names = fnn)
ex.inner.outer <- param.design(control,noise,direction="wide",responses=c("force","yield"))
ex.inner.outer              
## export e.g. to Excel or other program with which editing is more convenient
## Not run: 
##D    ### design written to default path as html and rda by export.design
##D    ### html can be opened with Excel
##D    ### data can be typed in 
##D    ### for preparation of loading back into R, 
##D    ###     remove all legend-like comment that does not belong to the data table itself
##D    ###     and store as csv
##D    ### reimport into R using add.response
##D    ### (InDec and OutDec are for working with German settings csv 
##D    ###     in an R with standard OutDec, i.e. wrong default option)
##D    getwd()  ## look at default path, works on most systems
##D    export.design(ex.inner.outer, OutDec=",")
##D    add.response("ex.inner.outer", "ex.inner.outer.csv", "ex.inner.outer.rda", InDec=",")
## End(Not run)



cleanEx()
nameEx("qua.design")
### * qua.design

flush(stderr()); flush(stdout())

### Name: qua.design
### Title: Function to switch between qualitative and quantitative factors
###   and different contrast settings
### Aliases: qua.design change.contr
### Keywords: design array

### ** Examples

## usage with all factors treated alike
y <- rnorm(12)
plan <- oa.design(nlevels=c(2,6,2))
lm(y~.,plan)
lm(y~., change.contr(plan))   ## with treatment contrasts instead
plan <- qua.design(plan, quantitative = "none")
lm(y~.,plan)
plan <- qua.design(plan, quantitative = "none", contrasts=c(B="contr.treatment"))
lm(y~.,plan)
plan <- qua.design(plan, quantitative = "none")
lm(y~.,plan)

plan <- qua.design(plan, quantitative = "all")
lm(y~.,plan)
plan <- qua.design(plan)  ## NA resets to default state
lm(y~.,plan)

## usage with individual factors treated differently
plan <- oa.design(factor.names = list(liquid=c("type1","type2"), 
     dose=c(0,10,50,100,200,500), temperature=c(10,15)))
str(undesign(plan))
## Not run: 
##D ## would cause an error, since liquid is character and cannot be reasonably coerced to numeric
##D plan <- qua.design(plan, quantitative = "all")
## End(Not run)
plan <- qua.design(plan, quantitative = "none")
str(undesign(plan))

plan <- qua.design(plan, quantitative = c(dose=TRUE,temperature=TRUE))
str(undesign(plan))
## reset all factors to default
plan <- qua.design(plan, quantitative = NA)
str(undesign(plan))
desnum(plan)
## add a response
y <- rnorm(12)
plan <- add.response(plan,y)
## set dose to treatment contrasts
plan <- qua.design(plan, quantitative = c(dose=FALSE), contrasts=c(dose="contr.treatment"))
str(undesign(plan))
desnum(plan)



cleanEx()
nameEx("represhape")
### * represhape

flush(stderr()); flush(stdout())

### Name: Reshape designs with repeated measurements
### Title: Reshape designs with repeated measurements
### Aliases: reptowide reptolong
### Keywords: array design

### ** Examples

    ### design without response data
    ### response variable y is added per default
    plan <- oa.design(nlevels=c(2,6,2), replication=2, repeat.only=TRUE)
    pw <- reptowide(plan)  ## make wide
    pl <- reptolong(pw)  ## make long again
    
    ### design with response and further data
    y <- rexp(24)
    temp <- rep(sample(19:30),each=2)  ## constant covariable
    prot.id <- factor(Letters[1:24]) ## non-constant character covariable
    plan.2 <- add.response(plan, y)
    plan.2$temp <- temp     ## not response
    plan.2$prot.id <- prot.id   ##not response
    plan.2
    reptowide(plan.2, constant="temp")



cleanEx()
nameEx("show.oas")
### * show.oas

flush(stderr()); flush(stdout())

### Name: show.oas
### Title: Function to display list of available orthogonal arrays
### Aliases: show.oas
### Keywords: array design

### ** Examples

   ## the first 10 orthogonal arrays with 24 to 28 runs
   show.oas(nruns = c(24,28))
   ## the first 10 orthogonal arrays with 24 to 28 runs
   ## excluding child arrays
   show.oas(nruns = c(24,28), parents.only=TRUE)
   ## the orthogonal arrays with 4 2-level factors, one 4-level factor and one 5-level factor
   show.oas(factors = list(nlevels=c(2,4,5),number=c(4,1,1)))
   ## the orthogonal arrays with 4 2-level factors, one 7-level factor and one 5-level factor
   show.oas(factors = list(nlevels=c(2,7,5),number=c(4,1,1)))
   ## the latter orthogonal arrays with the nlevels notation 
   ## (that can also be used in a call to oa.design subsequently)
   show.oas(nlevels = c(2,7,2,2,5,2))
   ## calling designs by name 
   show.oas(name=c("L12.2.11", "L18.2.1.3.7"))



cleanEx()
nameEx("utilities")
### * utilities

flush(stderr()); flush(stdout())

### Name: utilities
### Title: Utility functions for DoE packages, not intended for direct use
### Aliases: make.formulas make.generators des.recode Letters printBy
###   gen.fun generators generators.default generators.design
###   generators.catlg PFTs.from.variants matrix.fromPFTs rankPFT bestPFT
### Keywords: array design internal

### ** Examples

## default factor names for most design generating functions 
## (some quantitative designs have other default factor names)
## for up to 50 factors
Letters
## Not run: 
##D generators("7-2.2")
##D generators(catlg[2:8])
##D generators(FrF2(16,7))
##D generators(FrF2(16,5,blocks=4,alias.block.2fi=TRUE))
##D generators(FrF2(16,5,WPs=4,nfac.WP=2))
## End(Not run)
## column selections from L18 with one 2-level and six 3-level factors
v <- rbind(1:7, c(1:6,8), c(1:5,7:8), c(1:4,6:8), c(1:3,5:8), c(1:2,4:8), c(1,3:8))
## RPFTs
RPFTs <- DoE.base:::PFTs.from.variants(L18, v, rela=TRUE)
rpfts <- DoE.base:::matrix.fromPFTs(RPFTs)
rpfts
DoE.base:::rankPFT(rpfts)
DoE.base:::bestPFT(rpfts)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
