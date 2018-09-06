## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  fig.align = "centre",
  fig.height = 4,
  message = FALSE,
  warning = FALSE,
  error = FALSE)


## ---- include = F--------------------------------------------------------
# Run for internal use (not shown on slides)
library(SamplingStrata)
load("Nations.RData")

## ---- eval = T-----------------------------------------------------------
data(nations)
head(nations)

## ---- eval = T-----------------------------------------------------------
library(SamplingStrata)
frame <- buildFrameDF(nations,
                      id="Country",
                      X="Country",
                      Y=c("TFR","contraception",
                          "infant.mortality","GDP"),
                      domainvalue = "Continent")

head(frame)

## ---- eval = T-----------------------------------------------------------
strata <- buildStrataDF(frame, progress = FALSE)
head(strata)

## ---- eval = T-----------------------------------------------------------
cv <- as.data.frame(list(DOM=rep("DOM1",5),
                         CV1=rep(0.1,5),
                         CV2=rep(0.1,5),
                         CV3=rep(0.1,5),
                         CV4=rep(0.1,5),
                         domainvalue=c(1:5)
                             ))
cv

## ---- eval = F-----------------------------------------------------------
## solution1 <-
## 	optimizeStrata(
## 	errors = cv ,
## 	strata = strata,
## 	iter = 50,
## 	pops = 20,
## 	suggestions = NULL,
## 	showPlot = FALSE,
## 	writeFiles = FALSE)

## ---- out.width = "400px", echo = FALSE----------------------------------
knitr::include_graphics("images/Rplot_domain5.png")

## ---- eval = T-----------------------------------------------------------
sum(ceiling(solution1$aggr_strata$SOLUZ))	
nrow(solution1$aggr_strata)	


## ---- eval = T-----------------------------------------------------------
kmean <- KmeansSolution(strata, cv, nstrata=NA, showPlot = F)

## ---- out.width = "500px", echo = FALSE----------------------------------
knitr::include_graphics("images/Rplot_kmeans_domain1.png")

## ---- eval = F-----------------------------------------------------------
## solution2 <-
## 	optimizeStrata(
## 	errors = cv ,
## 	strata = strata,
## 	iter = 50,
## 	pops = 20,
## 	suggestions = kmean,
## 	showPlot = FALSE,
## 	writeFiles = FALSE)

## ---- out.width = "400px", echo = FALSE----------------------------------
knitr::include_graphics("images/Rplot_domain5_kmeans.png")

## ---- eval = T-----------------------------------------------------------
sum(ceiling(solution2$aggr_strata$SOLUZ))	
nrow(solution2$aggr_strata)	

## ---- eval = F-----------------------------------------------------------
## newstrata <- updateStrata(strata,solution2)
## framenew <- updateFrame(frame,newstrata)
## results1 <- evalSolution(framenew, solution2$aggr_strata,
##                          200, writeFiles=TRUE)

## ---- eval = T-----------------------------------------------------------
results1$coeff_var

## ---- out.width = "400px", echo = FALSE----------------------------------
knitr::include_graphics("images/cv_rounded.png")

## ---- eval = F-----------------------------------------------------------
## oustrata <- solution2$aggr_strata
## outstrata$SOLUZ <- ceiling(solution2$aggr_strata$SOLUZ)
## results2 <- evalSolution(framenew, outstrata, 200, writeFiles=TRUE)

## ---- eval = T-----------------------------------------------------------
results2$coeff_var

## ---- out.width = "400px", echo = FALSE----------------------------------
knitr::include_graphics("images/cv_ceiling.png")

## ---- eval = T-----------------------------------------------------------
newstrata <- updateStrata(strata, solution2)
framenew <- updateFrame(frame,newstrata)
sample <- selectSample(frame=framenew,outstrata=solution2$aggr_strata)
head(sample)

