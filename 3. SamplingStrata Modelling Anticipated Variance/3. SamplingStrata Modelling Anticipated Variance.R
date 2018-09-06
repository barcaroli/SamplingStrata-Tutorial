## ---- include = F--------------------------------------------------------
library(SamplingStrata)

## ---- eval = T-----------------------------------------------------------
data(nations)
head(nations)

## ---- eval = T-----------------------------------------------------------
set.seed(1234)
nations_sample <- nations[sample(c(1:207),70),]

## ---- eval = T-----------------------------------------------------------
mod_logGDP_INFMORT <- lm(log(nations_sample$infant.mortality) ~ log(nations_sample$GDP))
summary(mod_logGDP_INFMORT)

## ---- eval = T-----------------------------------------------------------
mod_logGDP_CONTRA <- lm(log(nations_sample$contraception) ~ log(nations_sample$GDP))
summary(mod_logGDP_CONTRA)

## ---- eval = T-----------------------------------------------------------
nations$progr <- c(1:nrow(nations))
nations$dom <- 1
frame <- buildFrameDF(nations,
                      id="Country",
                      X="progr",
                      Y=c("GDP","GDP"),
                      domainvalue = "dom")

## ---- eval = T-----------------------------------------------------------
cv <- as.data.frame(list(DOM=rep("DOM1",1),
                         CV1=rep(0.10,1),
                         CV2=rep(0.05,1),
                         domainvalue=c(1:1)
                    ))
cv

## ---- eval = T-----------------------------------------------------------
strata1 <- buildStrataDF(frame, progress = FALSE)
solution1 <- optimizeStrata(cv,
                           strata1,
                           iter = 50,
                           suggestions = KmeansSolution(strata1,cv),
                           writeFiles = TRUE,
                           showPlot = FALSE)
sum(solution1$aggr_strata$SOLUZ)

## ---- eval = T-----------------------------------------------------------
newstrata <- updateStrata(strata1,solution1)
framenew1 <- updateFrame(frame,newstrata)
framenew1 <- framenew1[order(framenew1$ID),]
framenew1$Y2 <- nations$infant.mortality
framenew1$Y3 <- nations$contraception
results1 <- evalSolution(framenew1, solution1$aggr_strata, 50, progress = FALSE)
results1$coeff_var

## ---- eval = T-----------------------------------------------------------
model <- NULL
model$beta[1] <- mod_logGDP_INFMORT$coefficients[2]
model$sig2[1] <- summary(mod_logGDP_INFMORT)$sigma
model$type[1] <- "loglinear"
model$gamma[1] <- 0
model$beta[2] <- mod_logGDP_CONTRA$coefficients[2]
model$sig2[2] <- summary(mod_logGDP_CONTRA)$sigma
model$type[2] <- "loglinear"
model$gamma[2] <- 0
model <- as.data.frame(model)
model

## ---- eval = T-----------------------------------------------------------
strata2 <- buildStrataDF(frame, model = model, progress = FALSE)
head(strata2)

## ---- eval = T-----------------------------------------------------------
strata2 <- buildStrataDF(frame, model = model, progress = FALSE)
solution2 <-
  optimizeStrata(
    errors = cv , 
    strata = strata2, 
    iter = 20, 
    pops = 20, 
    suggestions = KmeansSolution(strata2,cv),
    showPlot = FALSE,
    writeFiles = TRUE)

## ---- eval = T-----------------------------------------------------------
newstrata <- updateStrata(strata2,solution2)
framenew2 <- updateFrame(frame,newstrata)
framenew2 <- framenew2[order(framenew2$ID),]
framenew2$Y2 <- nations$infant.mortality
framenew2$Y3 <- nations$contraception
results2 <- evalSolution(framenew2, solution2$aggr_strata, 50, progress = FALSE)
results2$coeff_var

## ---- eval = T-----------------------------------------------------------
adjustedStrata <- adjustSize(size=45,solution2$aggr_strata)
results2 <- evalSolution(framenew2, adjustedStrata, 200, progress=FALSE)
results2$coeff_var

