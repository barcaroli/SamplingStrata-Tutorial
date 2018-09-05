nations <- read.csv("nations.csv")
library(psycho)
# Correlation matrix
# nations$logGDP <- log(nations$GDP)
# nations$logTFR <- log(nations$TFR)
# nations$logINFMORT <- log(nations$infant.mortality)
# nations$logCONTRACEPT <- log(nations$contraception)
cor <- correlation(df=nations[,c(2:5)])
summary(cor)
plot(cor)
# corlog <- correlation(df=nations[,c(8:11)])
# summary(corlog)
# plot(corlog)
# plot(nations[,c(2:5)])
plot(nations$GDP, nations$contraception)
plot(log(nations$GDP), log(nations$contraception))
plot(nations$GDP, nations$infant.mortality)
plot(log(nations$GDP), log(nations$infant.mortality))
# mod_GDP_INFMORT <- lm(nations$GDP ~ nations$infant.mortality)
# summary(mod_GDP_INFMORT)
set.seed(1234)
nations_sample <- nations[sample(c(1:207),70),]
modGDP_INFMORT <- lm(nations_sample$infant.mortality ~ nations_sample$GDP)
summary(modGDP_INFMORT)
mod_logGDP_INFMORT <- lm(log(nations_sample$infant.mortality) ~ log(nations_sample$GDP))
summary(mod_logGDP_INFMORT)
mod_logGDP_INFMORT$coefficients[2]
summary(mod_logGDP_INFMORT)$sigma

mod_GDP_CONTRA <- lm(nations_sample$contraception ~ nations_sample$GDP)
summary(mod_GDP_CONTRA)
mod_logGDP_CONTRA <- lm(log(nations_sample$contraception) ~ log(nations_sample$GDP))
summary(mod_logGDP_CONTRA)
mod_logGDP_CONTRA$coefficients[2]
summary(mod_logGDP_CONTRA)$sigma

# plot(nations$GDP, nations$infant.mortality)
# plot(log(nations$GDP), log(nations$infant.mortality))
# plot(nations$GDP, nations$contraception)
# plot(log(nations$GDP), log(nations$contraception))

library(SamplingStrata)
nations$progr <- c(1:nrow(nations))
nations$dom <- 1
frame <- buildFrameDF(nations,
                      id="Country",
                      X="progr",
                      Y=c("GDP","GDP"),
                      domainvalue = "dom")
cv <- as.data.frame(list(DOM=rep("DOM1",1),
                         CV1=rep(0.10,1),
                         CV2=rep(0.05,1),
                         domainvalue=c(1:1)
                    ))
cv
strata1 <- buildStrataDF(frame)
checkInput(cv,strata1,frame)
set.seed(1234)
# kmean <- KmeansSolution(strata1,cv)
solution1 <- optimizeStrata(cv,
                           strata1,
                           iter = 50,
                           suggestions = KmeansSolution(strata1,cv))
sum(solution1$aggr_strata$SOLUZ)

newstrata <- updateStrata(strata1,solution1)
framenew1 <- updateFrame(frame,newstrata)
framenew1 <- framenew1[order(framenew1$ID),]
framenew1$Y2 <- nations$infant.mortality
framenew1$Y3 <- nations$contraception
results1 <- evalSolution(framenew1, solution1$aggr_strata, 50, cens=NULL, writeFiles = TRUE)
results1$coeff_var


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
# nations$log_GDP <- log(nations$GDP)
# frame <- buildFrameDF(nations,
#                       id="Country",
#                       X="progr",
#                       Y="logGDP",
#                       domainvalue = "dom")
strata2 <- buildStrataDF(frame, model = model)
set.seed(1234)
# kmean <- KmeansSolution(strata2,cv)
solution2 <- optimizeStrata(cv,
                           strata2,
                           iter = 50,
                           suggestions = KmeansSolution(strata2,cv))
sum(solution2$aggr_strata$SOLUZ)

newstrata <- updateStrata(strata2,solution2)
framenew2 <- updateFrame(frame,newstrata)

# outstrata <-solution2$aggr_strata
# outstrata$SOLUZ <- ceiling(outstrata$SOLUZ)
# sum(outstrata$SOLUZ)
framenew2 <- framenew2[order(framenew2$ID),]
framenew2$Y2 <- nations$infant.mortality
framenew2$Y3 <- nations$contraception
# results2 <- evalSolution(framenew2, outstrata, 200, progress=FALSE)
results2 <- evalSolution(framenew2, solution2$aggr_strata, 200, progress=FALSE)
results2$coeff_var

adjustedStrata <- adjustSize(size=45,strata=solution2$aggr_strata)
results2 <- evalSolution(framenew2, adjustedStrata, 200, progress=FALSE)
results2$coeff_var
save.image(file="nations_models.RData")
