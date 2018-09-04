nations <- read.csv("nations.csv")
library(psycho)
# Correlation matrix
# nations$logGDP <- log(nations$GDP)
# nations$logTFR <- log(nations$TFR)
# nations$logINFMORT <- log(nations$infant.mortality)
# nations$logCONTRACEPT <- log(nations$contraception)
cor <- correlation(df=nations[,c(2:5)])
summary(cor)
# plot(cor)
# corlog <- correlation(df=nations[,c(8:11)])
# summary(corlog)
# plot(corlog)
# plot(nations[,c(2:5)])
# plot(nations$GDP, nations$contraceptions)
# plot(log(nations$GDP), log(nations$contraceptions))
# # plot(log(nations$GDP), log(nations$TFR))
mod_GDP_INFMORT <- lm(nations$infant.mortality ~ nations$GDP)
summary(mod_GDP_INFMORT)
mod_logGDP_INFMORT <- lm(log(nations$infant.mortality) ~ log(nations$GDP))
summary(mod_logGDP_INFMORT)
mod_logGDP_INFMORT$coefficients[2]
summary(mod_logGDP_INFMORT)$sigma

# mod_GDP_CONTRA <- lm(nations$GDP ~ nations$contraception)
# summary(mod_GDP_CONTRA)
# mod_logGDP_CONTRA <- lm(log(nations$GDP) ~ log(nations$contraception))
# summary(mod_logGDP_CONTRA)
# mod_logGDP_CONTRA$coefficients[2]
# summary(mod_logGDP_CONTRA)$sigma

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
                      Y=c("GDP"),
                      domainvalue = "dom")
cv <- as.data.frame(list(DOM=rep("DOM1",1),
                         CV1=rep(0.1,1),
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
framenew <- updateFrame(frame,newstrata)
framenew <- framenew[order(framenew$ID),]
framenew$Y2 <- nations$infant.mortality
results <- evalSolution(framenew, solution1$aggr_strata, 100, cens=NULL, writeFiles = TRUE)
results$coeff_var


modlog <- NULL
modlog$beta[1] <- mod_logGDP_INFMORT$coefficients[2]
modlog$sig2[1] <- summary(mod_logGDP_INFMORT)$sigma
modlog$type[1] <- "loglinear"
modlog$gamma[1] <- 1
modlog <- as.data.frame(modlog)
modlog

model <- NULL
model$beta[1] <- mod_GDP_INFMORT$coefficients[2]
model$sig2[1] <- summary(mod_GDP_INFMORT)$sigma
model$type[1] <- "linear"
model$gamma[1] <- 1
model <- as.data.frame(model)
model

 
strata2 <- buildStrataDF(frame, model = modlog)
set.seed(1234)
# kmean <- KmeansSolution(strata2,cv)
solution2 <- optimizeStrata(cv,
                           strata2,
                           iter = 50,
                           suggestions = KmeansSolution(strata2,cv)
                           )
# sum(solution2$aggr_strata$SOLUZ)

newstrata <- updateStrata(strata2,solution2)
framenew <- updateFrame(frame,newstrata)

# outstrata <-solution2$aggr_strata
# outstrata$SOLUZ <- ceiling(outstrata$SOLUZ)
# sum(outstrata$SOLUZ)
framenew <- framenew[order(framenew$ID),]
framenew$Y2 <- nations$infant.mortality
results <- evalSolution(framenew, solution2$aggr_strata, 50, cens=NULL, writeFiles = TRUE)
results$coeff_var

save.image(file="nations_models 1b.RData")
