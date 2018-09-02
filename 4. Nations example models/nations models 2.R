nations <- read.csv("nations.csv")
library(psycho)
# Correlation matrix
nations$logGDP <- log(nations$GDP)
nations$logTFR <- log(nations$TFR)
nations$logINFMORT <- log(nations$infant.mortality)
nations$logCONTRACEPT <- log(nations$contraception)
cor <- correlation(df=nations[,c(2:5)])
summary(cor)
plot(cor)
corlog <- correlation(df=nations[,c(8:11)])
summary(corlog)
plot(corlog)
# plot(nations[,c(2:5)])
plot(nations$GDP, nations$contraceptions)
plot(log(nations$GDP), log(nations$contraceptions))
# plot(log(nations$GDP), log(nations$TFR))
# mod_GDP_INFMORT <- lm(nations$GDP ~ nations$infant.mortality)
# summary(mod_GDP_INFMORT)
mod_logGDP_INFMORT <- lm(log(nations$GDP) ~ log(nations$infant.mortality))
summary(mod_logGDP_INFMORT)
mod_logGDP_INFMORT$coefficients[2]
summary(mod_logGDP_INFMORT)$sigma

mod_GDP_CONTRA <- lm(nations$GDP ~ nations$contraception)
summary(mod_GDP_CONTRA)
mod_logGDP_CONTRA <- lm(log(nations$GDP) ~ log(nations$contraception))
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
                         CV1=rep(0.08,1),
                         CV2=rep(0.05,1),
                         domainvalue=c(1:1)
                    ))
cv
strata1 <- buildStrataDF(frame)
checkInput(cv,strata1,frame)
set.seed(1234)
kmean <- KmeansSolution(strata1,cv)
solution1 <- optimizeStrata(cv,
                           strata1,
                           iter = 50,
                           suggestions = kmean)
sum(solution1$aggr_strata$SOLUZ)

newstrata <- updateStrata(strata1,solution1)
framenew <- updateFrame(frame,newstrata)
framenew <- framenew[order(framenew$ID),]
framenew$Y2 <- nations$infant.mortality
framenew$Y3 <- nations$contraception
results <- evalSolution(framenew, solution1$aggr_strata, 100, cens=NULL, writeFiles = TRUE)
results$coeff_var


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
kmean <- KmeansSolution(strata2,cv)
solution2 <- optimizeStrata(cv,
                           strata2,
                           iter = 50,
                           suggestions = kmean)
sum(solution2$aggr_strata$SOLUZ)

newstrata <- updateStrata(strata2,solution2)
framenew <- updateFrame(frame,newstrata)

outstrata <-solution2$aggr_strata
outstrata$SOLUZ <- ceiling(outstrata$SOLUZ)
sum(outstrata$SOLUZ)
framenew <- framenew[order(framenew$ID),]
framenew$Y2 <- nations$infant.mortality
framenew$Y3 <- nations$contraception
results <- evalSolution(framenew, outstrata, 100, cens=NULL, writeFiles = TRUE)
results$coeff_var

save.image(file="nations_models.RData")
