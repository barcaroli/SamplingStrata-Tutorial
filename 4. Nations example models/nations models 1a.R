nations <- read.csv("nations.csv")
library(psycho)
# Correlation matrix
# nations$logGDP <- log(nations$GDP)
# nations$logTFR <- log(nations$TFR)
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
mod_GDP_CONTRA <- lm(nations$contraception ~ nations$GDP)
summary(mod_GDP_CONTRA)
mod_logGDP_CONTRA <- lm(log(nations$contraception) ~ log(nations$GDP))
summary(mod_logGDP_CONTRA)
mod_logGDP_CONTRA$coefficients[2]
summary(mod_logGDP_CONTRA)$sigma

# mod_GDP_CONTRA <- lm(nations$GDP ~ nations$contraception)
# summary(mod_GDP_CONTRA)
# mod_logGDP_CONTRA <- lm(log(nations$GDP) ~ log(nations$contraception))
# summary(mod_logGDP_CONTRA)
# mod_logGDP_CONTRA$coefficients[2]
# summary(mod_logGDP_CONTRA)$sigma

# plot(nations$GDP, nations$contraception)
# plot(log(nations$GDP), log(nations$contraception))
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
                           mut_chance = 0.005,
                           suggestions = KmeansSolution(strata1,cv))
sum(solution1$aggr_strata$SOLUZ)

newstrata <- updateStrata(strata1,solution1)
framenew1 <- updateFrame(frame,newstrata)
framenew1 <- framenew1[order(framenew1$ID),]
framenew1$Y2 <- nations$contraception
results1 <- evalSolution(framenew1, solution1$aggr_strata, 100)
results1$coeff_var


modlog <- NULL
modlog$beta[1] <- mod_logGDP_CONTRA$coefficients[2]
modlog$sig2[1] <- summary(mod_logGDP_CONTRA)$sigma
modlog$type[1] <- "loglinear"
modlog$gamma[1] <- 1
modlog <- as.data.frame(modlog)
modlog

modlin <- NULL
modlin$beta[1] <- mod_GDP_CONTRA$coefficients[2]
modlin$sig2[1] <- summary(mod_GDP_CONTRA)$sigma
modlin$type[1] <- "linear"
modlin$gamma[1] <- 0
modlin <- as.data.frame(modlin)
modlin

modlin2 <- NULL
modlin2$beta[1] <- mean(nations$contraception/nations$GDP)
modlin2$sig2[1] <- var(nations$contraception/nations$GDP)
modlin2$type[1] <- "linear"
modlin2$gamma[1] <- 1
modlin2 <- as.data.frame(modlin2)
modlin2


strata2 <- buildStrataDF(frame, model = modlin)
set.seed(4321)
# kmean <- KmeansSolution(strata2,cv)
solution2 <- optimizeStrata(cv,
                           strata2,
                           minnumstr = 1,
                           iter = 50,
                           mut_chance = 0.005
                           ,
                           suggestions = KmeansSolution(strata2,cv,minnumstrat = 1)
                           )
sum(solution2$aggr_strata$SOLUZ)

newstrata <- updateStrata(strata2,solution2)
framenew2 <- updateFrame(frame,newstrata)

# outstrata <-solution2$aggr_strata
# outstrata$SOLUZ <- ceiling(outstrata$SOLUZ)
# sum(outstrata$SOLUZ)
framenew2 <- framenew2[order(framenew2$ID),]
framenew2$Y2 <- nations$contraception
set.seed(1234)
results2 <- evalSolution(framenew2, solution2$aggr_strata, 500, cens=NULL, writeFiles = TRUE)
results2$coeff_var

save.image(file="nations_models 1a.RData")
