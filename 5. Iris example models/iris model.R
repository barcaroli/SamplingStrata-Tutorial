data(iris)
library(psycho)
# Correlation matrix
cor <- correlation(df=iris[,c(1:4)])
summary(cor)
iris$id <- c(1:nrow(iris))
iris$dom <- rep(1,nrow(iris))
cv <- as.data.frame(list(DOM=rep("DOM1",1),
                         CV1=rep(0.05,1),
                         domainvalue=c(1:1)))
cv

frame <- buildFrameDF(iris,
                      id="id",
                      X=c("id"),
                      Y=c("Sepal.Width"),
                      domainvalue = "dom")
strata1 <- buildStrataDF(frame, model = NULL, progress = TRUE)


# kmean <- KmeansSolution(strata1,cv,maxclusters = 10)
solution1 <- optimizeStrata(
  cv , 
  strata1 , 
  iter = 50,
  pops = 20,
  suggestions = KmeansSolution(strata1,cv,maxclusters = 10),
  showPlot = TRUE
)
newstrata <- updateStrata(strata1,solution1)
framenew1 <- updateFrame(frame,newstrata)
framenew1 <- framenew1[order(framenew1$ID),]
framenew1$Y1 <- iris$Petal.Width
framenew1$Y2 <- iris$Petal.Length
framenew1$Y3 <- iris$Sepal.Width
framenew1$Y4 <- iris$Sepal.Length
results1 <- evalSolution(framenew1, solution1$aggr_strata, 50, cens=NULL, writeFiles = TRUE)
results1$coeff_var

# plot(iris[,c(1:4)])
modlin <- lm(iris$Petal.Length ~ iris$Sepal.Width)
summary(modlin)

model <- NULL
model$beta.lin[1] <- mean(iris$Petal.Width/iris$Sepal.Width)
model$sig2.lin[1] <- var(iris$Petal.Width/iris$Sepal.Width)
model$gamma[1] <- 1
model$type <- "linear"
model <- as.data.frame(model)
model

model <- NULL
model$beta[1] <- modlin$coefficients[2]
model$sig2[1] <- summary(modlin)$sigma
model$gamma[1] <- 1
model$type <- "linear"
model <- as.data.frame(model)
model


strata2 <- buildStrataDF(frame, model = model, progress = TRUE)
set.seed(1234)
# kmean <- KmeansSolution(strata2,cv,maxclusters = 10)
solution2 <- optimizeStrata(
  cv , 
  strata2 , 
  iter = 50,
  pops = 20,
  suggestions = kmean <- KmeansSolution(strata2,cv,maxclusters = 10),
  showPlot = TRUE
)

newstrata <- updateStrata(strata2,solution2)
framenew2 <- updateFrame(frame,newstrata)
framenew2 <- framenew2[order(framenew2$ID),]
framenew2$Y1 <- iris$Petal.Width
framenew2$Y2 <- iris$Petal.Length
framenew2$Y3 <- iris$Sepal.Width
framenew2$Y4 <- iris$Sepal.Length
results2 <- evalSolution(framenew2, solution2$aggr_strata, 200, cens=NULL, writeFiles = TRUE)
results2$coeff_var

save.image(file="iris models.RData")
