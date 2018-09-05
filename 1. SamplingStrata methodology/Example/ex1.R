ex1 <- read.csv2("ex1.csv")
library(SamplingStrata)
ex1$dom <- 1
ex1$Gender <- ifelse(ex1$Gender == "M",1,2)
frame <- buildFrameDF(ex1,
                      id="Id",
                      X=c("Gender","Income_class"),
                      Y="Savings",
                      domainvalue = "dom")
strata <- buildStrataDF(frame,progress=FALSE)
write.table(strata,"strata.csv",sep=";",row.names= F)
