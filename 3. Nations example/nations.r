library(SamplingStrata)
# Building the frame
nations <- read.csv("nations.csv")
frame <- buildFrameDF(nations,id="Country",X="Country",
                      Y=c("TFR","contraception","infant.mortality","GDP"),
                      domainvalue = "Continent")

head(frame)

strata <- buildStrataDF(frame)
head(strata)

# Precision constraints
cv <- as.data.frame(list(DOM=rep("DOM1",5),
                         CV1=rep(0.1,5),
                         CV2=rep(0.1,5),
                         CV3=rep(0.1,5),
                         CV4=rep(0.1,5),
                         domainvalue=c(1:5)
                             ))
cv
# Controllo
checkInput(cv,strata,frame)
			

# Optimization step
solution1 <-
	optimizeStrata(
	errors = cv , 
	strata = strata, 
	iter = 50, 
	pops = 20, 
	suggestions = NULL,
	showPlot = FALSE,
	writeFiles = FALSE)
sum(ceiling(solution1$aggr_strata$SOLUZ))	
nrow(solution1$aggr_strata)		

kmean <- KmeansSolution(strata, cv, nstrata=NA, showPlot = T)
solution2 <-
  optimizeStrata(
    errors = cv , 
    strata = strata, 
    iter = 50, 
    pops = 20, 
    suggestions = kmean,
    showPlot = FALSE,
    writeFiles = FALSE
  )
sum(ceiling(solution2$aggr_strata$SOLUZ))	
nrow(solution2$aggr_strata)		

newstrata <- updateStrata(strata,solution2)
framenew <- updateFrame(frame,newstrata)
results1 <- evalSolution(framenew, solution2$aggr_strata, 1000, writeFiles=TRUE)
results1$coeff_var
outstrata$SOLUZ <- ceiling(solution2$aggr_strata$SOLUZ)
# calculate expected CV's
results2 <- evalSolution(framenew, outstrata, 1000, writeFiles=TRUE)
results2$coeff_var

save.image(file="Nations.RData")

#----------------------------------------------

# Nuove variabili ausiliarie
frame$x2 <- var.bin(nations$TFR,bins=5)
frame$x3 <- var.bin(nations$GDP,bins=5)
write.table(frame,"frame.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)

table(frame$x2)
table(frame$x3)	
strata <- buildStrataDF(frame)
nrow(strata)
# Controllo
checkInput(errors,strata,frame)
# Ottimizzazione con tutte le variabili ausiliarie
solution <-
	optimizeStrata(
	errors , 
	strata , 
	cens = NULL, 
	strcens = FALSE,
	alldomains = TRUE,
	dom = NULL,	
	initialStrata = nrow(strata), 
	addStrataFactor = 0.01, 
	minnumstr = 2, 
	iter = 100, 
	pops = 20, 
	mut_chance = 0.001, 
	elitism_rate = 0.2,
	highvalue = 1e+08, 
	suggestions = NULL,
	realAllocation = TRUE,
	writeFile = "YES"
	)
sum(ceiling(solution$aggr_strata$SOLUZ))	
nrow(solution$aggr_strata)		

# Aggiornamento strati e frame

boxplot(val ~ cv, data = results$coeff_var,
        col = "orange",
        main = "Distribution of CV's in the domains",
        xlab = "Variables Y",
        ylab = "Value of CV")
diff <- read.csv("differences.csv")
numY <- sum(grepl("diff", colnames(diff)))
k <- ceiling(numY/4)
for (j in 1:k) {
    split.screen(c(2, 2))
    for (i in 1:4) {
        if (i + 4 * (j - 1) <= numY) {
            stmt <- paste("screen(", i, ")", sep = "")
            eval(parse(text = stmt))
			stmt <- paste("boxplot(diff",i,"~dom,data=diff,ylab='Differences',xlab='Domain',col = 'orange')",sep="")
			eval(parse(text = stmt))
            stmt <- paste("mtext(expression(Y", i , "), side=3, adj=0, cex=1.0, line=1)", 
                  sep = "")
            eval(parse(text = stmt))
        }
    }
}