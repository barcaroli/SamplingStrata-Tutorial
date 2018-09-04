library(stratification)
x = nations$GDP
design <- strata.LH(x = x, CV = 0.10, Ls = 12, takenone = 0,
          model = "linear",
          model.control = list(beta = 0.001372584, sig2 = 19.44731,gamma=0), 
          algo.control = list(maxstep = 20, rep = 1))
design
design$RRMSE
var.strata(design,y=nations$contraception)

