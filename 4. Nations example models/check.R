mean(framenew$Y1)
mean(framenew$Y2)
# mean(framenew$Y3)
m1 <- rep(0,1000)
m2 <- rep(0,1000)
# m3 <- rep(0,100)
outstrata <- solution2$aggr_strata
for (i in 1:1000) {
 s <- selectSample(framenew,outstrata) 
 m1[i] <- mean(s$Y1)
 m2[i] <- mean(s$Y2)
 # m3[i] <- mean(s$Y3)
}
# m
mean(m1)
sd(m1)
sd(m1)/mean(m1)
mean(m2)
sd(m2)
sd(m2)/mean(m2)
# mean(m3)
# sd(m3)
# sd(m3)/mean(m3)
