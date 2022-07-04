load("../../mcshapiro.test.RData")

# Problem n.1
# The files Morning.txt and Evening.txt contain the data about delays [min] registered in July 2017 on the flights
# operated by the company BeLate from Mexico City to Oaxaca and viceversa. The flights were scheduled twice per
# day, in the morning time (Morning.txt) and evening time (Evening.txt). Negative values indicate flights arrived
# earlier than scheduled. Assume the flights operated on the same day to be dependent, and the flight operated in
# different days to be independent.
# a) Verify at level 1% if there is a significant difference in the mean delay between morning and evening flights
# (introduce and verify the appropriate assumptions).
# b) Provide four Bonferroni intervals (global level 1%) for the mean delay in the morning flights and for the mean
# delay in the evening flights (introduce and verify the appropriate assumptions). Comment the results.
# c) On July 28, the scheduled arrival time for the morning flight from Oaxaca to Mexico City is 10:15. Would you
# believe (at level 99%) to be able to take an international flight to Italy at 12:15, knowing that the gate closes
# at 11:45?
#   

morning <- read.table("Morning.txt")
evening <- read.table("Evening.txt")

#a) repeated measures
flight.diff <- evening - morning

#gaussian assumption
mcshapiro.test(flight.diff) #ok

n <- dim(flight.diff)[1]
p <- dim(flight.diff)[2]
mu0 <- c(0,0)
sample.mean <- sapply(flight.diff,mean)
W <- cov(flight.diff) 
invW <- solve(W)  

T_0 <- n*(sample.mean - mu0)%*% invW %*%(sample.mean - mu0)

alpha <-0.01
fisher.q <- (n-1)*p/(n-p)*qf(1-alpha,p,n-p)

T_0 > fisher.q

#b) mean bonferroni
k <- 2
alpha <- 0.01
qT <- qt(1-alpha/2/k,n-1)

mean.morning <-sapply(morning,mean)
W = cov(morning)

conf.int.morn <- cbind(mean.morning - qT*sqrt(diag(W)/n),mean.morning,mean.morning +  qT*sqrt(diag(W)/n))
conf.int.morn

mean.evening <- sapply(evening,mean)
W = cov(evening)

conf.int.eve <- cbind(mean.evening - qT*sqrt(diag(W)/n),mean.evening,mean.evening+  qT*sqrt(diag(W)/n))
conf.int.eve

sd(morning$OAX.MEX)*6


# Problema 3
# The file Precolombian.txt collects the data about 167 Precolombian statues found in the area of Tula (Mexico)
# and exhibited in the National Museum of Mexico City. Some of these statues were attributed by experts to the
# Maya civilization, some to the Aztecs and the remaining to Toltecs. The dataset reports the dating (estimated
#                                                                                                     year in which the statues were built) and the aspect ratios of the statues (ratio between their hight and width).
# Knowing that, out of 100 statues found in the area, on average 20 of these are attributed to Aztecs, 10 to Maya
# and 70 to Toltecs, answer the following questions.
# a) Build a classifier for the variable civilization based on the available quantitative features. Report the mean
# within the groups identified by the variable civilization and a qualitative plot of the classification regions.
# Introduce and verify the appropriate assumptions.
# b) Compute the APER of the classifier.
# c) How would you classify a new statue dated 986 a.D. and with aspect ratio 1.4?

precolombian <- read.table("Precolombian.txt")

prior <- c(0.2,0.1,0.7)
library(MASS)

table(precolombian$Civilization)
maya <- precolombian[which(precolombian$Civilization=="Maya"),][-3]
aztec <- precolombian[which(precolombian$Civilization=="Aztec"),][-3]
toltec <- precolombian[which(precolombian$Civilization=="Toltec"),][-3]
# Assumptions
#normality
mcshapiro.test(maya)
mcshapiro.test(aztec)
mcshapiro.test(toltec)

#same variance
bartlett.test(maya,aztec)
bartlett.test(maya,toltec)
bartlett.test(aztec,toltec)
cov(maya)
cov(aztec)
cov(toltec)

class.model<- qda(as.matrix(precolombian[-3]), precolombian$Civilization, prior = prior)
class.model
classes <- predict(class.model)$class


x11()
par(mfrow=c(1,2))

plot(precolombian[-3],col=ifelse(classes=="Aztec", 1,ifelse(classes=="Maya", 2,3)),main="lda",pch=19)
plot(precolombian[-3],col=ifelse(precolombian[3]=="Aztec", 1,ifelse(precolombian[3]=="Maya", 2,3)),main="real",pch=19)

x11()
plot(precolombian[-3],col=ifelse(classes=="Aztec", 1,ifelse(classes=="Maya", 2,3)),main="lda",pch=19)

x  <- seq(min(precolombian[,1]), max(precolombian[,1]), length=200)
y  <- seq(min(precolombian[,2]), max(precolombian[,2]), length=200)
xy <- expand.grid(Altezza=x, Peso=y)


z.q  <- predict(class.model, xy)$post  
z1.q <- z.q[,1] - (z.q[,2]+z.q[,3])
z2.q <- z.q[,2] -  (z.q[,1]+z.q[,3])
z3.q <- z.q[,3] -  (z.q[,2]+z.q[,1]) 


contour(x, y, matrix(z1.q, 200), levels=0, drawlabels=F, add=T, lty=2)  
contour(x, y, matrix(z2.q, 200), levels=0, drawlabels=F, add=T, lty=2)
contour(x, y, matrix(z3.q, 200), levels=0, drawlabels=F, add=T, lty=2)

APER <- sum(classes != precolombian$Civilization)/dim(precolombian)[1]

predict(class.model,data.frame(Year=986,Aspect.Ratio=1.4))
points(data.frame(Year=986,Aspect.Ratio=1.4),col="gold",pch=4,lwd=2)
