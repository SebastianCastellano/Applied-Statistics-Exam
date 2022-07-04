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

# Problem n.2
# The file Mexican.txt collects the prices [Mexican pesos] of 300 Mexican meals consumed in July 2017 by tourists
# in three areas of Mexico (near Mexico City, Cancun, and Guanajato). The dataset also reports whether the meal
# was based on Tacos or Fajitas.
# a) Build a complete ANOVA model to characterise the price of a Mexican meal. Verify the assumptions of the
# model.
# b) Reduce the model based on appropriate statistical tests.
# c) Build Bonferroni intervals (global level 99%) for the mean differences between the price of a Mexican meal, in
# the groups identified at point (b).

setwd("./280618")
mexican <- read.table("Mexican.txt")
attach(mexican)

tacos.mexcity <- mexican[which(type.food=="Tacos" & area=="MexicoCity"),1]
tacos.cancun <- mexican[which(type.food=="Tacos" & area=="Cancun"),1]
tacos.guanajato <- mexican[which(type.food=="Tacos" & area=="Guanajato"),1]
fajitas.mexcity <- mexican[which(type.food=="Fajitas" & area=="MexicoCity"),1]
fajitas.cancun <- mexican[which(type.food=="Fajitas" & area=="Cancun"),1]
fajitas.guanajato <- mexican[which(type.food=="Fajitas" & area=="Guanajato"),1]

boxplot(price ~ type.food)
boxplot(price ~ area)

#a) ANOVA
#Gaussianity assumption for each group
shapiro.test(tacos.mexcity)$p
shapiro.test(tacos.cancun)$p
shapiro.test(tacos.guanajato)$p
shapiro.test(fajitas.mexcity)$p
shapiro.test(fajitas.cancun)$p
shapiro.test(fajitas.guanajato)$p

#Same covariance
var(tacos.mexcity)
var(tacos.cancun)
var(tacos.guanajato)


model1 <- aov(price ~ type.food + area + type.food:area)
summary.aov(model1)
#only the area seems to influence the price

#b) reduce the model
model2 <- aov(price ~ area)
summary.aov(model2)

#c) bonferroni
n1 <- 100 #=n1,n2,n3
n <- 300
g <- 3
p <- 1
k <- 3 #p*g*(g-1)/2

alpha <- 0.01
qT <- qt(1-alpha/2/k,n-g)

mean.sample <- tapply(price, area, mean)
Spooled <- sum(tapply(price, area, var))/3
# or sum(residuals(model2)^2)/(n-g)

conf.intervals <- cbind(rbind(mean.sample[1]-mean.sample[2] - qT*sqrt(Spooled*2/100),
                              mean.sample[2]-mean.sample[3] - qT*sqrt(Spooled*2/100),
                              mean.sample[1]-mean.sample[3] - qT*sqrt(Spooled*2/100)),
                        rbind(mean.sample[1]-mean.sample[2],
                              mean.sample[2]-mean.sample[3],
                              mean.sample[1]-mean.sample[3]),
                        rbind(mean.sample[1]-mean.sample[2] + qT*sqrt(Spooled*2/100),
                              mean.sample[2]-mean.sample[3] + qT*sqrt(Spooled*2/100),
                              mean.sample[1]-mean.sample[3] + qT*sqrt(Spooled*2/100)))

conf.intervals
detach(mexican)
#we can observe that only cancun is significantly above the other two, mexcity and guanajato accept the same mean hypothesis


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


# Problem n.4
# The file Hotels.txt contains the rates for a double room in 4??? hotels at Playa del Carmen, recorded for 83 days of
# the year 2017. For the rates consider a linear model, accounting for the time of the year, for the seasonality (wet
#                                                                                                                 season or not) and for the position of the hotel with respect to the sea (seafront or not):
#   Yg = ??0,g + ??1,g ·
# 
# 1 + cos 
# 4??
# 365
# t
#  + ,
# with  ??? N(0, ??2
# ) and g the grouping structure induced by the seasonality and by the seafront (consider a model
#                                                                                without interaction).
# a) Estimate the parameters of the model ({??0,g, ??1,g, ??}). Verify the model assumptions.
# b) Perform three statistical tests - each at level 1% - to verify if
# - there is a significant dependence of the mean rates on the seasonality (wet season or not);
# - there is a significant dependence of the mean rates on the position (seafront or not);
# - there is a significant difference in the temporal dynamic of the mean rates along the year depending on
# the seasonality or on the position of the hotel.
# c) Based on point (b), reduce the model and update the estimates of the parameters.
# d) Provide a confidence interval (level 99%) for the maximum of the mean rates of 4??? hotels at Playa del Carmen.

rm(list=setdiff(ls(),"mcshapiro.test"))
hotels <- read.table("Hotels.txt")
table(hotels$Season)

attach(hotels)
pos.dummy <- ifelse(Position=="Seafront",1,0)
season.dummy <- ifelse(Season=="WetSeason",1,0)

lmod <- lm(price ~ pos.dummy + season.dummy + I(1+cos(4*pi/365*t)))
summary(lmod)
sigma <- summary(lmod)$sigma

#model assumptions
x11()
par(mfrow=c(2,2))
plot(lmod)
end

x11()
plot(as.factor(Position), lmod$residuals)

x11()
plot(as.factor(Season), lmod$residuals)

x11()
plot(I(1+cos(4*pi/365*t)), lmod$residuals)

# stat tests
library(car)
linearHypothesis(lmod,rbind(c(0,0,1,0),c(0,1,0,0)),c(0,0),alpha=0.01)

summary(lm(price ~  pos.dummy + season.dummy + I(1+cos(4*pi/365*t))+ pos.dummy:I(1+cos(4*pi/365*t))+ season.dummy:I(1+cos(4*pi/365*t)) ))

# Confidence interval for the mean
which.max(lmod$fitted.values) # it is pos 1

x0 <- data.frame(pos.dummy=1,season.dummy=0,t=2)
predict(lmod,x0,interval = "confidence",level = 0.99)

