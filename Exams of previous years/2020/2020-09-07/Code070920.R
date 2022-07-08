# TDE 07.09.2020

setwd("~/Desktop/Poli/4° anno/Applied statistics/Old exams/070920")
load("~/Desktop/Poli/4° anno/Applied statistics/Old exams/mcshapiro.test.RData")

#===============================================================================

# EXERCISE 1

# The file weather.txt reports weather data collected on the first months of the 
# year 2020 in Milan. Each statistical unit corresponds to a day. For each unit, 
# the following measurements are reported: mean temperature (°C), min temperature (°C), 
# max temperature (°C), dew point (°C), humidity (%), visibility (km), mean wind (km/h) 
# and max wind (km/h).
# a) Perform a Principal Component Analysis of the dataset, evaluate whether it is 
#    appropriate to use the original variables or the standardized ones and proceed 
#    accordingly. Report a plot of the loadings of the first 3 principal components 
#    and interpret them.
# b) Report the scatter plot of the data along the first two principal components 
#    and describe how to interpret the four quadrants of the plane. Use the 
#    information about the day of the year to further interpret the results.
# c) Report the screeplot. Propose (with motivations) a dimensionality reduction 
#    of the dataset. Report the variance explained along the principal components retained.
# d) The measurements for the 1st of August are the following: mean temperature 30°C, 
#    min temperature 23°C, max temperature 36*C, dew point 22*C, humidity 65%, 
#    visibility 19km, mean wind 5km/h and max wind 15km/h.
#    Project the new datum on the reduced space identified at point (c).

data <- read.table('weather.txt', header=TRUE)
n <- dim(data)[1]
p <- dim(data)[2]

# Point a 

x11()
par(mar=rep(8,4))
boxplot(data, las=2, col='blue')
x11()
par(mar=rep(8,4))
boxplot(scale(x=data,center = T, scale=T), las=2, col='blue')
# It is better to use the standardized data

data <- scale(data,center = T, scale=T)

pc <- princomp(data, scores=T)
pc
summary(pc)

load.data <- pc$loadings
load.data
x11()
par(mfcol = c(3,1))
for(i in 1:3) barplot(load.data[,i], ylim = c(-1, 1), main=paste("PC",i))

# The first principal component represents a contrast between the 'Humidity' and the other variables 
# The second principal component represents a small contrast between 'Visibility' and the other variables 
# The third principal component is not really interpretable 

# Point b 

scores.data <- pc$scores
x11()
plot(scores.data[,1:2])         
abline(h=0, v=0, lty=2, col='grey')
x11() 
biplot(pc)   

# We can see that for the days between 50 and 100, the wind is high as well as the minimum temperature:
# indeed they are the first days of the year (winter days)

# Point c 

# We can reduce the dataset by considering only the first three principal components, 
# which explain the 97% of the variability 

x11()
plot(cumsum(pc$sd^2)/sum(pc$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data),labels=1:ncol(data),las=2)

# Point d (by Martina Del Basso)

new <- data.frame(MeanTemp=30, MinTemp=23, MaxTemp=36, DewPoint=22, Humidity=65, Visibility=19, MeanWind=5, MaxWind=15)
new.sd <- (new - colMeans(data))/(sapply(data,sd))

meanF <- colMeans(data.sd)
matplot(meanF, type='l', main = '0 PC', lwd=2, ylim=range(data.sd))
for(i in 1:3)
{
  projection <- matrix(meanF, dim(data.sd)[[1]], dim(data.sd)[[2]], byrow=T) + scores.data[,i] %*% t(load.data[,i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim=range(data.sd))
  matplot(meanF, type='l', lwd=2, add=T)
}
x<-pc$loadings[,1]      
y<-pc$loadings[,2]
z<-pc$loadings[,3]
new<-as.matrix(new)
coeff <- c((new%*%x), (new%*%y), (new%*%z))

#===============================================================================

# EXERCISE 2

# To compare the performances of the led bulbs produced by two brands 
# (Candle Inc. and Sunshine Corp.), brightness tests are performed on 50 led bulbs 
# independently sampled from those produced by Candle Inc. and 50 led bulbs
# independently sampled from those produced by Sunshine Corp.
# For each led bulb, the brightness is measured by two light meters positioned 
# at different distances from the led bulb: the first light meter is a few 
# centimeters from the led bulb and the second is positioned at one meter from
# the led bulb. The measurements on different led bulbs are independent (while 
# the measurements on the same led bulb are not).
# Files candle.txt and sunshine.txt report the measurements obtained with the 
# two light meters on each led bulb produced by Candle Inc. and Sunshine Corp. respectively.
# a) Perform a statistical test of level 95% to verify if the mean measurements on the 
#    led bulbs produced by the two brands differ. State the model assumptions 
#    required to perform the test and verify them.
# b) Compute and report the p-value of the test at point (a).
# c) Interpret the results of the test at point (a) through two Bonferroni 
#    intervals of global level 95% for appropriate differences in the mean. 
#    Comment the result.
# d) Is there statistical evidence to state that, at level 95%, the decrease in 
#    brightness between the two measurements of the led bulbs produced by 
#    Candle Inc. is in mean higher than the one of the led bulbs produced by Sunshine Corp.?

data_candle <- read.table('candle.txt', header=T)
data_sunshine<- read.table('sunshine.txt', header=T)

nc <- dim(data_candle)[1]     # nc=50
ns <- dim(data_sunshine)[1]   # ns=50
n = nc + ns
p  <- dim(data_candle)[2]     # p=2 , number of variables 

# Point a

candle.mean <- sapply(data_candle,mean)
sunshine.mean <- sapply(data_sunshine,mean)
candle.cov  <-  cov(data_candle)
sunshine.cov  <-  cov(data_sunshine)
Sp      <- ((nc-1)*candle.cov + (ns-1)*sunshine.cov)/(nc+ns-2)
# To compare the matrices
list(S1=candle.cov, S2=sunshine.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)  
alpha   <- .05
delta.0 <- c(0,0)
Spinv   <- solve(Sp)
T2 <- nc*ns/(nc+ns) * (candle.mean-sunshine.mean-delta.0) %*% Spinv %*% (candle.mean-sunshine.mean-delta.0)
cfr.fisher <- (p*(nc+ns-2)/(nc+ns-1-p))*qf(1-alpha,p,nc+ns-1-p)
T2 < cfr.fisher        
# FALSE: there's statistical evidence to reject H0 at level 5%

# Verify the assumption of Gaussianity 
mcshapiro.test(data_candle)
mcshapiro.test(data_sunshine)
# The p-values are both high, so the 2 populations are really Gaussian

# Point b 

P <- 1 - pf(T2/(p*(nc+ns-2)/(nc+ns-1-p)), p, nc+ns-1-p)
P  
# The p-value is low (=> we reject H0)

# Point c 

k <- p
cfr.t <- qt(1 - alpha/(k*2), n-1)

Bf_candle <- cbind(inf = candle.mean - cfr.t*sqrt(diag(candle.cov)/n),
                   center = candle.mean, 
                   sup = candle.mean + cfr.t*sqrt(diag(candle.cov)/n))
Bf_sunshine <- cbind(inf = sunshine.mean - cfr.t*sqrt(diag(sunshine.cov)/n),
                     center = sunshine.mean, 
                     sup = sunshine.mean + cfr.t*sqrt(diag(sunshine.cov)/n))
Bf_candle
Bf_sunshine
# They are all significant intervals since the 0 is not in the intervals 

# Point d 

delta_candle <- data_candle[,1] - data_candle[,2]   
delta_sunshine <- data_sunshine[,1] - data_sunshine[,2]
shapiro.test(delta_candle)
shapiro.test(delta_sunshine)
var.test(delta_candle, delta_sunshine)
# The assumption of Gaussianity is verified 

#H0 : (LM1c - LM2c) - (LM1s - LM2s) > 0 vs H1

sdeltac <- mean(delta_candle)
sdeltas <- mean(delta_sunshine)
sc <- sd(delta_candle)^2
ss <- sd(delta_sunshine)^2
sp <- ((n-1)*sc + (n-1)*ss)/(n+n-2)

T_stat <- (sdeltac - sdeltas)/sqrt(2*sp/n)
cfr.student.test <- qt(1-alpha, n+n-2)
Rej.test <- T_stat > cfr.student.test
p_val <- 1- pt(T_stat, n+n-2)
p_val
# The p-value is low => we reject H0
# => The decrease in brightness between the 2 measuraments of the led bulbs produced by Candle Inc. isn't
#    in mean higher than the one of the led bulbs produced by Sunshine Corp.

#===============================================================================

# EXERCISE 3

# Benedetta is learning to bake bread. To obtain the perfect dough rising, she is 
# performing some experiments to evaluate the effect of the kind of yeast used 
# and the effect of the waiting time (measured in hours) on the rising
# of the dough. In the different experiments, she used either the brewer's yeast (by) 
# or the sourdough (sd) and measured the rising of the dough as a percentage with 
# respect to the initial volume at different times. The file leaven.txt contains 
# the data collected by Benedetta in 80 experiments conducted in the last months.
# Consider a linear model assuming, for each kind of yeast, a polynomial of degree 
# at most 2 in waiting time (time). Impose the same intercept for the two models.
# a) Estimate the parameters of the model. Verify the model assumptions.
# b) Perform two statistical tests - each at level 1% - to verify if
#    - there is a significant dependence of the rising of the dough on the kind of yeast;
#    - there is statistical evidence that the degree of the polynomial for the 
#      brewer's yeast is higher than the degree of the polynomial for the sourdough.
# c) Based on the tests performed in (b) or any other test deemed relevant, reduce 
#    the model and update the model parameters.
# d) Benedetta wants to bake bread for dinner, but she can wait only 2 hours: 
#    which yeast do you suggest to use?
#    Provide a confidence interval with confidence level 99% for the mean of the 
#    rising of the dough in such conditions.

library(car)

data <- read.table('leaven.txt', header=TRUE)

data[(data[,3]=='sd'),3]=0
data[(data[,3]=='by'),3]=1

attach(data)

# Point a

fm <- lm(volume ~ time + I(time^2) + yeast:time + yeast:I(time^2))    # we put I, otherwise it doesn't the ^2
summary(fm)
# The parameters of the model are: 
fm$coefficients

# Verify the assumptions
x11()
par(mfrow=c(2,2))
plot(fm)   
shapiro.test(residuals(fm))
# The assumptions are verified 

# Point b

linearHypothesis(fm, rbind(c(0,0,0,1,0),c(0,0,0,0,1)), c(0,0), level = 0.01)
summary(fm)
# We have evidence that it there's a significant dependence of the rising of the dough on the kind of yeast 
# since the p-value of this test is very low and the interactions between yeast and the other regressors are
# significant (see the *)

linearHypothesis(fm, rbind(c(0,0,1,0,0)), c(0),level=0.01)
summary(fm)
linearHypothesis(fm, rbind(c(0,0,1,0,1)), c(0),level=0.01)
summary(fm)
# There is statistical evidence to say that the grade of the polynomial 'by' is greater than 'sd'

# Point c 

yeast<-as.numeric(yeast)
fm2 <- lm(volume ~ time + yeast:time + yeast:I(time^2))    
summary(fm2)

fm2$coefficients

# Point d 

z0 <- data.frame(time=2,yeast=0)
Conf0 <- predict(fm2, z0, interval='confidence', level=1-0.01) 
Conf0
z1 <- data.frame(time=2,yeast=1)
Conf1 <- predict(fm2, z1, interval='confidence', level=1-0.01)
Conf1
# The highest volume is obtained for yeast=0, so using the brewer's yeast
# The requested confidence interval is:
Conf0

#===============================================================================

# EXERCISE 4

# To forecast the tidal currents in the port of Dublin, the vertical motion 
# induced by the rise and fall of the tides is monitored. The file tide.txt 
# reports the measurements of the sea level in the port of Dublin collected every 
# half hour for one day. Since the time variation of the sea level is inherently 
# a continuous process, consider a functional data analysis approach. It is 
# known that the available measurements are affected by small errors.
# a) Perform a smoothing of the data using a B-spline basis of degree 3. 
#    Choose the number of basis functions usingn a generalized cross-validation 
#    criterion. Report the number of basis functions chosen, a plot of the B-spline
#    basis system used and a plot of the smoothed data.
# b) Compute approximate pointwise confidence intervals at the sampling times of 
#    the data and provide a plot.
# c) To study the currents induced by the tide, it is important to have a good 
#    estimate of the velocity of the tide.Compute an approximation of the first 
#    derivative of the curve from the data and the first derivative of the smoothed 
#    curve obtained at point (a). Provide a plot to compare the two and comment on the result.
# d) Would you suggest the use of a different basis system? Why?

library(fda)

data <- read.table('tide.txt', header=TRUE)

# Point a 

m <- 4
degree <- 3

levels <- data$level
xx <- data$time
NT <- length(xx)     # number of locations of observations

nbasis <- 5:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(c(0,23.5), nbasis[i], m)
  gcv[i] <- smooth.basis(xx, levels, basis)$gcv
}
x11()
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)
# The optimal number of basis is 12

nbasis <- 12
basis <- create.bspline.basis(rangeval=c(0,23.5), nbasis=nbasis, norder=m)
names(basis)
x11()
plot(basis, main='Basis')

# Original data
x11()
matplot(data$level, type='l', main='Data', xlab='hour', ylab='volume')
# Smoothed data
data.fd <- Data2fd(y = data$level, argvals = xx, basisobj = basis)
x11()
plot.fd(data.fd, main = 'Smooted curves',xlab='hour',ylab='volume')

# Point b 

basismat <- eval.basis(xx, basis)
S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat)                
sum(diag(S))   
Xsp <- smooth.basis(argvals=xx, y=levels, fdParobj=basis)
Xsp0 <- eval.fd(xx, Xsp$fd) 
df <- Xsp$df
sigmahat <- sqrt(sum((Xsp0-levels)^2)/(NT-df))
lb <- Xsp0-qnorm(0.975)*sigmahat*sqrt(diag(S))
ub <- Xsp0+qnorm(0.975)*sigmahat*sqrt(diag(S))
x11()
plot(xx,Xsp0,type="l",col="blue",lwd=2,ylab="")
points(xx,lb,type="l",col="blue",lty="dashed")
points(xx,ub,type="l",col="blue",lty="dashed")

# Point c

# Original data: 
rappincX1 <- (levels[3:NT]-levels[1:(NT-2)])/(xx[3:NT]-xx[1:(NT-2)])    # [y(t+1) - y(t-1)]/2*deltat =~ y'(t)
# Smoothed data:
est_coef = lsfit(basismat, levels, intercept=FALSE)$coef 
Xsp0 <- basismat %*% est_coef 
basismat1<- eval.basis(xx, basis, Lfdobj=1)
Xsp1 <- basismat1 %*% est_coef
  
x11(width = 14)
par(mfrow=c(1,2))
plot(xx,levels,xlab="t",ylab="observed data")
points(xx,Xsp0 ,type="l",col="blue",lwd=2)
legend("topleft", legend = c("noisy data","estimated curve"), col = c("black", "blue"), lwd = c(1,3,2))
plot(xx[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(xx,Xsp1 ,type="l",col="blue",lwd=2)

# Point d

# Maybe it is better Fourier becasue of the periodicity (answer by Martina Del Basso)
