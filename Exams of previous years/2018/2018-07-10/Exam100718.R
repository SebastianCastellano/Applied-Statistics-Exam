load("~/GitHub/Applied-Statistics-Exam/mcshapiro.test.RData")
# Exam 10/07/2018 revenues/running/sales-presales/focaccia ####

# Problem n.1
# The file Revenues.txt contains the total monthly income of 37 beach clubs in Santa Margherita Ligure (GE),
# during the months of June, July, August and September 2017.
# a) Perform a test of level 10% to verify if there exists any significant difference in the mean of monthly revenues
# along the considered period. Introduce and verify the appropriate assumptions.
# b) Build 4 simultaneous confidence intervals (global level 90%) to describe the dynamic over time of the mean
# revenues. Comment the results.

setwd("./100718")
load("../../mcshapiro.test.RData")

revenues <- read.table("Revenues.txt")

#Repeated measures
#a)
#Assumptions
mcshapiro.test(revenues)$p

revenues.diff <- revenues[-4]-revenues[-1]
n <- dim(revenues.diff)[1]
p <- dim(revenues.diff)[2]

alpha <- 0.1
q.fish <- (n-1)*p/(n-p)*qf(1-alpha,p,n-p)

sample.mean.d <- sapply(revenues.diff, mean)
mu0 <- c(0,0,0)
S <- cov(revenues.diff)
invS <- solve(S)

T0 <- (sample.mean.d-mu0)%*%invS%*%(sample.mean.d-mu0)
T0>q.fish #TRUE, there is evidence

#b)
alpha <- 0.1
k<-4
qT<- qt(1-alpha/2/k,n-1)
sample.mean <- sapply(revenues, mean)
S <- cov(revenues)

conf.int <- cbind(sample.mean-qT*sqrt(diag(S)/n), sample.mean, sample.mean +qT*sqrt(diag(S)/n))
conf.int

# Problem n.2
# The file Running.txt collects the times of 80 participants to the Portofino>Run 2018, for the sections Santa
# Margherita Ligure to Paraggi and Paraggi to Portofino.
# a) Use a hierarchical clustering method (Euclidean distance and single linkage) to identify two groups of participants, i.e., those who actually run and those who walked instead. Report the number of data within each
# cluster, the mean within the clusters and a qualitative plot of the results.
# b) Evaluate the performances of the clustering method at point (a) and identify the possible issues (if any). Propose,
# if needed, an alternative clustering method to identify the two groups of participants (report the number of
#                                                                                         data within each cluster, the mean within the clusters and a qualitative plot of the results).
# c) Provide four Bonferroni intervals (global level 95%) for the mean and the variances of the running times of the
# two sections.
setwd("./100718")
load("../../mcshapiro.test.RData")

running <- read.table("Running.txt")
plot(running)

#a)
D <- dist(running)
clust <- hclust(D,method = "single")
clusters <- cutree(clust,2)
plot(running,col=clusters+1)

table(clusters)
sapply(running[which(clusters==1),],mean)
sapply(running[which(clusters==2),],mean)

#b)
D <- dist(running)
clust <- hclust(D,method = "complete")
clusters <- cutree(clust,2)
plot(running,col=clusters+1)

table(clusters)
sapply(running[which(clusters==1),],mean)
sapply(running[which(clusters==2),],mean)

#c)
k <- 4
n <- 80
p <- 2
alpha <- 0.05
qT <- qt(1-alpha/2/k,n-1)
sample.mean <- sapply(running, mean)
S <- cov(running)

conf.int.mean <- cbind(sample.mean - qT*sqrt(diag(S)/n), sample.mean, sample.mean + qT*sqrt(diag(S)/n))
conf.int.mean 
#confidence intervals for the variance of a gaussian
q.chisq1 <- qchisq(alpha/2/k,n-1)
q.chisq2 <- qchisq(1-alpha/2/k,n-1)

conf.int.cov <- cbind(diag(S)*(n-1)/q.chisq2, diag(S),diag(S)*(n-1)/q.chisq1)
conf.int.cov

# Problem n.3
# The files Presales.txt and Sales.txt contain the prices of flip flops and swimsuits in 45 shops of Rapallo, before
# and after the summer sales 2017, respectively.
# a) Perform a statistical test (level 1%) to verify if the prices were discounted in mean of 20% during sales. Introduce
#                                and verify the appropriate assumptions.
#                                b) Provide a point estimate of the mean and of the covariance matrix of the discounts on flip flops and swimsuits
# during the summer sales 2017.
# c) Estimate an elliptical region that contains 99% of the discounts. Reports its analytical expression, its center,
# and the direction and length of its semi-axes.
setwd("./100718")
load("../../mcshapiro.test.RData")

presales <- read.table("Presales.txt")
sales <- read.table("Sales.txt")
boxplot(presales$Flip.Flops,sales$Flip.Flops,presales$Swimsuit,sales$Swimsuit)

discounts <- (presales-sales)/presales
boxplot(discounts)
#a) Test on the mean of a gaussian
#Assumptions
mcshapiro.test(presales)$p #ok
mcshapiro.test(discounts)$p #ok...

# Discount
mu0 = sapply(presales, mean)*0.8
alpha<-0.01
n <- 45
p<-2
q.fish <- (n-1)*p/(n-p)*qf(1-alpha/2,p,n-p)
mean.sales <- sapply(sales,mean)
S <- cov(sales)
invS <- solve(S)
T0 <- (mean.sales-mu0)%*%invS%*%(mean.sales-mu0)
T0>q.fish

#b)
discounts <- (presales-sales)/presales
k <- 4
n <- 45
p <- 2
alpha <- 0.05
qT <- qt(1-alpha/2/k,n-1)
sample.mean <- sapply(discounts, mean)
S <- cov(discounts)

conf.int.mean <- cbind(sample.mean - qT*sqrt(diag(S)/n), sample.mean, sample.mean + qT*sqrt(diag(S)/n))
conf.int.mean 
#confidence intervals for the variance of a gaussian
q.chisq1 <- qchisq(alpha/2/k,n-1)
q.chisq2 <- qchisq(1-alpha/2/k,n-1)

conf.int.cov <- cbind(diag(S)*(n-1)/q.chisq2, diag(S),diag(S)*(n-1)/q.chisq1)
conf.int.cov

#c)
alpha <- 0.01
radius <- sqrt(qf(1-alpha,p,n-p)/((n-p)/((n*p-p)*(1+1/n))))
center <- sapply(discounts,mean)
W <- cov(discounts)
directions <- eigen(W)$vectors
length <- sqrt(eigen(W)$values)*radius
#if we work asymptotically
radius_chi <- sqrt(qchisq(1-alpha, 2)*(1+1/n))

x11()
plot(discounts, asp=1, pch=1, main='Dataset of the Discounts')
library(mvnormtest)
ellipse(center=center, shape=W,radius, lwd=2, col = 'blue')
ellipse(center=center, shape=W,radius_chi, lwd=2, col = 'red')

library(mixtools)
mixtools::ellipse(mu=center,sigm=W,alpha=.01) # this ellipse uses the chisq asymptotiv radius

# Problem n.4
# The file Focaccia.txt contains the amount [kg] of focaccia sold in a bakery in Recco during 43 days of June and
# July 2017. For the kilos of sold focaccia consider the following linear model
# Yg = ??0,g + ??1,g · t + ,
# with t ??? [1 : 61] the index of the day, g = {weekend, weekday} the day of the week and  ??? N(0, ??2
# ).
# a) Estimate the 5 parameters of the model {??0,g, ??1,g, ??}. Verify the model assumptions.
# b) Perform two statistical tests - each at level 5% - to verify if
# - there is a significant dependence of the mean sales on the day of the week;
# - there is a significant difference between weekend and weekdays, in the increase of the mean sales along time.
# c) Based on point (b), reduce the model and update the estimates of the parameters.
# d) Perform a test of level 5% to verify if during weekends the mean amount of sold focaccia increases of 60 kg. In
# case, update the parameters' estimates.
# e) Based on the last model, provide a point prediction of the mean sales of focaccia on the 28th July 2017 (day
#                                                                                                             61, weekday).
setwd("./100718")
load("../../mcshapiro.test.RData")

focaccia<- read.table("Focaccia.txt")
attach(focaccia)

lmod <- lm(kg ~ ifelse(day=="weekday",1,0) +  t +   ifelse(day=="weekday",1,0):t)
summary(lmod)
sigma <- summary(lmod)$sigma

x11()
par(mfrow=c(2,2))
plot(lmod)
shapiro.test(lmod$residuals)$p

x11()
plot(t,lmod$residuals)

#c)

lmod1 <- lm(kg ~ ifelse(day=="weekday",1,0) +  t)
summary(lmod1)
anova(lmod,lmod1)
sigma1 <- summary(lmod1)$sigma
#d)
linearHypothesis(lmod1,c(0,1,0),-60)
#e)
predict(lmod1,data.frame(t=61,day="weekday"),interval = "confidence")

# Extra exercises from labs
setwd("./extra from labs")


#Extra exercises from labs
