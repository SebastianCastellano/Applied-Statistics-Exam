load("~/GitHub/Applied-Statistics-Exam/mcshapiro.test.RData")
library(e1071) #for svm
library(MASS) #for lda qda
library(mixtools) #for ellipse (chisq approx)
library(car) #for vif and car::ellipse (with radius param)

# Exam 03/07/2017 kimono/hanami/geisha/garden ####
load("../../mcshapiro.test.RData")
setwd("./030717")

#1st exercise 
# Problem n.1 TWO WAYS BALANCED ANOVA
# The file kimono.txt collects the value (ke) of 528 silk kimonos sold in Mitsukoshi department stores in Kyoto and
# Tokyo, both tailor-made and ready-to-wear.
# a) Formulate an ANOVA model for the value of a kimono as a function of the factors city (Kyoto or Tokyo) and
# type (tailor-made, ready-to-wear). Verify the assumptions of the model.
# b) Through appropriate statistical tests, propose a reduced model.
# c) Provide Bonferroni intervals (global level 95%) for the differences between the mean value of kimonos belonging
# to the homogeneous groups identified by the model at point (b).

kimono <- read.table("kimono.txt")
x11()
plot(kimono$value, col=ifelse(kimono$city=="Tokyo","red","blue"))
x11()
plot(kimono$value, col=ifelse(kimono$type=="hand-made","red","blue"))

#Separate 4 groups (balanced problem)
kimono.v1 <- kimono$value[which(kimono$city=="Tokyo" & kimono$type=="hand-made")]
kimono.v2 <- kimono$value[which(kimono$city=="Tokyo" & kimono$type=="ready-to-use")]
kimono.v3 <- kimono$value[which(kimono$city=="Kyoto" & kimono$type=="hand-made")]
kimono.v4 <- kimono$value[which(kimono$city=="Kyoto" & kimono$type=="ready-to-use")]


#a) check assumptions. 
#Normality, even though we have enough data
shapiro.test(kimono$value) #not gaussian
shapiro.test(kimono.v1) #gaussian
shapiro.test(kimono.v2) #gaussian
shapiro.test(kimono.v3) #gaussian
shapiro.test(kimono.v4) #gaussian

#Same covariance
var.test(kimono.v1,kimono.v2)
var.test(kimono.v1,kimono.v3)
var.test(kimono.v1,kimono.v4)
var.test(kimono.v2,kimono.v3)
var.test(kimono.v2,kimono.v4)
var.test(kimono.v3,kimono.v4)

cov(cbind(kimono.v1,kimono.v2,kimono.v3,kimono.v4)) #look the same good to go

#ANOVA
aov.city.type <- aov(kimono$value ~ kimono$type + kimono$city + kimono$type:kimono$city)
summary(aov.city.type)

#b) reduced model
aov.type <- aov(kimono$value ~ kimono$type)
summary(aov.type)

#c) Bonferroni confidence intervals on the diff of means
k <- 1
g <- 2

n1 <- length(kimono$value[which(kimono$type == "hand-made")])
n2 <- length(kimono$value[which(kimono$type == "ready-to-use")])
n <- n1 + n2

alpha <- 0.05
qT <- qt(1 - alpha/(2*k),df = n-g)

mean <- tapply(kimono$value, kimono$type, mean)

SSres <- sum(residuals(aov.type)^2)
#equivalent: SSres2 <- (n1-1)*var(kimono$value[which(kimono$type == "hand-made")])+(n2-1)*var(kimono$value[which(kimono$type == "ready-to-use")])

Spooled = SSres/(n-g)

conf.int.bonferroni <-c(mean[1]-mean[2] - qT*sqrt(Spooled*(1/n1+1/n2)), mean[1]-mean[2] ,mean[1]-mean[2] +qT*sqrt(Spooled*(1/n1+1/n2)) ) 


#2)Problem n.2 REPEATED MEASURES
# Having picnics in parks is very common in Japan, especially for the traditional custom of Hanami (flower viewing)
# during the bloom of cherry blossoms. The file bento.txt contains the total amount [g] of rice, sashimi, vegetables
# and okashi (traditional sweets) in the bent¯o's (packed lunches) of 32 volunteer families, consumed on March 26th
# 2017 (for Hanami) and on May 7th 2017 (normal season). Assuming independent the composition of bent¯o's of
# different families and not independent that of the same family, answer the following question.
# a) Perform a statistical test to verify if there is evidence of an impact of Hanami on the mean amount of rice,
# sashimi, vegetables and okashi in families bent¯o's. Verify the needed assumptions.
# b) Provide four T2 simultaneous confidence intervals (global confidence 95%) for the INCREASE in the mean consumption of rice, sashimi, vegetables and okashi in correspondence of the bloom of cherry blossoms. Comment
# the results.

# Import dataset
bento <- read.table("bento.txt")

# Check assumptions: 
# Gaussianity
mcshapiro.test(bento) #gaussian check ok

# Transform dataset
bento.dif <- bento[c(1:4)]-bento[c(5:8)]

#Test on the mean of a gaussian with H_0: mu = c(0,0,0,0)
mu0 = c(0,0,0,0)
sample.mean = sapply(bento.dif, mean)

W <- cov(bento.dif)
invW <- solve(W)

n<- dim(bento.dif)[1]
p <- dim(bento.dif)[2]

alpha <- 0.05
thresh <- (n-1)*(p)/(n-p) * qf(1-alpha,p,n-p)

T_0 <- n*(sample.mean - mu0)%*%invW%*%(sample.mean - mu0)

reject <- T_0 > thresh
p.value <- 1-pf(T_0 * (n-p)/(p*(n-1)),p,n-p)

print(paste("Reject H0:",reject,", p-value:",p.value))


#b) T2 intervals for the increase in mean
n<- dim(bento.dif)[1]
p <- dim(bento.dif)[2]
k <- 4

alpha <- 0.05

thresh <- sqrt((n-1)*p/(n-p)*f(1-alpha,p,n-p))
rejection.region <- cbind("-inf", mu0 + thresh * sqrt(diag(W)/n))
print(cbind(rejection.region,sample.mean,ifelse(sample.mean>as.numeric(rejection.region[,2]),"Increase", "No statistical increase")))

# We also check bonferroni out of curiosity
thresh <- qt(1-alpha/(k),n-1) #we don't divide by two since we wnat to check the increase
rejection.region <- cbind("-inf", mu0 + thresh * sqrt(diag(W)/n))
print(cbind(rejection.region,sample.mean,ifelse(sample.mean>as.numeric(rejection.region[,2]),"Increase", "No statistical increase")))

# Problem n.3
# The file geisha.txt collects data about Geisha hunting in Kyoto (i.e., tours finalized to spot a Geisha). The data
# report the duration (minutes) and starting time (in minutes after 16:00) of 130 trials (not all successful).
# a) Use a hierarchical clustering method based on Euclidean distance and single linkage to identify two groups of
# data (i.e., successful and unsuccessful tours). Report the centers of the clusters, the size of the clusters, the
# cophenetic coefficient and a qualitative plot of the results.
# b) Evaluate the quality of the clustering at point (a) and, in case you deem it unsatisfactory, repeat the procedure
# with another linkage at your choice.
# c) Identify the successful tours with the smaller group found with the clustering method at point (b). Having
# introduced and verified the needed assumptions, provide 4 Bonferroni intervals (global level 90%) for the difference in the mean characteristics of successful and unsuccessful tours, 
# and for the mean characteristics of a successful tour.
# d) Comment the results at point (c) and suggest a successful strategy for Geisha hunting.

#Import dataset
geisha <- read.table("geisha.txt")

# Distance
D <- dist(geisha)

#a) Clustering single linkage
clust <- hclust(D, method="single")
plot(geisha)
groups <- cutree(clust,2)
plot(geisha, col=groups+1)

#centers
tapply(geisha$duration, groups, mean)
tapply(geisha$time, groups, mean)
#size
table(groups)
#cophenetic coefficients
cor(cophenetic(clust),D)

#b) Clustering average linkage
clust <- hclust(D, method="average")
plot(geisha)
groups <- cutree(clust,2)
plot(geisha, col=groups+1)

# centers
m1<-sapply(geisha[which(groups==1),], mean)
m2<-sapply(geisha[which(groups==2),], mean)
m1
m2

# size
table(groups)

# cophenetic coefficients
cor(cophenetic(clust),D)

#c) Bonferroni for the difference of mean of succesful vs unsuccesful tours, and for the mean of succesful
# gaussian assumption:
mcshapiro.test(geisha[which(groups==1),]) #ok
mcshapiro.test(geisha[which(groups==2),]) #ok

# same covariance assumption
bartlett.test(geisha[which(groups==1),],geisha[which(groups==2),]) #ok


fit <- manova(as.matrix(geisha)  ~ factor(groups))
summary.manova(fit,test="Wilks")

# c.1) Bonferroni confidence intervals manova
n1<- table(groups)[1]
n2<- table(groups)[2]
n = n1+n2
p = dim(geisha)[2]
g = 2
k= p*g*(g-1)/2

alpha = 0.1

qT <- qt(1-alpha/(2*k), n-g)

m1 <- sapply(geisha[which(groups==1),], mean)
m2 < -sapply(geisha[which(groups==2),], mean)
W <- summary.manova(fit)$SS$Residuals
Spooled <- W/(n-g)

conf.int.diff12 = cbind( m1-m2-qT*sqrt(diag(Spooled)*(1/n1+1/n2)),m1-m2,m1-m2+qT*sqrt(diag(Spooled)*(1/n1+1/n2)))
conf.int.diff12  

# c.2) Bonferroni conf int for the mean of the succesful groups
m1 <- sapply(geisha[which(groups==1),], mean)
W1 <- cov(geisha[which(groups==1),])
n1 <- table(groups)[1]
qT <- qt(1-alpha/(2), n-1)

conf.int.m1 = cbind( m1-qT*sqrt(diag(W1)/n1),m1,m1+qT*sqrt(diag(W1)/n1))
conf.int.m1

# Problem n.4
# The file garden.txt collects the number of carps, maple trees, cherry trees and stones, and the extension [m2
#                                                                                                            ] of
# 156 of garden in the Kant¯o region of Japan. Experts believe that, to achieve an overall balance of elements, the
# Japanese gardens follow the model
# E = ??0 + ??1 · x1 + ??2 · x2 + ??3 · x3 + ??4 · x4 + ??,
# with E the extension of the garden, x1, x2, x3, x4 the number of carps, maple trees, cherry trees and stones
# respectively, and ?? ??? N(0, ??2
# ).
# a) Estimate the 6 parameters of the model and verify the model assumptions. Evaluate the residuals of the model.
# b) Perform two statistical tests to verify if
# - there is statistical evidence of a dependence of the mean garden extension on the number of maple or cherry
# trees;
# - there is statistical evidence of a dependence of the mean garden extension on lake elements (stones, carps).
# c) Based on the results at point (b), comment on possible model weaknesses and, if needed, reduce the dimensionality of the regressors. Comments the analysis and interpret the results.
# d) Update the estimates of the parameters using the analysis at point (c).

garden <- read.table("garden.txt")
attach(garden)
n<- dim(garden)[1]

#a) linear model
g<- lm(extension ~ carps + maple + cherry + stones)
summary(g)
x11()
par(mfrow = c(2,2))
plot(g)


betas <- g$coefficients
#verification of gaussianity of residuals
shapiro.test(g$residuals)
qqnorm(g$residuals)
qqline(g$residuals)
#gaussianity holds

#homoschedasticity of residuals and lack of patterns
x11()
plot(g$fitted.values,scale(g$residuals, scale = T, center = F))
points(g$fitted.values, rep(2,n), type = 'l', col = 'red')
points(g$fitted.values, rep(-2,n), type = 'l', col = 'red')
#all good, only a few important residuals, not a problem since we have a quite large n

x11()
plot(carps,scale(g$residuals, scale = T, center = F))
points(carps, rep(2,n), type = 'l', col = 'red')
points(carps, rep(-2,n), type = 'l', col = 'red')
#all good

x11()
plot(maple,scale(g$residuals, scale = T, center = F))
points(maple, rep(2,n), type = 'l', col = 'red')
points(maple, rep(-2,n), type = 'l', col = 'red')
#all good

x11()
plot(cherry,scale(g$residuals, scale = T, center = F))
points(cherry, rep(2,n), type = 'l', col = 'red')
points(cherry, rep(-2,n), type = 'l', col = 'red')
#all good

x11()
plot(stones,scale(g$residuals, scale = T, center = F))
points(stones, rep(2,n), type = 'l', col = 'red')
points(stones, rep(-2,n), type = 'l', col = 'red')
#all good

#residual assumptions hold

#b)

#we do NOT need to apply Bonferroni correction (or any SimCI correction), they would accounto for "they both have an impact, simultaneously", we just need that either one has an effect
#test_i : H0 : beta(i)= 0 vs H1
#T-statistic : abs(beta^(i)- 0)/(S*sqrt((Z'Z)^(-1)[i,i]) ~ t(n-(r+1))
#but here summary(g) already provides the results of this test (ONE AT THE TIME)
#
#The test required is og the kind:
#test_i : H0 : (beta(maple)= 0 & beta(cherry)= 0) vs H1
#we want to reject H0 if BOTH of the one at the time reject their individual H0

#maple -> 0.06 cherry -> 0.28   neither of them reject, but, keeping in mind that these p_values are quite conservative on the assumption,
#when we apply Bonferroni we divide the p_values for keeping into account that we want them to hold simultaneously
#the correction that would be needed here is not a simple multiplicative factor (P(union) = sumP(), while P(inters) = prodP(), but under independence, more complex computations would be needed)
#anyway the correction could only correct the p-values upward, so the fact that the p-value of cherry is border-line is not a problem

#carps->0.26 stones ->0.26, neither of them reject, trivially

#So we have a model which explains 65% of the variability, but which has no statistical evidence to say that any of the regressors used has an effect
#Not surprising, indeed computing the vif:
library(regclass)
vif(g)
#we notice that we have a high collinearity of our regressors
#we can apply backward selection, excluding from the model the regressor with the highest p-value for the test of its coefficient being zero
#ONE AT THE TIME!
summary(g)
#we remove cherry
g1 <- lm(extension ~ carps + maple + stones)
summary(g1)
#we remove carps
g2 <- lm(extension ~ maple + stones)
summary(g2)
#now we have a model with a high significance of our regressors without losing in terms of p-value
#this model is valid, indeed
anova(g,g2)
#p_val = 0.5 -> the reduced model doesn't lose wrt the complete one
shapiro.test(g2$residuals)
x11()
plot(g2$fitted.values,scale(g2$residuals, scale = T, center = F))
points(g2$fitted.values, rep(2,n), type = 'l', col = 'red')
points(g2$fitted.values, rep(-2,n), type = 'l', col = 'red')
#all assumptions are still valid


#we notice that we still have in our model one variable about the lake elements (stones) and one about the trees (maple)
#there is a high pair-wise correlation of the regressors, which caused a high variability on our estimates and "masked" the actual impact of these regressors

betas_new <- c(g2$coefficients[1], 0, g2$coefficients[2], 0, g2$coefficients[3])
names(betas_new) <- c("b0", "b1", "b2", "b3", "b4")
betas_new

# Exam 18/07/2017 tourists/horsecolic/castle/albatross ####
setwd("./180717")
load("../../mcshapiro.test.RData")
albatross <- read.table("albatross.txt")
attach(albatross)

#a)
#wind.factor <- as.factor(wind)
n <- dim(albatross)[1]
wind.factor <- rep(0,n)
wind.factor[which(wind=="upwind")]=1
table(wind.factor)
Va2 <- Va^2
Vi2 <- Vi^2
lmodel <- lm(distance ~ wind.factor + Va2 + Vi2 + wind.factor:Va2 + wind.factor:Vi2)
summary(lmodel)

#Verify assumptions
residuals <- lmodel$residuals
shapiro.test(residuals)
sigma <- summary(lmodel)$sigma

x11()
plot(lmodel$fitted.values,residuals)
abline(h=2*sigma,col="red")
abline(h=-2*sigma,col="red")

x11()
plot(Va2,residuals)
abline(h=2*sigma,col="red")
abline(h=-2*sigma,col="red")

x11()
plot(Vi2,residuals)
abline(h=2*sigma,col="red")
abline(h=-2*sigma,col="red")

x11()
par(mfrow=c(2,2))
plot(lmodel)

#b) Reduce the model (see if all 6 parameters (sigma excluded) are really necessary)
# We remove wind.factor:
lmodel2 <- lm(distance ~ Va2 + Vi2 + wind.factor:Va2 + wind.factor:Vi2)
summary(lmodel2)
anova(lmodel,lmodel2) #high pvalue: no difference: good we do not lose by reducing the model

#c) use linearHypothesis command to test gamma_{g} = - beta_{g}
linHyp1 <- linearHypothesis(lmodel2,rbind(c(0,1,1,0,0),c(0,0,0,1,1)),c(0,0))
linHyp1 #good to go

Va2.Vi2
lmodel3 <- lm(albatross$distance ~ I(Va^2-Vi^2) + wind.factor:I(Va^2-Vi^2))
summary(lmodel3)

#d) prediction intervals
x.upwind <- data.frame(Va = 35, Vi=25,wind.factor=1)
x.downwind <- data.frame(Va = 35, Vi=25,wind.factor=0)

#since we are predicting 2 confidence intervals at the same time we need Bonferroni correction for alpha
alpha<-0.01/2
predict(lmodel3,x.upwind,interval = "prediction",level = 1-alpha)
predict(lmodel3,x.downwind,interval = "prediction",level = 1-alpha)


# Problem n.1
# The file tourists.txt collects data on the flow of Italian tourism from outside Lombardy to Milan for the year
# 2015. Each statistical unit corresponds to a Region of origin and a month of observation. For each unit, the
# tourists' flow is quantified through the number of nights spent by clients in: '5 stars hotels', '4 stars hotels', '3
# stars hotels', '2 stars hotels', '1 star hotels', 'residences', 'B&B' and 'rented flats'.
# a) Perform a Principal Component Analysis of the dataset, by only focusing on the quantitative variables of the
# dataset; here, evaluate whether it is appropriate to use the original variables or the standardized ones and
# proceed accordingly. Interpret, when possible, the principal components. Report the numerical value of the
# first 3 loadings and the variance displayed by the data along those directions.
# b) Report (qualitatively) the scatter plot of the data along the first two PCs and describe how to interpret the
# data clouds in the four quadrants. Use the categorical variables 'Month' and 'Region' to further interpret the
# results at point (a).
# c) Propose (with motivations) a dimension reduction of the dataset.
setwd("./180717")
tourists <- read.table("tourists.txt")

tourists.num <- tourists[c(-1,-2)]
boxplot(tourists.num)
tourists.num.scaled <- scale(tourists.num)
boxplot(tourists.num.scaled)

tourists.pca <- princomp(tourists.num.scaled)
summary(tourists.pca)
loadings<-tourists.pca$loadings
loadings
barplot(tourists.pca$loadings[,1],main="1st")
barplot(tourists.pca$loadings[,2],main="2nd")
barplot(tourists.pca$loadings[,3],main="3rd")

#plot first two scores
plot(tourists.pca$scores[,1],tourists.pca$scores[,2])
abline(h=0, v=0, lty=2, col='grey')

#Months
table(tourists$Month) #balanced
col.month <- factor(tourists$Month, labels = rainbow(12))
plot(tourists.pca$scores[,1],tourists.pca$scores[,2],col = col.month, pch=19)
# text(tourists.pca$scores[,1],tourists.pca$scores[,2],labels = tourists$Month, cex=0.75)
legend('topright',levels(as.factor(tourists[,1])),fill=rainbow(12),bty='n')

#Seasons
sort(unique(tourists$Month)) #get alphabetical order to assign color labels to the factor below
col.season <- factor(tourists$Month, labels = c(1,1,2,2,2,1,1,2,1,2,2,1)) #1=black
plot(tourists.pca$scores[,1],tourists.pca$scores[,2],col = col.season)

#Region
table(tourists$Region.of.origin) #balanced
col.region <- factor(tourists$Region.of.origin, labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
plot(tourists.pca$scores[,1],tourists.pca$scores[,2],col = col.region)
text(tourists.pca$scores[,1],tourists.pca$scores[,2],labels = tourists$Region.of.origin, cex=0.75)

#Sud/nord
sort(unique(tourists$Region.of.origin)) #get alphabetical order to assign color labels to the factor below
col.sudnord <- factor(tourists$Region.of.origin, labels = c(1,1,2,1,1,2,2,1,2,2,1,2,1,1,1,2,2,1,1,1)) #1=black
plot(tourists.pca$scores[,1],tourists.pca$scores[,2],col = col.sudnord)


x11()
boxplot(tourists.pca$scores[,1]~tourists$Region.of.origin, las=2)#Indeed there are differences in the groupings, completely explainable by the different number of residents in each region
boxplot(tourists.pca$scores[,2]~tourists$Region.of.origin, las=2)

boxplot(tourists.pca$scores[,1]~tourists$Month) #here not so much
boxplot(tourists.pca$scores[,2]~tourists$Month,outcol)

# Problem n.2
# The dataset horsecolic.txt reports the data about 300 horses that have been affected by a colic and treated
# with drugs to alleviate their pain. The included quantitative variables are: 'Rectal.temperature', 'Pulse', 'Respiratory.rate', 'Packed.cell.volume' 
# (i.e., the number of red cells by volume in the blood). The dataset also reports
# an additional 'pain' variable, that is a qualitative variable indicating whether the horse is still suffering 
# (Pain = 'Yes') or not (Pain = 'No'). The latter variable is collected through a subjective judgement of experts, based on
# the evaluation of facial expressions and a behavioral assessment.
# a) Build 5 (error in the text there are only 4) Bonferroni confidence intervals (global level 99%) for the mean difference in 'Rectal.temperature',
# 'Pulse', 'Respiratory.rate' and 'Packed.cell.volume' between the horses with pain and without pain. Comment
# the results and identify the variables along which a significant difference exists. State and verify the appropriate
# assumptions.
# b) Based only on the assumptions you deem appropriate, build a classifier for the condition 'pain', based only
# on the variables along which the groups display a significant difference according to the analysis at point (a).
# Report the mean within the groups and the prior probabilities estimated from the sample. Report a qualitative
# plot of the partition induced by the classifier in the space identified by two of the used variables.
# c) Estimate the APER of classifier.
setwd("./180717")
horsecolic <- read.table("horsecolic.txt")
horses.pain <- horsecolic[which(horsecolic$Pain == "Yes"),][-5]
horses.nopain <- horsecolic[which(horsecolic$Pain == "No"),][-5]
horses.values <- horsecolic[-5]
#Bonferroni test on the difference of mean
#Check assumption
#Normality
mcshapiro.test(horses.nopain)
mcshapiro.test(horses.pain)

#Same variance
cov(horses.nopain)
cov(horses.pain)
bartlett.test(horses.nopain,horses.pain) #bartlett with small pvalue but we proceed

#Bonferroni
g=2
p=4
n1 <- dim(horses.nopain)[1]
n2 <- dim(horses.pain)[1]
n <- n1+n2
k <- p*g*(g-1)/2

pain.factor <- factor(horsecolic[[5]])

horses.manova <- manova(as.matrix(horses.values) ~ pain.factor)

SSres <- summary.manova(horses.manova)$SS$Residuals
Spooled <- SSres/(n-g)

m.nopain <- sapply(horses.nopain,mean)
m.pain <- sapply(horses.pain,mean)

alpha <- 0.01
qT <- qt(1-alpha/2/k,n-g)

conf.int <- cbind(m.pain-m.nopain - qT*sqrt(diag(Spooled)*(1/n1+1/n2)),m.pain-m.nopain, m.pain-m.nopain + qT*sqrt(diag(Spooled)*(1/n1+1/n2)  ))
conf.int                  

#Classifier

# LDA
library(MASS) #for lda/qda
pain.factor <- factor(horsecolic[[5]])
horses.lda <- lda(as.matrix(horses.values[c(2,3)]), pain.factor)
horses.lda
classification<-predict(horses.lda)$class

# Side by side plot
x11()
par(mfrow=c(1,2))
plot(horses.values[c(2,3)],col=ifelse(classification=="Yes", 1,2),main="lda")
plot(horses.values[c(2,3)],col=ifelse(pain.factor=="Yes", 1,2),main="real pain")

# All together + class. regions plot
x11()
plot(horses.values[c(2,3)],col=ifelse(classification=="Yes", 2,3),main="lda")
points(horses.values[c(2,3)][which(classification!=pain.factor),],col="gold",pch=19)
x  <- seq(min(horsecolic[,2]), max(horsecolic[,2]), length=200)
y  <- seq(min(horsecolic[,3]), max(horsecolic[,3]), length=200)
xy <- expand.grid(Altezza=x, Peso=y)
z.q  <- predict(horses.lda, xy)$post  
z1.q <- z.q[,1] - z.q[,2]
z2.q <- z.q[,2] - z.q[,1]
contour(x, y, matrix(z1.q, 200), levels=0, drawlabels=F, add=T, lty=2)  
contour(x, y, matrix(z2.q, 200), levels=0, drawlabels=F, add=T, lty=2)


# APER
misclass = sum(classification!=pain.factor)
conf.mat<-table(true.class = pain.factor, classifier = classification)
conf.mat
prior <- horses.lda$prior
prior

APER  <- conf.mat[1,2]/(conf.mat[1,2]+conf.mat[1,1])*prior[1]+conf.mat[2,1]/(conf.mat[2,1]+conf.mat[2,2])*prior[2]
APER

# library(e1071) for svm

# Problem n.3
# The file castle.txt collects the GPS coordinates of 27 castles in the province of Aosta. Assume the data to be
# independent realizations from a bivariate Gaussian distribution.
# a) Perform a statistical test to verify if the centre of the distribution is located in the centre of Aosta (Lat = 45.733,
#                                                                                                              Long = 7.333). Verify the assumptions of the test.
# b) Consistently with the results at point (a), estimate an elliptical region that contains 95% of the castles. Report:
#   the analytical expression of the region, its centre, the direction and the length of the principal axes of the ellipse.
# Report a qualitative plot of the region.
setwd("./180717")
castle <- read.table("castle.txt")

#Center
lat <- 45.733
long <- 7.333

#check normality
mcshapiro.test(castle)$p

#analysis on the mean of bivariate gaussian
mu0 <- c(lat,long)
n <- dim(castle)[1]
p <- dim(castle)[2]

W <- cov(castle)
invW <- solve(W)
sample.mean <- sapply(castle, mean)

T_0 <- n*(sample.mean-mu0) %*% invW %*% (sample.mean-mu0) 

alpha <- 0.05
threshold <- (n-1)*p/(n-p)*qf(1-alpha,p,n-p)

T_0>threshold

library(mixtools)
plot(castle, xlim=c(45.65,45.85),ylim=c(7.25,7.45))
points(data.frame(t(mu0)),col="blue",pch=19)
points(data.frame(t(sample.mean)),col="red",pch=18)
ellipse(sample.mean,W,col="red")

eig<-eigen(W)
#the directions of the axis are the directions of the eigenvectors of W, while the length of the axis is r*sqrt(eigenvalues) where r=sqrt(threshold)
abline(a=sample.mean[2]-0.9636228/0.2672659*sample.mean[1],b=0.9636228/0.2672659 )
abline(a=sample.mean[2]+0.2672659/0.9636228*sample.mean[1],b=-0.2672659/0.9636228 )
length_axis1=sqrt(eig$values[1]*threshold)
points(sample.mean+cbind(sqrt(length_axis1^2/(1+(0.9636228/0.2672659)^2)),sqrt(length_axis1^2/(1+(0.9636228/0.2672659)^2))*0.9636228/0.2672659),col="red")

#JG: WE ARE NOT BUILDING A CONFIDENCE REGION FOR THE MEAN, BUT A PREDICTION REGION FOR THE OBSERVATION!!!
#x ~ N(mean, SIGMA) -> (X - s_mean) ~ N(0, (1+1/n)SIGMA) (sum of two gaussians) -> (X - s_mean)'inv((1+1/n)SIGMA)(X - s_mean) ~ Chisq(2) (but we do not know SIGMA)
#(X - s_mean)/sqrt((1+1/n)) ~ N(0, SIGMA), S ~ W(SIGMA/(n-1), n-1)
#((n-1)-p+1)/((n-1)*p)((X - s_mean)/sqrt((1+1/n)))'inv(S)((X - s_mean)/sqrt((1+1/n))) ~ F(p, (n-1) - p +1 )
#region {((n-1)-p+1)/((n-1)*p)((X - s_mean)/sqrt((1+1/n)))'inv(S)((X - s_mean)/sqrt((1+1/n))) <= c^2}
# = {(((n-1)-p+1)/(((n-1)*p)*(1+1/n)))*(X - s_mean)'inv(S)(X - s_mean) <= c^2}
alpha <- 0.05
radius <- sqrt(qf(1-alpha,p,n-p)/((n-p)/((n*p-p)*(1+1/n))))
center <- sample.mean

directions <- eigen(W)$vectors
length <- eigen(W)$values*radius
#if we work asymptotically
radius_chi <- sqrt(qchisq(1-alpha, 2)*(1+1/n))

x11()
plot(castle, asp=1, pch=1, main='Dataset of the Differences')
ellipse(center=smean, shape=S,radius, lwd=2, col = 'blue')
ellipse(center=smean, shape=S,radius_chi, lwd=2, col = 'red')  
#similar results with using exact of asymptotic ellipse

# Problem n.4
# The actual landing distance of an albatross can be modeled as
# Y = ??g + ??g · V
# 2
# a + ??g · V
# 2
# i + ??,
# where Va [km/h] is the approaching velocity (i.e., the velocity of the bird at a given position before landing), Vi
# [km/h] the impact velocity (i.e., the velocity of the bird when it touches the land), g = 1, 2 denotes the wind
# conditions (g = 1 for upwind, g = 2 for downwind), and ?? ??? N(0, ??2
# ). Based on the data on the landing distances
# [m] and velocities [km/h] of 100 albatross landed in Australia (file albatross.txt), answer the following questions.
# a) Estimate the 7 parameters of the model (report ??g, ??g, ??g for g = 1, 2 and ??) and verify its assumptions.
# b) Based on appropriate test(s), reduce the model.
# c) Using model (b), test the hypothesis according to which ??g = ?????g, for g = 1, 2. Possibly propose a constrained
# model and estimate its parameters.
# d) Wilbur arbatross is giving Bianca and Bernie a lift to Australia. Do you deem the landing of Bianca and Bernie
# to be safe in case of upwind or downwind wind, on a 17 m long runway, if Wilbur is approaching with Va = 35
# km/h and Vi = 25 km/h? Base your answer on model (c) and on two intervals of global level 99% for Wilbur's
# landing distance.

setwd("./180717")
albatross <- read.table("albatross.txt")



# Exam 14/09/2017 sunchair/olives/knossos/tide ####

# Problem n.1
# The file sunchair.txt collects the prices [e] of 62 sun chairs of comparable characteristics sold by a famous ecommerce company, in 4 different periods of the year: Mar-May, Jun-July, Aug-Oct, Nov-Feb. Assume the prices
# of different chairs to be independent.
# a) Through an appropriate statistical test, verify if there exist a significant variation in the mean prices of a sun
# chair during the year. Introduce and verify the needed assumptions.
# b) Use four Bonferroni intervals (global level 95%) to describe the dynamic of the mean price of a sun chair. Based
# on your analyses, suggest the best period to buy a sun chair and its expected price.

setwd("./140917")
sunchair <- read.table("sunchair.txt")
boxplot(sunchair)

#Hypothesis
#Gaussianity
shapiro.test(sunchair$Mar.May)$p
shapiro.test(sunchair$Jun.July)$p
shapiro.test(sunchair$Aug.Oct)$p
shapiro.test(sunchair$Nov.Feb)$p

sunchair.diff <- sunchair[c(1,2,3)]-sunchair[c(2,3,4)]
boxplot(sunchair.diff)

#Now it's like a test on the mean of a gaussian
mu0 <- c(0,0,0)
sample.mean <- sapply(sunchair.diff,mean)

n <- dim(sunchair.diff)[1]
qminus1 <- dim(sunchair.diff)[2]

W <- cov(sunchair.diff)
invW <- solve(W)

alpha<-0.05
thresh <- (n-1)*qminus1/(n-qminus1)*qf(1-alpha,qminus1,n-qminus1)
  
T0 <- n*(sample.mean-mu0)%*%invW%*%(sample.mean-mu0)
T0 > thresh

#b) conf int bonferroni
alpha<-0.05
k<-4
p<-4
qT <- qt(1-alpha/2/k,n-1)

sample.mean <- sapply(sunchair,mean)
S <- cov(sunchair)

conf.int <- cbind(sample.mean - qT*sqrt(diag(S)/n),sample.mean,sample.mean + qT*sqrt(diag(S)/n))
conf.int
#best to buy in nov.feb, expected price is 38.67
  
# Problem n.2
# The province of Ascoli Piceno is well-known for the olive all'ascolana (i.e., breaded and fried stuffed olives),
# hereafter named just olives. The file olives.txt reports the total weight [g], the filling weight [g] and the weight
# of the fried breading of 40 olives served in the restaurant Dalla Luigina, and of 36 olives served at the Caff´e Muletti,
# in Ascoli Piceno.
# a) Is there a significant difference (level 95%) in the mean of the total weight, the filling weight and the breading
# weight of the olives served in the two restaurants? Introduce and verify the needed assumptions.
# b) Provide T2
# intervals for the mean difference between the total weight, the filling weight and the breading weight
# of the olives served Dalla Luigina and at Caff´e Muletti. Comment the results.
setwd("./140917")


olives <- read.table("olives.txt")
attach(olives)
#MANOVA

n <- dim(olives)[1]
p <- 3
olives_L <- olives[which(Restaurant=="Dalla Luigina"),-4]
olives_M <- olives[which(Restaurant=="Caffè Muletti"),-4]
n_L <- dim(olives_L)[1]
n_M <- dim(olives_M)[1]

#gaussianity
library(mvnormtest)
mcshapiro.test(olives_L)$p#we have gaussianity
mcshapiro.test(olives_M)$p

#variance
S_L <- cov(olives_L)
S_M <- cov(olives_M)
S_L
S_M
bartlett.test(olives_L,olives_M)
var.test(as.matrix(olives[1:3])~Restaurant) #not ok

#the test we need to perform is a manova:
alpha <- 0.05
man <- manova(as.matrix(olives[,1:3])~olives[,4])
summary.manova(man)
summary.aov(man)#we have strong evidence toward saying that there is a difference in the means of the two olives served
summary.manova(man)$stats[1,6] < alpha

#test statistic: (1/n1 + 1/n2)^(-1)[(smean1 - smean2) - delta0]'(S_pooled)^-1[(smean1 - smean2) - delta0] ~ ((n1 + n2 -2)*p/(n1 + n2 - 1 - p))*F(p, n1 + n2 -2 - p +1)
#T2 intervals are just the projection of the overall confidence region
#SimCI(a'X) = [a'sample_delta_X +- sqrt(((n-1)*p/(n-p))*Quantile_Fisher_1-alpha(p,n-p))*sqrt(a'S_p a*(1/n1 + 1/n2))]

smean_L <- sapply(olives_L[,1:3], mean)
smean_M <- sapply(olives_M[,1:3], mean)
sdelta_L_M <- smean_L - smean_M 

Spooled <- ((n_L - 1)*S_L + (n_M - 1)*S_M)/(n_L + n_M - 2)
#S_pooled2 <- summary.manova(man)$SS$Residuals/(n_L + n_M - 2)

alpha <- 0.05
q.fish <- ((n_L + n_M -2)*p/(n_L + n_M - 2 - p + 1))*qf(1-alpha,p, n_L + n_M - 2 - p +1)


T2CI <- cbind(inf= sdelta_L_M - sqrt(q.fish)*sqrt(diag(Spooled)*(1/n_L +1/n_M)), point = sdelta_L_M, sup = sdelta_L_M + sqrt(q.fish)*sqrt(diag(Spooled)*(1/n_L +1/n_M)))
rownames(T2CI) <- c("Tot_L - Tot_M", "Fill_L - Fill_M", "Bread_L - Bread_M")
T2CI
#Muletti makes on average bigger olives, with more filling and bread


# Problem n.3
# Recent studies in the Knossos area have discovered numerous fragments of amphoras. The positions of these
# fragments (file knossos.txt) with respect to the centre of Knossos Palace (considered as origin of the coordinate
#   system), suggest the existence of a second site of archeological interest.
# a) Identify two clusters of locations through a hierarchical clustering method (Euclidean distance and complete
#    linkage). Report the estimate of the mean within the groups, their size, and compute the cophenetic coefficient.
# b) Assume the identified groups to be independent. Having introduced and verified the needed assumptions, test
# the hypothesis according to which only one archeological site exists in the Knossos area. Write a report of max
# 3 lines to the archeologists summarising the results of the analysis.
setwd("./140917")

knossos <- read.table("knossos.txt")
plot(knossos)
#a)
D <- dist(knossos)
clust <- hclust(D,method="complete")
clust.groups <- cutree(clust,2)
x11()
plot(knossos, col=clust.groups+1)

#centers
centers1 <- sapply(knossos[which(clust.groups==1),], mean)
centers2 <- sapply(knossos[which(clust.groups==2),], mean)

points(as.data.frame(t(centers1)),pch=19)
points(as.data.frame(t(centers2)),pch=19)

#size
table(clust.groups)

#cophenetic coefficient
cor(cophenetic(clust),D)


#b) manova (see if means (centers) are the same)
knossos1 <- knossos[which(clust.groups==1),]
knossos2 <- knossos[which(clust.groups==2),]

#check asumptions
#gaussianity
mcshapiro.test(knossos1)$p
mcshapiro.test(knossos2)$p
#same variance
bartlett.test(knossos1,knossos2)

man <- manova(as.matrix(knossos) ~ clust.groups)
summary(man)

# Problema 4
# The Venice lagoon is affected by important tide phenomena, and particularly by the so-called acqua alta. Consider
# the following model for the sea level H [cm] at Punta della Salute (in Venice) at 17:00 of each day:
#   H = ??0 + ??1 · sin 
# 2??
# 28
# t
# 
# + ??2 · sin  ??
# 365
# (t ??? t0)
# 
# + ??3 · t + ??
# where t ??? [1, 365] is the day of the year, t0 = 82 indicates the vernal equinox, and ?? ??? N(0, ??2
# ). Interpret the
# first term as due to the effect of the moon motion (astronomical tide), the second term as associated with seasonal
# effects, the third as due to the global increase of the sea level. The file tide.txt reports the sea level measured at
# 17:00 on 203 days of 2015.
# a) Estimate the five parameters of the model. Report the estimates of ??i
# , i ??? {1,2,3}, and ??.
# b) Having introduced and verified the appropriate assumptions, perform two statistical tests to verify if
# - the mean sea level is influenced by the periodic components;
# - the mean sea level depends on the global increase of the sea level.
# c) Based on point (b), propose a reduced model and estimate its parameters.
# d) Based on model (c), provide two prediction intervals (global level 90%) for the sea level at 17:00 of 20th
# September 2017 (day 263 of 2017) and of 1st December 2017 (day 335 of 2017). Comment the results knowing
# that in Venice high-water is expected whenever the sea level is higher than 90 cm.
setwd("./140917")

tide <- read.table("tide.txt")


#A)
lmod <- lm(h ~ I(sin(2*pi/28*t)) + I(sin(pi/365*(t-82))) + t ,data=tide)
summary(lmod)
sigma <- summary(lmod)$sigma

#check gaussianity of residuals suncharand plot against fitted values 

#b) we can see from summary(lmod) that for both questions at least one pvalue is above alpha
#c) remove with priority
library(car)
vif(lmod) # a  bit high for last two regressors
lmod2 <- lm(h ~ I(sin(2*pi/28*t)) + t ,data=tide)
summary(lmod2)
anova(lmod,lmod2) #high pvalue good, we don't lose a lot in sum of sq
sigma <- summary(lmod)$sigma

#d) prediciton intervals
k <- 2
alpha <- 0.1 
newdata1 <- data.frame(t=263)
newdata2 <- data.frame(t=335)
predict(lmod2,newdata1,interval = "prediction",level = 1-alpha/k) #high tide
predict(lmod2,newdata2,interval = "prediction",level = 1-alpha/k) #we can exclude hight tide at global level alpha

# Exam 28/06/2018 morning/mexican/precolombian/hotels ####
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

setwd("./280618")
morning <- read.table("Morning.txt")
evening <- read.table("Evening.txt")

#a) repeated measures
# assumptions: Gaussianity
mcshapiro.test(morning)$p
mcshapiro.test(evening)$p

flight.diff <- evening - morning

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
k <- 4
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

#c) prediction intervals
alpha <- 0.01
T2 <- sqrt((n-1)*p/(n-p)*qf(1-alpha,p,n-p))
sample.mean <- mean.morning[2]
S <- var(morning$OAX.MEX)

c(sample.mean - T2*sqrt(S*(1+1/n)), sample.mean, sample.mean + T2*sqrt(S*(1+1/n)) )
#since the prediction region includes 1:30h delay, we are not certain to reach the gate by 11:45


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
# year in which the statues were built) and the aspect ratios of the statues (ratio between their height and width).
# Knowing that, out of 100 statues found in the area, on average 20 of these are attributed to Aztecs, 10 to Maya
# and 70 to Toltecs, answer the following questions.
# a) Build a classifier for the variable civilization based on the available quantitative features. Report the mean
# within the groups identified by the variable civilization and a qualitative plot of the classification regions.
# Introduce and verify the appropriate assumptions.
# b) Compute the APER of the classifier.
# c) How would you classify a new statue dated 986 a.D. and with aspect ratio 1.4?

rm(list=setdiff(ls(), "mcshapiro.test"))

setwd("./280618")
precolombian <- read.table("Precolombian.txt")
boxplot(precolombian$Year ~ precolombian$Civilization)
boxplot(precolombian$Aspect.Ratio ~ precolombian$Civilization)

prior <- c(0.2,0.1,0.7)
library(MASS)

table(precolombian$Civilization)
maya <- precolombian[which(precolombian$Civilization=="Maya"),][-3]
aztec <- precolombian[which(precolombian$Civilization=="Aztec"),][-3]
toltec <- precolombian[which(precolombian$Civilization=="Toltec"),][-3]
# Assumptions
#normality
mcshapiro.test(maya)$p
mcshapiro.test(aztec)$p
mcshapiro.test(toltec)$p

#same variance
bartlett.test(maya,aztec) #reject
bartlett.test(maya,toltec) #reject
bartlett.test(aztec,toltec) #reject

cov(aztec)
cov(maya)
cov(toltec)

# qda since no same variance assumption met
class.model<- qda(as.matrix(precolombian[-3]), precolombian$Civilization, prior = prior)
class.model
classes <- predict(class.model)$class


x11()
par(mfrow=c(1,2))
plot(precolombian[-3],col=ifelse(classes=="Aztec", 1,ifelse(classes=="Maya", 2,3)),main="qda",pch=19)
legend(x="topleft",legend=levels(as.factor(precolombian$Civilization)),pch=19, col=c(1,2,3))
plot(precolombian[-3],col=ifelse(precolombian[3]=="Aztec", 1,ifelse(precolombian[3]=="Maya", 2,3)),main="real",pch=19)
legend(x="topleft",legend=levels(as.factor(precolombian$Civilization)),pch=19, col=c(1,2,3))

x11()
plot(precolombian[-3],col=ifelse(classes=="Aztec", 1,ifelse(classes=="Maya", 2,3)),main="lda",pch=19)
legend(x="topleft",legend=levels(as.factor(precolombian$Civilization)),pch=19, col=c(1,2,3))

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

#APER
conf.mat <- table(precolombian$Civilization,classes)
conf.mat
APER <- 12/87*prior[2]
APER

#prediction
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
# Exam 14/02/2011 ATM ####
##### Problem 2 of 14/02/2011
# The ATM is considering the possibility of including in the turnstiles optical
# readers able to measure the size [mm] of tickets.
# In order to build a suitable software, it asks you to build a classification 
# rule which minimizes the expected number of errors.
# Starting with the measures relating to 100 regular tickets (true.txt file)
# and 100 counterfeit tickets (false.txt files) and knowing that the
# 0.5% of the banknotes in circulation is counterfeit:
# a) construct an appropriate classification rule (in particular, verify the 
#    assumptions when possibile, and provide a qualitative graph of the 
#    classification regions);
# b) compute the APER and discuss its value;
# c) on the basis of the rule identified at point (a), how will be classified
#    a ticket long 85.5 mm and wide 55.0 mm? What is the probability that the 
#    ticket will be false?

true <- read.table('true.txt', header=T)
false <- read.table('false.txt', header=T)
head(true)
head(false)

fv <- as.factor(rep(c('vero','falso'), c(100,100)))
fv

biglietti <- rbind(true, false) 

### question a)
# normality
mcshapiro.test(true)
mcshapiro.test(false)

# homogeneity of covariances
S1<-cov(true)
S2<-cov(false)
x11()
par(mfrow=c(1,2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))

x11()
plot(biglietti, main='Tickets', xlab='Length', ylab='Width', pch=20)
points(false, col='red', pch=20)
points(true, col='blue', pch=20)
legend('bottomleft', legend=levels(fv), fill=c('red','blue'), cex=.7)

# The assumption of homogeneity of covariances doesn't seem to be satisfied
dev.off()
dev.off()

# QDA
qda.bigl <- qda(biglietti, fv, prior = c(0.005, 0.995))
qda.bigl # look at the order in levels to set the priors!!
Qda.bigl <- predict(qda.bigl, biglietti)
Qda.bigl

x11()
plot(biglietti, main='Tickets', xlab='Length', ylab='Width', pch=20)
points(false, col='red', pch=20)
points(true, col='blue', pch=20)
legend('bottomleft', legend=levels(fv), fill=c('red','blue'), cex=.7)
points(qda.bigl$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

x  <- seq(min(biglietti[,1]), max(biglietti[,1]), length=200)
y  <- seq(min(biglietti[,2]), max(biglietti[,2]), length=200)
xy <- expand.grid(Lunghezza=x, Larghezza=y)

z  <- predict(qda.bigl, xy)$post   
z1 <- z[,1] - z[,2]   
z2 <- z[,2] - z[,1]      

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

dev.off()

### question b)

MC <- table(classe.vera=fv, classe.allocata=Qda.bigl$class)
MC
APER <- 66/100 * 0.005 
APER

### domanda c)

predict(qda.bigl, cbind(Lunghezza = 85.5, Larghezza = 55))

# Exam 12/04/2014 neve ####
#_______________________________________________________________________________
##### Problem 3 of 12/02/2014


# To cope with the economic crisis, the Hotel Mont Blanc in Courmayeur has 
# decided to apply special discounts on their Carnival rates in case the 
# snow at skiing facilities is predicted to be of poor quality. The file neve.txt 
# reports, for the last 60 years, the data on total snowfall [cm] and the medium
# temperature [°C] recorded in the two months from December to January, together 
# with the judgment on the quality of the Carnival snow provided by the Alpine Guides 
# Society of Courmayeur.
# a) Build a classifier for the quality of the Carnival snow that minimizes the expected
#    cost of misclassification (display a qualitative graph of the classification regions)
#    when assuming that:
#    - There is no economic loss in the case in which the snow is good and no discount
#      is applied; there is no economic loss in case the snow is bad and the Hotel
#      operates the discount; there is a loss of 3000 euros in the case the snow is bad
#      and no discounts are applied; there is a loss of 2000 euros in the case the snow is 
#      good and discounts are applied;
#    - A season characterized by good snow is associated with a higher variability in
#      temperatures and in the amount of snow.
# b) Compute the APER of the classifier.
# c) Based on the estimates at point (b), estimate the expected economic loss of the 
#    classifier.
# d) Based on the classifier build at point (a) and knowing that the last two months
#    December-January a total of 200 cm of snow have fallen and the average temperature
#    was -4 °C, would you recommend to the hotel to apply the special discount of Carnival?

# question a)
neve <- read.table('neve.txt', header=T)
good<-neve[neve[,3]=='good',1:2]
bad<-neve[neve[,3]=='bad',1:2]

mcshapiro.test(good)$pvalue
mcshapiro.test(bad)$pvalue

prior <- c(dim(bad)[1]/(sum(dim(bad)[1],dim(good)[1])),dim(good)[1]/(sum(dim(bad)[1],dim(good)[1])))
pb <- prior[1]
pg <- prior[2]

c.bg <- 2000
c.gb <- 3000

# Modified prior to account for the misclassification costs
prior.c <- c(bad=pb*c.gb/(c.bg*pg+c.gb*pb),good=pg*c.bg/(c.bg*pg+c.gb*pb))
prior.c

qda.m <- qda(giudizio ~ quantita + temperatura, data=neve, prior=prior.c)
qda.m

x11()
plot(neve[,1:2], main='Snow', xlab='V1', ylab='V2', pch=20)
points(bad, col='red', pch=20)
points(good, col='blue', pch=20)
legend('bottomleft', legend=levels(neve[,3]), fill=c('red','blue'), cex=.7)

points(qda.m$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

x  <- seq(min(neve[,1]), max(neve[,1]), length=200)
y  <- seq(min(neve[,2]), max(neve[,2]), length=200)
xy <- expand.grid(quantita=x, temperatura=y)

z  <- predict(qda.m, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

# question b) [APER]
Qda.m <- predict(qda.m)
table(classe.vera=neve[,3], classe.allocata=Qda.m$class)

APER  <- (2+3)/(46+14)
APER

# question c) [Expected economic loss]
(3*c.bg+2*c.gb)/60

# question d) [Classification of 2012-2013]
z0.new <- data.frame(quantita=200, temperatura=-4)
points(z0.new, pch=4, col='springgreen', lwd=2, cex=1.5)

predict(qda.m,z0.new)$class

graphics.off()

# Exam 9/09/2009 snow ####
#_______________________________________________________________________________
##### Problem 2 of 9/09/2009


# A young statistician from Tromso wants to build a classifier able to
# predict the presence or absence of snowfall on the day D + 1 using 
# the average temperature [°C] and humidity of day G. Using 
# the data of the last 30 days (file snow.txt) and knowing that in the
# last 40 years in Tromso it has snowed an average of 207 days a year:
# a) build a classifier [report the model assumptions and provide a 
#    qualitative graph of the classification regions].
# b) Estimate the APER of classifier (a), and compare it with that of the
#    trivial classifier.
# c) The temperature and the humidity measured yesterday in Tromso are 
#    -10 Â° C and 0.75, respectively. Estimate the likelihood of snow 
#    for today by using both the classifier (a) that the trivial classifier.


snow <- read.table('snow.txt', header=TRUE)
snow

attach(snow)

i1 <- which(Snow.G.1=='no-snow')
i2 <- which(Snow.G.1=='snow')

# question a)
mcshapiro.test(snow[i1,1:2])$p
mcshapiro.test(snow[i2,1:2])$p

x11()
plot(Temperature.G,Humidity.G, pch=19, col=ifelse(Snow.G.1=='no-snow','blue','lightblue'))

dev.off()

# QDA
library(MASS)
qda.s <- qda(snow[,1:2], snow[,3], prior=c(1-207/365,207/365))
qda.s

x11()
plot(snow[,1:2], main='Snow', pch=20, col=ifelse(Snow.G.1=='no-snow','blue','lightblue'))
legend('bottomleft', legend=levels(as.factor(snow[[3]])), fill=c('blue','steelblue2'), cex=.7)

points(qda.s$means, pch=4,col=c('blue','steelblue2') , lwd=2, cex=1.5)

x  <- seq(min(snow[,1]), max(snow[,1]), length=200)
y  <- seq(min(snow[,2]), max(snow[,2]), length=200)
xy <- expand.grid(Temperature.G=x, Humidity.G=y)

z  <- predict(qda.s, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

dev.off()

# question b)
Qda.s <- predict(qda.s)
table(classe.vera=snow[,3], classe.allocata=Qda.s$class)

prior <- c(1-207/365,207/365)

APER  <- 2/10*prior[1]+1/20*prior[2]
APER

# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- prior[1]
APER.banale

# question c)
new.day <- c(Temperature.G=-10, Humidity.G=0.75)
predict(qda.s, new.day)$posterior[2]

prior.snow=207/365
prior.snow

graphics.off()
