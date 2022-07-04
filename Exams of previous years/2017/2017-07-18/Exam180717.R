load("../../mcshapiro.test.RData")
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

tourists <- read.table("tourists.txt")

tourists.num <- tourists[c(-1,-2)]
boxplot(tourists.num)
tourists.num.scaled <- scale(tourists.num)
boxplot(tourists.num.scaled)

tourists.pca <- princomp(tourists.num.scaled)
summary(tourists.pca)
tourists.pca$loadings
barplot(tourists.pca$loadings[1,])
barplot(tourists.pca$loadings[2,])

plot(tourists.pca$scores[,1],tourists.pca$scores[,2])
plot(tourists.pca$scores[,1],tourists.pca$scores[,2])



# Problem n.2
# The dataset horsecolic.txt reports the data about 300 horses that have been affected by a colic and treated
# with drugs to alleviate their pain. The included quantitative variables are: 'Rectal.temperature', 'Pulse', 'Respiratory.rate', 'Packed.cell.volume' (i.e., the number of red cells by volume in the blood). The dataset also reports
# an additional 'pain' variable, that is a qualitative variable indicating whether the horse is still suffering (Pain =
#                                                                                                                  'Yes') or not (Pain = 'No'). The latter variable is collected through a subjective judgement of experts, based on
# the evaluation of facial expressions and a behavioral assessment.
# a) Build 5 Bonferroni confidence intervals (global level 99%) for the mean difference in 'Rectal.temperature',
# 'Pulse', 'Respiratory.rate' and 'Packed.cell.volume' between the horses with pain and without pain. Comment
# the results and identify the variables along which a significant difference exists. State and verify the appropriate
# assumptions.
# b) Based only on the assumptions you deem appropriate, build a classifier for the condition 'pain', based only
# on the variables along which the groups display a significant difference according to the analysis at point (a).
# Report the mean within the groups and the prior probabilities estimated from the sample. Report a qualitative
# plot of the partition induced by the classifier in the space identified by two of the used variables.
# c) Estimate the APER of classifier.

horsecolic <- read.table("horsecolic.txt")
horses.pain <- horsecolic[which(horsecolic$Pain == "Yes"),][-5]
horses.nopain <- horsecolic[which(horsecolic$Pain == "No"),][-5]

#Bonferroni test on the difference of mean
#Check assumption
#Normality
mcshapiro.test(horses.nopain)
mcshapiro.test(horses.pain)

#Same variance
cov(horses.nopain)
cov(horses.pain)
bartlett.test(horses.nopain,horses.pain) #boh

#Bonferroni
g=2
p=4
n1 <- dim(horses.nopain)[1]
n2 <- dim(horses.pain)[1]
n <- n1+n2
k <- p*g*(g-1)/2

pain.factor <- factor(horsecolic[[5]])

horses.manova <- manova(as.matrix(horses.values) ~ factor(horsecolic[[5]]))

SSres <- summary.manova(horses.manova)$SS$Residuals
Spooled <- SSres/(n-g)

m.nopain <- sapply(horses.nopain,mean)
m.pain <- sapply(horses.pain,mean)

alpha <- 0.01
qT <- qt(1-alpha/2/k,n-g)

conf.int <- cbind(m.pain-m.nopain - qT*sqrt(diag(Spooled)*(1/n1+1/n2)),m.pain-m.nopain, m.pain-m.nopain + qT*sqrt(diag(Spooled)*(1/n1+1/n2)  ))
conf.int                  

#Classifier
library(MASS) #for lda/qda

pain.factor <- factor(horsecolic[[5]])

horses.lda <- lda(as.matrix(horses.values[c(2,3)]), pain.factor)
horses.lda
classification<-predict(horses.lda)$class
x11()
par(mfrow=c(1,2))
plot(horses.values[c(2,3)],col=ifelse(classification=="Yes", 1,2),main="lda")
plot(horses.values[c(2,3)],col=ifelse(pain.factor=="Yes", 1,2),main="real pain")

horses.qda <- qda(as.matrix(horses.values[c(2,3)]), pain.factor)
horses.qda
classification<-predict(horses.qda)$class
x11()
par(mfrow=c(1,2))
plot(horses.values[c(2,3)],col=ifelse(classification=="Yes", 1,2),main="lda")
plot(horses.values[c(2,3)],col=ifelse(pain.factor=="Yes", 1,2),main="real pain")

misclass = sum(classification!=pain.factor)
misclass
APER <- misclass/n
APER

#library(e1071) for svm

# Problem n.3
# The file castle.txt collects the GPS coordinates of 27 castles in the province of Aosta. Assume the data to be
# independent realizations from a bivariate Gaussian distribution.
# a) Perform a statistical test to verify if the centre of the distribution is located in the centre of Aosta (Lat = 45.733,
#                                                                                                              Long = 7.333). Verify the assumptions of the test.
# b) Consistently with the results at point (a), estimate an elliptical region that contains 95% of the castles. Report:
#   the analytical expression of the region, its centre, the direction and the length of the principal axes of the ellipse.
# Report a qualitative plot of the region.

castle <- read.table("castle.txt")

lat <- 45.733
long <- 7.333

#check normality
mcshapiro.test(castle)

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

plot(castle, xlim=c(45.69,45.76),ylim=c(7.2,7.45))
points(data.frame(t(mu0)),col="blue",pch=19)
points(data.frame(t(sample.mean)),col="red",pch=18)
ellipse(sample.mean,W,col="red")

eigen(W)
