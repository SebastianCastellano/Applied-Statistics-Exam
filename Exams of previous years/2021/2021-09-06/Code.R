# TDE 06.09.2021

setwd("~/Desktop/Poli/4° anno/Applied statistics/Old exams/060921")
load("~/Desktop/Poli/4° anno/Applied statistics/Old exams/mcshapiro.test.RData")

#===============================================================================

# EXERCISE 1

# Pinna Nobilis is the largest species of bivalves in the Mediterranean Sea. 
# Their size is an index of how clean and unpolluted the waters are. At the Miramare 
# protected area in Trieste, a sample of 82 specimens of Pinna Nobilis have been measured: 
# both the height and the maximum width (both measure in centimetres) are reported in the
# le pinnanobilis.txt.
# a) Identify possible clusters within the data using a hierarchical clustering algorithm 
#    (Euclidean distance, complete linkage). Provide the plot of the dendrogram and 
#    qualitatively identify the optimal number of clusters.
# b) Assuming that the clusters identified at point a) have the same covariance structure,
#    formulate a MANOVA model for the geometrical features (height and width) of the 
#    specimens of Pinna Nobilis as a function of the clustering membership. Report the 
#    formulation of the model, the estimates of the paramenters and verify the
#    assumptions. Is there statistical evidence to state that the membership to a cluster 
#    has an effect on the mean features of the specimens of Pinna Nobilis?
# c) Provide confidence intervals for the differences between the mean features of specimens 
#    of Pinna Nobilis belonging to the identified clusters. Use a Bonferroni correction 
#    to ensure a 90% global level. Use the computed intervals to comment about the differences
#    among the clusters.

pinnanobilis <- read.table('pinnanobilis.txt', header=T)

# Point a

pinna.c <- dist(pinnanobilis, method='euclidean')
pinna.ec <- hclust(pinna.c, method='complete')

x11()
plot(pinna.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
# => The optimal number of clusters is k=2
rect.hclust(pinna.ec, k=2)

# Point b

# We have to do a one-way manova 

cluster.ec <- cutree(pinna.ec, k=2)      # euclidean-complete:

i1 <- which(cluster.ec==1)
i2 <- which(cluster.ec==2)

# Verify the assumptions:
# 1) gaussianity
mcshapiro.test(pinnanobilis[i1,])
mcshapiro.test(pinnanobilis[i2,])
# The assumption is verified 
# 2) homoscedasticity 
S  <-  cov(pinnanobilis)
S1 <-  cov(pinnanobilis[i1,])
S2 <-  cov(pinnanobilis[i2,])
x11(width=21)
par(mfrow=c(1,2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
# They are quite similar, so the assumption is verified 

### Model: X.ij = mu + tau.i + eps.ij; eps.ij~N_p(0,Sigma), X.ij, mu, tau.i in R^2
### Test:
### H0: tau.1 = tau.2  = (0,0)'
### H1: (H0)^c

fit <- manova(as.matrix(pinnanobilis) ~ cluster.ec)
summary.manova(fit,test="Wilks")

# Parameters
fit$coefficients   

summary.aov(fit)
# The p-values are small, so there is evidence that the membership to a cluster has a effect on the mean features of the specimens of Pinna Nobilis 

# Point c

p=2 
g=2
n1=length(pinnanobilis[i1,1])
n2=length(pinnanobilis[i2,1])
n=n1+n2
alpha <- 0.1
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

W <- summary.manova(fit)$SS$Residuals
m  <- sapply(pinnanobilis,mean)         # estimates mu
m1 <- sapply(pinnanobilis[i1,],mean)    # estimates mu.1=mu+tau.1
m2 <- sapply(pinnanobilis[i2,],mean)    # estimates mu.2=mu+tau.2

inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )   
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
center <- m1-m2

CI <- list(height_width=cbind(inf12, center, sup12))
CI
# There isn't the 0 in any IC, so they are all significant

# Another way:
# S_p <- ((n1 - 1)*S1 + (n2 - 1)*S2)/(n1 + n2  - 2)
# band <- sqrt(c(diag(S_p)*(1/n1 + 1/n2),diag(S_p)*(1/n1 + 1/n2)))*qT
# centers<- m1-m2
# CI <- cbind(inf = centers - band,
#             centers = centers,
#             sup = centers + band)
# CI

#===============================================================================

# EXERCISE 2

# The WWF is studying the quality of the waters of the Italian seas. To compare the 
# Adriatic and Tyrrhenian seas, the WWF researchers perform identical and independent 
# measurements on 8 protected sea areas of the Adriatic sea and on 10 protected sea 
# areas of the Mediterranean sea. The file waterquality.txt contains the values of 7
# pollution parameters measured by the researchers on the 18 sea areas. The higher the 
# pollution parameter, the more polluted the water.
# a) For each pollution parameter, perform a permutation one-sided test to look for 
#    possible statistical superiority of the quality of the waters of the Tyrrhenian sea. 
#    In detail, for each pollution parameter, use the difference of the sample means 
#    as test statistic and use 5000 random permutations with random seed equal to 2021 to 
#    estimate the permutational distribution. Report the value of the 7 test statistics
#    and their corresponding p-values.
# b) For which pollution parameters the waters of the Tyrrhenian sea can be considered 
#   superior to the waters of the Adriatic sea if the researchers want to limit the false 
#   discovery rate to a maximum value of 5%?
# c) For which pollution parameters the waters of the Tyrrhenian sea can be considered 
#    superior to the waters of the Adriatic sea if the researchers want to impose a 
#    probability at most 5% that at least one of the non-superior purity parameter is 
#    judged as superior?

data <- read.table('waterquality.txt', header=T)
n <- dim(data)[1]
p <- dim(data)[2]
mod <- c(rep(1, 8), rep(0,10))
adriatic <- data[which(mod==1),]
tirrenic <- data[which(mod==0),]
B <- 5000
na <- dim(adriatic)[1]
nt <- dim(tirrenic)[1]

# Point a

T_0 <- rep(NULL, p)
p_val <- rep(NULL, p)
# Test:
# H0: media_adriatic <= media_tirrenic vs H1
# T_stat = media_adriatic - media_tirrenic    -> we reject H0 when it is high
# We permute over the realization of the results of the stress test for each unit, 
# regardless of whether it was modified or not

for(i in 1:p){
  T_0[i] <- mean(adriatic[,i]) - mean(tirrenic[,i])
  x_base <- data[,i]
  set.seed(2021)
  T_stat <- rep(NULL, B)
  for(j in 1:B){
    perm <- sample(1:n)
    x_perm <- x_base[perm]
    T_stat[j] <- mean(x_perm[1:na]) - mean(x_perm[(na+1):n])
  }
  p_val[i] <- sum(T_stat >= T_0[i])/B
}

T_0
p_val
# We reject for i=2, i=7

# Point b

x11( width=14, height=7)
alpha=0.05
plot(1:7, p_val, ylim=c(0,1), type='b', pch=16, col='grey55', xlab='pairs treat',
     main='P-values')
abline(h=alpha, lty=2)

# Correction according to the false discovery rate (Benjamini-Hockberg)
p.fdr <- p.adjust(p_val, 'fdr')
p.fdr
lines(1:7, p.fdr, col='red', pch=16, type='b')

# Point c

# ????????????????





#===============================================================================

# EXERCISE 3 

# You and your closest friends have decided to rent a boat for your next holiday 
# in Greece. The file boats.txt reports data on boat rental collected from a website. 
# The dataset reports, for 80 search results, the price of the boat rental [e], 
# the length of the boat [meters], the engine power [KWatt], the draught (depth of 
# water needed to float a ship) [meters], the number of crew members, the year of 
# construction and the deck material (wood or fiberglass).
# a) Formulate a linear regression model for the price of the boat rental, as a 
#    function of all the other variables. Include in the model a possible dependence 
#    of the price of the boat rental on the categorical variable `deck material', 
#    but only in the intercept. Report the model and its parametrization, together 
#    with the estimates of all its parameters. Verify the model assumptions.
# b) Using the appropriate statistical test, state if you deem necessary to include 
#    into the model the variables related to the dimension of the boat (i.e. the 
#   length of the boat, the engine power and the draught).
# c) Using the appropriate statistical test, state if you deem necessary to include 
#    into the model the variables related to accessory features (i.e. number of 
#    crew members and deck material).
# d) Based on appropriate test(s), reduce the model and update the model parameters.
# e) You want to rent a boat with the following characteristics: length of the 
#    boat = 10 meters, engine power = 1070 KWatt, draught = 1.5 meters, number of 
#    crew members = 1, year of construction = 2015 and deck material = fiberglass. 
#    Using the last model, compute a pointwise estimate and a prediction interval 
#    of level 95% for the price of the boat rental.

data <- read.table('boats.txt', header=T)

attach(data)

# Point a

# Model:
# price = beta_0 + beta_1*length + beta_2*power + beta_3*draught + beta_4*crew + beta_5*year + beta_6*material + Eps

# Creation of a dummy for 'material'
i1 <- which(material=='wood')
i2 <- which(material=='fiberglass')
data[i1,7] <- 0
data[i2,7] <- 1

fm <- lm(price ~ length + power + draught + crew + year + material)
summary(fm) 

# Parameters:
coefficients(fm)

# Verify the assumptions
x11()
par(mfrow=c(2,2))
plot(fm)   
shapiro.test(residuals(fm))
# The assumptions are verified (although the tales of the qqplot are non perfectly on the line)

# Point b

linearHypothesis(fm, rbind(c(0,1,0,0,0,0,0), c(0,0,1,0,0,0,0), c(0,0,0,1,0,0,0)), c(0,0,0))
summary(fm)
# We have evidence that it is necessary to include into the model the variables 'length', 'power' and 'draught'
# since the p-value is very low and they are significant (see the *)

# Point c

linearHypothesis(fm, rbind(c(0,0,0,0,1,0,0), c(0,0,0,0,0,0,1)), c(0,0))
summary(fm)
# We have evidence that it is necessary to include into the model the variables 'crew' and 'material'
# since the p-value is very low and they are significant (see the *)

# Point d 

fm <- lm(price ~ length + power + draught + crew + year + material)
summary(fm) 

linearHypothesis(fm, rbind(c(0,0,0,1,0,0,0)), c(0))
summary(fm)
# The p-value is high and from the summary we see 'draught' is not significant, so we remove it 

fm <- lm(price ~ length + power + crew + year + material)
summary(fm) 

linearHypothesis(fm, rbind(c(0,0,0,0,1,0)), c(0))
summary(fm)
# The p-value is almost 0.05, so if we are considering 5% test the variable 'year' is significant, 
# but if we are considering 1% it is not and we remove it (also from the summary we see it is not significant)

fm <- lm(price ~ length + power + crew + material)
summary(fm) 
# All the variables are significant and the p-value of the test F is very small
# => we have found the reduced model 

# Parameters:
coefficients(fm)

# Point e 

z0 <- data.frame(length=10, power=1070, crew=1, material=as.character(1))
Pred <- predict(fm, z0, interval='prediction', level=1-0.05) 
Pred
# estimate = 2517.115
# IP = [2204.685 , 2829.544]

detach(data)

#===============================================================================

# EXERCISE 4

library(fda)
library(rgl)

data <- read.table('wind.txt', header=T)
data <- t(data)

# Point a 

m <- 3         # spline order 
degree <- 2    # spline degree  -> 4
nbasis <- 12

abscissa <- 1:24
basis <- create.bspline.basis(rangeval=c(1,24), nbasis=nbasis, norder=m)
names(basis)
x11()
plot(basis, main='Basis')
x11()
matplot(data, type='l', main='Data', xlab='hour', ylab='volume')
# Smoothed data
data.fd <- Data2fd(y = data, argvals = abscissa, basisobj = basis)
x11()
plot.fd(data.fd, main = 'Smooted curves',xlab='hour',ylab='volume')

coeffs_day1 <- data.fd$coefs[1:3,1]
coeffs_day1

# Point b 

pc <- pca.fd(data.fd,nharm=3,centerfns=TRUE)
cum_prop <- cumsum(pc$varprop)
cum_prop

# Scree plot
x11()
layout(cbind(1,2))
plot(pc$values/sum(pc$values), pch = 5)
plot(cumsum(pc$values)/sum(pc$values), type = 'l', ylim = c(0,1))
abline(h = 0.8, col = 'blue')

#Plot of the first 3 eigenfunctions 
x11()
layout(rbind(1,2,3))
plot(pc$harmonics[1,],col=1,ylab='FPC1')
plot(pc$harmonics[2,],col=2,ylab='FPC2')
plot(pc$harmonics[3,],col=2,ylab='FPC3')

# Point c 

# We can reduce the data by taking only the first and second principal components, 
# which explain the 90% of the variability

media <- mean.fd(data.fd)
x11()
plot(media,lwd=2,ylab='volume',main='FPC1')
lines(media+pc$harmonics[1,]*sqrt(pc$values[1]), col=2)   # red
lines(media-pc$harmonics[1,]*sqrt(pc$values[1]), col=3)   # green
# The red ones are the most windy days (positive), while the green ones are the less windy during all the day 
x11()
plot(media,lwd=2,ylab='volume',main='FPC2')
lines(media+pc$harmonics[2,]*sqrt(pc$values[2]), col=2)  # red
lines(media-pc$harmonics[2,]*sqrt(pc$values[2]), col=3)  # greenn
# The red ones are the most windy in the first hours of the day, while the green ones in the second half of the day
x11()
plot(media,lwd=2,ylab='volume',main='FPC3')
lines(media+pc$harmonics[3,]*sqrt(pc$values[3]), col=2)  # red
lines(media-pc$harmonics[3,]*sqrt(pc$values[3]), col=3)  # green
# There's less wind when there are more red ones, opposite for the green ones 

# Point d 

x11()
plot( pc$scores[,1],pc$scores[,2])
points(pc$scores[1,1],pc$scores[1,2],col='red')

# The second principal component is big, so there's wind mostly in the second half of the day



