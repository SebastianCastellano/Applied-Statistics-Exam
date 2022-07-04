load("../../mcshapiro.test.RData")

# Problem n.1
# The file sunchair.txt collects the prices [e] of 62 sun chairs of comparable characteristics sold by a famous ecommerce company, in 4 different periods of the year: Mar-May, Jun-July, Aug-Oct, Nov-Feb. Assume the prices
# of different chairs to be independent.
# a) Through an appropriate statistical test, verify if there exist a significant variation in the mean prices of a sun
# chair during the year. Introduce and verify the needed assumptions.
# b) Use four Bonferroni intervals (global level 95%) to describe the dynamic of the mean price of a sun chair. Based
# on your analyses, suggest the best period to buy a sun chair and its expected price.

sunchair <- read.table("sunchair.txt")

sunchair.diff <- sunchair[c(1,2,3)]-sunchair[c(2,3,4)]

#Assumptions for repeated measures
#Normality (and independence)
mcshapiro.test(sunchair) #ok

#Now it's like a test on the mean of a gaussian
mu0 <- cbind(0,0,0)
sample.mean <- sapply(sunchair.diff,mean)

n <- dim(sunchair.diff)[1]
qminus1 <- dim(sunchair.diff)[2]

W <- cov(sunchair.diff)
invW <- solve(W)

# Problem n.2
# The province of Ascoli Piceno is well-known for the olive all'ascolana (i.e., breaded and fried stuffed olives),
# hereafter named just olives. The file olives.txt reports the total weight [g], the filling weight [g] and the weight
# of the fried breading of 40 olives served in the restaurant Dalla Luigina, and of 36 olives served at the Caff´e Muletti,
# in Ascoli Piceno.
# a) Is there a significant difference (level 95%) in the mean of the total weight, the filling weight and the breading
# weight of the olives served in the two restaurants? Introduce and verify the needed assumptions.
# b) Provide T
# 2
# intervals for the mean difference between the total weight, the filling weight and the breading weight
# of the olives served Dalla Luigina and at Caff´e Muletti. Comment the results.


olives <- read.table("olives.txt")
attach(olives)
n <- dim(olives)[1]
p <- 3
olives_L <- olives[which(Restaurant=="Dalla Luigina"),]
olives_M <- olives[which(Restaurant=="Caffè Muletti"),]
n_L <- dim(olives_L)[1]
n_M <- dim(olives_M)[1]

#we first of all need to check gaussianity
library(mvnormtest)
mcshapiro.test(olives_L[,1:3])#we have gaussianity
mcshapiro.test(olives_M[,1:3])

S_L <- cov(olives_L[,1:3])
S_M <- cov(olives_M[,1:3])
S_L
S_M
x11()
image(1:p,1:p, S_L)
x11()
image(1:p,1:p, S_M)
bartlett.test(olives[,1:3], Restaurant)
#we can work with homoschedasticity

#the test we need to perform is a manova:
alpha <- 0.05
man <- manova(as.matrix(olives[,1:3])~olives[,4])
summary.manova(man)
summary.aov(man)
#we have strong evidence toward saying that there is a difference in the means of the two olives served
summary.manova(man)$stats[1,6] < alpha

#test statistic: (1/n1 + 1/n2)^(-1)[(smean1 - smean2) - delta0]'(S_pooled)^-1[(smean1 - smean2) - delta0] ~ ((n1 + n2 -2)*p/(n1 + n2 - 1 - p))*F(p, n1 + n2 -2 - p +1)
#T2 intervals are just the projection of the overall confidence region
#SimCI(a'X) = [a'sample_delta_X +- sqrt(((n-1)*p/(n-p))*Quantile_Fisher_1-alpha(p,n-p))*sqrt(a'S_p a*(1/n1 + 1/n2))]
smean_L <- sapply(olives_L[,1:3], mean)
smean_M <- sapply(olives_M[,1:3], mean)
sdelta_L_M <- smean_L - smean_M 
S_pooled <- ((n_L - 1)*S_L + (n_M - 1)*S_M)/(n_L + n_M - 2)
#S_pooled2 <- summary.manova(man)$SS$Residuals/(n_L + n_M - 2)
cfr.fisher <- ((n_L + n_M -2)*p/(n_L + n_M - 1 - p))*qf(1-alpha,p, n_L + n_M - 1 - p)

increment <- sqrt(cfr.fisher*diag(S_pooled)*(1/n_M + 1/n_L))
T2CI <- cbind(inf= sdelta_L_M - increment, point = sdelta_L_M, sup = sdelta_L_M + increment)
rownames(T2CI) <- c("Tot_L - Tot_M", "Fill_L - Fill_M", "Bread_L - Bread_M")
T2CI
#Muletti makes on average bigger olives, with more filling


# Problem n.3
# Recent studies in the Knossos area have discovered numerous fragments of amphoras. The positions of these
# fragments (file knossos.txt) with respect to the centre of Knossos Palace (considered as origin of the coordinate
#                                                                            system), suggest the existence of a second site of archeological interest.
# a) Identify two clusters of locations through a hierarchical clustering method (Euclidean distance and complete
#                                                                                 linkage). Report the estimate of the mean within the groups, their size, and compute the cophenetic coefficient.
# b) Assume the identified groups to be independent. Having introduced and verified the needed assumptions, test
# the hypothesis according to which only one archeological site exists in the Knossos area. Write a report of max
# 3 lines to the archeologists summarising the results of the analysis.

#a)
knossos <- read.table("knossos.txt")

D <- dist(knossos)
clust <- hclust(D,method="complete")
clust.groups <- cutree(clust,2)
x11()
plot(knossos, col=clust.groups+1)

centers1 <- sapply(knossos[which(clust.groups==1),], mean)
centers2 <- sapply(knossos[which(clust.groups==2),], mean)

points(as.data.frame(t(centers1)),pch=19)
points(as.data.frame(t(centers2)),pch=19)

table(clust.groups)

cor(cophenetic(clust),D)


#b) manova
knossos1 <- knossos[which(clust.groups==1),]
knossos2 <- knossos[which(clust.groups==2),]

#check asumptions
#gaussianity
mcshapiro.test(knossos1)
mcshapiro.test(knossos2)
#same variance
bartlett.test(knossos1,knossos2)

man <- manova(as.matrix(knossos) ~ clust.groups)
summary(man)
