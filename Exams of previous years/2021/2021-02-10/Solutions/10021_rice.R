setwd("./2021/100221")
rice <- read.table("Rice.txt",header=TRUE)
#we first control dimensionality of the two components -> to see if we need to standardize
summary(rice$major_axis)
summary(rice$eccentricity)
boxplot(rice)
#comparable -> no need for standardization

plot(rice)

n <- dim(rice)[1]
p <- dim(rice)[2]

diss <- dist(rice, method = 'euclidian')
x11()
image(1:n, 1:n, as.matrix(diss))


link <- hclust(diss, method = 'complete')
x11()
plot(link,main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
clust <- cutree(link,k= 3)
length(which(clust == 1))
length(which(clust == 2))
length(which(clust == 3))
#1st species -> 39
#2nd species -> 21
#3rd species -> 21
#not the best division, quite unstable wrt the 2/4 label division, but we know there are three species
mean1 <- colMeans(as.matrix(rice[which(clust == 1),]))
mean2 <- colMeans(as.matrix(rice[which(clust == 2),]))
mean3 <- colMeans(as.matrix(rice[which(clust == 3),]))
x11()
plot(rice, main = 'Clusters')
points(rice[which(clust == 1),], col = 'red')
points(rice[which(clust == 2),], col = 'blue')
points(rice[which(clust == 3),], col = 'green')
#clear that complete linkage is missing the true clusters, not surprising since it tries to create spherical structures
#here single linkage might work fine, since the clusters are made of points connected wrt each other, but an average linkage might better take into account the cloud division of our data

#moreover we already noticed from the dendogram that three clusters has an unstable division

link_s <- hclust(diss, method = 'single')
x11()
plot(link_s,main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

link_a <- hclust(diss, method = 'average')
x11()
plot(link_a,main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

#average linkage provides a stable division in three clusters, we work with that
clust_new <- cutree(link_a,k= 3)
n1 <- length(which(clust_new == 1))
n2 <- length(which(clust_new == 2))
n3 <- length(which(clust_new == 3))
#1st species -> 42
#2nd species -> 18
#3rd species -> 21
mean1_new <- colMeans(as.matrix(rice[which(clust_new == 1),]))
mean2_new <- colMeans(as.matrix(rice[which(clust_new == 2),]))
mean3_new <- colMeans(as.matrix(rice[which(clust_new == 3),]))
x11()
plot(rice, main = 'Clusters new')
points(rice[which(clust_new == 1),], col = 'red')
points(rice[which(clust_new == 2),], col = 'blue')
points(rice[which(clust_new == 3),], col = 'green')

#exactly what we want

rice1 <- rice[which(clust_new == 1),]
rice2 <- rice[which(clust_new == 2),]
rice3 <- rice[which(clust_new == 3),]


load("C:/Users/jacop/Desktop/università/da dare/Applied Statistics/AS lab/LAB_5/mcshapiro.test.RData")
library(mvnormtest)

#we test for gaussianity

mcshapiro.test(rice1)
mcshapiro.test(rice2)
mcshapiro.test(rice3)


#we can assume gaussianity

#
# I DO NOT REMEMBER WHETER THERE SHOULD BE OTHER ASSUMPTIONS OR GAUSSIANITY IS SUFFICIENT
#technically we are assuming the units are independent but we do not really have a way to test it (as far as I rembember)

cov1 <- cov(rice1)
cov2 <- cov(rice2)
cov3 <- cov(rice3)

alpha <- 0.05

#we take one at the time and apply bonferroni correction
#CI(mean of major axis for clust (i))[mean(i)_new[1] +- sqrt(cov(i)[1,1]/n(i))*quantile(t_student, n(i)-)], 1-alpha/2)]
#CI(variance for clust (i))[(n(i)-1)*cov(i)[1,1]/quantile(Chi2, n-1, 1-alpha/2); (n(i)-1)*cov(i)[1,1]/quantile(Chi2, n-1,alpha/2)]
#since (n-1)*S ~ sigma^2*Chi2(n-1)

CI.mean <- cbind(inf = t(cbind(mean1_new[1],mean2_new[1],mean3_new[1])) - sqrt(t(cbind(cov1[1,1]*qt(1-alpha/6, df = n1)/n1,cov2[1,1]*qt(1-alpha/6, df = n2)/n2,cov3[1,1]*qt(1-alpha/6, df = n3)/n3))),
                 center = t(cbind(mean1_new[1],mean2_new[1],mean3_new[1])),
                 sup = t(cbind(mean1_new[1],mean2_new[1],mean3_new[1]) + sqrt(cbind(cov1[1,1]*qt(1-alpha/6, df = n1)/n1,cov2[1,1]*qt(1-alpha/6, df = n2)/n2,cov3[1,1]*qt(1-alpha/6, df = n3)/n3))))
CI.var <- cbind(inf = t(cbind(cov1[1,1]*(n1-1)/qchisq(1-alpha/6,n1-1),cov2[1,1]*(n2-1)/qchisq(1-alpha/6,n2-1),cov3[1,1]*(n3-1)/qchisq(1-alpha/6,n3-1))),
                center = t(cbind(cov1[1,1],cov2[1,1],cov3[1,1])),
                sup = t(cbind(cov1[1,1]*(n1-1)/qchisq(alpha/6,n1-1),cov2[1,1]*(n2-1)/qchisq(alpha/6,n2-1),cov3[1,1]*(n3-1)/qchisq(alpha/6,n3-1))))
                 
                