load("~/GitHub/Applied-Statistics-Exam/mcshapiro.test.RData")
X <- read.table("~/GitHub/Applied-Statistics-Exam//Exams of previous years/2017/2017-07-03/geisha.txt")
if (dim(X)[2]=2)
  x11()
  plot(X)

#a) Hierarchical clustering
D <- dist(X) #distance matrix
clust <- hclust(D, method="average") #linkages: single,complete, average
x11()
plot(clust) #dendogram
groups <- cutree(clust,2)
#centers:
c1 <- sapply(X[which(groups==1),], mean)
c2 <-sapply(X[which(groups==2),], mean)
if (dim(X)[2]=2){
  x11()
  plot(X, col=groups+1) #red=group 1, green=group 2
  points(c1,c2)
} 
#sizes:
table(groups)
#cophenetic coefficient:
cor(cophenetic(clust),D)

#c) Bonferroni conf. int. for the difference of mean of groups
#Assumptions (gaussianity and same variance)
mcshapiro.test(X[which(groups==1),])$p 
mcshapiro.test(X[which(groups==2),])$p 
bartlett.test(X[which(groups==1),],X[which(groups==2),])

man <- manova(as.matrix(X)  ~ factor(groups))
summary.manova(man,test="Wilks")
n1<- table(groups)[1]
n2<- table(groups)[2]
n <- n1+n2
p <- dim(X)[2]
g <- 2
k <- p*g*(g-1)/2
alpha = 0.05
qT <- qt(1-alpha/2/k, n-g)
m1 <- sapply(X[which(groups==1),], mean)
m2 <- sapply(X[which(groups==2),], mean)
S <- summary.manova(man)$SS$Residuals
Spooled <- S/(n-g)
conf.int.diff12 = cbind(inf=m1-m2-qT*sqrt(diag(Spooled)*(1/n1+1/n2)),dif_mean=m1-m2,sup=m1-m2+qT*sqrt(diag(Spooled)*(1/n1+1/n2)))
conf.int.diff12  

#d) Bonferroni conf. int. for the mean of one of the groups
G <- X[which(groups==1),]
n <- dim(G)[1]
p <- dim(G)[2]
sample.mean <- sapply(G, mean)
S <- cov(G)
invS <- solve(S)
alpha <- 0.05
qT <- qt(1-alpha/2/p,n-1)
conf.int.B <- cbind(inf=sample.mean - qT*sqrt(diag(S)/n),mean=sample.mean,sup=sample.mean + qT*sqrt(diag(S)/n))