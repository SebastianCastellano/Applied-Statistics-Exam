X <- read.table("~/GitHub/Applied-Statistics-Exam//Exams of previous years/2017/2017-07-18/tourists.txt")

#a) Scaling
X.numeric <- X[-c(1,2)]
boxplot(X.numeric)
X.scaled<- scale(X.numeric)
boxplot(X.scaled)

#b) PCA
pca <- princomp(X.scaled)
summary(pca)
loadings<-pca$loadings
loadings
barplot(loadings[,1],main="1st")
barplot(loadings[,2],main="2nd")
barplot(loadings[,3],main="3rd")

#c) Plot first two scores
plot(pca$scores[,1],pca$scores[,2])
abline(h=0, v=0, lty=2, col='grey')

#d) Color based on a factor (e.g Months)
table(X$Month) #balanced
col.month <- factor(X$Month, labels = rainbow(12))
plot(pca$scores[,1],pca$scores[,2],col = col.month, pch=19)
# text(tourists.pca$scores[,1],tourists.pca$scores[,2],labels = tourists$Month, cex=0.75)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',levels(as.factor(X[,1])),fill=rainbow(12),bty='n')