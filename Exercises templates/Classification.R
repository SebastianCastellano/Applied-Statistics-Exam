load("~/GitHub/Applied-Statistics-Exam/mcshapiro.test.RData")
X<- read.table("~/GitHub/Applied-Statistics-Exam//Exams of previous years/2017/2017-07-18/horsecolic.txt")
X.values <- X[c(2,3)]
X.classes <- factor(X[[5]])
x11()
plot(X.values,col=ifelse(X.classes==levels(as.factor(X.classes))[1], "red","blue"),main="True classification")
legend('topright',levels(as.factor(X.classes)),fill=c("red","blue"),bty='n')

#a) LDA
library(MASS) #for lda/qda
# Assumptions: gaussianity and same variance of the groups
mcshapiro.test(X.values[which(X.classes=="Yes"),])$p
mcshapiro.test(X.values[which(X.classes=="No"),])$p
bartlett.test(X.values[which(X.classes=="Yes"),],X.values[which(X.classes=="No"),])
cov(X.values[which(X.classes=="Yes"),])
cov(X.values[which(X.classes=="No"),])
# Model
lda.mod <- lda(as.matrix(X.values), X.classes)
lda.mod
classification <-predict(lda.mod)$class
# Plot:
x11()
plot(X.values,col=ifelse(classification==levels(as.factor(X.classes))[1], "red","blue"),main="lda")
legend('topright',levels(as.factor(X.classes)),fill=c("red","blue"),bty='n')
points(X.values[which(classification==levels(as.factor(X.classes))[1] & X.classes==levels(as.factor(X.classes))[2]),],col="red",pch=19) #misclassified 1-> true 2
points(X.values[which(classification==levels(as.factor(X.classes))[2] & X.classes==levels(as.factor(X.classes))[1]),],col="blue",pch=19) #misclassified 2 -> true 1
# Separation lines:
x  <- seq(min(X.values[,1]), max(X.values[,1]), length=200)
y  <- seq(min(X.values[,2]), max(X.values[,2]), length=200)
xy <- expand.grid(Altezza=x, Peso=y)
z.q  <- predict(lda.mod, xy)$post  
z1.q <- z.q[,1] - z.q[,2]
z2.q <- z.q[,2] - z.q[,1]
contour(x, y, matrix(z1.q, 200), levels=0, drawlabels=F, add=T, lty=2)  
contour(x, y, matrix(z2.q, 200), levels=0, drawlabels=F, add=T, lty=2)

#a) QDA
library(MASS) #for lda/qda
# Assumptions: gaussianity
mcshapiro.test(X.values[which(X.classes=="Yes"),])$p
mcshapiro.test(X.values[which(X.classes=="No"),])$p
bartlett.test(X.values[which(X.classes=="Yes"),],X.values[which(X.classes=="No"),])
cov(X.values[which(X.classes=="Yes"),])
cov(X.values[which(X.classes=="No"),])
# Model
qda.mod <- qda(as.matrix(X.values), X.classes)
qda.mod
classification <-predict(qda.mod)$class
# Plot:
x11()
plot(X.values,col=ifelse(classification==levels(as.factor(X.classes))[1], "red","blue"),main="qda")
legend('topright',levels(as.factor(X.classes)),fill=c("red","blue"),bty='n')
points(X.values[which(classification==levels(as.factor(X.classes))[1] & X.classes==levels(as.factor(X.classes))[2]),],col="red",pch=19) #misclassified 1-> true 2
points(X.values[which(classification==levels(as.factor(X.classes))[2] & X.classes==levels(as.factor(X.classes))[1]),],col="blue",pch=19) #misclassified 2 -> true 1
# Separation lines:
x  <- seq(min(X.values[,1]), max(X.values[,1]), length=200)
y  <- seq(min(X.values[,2]), max(X.values[,2]), length=200)
xy <- expand.grid(Altezza=x, Peso=y)
z.q  <- predict(qda.mod, xy)$post  
z1.q <- z.q[,1] - z.q[,2]
z2.q <- z.q[,2] - z.q[,1]
contour(x, y, matrix(z1.q, 200), levels=0, drawlabels=F, add=T, lty=2)  
contour(x, y, matrix(z2.q, 200), levels=0, drawlabels=F, add=T, lty=2)

#b) APER
misclass <- sum(classification!=X.classes)
conf.mat<-table(true.class = X.classes, classifier = classification)
conf.mat
prior <- lda.mod$prior
APER  <- conf.mat[1,2]/(conf.mat[1,2]+conf.mat[1,1])*prior[1]+conf.mat[2,1]/(conf.mat[2,1]+conf.mat[2,2])*prior[2]
APER

# library(e1071) for svm
