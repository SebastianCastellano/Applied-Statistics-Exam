load("~/GitHub/Applied-Statistics-Exam/mcshapiro.test.RData")
X<- read.table("~/GitHub/Applied-Statistics-Exam//Exams of previous years/2017/2017-07-18/horsecolic.txt") #classes=2
X<- read.table("~/GitHub/Applied-Statistics-Exam//Exams of previous years/2022/2022-06-16/Exercise 2/musicCountry.txt",header = TRUE) 

X.values <- X[c(1,2)]
X.classes <- factor(X[[3]])
x11()
plot(X.values,col=ifelse(X.classes==levels(as.factor(X.classes))[1], "red","blue"),main="True classification")
legend('topright',levels(as.factor(X.classes)),fill=c("red","blue"),bty='n')

#a) Assumptions:  gaussianity (qda/lda) and same variance of the groups (lda only)
mcshapiro.test(X.values[which(X.classes==levels(X.classes)[1]),])$p
mcshapiro.test(X.values[which(X.classes==levels(X.classes)[2]),])$p
bartlett.test(X.values[which(X.classes==levels(X.classes)[1]),],X.values[which(X.classes==levels(X.classes)[2]),])
cov(X.values[which(X.classes==levels(X.classes)[1]),])
cov(X.values[which(X.classes==levels(X.classes)[2]),])

#b) LDA
library(MASS) #for lda/qda
lda.mod <- lda(as.matrix(X.values), X.classes) # remember to add prior if needed
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

#c) QDA
library(MASS) #for lda/qda
qda.mod <- qda(as.matrix(X.values), X.classes) # remember to add prior if needed
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

#d) APER
misclass <- sum(classification!=X.classes)
conf.mat<-table(true.class = X.classes, classifier = classification)
conf.mat
prior <- qda.mod$prior
APER  <- conf.mat[1,2]/(conf.mat[1,2]+conf.mat[1,1])*prior[1]+conf.mat[2,1]/(conf.mat[2,1]+conf.mat[2,2])*prior[2]
APER
# APER <- 0
# for(g in 1:G)
#   APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]  

#e) AER through leave-one-out cross-validation
errors_CV <- 0
n <- dim(X)[1]
for(i in 1:n){
  modCV.i <- qda(X.values[-i,], X.classes[-i], prior=prior)
  errors_CV <- errors_CV + as.numeric(predict(modCV.i,X.values[i,])$class != X.classes[i])
}
errors_CV
AERCV   <- sum(errors_CV)/n
AERCV

#f)Probability of a new sample being classified as class i
i <- 1
Prob <- conf.mat[i,i]/(sum(conf.mat[i,]))*prior[i] + t(conf.mat[-i,i]/(table(X.classes)[-i]))%*%prior[-i]
Prob



# library(e1071) for svm
