data <- read.table("activity.txt", header = T)
attach(data)
walk <- which(activity == "walking")
sit <- which(activity == "sitting")
lay <- which(activity == "laying")

load("C:/Users/jacop/Desktop/università/da dare/Applied Statistics/AS lab/LAB_5/mcshapiro.test.RData")
mcshapiro.test(data[walk,1:2])
mcshapiro.test(data[sit,1:2])
mcshapiro.test(data[lay,1:2])
Sw <- cov(data[walk, 1:2])
Ss <- cov(data[sit, 1:2])
Sl <- cov(data[lay, 1:2])
Sw
Ss
Sl
bartlett.test(accel ~ activity)
bartlett.test(gyro ~ activity)

#we cannot assume heteroschedasticity
#we must proceed with QDA

levels(as.factor(activity))
pr <- c(9, 12, 3)/(9+12+3)

library(MASS)
q <- qda(activity ~ accel + gyro, prior = pr)
q

a <- seq(min(accel), max(accel), length = 200)
g <- seq(min(gyro), max(gyro), length = 200)
ag <- expand.grid(accel = a, gyro = g)

z <- predict(q, ag)$post

z1 <- z[,1] - pmax(z[,2],z[,3])
z2 <- z[,2] - pmax(z[,1],z[,3])

x11()
plot(data[,1:2], main = "Classification regions")
points(data[walk,1:2], col = 'red')
points(data[sit,1:2], col = 'blue')
points(data[lay,1:2], col = 'green')
contour(a, g, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(a, g, matrix(z2, 200), levels=0, drawlabels=F, add=T)


q.APER.class <- predict(q, data[1:2])$class
t <- table(true = activity, classif = q.APER.class)
gr <- dim(t)[1]
APER <- 0
for(i in 1:gr){
  APER <- APER + pr[i]*sum(t[i, -i])/sum(t[i,])
}
APER

10*0.375/150 + 5*0.5/150 + 3*0.125/150


z_new <- data.frame(accel = 0.45, gyro = 0.52)
predict(q, z_new)$post
#it's more likely that the subject is sitting

library(class)
kn_5 <- knn(train = data[,1:2], test = ag, cl = activity, k = 5, prob=T)

z_k  <- as.numeric(kn_5)

x11()
plot(data[,1:2], main = "Classification regions k = 5")
points(data[walk,1:2], col = 'red')
points(data[sit,1:2], col = 'blue')
points(data[lay,1:2], col = 'green')
contour(a, g, matrix(z1, 200), levels=0, drawlabels=F, add=T, col = 'grey')
contour(a, g, matrix(z2, 200), levels=0, drawlabels=F, add=T, col = 'grey')

contour(a, g, matrix(z_k, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)


kn_5.APER <- knn(train = data[,1:2], test = data[,1:2], cl = activity, k = 5)
t.k <- table(true = activity, classif = kn_5.APER)
gr.k <- dim(t)[1]
error.rate.k <- sum(kn_5.APER!=activity)/length(activity)
APER.k <- 0
for(i in 1:gr.k){
  APER.k <- APER.k + pr[i]*sum(t.k[i, -i])/sum(t.k[i,])
}
APER
APER.k
error.rate.k
#As expected a k-mean classifier with a low enough k has a smaller error rate on the training set,
#indeed it has the tendency to interpolate data
#however by looking at the plot of the classification regions we can see how the shapes are similar, at least around the training set

#If we are just considering the error rate on the training set, as expected, knn has an excellent performance but, again, it has tendency to overfit the training set itself

