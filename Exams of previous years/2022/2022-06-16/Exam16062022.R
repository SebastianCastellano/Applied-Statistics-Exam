load("C:/Users/sebas/OneDrive/Desktop/Poli 2021-2022/Applied Statistics/Esame/mcshapiro.test.RData")
library(e1071) #for svm
library(MASS) #for lda qda
library(mixtools) #for ellipse (chisq approx)
library(car) #for vif and car::ellipse (with radius param)

#Exercise 1 ####
discomaniac<-read.table("discomaniac.txt",header=TRUE)
lipsticks <- read.table("lipsticks.txt",header=TRUE)

#a) repeated measures
disco.p.m <- discomaniac[c(3,4)]
lip.p.m <- lipsticks[c(3,4)]

dif <- disco.p.m - lip.p.m
n <- 20
p <- 2

sample.mean <- sapply(dif,mean)
S <- cov(dif)
invS <- solve(S)
mu0 <- c(0,0)

alpha <- 0.05
q.fish <- (n-1)*p/(n-p)*qf(1-alpha,p,n-p)

T0 <- n*(sample.mean-mu0)%*%invS%*%(sample.mean-mu0)
T0>q.fish # TRUE

#b) assumptions
mcshapiro.test(lip.p.m)$p
mcshapiro.test(disco.p.m)$p

#c)
x11()
plot(dif, asp=1, pch=1, main='Dataset of the Differences')
ellipse(center=sample.mean, shape=S,radius=sqrt(q.fish/n), lwd=2, col = 'blue')
points(0,0,pch=19,col=)

#d)
k<-4
qT <- qt(1-alpha/2/k,n-1)


conf.int.mean <- cbind(sample.mean - qT*sqrt(diag(S)/n), sample.mean, sample.mean + qT*sqrt(diag(S)/n))
conf.int.mean

q.chisq1 <- qchisq(alpha/2/k,n-1)
q.chisq2 <- qchisq(1-alpha/2/k,n-1)

conf.int.cov <- cbind(diag(S)*(n-1)/q.chisq2, diag(S),diag(S)*(n-1)/q.chisq1)
conf.int.cov



#Exercise 2 ####
musicCountry<-read.table("musicCountry.txt",header=TRUE)

music <- musicCountry[c(1,2)] 
prior <- c(0.1,0.9)
boxplot(music)

music <- data.frame(scale(music))

#Check normality
mcshapiro.test(music)$p # not ok bu there are some outliers so probably ok

#Check variance
cov(music) #ok we usa lda

class.model<- lda(as.matrix(music), musicCountry$release.country, prior = prior)
class.model
classes <- predict(class.model)$class


x11()
plot(music,col=ifelse(classes=="Germany", 1,2),main="lda",pch=19)
legend(x="topleft",legend=levels(as.factor(musicCountry$release.country)),pch=19, col=c(1,2))

x  <- seq(min(music[,1]), max(music[,1]), length=200)
y  <- seq(min(music[,2]), max(music[,2]), length=200)
xy <- expand.grid(price=x, average.length=y)


z  <- predict(class.model, xy)$post   
z1 <- z[,1] - z[,2]   
z2 <- z[,2] - z[,1]      

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

#b)
errors_CV <- 0
for(i in 1:150){
  LdaCV.i <- lda(music, musicCountry$release.country, prior=prior)
  errors_CV <- errors_CV + as.numeric(predict(LdaCV.i,music[i,])$class != musicCountry$release.country[i])
}
errors_CV

AERCV   <- sum(errors_CV)/188
AERCV

#c) 
table(classes)

#d) prediction
(50-mean(musicCountry$price))/var(musicCountry$price)
(3.5-mean(musicCountry$average.length))/var(musicCountry$average.length)
predict(class.model,data.frame(price=0.048,average.length=-0.3654))
points(data.frame(Year=986,Aspect.Ratio=1.4),col="gold",pch=4,lwd=2)



#Exercise 3 ####

#Exercise 3
danceability <- read.table("danceability.txt",header=TRUE)
attach(danceability)
#a)
lmod <- lm(danceability$danceability ~ loudness + energy + tempo)
summary(lmod)
lmod$coefficients
sigma <- summary(lmod)$sigma

#b)
x11()
par(mfrow=c(2,2))
plot(lmod )

x11()
plot(loudness,lmod$residuals)

x11()
plot(energy,lmod$residuals)

x11()
plot(tempo,lmod$residuals)

shapiro.test(lmod$residuals)

#c)
linearHypothesis(lmod,rbind(c(0,1,0,0),c(0,0,1,0)),c(0,0))

#d)
vif(lmod)
lmod2 <- lm(danceability$danceability ~ loudness + tempo)
anova(lmod,lmod2)
summary(lmod2)
lmod2$coefficients

#e)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
fm16.1mer <- lmer(danceability ~ loudness + tempo + (1|genre),data=danceability)

summary(fm16.1mer)

sigma2_eps <- as.numeric(get_variance_residual(fm16.1mer))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(fm16.1mer)) 
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE 

## visualization of the random intercepts with their 95% confidence intervals
dotplot(ranef(fm16.1mer, condVar=T))
