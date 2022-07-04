load("../../mcshapiro.test.RData")

#1st exercise
# Problem n.1
# The file kimono.txt collects the value (ke) of 528 silk kimonos sold in Mitsukoshi department stores in Kyoto and
# Tokyo, both tailor-made and ready-to-wear.
# a) Formulate an ANOVA model for the value of a kimono as a function of the factors city (Kyoto or Tokyo) and
# type (tailor-made, ready-to-wear). Verify the assumptions of the model.
# b) Through appropriate statistical tests, propose a reduced model.
# c) Provide Bonferroni intervals (global level 95%) for the differences between the mean value of kimonos belonging
# to the homogeneous groups identified by the model at point (b).



kimono <- read.table("kimono.txt")
plot(kimono$value, col=ifelse(kimono$city=="Tokyo","red","blue"))
plot(kimono$value, col=ifelse(kimono$type=="hand-made","red","blue"))

#Separate 4 groups (balanced)
kimono.v1 <- kimono$value[which(kimono$city=="Tokyo" & kimono$type=="hand-made")]
kimono.v2 <- kimono$value[which(kimono$city=="Tokyo" & kimono$type=="ready-to-use")]
kimono.v3 <- kimono$value[which(kimono$city=="Kyoto" & kimono$type=="hand-made")]
kimono.v4 <- kimono$value[which(kimono$city=="Kyoto" & kimono$type=="ready-to-use")]


#a) check assumptions. 
#Normality, even though we have enough data
shapiro.test(kimono$value) #not gaussian
shapiro.test(kimono.v1) #gaussian
shapiro.test(kimono.v2) #gaussian
shapiro.test(kimono.v3) #gaussian
shapiro.test(kimono.v4) #gaussian

#Same covariance
var.test(kimono.v1,kimono.v2)
var.test(kimono.v1,kimono.v3)
var.test(kimono.v1,kimono.v4)
var.test(kimono.v2,kimono.v3)
var.test(kimono.v2,kimono.v4)
var.test(kimono.v3,kimono.v4)

cov(cbind(kimono.v1,kimono.v2,kimono.v3,kimono.v4)) #look the same good to go

#ANOVA
aov.city.type <- aov(kimono$value ~ kimono$type + kimono$city + kimono$type:kimono$city)
summary(aov.city.type)

#b) reduced model
aov.type <- aov(kimono$value ~ kimono$type)
summary(aov.type)

#c) Bonferroni confidence intervals on the diff of means
k <- 1
g <- 2

n1 <- length(kimono$value[which(kimono$type == "hand-made")])
n2 <- length(kimono$value[which(kimono$type == "ready-to-use")])
n <- n1 + n2

alpha <- 0.05
qT <- qt(1 - alpha/(2*k),df = n-g)

mean <- tapply(kimono$value, kimono$type, mean)
W <- cov(as.matrix(kimono$value))

SSres <- sum(residuals(aov.type)^2)
#equivalent: SSres2 <- (n1-1)*var(kimono$value[which(kimono$type == "hand-made")])+(n2-1)*var(kimono$value[which(kimono$type == "ready-to-use")])

Spooled = SSres/(n-g)

conf.int <-c(mean[1]-mean[2] - qT*sqrt(Spooled*(1/n1+1/n2)), mean[1]-mean[2] ,mean[1]-mean[2] +qT*sqrt(Spooled*(1/n1+1/n2)) ) 


#2)Problem n.2
# Having picnics in parks is very common in Japan, especially for the traditional custom of Hanami (flower viewing)
# during the bloom of cherry blossoms. The file bento.txt contains the total amount [g] of rice, sashimi, vegetables
# and okashi (traditional sweets) in the bent¯o's (packed lunches) of 32 volunteer families, consumed on March 26th
# 2017 (for Hanami) and on May 7th 2017 (normal season). Assuming independent the composition of bent¯o's of
# different families and not independent that of the same family, answer the following question.
# a) Perform a statistical test to verify if there is evidence of an impact of Hanami on the mean amount of rice,
# sashimi, vegetables and okashi in families bent¯o's. Verify the needed assumptions.
# b) Provide four T
# 2
# simultaneous confidence intervals (global confidence 95%) for the increase in the mean consumption of rice, sashimi, vegetables and okashi in correspondence of the bloom of cherry blossoms. Comment
# the results.

bento <- read.table("bento.txt")
bento.dif <- bento[c(1:4)]-bento[c(5:8)]
#Repeated measures setting
#assumptions: only gaussianity.

mcshapiro.test(bento.dif) #gaussian check ok

#now it's the same as a test on the mean of a gaussian with H_0: mu = c(0,0,0,0)
mu0 = c(0,0,0,0)
sample.mean = sapply(bento.dif, mean)
W <- cov(bento.dif)
invW <- solve(W)
n<- dim(bento.dif)[1]
alpha <- 0.05
p <- dim(bento.dif)[2]

T_0 <- n*(sample.mean - mu0)%*%invW%*%(sample.mean - mu0)

thresh <- (n-1)*(p)/(n-p) * qf(1-alpha,p,n-p)
reject <- T_0 > thresh

p.value <- 1-pf(T_0 * (n-p)/(p*(n-1)),p,n-p)

#b) Bonferroni 
k <- 4
alpha <- 1-0.95
qT <- qt(1-alpha/(k), n-1) #notice thisi is unilateral since we just want to test the increment in consumption during hanami

rejection.region <- cbind("-inf", mu0 + qT * sqrt(diag(W)/n))
print(cbind(rejection.region,sample.mean) )


# Problem n.3
# The file geisha.txt collects data about Geisha hunting in Kyoto (i.e., tours finalized to spot a Geisha). The data
# report the duration (minutes) and starting time (in minutes after 16:00) of 130 trials (not all successful).
# a) Use a hierarchical clustering method based on Euclidean distance and single linkage to identify two groups of
# data (i.e., successful and unsuccessful tours). Report the centers of the clusters, the size of the clusters, the
# cophenetic coefficient and a qualitative plot of the results.
# b) Evaluate the quality of the clustering at point (a) and, in case you deem it unsatisfactory, repeat the procedure
# with another linkage at your choice.
# c) Identify the successful tours with the smaller group found with the clustering method at point (b). Having
# introduced and verified the needed assumptions, provide 4 Bonferroni intervals (global level 90%) for the difference in the mean characteristics of successful and unsuccessful tours, and for the mean characteristics of a
# successful tour.
# d) Comment the results at point (c) and suggest a successful strategy for Geisha hunting.

geisha <- read.table("geisha.txt")


D <- dist(geisha)

#a)
clust <- hclust(D, method="single")
plot(geisha)
groups <- cutree(clust,2)
plot(geisha, col=groups+1)

#centers
tapply(geisha$duration, groups, mean)
tapply(geisha$time, groups, mean)
#size
table(groups)
#cophenetic coefficients
cor(cophenetic(clust),D)

#b)
clust <- hclust(D, method="average")
plot(geisha)
groups <- cutree(clust,2)
plot(geisha, col=groups+1)

#centers
tapply(geisha$duration, groups, mean)
tapply(geisha$time, groups, mean)
#size
table(groups)
#cophenetic coefficients
cor(cophenetic(clust),D)

#c) Bonferroni for the difference and the mean of succesful
#gaussian assumption:
mcshapiro.test(geisha[which(groups==1),]) #ok
mcshapiro.test(geisha[which(groups==2),]) #ok

#same covariance assumption
bartlett.test(geisha[which(groups==1),],geisha[which(groups==2),]) #ok


fit <- manova(as.matrix(geisha)  ~ factor(groups))
summary.manova(fit,test="Wilks")

#Bonferroni
g=2
n1<- table(groups)[1]
n2<- table(groups)[2]
n = n1+n2
p = dim(geisha)[2]
g = 2
k= p*g*(g-1)/2
alpha = 0.1

qT <- qt(1-alpha/(2*k), n-g)

m1 = c(tapply(geisha$duration, groups, mean)[1],tapply(geisha$time, groups, mean)[1])
m2 = c(tapply(geisha$duration, groups, mean)[2],tapply(geisha$time, groups, mean)[2])
W <- summary.manova(fit)$SS$Residuals

conf.int.diff12 = cbind( m1-m2-qT*sqrt(diag(W)/(n-g)*(1/n1+1/n2)),m1-m2,m1-m2+qT*sqrt(diag(W)/(n-g)*(1/n1+1/n2)))
conf.int.diff12  

# test for the mean of the succesful groups
m1
var1 <- cov(geisha[which(groups==1),])
n1
qT <- qt(1-alpha/(2), n-1)

conf.int.m1 = cbind( m1-qT*sqrt(diag(var1)/n1),m1,m1+qT*sqrt(diag(var1)/n1))
conf.int.m1
