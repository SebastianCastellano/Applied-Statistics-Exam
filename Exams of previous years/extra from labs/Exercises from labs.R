# Exam 14/02/2011 ATM ####
##### Problem 2 of 14/02/2011
# The ATM is considering the possibility of including in the turnstiles optical
# readers able to measure the size [mm] of tickets.
# In order to build a suitable software, it asks you to build a classification 
# rule which minimizes the expected number of errors.
# Starting with the measures relating to 100 regular tickets (true.txt file)
# and 100 counterfeit tickets (false.txt files) and knowing that the
# 0.5% of the banknotes in circulation is counterfeit:
# a) construct an appropriate classification rule (in particular, verify the 
#    assumptions when possibile, and provide a qualitative graph of the 
#    classification regions);
# b) compute the APER and discuss its value;
# c) on the basis of the rule identified at point (a), how will be classified
#    a ticket long 85.5 mm and wide 55.0 mm? What is the probability that the 
#    ticket will be false?

true <- read.table('true.txt', header=T)
false <- read.table('false.txt', header=T)
head(true)
head(false)

fv <- as.factor(rep(c('vero','falso'), c(100,100)))
fv

biglietti <- rbind(true, false) 

### question a)
# normality
mcshapiro.test(true)
mcshapiro.test(false)

# homogeneity of covariances
S1<-cov(true)
S2<-cov(false)
x11()
par(mfrow=c(1,2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))

x11()
plot(biglietti, main='Tickets', xlab='Length', ylab='Width', pch=20)
points(false, col='red', pch=20)
points(true, col='blue', pch=20)
legend('bottomleft', legend=levels(fv), fill=c('red','blue'), cex=.7)

# The assumption of homogeneity of covariances doesn't seem to be satisfied
dev.off()
dev.off()

# QDA
qda.bigl <- qda(biglietti, fv, prior = c(0.005, 0.995))
qda.bigl # look at the order in levels to set the priors!!
Qda.bigl <- predict(qda.bigl, biglietti)
Qda.bigl

x11()
plot(biglietti, main='Tickets', xlab='Length', ylab='Width', pch=20)
points(false, col='red', pch=20)
points(true, col='blue', pch=20)
legend('bottomleft', legend=levels(fv), fill=c('red','blue'), cex=.7)
points(qda.bigl$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

x  <- seq(min(biglietti[,1]), max(biglietti[,1]), length=200)
y  <- seq(min(biglietti[,2]), max(biglietti[,2]), length=200)
xy <- expand.grid(Lunghezza=x, Larghezza=y)

z  <- predict(qda.bigl, xy)$post   
z1 <- z[,1] - z[,2]   
z2 <- z[,2] - z[,1]      

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

dev.off()

### question b)

MC <- table(classe.vera=fv, classe.allocata=Qda.bigl$class)
MC
APER <- 66/100 * 0.005 
APER

### domanda c)

predict(qda.bigl, cbind(Lunghezza = 85.5, Larghezza = 55))

# Exam 12/04/2014 neve ####
#_______________________________________________________________________________
##### Problem 3 of 12/02/2014


# To cope with the economic crisis, the Hotel Mont Blanc in Courmayeur has 
# decided to apply special discounts on their Carnival rates in case the 
# snow at skiing facilities is predicted to be of poor quality. The file neve.txt 
# reports, for the last 60 years, the data on total snowfall [cm] and the medium
# temperature [°C] recorded in the two months from December to January, together 
# with the judgment on the quality of the Carnival snow provided by the Alpine Guides 
# Society of Courmayeur.
# a) Build a classifier for the quality of the Carnival snow that minimizes the expected
#    cost of misclassification (display a qualitative graph of the classification regions)
#    when assuming that:
#    - There is no economic loss in the case in which the snow is good and no discount
#      is applied; there is no economic loss in case the snow is bad and the Hotel
#      operates the discount; there is a loss of 3000 euros in the case the snow is bad
#      and no discounts are applied; there is a loss of 2000 euros in the case the snow is 
#      good and discounts are applied;
#    - A season characterized by good snow is associated with a higher variability in
#      temperatures and in the amount of snow.
# b) Compute the APER of the classifier.
# c) Based on the estimates at point (b), estimate the expected economic loss of the 
#    classifier.
# d) Based on the classifier build at point (a) and knowing that the last two months
#    December-January a total of 200 cm of snow have fallen and the average temperature
#    was -4 °C, would you recommend to the hotel to apply the special discount of Carnival?

# question a)
neve <- read.table('neve.txt', header=T)
good<-neve[neve[,3]=='good',1:2]
bad<-neve[neve[,3]=='bad',1:2]

mcshapiro.test(good)$pvalue
mcshapiro.test(bad)$pvalue

prior <- c(dim(bad)[1]/(sum(dim(bad)[1],dim(good)[1])),dim(good)[1]/(sum(dim(bad)[1],dim(good)[1])))
pb <- prior[1]
pg <- prior[2]

c.bg <- 2000
c.gb <- 3000

# Modified prior to account for the misclassification costs
prior.c <- c(bad=pb*c.gb/(c.bg*pg+c.gb*pb),good=pg*c.bg/(c.bg*pg+c.gb*pb))
prior.c

qda.m <- qda(giudizio ~ quantita + temperatura, data=neve, prior=prior.c)
qda.m

x11()
plot(neve[,1:2], main='Snow', xlab='V1', ylab='V2', pch=20)
points(bad, col='red', pch=20)
points(good, col='blue', pch=20)
legend('bottomleft', legend=levels(neve[,3]), fill=c('red','blue'), cex=.7)

points(qda.m$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

x  <- seq(min(neve[,1]), max(neve[,1]), length=200)
y  <- seq(min(neve[,2]), max(neve[,2]), length=200)
xy <- expand.grid(quantita=x, temperatura=y)

z  <- predict(qda.m, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

# question b) [APER]
Qda.m <- predict(qda.m)
table(classe.vera=neve[,3], classe.allocata=Qda.m$class)

APER  <- (2+3)/(46+14)
APER

# question c) [Expected economic loss]
(3*c.bg+2*c.gb)/60

# question d) [Classification of 2012-2013]
z0.new <- data.frame(quantita=200, temperatura=-4)
points(z0.new, pch=4, col='springgreen', lwd=2, cex=1.5)

predict(qda.m,z0.new)$class

graphics.off()

# Exam 9/09/2009 snow ####
#_______________________________________________________________________________
##### Problem 2 of 9/09/2009


# A young statistician from Tromso wants to build a classifier able to
# predict the presence or absence of snowfall on the day D + 1 using 
# the average temperature [°C] and humidity of day G. Using 
# the data of the last 30 days (file snow.txt) and knowing that in the
# last 40 years in Tromso it has snowed an average of 207 days a year:
# a) build a classifier [report the model assumptions and provide a 
#    qualitative graph of the classification regions].
# b) Estimate the APER of classifier (a), and compare it with that of the
#    trivial classifier.
# c) The temperature and the humidity measured yesterday in Tromso are 
#    -10 Â° C and 0.75, respectively. Estimate the likelihood of snow 
#    for today by using both the classifier (a) that the trivial classifier.


snow <- read.table('snow.txt', header=TRUE)
snow

attach(snow)

i1 <- which(Snow.G.1=='no-snow')
i2 <- which(Snow.G.1=='snow')

# question a)
mcshapiro.test(snow[i1,1:2])$p
mcshapiro.test(snow[i2,1:2])$p

x11()
plot(Temperature.G,Humidity.G, pch=19, col=ifelse(Snow.G.1=='no-snow','blue','lightblue'))

dev.off()

# QDA
library(MASS)
qda.s <- qda(snow[,1:2], snow[,3], prior=c(1-207/365,207/365))
qda.s

x11()
plot(snow[,1:2], main='Snow', pch=20, col=ifelse(Snow.G.1=='no-snow','blue','lightblue'))
legend('bottomleft', legend=levels(as.factor(snow[[3]])), fill=c('blue','steelblue2'), cex=.7)

points(qda.s$means, pch=4,col=c('blue','steelblue2') , lwd=2, cex=1.5)

x  <- seq(min(snow[,1]), max(snow[,1]), length=200)
y  <- seq(min(snow[,2]), max(snow[,2]), length=200)
xy <- expand.grid(Temperature.G=x, Humidity.G=y)

z  <- predict(qda.s, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

dev.off()

# question b)
Qda.s <- predict(qda.s)
table(classe.vera=snow[,3], classe.allocata=Qda.s$class)

prior <- c(1-207/365,207/365)

APER  <- 2/10*prior[1]+1/20*prior[2]
APER

# Trivial classifier; classifies always as the most likely class a priori
APER.banale <- prior[1]
APER.banale

# question c)
new.day <- c(Temperature.G=-10, Humidity.G=0.75)
predict(qda.s, new.day)$posterior[2]

prior.snow=207/365
prior.snow

graphics.off()
