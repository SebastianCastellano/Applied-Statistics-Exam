load("~/GitHub/Applied-Statistics-Exam/mcshapiro.test.RData")

# Problem n.1
# The file IAMG.txt collects the data regarding the participation to the annual meetings of the International Association for 
# Mathematical Geosciences (IAMG), in the last 50 years. For each meeting, it reports the number of
# registered participants, the number of oral presentations and the number of no-show (i.e., the number of registered
#     participants that did not show up). Call X the vector whose components are the number of registered participants
# (X1), of oral presentations (X2) and of no-show (X3), and assume each meeting to be independent of the others.
# a) Build a confidence region (level 95%) for the mean of X. Characterize the region by reporting its expression,
# its center, the direction of the axes and the length of the semi-axes.
# b) Build three T2-simultaneous confidence intervals (level 95%) for: the mean number of registered participants,
# the mean number of oral presentations and the mean number of no-show.
# c) Perform a test of level 95% to verify the hypothesis according to which, in mean, only 90% of the registered
# participants actually show up at IAMG meetings.

IAMG <- read.table("IAMG.txt")
boxplot(IAMG)

#a) 95% percent conf region for mean of multivariate gaussian
n <- dim(IAMG)[1]
p <- dim(IAMG)[2]
sample.mean <- sapply(IAMG, mean)
S <- cov(IAMG)
invS <- solve(S)
alpha <- 0.05
qfish <- (n-1)*p/(n-p)*qf(1-alpha,p,n-p) 
#The analytical expression is (x-sample.mean)%*%invS%*%(x-sample.mean) <= qfish/n
#notice that we divide qfish by n one extra time, this comes form the variance which is S/n
center <- sample.mean 
radius <- sqrt(qfish/n)
axes <- eigen(S)$vectors
lenghts <- sqrt(eigen(S)$values)*radius

#b)T2 intervals (same alpha)
T2 <- cbind(inf=sample.mean - sqrt(qfish)*sqrt(diag(S)/n),mean=sample.mean,sup=sample.mean + sqrt(qfish)*sqrt(diag(S)/n))

#c)test H0: 0.1*registered - no_show = 0, bilateral alpha=0.05
R <- cbind(0.1,0,1)
mu0 <- 0
qT <- qt(1-alpha/2,n-1)
T.stat <- sqrt(n*(R%*%sample.mean-mu0)%*%solve(R%*%S%*%t(R))%*%(R%*%sample.mean-mu0))
T.stat > qT #true=reject H0
pvalue <- 1-pt(T.stat,n-1)


# Problem n.2
# The file Waiting.txt reports the waiting times for food - i.e., the time between the order of a course and the
# service - in 180 restaurants in Romania. The dataset also reports the type of course (starter, main course or
#     dessert) and the location of the restaurant (Iasi or Bucarest).
# a) Propose a complete ANOVA model for the waiting time as a function of the factors course (starters, main
#    course or dessert) and city (Iasi or Bucarest). Report and verify the assumptions of the model.
# b) Comment on the significance of the factors and of their interaction. If needed, propose a reduced model.
# c) Build Bonferroni confidence intervals (global level 95%) for the mean differences between the waiting times in
# the groups identified at point (b), and for the variances of the waiting times within the groups. Comment the
# results.

Waiting <- read.table("Waiting.txt")
attach(Waiting)
boxplot(waiting ~ course) 
boxplot(waiting ~ city)
boxplot(waiting ~ course+ city,las=2)

#a) two-ways anova
an <- aov(waiting ~ course+ city + course:city)
summary(an)
#assumptions are gaussianity of each group and same variance
#verification of assumptions:
shapiro.test(an$residuals)
x11()
qqnorm(an$residuals)
qqline(an$residuals)
#we can assume gaussianity IF we can also assure homoschedasticity amongst groups
var.test(waiting[which(city == "Iasi" & course == "Starter")], waiting[which(city == "Iasi" & course == "Main")])
var.test(waiting[which(city == "Iasi" & course == "Starter")], waiting[which(city == "Iasi" & course == "Dessert")])
var.test(waiting[which(city == "Iasi" & course == "Main")], waiting[which(city == "Iasi" & course == "Dessert")])
var.test(waiting[which(city == "Iasi" & course == "Starter")], waiting[which(city == "Bucarest" & course == "Starter")])
var.test(waiting[which(city == "Iasi" & course == "Starter")], waiting[which(city == "Bucarest" & course == "Main")])
var.test(waiting[which(city == "Iasi" & course == "Starter")], waiting[which(city == "Bucarest" & course == "Dessert")])
var.test(waiting[which(city == "Iasi" & course == "Main")], waiting[which(city == "Bucarest" & course == "Starter")])
var.test(waiting[which(city == "Iasi" & course == "Main")], waiting[which(city == "Bucarest" & course == "Main")])
var.test(waiting[which(city == "Iasi" & course == "Main")], waiting[which(city == "Bucarest" & course == "Dessert")])
var.test(waiting[which(city == "Iasi" & course == "Dessert")], waiting[which(city == "Bucarest" & course == "Starter")])
var.test(waiting[which(city == "Iasi" & course == "Dessert")], waiting[which(city == "Bucarest" & course == "Main")])
var.test(waiting[which(city == "Iasi" & course == "Dessert")], waiting[which(city == "Bucarest" & course == "Dessert")])
var.test(waiting[which(city == "Bucarest" & course == "Starter")], waiting[which(city == "Bucarest" & course == "Dessert")])
var.test(waiting[which(city == "Bucarest" & course == "Starter")], waiting[which(city == "Bucarest" & course == "Main")])
var.test(waiting[which(city == "Bucarest" & course == "Dessert")], waiting[which(city == "Bucarest" & course == "Main")])
#we have homoschedasticity (kind of)

#b)the only significant factor is the course, we reduce progressively
an2 <- aov(waiting ~ course+ course:city)
summary(an2)

an3 <- aov(waiting ~ course)
summary(an3)

#c)
n<-length(waiting)
g<-3
smean <- tapply(waiting,course,mean) # in alphabetical order so dessert, Main, Starter
Spooled <- sum((an3$residuals)^2)/(n-g)
n1 <- length(which(course=="Starter"))
n2 <- length(which(course=="Main"))
n3 <- length(which(course=="Dessert")) #balanced
alphaB <- 0.05/6
#smean_a - smean_b ~ N(mu_a - mu_b, simga^2*(1/n_a + 1/n_b)) = N(mu_a - mu_b, 2*simga^2/n)
#(smean_a - smean_b)/sqrt(2*sigma^2/n) ~ N(0,1)
# s = SS_res/(n-g) ~ sigma^2*Chi_square(n-g)
#(smean_a - smean_b)/sqrt(2*s/n) ~ t(n-g)
band <- qt(1-alphaB/2, an_new$df)*sqrt(s*2/n)
sdeltamean <- c(smean[1] - smean[2], smean[2] - smean[3], smean[3] - smean[1])
CI1 <- cbind(inf = sdeltamean - band,
             center = sdeltamean,
             sup = sdeltamean + band)
CI1
SS <- c(sum((an_new$residuals[which(course == "Main")])^2),sum((an_new$residuals[which(course == "Starter")])^2), sum((an_new$residuals[which(course == "Dessert")])^2))
chi_up <- qchisq(1-alphaB/2, n)
chi_down <- qchisq(alphaB/2, n)
CI2 <- cbind(inf = SS/chi_up,
             center = SS/(n-1),
             sup = SS/chi_down)
rownames(CI2)<-c("Main", "Starter", "Dessert")
CI2


# Problem n.3
# The file Sailing.txt collects the data on the daily values of consumed water [l/day] and sailing time [min/day]
# for 120 sailing cruises in Croatia, in August 2018. It also reports whether the sailboat was occupied by expert
# sailors (seadog) or by inexperienced vacationers (vacationer ). It has been estimated that in August, on average,
# only 20% of sailboats are occupied by expert sailors.
# a) Based on the available features, build two Bayes classifiers, A and B, for the kind of sailboat' occupation
# (vacationer, seadog), by assuming that:
#   A. the two populations are Gaussian with the same covariance structure;
# B. the two populations are Gaussian with different covariance structures.
# For each classifier report a qualitative plot of the classification regions, and the estimated posterior probability
# associated with the first observation (water = 32.08, sailing.time = 82.69 ).
# b) Evaluate the performances of the classifiers A and B and identify the best one.
# c) How would you classify the occupants of a sailboat with daily consumed water 35 l and daily sailing time 168
# min?
  
# Problem n.4
# The file Lumieres.txt reports the data on the participation to Rendez-vous, the show of sounds and lights which
# takes place every evening between the 1st June and the 31st August in the main square of Nancy (France). The
# dataset refers to the years 2016 to 2018, and reports the number of participants (n), the day of the representation
# (d, with d = 1 on the 1st June, ..., d = 92 on the 31st August), the temperature recorded that evening at 10 pm
# (temp) and the weather conditions (rain: yes/no). Consider the following model
# ni,g = ??0,g + ??1 · di + ??2 · di^2 + ??3 · temp + eps,
# where g ??? {1, 2} indicates the group according to the weather conditions (g = 1 for no rain, g = 2 for rain) and
# eps ??? N(0, ??^2).
# a) Estimate the 6 parameters of the model. Verify the model assumptions.
# b) Perform a statistical test to verify if the mean number of participants depends significantly on the day of the
# representation.
# c) Based on a statistical test of level 95%, reduce the model and update the parameter estimates.
# d) Perform a test to verify if the maximum of the expected number of participants is on the last day of July
# (d = 61) and, in case, update the estimates of the model parameters.
# e) Based on the last update of the model parameters, provide a prediction interval (probability 95%) for the
# number of people participating to the representation on the 28th July (d = 58, temp = 29, no rain).