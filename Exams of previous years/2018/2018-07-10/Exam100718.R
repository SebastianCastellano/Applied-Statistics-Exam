load("../../mcshapiro.test.RData")
# Problem n.4
# The file Focaccia.txt contains the amount [kg] of focaccia sold in a bakery in Recco during 43 days of June and
# July 2017. For the kilos of sold focaccia consider the following linear model
# Yg = ??0,g + ??1,g · t + ,
# with t ??? [1 : 61] the index of the day, g = {weekend, weekday} the day of the week and  ??? N(0, ??2
# ).
# a) Estimate the 5 parameters of the model {??0,g, ??1,g, ??}. Verify the model assumptions.
# b) Perform two statistical tests - each at level 5% - to verify if
# - there is a significant dependence of the mean sales on the day of the week;
# - there is a significant difference between weekend and weekdays, in the increase of the mean sales along time.
# c) Based on point (b), reduce the model and update the estimates of the parameters.
# d) Perform a test of level 5% to verify if during weekends the mean amount of sold focaccia increases of 60 kg. In
# case, update the parameters' estimates.
# e) Based on the last model, provide a point prediction of the mean sales of focaccia on the 28th July 2017 (day
#                                                                                                             61, weekday).

focaccia<- read.table("Focaccia.txt")

linmod <- lm(focaccia[[1]] ~ as.factor(focaccia$day) +  as.factor(focaccia$day):focaccia$t )
summary(linmod)
x11()
par(mfrow=c(2,2))
plot(linmod)

