X <- read.table("~/GitHub/Applied-Statistics-Exam//Exams of previous years/2017/2017-07-03/garden.txt")
attach(X)
n<- dim(X)[1]

#a) Linear model
#categ.factor <- ifelse(wind=="Yes",1,0) # categorical factor
lmod <- lm(extension ~ carps + maple + cherry + stones)
summary(lmod)
#Assumptions (gaussianity of residuals, homosckedasticity, independence from regressors (lack of patterns))
x11()
par(mfrow = c(2,2))
plot(lmod)
shapiro.test(lmod$residuals)$p
# qqnorm(g$residuals) # already present in plot(lmod)
# qqline(g$residuals)
x11()
plot(carps,scale(g$residuals, scale = T, center = F)) # repeatplot for other regressors

#b) Hypothesis testing on the coefficients
library(car)
linearHypothesis(lmod,rbind(c(0,1,0,0,0),c(0,0,1,-1,0)),c(0,0))

#c) Model reduction
library(regclass)
vif(lmod) # variance inflation factor: if big (above 5) risk of collinearity
#remove one at the time starting from highest p-value
lmod1 <- lm(extension ~ carps + maple + stones)
summary(lmod1)
# test if we lose in explainability
anova(lmod,lmod1) # we do not want to reject (high p-value = good)
#remember to check assumptions again

#d) Prediction and confidence intervals
x.new <- data.frame(Va = 35, Vi=25, wind.factor=1) 
k <- 2 #num intervals, Bonferroni correction
alpha<-0.01/k
predict(lmod1,x.new,interval = "prediction",level = 1-alpha) # interval=prediction/confidence