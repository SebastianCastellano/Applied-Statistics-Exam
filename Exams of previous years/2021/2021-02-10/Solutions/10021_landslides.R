g <- lm(landslides$'"rate"' ~ landslides$'"rain"' + landslides$'"hardness"' + landslides$'"coarse"' + landslides$'"fine"')
summary(g)
#verification of gaussianity
shapiro.test(g$residuals)
x11()
qqnorm(g$residuals)
qqline(g$residuals)
#residuals are gaussian

#homoschedasticity assumption
x11()
plot(g$fitted.values,g$residuals/summary(g)$sigma, main = 'fitted vs- standardized residuals')
points(g$fitted.values, rep(2, length(g$fitted.values)), type = 'l', col = 'red')
points(g$fitted.values, rep(-2, length(g$fitted.values)), type = 'l', col = 'red')

x11()
plot(landslides$'"rain"',g$residuals/summary(g)$sigma, main = 'rain vs- standardized residuals')
points(landslides$'"rain"', rep(2, length(g$fitted.values)), type = 'l', col = 'red')
points(landslides$'"rain"', rep(-2, length(g$fitted.values)), type = 'l', col = 'red')

x11()
plot(landslides$'"hardness"',g$residuals/summary(g)$sigma, main = 'hardness vs- standardized residuals')
points(landslides$'"hardness"', rep(2, length(g$fitted.values)), type = 'l', col = 'red')
points(landslides$'"hardness"', rep(-2, length(g$fitted.values)), type = 'l', col = 'red')
#with hardness we MIGHT not have homoschedasticity of the residuals, not surpsrising since it's not significant

x11()
plot(landslides$'"coarse"',g$residuals/summary(g)$sigma, main = 'coarse vs- standardized residuals')
points(landslides$'"coarse"', rep(2, length(g$fitted.values)), type = 'l', col = 'red')
points(landslides$'"coarse"', rep(-2, length(g$fitted.values)), type = 'l', col = 'red')

x11()
plot(landslides$'"fine"',g$residuals/summary(g)$sigma, main = 'fine vs- standardized residuals')
points(landslides$'"fine"', rep(2, length(g$fitted.values)), type = 'l', col = 'red')
points(landslides$'"fine"', rep(-2, length(g$fitted.values)), type = 'l', col = 'red')


#residuals are fine

#we might try and fit a model without hardness
g1 <- lm(landslides$'"rate"' ~ landslides$'"rain"' + landslides$'"coarse"' + landslides$'"fine"')
summary(g1)

#quick verification of assumptions:
shapiro.test(g1$residuals)
x11()
plot(g1$fitted.values,g1$residuals/summary(g1)$sigma, main = 'fitted vs- standardized residuals')
points(g1$fitted.values, rep(2, length(g1$fitted.values)), type = 'l', col = 'red')
points(g1$fitted.values, rep(-2, length(g1$fitted.values)), type = 'l', col = 'red')



#we do not lose much on R2
#with a formal test:
anova(g,g1)
#p_val -> 0.81 -> we use this reduced model, where all regressors are significant


#test : H0 : beta(coarse) - 2*beta(fine) = 0 vs H1
#T-statistic : abs(beta^(coarse) - 2*beta^(fine) - 0)/(S*sqrt(c'(Z'Z)^(-1)c)) ~ t(n-(r+1))

fm <- linearHypothesis(g1, c(0,0,1,-2), 0) 
summary(fm)
#p_val <- 0.9836


#we have evidence toward this assumption, we can use a model with just as input (2*coarse + fine) instead of coarse and fine separately (we've verified that coarse has twice the effect wrt fine)
x <- 2*landslides$'"coarse"' + landslides$'"fine"'
landslides2 <- cbind(landslides, debris = x)
attach(landslides2)
g2 <- lm(rate ~ rain + debris)
summary(g2)


z_0 <- c(1, 700, 10*2 + 8) #???already selecting the right regressors

pointest <- z_0%*%g2$coefficients
#30.26621
#CI = [z_0'beta +- S*sqrt(z_0'(Z'Z)^(-1)z0)quantile(t, 1 - alpha/2, n- (r+1))]
Z_0 <- data.frame(rain = 700,debris = 28)
CI <- predict(g2, Z_0, interval='confidence', level=1-0.01)

#CI = [30.04873, 30.48368], also confirms the pointwise prediction
