library(sp)
library(lattice)
library(geoR)
library(gstat)
hotels <- hotels[,-1]#1st row of tags that was messing everything up -> REMOVE "" FROM THE COLUMN NAMES
coordinates(hotels) <- c("x", "y")
bubble(hotels,'price',key.space='bottom')

#we are basically fitting price with its mean, with a model for the residual which is spherical + nugget

#sample variogram
svgm <- variogram(hotels$price ~ 1, hotels)
plot(svgm, main = 'Sample Variogram',pch=19)
plot(variogram(hotels$price ~ 1, hotels),pch=19)
#we fit the spherical part, from initial values which are reasonable
vfit <- fit.variogram(svgm, vgm(psill = 3500, model = "Sph", range = 500,nugget = 500))
plot(svgm, vfit, pch = 19)
#model
mod <- gstat(formula = price ~ 1, data = hotels, model = vfit)
#to get a0 it sufficies to predict, using the model, at any location
#the prediction is the mean (we only use the intercept) -> a0
predict(mod,hotels[1,] , BLUE = TRUE)
#indeed notice that the prediction is the same for
predict(mod,hotels[26,] , BLUE = TRUE)

#we have to check isotropy
x11()
plot(variogram(price ~ 1, hotels, alpha = c(0, 45, 90, 135)),pch=19)
#we may not have isotrpy


dummy <- rep(0, length(hotels))
dummy[which(hotels$winter=='"no"')] <- 1
hotels_dummy <- cbind(hotels,dummy)
names(hotels_dummy)[4] = 'dummy'
#new fit
svgm_new <- variogram(price ~ 1 + dummy + distance + distance:dummy, hotels_dummy)
plot(svgm_new, main = 'Sample Variogram',pch=19)
vfit_new <- fit.variogram(svgm_new, vgm(psill = 1000, model = "Sph", range = 500,nugget = 100))
plot(svgm_new, vfit_new, pch = 19)

mod_new <- gstat(formula = price ~ 1 + dummy + distance + distance:dummy, data = hotels_dummy, model = vfit_new)

#estimation of parameters
Z0_for_params <- hotels_dummy[1,]
Z0_for_params$distance[1]=0
Z0_for_params$dummy = 0
attach(hotels_dummy)
predict(mod_new,Z0_for_params, BLUE = TRUE)
a01 <- predict(mod_new,Z0_for_params, BLUE = TRUE)$var1.pred
#a01 = 447.1537 (intercept for winter)
Z0_for_params$dummy = 1
predict(mod_new,Z0_for_params, BLUE = TRUE)
a02 <- a01 + predict(mod_new,Z0_for_params, BLUE = TRUE)$var1.pred
#a02 = 667.8871 (intercept for summer)
Z0_for_params$distance[1]=1
Z0_for_params$dummy = 0
predict(mod_new,Z0_for_params, BLUE = TRUE)
a11 <- predict(mod_new,Z0_for_params, BLUE = TRUE)$var1.pred - a01
#a11 = -0.069258 (effect of distance on price for winter)
Z0_for_params$distance[1]=1
Z0_for_params$dummy = 1
predict(mod_new,Z0_for_params, BLUE = TRUE)
a12 <- predict(mod_new,Z0_for_params, BLUE = TRUE)$var1.pred - a02
#a12 = -447.1631 (effect of distance on price for summer)

x11()
plot(variogram(price ~ 1 + dummy + distance + distance:dummy, hotels_dummy, alpha = c(0, 45, 90, 135)),pch=19)
#we have a much more robust isotropy assumption with this last model, hence even if it might seem that the variability of the previously estimated parameter is lower, we cannot truly rely on that
#moreover the sill of this last variogram is much lower -> we have gained a lot in reducing the variability of our residuals



s0.new <- as.data.frame(cbind(342399.74, 5072272.75,sqrt((342362.58 -342399.74)^2 + (5072272.75 - 5072272.75)^2),  0))
names(s0.new) <- c('x','y', 'distance', 'dummy')
coordinates(s0.new) <- c('x','y')
predict(mod_new, s0.new, BLUE = TRUE)
price <- 4*predict(mod_new, s0.new, BLUE = TRUE)$var1.pred

#target price -> 1778.32
#we used this (BLUE) since we cannot assume the same realization of the residuals (i.e. same specific covariance structure, not just model) that happened in 2019
#so we  try to predict the mean, notice however that even if we took into account the dependence wrt nearby observations, the estimate wouldn't have changed much
#price_now <- 4*predict(mod_new, s0.new)$var1.pred = 1778.365

