load("C:/Users/jacop/Desktop/università/da dare/Applied Statistics/AS lab/LAB_5/mcshapiro.test.RData")
library(mvnormtest)

#test for Gaussianity
mcshapiro.test(shopping)

#we are working on repeated observations, we assume independence on our units, so we do not want to see patterns
plot(1:n, shopping$'"accesses"', type = 'l')
plot(1:n, shopping$'"men"', type = 'l')
plot(1:n, shopping$'"women"', type = 'l')
#we do not have recognisable patterns



#we can work with Gaussian assumption
n <- dim(shopping)[1]
p <- dim(shopping)[2]
alpha <- 0.05
x.mean <-colMeans(shopping)
S <- cov(shopping)
S_inv <- solve(S)
radius <- sqrt(((n-1)*p/(n-p))*qf(1-alpha, p, n-p))
#we do not have enough data to work asimptotically, but we have gaussianity
#Confidence Region : n*(mu - x.mean)' S_inv (mu - x.mean) <= ((n-1)*p/(n-p))*quantile(1 - alpha)_Fisher(p, n-p)
eigen(S/n)$vectors
radius*sqrt(eigen(S/n)$values)


#T2 are just the projections of the confidence region

T2 <- cbind(inf = x.mean - radius*sqrt(diag(S)/n),
            center = x.mean, 
            sup = x.mean + radius*sqrt(diag(S)/n))
#SimCI(a'mu) = [a'mu +- sqrt(((n-1)*p/(n-p))*qf(1-alpha, p, n-p))*sqrt(a'Sa / n)]
#in this case the vectors a are just the canonical basis in R3, so we can just take the compact notazion and identify them as the component of the vector diag(S)

#H0: x.mean(woman) + x.mean(mean) - 0.2*x.mean(accesses) = x.mean[2] + x.mean[3] - 0.2x.mean[1] >= 0 vs H1
#vec = c(-0.2, 1 , 1)
#pivotal stat: T = vec*(x.mean - c(0,0,0))/sqrt(vec'*S*vec/n) ~ t(n-1)
#rejection region:
  # T < t(1-alpha, n-1)
vec = c(-0.2, 1 , 1)
T = t(vec)%*%x.mean/sqrt(t(vec)%*%S%*%vec/n)
T_quant = qt(alpha, n-1)
T>=T_quant
p_val <- pt(T, n-1)
#we have evidence to accept H0 -> p_val = 0.995