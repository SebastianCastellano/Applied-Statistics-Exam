setwd("./2021/200121")
wine <- read.table("wine.txt", header = T)
attach(wine)
n <- dim(wine)[1]
r <- length(levels(as.factor(region)))
c <- length(levels(as.factor(color)))

#So we build a model:
#alcohol_r_c_k = tau_r + gamma_k + eps_r_c_k, eps_r_c_k ~ N(0,sigma^2) iid
an <- aov(alcohol ~ region + color + region:color)

shapiro.test(an$residuals)
#we can assume gaussianity
rc <- interaction(region, color)
bartlett.test(an$residuals ~ rc)
#we can assume homoschedasticity

summary(an)
#looking at the (one at the time) confidence intervals and performing backward selection we reduce the model:
an1 <- aov(alcohol ~ color + region:color)

shapiro.test(an1$residuals)
bartlett.test(an1$residuals ~ rc) #still valid

summary(an1)
#we have the same sum of squares for the residuals, the model is still significant
#we perform one more step
an_red <- aov(alcohol ~ color)

shapiro.test(an_red$residuals)
bartlett.test(an_red$residuals ~ color) #still valid

summary(an_red)
#still the same sum of squares for the residuals, the model is still significant
#So we end up with the model:
#alcohol_c_n = tau_c + eps_c_n   eps_c_n ~ N(0,sigma^2) iid

taus <- c(tau_red = an_red$coefficients[1],
          tau_white = an_red$coefficients[1] + an_red$coefficients[2]) #means
s <- sum((an_red$residuals)^2)/(n-c) #spooled



n_r <- length(which(color=="red"))
n_w <- length(which(color=="white"))
smean_r <- mean(alcohol[which(color=="red")])
smean_w <- mean(alcohol[which(color=="white")])
s_r <- var(alcohol[which(color=="red")])
s_w <- var(alcohol[which(color=="white")])

alpha_B <- 0.01/4

q_stud <- qt(1-alpha_B/2, df = c(n_r, n_w))
ss_int <- c(s_r/n_r, s_w/n_w)
smeans <- c(smean_r, smean_w)

CI_mean <- cbind(inf = smeans - q_stud*sqrt(ss_int),
                 center = smeans,
                 sup = smeans + q_stud*sqrt(ss_int))
rownames(CI_mean) <- c("red", "white")

q_chi_high <- qchisq(1-alpha_B/2, df = c(n_r - 1, n_w - 1))
q_chi_low <- qchisq(alpha_B/2, df = c(n_r - 1, n_w - 1))
ss <- c(s_r, s_w)
ss_adj <- c(s_r*(n_r - 1), s_w*(n_r - 1))
CI_var <- cbind(inf = ss_adj/q_chi_high,
                center = ss,
                sup = ss_adj/q_chi_low)
rownames(CI_var) <- c("red", "white")
CI_mean
CI_var

#alternatively one could just use the already verified assumption that the two variances are euqal and do:
alpha_B_new <- 0.01/3
band <- qt(1-alpha_B_new/2, n-c)*sqrt(s/c(n_r, n_w))
CI_mean_new <- cbind(inf = smeans - band,
                 center = smeans,
                 sup = smeans + band)
q_chi_high_new <- qchisq(1-alpha_B_new/2, n-c)
q_chi_low_new <- qchisq(alpha_B_new/2, n-c)
CI_var_new <- cbind(inf = s*(n-c)/q_chi_high_new,
                    center = s,
                    sup = s*(n-c)/q_chi_low_new)
CI_mean
CI_mean_new
CI_var
CI_var_new


#in either case we can eventually further confirm that the homoschedasticity assumption is satisfied
#moreocer we have statistical evidence to say that red wine contains, on average, more alcohol than white wine
