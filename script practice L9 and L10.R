#9101
#a
t=numeric(10000)
for (i in 1:10000){
  x=rnorm(6)
  t[i] = max(x)-min(x)
}

#one sided test:
critvalue=quantile(t, 0.95) # critvalue =4.053686

#b
X=c(2.0,2.9,-0.5,0.3,-0.8,-0.1)
teststat=max(X)-min(X)


#9103
t=numeric(10000)
for (i in 1:10000){
  x=rnorm(6,0,sqrt(2))
  t[i] = max(x)-min(x)
}
mean(t>critvalue)


#9104
#a
t=numeric(10000)
for (i in 1:10000){
  x=rnorm(6,0,1)
  t[i] = var(x)
}
#one sided test:
critvalue=quantile(t, 0.95)

#b
t=numeric(10000)
for (i in 1:10000){
  x=rnorm(6,0,sqrt(2))
  t[i] = var(x)
}
mean(t>critvalue)

#9304
X = c(1.0, 5.2, 2.1, 9.3, 0.7, 1.5, 2.5, 0.9, 1.0, 3.8, 6.6, 1.6)
hat_lambda <- 1/mean(X) #MLE
lambda_0= 1

loglikfun = function (lambda) sum(dexp(X, rate = lambda, log = TRUE))
step = 0.001
grid = c(lambda_0-step/2, lambda_0+step/2)
loglik = sapply(grid, loglikfun)
Tscore = diff (loglik)/step #-24.20
Twald = hat_lambda-lambda_0 #-0.67
LRT = loglikfun(hat_lambda)-loglikfun(lambda_0) #10.95

#9305
X = c(1.0, 5.2, 2.1, 9.3, 0.7, 1.5, 2.5, 0.9, 1.0, 3.8, 6.6, 1.6)
L0=1 #H0
loglikfun = function(lambda) sum(dexp(X, rate = lambda, log = TRUE))
L_hat=optimize(loglikfun, c(0,5), maximum = TRUE)$maximum
step = 0.001
grid = c(L0-step/2, L0+step/2)
loglik = sapply(grid, loglikfun)
Tscore = diff(loglik)/step #-24.20
LRT = loglikfun(L_hat)-loglikfun(L0) #10.95
Twald = L_hat-L0 #-0.67


#9307 en 9308


#10106
# two-sided test
X=c(-1.2,1.8,-0.5,0.4,3.4,3.6)
n=length(X)
mu = 0
var_0 = 1 #theta_0
var_hat2 = var(X) #theta_hat
var_hat = optimize(loglikfun, c(0,10), maximum = TRUE)$maximum

loglikfun = function(variance) sum(dnorm(X, mean = 0, sd = sqrt(variance), log=TRUE))
LRT = loglikfun (var_hat) - loglikfun(var_0) # 7.02
critvalue=qchisq(0.95, df = 1)/2 #1.92
#teststat > critvalue so we reject H0

p_value = pchisq(LRT, 1, lower.tail = FALSE)





































