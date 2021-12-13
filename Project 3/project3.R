# Gets MLE and MOM from the same data
f1 <- function(n, theta) {
  x = runif(n, min = 0, max = theta)
  mle = max(x)
  mom = 2 * mean(x)
  return(c(mle, mom))
}

# Gets the MSE of MLE and MOM from N replications, estimating theta with n trials
f2 <- function(N, n, theta) {
  estimates = replicate(N, f1(n, theta))
  t1 = (estimates-theta)^2
  mle = mean(t1[1,])
  mom = mean(t1[2,])
  mle = round(mle, digits = 3)
  mom = round(mom, digits = 3)
  avgs = c(mle, mom)
  t2 = c(n, theta, avgs)
  return (t2)
}

n_arr = c(1,2,3,5,10,30)
theta_arr = c(1,5,50,100)

res = c()

for (theta in theta_arr){ # Performs all of the trials by iterating through arrays
  for (n in n_arr){
    res <- rbind(res, f2(1000, n, theta))
  }
}
colnames(res) = c("n","Theta","MLE Error","MOM Error")


# Graphs all of the information
# MLE and MOM for the same theta/n pairs
# MLE is in cyan
# MOM is in coral
for (iter in seq(1,length(theta_arr))) {
  plot(res[(6 * iter-5):(6 * iter), 1], res[(6 * iter-5):(6 * iter), 3], xlab="n", ylab="Mean Squared Error", main=paste("MLE and MOM, Theta = ", res[(6 * iter), 2]), col="cyan")
  points(res[(6 * iter-5):(6 * iter), 1], res[(6 * iter-5):(6 * iter), 4], xlab="", ylab="", col="coral")
  legend("topright", legend=c("MLE", "MOM"), col=c("cyan", "coral"), pch=1:1,cex=0.8)
}

#####################
f3 <- function(theta, dat) {
  result = length(dat) * log(theta) - (theta + 1) * sum(log(dat))
  return(-result)
}

# Data
dat = c(21.72, 14.65, 50.42, 28.78, 11.23)s

# Optimization function
res = optim(par=1, fn=f3, method="L-BFGS-B", hessian=TRUE, lower=.01, dat=dat)

# Getting standard error from hessian matrix
se = sqrt(1/res$hessian)

# Setting up confidence interval for 95% confidence
ci = c(res$par - 1 * se * qnorm(.975), res$par + 1 * se * qnorm(.975))
