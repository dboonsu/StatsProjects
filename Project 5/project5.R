#### Question 1 ####

# Reads the csv file
data = read.csv("C:/Users/David/Desktop/bodytemp-heartrate.csv")

# Provides basic statistics
summary <- function(col) {
  print(mean(col))
  print(median(col))
  print(var(col))
}

# Separates the data into temperature and heart rate
temp = data[,c(1)]
heart = data[,c(3)]

# Separates the data into female and male
mtemp = data$body_temperature[which(data$gender == 1)]
ftemp = data$body_temperature[which(data$gender == 2)]
mheart = data$heart_rate[which(data$gender == 1)]
fheart = data$heart_rate[which(data$gender == 2)]

# Boxplots to compare distributions for temperature
boxplot(mtemp, ftemp, names=c("Male Temperatures", "Female Temperatures"), main="Body Temperatures")
summary(mtemp)
summary(ftemp)

# Boxplots to compare distributions for heart rate
boxplot(mheart, fheart, names=c("Male Heart Rate", "Female Heart Rate"), main="Heart Rates")
summary(mheart)
summary(fheart)

# Scatterplot for male and female temperature and heart rate
plot(temp, heart, xlab="Temperatures", ylab="Heart Rates", main="Temperatures vs Heart Rates")
abline(lm(heart~temp))
cor(temp, heart)

# Scatterplot for male temperature and heart rate
plot(mtemp, mheart, xlab="(M) Temperatures", ylab="(M) Heart Rates", main="(M) Temperatures vs Heart Rates")
abline(lm(mheart~mtemp))
cor(mtemp, mheart)

# Scatterplot for female temperature and heart rate
plot(ftemp, fheart, xlab="(F) Temperatures", ylab="(F) Heart Rates", main="(F) Temperatures vs Heart Rates")
abline(lm(fheart~ftemp))
cor(ftemp, fheart)

#### Question 2 ####

n_arr = c(5, 10, 30, 100)
lambda_arr = c(.01, .1, 1, 10)
alpha = .05

# Creates a Z-interval and sees if the true value (1/lambda) is captured by the
#  interval. Returns 1 if the true value is captured and 0 if the true value is 
#  not captured
checkZInt <- function(n, lambda) {
  distr = rexp(n, rate=lambda)
  se = sd(distr)/sqrt(n)
  upper = mean(distr) + qnorm(1-.025) * se
  lower = mean(distr) - qnorm(1-.025) * se
  tm = 1/lambda
  if (lower < tm && tm < upper) {
    return (1)
  }
  return (0)
}

# Performs the n/lambda combination 5000 times and returns how many successes 
#  there were
zTrials <- function(n, lambda) {
  values = replicate(5000, checkZInt(n, lambda))
  success = values[which(values==1)]
  return (length(success))
}

# For bootstrapping
mean_ <- function(n, lambda) {
  distr = rexp(n, lambda)
  return (mean(distr))
}

# Creates a percentile bootstrap interval and sees if the true value (1/lambda) 
#  is captured by the interval. Returns 1 if the true value is captured and 0  
#  if the true value is not captured. The confidence interval is 95%, thus taking
#  the 25th and 975th sorted values will provide the confidence interval.
checkBInt <- function(n, lambda) {
  distr = rexp(n, lambda)
  tm = 1/lambda
  lambda1 = 1/(mean(distr))
  results = replicate(1000, mean_(n, lambda1))
  lower = sort(results)[25]
  upper = sort(results)[975]
  if (lower < tm && tm < upper) {
    return (1)
  }
  return (0)
}

# Performs the n/lambda combination 5000 times and returns how many successes 
#  there were
bTrials <- function(n, lambda) {
  values = replicate(5000, checkBInt(n, lambda))
  success = values[which(values==1)]
  return (length(success))
}

zRes = c()
bRes = c()
# THIS IS WHERE THE ACTUAL CALCULATIONS ARE PERFORMED, UNCOMMENT THIS TO RUN THEM
#for (lambda in lambda_arr){ # Performs all of the trials by iterating through arrays
#  for (n in n_arr){
#    zRes <- rbind(zRes, zTrials(n, lambda))
#    bRes <- rbind(bRes, bTrials(n, lambda))
#  }
#}

# I MOVED THE RESULTS HERE, SO I WOULDN'T HAVE TO RUN THE CALCULATIONS EVERYTIME
# SEE THE ABOVE BLOCK OF CODE TO SEE HOW THESE ARE CALCULATED
# zRes = z-interval results sorted by N (sample size) or L (lambda)
# bRes = percentile bootstrap results sorted, again, by N (sample size) or L (lambda)
zResN = c(4054,4366,4589,4678,4035,4357,4609,4698,4064,4314,4591,4705,4059,4332,4592,4673)
zResL = c(4054,4035,4064,4059,4366,4357,4314,4332,4589,4609,4591,4592,4678,4698,4705,4673)
bResN = c(4476,4617,4718,4742,4486,4624,4702,4768,4493,4602,4680,4720,4513,4619,4702,4750)
bResL = c(4476,4486,4493,4513,4617,4624,4602,4619,4718,4702,4680,4702,4742,4768,4720,4750)

# Graphs the results sorted by lambda
for (iter in seq(1,length(lambda_arr))) {
  plot(n_arr[(1):(4)], zResN[(4 * iter-3):(4 * iter)], xlim=c(0,100), ylim=c(3000,5000), xlab="n", ylab="Successes", main=paste("Lambda = ", lambda_arr[iter]), col="cyan")
  points(n_arr[(1):(4)], bResN[(4 * iter-3): (4 * iter)], col="coral")
  legend("bottomright", legend=c("Z-Interval", "Boostrap Interval"), col=c("cyan", "coral"), pch=1:1,cex=0.8)
}

# Graphs the results sorted by n
for (iter in seq(1,length(n_arr))) {
  plot(lambda_arr[(1):(4)], zResL[(4 * iter-3):(4 * iter)], xlim=c(0,12), ylim=c(3000,5000), xlab="Lambda", ylab="Successes", main=paste("N = ", n_arr[iter]), col="cyan")
  points(lambda_arr[(1):(4)], bResL[(4 * iter-3): (4 * iter)], col="coral")
  legend("bottomright", legend=c("Z-Interval", "Boostrap Interval"), col=c("cyan", "coral"), pch=1:1,cex=0.8)
}
