#### Question 1 ####

# Import the bootstrapping library
library(boot)

# Read the GPA data
data = read.csv("C:/Users/David/Desktop/gpa.csv")

GPA = data[,c(1)]
ACT = data[,c(2)]
plot(GPA, ACT, xlab="GPA", xlim=c(0,4), ylim=c(0,36), ylab="ACT", main="GPA vs ACT")

# Line of regression
abline(lm(ACT~GPA))

# Correlation
cor(GPA, ACT)

# Gets the correlation between bootstrapped indices
correlation <- function(data, indices) {
  GPA = data[,c(1)]
  ACT = data[,c(2)]
  tGPA = data$gpa[indices]
  tACT = data$act[indices]
  return(cor(tGPA, tACT))
}

(stat.boot = boot(data, correlation, R=999, sim="ordinary", stype="i"))
mean(stat.boot$t)
boot.ci(boot.out = stat.boot, type = "perc")

# Manual 95% percentile bootstrap estimate
# alpha/2th and 1-alpha/2th quartiles
sort(stat.boot$t)[c(25,975)]


#### Question 2 ####

# Import the bootstrapping library
library(boot)

# Read the VOLTAGE data
data = read.csv("C:/Users/David/Desktop/VOLTAGE.csv")

remote = data$voltage[which(data$location == 0)]
local = data$voltage[which(data$location == 1)]

# Descriptive stats
summary <- function(col) {
  print(mean(col))
  print(median(col))
  print(quantile(col, prob=c(.25,.75)))
  print(max(col))
  print(min(col))
  print(var(col))
}

boxplot(local, remote, names=c("Local", "Remote"), ylab="Volts", main="Recorded Voltages")

# For descriptive stats
summary(local)
summary(remote)

# Calculating the Confidence Interval
se = sqrt(var(local)/30 + var(remote)/30)
difference = mean(local) - mean(remote)
upper = difference + qnorm(1-.025) * se
lower = difference - qnorm(1-.025) * se
paste(lower, upper)
t.test(local, remote, alternative="two.sided", paired=FALSE, var.equal=FALSE, conf.level=.95)


#### Question 3 ####

# Import the bootstrapping library
library(boot)

# Read the VAPOR data
data = read.csv("C:/Users/David/Desktop/VAPOR.csv")

calculated = data[,c(2)]
actual = data[,c(3)]

#print(var(calculated))
#print(var(actual))

difference = calculated-actual
se = sd(difference)/4
difference = mean(calculated) - mean(actual)
upper = difference + qt(1-.025, 15) * se
lower = difference - qt(1-.025, 15) * se
paste(lower, upper)

t.test(calculated, actual, alternative="two.sided", paired=TRUE, var.equal=FALSE, conf.level=.95)
