stats <- function(column) {
  cat("Mean:\t", mean(column), "\n")
  cat("SD:\t", sd(column), "\n")
  cat("Range:\t", range(column), "\n")
  cat("Median:\t", median(column), "\n")
  cat("IQR:\t", IQR(column), "\n")
} # Displays relevant descriptive statistics

csv <- read.csv("C:/Users/David/Desktop/roadrace.csv") # Reads the roadrace CSV
fromMaine <- subset(csv, csv[["Maine"]] == "Maine") # Gets the Mainers from the Maine column
fromAway <- subset(csv, csv[["Maine"]] == "Away") # Gets the runners from Away from the Maine column
tab <- matrix(c(nrow(fromMaine), nrow(fromAway)), ncol=2) # Turns the data into a matrix
colnames(tab) <- c('Maine', 'Away') # Adds column names into the matrix
barplot(tab, main="Beach to Beacon 10K Participants", ylab="Number of Participants") # Graphs as a bar plot
stats(tab) # Displays the statistics for Maine/Away participants (not very useful except for range)
hist(fromMaine$Time..minutes., xlim=(c(0,160)), ylim=(c(0,2000)), main="Maine Running Times", xlab = "Minutes", ylab = "Frequency") # Histogram of time in minutes for Maine runners
hist(fromAway$Time..minutes., xlim=(c(0,160)), ylim=(c(0,2000)), main="Away Running Times", xlab = "Minutes", ylab = "Frequency") # Histogram of time in minutes for Away runners
stats(fromMaine$Time..minutes.) # Displays the statistics for the time in minutes of Maine runners
stats(fromAway$Time..minutes.) # Displays the statistics for the time in minutes of Away runners
boxplot(fromMaine$Time..minutes., fromAway$Time..minutes., ylim=(c(0,160)), main="Maine Running Times", names = (c("Maine", "Away")), ylab = "Minutes") #Displays the data as a side-by-side boxplot
men <- subset(csv, csv[["Sex"]] == "M") # Separates men in Sex column
women <- subset(csv, csv[["Sex"]] == "F") # Separates women in Sex column
boxplot(as.integer(men$Age), as.integer(women$Age), ylim=(c(0,100)), main="Ages of Runners", names = (c("Men", "Women")), ylab="Age") # Side-by-side boxplot for ages of women and men participants
stats(as.integer(men$Age)) # Displays the statistics for age of men
stats(as.integer(women$Age)) # Displays the statistics for age of women
csv1 <- read.csv("C:/Users/David/Desktop/motorcycle.csv") # Reads the motorcycle csv
boxplot(csv1$Fatal.Motorcycle.Accidents, main = "Fatal Motorcycle Accidents in South Carolina", ylab = "Number of Accidents") # Boxplot for the number of accident in each county
outliers = which(csv1$Fatal.Motorcycle.Accidents > quantile(csv$Fatal.Motorcycle.Accidents, .75) + 1.5 * (IQR(csv$Fatal.Motorcycle.Accidents))) # Finds outliers (1.5IQR + Q3)
counties = csv1$County[outliers] # Gets the county names
stats(csv1$Fatal.Motorcycle.Accidents) # Displays the statistics for motorcycle accidents in SC