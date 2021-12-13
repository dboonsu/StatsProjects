draws10000 = replicate(10000, max(rexp(n=1, rate=.1), rexp(n=1, rate=.1))) # Performs the equation (second argument) 10,000 (first argument) times.
hist(draws10000, prob=TRUE) # Draws the histogram, but with probabilities so the curve will fit properly
curve((.2*exp(-.1*x)-.2*exp(-.2*x)),xlim=range(0,100), add=TRUE) # Adds the curve onto the current plot
mean(draws10000) # Calculates the mean of the 10,000 trials
sum(draws10000>15)/length(draws10000) # Calculates the average of successes, where the satellite survives for more than 15 years,

# Other trials were performed where n = 1,000 and n = 100,000. The variable was changed and n was substituted for in arguments.

x = runif(10000, min=0, max=1) # Generates 10,000 random numbers between 0 and 1 for an x coordinate
y = runif(10000, min=0, max=1) # Generates 10,000 random numbers between 0 and 1 for a y coordinate
z = (x-.5)^2 + (y-.5)^2 <= .5^2 # Determines if the point is inside the circle
sum(z == TRUE)* 4/ 10000 # Performs the other necessary calculations to approximate pi.
