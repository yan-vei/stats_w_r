### Stats with R Exercise sheet 2

###############################################################
# Deriving sampling distribution and confidence intervals
###############################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, November 18th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## Remember to answer each subquestion and test your code, before you submit.


## Please write below your (and your teammates') name and matriculation number. 
## Name: Yana Veitsman
## Matriculation number: 7054842
## Name: Anthony Dsouza
## Matriculation number: 7053485
## Name: Tyler Lee
## Matriculation number: 7054832

## Only 1 member needs to submit! 

#############################################
## Exercise 1: Concepts
#############################################

## The following code performs a t test (covered in week 6) to compare mean monthly accident deaths between the
## years 1973 and 1978 in the US

t.test(USAccDeaths[1:12], USAccDeaths[61:72], alternative="greater")

## Look at the output and - without knowing the details of the underlying test - try to answer
## the following questions:

## a) Is the test significant at the alpha level of 0.05?
# We can say that our H0 is that there is no significant difference between the two groups, while H1 
# that there is such a difference between the two groups. Then we can look at the p-value calculated, and
# we can see that it is less than our significance level, so we can reject the null hypothesis, which would
# mean that there is a significant difference between the two groups.

## b) What is the probability that this result is subject to a Type I error?
# The Type I error is set by the chosen significance level, so the probability of that happening is 0.05.

## c) Is this output also suited to tell you something about the Type II error probability?
# No, because the power of the statistical test is not calculated, therefore we cannot say anything
# about the probability of the Type II error.

## d) Is this test one-tailed or two-tailed?
# Since we are not using a particular direction in our hypothesis, it is a two-tailed test.

## e) Do you think the test should be one-tailed or two-tailed?
# It depends on the hypothesis that we would like to test. If we want to know the specific direction of the
# difference between the two groups, than we should use one-tailed test. When only just the fact that the difference
# exists matters, we can use the two-tailed test. Here we can probably use a one-tailed test to compare
# whether one year had more or less accidents than another.

###############################################################
### Exercise 2: Deriving sampling distributions
###############################################################
## In this exercise, we're going to derive sampling distributions for the mean with 
## different sample sizes.

## a) We will use the length of major rivers of the US as our dataset,
##  which can be found in the dataset rivers. What can you find out about this dataset?
rivers_mean <- mean(rivers)
rivers_sd <- sd(rivers)

x_rivers <-  rivers
y_rivers <- dnorm(x_rivers, mean=rivers_mean, sd=rivers_sd, log=FALSE)
plot(x_rivers, y_rivers)
abline(v = mean(rivers_mean), lty = 3)

# From plotting the distribution, we can see that it is positively skewed with the mean roughly at 591
# and the standard deviation of ~493.

## b) Make a histogram (with 30 breaks) and a boxplot of the data,
hist(rivers, main = "Histogram of rivers' length", xlab = "Length", ylab = "Number of rivers", col = "skyblue", border = "black", breaks=30)

boxplot(rivers, col = 'blue', na.rm = TRUE)

## c) This is clearly not a normal distribution. What do you observe?
# From the histogram we can observe that there are many more rivers that have length less than 1000, so 
# the distribution is skewed.
# From the boxplot we see that there are many outliers, and that the distribution is positively skewed,
# since the box is longer above the median.

## d) Compare mean and median of the dataset
mean(rivers)
median(rivers)

## e) Now, we are going to draw a smaller sample from rivers
### Use the function sample() to create a sample of five instances from rivers
### assign it to sample5
## in order to produce comparable results, please first execute this line:
set.seed(8254)
sample5 <- sample(rivers, 5)

## f) draw another 2 samples of 5 called sample5b and sample5c
sample5b <- sample(rivers, 5)
sample5c <- sample(rivers, 5)

## g) calculate the mean of each of the three samples
mean(sample5)
mean(sample5b)
mean(sample5c)

## h) Are the values different? Why?
# These values are very different, because our random sample size (even if taken using the same seed)
# is small.

## i) How do the sample means relate to the actual mean?
# It seems that given the dataset's size, the random sample means don't correlate with the dataset's mean
# until the random sample size grows enough (by the law of large numbers)

## j)   In order to draw a distribution of such a sample, we want to calculate the
###   mean of 1000 samples, each of size 5. However, we don't want to repeat 
###   question e and f 1000 times. Use a for loop to draw 1000 samples of size 5
###   and store the mean of each sample in the vector means5.
means5 <- numeric(1000)

for (i in 1:1000) {
  sample_rivers <- sample(rivers, 5)
  means5[i] <- mean(sample_rivers)
}

## k) Repeat the for-loop in question j, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.
means50 <- numeric(1000)

for (i in 1:1000) {
  sample_rivers <- sample(rivers, 50)
  means50[i] <- mean(sample_rivers)
}

## l) Draw histograms of means5 and means50. 
hist(means5, main = "Histogram of rivers' length - means 5", xlab = "Mean", ylab = "Number of rivers", col = "skyblue", border = "black", breaks=30)
hist(means50, main = "Histogram of rivers' length - means 6", xlab = "Mean", ylab = "Number of rivers", col = "skyblue", border = "black", breaks=30)

## m) Describe in what way they differ
# The first histogram is skewed, while the second one seems to be distributed normally.

## n) Why do you observe a skew for means5, but not for means50?
# Because it is a small sample size that is not representative of the entire population.

## o) Repeat steps j and k, but this time calculate the maximum of the samples of 
##  size 5 and 50 and assign them to max5 and max50.
max5 <- numeric(1000)
max50 <- numeric(1000)

for (i in 1:1000) {
  sample_5 <- sample(rivers, 5)
  max5[i] <- max(sample_5)
  
  sample_50 <- sample(rivers, 50)
  max50[i] <- max(sample_50)
}

## p) Plot the histograms.
hist(max5, main = "Histogram of rivers' length - maxs 5", xlab = "Max", ylab = "Number of rivers", col = "skyblue", border = "black", breaks=30)
hist(max50, main = "Histogram of rivers' length - maxs 50", xlab = "Max", ylab = "Number of rivers", col = "skyblue", border = "black", breaks=30)

## q) Do you observe the same pattern as for the sampling distribution of the mean?
## Why do you think this is?
# The pattern is quite different, because the bigger sample's size max is likely to be bigger, since more observations
# are drawn. In case with the smaller sample size, the max's also seem to be skewed.

###############################################################
### Exercise 3: Confidence interval
###############################################################

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.


## a) What does a confidence interval mean from the perspective of experiment replication?
# Confidence interval means that the mean of the experiment would fall within a certain range with a
# certain confidence level.

## b) please install and load packages sciplot and lsr
install.packages("sciplot")
install.packages("lsr")

library(languageR)
library(lsr)

## c) calculate 95% Confidence Intervals for rivers, sample5 and sample5c. You can
##    use the function ciMean()
ciMean(rivers, conf = 0.95)
ciMean(sample5, conf = 0.95)
ciMean(sample5c, conf = 0.95)

## d) Why are these intervals so different?
# Because the rivers dataset contains the true population mean, while the random samples don't and are drawn
# from very different parts of the population, so their mean varies larger.

## e) Is the true mean contained in each interval?
mean(rivers)
# Yes, the true mean is contained in every interval.

## f) Do you note anything odd wrt the confidence interval for sample5?
# For some reason it includes a negative value in the lower range of the CI, which is not possible in reality,
# since the rivers cannot have a negative length.

## g) We will now calculate the confidence interval for the mean of rivers by hand. 
##  A commonly used formula for 95% confidence intervals is mean±1.96*SE
lower_bound_95 <- rivers_mean - 1.96*(rivers_sd / sqrt(length(rivers)))
upper_bound_95 <- rivers_mean + 1.96*(rivers_sd / sqrt(length(rivers)))

lower_bound_95
upper_bound_95

## h) Do your results align with the output of ciMean above? (There might be a bit of rounding error)
# Yes, the results align with a small rounding error.

## i) Now, please calculate a 90% confidence interval without the ciMean function. 
##  Hint: You need to replace the 1.96, which is the 97.5 quantile of the normal distribution.
lower_bound_90 <- rivers_mean - 1.645*(rivers_sd / sqrt(length(rivers)))
upper_bound_90 <- rivers_mean + 1.645*(rivers_sd / sqrt(length(rivers)))

lower_bound_90
upper_bound_90

## j) Compare to the interval in g!
# The lower bound point is closer to the mean, e.g. larger than the lower bound of the 95 confidence interval.
# The upper bound point is also closer to the mean, e.g. smaller than the upper bound of the 90 confidence interval.