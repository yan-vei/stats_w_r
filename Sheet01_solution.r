### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, November 11. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## You are required to work together in groups of three students.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete, please do not leave out subquestions!

## Please write below your (and your teammates') name and matriculation number. 
## Name: Yana Veitsman
## Matriculation number: 7054842
## Name: Anthony Dsouza
## Matriculation number: 7053485
## Name: Tyler Lee
## Matriculation number: 7054832


## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## cms discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()

## b) Get help with this function.
help(getwd)
# alternatively - ?getwd

## c) Change your working directory to another directory.
setwd("..")
getwd()

#############
### Exercise 2: Normal distribution plotting and histograms
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the range for which you want to plot the 
##    normal distribution (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.75. Assign this to the 
##    variable x.
x <- seq(from = -5, to = 5, by = 0.750)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution. Use the defaults for mean and sd (standard normal distribution)
help(dnorm)
y <- dnorm(x, mean = 0, sd = 1, log = FALSE)

## c) Now use plot() to plot the normal distribution for z values of "x". Specify
## the type to be line using the suitable argument of plot()
plot(x, y)

## d) This plot does not look like a smooth normal distribution. Change the vector
##  x to have smaller increments and plot again (you also need to update y)
x1 <- seq(from = -5, to = 5, by = 0.15)
y1 <- dnorm(x1, mean = 0, sd = 1, log = FALSE)
plot(x1, y1)

## e) Take a look at the trees dataset (You can see it by typing "trees"), which 
##    has height, girth and volume.
##    Then select only the girth part and store it in a variable "treesGirth".
trees
treesGirth <- trees$Girth
treesGirth

## f) Calculate the mean and standard deviation of treesGirth and plot a normal
##    distribution with these parameters (NB:you should not use the same x range 
##    as above!)
trees_mean <- mean(treesGirth)
trees_sd <- sd(treesGirth)

y_trees <- dnorm(treesGirth, mean = trees_mean, sd = trees_mean, log = FALSE)
plot(treesGirth, y_trees)

## g) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the mean of tree girth using
##    the argument 'v'.
##    In order to get a dotted line, set the argument 'lty' to 3.
help(abline)
abline(v = mean(treesGirth), lty = 3)

## h) We observe two additional tree girth values (19.8 and 9.1). What's the 
##    likelihood that these heights (or more extreme ones) respectively 
##    come from the normal distribution from g)?
p1 <- 19.8
p2 <- 9.1
likelihood_p1 <- pnorm(p1, mean = trees_mean, sd = trees_sd)
likelihood_p2 <- pnorm(p2, mean = trees_mean, sd = trees_sd)

1-likelihood_p1
likelihood_p2

## i) What do you conclude from the p-values? (informal)
# Given that the p-values of p1 and p2 we can conclude that both points are unlikely to belong to the normal
# distribution, since their p-values are closer to 0 than to 1.

## j) Plot the actual values of girth as a histogram.
hist(treesGirth, main = "Histogram of trees' girth", xlab = "Girth", ylab = "Number of trees", col = "skyblue", border = "black", breaks=10)

## k) Create a kernel density plot for the actual values of girth using density()
density_plot <- density(treesGirth)
plot(density_plot)

## l) Is this data likely from a normal distribution? How would you check ?
##    (describe in words, remember to comment out text)
# So, we can visually assess both the histogram and the kernel density plot - if they resemble a bell,
# then it might be a sign of the distribution being normal. However, we can also do a QQ-plot, and if
# all the points fall on a straight line, it means it is likely a normal distribution

qqnorm(treesGirth)
qqline(treesGirth)

# Given that the histogram doesn't look shaped like a bell, neither does the kernel density plot (it has
# a "bump" on the right side") nor the points of the QQ-plot are situated on a straight line, 
# the data is unlikely to be from a normal distribution.

## m) What does this mean for our results from subquestions h and i?
# It means that both values are highly unlikely to come from a normal distributio, and our initial
# assumption was correct.


###############
### Exercise 3: Participants' age & boxplots
###############

## In this exercise, we will deal with data from a package.
## a) Install the package "languageR" and load it.
install.packages("languageR")
library(languageR)
## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##  Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the summary.
str(dutchSpeakersDistMeta)
summary(dutchSpeakersDistMeta)
## c) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.
help(boxplot)
boxplot(AgeYear ~ Sex, data = dutchSpeakersDistMeta, col = 'blue', na.rm = TRUE)
## d) Does it seem as if either of the two groups has more variability in age?
# -----> Female has more variability in age as the box is bigger.

## e) Do you see any outliers in either of the two groups?
# -----> Yes, Group Male has two dots (outliers)

## f) change the whiskers to extend to 2* the inter-quartile range using the 
##  argument range (check documentation)
boxplot(AgeYear ~ Sex, data = dutchSpeakersDistMeta, col = 'blue', na.rm = TRUE, range = 2)

## g) What changed?
# -----> The lower whisker on Group Male moved to 1930 from between 1930-1940

## h) What corresponds to the inter-quartile range in the boxplot?
# -----> IQR is the length of the box, which represents middle 50% of the data.

## i) Is the plot positively or negatively skewed?
# -----> Box is longer below the Median, so it is negatively skewed

##############################################
### Exercise 4: Dataframes and mensa meals
##############################################

## The chef at mensa is interested in what meal preferences international students
## show. At the beginning of the semester, you therefore go to Mensa and wait for a large 
## group of English speaking students, who turn out to be freshmen from European Management.
## Out of 24 students, 22 agree to tell you what meal they just had.

## The data follow:

# Meal1 Meal2 Meal1 Meal1 Meal2 Vegan Fish Meal2 Vegan Fish Meal1 Meal1 Fish Meal2 Meal2 Meal1 
# Meal1 Meal2 Meal1 Fish Fish Fish

## a) Is this a random sample of the population?
# This is not a random sample, because it only represents the 22 students who agreed to tell what 
# type of meal they had; moreover, the sample of English speaking students is overall not representative
# of all the freshmen in the above mentioned program.

## b) What measurement scale is this data?

### 'pps' is nominal scale. It may appear to be ordinal scale because of the ID-codes, 
### but person 1 is not before or after person 2 in any sense, because the assignment of ID-codes is arbitrary.

### Meals is also nominal scale because it is not quantitative data and cannot be ordered.
### For example, Vegan is not logically before or after Fish. Meal1 and Meal2 sound ordered,
###  but there is not any reason for this other than the order they appear on the menu.

## c) You will now create a dataframe of this data. Start by creating a vector 
##    with participant IDs. Your vector should be named 'pps', and your 
##    participants should be labeled from 1 to 22

pps <- 1:22

## d) next, make a vector of the meals and assign it to 'meals'

meals <- unlist(strsplit('Meal1 Meal2 Meal1 Meal1 Meal2 Vegan Fish Meal2 Vegan Fish Meal1 Meal1 Fish Meal2 Meal2 Meal1 Meal1 Meal2 Meal1 Fish Fish Fish', ' +'))

## e) Create a dataframe including pps and meals. Assign this to 'mensa'. 

mensa <- data.frame(pps, meals)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class are the variable 'pps' and 'meals'?

summary(mensa)
lapply(mensa, class)

## 'pps' is integer and 'meals' is integer

## g) Change the class of 'pps' and 'meals' to factor. 

mensa = lapply(mensa, as.factor)

## h) Why is factor a better class for these variables? (answer for both separately)

### Factor is a better class for 'pps' because it is nominal scale.
### Likewise, factor is a better classes for meals because it is also nominal scale.

## i) What plot you have seen in the lecture can best represent the outcome variable? Why?

### A bar plot is appropriate for representing the frequency of different levels in the sample.

## j) Make the plot! (Hint: you need to first compute frequencies)

frequencies <- table(mensa$meals)
barplot(frequencies, main='Meal Preferences', xlab='Meal type')

## k) What is the mode of meals?

###Meal1 is the mode of meals because it occurs the most frequently in the sample.









