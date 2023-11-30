### Stats with R Exercise sheet 4

##########################
#Week 5: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, December 2. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates') name and matriculation number. 
## Name: Yana Veitsman
## Matriculation number: 7054842
## Name: Anthony Dsouza
## Matriculation number: 7053485
## Name: Tyler Lee
## Matriculation number: 7054832

###########################################################################################
###########################################################################################

#####################################################
### 1. t test from scratch
#####################################################

## In this exercise, we will use the dataset trees again, but this time look at the height of black  
## cherry trees. 
## Imagine that a friend of yours is a landscaper, who plans to plant a group of trees behind 
## a row of paper birch trees.
## Her plan is that the group will be visible behind the birch trees, once they are fully grown. She
## knows that birch trees typically grow to about 66 ft and now she asks you, whether the black cherry 
## tree would be a good choice.
## To answer this question, you will use the one-sample t-test and calculate it without 
## the t.test function.

## a) Why do you need a one-sample t-test here?
# We are trying to learn whether our unknown population mean is actually 
# different from 66 ft; we don't know the standard deviation of the population,
# and we are testing on ratio scale data. Given the above, the most appropriate
# test to use is the one-sample t-test.

## b) Do you need a one-tailed or two-tailed test?
# So, we want the trees to be visible behind the birch trees, which means the
# cherries would need to be greater than 66 ft tall. Therefore, one-tailed
# test will suffice.

## c) check whether the normality assumption is violated
# To check for normality, we can build a QQ-plot of the data and/or the density 
# kernel plot.
trees_heights <- trees$Height

qqnorm(trees_heights)
qqline(trees_heights)

density_plot <- density(trees_heights)
plot(density_plot)

# It seems that the normality assumption is violated, because the density_plot 
# is has two peaks instead of one, and the points on the QQ-plot don't lie
# on the straight line.

## d) calculate the t statistic without using the t.test function

sample_mean <- mean(trees_heights)
sample_sd <- sd(trees_heights)
sample_size <- length(trees_heights)
pop_mean <- 66

t_statistic <- (sample_mean - pop_mean) / (sample_sd / sqrt(sample_size))

## e) What degrees of freedom do you need?

df <- sample_size - 1

## f) Find the critical value for your test, using an alpha-level of 0.001 (it's really expensive to
## plant those trees, so you want to make sure that the difference is really there)

critical_value <- qt(0.001, df)

## g) What is your conclusion (in terms of the null hypothesis)?
# Our null hypothesis is that the mean height of the cherry trees is 66 feet
# or less (and we want to disprove this). 
# The absolute t-statistic value is greater than the critical value,
# therefore, we can reject the null hypothesis.

t_statistic > critical_value

## h) Now, run the same t test using the t.test function

t.test(trees_heights, mu=66)

## i) Given your friend wants the trees of her choice to be at least 5 ft higher than the birch trees, 
##    will you recommend planting cherry trees?

# If we consider the lowest value from the 95% CI, 73.6 feet,
# it will still be at least 5 ft more than 66 ft, therefore, yes,
# we can recommend planting cherry trees.

#####################################################
### 2. Restructuring, filtering, plotting, and  t test
#####################################################

library(lsr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(languageR)

## We will be working with the dataset lexdec from the package languageR
## In short, this data set contains reaction times from an experiment, where participants had 
## to decide whether something is a word or not (nonword). Only responses for real words are
## included and there were 79 measurements per participant.
## 
## Variables we will be interested in include 
## Subject (code for a participant)
## Complex (whether the word is a compound (e.g.blackberry) or not (e.g.cherry))
## RT (log reaction time)
## Sex (of the participant)
## NativeLanguage (native language of participant)
## Correct (whether participants correctly responded with "word")


## a. Create the dataset lex, which is a copy of lexdec, but only includes the columns 
##  indicated above

## b. as we are only interested in the response time for correct responses, filter out 
##  any incorrect responses

## Say you are interested in the influence of the complexity of a word on lexical decision time.
## Before we start testing, we want to get an impression of the data and create a barplot of 
## the mean by complexity, including error bars that show the 95% CI.
## Here, we define a function to calculate the standard error, which is needed for the CI:
## (just execute the next line, as you will need the function in c.)
se = function(x){sd(x)/sqrt(length(x))}

## c. To start, we need to summarize the data. Use the functions group_by() in combination with
##  summarise(). In particular, you need to group by Complex and get the mean as well as the
##  se of RT. Store the result to summaryByComplex
##  You will find examples of how the summarizing can be done here:
##  https://datacarpentry.org/R-genomics/04-dplyr.html#split-apply-combine_data_analysis_and_the_summarize()_function

## d. Describe the resulting data set (summaryByComplex) in your own words

## e. Now use summaryByComplex to create the barplot with error bars denoting the 95% CI
##  (i.e. mean +/-1.96 * se)

## f. The barplot always starts at zero, which makes the portion of the graph, we are most 
##  interested in (i.e. the spread of the error bars) hard to perceive. As an alternative,
##  construct a line plot of the same data, again including error bars.
##  Hint: if you get a complaint, try to add group = 1 to your aes

## g. Gauging from the plot, does it look like there's an important difference in mean RT 
##  for complex and simplex words?

## h. Let's go back to the original data frame "lex".
##  Now that you've taken a look at the data, you want to get into the stats.
##  You want to compute a t-test for the average RT for simplex vs complex nouns.
##  Why can't you compute a t-test on the data as they are now? 
##  Hint: Which assumption is violated?

## i. We need to restructure the data to only one observation (average RT) per subject 
##  and complex/simplex (Complex). We will again use group_by and summarize, but
##  this time we have to group by Subject and Complex, while we only need the mean to be 
##  stored, not the se. Assign the result to bySubj

## j. Create histograms of the RT data in bySubj depending on the frequency category 
##  and display them side by side. Set the binwidth to 0.08

## k. Display the same data in density plots. 

## l. Based on the histograms and the density plots - are these data likely coming
## from a normal distribution?

## m. Create boxplots of the mean RT in bySubj by Freq

## n. We will ignore our results from l-m and compute a t-test to compare the mean RT between 
##  lexical decisions on complex vs simplex words using the data in bySubj.
##  Do you need a paired t-test or independent sample t-test? why?

## o. Compute the t-test you specified above

## p. What does the output tell you? What conclusions do you draw?

## q. Compute the effect size using Cohen's D. 

## r.  Which effect size do we get? How do you interpret this result?

## s. Why would you report the effect size in addition to the p-value?

#####################################################
### 3. Another T-test
#####################################################


## a. Now let's look at another question, namely whether the native language of the participant 
##  influences their reaction time. Check out the variable NativeLanguage. Can you use a t-test to pursue this 
##  question and which type of t-test would you use? 

## b. Use again group_by and summarize to obtain by subject means of RT, but
## this time with regard to NativeLanguage and assign it to bySubjLang
## Perform the t-test you decided for.

## c. What do you conclude?

## d. Choose an appropriate plot to visualize the result


#############################################
### 4. T-Tests for different sample sizes
#############################################

## In this exercise we will again use simulation to explore the independent samples t-test 
## with different samples. 
## We will take a similar example as discussed in the lecture. A class has two tutors, and we want 
## to find out which tutor is better by comparing the performance of the students in the final 
## exam by tutor group. First set a seed to make sure your results can be reproduced

set.seed(9273)
## a. Generate 10 samples from a normal distribution with mean 20 and sd 8 and save it in a variable 
##  called "tutor1_grades"

## b. Now we generate our second sample of size 10, this time for tutor 2 and with mean 28 and 
## sd 10

## c. Combine the two samples and store the result into one vector called "score" (it should 
##    first show all scores from tutor1 followed by the scores of tutor2)

## d. Create a vector called tutor indicating which tutor the score belongs to: it should show 
##   "tutor1" 10 times followed by "tutor2" 10 times

## e. Create a data frame named "data_frame" having 2 columns "tutor", "score" created above.

## f. run the independent samples TTest (independentSamplesTTest) and formulate the findings as discussed 
###  in the lecture. 

## Time to play around!

## g. Repeat the whole experiment you performed above with different sample size 
##  (the number of samples drawn from each tutor group). How big does your sample need to be in order
##  for the t test to be significant when keeping mean and sd constant?
## make sure to set the seed again before you run your code to be able to reproduce results

## h.	repeat the whole experiment you performed in a-f with different means.
##   What do you find? When is the test more likely to come out significant?

## i.	Now, vary the standard deviation, keeping means and sample size constant!
##   What do you find? When is the test more likely to come out significant?
