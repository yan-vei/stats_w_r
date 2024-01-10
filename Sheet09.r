### Stats with R Exercise sheet 9

##################################################################################
# Week 11: Model Families and Logistic Regression
##################################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, January 13. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name:
## Matriculation number:
## Name:
## Matriculation number:
## Name:
## Matriculation number:

##################################################################################
##################################################################################

# The following line of code clears your workspace:
rm(list = ls())

##################################################################################
## Exercise 1: Logistic regression
##################################################################################

require(carData)
require(dplyr)
require(lme4)
require(ggplot2)

## Look at the dataset Arrests from the carData package. It is collecting of data on police 
## treatment of individuals arrested in Toronto for simple possession of small quantities of marijuana.
## The primary variable of interest for us is "released" which indicates whether arrestee was released
## or taken into custody directly

## a) Build a simple logistic regression model that models the probability of release 
##    (binary) based on colour (categorical), age (numeric) and  year (numeric) without 
##    an interaction and store it in mRel. 
##    You have to use the glm() function and specify the family correctly.

data("Arrests")
summary(Arrests)

mRel <- glm(
  data = Arrests,
  released ~ colour + age + year,
  family = binomial
)
## b) Look at the summary. What does the intercept correspond to?

summary(mRel)

# The intercept corresponds to the estimated log odds of the response variable 
# (released in this case) being in the success category when all others predictor
# variables are 0

## c) Were older offenders more likely to be released than young ones?

# Considering that age has a negative coefficient (-1.054e-02), older offenders
# are less likely to be released in comparison to younger inmates. Also seeing 
# the p-value for age is 0.0141 (p-value < 0.05), shows that the effect of age 
# on "Released" is statistically significant.

## d) Imagine the Toronto police arrested two persons for marijuana possession:
##    Jeffrey L. was arrested in 2001, aged 52 and white, Snoop D. was arrested 
##    in 1994, aged 23 and black.
##    Calculate their expected release outcome on the logit scale (i.e. the scale of the model) 
##    either by hand or using predict() with a new data.frame

####################### Using predict() #######################
colour <- c("White", "Black")
age <- c(52, 23)
year <- c(2001, 1994)
name <- c("Jeffery L.", "Snoop D")
test.data = data.frame(name, age, colour, year)



logits <- predict(
  mRel, 
  newdata = test.data
)

## just to check if my calculation is correct 
logits.prob <- predict(
  mRel, 
  newdata = test.data, 
  family = "binomial",
  type = "response"
) 



######## RESULT ########  
##        1         2 ##
## 0.8316179 0.6757119##

### calculation is correct

####################### By hand #######################

### I'll do it later

## e) Transform your results from d to the probability scale, using the formula given on the slides. 
##    You can check your calculation by asserting the probabilities lie in the 0-1 range. 
##    For whom does the model predict the higher probability of release?

logits2prob <- function(x){
  # formula : e^x / (1 + e^x)
  exp(x) / (1 + exp(x))
}

logits2prob(logits)

# from the calculation by hand, we see that Jeffery L. has a higher 
# probability of being released (probability = 0.8316179) as compared to 
# Snoop D (probability = 0.6757119)


##################################################################################
## Exercise 2: Generalized Linear Mixed effect models
##################################################################################

## In this exercise, we will again look at connections between coffee consumption 
## and sleep (among others). The data set "coffee.csv" contains data from 10 students, 
## who reported on 10 randomly chosen days of the year: 
##  sleep:  how many hours of sleep they had in the previous night
##  mood:   how happy they felt on a scale from 1 (very unhappy)-10 (extremely happy)
##  coffee: how many cups of coffee they had on that day
## In addition, the maximal temperature on that day was entered into the dataset.

## Our research hypotheses are: 
## students consume more coffee, when they are tired
## students consume more coffee, if they don't feel well
## students consume more coffee, when it is cold outside

## a) Download the data set from cms and read it in, store it in a variable called: coffeedat

## b) Plot the number of consumed cups of coffee in three individual scatterplots 
##    by sleep, mood, and temperature. 
##    You can use geom_jitter() to get a nicer plot

## c) Can you detect an obvious relationship in any of the plots?

## d) Fit a simple linear regression model with all three predictors and store it in linmod

## e) Fit a generalized linear model with the appropriate family 
##    (hint: coffee is a count variable) and store it in poimod

## f) Look at the two summaries of the models and write what changed?

## g) In fact, we have repeated measures in our design, so refit the model 
##    including a random intercept for subject using glmer() with the correct 
##    family specification and store it in mixedpoi

## h) Look at the summary and report what changed in comparison to both linmod and poimod.

## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin

## j) Compare the AIC for all four models. Which one has the best fit?

## k) And which model is conceptually the appropriate one? Explain why.

## l) Finally, report on the effects of interest in light of our research hypotheses 
##    specified above for the model you chose in k)
