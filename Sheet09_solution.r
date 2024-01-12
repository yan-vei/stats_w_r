### Stats with R Exercise sheet 9

##################################################################################
# Week 11: Model Families and Logistic Regression
##################################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, January 13. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name: Yana Veitsman
## Matriculation number: 7054842
## Name: Anthony Dsouza
## Matriculation number: 7053485
## Name: Tyler Lee
## Matriculation number: 7054832

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

## e) Transform your results from d to the probability scale, using the formula given on the slides. 
##    You can check your calculation by asserting the probabilities lie in the 0-1 range. 
##    For whom does the model predict the higher probability of release?

logits2prob <- function(x){
  # formula : e^x / (1 + e^x)
  exp(x) / (1 + exp(x))
}

logits2prob(logits)

# From the calculation we establish that Jeffery L. has a higher 
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

coffeedat <- read.csv("coffee.csv")

## b) Plot the number of consumed cups of coffee in three individual scatterplots 
##    by sleep, mood, and temperature. 
##    You can use geom_jitter() to get a nicer plot

ggplot(data = coffeedat, aes(x=sleep, y=coffee)) +
  geom_jitter()

ggplot(data = coffeedat, aes(x=mood, y=coffee)) +
  geom_jitter()

ggplot(data = coffeedat, aes(x=temperature, y=coffee)) +
  geom_jitter()

## c) Can you detect an obvious relationship in any of the plots?

# There seem to be slight negative and positive relationships in the first
# and the third plot respectively. The second plot, however, is very disperse
# and the relationship is not clear.

## d) Fit a simple linear regression model with all three predictors and store it in linmod

linmod <- lm(formula = coffee ~ sleep + mood + temperature, 
             data = coffeedat)

## e) Fit a generalized linear model with the appropriate family 
##    (hint: coffee is a count variable) and store it in poimod

poimod <- glm(coffee ~ sleep + mood + temperature, 
              data = coffeedat,
              family = "poisson")

## f) Look at the two summaries of the models and write what changed?

summary(linmod)
summary(poimod)

# The coefficients of all predictors and the intercept have all changed. 
# In the linmod, only the result for temperature, t(3, 96) = 1.553, p < 0.05 is 
# significant. However, in the poimod, the results for all, 
# sleep (t(3, 96)=-2.789, p<0.05), mood (t(3,96)=-3.64, p<0.05)
# and temperature (t(3,96)=2.233, p<0.05)

## g) In fact, we have repeated measures in our design, so refit the model 
##    including a random intercept for subject using glmer() with the correct 
##    family specification and store it in mixedpoi

mixedpoi <- glmer(formula = coffee ~ sleep + mood + temperature + (1|subj),
                data = coffeedat,
                family = "poisson")

## h) Look at the summary and report what changed in comparison to both linmod and poimod.

summary(mixedpoi)

# In mixedpoi model the results for the sleep (t(3,96)=-2.697, p<0.05) and 
# mood (t(3,96)=-4.014, p<0.05) stay significant, while the result for 
# the temperature value is not significant (t(3,96)=1.242, p>0.05).

## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin

mixedlin <- glmer(formula = coffee ~ sleep + mood + temperature + (1|subj),
                data = coffeedat,
                family = gaussian)

## j) Compare the AIC for all four models. Which one has the best fit?

AIC(linmod, poimod, mixedlin, mixedpoi) 

# mixedpoi model approximates the best fit given its AIC value of
# 478.7281, which is the lowest across all models (and the lower the AIC-value)
# is, the better.

## k) And which model is conceptually the appropriate one? Explain why.

# Conceptually, the mixed-effects model for the poisson family seems to be the most
# appropriate. The model accounts for the binary nature of the DV
# as well as for the random effects of the repeated measures and variations
# between the subjects.

## l) Finally, report on the effects of interest in light of our research hypotheses 
##    specified above for the model you chose in k)

# Students consume more coffee, when they are tired - correct. Given 
# t(3,96)=-2.697, p<0.05 for sleep, we can reject the null hypothesis of coffee
# consumed and sleep amount not having a correlation.

# Students consume more coffee, if they don't feel well - correct. Given
# t(3,96)=-4.014, p<0.05 for mood, we can reject the null hypothesis of coffee
# consumed and mood not having a correlation.

# Students consume more coffee, when it is cold outside - incorrect. Given
# t(3,96)=1.242, p>0.05, we cannot reject the null hypothesis of the outside
# temperature and coffee consumption not being correlated.
