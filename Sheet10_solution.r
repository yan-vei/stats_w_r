### Stats with R Exercise sheet 10

###############################################################################
# Week 12: Model Selection, Transformations, Power
###############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, January 20. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name: Yana Veitsman
## Matriculation number: 7054842
## Name: Anthony Dsouza
## Matriculation number: 7053485
## Name: Tyler Lee
## Matriculation number: 7054832

###############################################################################
###############################################################################

# The following line of code clears your workspace:
rm(list = ls())


###############################################################################
### Exercise 1 Simplifying random effect structures
###############################################################################

library(lme4)
library(languageR)

##  Using the lexdec data set again, you want to fit the model that tests for 
##  effects of complexity, the type of the previous Word and the native 
##  language of the participant:

m <- lmer(RT ~ PrevType + Complex + NativeLanguage + 
              (PrevType + Complex | Subject) + (PrevType + NativeLanguage | Word), 
        data = lexdec, REML = F)

## a) Unfortunately, the maximal model given above gives a warning that indicates 
##    that the model is too complex for the data. In order to get a model that converges 
##    without warnings, try to use backwards selection on the random effects. 
##    First exclude the random effect that is least contributing to the model fit and so on 
##    (this may require multiple steps and a large number of fitted models!). 
##    Use model comparison to decide which effects can be excluded.
##    You may exclude random effects only, if they don't contribute significantly with alpha set to 0.1

m1 <- lmer(RT ~ PrevType + Complex + NativeLanguage + (PrevType | Subject) + 
             (PrevType + NativeLanguage | Word), data=lexdec, REML=F)
m2 <- lmer(RT ~ PrevType + Complex + NativeLanguage + (PrevType + Complex | Subject) +
             (PrevType | Word), data=lexdec, REML=F)
m3 <- lmer(RT ~ PrevType + Complex + NativeLanguage + (Complex | Subject) + 
             (PrevType + NativeLanguage| Word), data=lexdec, REML=F)
m4 <- lmer(RT ~ PrevType + Complex + NativeLanguage + (Complex | Subject) +
             (PrevType | Word), data=lexdec, REML=F)

# m1 fails to converge.
# Try to figure out which model to use by applying ANOVA on m2, m3, and m4.

anova(m2, m3)
anova(m4, m2)
anova(m4, m3)

# From ANOVA, we see that m3 is better fit than m4, based on AIC(m3)=-951.52
# and BIC(m3)=-891, t(3,14)=4.0948, p>0.05 is not significant.
# m2 is better fit than m3, based on AIC(m2)=-969.89, BIC(m2)=-894.09,
# t(3,14)=24.372, p<0.05, which is significant.

## b) Comment on your result in a): were you able to produce a suitable model without convergence problems?

# Yes, by removing the random intercept and the slope for Complex based on the
# Subject grouping, we were able to produce a suitable converging model.

## c) Another approach is to simplify the random effect structure by excluding correlations. 
##    Try out whether this would have solved the problem.

m_simplified1 <- lmer(RT ~ PrevType + Complex + NativeLanguage + (1 | Subject) + 
                       (PrevType + NativeLanguage | Word), data=lexdec, REML=F)
m_simplified2 <- lmer(RT ~ PrevType + Complex + NativeLanguage + 
                        (PrevType + Complex | Subject) + (1 | Word), 
                      data=lexdec, REML=F)

# m_simplified1 gives a boundary warning, indicating a potential issue with 
# the model.
# m _simplified2, however, converges.


###############################################################################
### Exercise 2 Simulations and power
###############################################################################

## In the following we provide you with code for simulations. 
## The goal of the exercise is for you to try out the code and understand what it does.
## Please always execute the code at least 5 times for each subquestion, to see how stable 
## the results are -- this is necessary because we are sampling the data randomly, 
## so it could be that we sometimes get more or less "lucky" draws. 


n        <- 200 # number of observations to be simulated
predA    <- c(rep(1,n/2),rep(0,n/2))
predB    <- rnorm(n, 3, 2)
interact <- 0.4*(predA*predB) 
error    <- rnorm(n, 0, 8)
resp     <- 12 + 4.3*predA - 2.8*predB + interact + error
d        <- data.frame(predA, predB, resp)

## a) What type of predictors are predA and predB?

## b) Write down what values you would hope for the model to estimate in the ideal case:

#    i)   intercept   = 
#    ii)  predA       = 
#    iii) predB       = 
#    iv)  predA:predB = 
#    v)   Residual standard error = 

m1<- lm(resp~predA*predB, data=d)
summary(m1)  

## c) Can the model recover the original model structure and estimate correct coefficients 
##    for the predictors?

## d) What happens if you change the number of subjects? (specify the numbers you tried out!)

## e) What happens if you change the variance of the error term? (specify the numbers you tried out!)

## f) What happens if you change the effect sizes? (specify the numbers you tried out!)


## Next we include the above code into a loop to calculate the power of the experiment 
## number of simulated data sets
sim = 1000 # number of simulations
n   = 200  # number of participants in each simulation

## results matrix
results = matrix(nrow=sim, ncol=4)

colnames(results) <- c("Intercept", "predA", "predB", "interaction")
for(i in c(1:sim)){
  predA    <- c(rep(1,n/2),rep(0,n/2))
  predB    <- rnorm(n, 3, 2)
  interact <- 0.4*(predA*predB)
  error    <- rnorm(n, 0, 8)
  resp     <- 12 + 4.3*predA - 2.8*predB + interact + error
  d        <- data.frame(predA, predB, resp)
  m1       <- lm(resp~predA*predB, data=d)
  
  ## store the resulting p-values in the results matrix
  results[i,] = summary(m1)$coefficients[,4]
}

## g) We use the above code and the results matrix to calculate power. Recall that the power is 
##    the probability of rejecting the Null hypothesis, given a specific effect size.
##    We can approximate this by calculating the proportion of simulated datasets, 
##    where the effect comes out significant, i.e. below 0.05. 
##    Calculate the power based on the simulations for all three effects of interest 
##    (i.e., predA, predB and the interaction) individually.

## h) How does power change when you decrease your alpha level to 0.01?

## i) Why do you think power is higher for predB compared to predA?

## j) Let's do a power calculation: Given the effect sizes from above, find the minimal sample size n 
##  for which:
##    a. we have 80% power to find an effect of predA
##    b. we have 95% power to find an effect of predB
##  You can use the code from above and try out different values, or include it in another loop to test
##  multiple sample sizes simultaneously
