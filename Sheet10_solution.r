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

# predA is binary - it is composed as a vector of 1's and 0's.
# predB is continuous - as it is derived using the rnorm function.

## b) Write down what values you would hope for the model to estimate in the ideal case:

#    i)   intercept   = 12
#    ii)  predA       = 4.3
#    iii) predB       = -2.8
#    iv)  predA:predB = 0.4
#    v)   Residual standard error = 8

m1<- lm(resp~predA*predB, data=d)
summary(m1)  

## c) Can the model recover the original model structure and estimate correct coefficients 
##    for the predictors?

# During different generations we get different values that might be slightly
# off. However, during the 5 suggested runs, we can observe that each value is
# recovered almost correctly at least once.

## d) What happens if you change the number of subjects? (specify the numbers you tried out!)

# Try the following numbers of subjects:
n <- c(20,100,300,500)

# Within the loop, run the same code as above:
for (i in 1:length(n)) {
  
  num_subj <- n[i]
  predA    <- c(rep(1,num_subj/2),rep(0,num_subj/2))
  predB    <- rnorm(num_subj, 3, 2)
  interact <- 0.4*(predA*predB) 
  error    <- rnorm(num_subj, 0, 8)
  resp     <- 12 + 4.3*predA - 2.8*predB + interact + error
  d        <- data.frame(predA, predB, resp)
  
  m<- lm(resp~predA*predB, data=d)
  print(summary(m))
}

# It seems that as the number of observations grows, the precision of the 
# prediction of the model grows as well.

## e) What happens if you change the variance of the error term? (specify the numbers you tried out!)

# Try the following distributions of the error term - with the smaller/bigger
# sd and similar mean:
error1 <- rnorm(n, 0, 2)
error2 <- rnorm(n, 0, 16)

# Run the code above for the specified errors
n        <- 200 
predA    <- c(rep(1,n/2),rep(0,n/2))
predB    <- rnorm(n, 3, 2)
interact <- 0.4*(predA*predB)
resp1    <- 12 + 4.3*predA - 2.8*predB + interact + error1
resp2    <- 12 + 4.3*predA - 2.8*predB + interact + error2
d1       <- data.frame(predA, predB, resp1)
d2       <- data.frame(predA, predB, resp2)

m1 <- lm(resp1~predA*predB, data=d1)
m2 <- lm(resp2~predA*predB, data=d2)

summary(m1)
summary(m2)

# The modification of the error term respectively increases/descreases the spread 
# of the error variability in the specified distribution for the intercept,
# predictors and their interaction. The higher deviation of the error means
# the higher error of the model's prediction and vice versa.

## f) What happens if you change the effect sizes? (specify the numbers you tried out!)

effectA1 <- 2
effectA2 <- 8
effectB1 <- 1
effectB2 <- 5

# Run the code as above to monitor the changes

################## THIS RANDOMLY STARTED ERRORING OUT, NOT SURE WHY

n        <- 200 
predA    <- c(rep(1,n/2),rep(0,n/2))
predB    <- rnorm(n, 3, 2)
interact <- 0.4*(predA*predB)

respA1    <- 12 + effectA1*predA - 2.8*predB + interact + error
respA2    <- 12 + effectA2*predA - 2.8*predB + interact + error
respB1    <- 12 + 4.3*predA - effectB1*predB + interact + error
respB2    <- 12 + 4.3*predA - effectB2*predB + interact + error
respA1B1  <- 12 + effectA1*predA - effectB1*predB + interact + error

dA1       <- data.frame(predA, predB, respA1)
dA2       <- data.frame(predA, predB, respA2)
dB1       <- data.frame(predA, predB, respA2)
dB2       <- data.frame(predA, predB, respA2)
dA1B1     <- data.frame(predA, predB, respA1B1)

m1 <- lm(respA1~predA*predB, data=dA1)
m2 <- lm(respA2~predA*predB, data=dA2)
m3 <- lm(respB1~predA*predB, data=dB1)
m4 <- lm(respB2~predA*predB, data=dB2)
m5 <- lm(respA1B1~predA*predB, data=dA1B1)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

# From the effect size's changes, we can see that 
# an increase in the effect size increases the relationship
# between the predictor variable and the outcome variable and vice
# versa. It also influences the variability of the interaction term
# of the predictors, making one predictor or another more or less prominent
# based on the bigger or smaller effect size.

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

power_predA1 <- mean(results[, "predA"] < 0.05)
power_predB1 <- mean(results[, "predB"] < 0.05)
power_interaction1 <- mean(results[, "interaction"] < 0.05)

print(power_predA1)
print(power_predB1)
print(power_interaction1)

## h) How does power change when you decrease your alpha level to 0.01?

power_predA2 <- mean(results[, "predA"] < 0.01)
power_predB2 <- mean(results[, "predB"] < 0.01)
power_interaction2 <- mean(results[, "interaction"] < 0.01)

print(power_predA2)
print(power_predB2)
print(power_interaction2)

# With the decrease of alpha to 0.01, the power changes for the predA and
# the interaction, becoming less. The power for the predB stays the same and is
# equal to 1.

## i) Why do you think power is higher for predB compared to predA?

# Even though the effect size of predB is less, it is the case that the variance of
# predB is also lower, so the power for predB is actually higher.

## j) Let's do a power calculation: Given the effect sizes from above, find the minimal sample size n 
##  for which:
##    a. we have 80% power to find an effect of predA
##    b. we have 95% power to find an effect of predB
##  You can use the code from above and try out different values, or include it in another loop to test
##  multiple sample sizes simultaneously

# Initialize all the necessary variables for the calculation
power_predA <- 0.8
power_predB <- 0.95
n <- 360

for(i in c(1:sim)){
      predA    <- c(rep(1,n/2),rep(0,n/2))
      predB    <- rnorm(n, 3, 2)
      interact <- 0.4*(predA*predB)
      error    <- rnorm(n, 0, 8)
      resp     <- 12 + 4.3*predA - 2.8*predB + interact + error
      d        <- data.frame(predA, predB, resp)
      m1       <- lm(resp~predA*predB, data=d)
      
      results[i,] = summary(m1)$coefficients[,4]
  }
  
power_A <- mean(results[, "predA"] < 0.05)
power_B <- mean(results[, "predB"] < 0.05)
  
if (power_A >= power_predA) {
    print(power_A) 
}
  
if (power_B >= power_predB) {
    print(power_B)
}

# At around n=360 (different for different trials), power of A and power of B
# seem to both stabilize and be above the desired values of 0.8 and 0.95.

