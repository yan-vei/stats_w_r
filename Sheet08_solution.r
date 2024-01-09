### Stats with R Exercise sheet 8

##############################################################################
# Week 10: Linear Mixed Effects Models
##############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, January 6. Write the code below the questions. 
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

library(languageR)
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(dplyr)


###############################################################################
### 1. Linear mixed model for chicken growth 
###############################################################################

## a) We will first look at the dataset ChickWeight, which is already 
##    loaded in base R. Check out the help page of the data set to understand 
##    how the data was collected and look at the summary.
data("ChickWeight")
?ChickWeight

## b) Let's plot the data. 
##    1) Group the data by Diet and Time. Use a function summarySE() 
##       from Rmisc library to get the mean and se of weight. 
##       Assign resulting dataset to aggData.

library(Rmisc)
?summarySE()

aggData  <- ChickWeight%>%
  group_by(Diet, Time) 

aggDataSummary <- summarySE(data = aggData, measurevar = "weight")


##    2) Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##       Also add errorbars (mean+/-1.96*se)

aggDataSummary <- aggDataSummary %>%
  mutate(up_ci = weight + 1.96 * se,
         low_ci = weight - 1.96 * se)

ggplot(data = aggData, mapping = aes(x = Time, y = weight, color = Diet)) +
  geom_line() +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = aggDataSummary$low_ci, ymax = aggDataSummary$up_ci), width = 0.3) +
  facet_wrap(~ Diet, nrow = 1, scales = "free_y")

## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##    by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##    instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##    actual data

ggplot(data = ChickWeight, mapping = aes(x = Time, y = weight, color = Chick)) +
  geom_line() +
  facet_wrap(~ Diet, nrow = 1, scales = "free_y")

## d) What do you observe, looking at c?

### Over time, the weight increases linearly for chicks of all diets. 
### However, Diet 1 seems to be influencing the weight less, given that the majority
### of the chicks only reach the weight of 200 by the end of the measuring period.
### On the contrary, Diets 3 and 4 seem to be the best fit, given that most 
### of the chicks grow beyond the weight of 200.

## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##    looking for an interaction between time after birth and the diet type. Before running the model,
##    specify:

##    1) What fixed effect(s) do you enter into the model?

###      Diet and time will be the fixed effects as these are variables are assumed
###      to have a constant effect on the DV.

##    2) what random effect(s) should be included to account for the repeated measures structure of the data?

###      The random effect for an each individual chick should be included, so we
###      can account for the individual variability between the subjects.

##    3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?

###      We need to add a random intercept for Chick and a random slope for Time.


## f) Run the model you specified in e) using lmer() and assign it to chickmod
chickmod <- lmer(weight ~ Diet * Time + (1 + Time | Chick), data = ChickWeight)


## g) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull
chicknull <- lmer(weight ~ Diet + Time + (1 + Time|Chick), 
                 data = ChickWeight)

## h) compare the two models using the anova() function, which performs a likelihood ratio test
anova(chickmod, chicknull)

### Given the (chicknull, AIC) = 4834.1, (chicknull, BIC) = 4873.3 and
### (chickmod, AIC) = 4824.2, (chickmod, BIC) = 4876.5, and corresponding log likelihoods
### of -2408 and -2400.1, the chickmod model seems to be a slightly better fit
### to the data.

## i) Report the p-value (from h) and the conclusion with respect to the research hypothesis

### Given (3,22) and p=0.001217 at the significance level of 0.05, we can reject the 
### the null hypothesis and conclude that the more complex model (chickmod)
### is the better fit, confirming our suggestion from h.

## j) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])

### From the plot we can see that there seems to be a high variability in how 
### chicks respond to fixed effects. There seem to be no apparent clusters
### or outliers in the plot, however, the slope is steep, which indicates
### a substantial change in weight.

#####################################################
### 2. Random effect structures 
#####################################################

## a) Let's return to the lexdec data set from Sheet 4 and suppose, we want to look 
##    at effects of the word type of the previously presented word (each subject saw a 
##    different randomized sequence) and effects of the complexity of the word itself, while 
##    taking into account the dependence between data points collected on the same word and from the same subject. 
##    Which of the following models has a maximal random effect structure given the experimental design?
##    Motivate your choice.

m1 = lmer(RT ~ PrevType + Complex + (PrevType        |Subject) + (         Complex| Word), lexdec)
m2 = lmer(RT ~ PrevType + Complex + (PrevType+Complex|Subject) + (PrevType        | Word), lexdec)
m3 = lmer(RT ~ PrevType + Complex + (PrevType+Complex|Subject) + (PrevType+Complex| Word), lexdec)
m4 = lmer(RT ~ PrevType + Complex + (         Complex|Subject) + (PrevType        | Word), lexdec)
m5 = lmer(RT ~ PrevType + Complex + (PrevType+Complex|Subject) + (1               | Word), lexdec)

anova(m1, m2, m3, m4, m5)

### m4(AIC=-950.28, BIC=-896.14) appears to be the best fit model, based on the 
### ANOVA results.
### m4 captures random intercepts for Complex within Subject and PrevType within 
### Word, and it seems to be the best fit for the variability in the study.
### other models include more random slopes and intercepts as necessary,
### which might lead to overfitting (aka simple memorization) of the data.

## b) You want to relate students' performance in the advanced algebra course in a summer school in Saarbrücken
##    to their final math grade in school. The summer school course has 200 participants, coming from 8 different
##    partner Universities from all over Germany. These 200 participants were randomly split into 10 tutorial groups,
##    where each tutorial was held by a different tutor.
##    Given the design of your study, what random effects should you add to the following model:
##    NOTE: We accept only answers with explanations!

### In this case, we need to account for both between and within subject variability.

### We should include the individual variability between students as a random effect to
### account for the within-subject variability.

### Our random intercepts should include the tutorial groups nested within universities
### to alleviate the variability of the students backgrounds as well as the tutors
### themselves (given that individual tutor's teaching might also influence the DV)
### to account for between-subject variability.


### The formula therefore is:
#lmer(advancedAlgebraScore ~ mathGrade + (tutorialGroup + student | university) 
#+ (1 + student) + (student | tutor), data=ourData)
### Where mathGrade is the fixed effect, (tutorialGroup + student | university) 
### is the inclusion of the random intercept and the slope of the tutorial group
### and student nested within the university, and (student | tutor) is the inclusion
### of the random intercept and slope reflecting the influence of the tutor on 
### the student.


#################################### REFERENCE ######################################

# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015590.html

# https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer

# https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html


