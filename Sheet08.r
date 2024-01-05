### Stats with R Exercise sheet 8

##############################################################################
# Week 10: Linear Mixed Effects Models
##############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, January 6. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name:
## Matriculation number:
## Name:
## Matriculation number:
## Name:
## Matriculation number:

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
  #summarySE(measurevar = "weight")

aggDataSummary <- summarySE(data = aggData, measurevar = "weight")


##    2) Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##       Also add errorbars (mean+/-1.96*se)

aggDataSummary <- aggDataSummary %>%
  mutate(up_ci = weight + 1.96 * se,
         low_ci = weight - 1.96 * se)

ggplot(data = aggData, mapping = aes(x = Time, y = weight, color = Diet)) +
  geom_line() +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = aggDataSummary$low_ci, ymax = aggDataSummary$up_ci), width = 0.2)

## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##    by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##    instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##    actual data

ggplot(data = ChickWeight, mapping = aes(x = Time, y = weight, color = Chick)) +
  geom_line() +
  facet_wrap(~ Diet, nrow = 1, scales = "free_y")

## d) What do you observe, looking at c?

### Over time, the weight of the chicks increases linearly (almost) for most Chicks. Diet 4 and Diet 1 seem the best
### and worst respectively

## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##    looking for an interaction between time after birth and the diet type. Before running the model,
##    specify:

##    1) What fixed effect(s) do you enter into the model?

### Diet and time will be the fixed effects as these are variables we can
### control in our experiment.

##    2) what random effect(s) should be included to account for the repeated measures structure of the data?

### the Chicks will be he random effects as we have no control on how they will respond
### to the fixed effects (diet and time).

##    3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?

### We should add Diet | Chick and Time | Chick

## f) Run the model you specified in e) using lmer() and assign it to chickmod

chickmod <- lmer(weight ~ Diet * Time + (1|Chick), 
                         data = ChickWeight)

chickmod.test <- lmer(weight ~ Diet * Time + (Diet|Chick), 
                 data = ChickWeight)

chickmod.control <- lmer(weight ~ Diet * Time + (1|Chick), 
                 data = ChickWeight,
                 control = lmerControl(optimizer = "Nelder_Mead"))

chickmod.diet <- lmer(weight ~ Diet * Time + (Diet|Chick),
                      data = ChickWeight,
                      control = lmerControl(optimizer = "Nelder_Mead"))
# chickmod.time <- lmer(weight ~ Diet * Time + (1 + time|Chick), data = ChickWeight)


## g) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull
chicknull <- lmer(weight ~ Diet + Time + (1|Chick), data = ChickWeight)


## h) compare the two models using the anova() function, which performs a likelihood ratio test
anova(chickmod, chicknull)
anova(chickmod.control, chicknull)

## i) Report the p-value (from h) and the conclusion with respect to the research hypothesis


## j) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])



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



## b) You want to relate students' performance in the advanced algebra course in a summer school in Saarbrücken
##    to their final math grade in school. The summer school course has 200 participants, coming from 8 different
##    partner Universities from all over Germany. These 200 participants were randomly split into 10 tutorial groups,
##    where each tutorial was held by a different tutor.
##    Given the design of your study, what random effects should you add to the following model:
##    NOTE: We accept only answers with explanations!

## lmer(advancedalgebrascore ~ mathGrade, someData)








#################################### REFERENCE ######################################

# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015590.html

# https://stats.stackexchange.com/questions/242109/model-failed-to-converge-warning-in-lmer

# https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html


