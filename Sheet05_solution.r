### Stats with R Exercise sheet 5

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, December 9. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name: Yana Veitsman
## Matriculation number: 7054842
## Name: Anthony Dsouza
## Matriculation number: 7053485
## Name: Tyler Lee
## Matriculation number: 7054832

###########################################################################################



#################################
### Exercise 1: One-way ANOVA
#################################

library(ggplot2)
library(dplyr)
library(MASS)
library(car)
library(tidyr)

## This time we will be working with the "anorexia" data frame (package 'MASS') 

## This is a data set of a clinical study with 3 conditions: Two groups received an active treatment,
## while the control group did not receive treatment. The study population is anorexia patients
## and the recorded response is the weight before the study and the weight after the study for
## for each patient.


## a) Load the dataset, store it into a variable called "data", and briefly inspect it. 
## Feel free to make some plots and calculate some statistics in order to understand 
## the data.

data <- anorexia

## b) In a first step, we will concentrate on the dependent variable Postwt and
##  Treat as the predictor variable (we will assume that the weight before treatment is comparable between groups). 
##  Please formulate a sensible research hypothesis.

#### Treatment (CBT or FT ) has an effect on post-treatment weight. 
#### Null hypothesis: Treatment does not have an effect on post-treatment
#### weight.

## c) Build a boxplot of Postwt depending on "Treat". Please use ggplot here and below!

ggplot(data, aes(x=Treat,y=Postwt)) +
  geom_boxplot()

## d) Looking at the boxplots, is there a difference between the weight between the
##  3 treatment groups?

#### Yes, in comparison to the control group, the mean post-treatment weight is
#### modestly higher for those who underwent CTB and much higher for those who
#### underwent FT.

## e) Now we are ready to perform 1-way ANOVA: please use the function aov() on 
## Postwt depending on Treat and assign the result to aov1way

aov1way <- aov(formula = Postwt ~ Treat, data=data)

## f) Look at the summary of aov1way

summary(aov1way)

## g) State your conclusion

#### The F-value is 8.651 and the p-value is 0.000444 (significant), which 
#### indicates that treatment is associated with different post-treatment 
#### weights.

## h) Use paired.t.test in order to test which levels of Treat are actually different. Use
## "bonferroni" as the method of p-value adjustment.

pairwise.t.test(data$Postwt, data$Treat, p.adjust.method = 'bonferroni') 

## i) Bonferroni is known to be a conservative method: it preserves the nominal alpha level,
##  but lacks power to detect effects. An alternative is the "holm" method, which also
##  preserves the overall alpha level, but is less conservative. Try this method.

pairwise.t.test(data$Postwt, data$Treat, p.adjust.method = 'holm')

## j) State your conclusions.

#### Using Bonferroni, the p-value between the control group and FT is 0.0003
#### (significant), which indicates that FT is associated with a different 
#### post-treatment weight. The p-value between CBT and the control group is 
#### 0.0680 (not significant) and the p-value between CBT and FT is 0.1040, 
#### (not significant), which indicates CBT is not associated with a different 
#### post-treatment weight in comparison to the control group or FT.

#### Using Holm, the p-value between the control group and FT is 0.0003
#### (significant), the p-value between CBT and the control group is 
#### 0.0453 (significant) and the p-value between CBT and FT is 0.0453, 
#### (significant), all of which indicates the difference between any two of  
#### the three groups is associated with different post-treatment weight.

##################################
### Exercise 2: 2-way ANOVA
##################################

## Above, we have only looked at post treatment weights. If the sample is big and
## patients were randomly assigned to treatment groups, this is fine to measure the
## success of the treatment as we can assume that weight before the treatment is 
## similar between groups.

## a) Create a graph to see whether prewt is similar between Treat groups.
ggplot(data, aes(x=Treat,y=Prewt)) +
  geom_boxplot()

## b) What is your conclusion?

#### Given the control group, the mean pre-treatment weight is
#### only slightly more that in the groups that later received CBT and FT,
### so the difference is negligible. 

## Next, we will transform the data set, such that we have one variable combining
## both Prewt and Postwt values and an additional factor coding for Time. This will allow us
## to directly address the change in weight under different treatments in a factorial
## ANOVA.
## Please run the following command.

data_long = anorexia%>% pivot_longer(c(Postwt,Prewt), names_to = "Time", values_to = "Weight") %>%
  mutate(Time = factor(Time, levels=c("Prewt","Postwt")))
summary(data_long)

## c) Plot boxplots for the distribution of `Weight` for each of the `Time` 
## values for data_long. Build 3 plots (each containing 2 boxplots) side by side depending on the 
## `Treat` variable.

ggplot(data_long, aes(x=Time,y=Weight, fill=Treat)) +
  geom_boxplot()

## d) Describe the pattern you observe in c)

### The mean of the weight is not significantly different between the three groups
### in the first boxplot that plots the pre-treatment weight.
### However, the mean weight in the case of the post-treatment measurements
### is significantly more in the case of the group that received FT, the CBT
### group's weight increase is not significant, and the control group's weight
### hasn't changed.

## e) build a two-way ANOVA including Time and Treat as predictors and their interaction
##  and assign it to aov2way.

aov2way <- aov(formula=Weight ~ Time * Treat, data = data_long)
summary(aov2way)

## f) Report your results in line with the research question.

#### The F-value for Time:Treat is 3.828 and the p-value is 0.024097. this indicates that with 
#### an alpha level of 0.05, we can reject the null hypothesis because at least
#### one of the groups has a significantly different mean from the others.

## g) In order to evaluate the interaction, we will use pairwise tests again. The
## function, we are going to use here is TukeyHSD. Please call the function on the 
##  two-way anova

tukey_hsd <- TukeyHSD(aov2way, "Time:Treat")

## h) The interaction between Time and Treat produces 15 (!) different comparisons,
##  but not all of them are meaningful to us. Please select three comparisons to report, 
##  which conceptually make most sense! Explain your choice!

### Conceptually, the combinations of Prewt/Postwt:TreamentOption-Cont with the
### biggest difference in mean would make the most sense to report on given
### the null hypothesis.

top_diffs <- as.data.frame(tukey_hsd$`Time:Treat`[, c("diff", "lwr", "upr", "p adj")])
top_diffs <- top_diffs[order(top_diffs$diff, decreasing = TRUE), ][1:3, ]
top_diffs

#################################################
### Exercise 3: independence assumption
#################################################

## The two-way ANOVA above violates the independence assumption.
##  a) Explain why.

### The two-way ANOVA from above violates the independence assumption because 
### we are measuring the differences in weights over time, so it is likely
### that there will be correlations between the pre-treatment and post-treatment
### weights within each one of the treatment options groups.So there is interaction
### between the factors.

##  b) Can you think of a way to conduct an ANOVA on this dataset without violating
##  the independence assumption, but taking into account differences between groups 
##  prior to treatment?

### We can include the interaction factor into account and conduct the test that way.
