### Stats with R Exercise sheet 7

##############################################################################
#Week8: Checking Assumptions underlying ANOVA and linear regression
##############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Thursday, December 28. Write the code below the questions. 
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

# The following line of code clears your workspace.

rm(list = ls())

################################
### Please, use ggplot to make plots in all exercises unless specified differently!
################################

################################
### Exercise 1 ANOVA assumptions
################################
library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(ggplot2)

data_long = anorexia%>% pivot_longer(c(Postwt,Prewt), names_to = "Time", values_to = "Weight") %>%
  mutate(Time = factor(Time, levels=c("Prewt","Postwt")))
## a) Please rerun the two-way interaction model from sheet 5 (modelling weight by time and treatment and
##  their interaction for the data_long data set)

## b) Run the levene test for this model

## c) What do you conclude?

## d) Look at homoscedacity using plots instead

################################
### Exercise 2 LM assumptions
################################

##a) Load the dataset Salaries from package carData and store it in a variable called data. 
# Familiarize yourself with the content of the dataset: https://r-data.pmagunia.com/dataset/r-dataset-package-car-salaries

data <- Salaries

## b) Run a simple regression, just including 'years in service' as predictor and salary as the dependent variable
##  Store it in lm1

lm1 <- lm(formula = salary ~ yrs.service, data=data)

## c) Report and explain the effect of 'years in service'

#### For every year in service, salary increases by 779.6 dollars.

## d) Make a scatterplot of salary by 'years in service', including the regression line

ggplot(data, aes(x=yrs.service, y=salary)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE)

## e) Next, fit a model of salary including 'years in service' and discipline as predictors, store it in lm2

lm2 <- lm(formula = salary ~ yrs.service + discipline + 0, data=data)

## f) Report and explain the effects of 'years in service' and discipline.

#### For every year in service, salary is expected to increase by 862.8 dollars. 
#### The salary with no years of service is expected to be 91335.8 dollars if 
#### the professor is of discipline A. The salary with no years of service is
#### expected to be 104519.9 dollars if the professor is of discipline B.

##  Next we want to plot a model where both predictors are shown. For those we first store the predicted values
## of our model:
data$sal_pred = fitted(lm2)

## g) Now, plot the original data (salary by 'years in service' with different colors for disicpline), but use the 
## fitted values (sal_pred) inside geom_smooth() or geom_line(), otherwise, it will display regression lines 
## assuming an interaction
## The resulting plot should show the data points in different colors and two parallel regression lines.

ggplot(data, aes(x=yrs.service, y=salary, color=discipline)) +
  geom_point() +
  geom_line(data=data,aes(x=yrs.service, y=sal_pred, color=discipline))

## h) Run a regression model that includes also the interaction between 'years in service' and discipline and store it
##  as lm3

lm3 <- lm(formula = salary ~ yrs.service + discipline + 
            yrs.service * discipline + 0, data=data)

## i) Plot the results of the model! (This time no need to specify the pred data set)

ggplot(data, aes(x=yrs.service, y=salary, color=discipline)) +
  geom_point() +
  geom_smooth(se=FALSE)

## j) Report the results of lm3 and interpret with the help of the graph in i)

#### For professors of discipline A, salary is predicted to increase rapidly
#### until about 15 years of service, plateau until about 35 years of service
#### and increase at a moderate rate thereafter.

#### For professors of discipline B, salary is predicted to rapidly increase 
#### until about 10 years of service, increase at a steadily decreasing rate  
#### until about 20 years of service and then decrease at a steadily increasing
#### rate thereafter.

## k) To do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot, 
## see lecture notes for syntax)

## l) Which plot do you turn to to check for homogeneity of residuals (homoscedasticity)? What do you conclude?

## m) Which one do you use for normality of residuals? What is your conclusion?

################################
### Exercise 3 LM outliers
################################

## a) run a lm model on the same data set, but this time, using the predictors discipline and yrs since phd 
##  and their interaction and assign the result to lm4

lm4 <- lm(formula = salary ~ yrs.since.phd + discipline + 
            yrs.since.phd * discipline + 0, data=data)

## b) look at the model summary. Which effects are significant?

#### The effect of years since phd and discipline on salary are significant with
#### alpha = 0.05; however, the effect of the interaction between years since phd 
#### and discipline on salary is not significant.

## c) How do you check for influential data points (aka bad outliers)?

## d) Try fitting the same model with the most suspicious data point excluded. What do you find?