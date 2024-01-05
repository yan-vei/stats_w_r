### Stats with R Exercise sheet 7

##############################################################################
#Week8: Checking Assumptions underlying ANOVA and linear regression
##############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Thursday, December 28. Write the code below the questions. 
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

aov2way <- aov(formula=Weight ~ Time * Treat, data = data_long)

## b) Run the levene test for this model

leveneTest(aov2way)

## c) What do you conclude?

### Given Time and Treatment (F(5, 138)=1.9042, p>0.05) we cannot reject the null 
### hypothesis about the equality of variance in the 2 groups, hence we can assume
### their homoscedacity.

## d) Look at homoscedacity using plots instead

### Using residuals vs. fitted
ggplot(data_long, aes(fitted(aov2way), residuals(aov2way))) +
  geom_point(color="blue") +
  geom_hline(yintercept=0, linetype="dashed", color="red")

### Using Scale-Location
ggplot(data_long, aes(fitted(aov2way), residuals(aov2way))) +
  geom_point(color="blue") +
  geom_smooth(se=FALSE, method="lm", color="red")

### Using residuals vs.leverage
ggplot(data_long) +
  geom_point(aes(x=hatvalues(aov2way), y=rstandard(aov2way)), color="blue") +
  geom_hline(yintercept=0, linetype="dashed", color="red")

################################
### Exercise 2 LM assumptions
################################

##a) Load the dataset Salaries from package carData and store it in a variable called data. 
# Familiarize yourself with the content of the dataset: https://r-data.pmagunia.com/dataset/r-dataset-package-car-salaries

data <- Salaries

## b) Run a simple regression, just including 'years in service' as predictor and salary as the dependent variable
##  Store it in lm1

lm1 <- lm(formula = salary ~ yrs.service, data=data)
summary(lm1)

## c) Report and explain the effect of 'years in service'

### For every year in service, salary increases by 779.6 dollars.

## d) Make a scatterplot of salary by 'years in service', including the regression line

ggplot(data, aes(x=yrs.service, y=salary)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE)

## e) Next, fit a model of salary including 'years in service' and discipline as predictors, store it in lm2

lm2 <- lm(formula = salary ~ yrs.service + discipline + 0, data=data)
summary(lm2)

## f) Report and explain the effects of 'years in service' and discipline.

### For every year in service, salary is expected to increase by 862.8 dollars. 
### The salary with no years of service is expected to be 91335.8 dollars if 
### the professor is of discipline A. The salary with no years of service is
### expected to be 104519.9 dollars if the professor is of discipline B.

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

### For professors of discipline A, salary is predicted to increase rapidly
### until about 15 years of service, plateau until about 35 years of service
### and increase at a moderate rate thereafter.

### For professors of discipline B, salary is predicted to rapidly increase 
### until about 10 years of service, increase at a steadily decreasing rate  
### until about 20 years of service and then decrease at a steadily increasing
### rate thereafter.

## k) To do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot, 
## see lecture notes for syntax)

par(mfcol=c(2,3))
plot(lm3)
which=seq(1,6)

## l) Which plot do you turn to to check for homogeneity of residuals (homoscedasticity)? What do you conclude?

### You turn to the Scale-Location plot. The red line is approximately flat and 
### horizontal. This indicates that variance is nearly constant. 

## m) Which one do you use for normality of residuals? What is your conclusion?

### You use the Q-Q Residuals plot for the normality of residuals. The points 
### approximately follow the straight, dotted line, which indicates that the 
### residuals are normally distributed.

################################
### Exercise 3 LM outliers
################################

## a) run a lm model on the same data set, but this time, using the predictors discipline and yrs since phd 
##  and their interaction and assign the result to lm4

lm4 <- lm(formula = salary ~ yrs.since.phd + discipline + 
            yrs.since.phd * discipline + 0, data=data)
summary(lm4)

## b) look at the model summary. Which effects are significant?

### Years since Phd and Discipline on salary with (F(393,4)=1873, p<0.05) are
### significant; however, the interaction b/n Years since Phd and discipline is 
### not significant with pr(>t)>0.05.

## c) How do you check for influential data points (aka bad outliers)?

### Let's use leverage values for this
ggplot(data, aes(hatvalues(lm4), rstandard(lm4))) +
  geom_point(color="blue") +
  geom_hline(yintercept=0, linetype="dashed", color="red")

### High leverage points extend along x-axis. 

## d) Try fitting the same model with the most suspicious data point excluded. What do you find?

### Let's exclude the data point with the biggest hat value.
index <- which.max(hatvalues(lm4))

### Exclude the value from the dataset
data_with_excluded <- data[-index, ]

### Run the same model and show summary
lm_with_excluded <- lm(formula = salary ~ yrs.since.phd + discipline + 
            yrs.since.phd * discipline + 0, data=data_with_excluded)
summary(lm_with_excluded)

### The interaction b/n Years since Phd and Discipline with (F(392, 4)=1859, p<0.05)
### is significant.
