### Stats with R Exercise sheet 6

##########################
#Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, December 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## Please use the ggplot2 library for all graphs in this homework.
## Make sure that you answered all subquestions and that the code runs!


## Please write below your (and your teammates') name, matriculation number. 
## Name: 
## Matriculation number: 

###########################################################################################
###########################################################################################

library(languageR)
library(ggplot2)
library(dplyr)
library(carData)
#######################
### Exercise 1: Correlation
#######################

## We will use the dataset UN98 from the package carData. 
## a) Load the package and inspect the data set

## b) create the dataset AsiaMale, containing the variables educationMale lifeMale GDPperCapita 
##    economicActivityMale and illiteracyMale and the subset of Asian countries.

## c) Let's say you're interested in whether there is a linear relationship between 
## illiteracy percentage and life expectancy of males in the different countries. 
## Take a look at the relationship between the two variables by 
## means of a scatterplot (use the ggplot library for this).

## d) Judging from the graph, do you think that the two variables are 
## in any way correlated with one another?

## e) Get the correlations between all variables in the data set using cor().
## Tell R to only include complete pairs of observations. 

## f) Concentrate on the row for the life expectancy in males. Interpret the five numbers you see there
##   explaining for each number which direction the correlation takes and how strong it is.

## g) Is the correlation between life expectancy and GDPperCapita significant? Use cor.test()

## h) Calculate the Spearman rank correlation between life expectancy and GDPperCapita and compare
## it to the pearson correlation calculated above.

## i) make a scatterplot of this relationship.

## j) Looking at the graph, why do you think Spearman's rho is better suited than the Pearson 
## correlation to describe the relationship between the two variables?

## k) Using the function paired.r from the package psych, compare the correlations between life expectancy 
##  and economic activity on the one hand, and life expectancy and illiteracy on the other hand.
##  Hint: the degrees of freedom in a correlation test are equal to N-2

## l) What do you conclude from k?

## m) What would be the result, if the two variables would be independent?

################################
### Exercise 2: Regression
################################


## We will use the same dataset as above, but first scale the GDP to be in the unit of
## thousand dollars
AsiaMale$GDPt = AsiaMale$GDPperCapita/1000

## a) Run a regression model of life expectancy by GDPt and look at the summary.
## General form: 
## "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
## "summary(modelname)"

## b) Interpret the model from a. What do intercept and the coefficient of GDPt tell you?

## c) What about the model fit: What proportion of the total variance is explained by your model?

## d) Now let's turn to the relationship between life expectancy and illiteracy.  Run the regression and 
# interpret.

## e) Plot lifeMale by illiteracyMale and add a regression line to your plot


###################################
### Exercise 3: Multiple Regression
###################################

## We will use the same data set as in 2. This time, we will look at the effect of illiteracy rate
## and GDP on life expectancy simultaneously. 

## a) Run a multiple regression model with illiteracyMale and GDPt as predictors
## General form: 
## "modelname <- lm(outcome ~ predictor1+predictor2+.., data = dataFrame, na.action = an action)"
## "summary(modelname)"

## b) Interpret the model: what do intercept and the 2 coefficients tell you? What about significance?

## c) Compare to the model in 2a (only including GDP), has the model fit improved? How about
## the model in 2d (only including illiteracy)?

## d) Look up the GDP and illiteracyMale for United.States and Brazil in the original data set (UN98)

## e) Using the model from 3a:  What is the predicted life expectancy for United.States and Brazil?
##  Calculate "by hand", i.e. do not use predict() and show your calculation. Don't forget to divide
##  the GDPperCapita by 1000 first!

## f) Run an additional model of life expectancy for the AsiaMale data set including also economicActivityMale

## g) Do you think inclusion of economicActivity into the model is a good idea?
