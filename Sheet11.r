### Stats with R Exercise sheet 11

###############################################################################
#Week 14: Bayesian statistics 2
###############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, February 5. Write the code below the questions. 
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

## The following line of code clears your workspace.
rm(list = ls())


###############################################################################
### Exercise 1: a first Bayesian regression model
###############################################################################

##  We will again be using the salaries dataset from library carData 
##  We will run a multiple regression model, and repeat it 
##  as a Bayesian analysis using package 'brms'.
##  As a first step, please run the following lines, that create a centered version of the
##  variable yrs.since.phd and apply sum coding to the factor discipline.
##  As a result, the intercept now represents the salary of a professor with mean number
##  of years since their phd and averaged across the two disciplines. This will make later
##  sampling more stable. 

require(carData)
data = Salaries
data$salary = data$salary/1000
data$yrs.since.phd = scale(data$yrs.since.phd, scale=FALSE)
contrasts(data$discipline) = contr.sum(2)

## a) Load the package brms


## b) Fit a (frequentist) linear model of salary including yrs.since.phd, discipline, and their interaction 
##    as predictors, store it in lm1. 


## c) Fit the same model as a Bayesian regression using the function brm() 
##    and using only defaults (you don't need to specify priors or fitting 
##    parameters like chains and iterations). Store it in bm1
##    Note that estimating a Bayesian model needs more time than a linear model - 
##    1-5 minutes are normal (depending on your computer)


## d) Look at the summaries of bm1 and lm1


## e) How do the parameter estimates compare?


## f) Plot the bayesian model using the default 'plot' function. This will give you the posteriors 
##   of the model parameters as well as the trace plots, which give you an indication 
##   of the convergence of your model. The trace plots are supposed to look like  
##   "fat hairy caterpillars", i.e. the different chains should not be separated in 
##   any part of the plot and there should not be a general pattern. Is this the case?


## g) Store the posterior samples of b_yrs.since.phd in the variable ps_yrs. 
##    Use the function as_draws_df()


## h) A sociologist claims that salary can't increase by more than 1250 Dollars a year
## for professors. What is the probability of the year effect being bigger than 1.25 (=1250 Dollars)
## given your posterior samples?  Do you agree with the sociologist?


## i) Derive 95% and 80% credible intervals from ps_yrs Compare to the results in d)


## j) We want the model to run quicker. Change the settings such that each chain 
##    only has 200 iterations with 1/4 of them as warmup. Store the result in bm2 
##    and look at summary and trace plots. Use the provided seed to be able to better 
##    compare your results (or try a different one, but provide it together with your answer!)
set.seed(1111)


## k) Do you think reducing the iterations was a good idea? Give reasons!


###############################################################################
### Exercise 2: Hypothesis testing
###############################################################################

## We will continue with our model from above (bm1)

## a) Let us concentrate on the interaction effect. What does the credible interval suggest? 
##  Comment on it!


## Next, we will use Bayes factor to argue whether the interaction effect is improving the model.
## Since here the priors are more important, we will use proper priors (=that sum to 1) instead
## of the flat priors set by default.

## b) Look at the help function for the function prior, which you will have to call inside brm


## c) We will use quite vage normal priors that are theory-independent (centered around 0) and have a 
##  standard deviation of 20. Please rerun bm1 as bm3 using these priors for all 3
##  regression coefficients and setting the option save_pars to save_pars(all = TRUE), as this is
##  needed for the calculation of Bayes factor.


## d) double-check, whether your priors are correct using the function prior_summary(). 
##  It should show your normal prior for all coefficients, but student default priors for intercept
##  and sigma.


## e) Now run the same model again, but without the interaction. Store it in bm3_reduced


## f) call the function bayes_factor() on the two models. 


## g) What is your conclusion? Note that the function gives the Bayes factor with the first model 
##  in the enumerator, the resulting Bayes factor thus quantifies the evidence in favor of the first model.
##  You can switch the order to get the Bayes factor that gives evidence for the second model over the first one.


## h) Now calculate the Bayes factor for the main effect of discipline. Here, you have to start with the
##  model without the interaction and compare to a model just containing yrs.since.phd

