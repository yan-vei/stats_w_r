### Stats with R Exercise sheet 3

#################################################
#Tests for Categorical Data and cleaning data
#################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Saturday, November 25th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms


## Please write below your (and your teammates') name and matriculation number. 
## Name: Yana Veitsman
## Matriculation number: 7054842
## Name: Anthony Dsouza
## Matriculation number: 7053485
## Name: Tyler Lee
## Matriculation number: 7054832

## Only 1 member needs to submit! 

#################################################################################
##  Exercise 1: Cleaning data
#################################################################################

## download the file insomnia23.csv from cms
## The made-up dataset insomnia contains data of a survey on 63 students. 
## They are asked two questions: whether they regularly encounter sleep problems
## and what their preferred and most consumed drink is.

## a. Load the libraries stringr, dplyr, tidyr and forcats
library(stringr)
library(dplyr)
library(tidyr)
library(forcats)

## b. read in the data
insomnia_data <- read.csv('insomnia23.csv')

## c. get a summary of the dataset
summary(insomnia_data)

## d. the variable sleepProblem should be a numerical variable and have 0 for no Problem 
##    and 1 for sleep problems.
##    Make sure that this is the case
insomnia_data %>% filter(sleepProblem != 1 & sleepProblem != 0)

#### This is not the case because 11 and 'yes' are values in data$sleepProblem

insomnia_data <- insomnia_data %>% 
  mutate(sleepProblem = replace(sleepProblem, sleepProblem == 'yes' | sleepProblem == '11', 1))
insomnia_data %>% filter(sleepProblem != 1 & sleepProblem != 0)

#### Now, both 11 and 'yes' are replaced with 1, and there are not more invalid 
####values in insomnia_data$sleepProblem

## e. how many students encounter sleep problems?

insomnia_data %>% filter(sleepProblem != 1) %>% count()

#### 41 students encounter sleep problems

## f. how many different drinks do students name? (transform the variable into a 
## factor first)

insomnia_data$drink <- as.factor(insomnia_data$drink) 
length(unique(insomnia_data$drink))

#### students name seven different drinks

## g. collapse factor levels which were spelled wrong

spelling_corrections <- c('coffe' = 'coffee','cofee' = 'coffee','Koffee'='coffee',
                          'coffee'='coffee','tee' = 'tea','tea'='tea',
                          'water'='water')
insomnia_data <- insomnia_data %>% mutate(drink = spelling_corrections[match(insomnia_data$drink, names(spelling_corrections))]) 
      
## h. Next, check the variable gender. What is the problem here? Fix it.

####some entries are uppercase, some are lowercase
insomnia_data <- insomnia_data %>% 
  mutate(gender = tolower(gender))


## i. Next, check the variable subject. What is the problem here? Fix it.

#### Some entries are capitalized, others are not
insomnia_data <- insomnia_data %>%
  mutate(subject = str_trim(tolower(subject)))

## j. Assign your cleaned dataset to clean
clean <- insomnia_data

####################################################################
### Exercise 2: Chi-squared test
####################################################################

## consider the data set from above. If you had problems performing the
## required cleaning steps, note that you can also do them by hand
## Now consider we want to see whether the preferred drink influences sleep problems

## a. formulate in plain English what the Null hypothesis is in this context

#### The Null hypothesis is that the preferred drink does NOT influence sleep problems.

## Next we will go through the individual steps of a chisquare test, 
## without using the build-in function to practise (in the exam, you will not have
## access to r, but maybe asked to do some calculations by hand)

## b. tabulate the data using table()

insomnia_table <- table(clean$drink, clean$sleepProblem)

## c. calculate row (r1_t, r2_t..) and column (c1_t, c2_t..) totals as well as
##  the overall total by adding up the relevant numbers

row_sums <- rowSums(as.data.frame.matrix(insomnia_table))
row_sums
col_sums <- colSums(as.data.frame.matrix(insomnia_table))
col_sums
overall_total <- sum(row_sums)
overall_total

## d. calculate the expected value for each cell in the table

#### The following table yields the expected values for each cell:
expected_table <- as.matrix(row_sums) %*% t(as.matrix(col_sums)) / overall_total
expected_table

## e. What does "expected" mean in this context?

#### The "expected" value for any one cell is the mean value of the probability 
#### distribution that we will get if Null Hypothesis is true.

## f. calculate the test statistic using the observed and expected frequencies

observed <- as.vector(insomnia_table)
expected <- as.vector(expected_table)
X <- sum((observed - expected) ** 2 / expected)
X


## g. How many degrees of freedom? Why?

#### Two degrees of freedom because there are three rows and two columns

## h. get the critical value of chisquare using qchisq() and an alpha of 0.95

critical <- qchisq(0.95, 2)
critical

## i. what is the conclusion of the test? Why?

difference_from_expected <- abs(insomnia_table - expected_table) 
#calculate difference between observed and expected values for each cell
difference_from_expected > rep(critical, length(observed)) 
# calculate whether the difference exceeds the critical value for each cell

#### The observed values in the coffee row diverge from the expected values by a 
#### difference greater than the critical value. 
#### Therefore, we reject the Null Hypothesis that preferred drink does NOT 
#### affect sleep problems.

## j. now take the shortcut and use r's build in function chisq.test()
##    and assign the result to chitest

chitest <- chisq.test(insomnia_table)

## k. is the x-squared value the same as your calculated one? If not, please try 
## finding your mistake by comparing the expected frequencies you calculated with
## chitest$expected

chitest$statistic == X # is the value from chisq.test equal to our calculated value?

## l. which function do you have to use to find the p-value? Remember that the 
##  p-value indicates how likely it is to find a particular chisquare value or a 
##  more extreme one, given independence

#### You have to use the pchisq() function.
pchisq(X, 2)


####################################################################
### Exercise 3: Confounding variables
####################################################################

## In the above dataset, there were two other demographic variables: gender and subject.
## In this exercise, we want to find out whether they might be confounds for the
## relationship between drink and sleeping problems.

## a.  What is a confound? Why can it be problematic? Describe in your own words

#### Confounds are additional variables that affect both the independent and 
#### dependent variables.
#### It is problematic because you may be observing the effects of a confound 
#### rather than the effect of your independent variable upon your dependent 
#### variable.

## b. Please argue whether gender is a confound here. 
##    Hint: You have to run 2 chisquare tests to find out

#### Our Null Hypothesis here is that gender does not affect either drink 
#### preference or sleep problems.

drink_v_gender <- table(clean$gender, clean$drink)
X_drink_gender <- chisq.test(drink_v_gender)
X_drink_gender$p.value
sleepProblem_v_gender <- table(clean$gender, clean$sleepProblem)
X_sleep_gender <- chisq.test(sleepProblem_v_gender)
X_sleep_gender$p.value

#### Gender is a confound here because the p-value in both tests is less than 0.05.
#### In other words, we can reject the Null Hypothesis in both cases.

## c. Please argue whether subject is a confound here.


drink_v_subject <- table(clean$subject, clean$drink)
X_drink_subject <- chisq.test(drink_v_subject)
X_drink_subject$p.value
sleepProblem_v_subject <- table(clean$subject, clean$sleepProblem)
X_sleep_subject <- chisq.test(sleepProblem_v_subject)
X_sleep_subject$p.value

#### Subject is not while it affects drink preference, it does not affect 
#### sleep problems.
#### In plain English, even if subject affects drink preferences,
#### it does not have its own direct relationship to sleep problems.

#### The following was to check the first test manually.
row_sums <- rowSums(as.data.frame.matrix(drink_v_subject))
col_sums <- colSums(as.data.frame.matrix(drink_v_subject))
overall_sum <- sum(row_sums)
expected_table <- as.matrix(row_sums) %*% t(as.matrix(col_sums)) / overall_sum

observed <- as.vector(drink_v_subject)
expected <- as.vector(expected_table)
X_check <- sum((observed - expected)**2/expected)
X_check


## d. Given your results from b and c, do you have to change your interpretation of
##   the relationship between sleepProblems and preferred drink?

#### Yes, because you would have to account for the effect of gender on sleep 
#### problems in addition to the effect of preferred drink on sleep problems. 


#########################################
##Exercise 4. Binomial distribution
#########################################
##  In a board game, you have to roll a fair die. You will get a point, 
##  each time the number is higher than 3. You roll 20 times

## a) What is the chance in a single roll of earning a point?

#### A fair die has 6 sides labelled 1 through 6. Of these, three sides have a 
#### value greater than 3.
#### Since the probability of landing on any one side is 1/6, the probability 
#### of landing on a side with a value greater than 3 is 3/6 = 50%.
#### So the probability of earning a point is 50%

## b) Please calculate the probability of getting exactly 8 points.
##    Calculate this using the dbinom() function.

exactly_8_points <- dbinom(x = 8, size = 20, prob = 1/2)
print(paste("Probability of getting exactly 8 points is", exactly_8_points))

## c) Next please calculate the probability of getting less than 5 points
prob_less_than_5 <- sum(pbinom(4, size = 20, prob = 1/2))
print(paste("Probability of gettting less than 5 points :", prob_less_than_5))

## d) What is the difference between density function and distribution function?#

#### The density function (in this case dbinom) gives the probability of a 
#### certain event occurring, e.g. 8 successes out of 20.
#### The distribution function gives the cumultative probability of all events 
#### less than or equal an event occurring, e.g. 4 or less successes out of 20. 


#########################################
##Exercise 5
#########################################

##  In order to better understand the relationship between sleeping problems and 
##  consumed drinks, we set up a better controlled experiment: 
##  For two weeks, students are asked to drink mostly coffee and are then asked
##  whether they encountered sleep problems. For another two weeks, the same students
##  are asked to switch to tea and then again asked for sleeping problems.

## a) Can you use the ChiSquare test in this situation? Explain and motivate

#### No, because the observations are not independent. In a practical sense, you might 
#### be observing lingering effects of coffee consumption after the students switch to 
#### tea. 

# H0 : There is no association between type of drink and sleep problems
# H1 : There is significant association between the type of drink and sleep problems.

## b) Is there an alternative test you could use? Why would this be appropriate

#### You could use McNemar's test instead because this test is suited to 
#### non-independent observations.

