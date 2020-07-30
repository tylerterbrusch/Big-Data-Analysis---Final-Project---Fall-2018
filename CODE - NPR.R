# Tyler Terbrusch
# Final Project Code
# Researching NPR Donations Data
# Creating a Prediction Model

rm(list=ls())
set.seed(73)

#### Setting Working Directory
setwd("~/Desktop/UCONN GRADUATE SCHOOL/MSQE/Fall 2018/ECON 5495 - 003 - Topics in Economics Seminar - Big Data Analysis/Final Project")
dir()

#### Loading Data
npr<-read.csv("npr.csv", header=T)
dim(npr)
# NPR dataset has 3731 observations with 22 variables

head(npr)
tail(npr)

#### Description of Variables:

# id: Sequential Identification Number
# state: State
# msapop: MSA population of station (MSA = Metropolitan Statistical Area)
# age: age of listener
# listen: station listening time in quarter of an hour
# cont: total number of contributions
# city: City
# gother: contributions of others
# members: total number of members at station
# tlisten: total number of listeners in market
# popdens: total pop / land area of msa
# tprice: price with corrections
# donate: =1 if donate to public radio, 0 otherwise
# income: listener income in $1,000
# pgrad: =1 if masters or higher
# cgrad: =1 if bachelors
# scol: =1 if some college, no degree
# hsgrad: =1 if high school degree
# female: =1 if female
# white: =1 if white
# retired: =1 if retired
# gift1: contributions in dollars

str(npr)
# City is a Factor

#### Descriptive Statistics

cities<-c(table(npr$city))
length(cities)
# There are 51 cities represented in this dataset

states<-c(table(npr$state))
length(states)
# There are 34 states represented in this dataset

mean(npr$msapop, na.rm=T)
sd(npr$msapop, na.rm=T)
min(npr$msapop, na.rm=T)
max(npr$msapop, na.rm=T)

mean(npr$age, na.rm=T)
sd(npr$age, na.rm=T)
min(npr$age, na.rm=T)
max(npr$age, na.rm=T)

mean(npr$listen, na.rm=T)
sd(npr$listen, na.rm=T)
min(npr$listen, na.rm=T)
max(npr$listen, na.rm=T)
table(npr$listen)

mean(npr$cont, na.rm=T)
sd(npr$cont, na.rm=T)
min(npr$cont, na.rm=T)
max(npr$cont, na.rm=T)
table(npr$cont)
# Because non-responses (people who did not contribute) were originally coded as NA's, this mean and standard deviaton
# measures the average amount of conntributions among people who did donate at least once
# The mean and standard deviatiomn with NA's coded as 0's is below

mean(npr$gother, na.rm=T)
sd(npr$gother, na.rm=T)
min(npr$gother, na.rm=T)
max(npr$gother, na.rm=T)

mean(npr$members, na.rm=T)
sd(npr$members, na.rm=T)
min(npr$members, na.rm=T)
max(npr$members, na.rm=T)

mean(npr$tlisten, na.rm=T)
sd(npr$tlisten, na.rm=T)
min(npr$tlisten, na.rm=T)
max(npr$tlisten, na.rm=T)

mean(npr$popdens, na.rm=T)
sd(npr$popdens, na.rm=T)
min(npr$popdens, na.rm=T)
max(npr$popdens, na.rm=T)

mean(npr$tprice, na.rm=T)
sd(npr$tprice, na.rm=T)
min(npr$tprice, na.rm=T)
max(npr$tprice, na.rm=T)

mean(npr$donate, na.rm=T)
sd(npr$donate, na.rm=T)
min(npr$donate, na.rm=T)
max(npr$donate, na.rm=T)

mean(npr$income, na.rm=T)
sd(npr$income, na.rm=T)
min(npr$income, na.rm=T)
max(npr$income, na.rm=T)

a<-mean(npr$pgrad, na.rm=T)
a
sd(npr$pgrad, na.rm=T)
min(npr$pgrad, na.rm=T)
max(npr$pgrad, na.rm=T)

b<-mean(npr$cgrad, na.rm=T)
b
sd(npr$cgrad, na.rm=T)
min(npr$cgrad, na.rm=T)
max(npr$cgrad, na.rm=T)

c<-mean(npr$scol, na.rm=T)
c
sd(npr$scol, na.rm=T)
min(npr$scol, na.rm=T)
max(npr$scol, na.rm=T)

d<-mean(npr$hsgrad, na.rm=T)
d
sd(npr$hsgrad, na.rm=T)
min(npr$hsgrad, na.rm=T)
max(npr$hsgrad, na.rm=T)

nohs<-a+b+c+d
1-nohs
# 2.76% of dataset did not finish High School

mean(npr$female, na.rm=T)
sd(npr$female, na.rm=T)
min(npr$female, na.rm=T)
max(npr$female, na.rm=T)

mean(npr$white, na.rm=T)
sd(npr$white, na.rm=T)
min(npr$white, na.rm=T)
max(npr$white, na.rm=T)

mean(npr$retired, na.rm=T)
sd(npr$retired, na.rm=T)
min(npr$retired, na.rm=T)
max(npr$retired, na.rm=T)

mean(npr$gift1, na.rm=T)
sd(npr$gift1, na.rm=T)
min(npr$gift1, na.rm=T)
max(npr$gift1, na.rm=T)

# Note: finding the min's and max's for the indicator variables was a check to make sure there were no mistakes in the data

# Cleaning up Data:

# cont variable: This variable tracks how many contributions individuals have made, but if an individual has not donated, then this
# variable was given a NA value
# I am going to recode the NA's to zeros
# To make sure that everything is correctly coded:
mean(npr[is.na(npr$cont), "donate"])
sum(npr[is.na(npr$cont), "donate"])
# we see that when cont = NA, then donate = 0

npr$cont[is.na(npr$cont)]<-0
head(npr)
tail(npr)
# NA's have been recoded to 0's
# I wanted to recode these to 0's for the subset selection methods, I did not want observations with NA's to be removed from analysis
# If that happened, I'd be left with essentially a data set consisting only of individuals who donated

# Mean and Standard Deviation of Total Contributions variable after recoding
mean(npr$cont)
sd(npr$cont)
min(npr$cont)
max(npr$cont)

# I also want to transform the state variable from a numeric variable into a character variable
# These numbers for the states seem to be randomly assigned to denote which state is which (They are not the states in alphabetical order, I checked)
# By transforming the state variable into characters, these variables can now act like "state names" and can have predictive power as well
# as show if there are any state effects that increase individual's propensity to donate
npr$state<-as.character(npr$state)
head(npr)
str(npr)
table(npr$state)

##########

# Subset Selection

#### Best Subset Selection Method

# Checking if there are any NA values left in the data set:
sum(is.na(npr))
# There are 8 NA values left in the dataset

npr1 <- na.omit(npr)
dim(npr1) 
# 3723 observations, 22 variables are left after removing NA's
sum(is.na(npr1)) 
# 0 NA values left
# I want to take the ID variables out of the data set I am using for variable selection because this ID variable is an arbitrarily assigned
# number that denotes an individual observation; it is just an index of individuals 
npr1$id<-NULL
head(npr1)

# I also am going to take out the variables cont and gift1; these variables are perfectly related with donate, where if a person has not donated, then
# their values for cont(total contributions) will be 0 and value for gift1(contributions in dollars) will be 0, if they have donated then these variables will
# have a value greater than zero; this perfect correlation (essentially the donate variable in a different scale) will exclude them from consideration in subset selection

npr1$cont<-NULL
npr1$gift1<-NULL

head(npr1)

# Package with subset selection function:
library(leaps)
ncol(npr1)
# 19 variables left in the dataset, 18 will be considered for the model (the one variable not being considered is the dependent variable, donate)
# full.fit <- regsubsets(donate~.,data=npr1, nvmax=18, really.big=T) 
# Note, I needed to use the option: really.big=True
# Needed to use this option because data set is so large
# Note: This process is very slow computationally; after running for several hours, I decided to abort and will try a different approach
dim(npr1)
# The npr1 data set with 3723 rows and 19 variables was too large for the Best Subset Selection method, the computational cost
# was too high and was taking too long to process

# Will randomly sample 200 rows from npr1 data set and do best subset selection with this randomly sampled dataset
# My intuition is that with 200 observations, the best variables will be the same as with all of the observations
# best_selection_set<-sample(1:nrow(npr1), 200, replace=FALSE) 
# sampled_rows<-npr1[best_selection_set, ]
# dim(sampled_rows)

# Best Subset Selection on reduced size dataset (200 rows, 19 columns)
# full.fit <- regsubsets(donate~., data=npr1, nvmax=18) 

# Even though the size of the data set's rows has been reduced, still get the following error message:  

# Error in leaps.exhaustive(a, really.big) : 
#  Exhaustive search will be S L O W, must specify really.big=T
# In addition: Warning message:
#  In leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in,  :
#                   11  linear dependencies found

### The Best Subset Selection method was not successful with this data, the computational cost was too high
# 2^p models was computationally costly, what added to this computational cost was that R created indicator variables for each city and state
# Processing the best subset selection was too much, the p is 16 variables + 51 cities + 34 states; 2^101 is way too large

# Will remove the state and city variables from the data set so that reduce the dimension and can do Best Subset Selection method'
# Realized I should omit these variables after trying Forward Stepwise Selection with the city and state variables included in the data set

# Forward Stepwise Selection (First Attempt)

# dim(npr1)

# regfit.fwd <- regsubsets(donate~., data=npr1, nvmax=20, method="forward")
# reg.summary.fwd <- summary(regfit.fwd)
# reg.summary.fwd

# After reviewing output, decided to omit State and City variables from consideration for variable selection; for simplicity of model
# R created indicator variables for each individual State and City making this output too large

npr1$state<-NULL
npr1$city<-NULL

head(npr1)
dim(npr1)
# 17 variables in dataset, 16 will be considered for explanatory variables

#### Best Subset Selection method

full.fit <- regsubsets(donate~., data=npr1, nvmax=16) 
# This is much faster computationally, instantly is processed
# 2^16 models is much more easily processed
sum.full.fit<-summary(full.fit)
sum.full.fit
# In review of this output, can see several interesting outcomes: age does not play as large a role as I thought, variables in relation to population do not
# seem to be as relevant (I thought more populated areas like cities could have better content, listeners are more likely to donate)
# intuitively, the listen (amount of time spent listening to NPR) variable is very important
names(sum.full.fit)

# Note: the maximum adjr2 value gives best model; for Cp and BIC the smallest value gives the best model
sum.full.fit$adjr2
sum.full.fit$cp
sum.full.fit$bic

# To see what number of regressors have the best Adjusted R2, Cp, and BIC

# Adjusted R-squared
max.adjr2.full <- which.max(sum.full.fit$adjr2) 
max.adjr2.full
# 12 regressors gives the maximum adjusted R2
sum.full.fit
# These 12 regressors are: msapop, listen, members, tlisten, popdens, tprice, income, pgrad, cgrad, female,  white, retired
full.adjr2.vars<-c("msapop", "listen", "members", "tlisten", "popdens", "tprice", "income", "pgrad", "cgrad", "female", "white", "retired")

# Bayes Information Criterion (BIC)
min.bic.full <- which.min(sum.full.fit$bic)
min.bic.full
# 9 regressors gives us the minimum BIC
sum.full.fit
# These 9 regressors are: listen, members, tlisten, tprice, income, pgrad, cgrad, female, retired
full.bic.vars<-c("listen", "members","tlisten", "tprice", "income", "pgrad", "cgrad", "female", "retired")

# Cp
min.cp.full<-which.min(sum.full.fit$cp)
min.cp.full
# 10 regressors give us the minimum Cp
sum.full.fit
# These 10 regressors are: listen, members, tlisten, tprice, income, pgrad, cgrad, female, white, retired 
full.cp.vars<-c("listen", "members", "tlisten", "tprice", "income", "pgrad", "cgrad", "female", "white", "retired")

#### Forward Stepwise Selection (Second Attempt)

regfit.fwd <- regsubsets(donate~., data=npr1, nvmax=16, method="forward")
reg.summary.fwd <- summary(regfit.fwd)
reg.summary.fwd

names(reg.summary.fwd)
reg.summary.fwd$adjr2
reg.summary.fwd$cp
reg.summary.fwd$bic

# To see what number of regressors have the best Adjusted R2, Cp, and BIC

# Adjusted R-squared
max.adjr2.fwd <- which.max(reg.summary.fwd$adjr2) 
max.adjr2.fwd
# 12 regressors gives the maximum adjusted R2
reg.summary.fwd
# These 12 regressors are: msapop, listen, members, tlisten, popdens, tprice, income, pgrad, cgrad, female,  white, retired
f.adjr2.vars<-c("msapop", "listen", "members", "tlisten", "popdens", "tprice", "income", "pgrad", "cgrad", "female", "white", "retired")

# Bayes Information Criterion (BIC)
min.bic.fwd <- which.min(reg.summary.fwd$bic)
min.bic.fwd
# 9 regressors gives us the minimum BIC
reg.summary.fwd
# These 9 regressors are: listen, members, tlisten, tprice, income, pgrad, cgrad, female, retired
f.bic.vars<-c("listen", "members","tlisten", "tprice", "income", "pgrad", "cgrad", "female", "retired")

# Cp
min.cp.fwd<-which.min(reg.summary.fwd$cp)
min.cp.fwd
# 10 regressors give us the minimum Cp
reg.summary.fwd
# These 10 regressors are: listen, members, tlisten, tprice, income, pgrad, cgrad, female, white, retired 
f.cp.vars<-c("listen", "members", "tlisten", "tprice", "income", "pgrad", "cgrad", "female", "white", "retired")

##### Backward Stepwise Selection
regfit.bwd <- regsubsets(donate~., data=npr1, nvmax=16, method="backward")
reg.summary.bwd <- summary(regfit.bwd)
reg.summary.bwd

reg.summary.bwd$adjr2
reg.summary.bwd$cp
reg.summary.bwd$bic

# To see what number of regressors have the best Adjusted R2, Cp, and BIC

# Adjusted R-squared
max.adjr2.bwd <- which.max(reg.summary.bwd$adjr2) 
max.adjr2.bwd
# 12 regressors gives the maximum adjusted R2
reg.summary.bwd
# These 12 regressors are:  msapop, listen, members, tlisten, popdens, tprice, income, pgrad, cgrad, female,  white, retired
b.adjr2.vars<-c("msapop", "listen", "members", "tlisten", "popdens", "tprice", "income", "pgrad", "cgrad", "female", "white", "retired")

# Bayes Information Criterion (BIC)
min.bic.bwd <- which.min(reg.summary.bwd$bic)
min.bic.bwd
# 9 regressors gives us the minimum BIC 
reg.summary.bwd
# These 9 regressors are: listen, members, tlisten, tprice, income, pgrad, cgrad, female, retired
b.bic.vars<-c("listen", "members","tlisten", "tprice", "income", "pgrad", "cgrad", "female", "retired")

# Cp
min.cp.bwd<-which.min(reg.summary.bwd$cp)
min.cp.bwd
# 10 regressors give us the minimum Cp
reg.summary.bwd
# These 10 regressors are: listen, members, tlisten, tprice, income, pgrad, cgrad, female, white, retired 
b.cp.vars<-c("listen", "members", "tlisten", "tprice", "income", "pgrad", "cgrad", "female", "white", "retired")

# The Best Subset Selection and Forward and Backward stepwise subset selection method produce the same sets of variables for each statistic (adjr2, Cp, BIC)
# There are 3 different models (3 different sets of regressors) to consider and see which set of variables is the best at predicting whether
# an individual will donate to NPR or not

##########

# Testing Accuracy of Models

# Splitting Dataset into Training Dataset, Test Dataset, and Double up Dataset
set.seed(73)
random <- sample(1:nrow(npr1), 2500, replace=FALSE) 
train<- npr1[random,]
remaining<-npr1[-random, ]
random2<-sample(1:nrow(remaining), (ceiling(nrow(remaining)/2)), replace=FALSE)
test<-remaining[-random2, ]
doubleup<-remaining[random2, ]


# 3 Different Sets of Variables to Test on 4 Different Methods (LPM, Logit, LDA, KNN)
# Will use npr1 dataset

# Notation: 
# Adjusted R-squared = 1
# BIC = 2
# Cp = 3

full.adjr2.vars

# Adjusted R2 recommended model (12 regressors): msapop, listen, members, tlisten, popdens, tprice, income, pgrad, cgrad, female,  white, retired

#### Linear Probability Model
lpm1<-lm(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, data=train)
summary(lpm1)

# Assessing Accuracy of Linear Probability Model
lpm.prob1<-predict(lpm1, test, type="response") 
lpm.pred1<-rep(0, nrow(test)) 
lpm.pred1[lpm.prob1>0.5] <- 1 
table(lpm.pred1) 
table(test$donate)
table(lpm.pred1, test$donate) 
lpm.accuracy1.1<- 100*(mean(lpm.pred1 == test$donate))
lpm.accuracy1.1

# Double Up:
lpm.prob1.2<-predict(lpm1, doubleup, type="response")
lpm.pred1.2<-rep(0, nrow(doubleup))
lpm.pred1.2[lpm.prob1.2>0.5]<-1
table(lpm.pred1.2)
table(doubleup$donate)
table(lpm.pred1.2, doubleup$donate)
lpm.accuracy1.2<-100*(mean(lpm.pred1.2==doubleup$donate))
lpm.accuracy1.2

#### Logistic Regression Model
glm1<-glm(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, family=binomial, data=train)
summary(glm1)

# Assessing Accuracy of GLM Model
glm1<-glm(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, family=binomial, maxit=100, data=train)
glm.prob1<-predict(glm1, test, type="response") 
glm.pred1<-rep(0, nrow(test)) 
glm.pred1[glm.prob1>0.5] <- 1 
table(glm.pred1) 
table(test$donate)
table(glm.pred1, test$donate) 
glm.accuracy1.1<- 100*(mean(glm.pred1 == test$donate))
glm.accuracy1.1

# Double Up:
glm.prob1.2<-predict(glm1, doubleup, type="response")
glm.pred1.2<-rep(0, nrow(doubleup))
glm.pred1.2[glm.prob1.2>0.5]<-1
table(glm.pred1.2)
table(doubleup$donate)
table(glm.pred1.2, doubleup$donate)
glm.accuracy1.2<-100*(mean(glm.pred1.2==doubleup$donate))
glm.accuracy1.2

# Linear Discriminant Analysis
library(MASS)

lda.fit1 <- lda(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, data=train)

# Assessing Accuracy of LDA Model
lda.fit1<-lda(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, data=train)
lda.pred1 <- predict(lda.fit1, test)
lda.class1 <- lda.pred1$class
table(lda.class1)
table(lda.class1, test$donate)
lda.accuracy1.1<-100*(mean(lda.class1==test$donate))
lda.accuracy1.1

# Double Up
lda.pred1.2<-predict(lda.fit1, doubleup)
lda.class1.2<- lda.pred1.2$class
table(lda.class1.2)
table(lda.class1.2, doubleup$donate)
lda.accuracy1.2<-100*(mean(lda.class1.2==doubleup$donate))
lda.accuracy1.2

# Quadratic Discriminant Analysis
qda.fit <- qda(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, data=train)
# Unfortunately, this QDA does not work with this dataset due to the amount of parameters that
# would need to be estimated and the lack of observations that did donate

# K-Nearest Neighbor For Classification

# 10 Fold K-Fold Cross-Validation to Find Optimal Number of Neighbors:
library(class)
set.seed(73)
zz1 <- rep(NA,100)
cv.error1 <- matrix(zz1,10,10)

npr1.1.temp <- npr1[ ,c("msapop", "listen", "members", "tlisten", "popdens", "tprice", "income", "pgrad", "cgrad", "female", "white", "retired", "donate")]
n <- nrow(npr1.1.temp)
ni <- n/10

for (j in 1:10){
  for(i in 1:10){
    test.index<-(ni*(i-1)+1):(ni*i)
    test.data<-npr1.1.temp[test.index, ]
    train.data<-npr1.1.temp[-test.index, ]
    yhat<-knn(train.data[ ,1:12], test.data[ ,1:12], train.data[ ,"donate"], k=j*5)
    cv.error1[i,j]<-mean(yhat!=test.data[,"donate"])
  }
}
colMeans(cv.error1)
# 0.3352151 0.3215054 0.3193548 0.3174731 0.3209677 0.3241935 0.3260753 0.3319892 0.3357527 0.3338710
which.min(colMeans(cv.error1))
# The smallest amount of error is with 20 neighbors

# KNN Estimation
head(npr1.1.temp)
# Donate is in the 13th column

# Need to standardize variables when do KNN for Classification
set.seed(73)
random <- sample(1:nrow(npr1), 2500, replace=FALSE) 
train.knn.1<-scale(npr1.1.temp[random,-13])
remaining.knn.1<-npr1.1.temp[-random, ]
random2<-sample(1:nrow(remaining), (ceiling(nrow(remaining)/2)), replace=FALSE)
test.knn.1<-scale(remaining.knn.1[-random2, -13])
doubleup.knn.1<-scale(remaining.knn.1[random2, -13])

# Assessing Accuracy of KNN Model
train1.X <- train.knn.1
test1.X <- test.knn.1
train1.Y <- npr1.1.temp[random, "donate"]
test1.Y <- remaining.knn.1[-random2, "donate"]
knn.pred.1 <- knn(train1.X, test1.X, train1.Y, k=20) # 20 neighbors 
mean(test1.Y != knn.pred.1)  # Test error rate
mean(test1.Y == 1) 
table(knn.pred.1, test1.Y) 
knn.accuracy1.1<-100*(mean(knn.pred.1==test1.Y))
knn.accuracy1.1

# Double Up
test1.2.X <- doubleup.knn.1
test1.2.Y <- remaining.knn.1[random2, 13]
knn.pred.1.2 <- knn(train1.X, test1.2.X, train1.Y, k=20) # 20 neighbors 
mean(test1.2.Y != knn.pred.1.2)  # Test error rate
mean(test1.2.Y == 1) 
table(knn.pred.1.2, test1.2.Y) 
knn.accuracy1.2<-100*(mean(knn.pred.1.2==test1.2.Y))
knn.accuracy1.2

########################################################################

full.bic.vars

# BIC recommended model (9 regressors): listen, members, tlisten, tprice, income, pgrad, cgrad, female, retired 

#### Linear Probability Model
lpm2<-lm(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+retired, data=train)
summary(lpm2)

# Assessing Accuracy of LPM Model
lpm.prob2<-predict(lpm2, test, type="response") 
lpm.pred2<-rep(0, nrow(test)) 
lpm.pred2[lpm.prob2>0.5] <- 1 
table(lpm.pred2) 
table(test$donate)
table(lpm.pred2, test$donate) 
lpm.accuracy2.1<- 100*(mean(lpm.pred2 == test$donate))
lpm.accuracy2.1

# Double Up:
lpm.prob2.2<-predict(lpm2, doubleup, type="response")
lpm.pred2.2<-rep(0, nrow(doubleup))
lpm.pred2.2[lpm.prob2.2>0.5]<-1
table(lpm.pred2.2)
table(doubleup$donate)
table(lpm.pred2.2, doubleup$donate)
lpm.accuracy2.2<-100*(mean(lpm.pred2.2==doubleup$donate))
lpm.accuracy2.2

#### Logistic Regression Model
glm2<-glm(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+retired, family=binomial, data=train)
summary(glm2)

# Assessing Accuracy of GLM Model
glm2<-glm(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+retired, family=binomial, data=train)
glm.prob2<-predict(glm2, test, type="response") 
glm.pred2<-rep(0, nrow(test)) 
glm.pred2[glm.prob2>0.5] <- 1 
table(glm.pred2) 
table(test$donate)
table(glm.pred2, test$donate) 
glm.accuracy2.1<- 100*(mean(glm.pred2 == test$donate))
glm.accuracy2.1

# Double Up:
glm.prob2.2<-predict(glm2, doubleup, type="response")
glm.pred2.2<-rep(0, nrow(doubleup))
glm.pred2.2[glm.prob2.2>0.5]<-1
table(glm.pred2.2)
table(doubleup$donate)
table(glm.pred2.2, doubleup$donate)
glm.accuracy2.2<-100*(mean(glm.pred2.2==doubleup$donate))
glm.accuracy2.2

# Linear Discriminant Analysis

lda.fit2<-lda(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+retired, data=train)

# Assessing Accuracy of LDA Model
lda.fit2<-lda(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+retired, data=train)
lda.pred2<-predict(lda.fit2, test)
lda.class2 <- lda.pred2$class
table(lda.class2)
table(lda.class2, test$donate)
lda.accuracy2.1<-100*(mean(lda.class2==test$donate))
lda.accuracy2.1

# Double Up
lda.pred2.2<-predict(lda.fit2, doubleup)
lda.class2.2<- lda.pred2.2$class
table(lda.class2.2)
table(lda.class2.2, doubleup$donate)
lda.accuracy2.2<-100*(mean(lda.class2.2==doubleup$donate))
lda.accuracy2.2

# K-Nearest Neighbor For Classification

# 10 Fold K-Fold Cross-Validation to Find Optimal Number of Neighbors:
set.seed(73)
zz2 <- rep(NA,100)
cv.error2 <- matrix(zz2,10,10)

npr2.1.temp <- npr1[ ,c("listen", "members", "tlisten", "tprice", "income", "pgrad", "cgrad", "female", "retired", "donate")]
n <- nrow(npr2.1.temp)
ni <- n/10

for (j in 1:10){
  for(i in 1:10){
    test.index<-(ni*(i-1)+1):(ni*i)
    test.data<-npr2.1.temp[test.index, ]
    train.data<-npr2.1.temp[-test.index, ]
    yhat<-knn(train.data[ ,1:9], test.data[ ,1:9], train.data[ ,"donate"], k=j*5)
    cv.error2[i,j]<-mean(yhat!=test.data[,"donate"])
  }
}
colMeans(cv.error2)
#  0.3465054 0.3411290 0.3231183 0.3274194 0.3260753 0.3268817 0.3287634 0.3357527 0.3381720 0.3392473
which.min(colMeans(cv.error2))
# The smallest amount of error is with 15 neighbors

# KNN Estimation

head(npr2.1.temp)
# Donate is in the 10th column

# Need to standardize variables when do KNN for Classification
set.seed(73)
random <- sample(1:nrow(npr1), 2500, replace=FALSE) 
train.knn.2<-scale(npr2.1.temp[random,-10])
remaining.knn.2<-npr2.1.temp[-random, ]
random2<-sample(1:nrow(remaining), (ceiling(nrow(remaining)/2)), replace=FALSE)
test.knn.2<-scale(remaining.knn.2[-random2, -10])
doubleup.knn.2<-scale(remaining.knn.2[random2, -10])

# Assessing Accuracy of KNN Model
train2.X <- train.knn.2
test2.X <- test.knn.2
train2.Y <- npr2.1.temp[random, "donate"]
test2.Y <- remaining.knn.2[-random2, "donate"]
knn.pred.2 <- knn(train2.X, test2.X, train2.Y, k=15) # 15 neighbors 
mean(test2.Y != knn.pred.2)  # Test error rate
mean(test2.Y == 1) 
table(knn.pred.2, test2.Y) 
knn.accuracy2.1<-100*(mean(knn.pred.2==test2.Y))
knn.accuracy2.1

# Double Up
test2.2.X <- doubleup.knn.2
test2.2.Y <- remaining.knn.2[random2, 10]
knn.pred.2.2 <- knn(train2.X, test2.2.X, train2.Y, k=15) # 15 neighbors 
mean(test2.2.Y != knn.pred.2.2)  # Test error rate
mean(test2.2.Y == 1) 
table(knn.pred.2.2, test2.2.Y) 
knn.accuracy2.2<-100*(mean(knn.pred.2.2==test2.2.Y))
knn.accuracy2.2

########################################################################

full.cp.vars

# Cp recommended model (10 regressors): listen, members, tlisten, tprice, income, pgrad, cgrad, female, white, retired

#### Linear Probability Model
lpm3<-lm(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+white+retired, data=train)
summary(lpm3)

# Assessing Accuracy of LPM Model
lpm.prob3<-predict(lpm3, test, type="response") 
lpm.pred3<-rep(0, nrow(test)) 
lpm.pred3[lpm.prob3>0.5] <- 1 
table(lpm.pred3) 
table(test$donate)
table(lpm.pred3, test$donate) 
lpm.accuracy3.1<- 100*(mean(lpm.pred3 == test$donate))
lpm.accuracy3.1

# Double Up:
lpm.prob3.2<-predict(lpm3, doubleup, type="response")
lpm.pred3.2<-rep(0, nrow(doubleup))
lpm.pred3.2[lpm.prob3.2>0.5]<-1
table(lpm.pred3.2)
table(doubleup$donate)
table(lpm.pred3.2, doubleup$donate)
lpm.accuracy3.2<-100*(mean(lpm.pred3.2==doubleup$donate))
lpm.accuracy3.2

#### Logistic Regression Model
glm3<-glm(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+white+retired, family=binomial, data=train)
summary(glm3)

# Assessing Accuracy of GLM Model
glm3<-glm(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+white+retired, family=binomial, data=train)
glm.prob3<-predict(glm3, test, type="response") 
glm.pred3<-rep(0, nrow(test)) 
glm.pred3[glm.prob3>0.5] <- 1 
table(glm.pred3) 
table(test$donate)
table(glm.pred3, test$donate) 
glm.accuracy3.1<- 100*(mean(glm.pred3 == test$donate))
glm.accuracy3.1

# Double Up:
glm.prob3.2<-predict(glm3, doubleup, type="response")
glm.pred3.2<-rep(0, nrow(doubleup))
glm.pred3.2[glm.prob3.2>0.5]<-1
table(glm.pred3.2)
table(doubleup$donate)
table(glm.pred3.2, doubleup$donate)
glm.accuracy3.2<-100*(mean(glm.pred3.2==doubleup$donate))
glm.accuracy3.2

# Linear Discriminant Analysis

lda.fit3<-lda(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+white+retired, data=train)

# Assessing Accuracy of LDA Model
lda.fit3<-lda(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+white+retired, data=train)
lda.pred3<-predict(lda.fit3, test)
lda.class3 <- lda.pred3$class
table(lda.class3)
table(lda.class3, test$donate)
lda.accuracy3.1<-100*(mean(lda.class3==test$donate))
lda.accuracy3.1

# Double Up
lda.pred3.2<-predict(lda.fit3, doubleup)
lda.class3.2<- lda.pred3.2$class
table(lda.class3.2)
table(lda.class3.2, doubleup$donate)
lda.accuracy3.2<-100*(mean(lda.class3.2==doubleup$donate))
lda.accuracy3.2

# K-Nearest Neighbor For Classification

# 10 Fold K-Fold Cross-Validation to Find Optimal Number of Neighbors:
set.seed(73)
zz3 <- rep(NA,100)
cv.error3 <- matrix(zz3,10,10)

npr3.1.temp <- npr1[,c("listen", "members", "tlisten", "tprice", "income", "pgrad", "cgrad", "female", "white", "retired", "donate")]
n <- nrow(npr3.1.temp)
ni <- n/10

for (j in 1:10){
  for(i in 1:10){
    test.index<-(ni*(i-1)+1):(ni*i)
    test.data<-npr3.1.temp[test.index, ]
    train.data<-npr3.1.temp[-test.index, ]
    yhat<-knn(train.data[ ,1:10], test.data[ ,1:10], train.data[ ,"donate"], k=j*5)
    cv.error3[i,j]<-mean(yhat!=test.data[ ,"donate"])
  }
}
colMeans(cv.error3)
# 0.3465054 0.3379032 0.3231183 0.3263441 0.3271505 0.3274194 0.3287634 0.3357527 0.3381720 0.3392473
which.min(colMeans(cv.error3))
# The smallest amount of error is with 15 neighbors

# KNN Estimation

head(npr3.1.temp)
# donate is in 11th column

# Need to standardize variables when do KNN for Classification
set.seed(73)
random <- sample(1:nrow(npr1), 2500, replace=FALSE) 
train.knn.3<-scale(npr3.1.temp[random,-11])
remaining.knn.3<-npr3.1.temp[-random, ]
random2<-sample(1:nrow(remaining), (ceiling(nrow(remaining)/2)), replace=FALSE)
test.knn.3<-scale(remaining.knn.3[-random2, -11])
doubleup.knn.3<-scale(remaining.knn.3[random2, -11])

# Assessing Accuracy of KNN Model
train3.X <- train.knn.3
test3.X <- test.knn.3
train3.Y <- npr3.1.temp[random, "donate"]
test3.Y <- remaining.knn.3[-random2, "donate"]
knn.pred.3 <- knn(train3.X, test3.X, train3.Y, k=15) # 15 neighbors 
mean(test3.Y != knn.pred.3)  # Test error rate
mean(test3.Y == 1) 
table(knn.pred.3, test3.Y) 
knn.accuracy3.1<-100*(mean(knn.pred.3==test3.Y))
knn.accuracy3.1

# Double Up
test3.2.X <- doubleup.knn.3
test3.2.Y <- remaining.knn.3[random2, 11]
knn.pred.3.2 <- knn(train3.X, test3.2.X, train3.Y, k=15) # 15 neighbors 
mean(test3.2.Y != knn.pred.3.2)  # Test error rate
mean(test3.2.Y == 1) 
table(knn.pred.3.2, test3.2.Y) 
knn.accuracy3.2<-100*(mean(knn.pred.3.2==test3.2.Y))
knn.accuracy3.2

########################################################################

lpm.accuracy<-data.frame(lpm.accuracy1.1, lpm.accuracy1.2, lpm.accuracy2.1, lpm.accuracy2.2, lpm.accuracy3.1, lpm.accuracy3.2)
glm.accuracy<-data.frame(glm.accuracy1.1, glm.accuracy1.2, glm.accuracy2.1, glm.accuracy2.2, glm.accuracy3.1, glm.accuracy3.2)
lda.accuracy<-data.frame(lda.accuracy1.1, lda.accuracy1.2, lda.accuracy2.1, lda.accuracy2.2, lda.accuracy3.1, lda.accuracy3.2)
knn.accuracy<-data.frame(knn.accuracy1.1, knn.accuracy1.2, knn.accuracy2.1, knn.accuracy2.2, knn.accuracy3.1, knn.accuracy3.2)

# Average the test and double up accuracies
# Linear Probability Model
lpm1.accuracy<-(lpm.accuracy1.1+lpm.accuracy1.2)/2
lpm2.accuracy<-(lpm.accuracy2.1+lpm.accuracy2.2)/2
lpm3.accuracy<-(lpm.accuracy3.1+lpm.accuracy3.2)/2

# Logit
glm1.accuracy<-(glm.accuracy1.1+glm.accuracy1.2)/2
glm2.accuracy<-(glm.accuracy2.1+glm.accuracy2.2)/2
glm3.accuracy<-(glm.accuracy3.1+glm.accuracy3.2)/2

# Linear Discriminant Analysis
lda1.accuracy<-(lda.accuracy1.1+lda.accuracy1.2)/2
lda2.accuracy<-(lda.accuracy2.1+lda.accuracy2.2)/2
lda3.accuracy<-(lda.accuracy3.1+lda.accuracy3.2)/2

# K Nearest Neighbor
knn1.accuracy<-(knn.accuracy1.1+knn.accuracy1.2)/2
knn2.accuracy<-(knn.accuracy2.1+knn.accuracy2.2)/2
knn3.accuracy<-(knn.accuracy3.1+knn.accuracy3.2)/2

accuracy<-c(lpm1.accuracy, lpm2.accuracy, lpm3.accuracy, glm1.accuracy, glm2.accuracy, glm3.accuracy, lda1.accuracy, lda2.accuracy, lda3.accuracy, knn1.accuracy, knn2.accuracy, knn3.accuracy)
which.max(accuracy)
# 5: glm2.accuracy: logit model from BIC

# Find the smallest difference between the Test Data accuracy and Double Up Data accuracy to see which method is most consistent
# Linear Probability Model
lpm1.accuracy_d<-abs(lpm.accuracy1.1-lpm.accuracy1.2)
lpm2.accuracy_d<-abs(lpm.accuracy2.1-lpm.accuracy2.2)
lpm3.accuracy_d<-abs(lpm.accuracy3.1-lpm.accuracy3.2)

# Logit
glm1.accuracy_d<-abs(glm.accuracy1.1-glm.accuracy1.2)
glm2.accuracy_d<-abs(glm.accuracy2.1-glm.accuracy2.2)
glm3.accuracy_d<-abs(glm.accuracy3.1-glm.accuracy3.2)

# Linear Discriminant Analysis
lda1.accuracy_d<-abs(lda.accuracy1.1-lda.accuracy1.2)
lda2.accuracy_d<-abs(lda.accuracy2.1-lda.accuracy2.2)
lda3.accuracy_d<-abs(lda.accuracy3.1-lda.accuracy3.2)

# K Nearest Neighbor
knn1.accuracy_d<-abs(knn.accuracy1.1-knn.accuracy1.2)
knn2.accuracy_d<-abs(knn.accuracy2.1-knn.accuracy2.2)
knn3.accuracy_d<-abs(knn.accuracy3.1-knn.accuracy3.2)

consistency<-c(lpm1.accuracy_d, lpm2.accuracy_d, lpm3.accuracy_d, glm1.accuracy_d, glm2.accuracy_d, glm3.accuracy_d, lda1.accuracy_d, lda2.accuracy_d, lda3.accuracy_d, knn1.accuracy_d, knn2.accuracy_d, knn3.accuracy_d)
which.min(consistency)
which.max(consistency)

# Assigning scores to optimize between accuracy and consistency
con<-order(consistency)
acc<-order(accuracy, decreasing = T)
ranks<-data.frame(cbind(con, acc))
class(ranks)
ranks
model.number<-seq(1:12)
scores<-data.frame(model.number)
scores$performance.score<-NA
# If a model was the 5th most accurate and the 7th most consistent, then this model's performance score = 12
# Want to find the model with the lowest number (meaning it was closest to being the most accurate and consistent)
scores[1,"performance.score"]<- 12
scores[2,"performance.score"]<- 12
scores[3,"performance.score"]<- 13
scores[4,"performance.score"]<- 10
scores[5,"performance.score"]<- 13
scores[6,"performance.score"]<- 13
scores[7,"performance.score"]<- 11
scores[8,"performance.score"]<- 16
scores[9,"performance.score"]<- 15
scores[10,"performance.score"]<- 13
scores[11,"performance.score"]<- 12
scores[12,"performance.score"]<- 16

scores
# Manually put in the accuracy, consistency scores, and added them for each model to create each model's performance score
# to create table below

#  model.number           acc con performance.score
#            1             8   4                12
#            2             4   8                12
#            3             6   7                13
#            4             7   3                10
#            5             1   12               13
#            6             3   10               13
#            7             2    9               11
#            8             5   11               16
#            9             9    6               15
#           10            12    1               13
#           11            10    2               12
#           12            11    5               16
 
# Model 4 has the best performance score of 10
# The model with the highest average accuracy(70.8114) and overall accuracy(73.15876) is Model 5 (performance score of 13)

  
# Most accurate prediction model: glm2: Logit Model with variables selected by lowest BIC
# The highest overall accuracy (with the test data: 73.16%, but lowest consistency)

# With Full Data set:
glm.best<-glm(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+retired, family=binomial, data=npr)
summary(glm.best)

# Testing Accuracy on Test Set of 1200 observations (Test set earlier was 600 observations) and different Seed Number:
set.seed(3)
random<-sample(1:nrow(npr), 2500, replace=F)
train1<-npr[random,]
test1<-npr[-random,]
train1<-na.omit(train1)
test1<-na.omit(test1)

glm.best<-glm(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+retired, family=binomial, data=train1)
glm.prob.best<-predict(glm.best, test1, type="response") 
glm.pred.best<-rep(0, nrow(test1)) 
glm.pred.best[glm.prob.best>0.5] <- 1 
table(glm.pred.best) 
table(test1$donate)
table(glm.pred.best, test1$donate) 
glm.accuracy.best<- 100*(mean(glm.pred.best == test1$donate))
glm.accuracy.best

# This model is around 72.44% accurate

#################


## For Estimation Results section:

# With Full Data set:
glm.full<-glm(donate~listen+members+tlisten+tprice+income+pgrad+cgrad+female+retired, family=binomial, data=npr)
summary(glm.full)

# Marginal Effects
library(margins)
marginal.effects<-margins(glm.full)
summary(marginal.effects, type="response")

#################

# Residual Bootstrap to Estimate 95% confidence intervals for z-statistics of GLM Model (Not Marginal Effects)

# Intercept
set.seed(73)
z.intercept<-summary(glm.full)$coefficients[1,3]

zstar.intercept <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.intercept[r] <- (glm.full.star$coefficients[1]-glm.full$coefficients[1])/summary(glm.full.star)$coefficients[1,2]
}

zstar.ordered <- sort(zstar.intercept, decreasing=FALSE)
cv.star.intercept <- c(zstar.ordered[25], zstar.ordered[975])
z.intercept
cv.star.intercept 



# Listen
set.seed(73)
z.listen<-summary(glm.full)$coefficients[2,3]

zstar.listen <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.listen[r] <- (glm.full.star$coefficients[2]-glm.full$coefficients[2])/summary(glm.full.star)$coefficients[2,2]
}

zstar.ordered <- sort(zstar.listen, decreasing=FALSE)
cv.star.listen <- c(zstar.ordered[25], zstar.ordered[975])
z.listen
cv.star.listen 

# members
set.seed(73)
z.members<-summary(glm.full)$coefficients[3,3]

zstar.members <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.members[r] <- (glm.full.star$coefficients[3]-glm.full$coefficients[3])/summary(glm.full.star)$coefficients[3,2]
}

zstar.ordered <- sort(zstar.members, decreasing=FALSE)
cv.star.members <- c(zstar.ordered[25], zstar.ordered[975])
z.members
cv.star.members 

# tlisten
set.seed(73)
z.tlisten<-summary(glm.full)$coefficients[4,3]

zstar.tlisten <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.tlisten[r] <- (glm.full.star$coefficients[4]-glm.full$coefficients[4])/summary(glm.full.star)$coefficients[4,2]
}

zstar.ordered <- sort(zstar.tlisten, decreasing=FALSE)
cv.star.tlisten <- c(zstar.ordered[25], zstar.ordered[975])
z.tlisten
cv.star.tlisten 

# tprice
set.seed(73)
z.tprice<-summary(glm.full)$coefficients[5,3]

zstar.tprice <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.tprice[r] <- (glm.full.star$coefficients[5]-glm.full$coefficients[5])/summary(glm.full.star)$coefficients[5,2]
}

zstar.ordered <- sort(zstar.tprice, decreasing=FALSE)
cv.star.tprice <- c(zstar.ordered[25], zstar.ordered[975])
z.tprice
cv.star.tprice 

# income
set.seed(73)
z.income<-summary(glm.full)$coefficients[6,3]

zstar.income <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.income[r] <- (glm.full.star$coefficients[6]-glm.full$coefficients[6])/summary(glm.full.star)$coefficients[6,2]
}

zstar.ordered <- sort(zstar.income, decreasing=FALSE)
cv.star.income <- c(zstar.ordered[25], zstar.ordered[975])
z.income
cv.star.income

# pgrad
set.seed(73)
z.pgrad<-summary(glm.full)$coefficients[7,3]

zstar.pgrad <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.pgrad[r] <- (glm.full.star$coefficients[7]-glm.full$coefficients[7])/summary(glm.full.star)$coefficients[7,2]
}

zstar.ordered <- sort(zstar.pgrad, decreasing=FALSE)
cv.star.pgrad <- c(zstar.ordered[25], zstar.ordered[975])
z.pgrad
cv.star.pgrad 

# cgrad
set.seed(73)
z.cgrad<-summary(glm.full)$coefficients[8,3]

zstar.cgrad <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.cgrad[r] <- (glm.full.star$coefficients[8]-glm.full$coefficients[8])/summary(glm.full.star)$coefficients[8,2]
}

zstar.ordered <- sort(zstar.cgrad, decreasing=FALSE)
cv.star.cgrad <- c(zstar.ordered[25], zstar.ordered[975])
z.cgrad
cv.star.cgrad 

# female
set.seed(73)
z.female<-summary(glm.full)$coefficients[9,3]

zstar.female <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.female[r] <- (glm.full.star$coefficients[9]-glm.full$coefficients[9])/summary(glm.full.star)$coefficients[9,2]
}

zstar.ordered <- sort(zstar.female, decreasing=FALSE)
cv.star.female <- c(zstar.ordered[25], zstar.ordered[975])
z.female
cv.star.female 

# retired
set.seed(73)
z.retired<-summary(glm.full)$coefficients[10,3]

zstar.retired <- NA

for (r in 1:1000){
  ustar <- sample(glm.full$residuals, nrow(npr), replace=TRUE)
  ystar <- glm.full$coefficients[1] + glm.full$coefficients[2]*npr$listen + glm.full$coefficients[3]*npr$members + glm.full$coefficients[4]*npr$tlisten + glm.full$coefficients[5]*npr$tprice + glm.full$coefficients[6]*npr$income + glm.full$coefficients[7]*npr$pgrad + glm.full$coefficients[8]*npr$cgrad + glm.full$coefficients[9]*npr$female +  glm.full$coefficients[10]*npr$retired + ustar
  glm.full.star <- glm(ystar~npr$listen + npr$members + npr$tlisten + npr$tprice + npr$income + npr$pgrad + npr$cgrad + npr$female+ npr$retired)
  zstar.retired[r] <- (glm.full.star$coefficients[10]-glm.full$coefficients[10])/summary(glm.full.star)$coefficients[10,2]
}

zstar.ordered <- sort(zstar.retired, decreasing=FALSE)
cv.star.retired <- c(zstar.ordered[25], zstar.ordered[975])
z.retired
cv.star.retired

########################################################################

# Assessing Model with best "performance score":
# Model with the best performance score is Logit model recommended by Adjusted R-squared
glm1<-glm(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, family=binomial, data=train)
summary(glm1)

# With Full Data set:
glm1<-glm(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, family=binomial, data=npr)
summary(glm1)

# Testing Accuracy on Test Set of 1200 observations (Test set earlier was 600 observations) and different Seed Number:

glm.best.perf<-glm(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, family=binomial, data=train1)
glm.prob.best2<-predict(glm.best.perf, test1, type="response") 
glm.pred.best2<-rep(0, nrow(test1)) 
glm.pred.best2[glm.prob.best2>0.5] <- 1 
table(glm.pred.best2) 
table(test1$donate)
table(glm.pred.best2, test1$donate) 
glm.accuracy.perf<- 100*(mean(glm.pred.best2 == test1$donate))
glm.accuracy.perf

# This model is around 72.36% accurate, so it appears to be less accurate than the GLM model recommended by BIC
# This model performs better in terms of AIC (4186.9 versus 4197.6 for GLM by BIC), but because this model is a bit more complex,
# it includes more regressors and is not as accurate, going to recommend the BIC GLM model

#################

# Assessing Accuracy of LDA Model with Adjusted R-Squared set of variables
# Second most accurate model

lda.fit.adjr<-lda(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, data=train1)
lda.pred.adjr <- predict(lda.fit.adjr, test1)
lda.class.adjr <- lda.pred.adjr$class
table(lda.class.adjr)
table(lda.class.adjr, test1$donate)
lda.accuracy.adjr<-100*(mean(lda.class.adjr==test1$donate))
lda.accuracy.adjr

# This model is 72.03% accurate

# LDA Model on Full Data set:

lda<-lda(donate~msapop+listen+members+tlisten+popdens+tprice+income+pgrad+cgrad+female+white+retired, data=npr)
lda


# Note, I moved this code out of the way down to the bottom so that the bootstrap procedures could be right after the running of the BIC GLM model