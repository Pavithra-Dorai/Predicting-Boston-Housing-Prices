

## 0 load the package 

library(leaps)



## 1 create a data frame

# load the data 
housing.df <- read.csv("BostonHousing.csv")

# remove the variable CAT..MEDV
housing.df$CAT..MEDV <- NULL

# first six rows
head(housing.df)

# column names 
names(housing.df)



## 2 fit a multiple linear regression model

# linear regression 
lm <- lm(MEDV ~ CRIM + CHAS + RM, data = housing.df)

# summary table 
summary(lm)



## 3 predict for a new sample  

# create a data frame 
new <- data.frame(CHAS = 0, CRIM = 0.1, RM = 6)

# make a prediction 
predicted_price <- predict(lm, new)


## 4 data partition 

# set seed for reproducing the partition 
set.seed(1)

# row numbers of the training set 
train.index <- sample(1:dim(housing.df)[1], 0.6 * dim(housing.df)[1])

# training set 
train.df <- housing.df[train.index, ]

# test set 
test.df <- housing.df[-train.index, ]


## 5 exhaustive research 

# exhaustive search
search <- regsubsets(MEDV ~ ., data = train.df, nbest = 1, nvmax = 12, method = "exhaustive")

# summary table 
sum <- summary(search)
sum

# show models
sum$which

# show metrics
sum$adjr2


## 6 model with the highest adjusted R-squared 

# sort the adjusted r-squared 
best_model <- order(sum$adjr2, decreasing = T)[1]

# a logical vector indicating which elements are in the 10th model
model_10_predictors <- sum$which[10, ]

# names of the predictors in the 10th model 
selected.vars <- names(train.df)[sum$which[best_model, -1]]

# fit a linear regression model using the training set 
lm.search <- lm(MEDV ~ ., data = train.df[, selected.vars])

# make predictions on the test set 
predicted_prices <- predict(lm.search, newdata = test.df[, selected.vars])
predicted_prices

# MSE in the test set 
mean((test.df$MEDV - predicted_prices)^2)



## 7 backward elimination 

# fit the model with all 12 predictors  
lm.full <- lm(MEDV ~ ., data = train.df)

# use step() to run backward elimination 
lm.step.backward <- step(lm.full, direction = "backward")

# summary table 
summary(lm.step.backward)

# making predictions on the test set
predicted_prices_backward <- predict(lm.step.backward, newdata = test.df)
predicted_prices_backward

# MSE in the test set 
mean((test.df$MEDV - predicted_prices_backward)^2)



## 8 forward selection

# create model with no predictors
lm.null <- lm(MEDV ~ 1, data = train.df)

# use step() to run forward selection  
lm.step.forward <- step(lm.null, scope = list(lower = lm.null, upper = lm.full), direction = "forward")

# summary table 
summary(lm.step.forward)

# making predictions on the test set
predicted_prices_forward <- predict(lm.step.forward, newdata = test.df)
predicted_prices_forward

# MSE in the test set 
mean((test.df$MEDV - predicted_prices_forward)^2)



## 9 stepwise regression 

# use step() to run stepwise regression  
lm.step.both <- step(lm.full, direction = "both")

# summary table 
summary(lm.step.both)

# make predictions on the test set
predicted_prices_both <- predict(lm.step.both, newdata = test.df)
predicted_prices_both

# MSE in the test set 
mean((test.df$MEDV - predicted_prices_both)^2)

