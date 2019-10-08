library(data.table);
set.seed(140);

#### (*) Exercise 1: Mean Absolute Percentage Error, MAPE, and Root Square Mean Error, RMSE are two ####
#### popular metrics when measuring regression models. The first one is defined as:

#### https://en.wikipedia.org/wiki/Mean_absolute_percentage_error


#### and the second one as:

#### https://en.wikipedia.org/wiki/Root-mean-square_deviation

#### Using these definitions create two functions called MAPE and RMSE, one to compute each metric, and 
#### call them using these vectors representing predictions and their corresponding real values.

real <- sample(1:10, 100, replace = TRUE);
predictions <- real + runif(length(real), -1, 1);


MAPE <- function(prediction, real){
  ret <- 100*mean(abs((prediction - real) / real));
  return(ret);
}

RMSE <- function(prediction, real){
  mse <- mean((prediction - real)^2)
  ret <- sqrt(mse);
  return(ret);
}

MAPE(predictions, real);
RMSE(predictions, real);

#### (*) Exercise 2: In the script we saw in class we repeated three steps (independent of the model) ####
#### several times for our regression models:

#### 1) Get model predictions
#### 2) Get errors
#### 3) Compute Metrics (MSE and MAE)

#### Imagine that you have the following model and dataset

index_train <- sample(1:nrow(mtcars), 0.7*nrow(mtcars));
model <- lm(mpg ~ ., data = mtcars[index_train,]);
dataset <- setDT(mtcars[-index_train,]);
dataset;

#### Create a single function 'ml_wrapper' to compute the three previous steps with a call like this one

ml_wrapper(model = model, dataset = dataset, target = dataset$mpg);

#### The output should be

# $`predictions`
# 1        2        3        4        5        6        7        8        9       10 
# 23.66050 19.37174 17.59102 19.26578 17.03017 14.09378 13.84086 23.68908 23.58032 26.01283 
# 
# $errors
# 1         2         3         4         5         6         7         8         9        10 
# 2.660501 -2.028258 -1.108979 -5.134222  1.830166  3.693779  3.440856 -6.710920  7.780324  4.612833 
# 
# $mse
# [1] 19.45
# 
# $mae
# [1] 3.9


ml_wrapper <- function(model, dataset, target){
  
  # 1) Get model predictions
  predictions <- predict(model, newdata = dataset);
  
  # 2) Get errors
  errors <- predictions - target;
  
  # 3) Compute Metrics (MSE and MAE)
  mse <- round(mean(errors^2), 2);
  mae <- round(mean(abs(errors)), 2);
  
  return(list(predictions = predictions,
         errors = errors,
         mse = mse,
         mae = mae));
}

#### (*) Exercise 3: These two commands are equivalent ####

model_1 <- lm(mpg ~ wt, data = mtcars[index_train,]);
model_2 <- lm("mpg ~ wt", data = mtcars[index_train,]);
identical(model_1$coefficients, model_2$coefficients);

#### Following the syntax of model_2, try now to create an equivalent call to the following one

model_1 <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear, data = mtcars[index_train,]);

#### without explicitly writing the names of the predictor variables. Take notice that the only variables
#### not used as predictors are "carb" and the target itself, "mpg". Suggestion: Use paste0.

formula <- paste0("mpg ~ ",paste0(setdiff(colnames(mtcars), c("mpg", "carb")), collapse = " + "));
model_2 <- lm(formula, data = mtcars[index_train,]);
identical(model_1$coefficients, model_2$coefficients);


#### (*) Exercise 4: Imagine you have the following metrics evaluating several regression models ####
#### for the same problem. 

comp <- data.table(model = c("model1", "model2", "model3", "model4", "model5"), 
                   mae_train = c(0.8, 0.3, 0.6, 1.7, 3.1),
                   mae_test = c(1.0, 2.8, 0.7, 1.7, 5.6));
comp;

#### For each model, try to guess if it that is a case of overfitting, underfitting or none. 

# model 1: None
# model 2: Overfitting
# model 3: None
# model 4: Underfitting
# model 5: Underfitting

#### (*) Exercise 5: Imagine you have the following machine learning model ####

library(e1071);

index_train <- sample(1:nrow(iris), 0.7*nrow(iris));
train <- iris[index_train,];
model <- svm(Species ~ ., data = train, kernel="radial"); 
model;

#### Is this a classification or a regression problem? Try to build a linear model (regressor or
#### classifier depending on your answer) for this problem. Suggestion: [2.1.3] section of script_s16-17.R

model <- glm(Species ~ ., data = train, family = "binomial");


#### (**) Exercise 6: Considering the problem in exercise 5, let's use accuracy as ####
#### evaluation metric for the following model

model <- svm(Species ~ ., data = train, kernel="radial",
             cost = 10^-3, gamma = 10^-5); 

accuracy <- function(prediction, real){
  ret <- 100*sum(prediction == real)/length(real);
  return(ret);
}

ml_wrapper <- function(model, dataset, target, metric_f){
  
  # 1) Get model predictions
  predictions <- predict(model, newdata = dataset);
  
  # 2) Compute Metrics
  metric <- metric_f(predictions, target);
  
  return(metric);
}

results_train <- ml_wrapper(model, iris[index_train,], iris$Species[index_train], accuracy);
results_test <- ml_wrapper(model, iris[-index_train,], iris$Species[-index_train], accuracy);
results_train;
results_test;

#### This model has a clear problem of underfitting. Can you fix this problem without using
#### a hyperparameter grid search? Suggestion: low values of cost means low complexity.

model <- svm(Species ~ ., data = train, kernel="radial",
             cost = 1); 

results_train <- ml_wrapper(model, iris[index_train,], iris$Species[index_train], accuracy);
results_test <- ml_wrapper(model, iris[-index_train,], iris$Species[-index_train], accuracy);
results_train;
results_test;



#### (**) Exercise 7: Random Forests are another popular choice for regression models ####
#### You can install, load and check the documentation of its library like this

# install.packages("randomForest")
library(randomForest);
help(randomForest);

#### Build a Random Forest model to predict 'Species' in the iris dataset and
#### compare its results with the ones you achieved in exercise 7.

model <- randomForest(Species ~ ., data = train); 

results_train <- ml_wrapper(model, iris[index_train,], iris$Species[index_train], accuracy);
results_test <- ml_wrapper(model, iris[-index_train,], iris$Species[-index_train], accuracy);
results_train;
results_test;

#### (**) Exercise 8: The use of global accuracy as evaluation metric can have ####
#### significant drawbacks. For instance, imagine you are working in an unbalance
#### problem like this one

target <- c(rep(1,990),rep(0,10));
table(target);

#### if we build a dummy model that simply outputs one regardless of the input

predictions <- rep(1, length(target));

#### we would get an accuracy of 99%

accuracy(predictions, target);


#### Do you think a dummy constant model that always give the value of 1 as prediction
#### is a good machine learning model? That's why the Area Under the Curve, AUC, metric
#### is usually the choice in these situations. You can see it's definition here:
#### https://developers.google.com/machine-learning/crash-course/classification/roc-and-auc?hl=es-419
#### Try to create a function to compute this AUC value. For instance, the following calls

AUC(target, predictions);
AUC(target, runif(length(target), 0, 1));
AUC(target, target + runif(length(target), 0, 2));
AUC(target, target);

#### Should give you the following results respectively
# Area under the curve: 0.5
# Area under the curve: 0.5075
# Area under the curve: 0.8555
# Area under the curve: 1

#### Suggestion: use pROC library

# install.packages("pROC");
library(pROC);
AUC <- function(target, prediction){
  roc_curve <- roc(target, prediction);
  auc(roc_curve);
}

#### (***) Exercise 9: Replicate the grid search we did in [1.2.3] of Session 16-17 ####
#### class material but using now foreach. The number of cores selected to do the parallelization
#### is up to you.

# setting seed to reproduce results of random sampling
set.seed(100); 

# Convert mtcars to data.table
dat <- as.data.table(mtcars);

# row indices for training data (70%)
train_index <- sample(1:nrow(dat), 0.7*nrow(dat));  

# row indices for validation data (15%)
val_index <- sample(setdiff(1:nrow(dat), train_index), 0.15*nrow(dat));  

# row indices for test data (15%)
test_index <- setdiff(1:nrow(dat), c(train_index, val_index));

# split data
train <- dat[train_index]; 
val <- dat[val_index]; 
test  <- dat[test_index];

dim(dat);
dim(train);
dim(val);
dim(test);

# Load libraries
library(doParallel);
library(foreach);

# Start cluster
stopImplicitCluster();
registerDoParallel(cores = detectCores());

### Define grid
c_values <- 10^seq(from = -3, to = 3, by = 1);
eps_values <- 10^seq(from = -3, to = 0, by = 1);
gamma_values <- 10^seq(from = -3, to = 3, by = 1);

### Compute grid search
grid_results <-  foreach (c = c_values, .combine = rbind)%:%
  foreach (eps = eps_values, .combine = rbind)%:%
    foreach (gamma = gamma_values, .combine = rbind)%dopar%{
      
      library(e1071);
      library(data.table);
      
      print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma));
      
      # train SVM model with a particular set of hyperparamets
      model <- svm(mpg ~ ., data = train, kernel="radial",
                   cost = c, epsilon = eps, gamma = gamma);
      
      # Get model predictions
      predictions_train <- predict(model, newdata = train);
      predictions_val <- predict(model, newdata = val);
      
      # Get errors
      errors_train <- predictions_train - train$mpg;
      errors_val <- predictions_val - val$mpg;
      
      # Compute Metrics
      mse_train <- round(mean(errors_train^2), 2);
      mae_train <- round(mean(abs(errors_train)), 2);
      
      mse_val <- round(mean(errors_val^2), 2);
      mae_val <- round(mean(abs(errors_val)), 2);
      
      # Build comparison table
      data.table(c = c, eps = eps, gamma = gamma, 
                 mse_train = mse_train, mae_train = mae_train,
                 mse_val = mse_val, mae_val = mae_val);
    }

# Order results by increasing mse and mae
grid_results <- grid_results[order(mse_val, mae_val)];

# Check results
grid_results[1];


#### You can also use random search so you do not try all possible combinations of
#### points. An heuristic will decide which points to check. You can do 
#### that using caret library: https://topepo.github.io/caret/random-hyperparameter-search.html

library(caret);

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           search = "random");

fit <- train(mpg ~ ., data = train, 
                 method = "svmRadialSigma",
                 metric = "RMSE",
                 tuneLength = 30,
                 trControl = fitControl,
                 tuneGrid = data.frame(sigma = gamma_values,
                                       C = c_values));

fit;
grid_results <- data.table(fit$results)[order(RMSE)];
grid_results[1];

#### (***) Exercise 10: Your goal now is to predict the 'wt' column of mtcars. ####
#### Find a model with less than 0.05 of MSE for the test set.

# Start cluster
stopImplicitCluster();
registerDoParallel(cores = detectCores());

### Define grid
c_values <- seq(from = 200, to = 300, length.out = 10);
eps_values <- seq(from = 0.04, to = 0.07, length.out = 10);
gamma_values <- seq(from = 0.006, to = 0.01, length.out = 10);

### Compute grid search
grid_results <-  foreach (c = c_values, .combine = rbind)%:%
  foreach (eps = eps_values, .combine = rbind)%:%
  foreach (gamma = gamma_values, .combine = rbind)%dopar%{
    library(e1071);
    library(data.table);
    
    print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma));
    
    # train SVM model with a particular set of hyperparamets
    model <- svm(wt ~ ., data = train, kernel="radial",
                 cost = c, epsilon = eps, gamma = gamma);
    
    # Get model predictions
    predictions_train <- predict(model, newdata = train);
    predictions_val <- predict(model, newdata = val);
    
    # Get errors
    errors_train <- predictions_train - train$wt;
    errors_val <- predictions_val - val$wt;
    
    # Compute Metrics
    mse_train <- round(mean(errors_train^2), 2);
    mse_val <- round(mean(errors_val^2), 2);
    
    # Build comparison table
    data.table(c = c, eps = eps, gamma = gamma, 
               mse_train = mse_train,
               mse_val = mse_val);
  }

# Order results by increasing mse and mae
grid_results <- grid_results[order(mse_val, mse_train)];

# Check results
best <- grid_results[1];

### Train final model
# train SVM model with best found set of hyperparamets
model <- svm(wt ~ ., data = train, kernel="radial",
             cost = best$c, epsilon = best$eps, gamma = best$gamma);

# Get model predictions
predictions_train <- predict(model, newdata = train);
predictions_val <- predict(model, newdata = val);
predictions_test <- predict(model, newdata = test);

# Get errors
errors_train <- predictions_train - train$wt;
errors_val <- predictions_val - val$wt;
errors_test <- predictions_test - test$wt;

# Compute Metrics
mse_train <- round(mean(errors_train^2), 2);
mse_val <- round(mean(errors_val^2), 2);
mse_test <- round(mean(errors_test^2), 2);

## Summary
sprintf("MSE_train = %s - MSE_val = %s - MSE_test = %s", mse_train, mse_val, mse_test);
