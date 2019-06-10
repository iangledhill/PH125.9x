# Author: Ian Gledhill
# Descrition: 
#

set.seed(2039)

source("Setup.R")
source("Functions.R")

#
# Create a training and test data set
#

trainPercentage = 0.8

data <- createTrainAndTestSets(edx, trainPercentage)

train_set <- data$train_set;
test_set <- data$test_set;

library(tidyverse)

#
# Build results
#

rmse_results <- tibble(method=character(), RMSE=double())

rmse_results <- rbind(rmse_results, add_Result("Mean", predict_UsingMean(train_set, test_set)))
rmse_results <- rbind(rmse_results, add_Result("Movie Effect Model", predict_UsingMovieEffect(train_set, test_set)))
rmse_results <- rbind(rmse_results, add_Result("User Effect Model", predict_UsingUserEffect(train_set, test_set)))

lambdas <- seq(0,10,0.25)

regulated_rmses <- sapply(lambdas, function(lambda) {
  predict_UsingRegulatedMovieAndUserEFfect(train_set, test_set, lambda)
})

regulated_data <- tibble(RMSE=regulated_rmses, method=paste("Regulated (",lambdas, ")"))

min_regulated <- which.min(regulated_data$RMSE)
min_lambda <- lambdas[min_regulated]

rmse_results <- rbind(rmse_results, regulated_data[min_regulated,])

rmse_results$method <- factor(rmse_results$method, levels=rmse_results$method)

#
# Build plots
#

