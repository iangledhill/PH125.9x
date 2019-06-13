#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Ian Gledhill
# Descrition: This is the main script for analyzing the movie ratings.
#_____________________________________________________________________

set.seed(2039)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Include supplemntar scripts for extreacting the 10% sample and 
# for defining addiitonal functions used by this script.
#
#_____________________________________________________________________
source("Setup.R")
source("Functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a training and test data set
#_____________________________________________________________________

trainPercentage = 0.8

data <- createTrainAndTestSets(edx, trainPercentage)

train_set <- data$train_set;
test_set <- data$test_set;


library(tidyverse)
library(broom)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Obtain some summary information
#_____________________________________________________________________

# Some movies picked out for the report

movies <- group_by(edx, title) %>% 
  summarize(n = n(), average = mean(rating))

movies_by_mean <- movies  %>%
  # remove highly rated outliers
  filter(n > 20) %>% 
  arrange(desc(average), desc(n))

best_movie <- movies_by_mean[1,]

worst_movie <- movies_by_mean[nrow(movies_by_mean),]

movies_by_n <- movies  %>%
  arrange(desc(n))

most_movie <- movies_by_n[1,]

least_movie <- movies_by_n[nrow(movies_by_mean),]


# Some users picked out for the report  

users <- group_by(edx, userId) %>% 
  summarize(n = n(), average = mean(rating))

users_by_mean <- users  %>%
  arrange(desc(average))

best_user <- users_by_mean[1,]

worst_user <- users_by_mean[nrow(users_by_mean),]

users_by_n <- users  %>%
  arrange(desc(n))

most_user <- users_by_n[1,]

least_user <- users_by_n[nrow(users_by_n),]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build the prediction results
#_____________________________________________________________________

rmse_results <- tibble(method=character(), RMSE=double())

rmse_results <- rbind(rmse_results, add_Result("Mean", predict_UsingMean(train_set, test_set)))
rmse_results <- rbind(rmse_results, add_Result("Movie Effect Model", predict_UsingMovieEffect(train_set, test_set)))
rmse_results <- rbind(rmse_results, add_Result("User Effect Model", predict_UsingUserEffect(train_set, test_set)))

lambdas <- seq(0,10,1)

regulated_rmses <- sapply(lambdas, function(lambda) {
  predict_UsingRegulatedMovieAndUserEFfect(train_set, test_set, lambda)
})

regulated_data <- tibble(RMSE=regulated_rmses, method=paste("Regulated (",lambdas, ")"))

min_regulated <- which.min(regulated_data$RMSE)
min_lambda <- lambdas[min_regulated]

rmse_results <- rbind(rmse_results, regulated_data[min_regulated,])

rmse_results$method <- factor(rmse_results$method, levels=rmse_results$method)


