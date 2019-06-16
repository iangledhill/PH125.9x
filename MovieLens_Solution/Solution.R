#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Ian Gledhill
# Descrition: This is the main script for analyzing the movie ratings.
#_____________________________________________________________________

set.seed(2039)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Begin Setup Code
#=====================================================================
# Code from https://www.edx.org/course/data-science-capstone
#
# This takes a random 10% sample of the full 10M reviews and saves them in a 
# data.frame called edx. 
#
#_____________________________________________________________________

###################################
# Create edx set and validation set
###################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End Setup Code
#_____________________________________________________________________


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Begin Supplemntary Functions
#=====================================================================

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define a function that will calculate the RMSE for two sets.
#_____________________________________________________________________
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Partitions the supplied data into a test and train data set using the 
# supplied percentage to the training data set.
#_____________________________________________________________________
createTrainAndTestSets <- function(data, trainPercentage){
  test_index <- createDataPartition(y = data$rating,
                                    times = 1,
                                    p = 1 - trainPercentage,
                                    list = FALSE)
  
  train_set <- data[-test_index,]
  
  test_set <- data[test_index,]
  
  test_set <- test_set %>%
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  list(train_set = train_set, test_set = test_set)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a data frame from the method name and RMSE result.
#_____________________________________________________________________
add_Result <- function(name, rmse){
  data_frame(method=name, RMSE = rmse)  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate the RMSE using the mean as the predictor.
#_____________________________________________________________________
predict_UsingMean <- function(train_set, test_set){
  mu <- mean(train_set$rating)
  RMSE(test_set$rating, mu)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate the RMSE using the mean and the movie effect as the
# predictor.
#_____________________________________________________________________

predict_UsingMovieEffect <- function(train_set, test_set){
  mu <- mean(train_set$rating)
  
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu))
  
  predicted_ratings <- mu + test_set %>% 
    left_join(movie_avgs, by='movieId') %>% 
    .$b_i
  
  RMSE(test_set$rating, predicted_ratings)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate the RMSE using the mean as the user effect.
#_____________________________________________________________________

predict_UsingUserEffect <- function(train_set, test_set){
  mu <- mean(train_set$rating)
  
  user_avgs <- train_set %>% 
    group_by(userId) %>% 
    summarize(b_u = mean(rating - mu))
  
  predicted_ratings <- mu + test_set %>% 
    left_join(user_avgs, by='userId') %>% 
    .$b_u
  
  RMSE(test_set$rating, predicted_ratings)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate the RMSE using the mean and regulated movie and user as 
# the predictor.
#_____________________________________________________________________

predict_UsingRegulatedMovieAndUserEFfect <- function(train_set, test_set, lambda) {
  
  # Average rating of all movies
  mu <- mean(train_set$rating)
  
  # Regularized movie effect
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + lambda))
  
  # Regularized user effect
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu) / (n() + lambda))
  
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  RMSE(test_set$rating, predicted_ratings)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End Supplemntary Code
#_____________________________________________________________________

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


