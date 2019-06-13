#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Ian Gledhill
# Descrition: This defines a number of functions used to calculate the
#             different methods.
#_____________________________________________________________________


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
