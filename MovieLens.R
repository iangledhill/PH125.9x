set.seed(1, sample.kind = "Rounding")

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


# Q1
nrow(edx)
ncol(edx)


# Q2
edx %>% group_by(rating) %>% summarize(n = n())

# Q3
length(unique(edx$movieId))
# or 
n_distinct(edx$movieId)

# Q4
n_distinct(edx$userId)

# Q5
#edx %>% 
#  filter(genres %in% c("Drama", "Comedy", "Thriller", "Romance")) %>% 
#  group_by(genres) %>% 
#  summarize(n = n())

tidyr::separate_rows(edx, genres, sep = "\\|") %>% 
  filter(genres %in% c("Drama", "Comedy", "Thriller", "Romance")) %>% 
  group_by(genres) %>% 
  summarize(n = n())


# Q6
edx %>% 
# Title has year at the end, so this won't work
#  filter(title %in% c("Forrest Gump", "Jurassic Park", "Pulp Fiction", "The Shawshank Redemption", "Speed 2: Cruise Control")) %>% 
  group_by(title) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))

# Q7
edx %>% 
  # Title has year at the end, so this won't work
  #  filter(title %in% c("Forrest Gump", "Jurassic Park", "Pulp Fiction", "The Shawshank Redemption", "Speed 2: Cruise Control")) %>% 
  group_by(rating) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))
