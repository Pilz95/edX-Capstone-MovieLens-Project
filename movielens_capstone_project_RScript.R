##### SETUP #####
# change general options
options(scipen = 10000, digits = 5, timeout = 120)

# save / load data from last session
here::here()
# load('movielens_capstone_project.RData')

##### INSTALLATION #####
# install packages using pacman
if(!require(pacman)) install.packages('pacman', repos = 'http://cran.us.r-project.org')
pacman::p_load(pacman, # Package Management Tool
               tidyverse, # Easily Install and Load the 'Tidyverse'
               # magrittr, # A Forward-Pipe Operator for R
               # batman, # Convert Categorical Representations of Logicals to Actual Logicals
               # stringi, # Character String Processing Facilities; addition to stringr
               # rio, # A Swiss-Army Knife for Data I/O; imports readxl & openxlsx
               tidylog, # Logging for 'dplyr' and 'tidyr' Functions; See what is happening
               # janitor, # Simple Tools for Examining and Cleaning Dirty Data; clean column names
               # broom, # Convert Statistical Objects into
               lubridate, # Make Dealing with Dates a Little Easier
               caret, # Classification and Regression train
               here, # A Simpler Way to Find Your Files
               # pdftools, # Text Extraction, Rendering and Converting of PDF Documents
               # matrixStats, # Functions that Apply to Rows and Columns of Matrices (and to Vectors)
               # Rborist, # Extensible, Parallelizable Implementation of the Random Forest Algorithm
               # randomForest, # Classification and regression based on a forest of trees
               # dslabs, # Data Science Labs
               # tinytex, # Install, Maintain and Compile LaTeX Documents
               recosystem, # Recommender System using Matrix Factorization
               plot.matrix # visualize a matrix as is with a heatmap
               )


##### IMPORT AND TRANSFORM #####
### create data sets
# # alternative: load data set that was already prepared
# # load('movielens_quiz.RData')
# 
# # Create edx and final_holdout_test sets
# 
# # Note: this process could take a couple of minutes
# 
# # if(!require(tidyverse)) install.packages('tidyverse', repos = 'http://cran.us.r-project.org')
# # if(!require(caret)) install.packages('caret', repos = 'http://cran.us.r-project.org')
# 
# # library(tidyverse)
# # library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- 'ml-10M100K.zip'
if(!file.exists(dl)) download.file('https://files.grouplens.org/datasets/movielens/ml-10m.zip', dl)

ratings_file <- 'ml-10M100K/ratings.dat'
if(!file.exists(ratings_file)) {
  unzip(dl, ratings_file) }

movies_file <- 'ml-10M100K/movies.dat'
if(!file.exists(movies_file)) {
  unzip(dl, movies_file) }

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed('::'), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c('userId', 'movieId', 'rating', 'timestamp')
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed('::'), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c('movieId', 'title', 'genres')
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = 'movieId')

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind='Rounding') # if using R 3.6 or later
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>%
  semi_join(edx, by = 'movieId') %>%
  semi_join(edx, by = 'userId')

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

### split the edx data into a train and a validation set so that the 
### final_holdout_test stays untouched
# create indices, ratio 90% train, 10% validation
set.seed(1, sample.kind = 'Rounding') # if using R 3.6 or later; WHY do I get a warning?
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in edx_validation_set are also in edx set
edx_validation_set <- temp %>%
  semi_join(edx_train_set, by = 'movieId') %>%
  semi_join(edx_train_set, by = 'userId')

# Add rows removed from edx_validation_set back into edx_train_set
removed <- anti_join(temp, edx_validation_set)
edx_train_set <- rbind(edx_train_set, removed)

rm(removed, temp, test_index)

##### EXAMINATION #####
### inspect train data set
# structure
str(edx)

# # summary
summary(edx)

# numbers of individual users, movies, ratings and genres
edx %>% 
  select(-title, -timestamp) %>%
  map_df(. %>% n_distinct())

# how are the ratings distributed
edx %>%
  mutate(group = ifelse(rating%%1==0, 'whole star', 'half star')) %>% 
  ggplot(aes(rating, fill = group)) +
  geom_histogram(binwidth = 0.25, color = 'black') +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle('Rating distribution') # +
  # theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5),
  #       legend.position = 'bottom', axis.text.y = element_text(hjust = 1, colour = colors)) 

# which movies are rated most often, which ones only once and what are the actual numbers
# Most rated films
edx %>% group_by(title) %>%
  summarize(n_ratings = n()) %>%
  arrange(desc(n_ratings)) %>% 
  head()

# Number of movies rated once
edx %>% group_by(title) %>%
  summarize(n_ratings = n()) %>%
  filter(n_ratings == 1) %>%
  summarise(n = n()) %>% 
  pull()

# histogram number of ratings by userID
edx %>%
  group_by(userId) %>% 
  summarize(count = n()) %>%
  ggplot(aes(count)) +
  geom_histogram(bins = 30, color = 'black') +
  scale_x_log10() +
  ggtitle('Number of ratings by userId') +
  labs(x='Number or ratings' ,
       y='number of users')

# histogram number of ratings by movieId
edx %>%
  group_by(movieId) %>% 
  summarize(count=n()) %>%
  ggplot(aes(count)) +
  geom_histogram(bins=30, color = 'black') +
  scale_x_log10() +
  ggtitle('Number of ratings by movieId') +
  labs(x='Number of ratings' ,
       y='Number of movies')

### timestamps
# first and last rating, duration
edx %>% 
  mutate(min = date(as_datetime(min(edx$timestamp))),
         max = date(as_datetime(max(edx$timestamp))),
         duration = time_length(max-min, 'years')) %>% 
  select(min,max,duration) %>% 
  distinct() %>% 
  as_tibble()

# total ratings per year, 
edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "black") + 
  ggtitle("Total Ratings Per Year") +
  xlab("Year") +
  ylab("Ratings") +
  theme(plot.title = element_text(hjust = .0), plot.subtitle = element_text(hjust = .5),
        legend.position = 'bottom', axis.text.y = element_text(hjust = 1),
        axis.text.x = element_text(hjust = 1, angle = 45))

# ratings per year, violin plot
edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  group_by(year) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(year, rating)) +
  geom_violin() + 
  ggtitle("Rating Distribution Per Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  theme(plot.title = element_text(hjust = .0), plot.subtitle = element_text(hjust = .5),
        legend.position = 'bottom', axis.text.y = element_text(hjust = 1),
        axis.text.x = element_text(hjust = 1, angle = 45))

### genres
# categories with the least and the most movies
genres <- tibble(count = str_count(edx$genres, fixed("|")), genres = edx$genres) %>% 
  group_by(count, genres) %>%
  summarise(n = n()) %>%
  arrange(n)
genres[c(1:5, (nrow(genres)-5):nrow(genres)), 2:3]

# how many genres counts how often
edx %>% 
  group_by(genres) %>% 
  summarize(n = n(),
            count = as.numeric(str_count(genres, fixed('|')) + 1)
  ) %>% 
  distinct() %>% 
  ggplot(aes(count)) +
  geom_histogram(bins = 8, color = 'black') +
  ggtitle('Amount of different genres in the genres catagory')

##### ANALYSIS #####
### Model 1: Avarage movie rating
# no code to support the report necessary

### Model 2: Movie effect model 
# no code to support the report necessary

### Model 3: Movie and user effect model 
# no code to support the report necessary

### Model 4: Regularized movie and user effect model
# no code to support the report necessary

### Model 5: Matrix factorization
# recommendations system problem as a matrix
set.seed(1, sample.kind = 'Rounding')
random_users <- sample(unique(edx$userId), 100)
user_movie_matrix <- edx %>% 
  filter(userId %in% random_users) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  pivot_wider(names_from = 'movieId',
              values_from = 'rating') %>%
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.)
class(user_movie_matrix)
# rm(random_users)

# plot the matrix
user_movie_matrix %>% 
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
title("Movie/User combination Matrix")

# time consumption classical matrix transformation
# set seed
set.seed(1, sample.kind = 'Rounding')
time <- vector(length = 10L)
# repeat the matrix transformation ten times
for (i in 1:10) {
# sample users from edx data set, n = 2000
random_users_2 <- sample(unique(edx$userId), 3000)
# save start time
start <- Sys.time()
# perform matrix transformation of edx subset
edx %>% 
  filter(userId %in% random_users_2) %>%
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = 'movieId',
              values_from = 'rating') %>%
  as.matrix() %>% 
  t()
# save the time the transformation takes
time[i] <- Sys.time() - start
}
# take the average of the computing times, print it
time_mean <- mean(time)
time_mean
# rm(random_users, time, time_mean)


##### RESULTS #####
# define RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
  }
  
### model 1: average across every entry, no user or movie effect
# calculate the average, then the RMSE
mu_hat <- mean(edx_train_set$rating)
mu_hat
model_1_rmse <- RMSE(edx_validation_set$rating, mu_hat)

### model 2: add an independent error term b_i that expresses rating differences for movies
# calculate movie effect b_i
b_i <- edx_train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

# predict all unknown ratings with mu and b_i
predicted_ratings <- edx_validation_set %>% 
  left_join(b_i, by = 'movieId') %>%
  mutate(pred = mu_hat + b_i) %>%
  pull(pred)

# calculate RMSE with movie effect
model_2_rmse <- RMSE(edx_validation_set$rating, predicted_ratings)


### model 3: introduce the user bias term b_u; each user u is given a bias term
# calculate user effect b_u
b_u <- edx_train_set %>% 
  left_join(b_i, by = 'movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu_hat - b_i))

# predict all unknown ratings with mu, b_i and b_u
predicted_ratings <- edx_validation_set %>% 
  left_join(b_i, by = 'movieId') %>% 
  left_join(b_u, by = 'userId') %>% 
  mutate(pred = mu_hat + b_i + b_u) %>% 
  pull(pred)

# calculate RMSE with movie and user effect
model_3_rmse <- RMSE(edx_validation_set$rating, predicted_ratings)


### model 4: Finally, we employ regularization and introduce a penalty term
# write function to output and save RMSE of each lambda
regularization_model <- function(train_set, validation_set, lambda){
  # calc. mu_hat (average rating across edx_traning_set)
  mu_hat <- mean(train_set$rating)
  # calc. regularized movie bias term
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+lambda))
  # calc. regularized user bias term
  b_u <- train_set %>% 
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat)/(n()+lambda))
  # compute validation_set predictions
  predicted_ratings <- validation_set %>% 
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    filter(!is.na(b_i), !is.na(b_u)) %>% 
    mutate(pred = mu_hat + b_i + b_u) %>%
    pull(pred)
  # output RMSE
  return(RMSE(predicted_ratings, validation_set$rating))
}

# write lambda sequence
lambdas <- seq(0, 10, 0.25)
# sapply lambdas with edx_train_set and edx_validation_set
rmses <- sapply(lambdas, 
                regularization_model,
                train_set = edx_train_set,
                validation_set = edx_validation_set)

# plot lambdas vs. rmses
tibble(lambda = lambdas, rmse = rmses) %>% 
  ggplot(aes(lambda, rmse)) + 
  geom_point() +
  ggtitle('Regularization', subtitle = 'Mimimalization of the penalty term')

# for which lambda is RMSE minimized
lambdas[which.min(rmses)]
# save tuned lambda
tuned_lambda <- lambdas[which.min(rmses)]
# save rsme for model 4
model_4_rmse <- min(rmses)
model_4_rmse


### model 5: Matrix factorization
# can't use tibble or data_frames because it uses to much RAM
# use recosystem package
set.seed(9999, sample.kind = 'Rounding')

# convert data into recosystem format
edx_train_data <- with(edx_train_set, data_memory(user_index = userId,
                                                  item_index = movieId,
                                                  rating = rating))
edx_validation_data <- with(edx_validation_set, data_memory(user_index = userId,
                                                            item_index = movieId,
                                                            rating = rating))

# create a recommendation model object
r <- Reco()

# select the best tuning parameters
opts <- r$tune(edx_train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))
# train the model
r$train(edx_train_data, opts = c(opts$min, nthread = 4, niter = 20))

# predict ratings
predicted_ratings <- r$predict(edx_validation_data, out_memory())

# calculate rmse
model_5_rmse <- RMSE(edx_validation_set$rating, predicted_ratings)

### do it for the final_holdout_test
# convert data into recosystem format
edx_data <- with(edx, data_memory(user_index = userId,
                                  item_index = movieId,
                                  rating = rating))
final_holdout_test_data <- with(final_holdout_test, data_memory(user_index = userId,
                                                                item_index = movieId,
                                                                rating = rating))

# create a recommendation model object
r_final <- Reco()

# select the best tuning parameters
opts_final <- r$tune(edx_data, opts = list(dim = c(10, 20, 30), 
                                           lrate = c(0.1, 0.2),
                                           costp_l2 = c(0.01, 0.1), 
                                           costq_l2 = c(0.01, 0.1),
                                           nthread  = 4, niter = 10))
# train the model
r_final$train(edx_data, opts = c(opts_final$min, nthread = 4, niter = 20))

# predict ratings
predicted_ratings <- r$predict(final_holdout_test_data, out_memory())

# calculate rmse
matrix_fact_final_rmse <- RMSE(final_holdout_test$rating, predicted_ratings)


##### RESULTS #####
# include result in tibble rmse_results
rmse_results <- tibble(method = c('Just the average',
                                  'Movie effect',
                                  'Movie and user effect',
                                  'Regulized movie and user effect',
                                  'Matrix factorization',
                                  'Final RMSE on final holdout set'), 
                       RMSE = c(model_1_rmse,
                                model_2_rmse,
                                model_3_rmse,
                                model_4_rmse,
                                model_5_rmse,
                                matrix_fact_final_rmse)
                       )
view(rmse_results)

##### EXPORT #####
# save all data using a relative path
save.image(here('movielens_capstone_project.RData'))
