## Dibyajyoti Sarkar
## MovieLens Project 
## HarvardX: PH125.9x - Capstone Project
## https://github.com/dj-sarkar/

#################################################
# MovieLens Rating Prediction Project Code 
################################################

#### Introduction ####

## Dataset ##

#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes for loading required package: tidyverse and package caret
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
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


# The Validation subset will be 10% of the MovieLens data.
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
#Make sure userId and movieId in validation set are also in edx subset:
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#### Methods and Analysis ####

### Data Analysis ###

# First rows of data
head(edx) %>%
  print.data.frame()

# Total unique movies and users
summary(edx)

# Number of unique movies and users in the edx dataset 
n_distinct(edx$movieId)
n_distinct(edx$genres)
n_distinct(edx$userId)
nrow(edx)

# First rows of data
head(validation) %>%
  print.data.frame()

# Total unique movies and users
summary(validation)

# Number of unique movies and users in the validation dataset 
n_distinct(validation$movieId)
n_distinct(validation$genres)
n_distinct(validation$userId)
nrow(validation)


# Ratings Distribution
edx_upd %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "blue") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  xlab("Number of Ratings") +
  ylab("Rating") +
  ggtitle("Rating Distribution")

# Plot number of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue") +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Number of Movies") +
  ggtitle("Number of Ratings per Movie")


# Movies rated only once (20)
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:20) %>%
  knitr::kable()


# Plot number of ratings given by users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue") +
  scale_x_log10() +
  xlab("Number of Ratings") + 
  ylab("Number of Users") +
  ggtitle("Number of Ratings given by Users")


# Plot mean movie ratings given by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "blue") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_light()


### Modelling Approach ###

## 1. Average Movie Rating Model ##

# Compute the dataset's mean rating
mu <- mean(edx$rating)
mu

# Test results based on simple prediction
model_1_rmse <- RMSE(validation$rating, mu)
model_1_rmse

# Check results
# Save prediction to data frame
rmse_results <- data_frame(method = "Average movie rating model", RMSE = model_1_rmse)
rmse_results %>% knitr::kable()

## 2. Movie Effect Model ##

# Simple model taking into account the movie effect b_i
# Subtract the rating minus the mean for each rating the movie received
# Plot number of movies with the computed b_i
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("blue"),
                     ylab = "Number of Movies", main = "Number of Movies with Computed b_i")


# Test and save predicted rmse results to data frame
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_2_rmse ))
# Check results
rmse_results %>% knitr::kable()

## 3. Movie and User Effect Model ##

# Plot penaly term user effect #
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs%>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("blue"))


user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


# Test and save predicted rmse results to data frame 
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and User Effect Model",  
                                     RMSE = model_3_rmse))

# Check result
rmse_results %>% knitr::kable()

## 4. Regularized Movie and User Effect Model ##

# Lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 10, 0.25)


# For each lambda,find b_i & b_u, followed by rating prediction & testing
# Below code might take some time  
model_4_rmse <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, model_4_rmse)  


# Optimal lambda                                                             
lambda <- lambdas[which.min(model_4_rmse)]
lambda

# Test and save predicted rmse results to data frame                                                             
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie and User Effect Model",  
                                     RMSE = min(model_4_rmse)))

# Check result
rmse_results %>% knitr::kable()

#### Results ####                                                            
# RMSE Results Overview                                                          
rmse_results %>% knitr::kable()
